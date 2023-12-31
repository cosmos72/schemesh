;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;
;;; Common backend of libraries (schemesh parser r6rs) and (schemesh parser scheme)
;;;
(library (schemesh parser lisp (0 1))
  (export
    lex-lisp parse-lisp parse-lisp* parse-lisp-list parse-lisp-parens)
  (import
    (rnrs)
    (only (chezscheme)
       box bytevector fx1+ fx1-
       fxvector fxvector-set! make-fxvector
       read-token reverse!)
    (only (schemesh bootstrap) while until)
    (only (schemesh containers misc) reverse*!)
    (schemesh lineedit parens)
    (schemesh lineedit parser))


;; Read a single r6rs or Chez Scheme token from textual input port 'in.
;; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
;; in pure R6RS.
;;
;; Return two values: token value and its type.
(define (lex-lisp ctx flavor)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  (let ((value (try-read-parser-directive ctx)))
    (if (symbol? value)
      (if (eq? 'eof value)
        ;; yes, #!eof is an allowed directive:
        ;; it injects (eof-object) in token stream, with type 'eof
        ;; thus simulating an actual end-of-file in input port.
        ;; Reason: historically used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
        ;; cannot switch to other parser here: just return it and let caller switch
        (values (get-parser ctx value (caller-for flavor)) 'parser))
      ;; read a single token with Chez Scheme (read-token),
      ;; then replace (values '{ 'atomic) with (values #f 'lbrace)
      ;; and replace (values '} 'atomic) with (values #f 'rbrace)
      ;; because we use them to switch to shell parser. For example,
      ;;    {ls -l > log.txt}
      ;; is equivalent to
      ;;    (#!shell ls -l > log.txt)
      (let-values (((type value start end) (read-token (parsectx-in ctx))))
        (if (eq? 'atomic type)
          (case value
            (({)  (values #f 'lbrace))
            ((})  (values #f 'rbrace))
            (else (values value type)))
          (values value type))))))


;; Return the symbol, converted to string,
;; of most token types returned by Chez Scheme (read-token),
;;
;; Also recognizes and converts to string the additional types
;; 'lbrace and 'rbrace introduced by (lex-lisp)
(define (lex-type->string type)
  (case type
    ((box) "#&")   ((dot) ".")    ((fasl) "#@")  ((insert) "#N#")
    ((lbrace) "{") ((lbrack) "[") ((lparen) "(") ((mark) "#N=") ((quote) "'")
    ((rbrace) "}") ((rbrack) "]") ((rparen) ")") ((record-brack) "#[")
    ((vfxnparen) "#Nvfx") ((vfxparen) "#vfx")
    ((vnparen)   "#Nv")   ((vparen)   "#v")
    ((vu8nparen) "#Nvu8") ((vu8paren) "#vu8")
    (else (symbol->string type))))


;; Read Scheme tokens from textual input port 'in'
;; by repeatedly calling (lex-lisp) and construct a Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values: parsed form, and #t.
;; If end-of-file is reached, return (eof-object) and #f.
(define (parse-lisp ctx flavor)
  (let-values (((value type) (lex-lisp ctx flavor)))
    (let-values (((ret-value ret-type) (parse-lisp-impl ctx value type flavor)))
      (values ret-value (not (eq? 'eof ret-type))))))

;;
;; Read Scheme tokens from textual input port 'in'
;; by repeatedly calling (lex-lisp) and construct a Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return parsed form.
;; Raises syntax-errorf if end of file is reached before reading a complete form.
(define (parse-lisp* ctx flavor)
  (let-values (((value ok) (parse-lisp ctx flavor)))
    ;; cannot switch to other parser here, and caller does not expect it => raise
    (unless ok
      (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file"))
    (when (parser? value)
      (syntax-errorf ctx (caller-for flavor)
        "parser directive #!... can only appear in lists, not in single-form contexts: ~a"
        (string-append "#!" (symbol->string (parser-name value)))))
    value))

;;
;; Common back-end of (parse-lisp) and (parse-lisp*)
;; Read Scheme tokens from textual input port 'in'
;; by repeatedly calling (lex-lisp) and construct a Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values: the parsed form, and its type.
(define (parse-lisp-impl ctx value type flavor)
  (values
    (case type
      ;; cannot switch to other parser here: just return it and let caller switch
      ((atomic eof parser) value)
      ((box)
        (unless (eq? 'scheme flavor)
          (syntax-errorf ctx (caller-for flavor)
            "invalid token in #!r6rs syntax, only allowed in #!scheme syntax: ~a"
            (lex-type->string type)))
        (list 'box  (parse-lisp* ctx flavor)))
      ;; if type = 'quote, value can be one of:
      ;;    'quote  'quasiquote  'unquote  'unquote-splicing
      ;;    'synyax 'quasisyntax 'unsyntax 'unsyntax-splicing
      ((quote)             (list value (parse-lisp* ctx flavor)))
      ((lbrack lparen)     (parse-lisp-list ctx type '() flavor))
      ;; lbrace i.e. { switches to shell parser until corresponding rbrace i.e. }
      ((lbrace)
        (let ((other-parse-list (parser-parse-list
                (get-parser ctx 'shell (caller-for flavor)))))
          (other-parse-list ctx type '())))
      ;; parse the various vector types, with or without explicit length
      ((vparen vu8paren)
        (parse-vector ctx type value flavor))
      ((vfxnparen vfxparen vnparen vu8nparen)
        (unless (eq? 'scheme flavor)
          (syntax-errorf ctx (caller-for flavor)
            "invalid token in #!r6rs syntax, only allowed in #!scheme syntax: ~a"
            (lex-type->string type)))
        (parse-vector ctx type value flavor))
      ; TODO implement types: record-brack fasl insert mark
      (else   (syntax-errorf ctx (caller-for flavor) "unexpected token type: ~a" type)))
    type))

;;
;; Read Scheme forms from textual input port 'in', until a token ) or ] or } matching
;; the specified begin-type token is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return a list containing parsed forms.
;; Raise syntax-errorf if mismatched end token is found, as for example ']' instead of ')'
;;
;; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-lisp-list ctx begin-type already-parsed-reverse flavor)
  (let* ((ret already-parsed-reverse)
         (again? #t)
         (reverse? #t)
         (end-type (case begin-type
                     ((lbrace) 'rbrace) ((lbrack) 'rbrack) (else 'rparen)))
         (check-list-end (lambda (type)
           (unless (eq? type end-type)
             (syntax-errorf ctx (caller-for flavor) "unexpected token ~a, expecting ~a"
               (lex-type->string type) (lex-type->string end-type))))))
    (while again?
      (let-values (((value type) (lex-lisp ctx flavor)))
        (case type
          ((eof)
            (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file"))
          ((parser)
            ;; switch to other parser until the end of current list
            (let ((other-parse-list (parser-parse-list value)))
              (set! ret (other-parse-list ctx begin-type ret))
              (set! reverse? #f)
              (set! again? #f)))
          ((rparen rbrack rbrace)
            (check-list-end type)
            (set! again? #f))
          ((dot)
            (let-values (((value-i type-i) (parse-lisp ctx flavor)))
              (when (eq? 'parser type-i)
                ;; switch to other parser
                (let ((other-parse* (parser-parse* value-i)))
                  (set! value-i (other-parse* ctx))))
              (set! ret (reverse*! (cons value-i ret)))
              (set! reverse? #f)
              (set! again? #f))
            ;; then parse ) or ] or }
            (let-values (((value type) (lex-lisp ctx flavor)))
              (check-list-end type)))
          (else
            ;; parse a single form and append it
            (let-values (((value-i type-i)
                            (parse-lisp-impl ctx value type flavor)))
              (when (eq? 'eof type-i)
                (syntax-errorf ctx (caller-for flavor) "unexpected ~a" type-i))
              (set! ret (cons value-i ret)))))))
    (if reverse? (reverse! ret) ret)))


;; Read Scheme forms from textual input port 'in' until a token ) or ] or } matching vec-type
;; is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return a vector, fxvector or bytevector containing parsed forms.
;; Raise syntax-errorf if mismatched end token is found, as for example ] instead of )
(define (parse-vector ctx vec-type length flavor)
  (let ((values (parse-lisp-list ctx vec-type '() flavor)))
    (case vec-type
      ((vfxnparen) (create-fxvector   length values))
      ((vnparen)   (create-vector     length values))
      ((vu8nparen) (create-bytevector length values))
      ((vfxparen)  (apply fxvector   values))
      ((vparen)    (apply vector     values))
      ((vu8paren)  (apply bytevector values))
      (else  (syntax-errorf ctx (caller-for flavor) "unexpected ~a" vec-type)))))

(define (caller-for flavor)
  (if (eq? flavor 'r6rs)
    'parse-r6rs
    'parse-scheme))

(define (create-fxvector length values)
  (let ((vec (make-fxvector length))
        (elem (if (null? values) 0 (car values))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i length) vec)
      (fxvector-set! vec i elem)
      (unless (null? values)
        ;; if we run out of values, fill remainder with last element in values
        (set! values (cdr values))
        (unless (null? values)
          (set! elem (car values)))))))

(define (create-vector length values)
  (let ((vec (make-vector length))
        (elem (if (null? values) 0 (car values))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i length) vec)
      (vector-set! vec i elem)
      (unless (null? values)
        ;; if we run out of values, fill remainder with last element in values
        (set! values (cdr values))
        (unless (null? values)
          (set! elem (car values)))))))

(define (create-bytevector length values)
  (let ((vec (make-bytevector length))
        (elem (if (null? values) 0 (car values))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i length) vec)
      (bytevector-u8-set! vec i elem)
      (unless (null? values)
        ;; if we run out of values, fill remainder with last element in values
        (set! values (cdr values))
        (unless (null? values)
          (set! elem (car values)))))))


;; consume text input port until one of the characters ( ) [ ] { } | " #| and return it as a char.
;; also recognize and return parser directives #!... and return them as a symbol.
(define (scan-lisp-parens-or-directive ctx)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  ;; yes, #!eof is an allowed directive:
  ;; it injects (eof-object) in token stream, with type 'eof
  ;; thus simulating an actual end-of-file in input port.
  ;; Reason: historically used to disable the rest of a file, to help debugging
  ;;
  ;; cannot switch to other parser here: just return its name and let caller switch
  (or (try-read-parser-directive ctx)
      (scan-lisp-parens ctx)))


;; read until one of ( ) [ ] { } " | #| is found. ignore them if they are preceded by #\
;; Does not recognize parser directives #!... use (scan-lisp-parens-or-directive)
;; for that
(define (scan-lisp-parens ctx)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\( #\) #\[ #\] #\{ #\} #\" #\|) (set! ret ch))
          ((#\#)  (set! ret (scan-lisp-sharp ctx)))
          ((#\\)  (parsectx-read-char ctx)) ; consume one char after \
          ((#\;)  (parsectx-skip-line ctx))
          (else (when (eof-object? ch) (set! ret ch))))))
    ret))


;; scan a token after # (the # character was already consumed) and return it
(define (scan-lisp-sharp ctx)
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\\) (parsectx-read-char ctx) #f) ; consume one char after #\
      ((#\( #\[ #\{) ch) ; treat #( #[ #{ respectively as ( [ {
      ((#\|) #\#) ; found start of block comment #| thus return #
      (else #f))))


;; skip all characters until sequence |# which is consumed too
;; return #t if |# is found before end-of-file, otherwise return #f
(define (skip-lisp-block-comment ctx)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (cond
          ((eof-object? ch)
            (set! ret ch))
          ((and (eqv? #\| ch) (eqv? #\# (parsectx-peek-char ctx)))
            (set! ret (parsectx-read-char ctx))))))
    (char? ret)))



;; skip characters until the end of double-quoted string
;; (the initial " character was already consumed)
;; Also consume the final #\"
;;
;; return #t if #\" is found before end-of-file, otherwise return #f
(define (skip-lisp-double-quotes ctx)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\") (set! ret ch))
          ((#\\) (parsectx-read-char ctx))
          (else (when (eof-object? ch)
                  (set! ret ch))))))
    (char? ret)))


;; Read Scheme forms from textual input port (parsectx-in ctx),
;; collecting grouping tokens i.e. ( ) [ ] { } |# #| " " | |
;; and filling paren with them.
;;
;; If a parser directive #!... is found, switch to the corresponding parser
;; until the end of current group.
;;
;; Stops on end-of-file, or when closing token matching start-ch is found.
;; Such closing token is consumed too.
;;
;; Flavor must be either 'r6rs or 'scheme
;;
;; Return a parens containing the collected grouping tokens.
(define (parse-lisp-parens ctx start-ch flavor)
  (assert (parsectx? ctx))
  (when start-ch
    (assert (char? start-ch)))
  (assert (symbol? flavor))
  (let* ((paren  (make-parens flavor start-ch))
         (end-ch (case start-ch ((#\() #\)) ((#\[) #\]) ((#\{) #\}) (else #f)))
         (pos    (parsectx-pos ctx))
         (ret    #f)
         (%paren-fill-end! (lambda (paren)
           (parens-end-x-set! paren (fx1- (car pos)))
           (parens-end-y-set! paren (cdr pos)))))
    (parens-start-x-set! paren (fx- (car pos) (if start-ch 1 0)))
    (parens-start-y-set! paren (cdr pos))
    (unless start-ch
      (parens-ok?-set! paren #t))
    (until ret
      (let ((token (scan-lisp-parens-or-directive ctx)))
        (cond
          ((not token) ; not a grouping token
             #f)

          ((eqv? token end-ch) ; found matching close token
             (parens-ok?-set! paren #t)
             (set! ret #t))

          ((symbol? token)
             ; recurse to other parser until end of current list
             (let* ((other-parser       (get-parser-or-false ctx token))
                    (other-parse-parens (and other-parser (parser-parse-parens other-parser)))
                    (other-parens       (and other-parse-parens (other-parse-parens ctx start-ch))))
               (when other-parens
                 (parens-inner-append! paren other-parens)
                 (set! ret #t))))

          ((memv token '(#\( #\[))               #| make vscode happy: #\) |#
             ; recursion: call lisp parser on nested list
             (parens-inner-append! paren (parse-lisp-parens ctx token flavor)))

          ((eqv? token #\{)
             ; recursion: call shell parser on nested list
             (let* ((other-parser (get-parser-or-false ctx 'shell))
                    (other-parse-parens (and other-parser (parser-parse-parens other-parser)))
                    (other-parens (if other-parse-parens
                                    (other-parse-parens ctx token)
                                    (parse-lisp-parens ctx token flavor))))
               (when other-parens
                 (parens-inner-append! paren other-parens))))

          ((memv token '(#\" #\| #\#))
             ; found quoted string, quoted symbol or block comment.
             ; create and fill nested parens object
             (let ((inner (make-parens flavor token)))
               (parens-start-x-set! inner (fx- (car pos) (if (eqv? token #\#) 2 1)))
               (parens-start-y-set! inner (cdr pos))
               (when (cond
                       ((eqv? token #\")               ; parse "some string"
                         (skip-lisp-double-quotes ctx))
                       ((eqv? token #\|)               ; parse |identifier|
                         (parsectx-skip-until-char ctx #\|))
                       (#t                             ; parse #| block comment |#
                         (skip-lisp-block-comment ctx)))
                 (%paren-fill-end! inner)
                 (parens-inner-append! paren inner))))

          ((eof-object? token)
             (set! ret 'err))

          ; ignore unexpected tokens
          )))

    (when (or (eq? #t ret) (not start-ch))
      (%paren-fill-end! paren))
    paren))


(define (paren-caller-for flavor)
  (if (eq? flavor 'r6rs)
    'parse-r6rs-parens
    'parse-scheme-parens))

) ; close library
