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
    (only (chezscheme) format
       box bytevector fx1+ fx1-
       fxvector fxvector-set! make-fxvector
       read-token reverse!)
    (only (schemesh bootstrap) while until)
    (only (schemesh containers misc) reverse*!)
    (schemesh parser base))


;; Read a single r6rs or Chez Scheme token from textual input port 'in.
;; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
;; in pure R6RS.
;;
;; Return two values: token value and its type.
(define (lex-lisp ctx flavor)
  (ctx-skip-whitespace ctx 'also-skip-newlines)
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
      (let-values (((type value start end) (read-token (parse-ctx-in ctx))))
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
      ;; TODO implement types: record-brack fasl insert mark
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


;; consume text input port until one of the characters ( ) [ ] { } " and return it.
;; also recognize and return parser directives #!... and return them
(define (scan-lisp-parens-or-directive ctx)
  (ctx-skip-whitespace ctx 'also-skip-newlines)
  ;; yes, #!eof is an allowed directive:
  ;; it injects (eof-object) in token stream, with type 'eof
  ;; thus simulating an actual end-of-file in input port.
  ;; Reason: historically used to disable the rest of a file, to help debugging
  ;;
  ;; cannot switch to other parser here: just return its name and let caller switch
  (or (try-read-parser-directive ctx)
      (scan-lisp-parens ctx)))


;; read until one of ( ) [ ] { } " | is found. ignore them if they are preceded
;; by #\ and also ignore comments
;; consume text input port until one of ( ) [ ] { } " and return it.
;; Does not recognize parser directives #!... use (scan-lisp-parens-or-directive)
;; for that
(define (scan-lisp-parens ctx)
  (let ((ret #f))
    (until ret
      (let ((ch (ctx-read-char ctx)))
        (case ch
          ((#\( #\) #\[ #\] #\{ #\} #\" #\|) (set! ret ch))
          ((#\#)  (set! ret (skip-lisp-sharp ctx)))
          ((#\;)  (ctx-skip-line ctx))
          (else (when (eof-object? ch) (set! ret ch))))))
    ret))


;; skip a token after # (the # character was already consumed)
(define (skip-lisp-sharp ctx)
  (let ((ch (ctx-read-char ctx)))
    (case ch
      ((#\\) (ctx-read-char ctx) #f) ; consume one char after #\
      ((#\( #\[ #\{) ch) ; treat #( #[ #{ respectively as ( [ {
      ((#\|) (skip-lisp-block-comment ctx))
      (else #f))))


;; skip all characters until |#
(define (skip-lisp-block-comment ctx)
  (let ((done? #f))
    (until done?
      (let ((ch (ctx-read-char ctx)))
        (cond
          ((eof-object? ch)
        (set! done? #t))
          ((and (eqv? #\| ch) (eqv? #\# (ctx-peek-char ch)))
            (ctx-read-char ch)
            (set! done? #t)))))))


;; skip characters until the end of double-quoted string
;; (the initial " character was already consumed)
;; do NOT consume the final "
(define (scan-lisp-double-quotes ctx)
  (let ((done? #f))
    (until done?
      (let ((ch (ctx-peek-char ctx)))
        (cond
          ((or (eqv? #\" ch) (eof-object? ch)) (set! done? #t))
          ((eqv? #\\ ch) (ctx-read-char ch)))
        (unless done?
          (ctx-read-char ctx))))))


;; Read Scheme forms from textual input port (parse-ctx-in ctx),
;; collecting grouping tokens i.e. ( ) [ ] { } " | and filling paren with them.
;;
;; If a parser directive #!... is found, switch to the corresponding parser
;; until the end of current group.
;;
;; Stops on end-of-file, or when a closing token matching the opening token
;; (parens-token paren) is found. Such closing token is consumed too.
;;
;; Return the updated parser to use.
(define (parse-lisp-parens ctx paren parser)
  (assert (parse-ctx? ctx))
  (assert (parens? paren))
  (assert (parser? parser))
  (let* ((start (parens-token paren))
         (end (case start ((#\() #\)) ((#\[) #\]) (else #f)))
         (pos (parse-ctx-pos ctx))
         (done? #f)
         (fill-end? #t)
         (%paren-fill-end! (lambda (paren)
           (parens-end-x-set! paren (fx1- (car pos)))
           (parens-end-y-set! paren (cdr pos)))))
    (until done?
      (let ((token (scan-lisp-parens-or-directive ctx)))
        (cond
          ((symbol? token)
             ; switch to other parser until end of current list
             (set! parser (get-parser ctx 'shell (paren-caller-for parser)))
             (set! done? #t)
             (set! fill-end? #f))

          ((memv token '(#\( #\[ #\{ #\" #\|))           #| make vscode happy: #\) |#
             ; found opening token, create and fill nested parens object
             (let ((inner (make-parens (parser-name parser) token)))
               (parens-start-x-set! inner (fx1- (car pos)))
               (parens-start-y-set! inner (cdr pos))
               (parens-inner-append! paren inner)
               (cond
                 ((eqv? token #\{)
                   ; switch to shell parser until matching #\}
                   (let ((other-parser (get-parser ctx 'shell (paren-caller-for parser))))
                     ((parser-parse-parens other-parser) ctx inner)))
                 ((eqv? token #\")
                   ; parse "some string"
                   (scan-lisp-double-quotes ctx) ; skip content of double-quoted string
                   (ctx-read-char ctx)           ; consume final double quotes
                   (%paren-fill-end! inner))
                 ((eqv? token #\|)
                   ; parse |identifier|
                   (ctx-skip-until-char ctx #\|)
                   (%paren-fill-end! inner))
                 (#t
                   ; recursion: call lisp parser on nested list
                   (parse-lisp-parens ctx inner parser)))))

           ((eqv? token end) ; found matching close token
              (set! done? #t))

           ((eof-object? token)
             (set! done? #t))

           (#t ; ignore unexpected token
              #f))))

    (when fill-end?
      (%paren-fill-end! paren))
    parser))


(define (paren-caller-for parser)
  (if (eq? (parser-name parser) 'r6rs)
    'parse-r6rs-parens
    'parse-scheme-parens))

) ; close library
