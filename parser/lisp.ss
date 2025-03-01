;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;
;;; Common backend of libraries (schemesh parser r6rs) and (schemesh parser scheme)
;;;
(library (schemesh parser lisp (0 7 7))
  (export
    lex-lisp parse-lisp-forms parse-lisp-paren read-lisp-token)
  (import
    (rnrs)
    (only (chezscheme)
      append! box bytevector char-name fx1+ fx1- fxvector fxvector-set! include
      make-fxvector read-token reverse! top-level-value void)
    (only (schemesh bootstrap)            assert* debugf while until)
    (only (schemesh containers charspan)  charspan charspan-insert-right! charspan->string*!)
    (only (schemesh containers hashtable) hashtable)
    (only (schemesh containers list)      list-reverse*!)
    (only (schemesh containers string)    string-iterate)
    (only (schemesh containers utf8b)     integer->char*)
    (schemesh lineedit paren)
    (schemesh lineedit parser))


(include "parser/lisp-read-token.ss")




;; Read a single r6rs or Chez Scheme token from textual input port 'in.
;;
;; Return two values: token value and its type.
(define (lex-lisp ctx flavor)
  (parsectx-skip-whitespace ctx 'also-skip-newlines)
  (let ((value (parsectx-try-read-directive ctx)))
    (if (symbol? value)
      (if (eq? 'eof value)
        ;; yes, #!eof is an allowed directive:
        ;; it injects (eof-object) in token stream, with type 'eof
        ;; simulating an actual end-of-file in input port.
        ;; Reason: traditionally used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
        ;; cannot switch to other parser here: just return it and let caller switch
        (values (get-parser ctx value (caller-for flavor)) 'parser))
      ;; read a single token with (read-lisp-token)
      (read-lisp-token ctx flavor))))


;; Return the symbol, converted to string,
;; of most token types returned by (read-lisp-token),
;;
;; Also recognizes and converts to string the additional types
;; 'lbrace and 'rbrace introduced by (read-lisp-token)
(define (lex-type->string type)
  (case type
    ((box) "#&")   ((dot) ".")    ((fasl) "#@")  ((insert) "#N#")
    ((lbrace) "{") ((lbrack) "[") ((lparen) "(") ((mark) "#N=") ((quote) "'")
    ((rbrace) "}") ((rbrack) "]") ((rparen) ")") ((record-brack) "#[")
    ((vflnparen) "#Nvfl") ((vflparen) "#vfl")
    ((vfxnparen) "#Nvfx") ((vfxparen) "#vfx")
    ((vnparen)   "#Nv")   ((vparen)   "#v")
    ((vu8nparen) "#Nvu8") ((vu8paren) "#vu8")
    (else (symbol->string type))))


;; Read Scheme tokens from textual input port 'in'
;; by repeatedly calling (lex-lisp) and construct a Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return parsed form, or new parser to use.
;;
;; Raises syntax-errorf if end of file is reached before reading a complete form.
(define (parse-lisp ctx flavor)
  (let-values (((value type) (lex-lisp ctx flavor)))
    (parse-lisp-impl ctx value type flavor)))


;; Read Scheme tokens from textual input port 'in', assuming value (and its type) was just read,
;; repeatedly call (lex-lisp) to read further tokens, and construct a single Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return a single parsed form, or new parser to use, or (eof-object) if end-of-file is reached
(define (parse-lisp-impl ctx value type flavor)
  (case type
    ;; cannot switch to other parser here: just return it and let caller switch
    ((atomic parser) value)
    ((eof)
      (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file"))
    ((box)
      (unless (eq? 'scheme flavor)
        (syntax-errorf ctx (caller-for flavor)
          "invalid token in #!r6rs syntax, only allowed in #!scheme syntax: ~a"
          (lex-type->string type)))
      (list 'box  (parse-lisp ctx flavor)))
    ;; if type = 'quote, value can be one of:
    ;;    'quote  'quasiquote  'unquote  'unquote-splicing
    ;;    'syntax 'quasisyntax 'unsyntax 'unsyntax-splicing
    ;;    'datum-comment
    ((quote)
      (let ((next (parse-lisp ctx flavor)))
        (if (eq? 'datum-comment value)
          ; skip the whole lisp form read above,
          ; then call (parse-lisp again) and return its value
          (parse-lisp ctx flavor)
          ; quote the whole lisp form read above and return it
          (list value next))))
    ((lparen lbrack)
      (let-values (((ret _) (parse-lisp-forms ctx type flavor)))
        ret))
    ((lbrace)
      (when (eq? flavor 'r6rs)
        (syntax-errorf ctx (caller-for flavor)
          "invalid token in #!r6rs syntax, only allowed in #!scheme syntax: ~a" #\{))
      ; switch to shell parser until corresponding }
      (let ((other-parse-forms (parser-parse-forms (get-parser ctx 'shell (caller-for flavor)))))
        (let-values (((other-forms _) (other-parse-forms ctx type)))
          other-forms)))
    ;; parse the various vector types, with or without explicit length
    ((vparen vu8paren)
      (parse-vector ctx type value flavor))
    ((vflnparen vflparen vfxnparen vfxparen vnparen vu8nparen)
      (unless (eq? 'scheme flavor)
        (syntax-errorf ctx (caller-for flavor)
          "invalid token in #!r6rs syntax, only allowed in #!scheme syntax: ~a"
          (lex-type->string type)))
      (parse-vector ctx type value flavor))
    ;; TODO implement types: record-brack fasl insert mark
    (else
      (syntax-errorf ctx (caller-for flavor) "unexpected token type: ~a" type))))


;; Read Scheme forms from textual input port 'in', until a token ) or ] or } matching
;; the specified begin-type token is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values:
;; 1. list of parsed forms
;; 2. updated parser to use, or #f if parser was not changed
;;
;; At top-level (i.e. begin-type is 'eof) usually returns (form1 form2 ...)
;; i.e. a list containing multiple forms, unless a #!... directive is found
;;
;; At nested levels (i.e. if begin-type is not 'eof) usually returns (token1 token2 ...)
;; i.e. a list containing a single parsed form, unless a #!... directive is found,
;;
;; Raise syntax-errorf if a mismatched end token is found, as for example ']' when expecting ')'
(define (parse-lisp-forms ctx begin-type flavor)
  (let* ((ret '())
         (ret-parser #f)
         (again? #t)
         (reverse? #t)
         (parser   #f)
         (end-type (case begin-type
                     ((eof)    'eof)
                     ((lbrace) 'rbrace)
                     ((lbrack) 'rbrack)
                     (else 'rparen)))
         (assert-list-end-type (lambda (type)
           (unless (eq? type end-type)
             (syntax-errorf ctx (caller-for flavor) "unexpected token ~a, expecting ~a"
               (lex-type->string type) (lex-type->string end-type)))))
         (%merge! (lambda (forms)
           ; (debugf "... parse-lisp-forms > %merge! ret=~s other-forms=~s" (if reverse? (reverse ret) ret) forms)
           (cond
             ((null? ret)
                (set! ret forms))
             ((eq? 'eof end-type)
               (set! ret (append! (if reverse? (reverse! ret) ret) forms)))
             (else
               (set! ret (append! (if reverse? (reverse! ret) ret) (list forms)))))
           ; (debugf "... parse-lisp-forms < %merge! ret=~s" ret)
           (set! reverse? #f))))
    ; (debugf "->   parse-lisp-forms end-type=~s" end-type)
    (while again?
      (let-values (((value type) (lex-lisp ctx flavor)))
        ; (debugf "... parse-lisp-forms ret=~s value=~s type=~s end-type=~s" (if reverse? (reverse ret) ret) value type end-type)
        (case type
          ((eof)
            (unless (eq? type end-type)
              (syntax-errorf ctx (caller-for flavor) "unexpected end-of-file after ~a"
                (if reverse? (reverse! ret) ret)))
            (set! again? #f))
          ((parser)
            (set! parser value)
            (set! again? #f)
            ; switch to other parser until the end of current list
            (let ((other-parse-forms (parser-parse-forms value)))
              (let-values (((other-forms updated-parser) (other-parse-forms ctx begin-type)))
                (when updated-parser
                  (set! parser updated-parser))
                (%merge! other-forms))))
          ((rparen rbrack rbrace)
            (assert-list-end-type type)
            (set! again? #f))
          ((dot)
            ;; parse '.' followed by last form and matching token ) or ] or },
            ;; and create an improper list
            (let ((value (parse-lisp ctx flavor)))
              (when (parser? value)
                (syntax-errorf ctx (caller-for flavor) "unsupported syntax change directive after dot"))
              (set! ret (list-reverse*! (cons value ret)))
              (set! reverse? #f)
              (set! again? #f))
            ;; then parse ')' ']' or '}'
            (let-values (((value type) (lex-lisp ctx flavor)))
              (assert-list-end-type type)))
          (else
            ;; parse a single form and append it
            (let ((value-i (parse-lisp-impl ctx value type flavor)))
              (set! ret (cons value-i ret)))))))
    ; (debugf "<-  parse-lisp-forms ret=~s" (if reverse? (reverse ret) ret))
    (values
      (if reverse? (reverse! ret) ret)
      parser)))


;; Read Scheme forms from textual input port 'in' until a token ) or ] or } matching vec-type
;; is found.
;; Automatically change parser when directive #!... is found.
;;
;; Return a vector, fxvector or bytevector containing parsed forms.
;; Raise syntax-errorf if mismatched end token is found, as for example ] instead of )
(define (parse-vector ctx vec-type length flavor)
  (let-values (((values _) (parse-lisp-forms ctx vec-type flavor)))
    (case vec-type
      ((vflnparen) (create-flvector   length values))
      ((vfxnparen) (create-fxvector   length values))
      ((vnparen)   (create-vector     length values))
      ((vu8nparen) (create-bytevector length values))
      ((vflparen)  (apply (top-level-value 'flvector) values)) ; requires Chez Scheme >= 10.0.0
      ((vfxparen)  (apply fxvector   values))
      ((vparen)    (apply vector     values))
      ((vu8paren)  (apply bytevector values))
      (else  (syntax-errorf ctx (caller-for flavor) "unexpected ~a" vec-type)))))

;; requires Chez Scheme >= 10.0.0
(define (create-flvector length values)
  (%create-vector length values 0.0 (top-level-value 'make-flvector) (top-level-value 'flvector-set!)))

(define (create-fxvector length values)
  (%create-vector length values 0 make-fxvector fxvector-set!))

(define (create-vector length values)
  (%create-vector length values 0 make-vector vector-set!))

(define (create-bytevector length values)
  (%create-vector length values 0 make-bytevector bytevector-u8-set!))

(define (%create-vector length values default-value vector-maker vector-setter!)
  (let ((vec (vector-maker length))
        (elem (if (null? values) default-value (car values))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i length) vec)
      (vector-setter! vec i elem)
      (unless (null? values)
        ;; if we run out of values, fill remainder with last element in values
        (set! values (cdr values))
        (unless (null? values)
          (set! elem (car values)))))))


;; consume text input port until one of the characters ( ) [ ] { } | " #| and return it as a char.
;; also recognize and return parser directives #!... and return them as a symbol.
(define (scan-lisp-paren-or-directive ctx)
  ;; yes, #!eof is an allowed directive:
  ;; it injects (eof-object) in token stream, with type 'eof
  ;; thus simulating an actual end-of-file in input port.
  ;; Reason: historically used to disable the rest of a file, to help debugging
  ;;
  (let ((ret #f))
    (until ret
      (parsectx-skip-whitespace ctx 'also-skip-newlines)
      (let ((ch (parsectx-read-char ctx)))
        (case ch
          ((#\( #\) #\[ #\] #\{ #\} #\" #\|) (set! ret ch))
          ((#\#)  (set! ret (scan-lisp-sharp ctx)))
          ((#\\)  (scan-lisp-backslash ctx))
          ((#\;)  (parsectx-skip-line ctx))
          (else (when (eof-object? ch) (set! ret ch))))))
    ret))

;; recognize scheme syntax after backslash: either a single character, or x...;
(define (scan-lisp-backslash ctx)
  (when (eqv? #\x (parsectx-read-char ctx))
    (while (let ((ch (parsectx-read-char ctx)))
              (and (char? ch) (not (char=? ch #\;)))))))


;; scan a token after # (the # character was already consumed) and return it
(define (scan-lisp-sharp ctx)
  (let ((ch (parsectx-read-char ctx)))
    (case ch
      ((#\\) (parsectx-read-char ctx) #f) ; consume one char after #\
      ((#\( #\[ #\{) ch) ; treat #( #[ #{ respectively as ( [ {
      ((#\|) #\#) ; found start of block comment #| thus return #
      ((#\!)        (parsectx-read-directive ctx)) ; found parser directive
      (else #f))))


;; skip all characters until sequence |# which is consumed too
;; return #t if |# is found before end-of-file, otherwise return #f
;;
;; if nested block comments #| |# are found, append them to paren
(define (skip-lisp-block-comment ctx flavor paren)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (cond
          ((eof-object? ch)
            (set! ret ch))
          ((and (eqv? #\| ch) (eqv? #\# (parsectx-peek-char ctx)))
            (set! ret (parsectx-read-char ctx)))
          ((and (eqv? #\# ch) (eqv? #\| (parsectx-peek-char ctx)))
            (parsectx-read-char ctx) ; consume #\|
            (paren-inner-append! paren
              (parse-lisp-paren-inner ctx flavor ch))))))
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
;; collecting grouping tokens i.e. ( ) [ ] { } #| |# " " | |
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
;; Return a paren containing the collected grouping tokens.
(define (parse-lisp-paren ctx start-ch flavor)
  (assert* 'parse-lisp-paren (parsectx? ctx))
  (when start-ch
    (assert* 'parse-lisp-paren (char? start-ch)))
  (assert* 'parse-lisp-paren (symbol? flavor))
  (let* ((paren  (make-paren flavor (or start-ch #t)))
         (end-ch (case start-ch ((#\() #\)) ((#\[) #\]) ((#\{) #\})
                                (else (or start-ch #t))))
         (ret    #f))

    ; (debugf "->   parse-lisp-paren start-ch=~a" start-ch)
    (let-values (((x y) (parsectx-previous-pos ctx (if start-ch 1 0))))
      (paren-start-xy-set! paren x y))
    (until ret
      (let ((token (scan-lisp-paren-or-directive ctx)))
        ; (debugf "... parse-lisp-paren token=~s paren=~s" token paren)
        (cond
          ((not token) ; not a grouping token
             #f)

          ((symbol? token)
             ; recurse to other parser until end of current list
             (let* ((other-parser      (get-parser-or-false ctx token))
                    (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                    (other-paren       (and other-parse-paren (other-parse-paren ctx start-ch))))
               (when other-paren
                 (paren-inner-append! paren other-paren)
                 (set! ret #t))))

          ((memv token '(#\( #\[
                         #| ] ) |#             ; help vscode
                         ))
             ; recursion: call lisp parser on nested list
             (paren-inner-append! paren (parse-lisp-paren ctx token flavor)))

          ((eqv? token #\{)
             ; recursion: call shell parser on nested list
             (let* ((other-parser (get-parser-or-false ctx 'shell))
                    (other-parse-paren (and other-parser (parser-parse-paren other-parser)))
                    (other-paren (if other-parse-paren
                                   (other-parse-paren ctx token)
                                   (parse-lisp-paren ctx token flavor))))
               (when other-paren
                 (paren-inner-append! paren other-paren))))

          ((eqv? token end-ch) ; found matching close token
             (set! ret #t))

          ((memv token '( #| { [ ( |#     ; help vscode
                         #\) #\] #\}))
            ; found mismatched close token
            (let-values (((x y) (parsectx-previous-pos ctx 1)))
              (paren-inner-append! paren (make-paren/bad-close flavor token x y))))

          ((memv token '(#\" #\| #\#))
             ; found quoted string, quoted symbol or block comment.
             ; create and fill nested paren object
             (paren-inner-append! paren
               (parse-lisp-paren-inner ctx flavor token)))

          ((eof-object? token)
             (set! ret (if start-ch 'err #t)))

          ; ignore unexpected tokens, including mismatched close tokens
          )))

    (paren-fill-end! ctx paren (and (eq? #t ret) end-ch))
    paren))


(define (parse-lisp-paren-inner ctx flavor token)
  (let ((paren (make-paren flavor token)))
    ; (parsectx-previous-pos ctx 2) may not be set
    (let-values (((x y) (parsectx-previous-pos ctx 1)))
      (paren-start-xy-set! paren
        (if (and (eqv? token #\#) (fx>? x 0)) (fx1- x) x)
        y))
    (paren-fill-end! ctx paren
      (cond
        ((eqv? token #\")               ; parse "some string"
           (and (skip-lisp-double-quotes ctx) #\"))
        ((eqv? token #\|)               ; parse |identifier|
           (and (parsectx-skip-until-char ctx #\|) #\|))
        (else                           ; parse #| block comment |#
           (and (skip-lisp-block-comment ctx flavor paren) #\#))))
    paren))


(define (paren-fill-end! ctx paren end-token)
  (let-values (((x y) (if (char? end-token)
                        (parsectx-previous-pos ctx 1)
                        (parsectx-current-pos ctx))))
    (paren-end-xy-set! paren x y)
    (paren-end-token-set! paren end-token)
    ; (debugf "lisp paren-fill-end! paren=~s end-token=~s x=~s y=~s" paren end-token x y)
  ))

) ; close library
