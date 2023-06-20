;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh parser scheme (0 1))
  (export
    lex-scheme parse-scheme parse-scheme* parser-scheme)
  (import
    (rnrs)
    (only (chezscheme)
       box bytevector fx1+
       fxvector fxvector-set! make-fxvector
       read-token reverse!)
    (only (schemesh bootstrap) while)
    (only (schemesh containers misc) reverse*!)
    (schemesh parser base))

; Read a single Scheme token from textual input port 'in.
; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
; in pure R6RS.
;
; Return two values: token value and its type.
(define (lex-scheme in enabled-parsers)
  (skip-whitespace in 'also-skip-newlines)
  (let ((value (try-read-parser-directive in)))
    (if (symbol? value)
      (if (eq? 'eof value)
        ; yes, #!eof is an allowed directive:
        ; it injects (eof-object) in token stream, with type 'eof
        ; thus simulating an actual end-of-file in input port.
        ; Reason: historically used to disable the rest of a file, to help debugging
        (values (eof-object) 'eof)
        ; cannot switch to other parser here: just return it and let caller switch
        (values (get-parser value enabled-parsers 'parse-scheme) 'parser))
      ; read a single token with Chez Scheme (read-token),
      ; then replace (values '{ 'atomic) with (values #f 'lbrace)
      ; and replace (values '} 'atomic) with (values #f 'rbrace)
      ; because we use them to switch to shell parser. For example,
      ;    {ls -l > log.txt}
      ; is equivalent to
      ;    (#!shell ls -l > log.txt)
      (let-values (((type value start end) (read-token in)))
        (if (eq? 'atomic type)
          (case value
            (({)  (values #f 'lbrace))
            ((})  (values #f 'rbrace))
            (else (values value type)))
          (values value type))))))

;
; Return the symbol, converted to string,
; of most token types returned by Chez Scheme (read-token),
;
; Also recognizes and converts to string the additional types
; 'lbrace and 'rbrace introduced by (lex-scheme)
(define (lex-type->string type)
  (case type
    ((box) "#&")   ((dot) ".")    ((fasl) "#@")  ((insert) "#N#")
    ((lbrace) "{") ((lbrack) "[") ((lparen) "(") ((mark) "#N=") ((quote) "'")
    ((rbrace) "}") ((rbrack) "]") ((rparen) ")") ((record-brack) "#[")
    ((vfxnparen) "#Nvfx") ((vfxparen) "#vfx")
    ((vnparen)   "#Nv")   ((vparen)   "#v")
    ((vu8nparen) "#Nvu8") ((vu8paren) "#vu8")
    (else "???")))
;
; Read Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-scheme) and construct a Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-scheme in enabled-parsers)
  (let-values (((value type) (lex-scheme in enabled-parsers)))
    (let-values (((ret-value ret-type) (parse-scheme-impl value type in enabled-parsers)))
      (values ret-value (not (eq? 'eof ret-type))))))

;
; Read Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-scheme) and construct a Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return parsed form.
; Raises syntax-violation if end of file is reached before reading a complete form.
(define (parse-scheme* in enabled-parsers)
  (let-values (((value ok) (parse-scheme in enabled-parsers)))
;   cannot switch to other parser here, and caller does not expect it => raise
    (unless ok
      (syntax-violation 'parse-scheme "unexpected end-of-file" 'eof))
    (when (parser? value)
      (syntax-violation 'parse-scheme "parser directive #!... can only appear in lists, not in single-form contexts: #!" (parser-name value)))
    value))

;
; Common back-end of (parse-scheme) and (parse-scheme*)
; Read Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-scheme) and construct a Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return two values: the parsed form, and its type.
(define (parse-scheme-impl value type in enabled-parsers)
  (values
    (case type
      ; cannot switch to other parser here: just return it and let caller switch
      ((atomic eof parser) value)
      ((box)               (list 'box  (parse-scheme* in enabled-parsers)))
      ; if type = 'quote, value can be one of:
      ;    'quote  'quasiquote  'unquote  'unquote-splicing
      ;    'synyax 'quasisyntax 'unsyntax 'unsyntax-splicing
      ((quote)             (list value (parse-scheme* in enabled-parsers)))
      ((lbrack lparen)     (parse-scheme-list type in '() enabled-parsers))
      ; lbrace i.e. { switches to shell parser until corresponding rbrace i.e. }
      ((lbrace)
        (let ((other-parse-list (parser-parse-list
                (get-parser 'shell enabled-parsers 'parse-scheme))))
          (other-parse-list type in '() enabled-parsers)))
      ; parse the various vector types, with or without explicit length
      ((vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)
        (parse-vector type value in enabled-parsers))
      ; TODO: ((record-brack) ... )
      (else   (syntax-violation 'parse-scheme "unexpected token type" type)))
    type))

;
; Read Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return return a list containing parsed forms.
; Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
;
; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-scheme-list begin-type in already-parsed-reverse enabled-parsers)
  (let* ((ret already-parsed-reverse)
         (again? #t)
         (reverse? #t)
         (end-type (case begin-type
                     ((lbrace) 'rbrace) ((lbrack) 'rbrack) (else 'rparen)))
         (check-list-end (lambda (type)
           (unless (eq? type end-type)
             (syntax-violation
               'parse-scheme
               (string-append "unexpected token " (lex-type->string type)
                  ", expecting " (lex-type->string end-type))
               type)))))
    (while again?
      (let-values (((value type) (lex-scheme in enabled-parsers)))
        (case type
          ((eof)
            (syntax-violation 'parse-scheme "unexpected" type))
          ((parser)
            ; switch to other parser until the end of current list
            (let ((other-parse-list (parser-parse-list value)))
              (set! ret (other-parse-list begin-type in ret enabled-parsers))
              (set! reverse? #f)
              (set! again? #f)))
          ((rparen rbrack rbrace)
            (check-list-end type)
            (set! again? #f))
          ((dot)
            (let-values (((value-i type-i) (parse-scheme in enabled-parsers)))
              (when (eq? 'parser type-i)
                ; switch to other parser
                (let ((other-parse* (parser-parse* value-i)))
                  (set! value-i (other-parse* in enabled-parsers))))
              (set! ret (reverse*! (cons value-i ret)))
              (set! reverse? #f)
              (set! again? #f))
            ; then parse ) or ] or }
            (let-values (((value type) (lex-scheme in enabled-parsers)))
              (check-list-end type)))
          (else
            ; parse a single form and append it
            (let-values (((value-i type-i)
                            (parse-scheme-impl value type in enabled-parsers)))
              (when (eq? 'eof type-i)
                (syntax-violation 'parse-scheme "unexpected" type-i))
              (set! ret (cons value-i ret)))))))
    (if reverse? (reverse! ret) ret)))

;
; Read Scheme forms from textual input port 'in' until a token ) or ] or } matching vec-type
; is found.
; Automatically change parser when directive #!... is found.
;
; Return a vector, fxvector or bytevector containing parsed forms.
; Raise syntax-violation if mismatched end token is found, as for example ] instead of )
(define (parse-vector vec-type length in enabled-parsers)
  (let ((values (parse-scheme-list vec-type in '() enabled-parsers)))
    (case vec-type
      ((vfnxparen) (create-fxvector   length values))
      ((vnparen)   (create-vector     length values))
      ((vu8nparen) (create-bytevector length values))
      ((vfxparen)  (apply fxvector   values))
      ((vparen)    (apply vector     values))
      ((vu8paren)  (apply bytevector values))
      (else  (syntax-violation 'parse-scheme "unexpected" vec-type)))))

(define (create-fxvector length values)
  (let ((vec (make-fxvector length))
        (elem (if (null? values) 0 (car values))))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i length) vec)
      (fxvector-set! vec i elem)
      (unless (null? values)
        ; if we run out of values, fill remainder with last element in values
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
        ; if we run out of values, fill remainder with last element in values
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
        ; if we run out of values, fill remainder with last element in values
        (set! values (cdr values))
        (unless (null? values)
          (set! elem (car values)))))))

(define parser-scheme
  (let ((ret (make-parser 'scheme parse-scheme parse-scheme* parse-scheme-list)))
    (lambda ()
      ret)))

) ; close library
