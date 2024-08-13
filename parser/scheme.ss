;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;
;;; Lexer and parser for Chez Scheme syntax.
;;;
(library (schemesh parser scheme (0 1))
  (export
    lex-scheme parse-scheme1 parse-scheme-forms parser-scheme)
  (import
    (rnrs)
    (only (chezscheme)
       box bytevector fx1+
       fxvector fxvector-set! make-fxvector
       read-token reverse!)
    (only (schemesh bootstrap) while)
    (schemesh lineedit parser)
    (schemesh parser lisp))

;; Read a single Chez Scheme token from textual input port 'in.
;; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
;; in pure R6RS.
;;
;; Return two values: token value and its type.
(define (lex-scheme ctx)
  (lex-lisp ctx 'scheme))


;; Read Chez Scheme tokens from textual input port 'in'
;; by repeatedly calling (lex-scheme) and construct a Chez Scheme form.
;; Automatically change parser when directive #!... is found.
;;
;; Return parsed form.
;; Raise syntax-errorf if end-of-file is reached before completely reading a form.
(define (parse-scheme1 ctx)
  (let ((value (parse-lisp ctx 'scheme)))
    (when (eof-object? value)
      (syntax-errorf ctx 'parse-scheme "unexpected end-of-file"))
    value))



; Read Chez Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list containing parsed forms, usually prefixed by (begin ...)
; Raise syntax-errorf if mismatched end token is found, as for example ']' instead of ')'
(define (parse-scheme-forms ctx begin-type)
  (let ((ret (parse-lisp-list ctx begin-type '() 'scheme)))
    (values
      (if (or (not (pair? ret)) (eq? 'begin (car ret)) (null? (cdr ret)))
        ret
        (cons 'begin ret))
      #f)))


;; Read Chez Scheme forms from textual input port (parsectx-in ctx),
;; collecting grouping tokens i.e. ( ) [ ] { } |# #| " " | |
;; and filling paren with them.
;;
;; If a parser directive #!... is found, switch to the corresponding parser
;; until the end of current group.
;;
;; Stops on end-of-file, or when closing token matching start-ch is found.
;; Such closing token is consumed too.
;;
;; Return a paren containing the collected grouping tokens.
(define (parse-scheme-paren ctx start-ch)
  (parse-lisp-paren ctx start-ch 'scheme))


(define parser-scheme
  (let ((ret (make-parser 'scheme parse-scheme-forms parse-scheme-paren)))
    (lambda ()
      ret)))

) ; close library
