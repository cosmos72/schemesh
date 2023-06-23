;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;
;;; Lexer and parser for r6rs Scheme syntax.
;;;
(library (schemesh parser r6rs (0 1))
  (export
    lex-r6rs parse-r6rs parse-r6rs* parser-r6rs)
  (import
    (rnrs)
    (only (chezscheme)
       box bytevector fx1+
       fxvector fxvector-set! make-fxvector
       read-token reverse!)
    (only (schemesh bootstrap) while)
    (only (schemesh containers misc) reverse*!)
    (schemesh parser base)
    (schemesh parser lisp))

; Read a single Scheme token from textual input port 'in.
; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
; in pure R6RS.
;
; Return two values: token value and its type.
(define (lex-r6rs in enabled-parsers)
  (lex-lisp in enabled-parsers 'r6rs))


; Read Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-r6rs) and construct a Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-r6rs in enabled-parsers)
  (parse-lisp in enabled-parsers 'r6rs))


; Read Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-r6rs) and construct a Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return parsed form.
; Raises syntax-violation if end of file is reached before reading a complete form.
(define (parse-r6rs* in enabled-parsers)
  (parse-lisp* in enabled-parsers 'r6rs))


; Read Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return return a list containing parsed forms.
; Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
;
; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-r6rs-list begin-type in already-parsed-reverse enabled-parsers)
  (parse-lisp-list begin-type in already-parsed-reverse enabled-parsers 'r6rs))


(define parser-r6rs
  (let ((ret (make-parser 'r6rs parse-r6rs parse-r6rs* parse-r6rs-list)))
    (lambda ()
      ret)))

) ; close library
