;;; Copyright (C) 2023 by Massimiliano Ghilardi
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
    lex-scheme parse-scheme parse-scheme* parser-scheme)
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

; Read a single Chez Scheme token from textual input port 'in.
; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
; in pure R6RS.
;
; Return two values: token value and its type.
(define (lex-scheme in enabled-parsers)
  (lex-lisp in enabled-parsers 'scheme))


; Read Chez Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-scheme) and construct a Chez Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-scheme in enabled-parsers)
  (parse-lisp in enabled-parsers 'scheme))


; Read Chez Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-scheme) and construct a Chez Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return parsed form.
; Raises syntax-violation if end of file is reached before reading a complete form.
(define (parse-scheme* in enabled-parsers)
  (parse-lisp* in enabled-parsers 'scheme))


; Read Chez Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list containing parsed forms.
; Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
;
; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-scheme-list begin-type in already-parsed-reverse enabled-parsers)
  (parse-lisp-list begin-type in already-parsed-reverse enabled-parsers 'scheme))


; Read Chez Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list of parens objects, each containing the position and type of
; matching parentheses/brackets/braces/quotes
; Should not raise any condition for invalid input.
;
; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-scheme-parens begin-type in pos already-parsed-reverse enabled-parsers)
  (parse-lisp-parens begin-type in pos already-parsed-reverse enabled-parsers 'scheme))


(define parser-scheme
  (let ((ret (make-parser 'scheme parse-scheme parse-scheme* parse-scheme-list parse-scheme-parens)))
    (lambda ()
      ret)))

) ; close library
