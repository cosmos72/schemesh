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
    (schemesh lineedit parser)
    (schemesh parser lisp))

; Read a single r6rs Scheme token from textual input port 'in.
; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
; in pure R6RS.
;
; Return two values: token value and its type.
(define (lex-r6rs ctx)
  (lex-lisp ctx 'r6rs))


; Read r6rs Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-r6rs) and construct a r6rs Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-r6rs ctx)
  (parse-lisp ctx 'r6rs))


; Read r6rs Scheme tokens from textual input port 'in'
; by repeatedly calling (lex-r6rs) and construct a r6rs Scheme form.
; Automatically change parser when directive #!... is found.
;
; Return parsed form.
; Raises syntax-errorf if end of file is reached before reading a complete form.
(define (parse-r6rs* ctx)
  (parse-lisp* ctx 'r6rs))


; Read r6rs Scheme forms from textual input port 'in', until a token ) or ] or } matching
; the specified begin-type token is found.
; Automatically change parser when directive #!... is found.
;
; Return a list containing parsed forms.
; Raise syntax-errorf if mismatched end token is found, as for example ']' instead of ')'
;
; The argument already-parsed-reverse will be reversed and prefixed to the returned list.
(define (parse-r6rs-list ctx begin-type already-parsed-reverse)
  (parse-lisp-list ctx begin-type already-parsed-reverse 'r6rs))


;; Read r6rs Scheme forms from textual input port (parsectx-in ctx),
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
(define (parse-r6rs-paren ctx start-ch)
  (parse-lisp-paren ctx start-ch 'r6rs))


(define parser-r6rs
  (let ((ret (make-parser 'r6rs parse-r6rs parse-r6rs* parse-r6rs-list parse-r6rs-paren)))
    (lambda ()
      ret)))

) ; close library
