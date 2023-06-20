;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser base (0 1))
  (export
    make-parser parser? parser-name parser-parse parser-parse* parser-parse-list
    get-parser to-parser skip-whitespace try-unread-char try-read-parser-directive)
  (import
    (rnrs)
    (only (chezscheme) record-writer unread-char)
    (only (schemesh bootstrap) while)
    (schemesh containers charspan))

; parser is an object containing two functions:
;   parser-parse will parse a single form,
;   parser-parse-list will parse a list of forms.
(define-record-type
  (parser %make-parser parser?)
  (fields name parse parse* parse-list)
  (nongenerative #{parser cd39kg38a9c4cnwzwhghs827-24}))

; create a new parser
(define (make-parser name parse parse* parse-list)
  (assert (symbol?    name))
  (assert (procedure? parse))
  (assert (procedure? parse*))
  (assert (procedure? parse-list))
  (%make-parser name parse parse* parse-list))

;
; Find and return the parser corresponding to given parser-name (which must be a symbol)
; in enabled-parsers.
; Raise (syntax-violation caller ...) if not found.
;/
(define (get-parser parser-name enabled-parsers caller)
  (let ((parser (and enabled-parsers
                     (hashtable-ref enabled-parsers parser-name #f))))
    (unless parser
      (syntax-violation caller "no parser found for directive #!" parser-name))
    parser))

;
; Convert a parser name to parser:
; if p is a parser, return p
; if p is a symbol, return (get-parser p enabled-parsers caller)
; otherwise raise condition
;/
(define (to-parser p enabled-parsers caller)
  (if (parser? p)
    p
    (get-parser p enabled-parsers caller)))

;
; return #t if ch is a character and is <= ' '.
; otherwise return #f
;/
(define (is-whitespace-char? ch newline-is-whitespace?)
  (and (char? ch) (char<=? ch #\space)
       (or newline-is-whitespace? (not (char=? ch #\newline)))))

;
; read and discard all initial whitespace in textual input stream 'in'.
; characters are considered whitespace if they are <= ' '
;/
(define (skip-whitespace in newline-is-whitespace?)
  (while (is-whitespace-char? (peek-char in) newline-is-whitespace?)
    (read-char in)))

;
; return truthy if ch is a character whose value is a number,
; or an ASCII letter, or '_', or greater than (integer->char 127).
; Otherwise return #f
;/
(define (is-simple-identifier-char? ch)
  (and (char? ch)
       (or (and (char>=? ch #\0) (char<=? ch #\9))
           (and (char>=? ch #\A) (char<=? ch #\Z))
           (and (char>=? ch #\a) (char<=? ch #\z))
           (char=? ch #\_)
           (char>? ch #\delete))))

;
; Try to unread a character from textual input port 'in'.
;
; Raise condition if Chez Scheme (unread-char ch in) fails:
; it will happen ch is different from last character read from input port.
;/
(define (try-unread-char ch in)
  (unread-char ch in)
  (assert (eqv? ch (peek-char in))))

;
; Try to read a parser directive #!... from textual input port 'in'
; Does NOT skip whitespace in input port.
;
; If port's first two characters are a parser directive #!
; then read the symbol after it, and return such symbol.
;
; Otherwise do nothing and return #f i.e. do not consume any character or part of it.
;/
(define (try-read-parser-directive in)
  (let ((ret #f))
    (when (eqv? #\# (peek-char in))
      (read-char in)
      (if (eqv? #\! (peek-char in))
        (let ((csp (charspan)))
          (charspan-reserve-back! csp 10)
          (read-char in)
          (while (is-simple-identifier-char? (peek-char in))
            (charspan-insert-back! csp (read-char in)))
          (set! ret (string->symbol (charspan->string csp))))
        (try-unread-char #\# in)))
    ret))

; customize how "parser" objects are printed
(record-writer (record-type-descriptor parser)
  (lambda (p port writer)
    (display "#<parser " port)
    (display (parser-name p) port)
    (display ">" port)))

) ; close library
