;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit parser (0 1))
  (export
    make-parsectx make-parsectx* make-parsectx-from-string parsectx?
    parsectx-in parsectx-pos parsectx-enabled-parsers

    make-parser parser?
    parser-name parser-parse parser-parse* parser-parse-list parser-parse-parens
    get-parser to-parser

    parsectx-peek-char parsectx-read-char parsectx-unread-char parsectx-skip-whitespace
    parsectx-skip-line parsectx-skip-until-char
    try-read-parser-directive

    syntax-errorf)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) break fx1+ fx1- make-format-condition record-writer unread-char void)
    (only (schemesh bootstrap) try until while)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (schemesh containers span)
    (schemesh containers charspan))


;; parser is an object containing four procedures:
;;   parser-parse and parser-parse* will parse a single form,
;;   parser-parse-list will parse a list of forms,
;;   parser-parse-parens will scan a list of forms and return matching parentheses/brackets/braces/quotes
(define-record-type
  (parser %make-parser parser?)
  (fields
    name
    parse
    parse*
    parse-list
    parse-parens)
  (nongenerative #{parser cd39kg38a9c4cnwzwhghs827-24}))


;; create a new parser
(define (make-parser name parse parse* parse-list parse-parens)
  (assert (symbol?    name))
  (assert (procedure? parse))
  (assert (procedure? parse*))
  (assert (procedure? parse-list))
  (assert (procedure? parse-parens))
  (%make-parser name parse parse* parse-list parse-parens))


;; Find and return the parser corresponding to given parser-name (which must be a symbol)
;; in enabled-parsers.
;;
;; Argument ctx must be one of: #f, a parsectx or a hashtable symbol -> parser
;;
;; Raise (syntax-errorf ctx who ...) if not found.
(define (get-parser ctx parser-name who)
  (let* ((enabled-parsers
           (if (parsectx? ctx) (parsectx-enabled-parsers ctx) ctx))
         (parser (and enabled-parsers
                     (hashtable-ref enabled-parsers parser-name #f))))
    (unless parser
      (syntax-errorf ctx who "no parser found for directive ~a"
        (string-append "#!" (symbol->string parser-name))))
    parser))


;; Convert a parser name to parser:
;; if p is a parser, return p
;; if p is a symbol, return (get-parser ctx p who)
;; otherwise raise (syntax-errorf ctx who ...)
;;
;; Argument ctx must be one of: #f, a parsectx or a hashtable name -> parser
(define (to-parser ctx p who)
  (if (parser? p)
    p
    (get-parser ctx p who)))


;; parsectx contains arguments common to most parsing procedures contained in a parser:
;;   parsectx-in is the textual input port to read from
;;   parsectx-pos is a pair (x . y) representing the position in the input port
;;   parsectx-enabled-parsers is #f or an hashtable symbol -> parser containing enabled parsers,
;;     see (parsers) in parser/parser.ss
(define-record-type
  (parsectx %make-parsectx parsectx?)
  (fields
    in    ; textual input port to read from
    pos   ; pair (x . y) containing two fixnums: current x and y position in the input port
    enabled-parsers) ; #f or an hashtable symbol -> parser
  (nongenerative #{parsectx ghczmwc88jnt51nkrv9gaocnv-423}))


;; create a new parsectx. Arguments are
;;   in: mandatory, the textual input port to read from
;;   enabled-parsers: optional, #f or an hashtable name -> parser containing enabled parsers.
;;                    see (parsers) in parser/parser.ss
;;   x: optional, a fixnum representing the initial x position in the input port
;;   y: optional, a fixnum representing the initial y position in the input port
(define make-parsectx
  (case-lambda
    ((in)               (make-parsectx* in #f 0 0))
    ((in enabled-parsers) (make-parsectx* in enabled-parsers 0 0))
    ((in enabled-parsers x) (make-parsectx* in enabled-parsers x 0))
    ((in enabled-parsers x y) (make-parsectx* in enabled-parsers x y))))


;; create a new parsectx. Arguments are the same as (make-parsectx)
;; with the difference that they are all mandatory
(define (make-parsectx* in enabled-parsers x y)
  (assert (input-port? in))
  (assert (textual-port? in))
  (assert (fixnum? x))
  (assert (fixnum? y))
  (when enabled-parsers
    (hashtable-iterate enabled-parsers
      (lambda (cell)
        (let ((name  (car cell))
              (parser (cdr cell)))
          (assert (symbol? name))
          (assert (parser? parser))))))
  (%make-parsectx in (cons x y) enabled-parsers))


;; create a new parsectx. Arguments are
;;   str: mandatory, the string to read from
;;   enabled-parsers: optional, #f or an hashtable name -> parser containing enabled parsers.
;;                    see (parsers) in parser/parser.ss
(define make-parsectx-from-string
  (case-lambda
    ((str)                 (make-parsectx* (open-string-input-port str) #f 0 0))
    ((str enabled-parsers) (make-parsectx* (open-string-input-port str) enabled-parsers 0 0))))


;; update parsectx position (x . y) after reading ch from textual input port
(define (parsectx-increment-pos ctx ch)
  (when (char? ch) ; do not advance after reading #!eof
    (let ((pos (parsectx-pos ctx)))
      (if (char=? ch #\newline)
        (begin ; newline -> set x to 0, increment y
          (set-car! pos 0)
          (set-cdr! pos (fx1+ (cdr pos))))
        ; only increment x
        (set-car! pos (fx1+ (car pos)))))))


;; update parsectx position (x . y) after unreading ch from textual input port
(define (parsectx-decrement-pos ctx ch)
  (when (char? ch) ; do not rewind after reading #!eof
    (let ((pos (parsectx-pos ctx)))
      (if (char=? ch #\newline)
        (begin ; newline -> set x to (greatest-fixnum), decrement y
          (set-car! pos (greatest-fixnum))
          (set-cdr! pos (fx1- (cdr pos))))
        ; only decrement x
        (set-car! pos (fx1- (car pos)))))))


;; Peek a character from textual input port (parsectx-in ctx)
(define (parsectx-peek-char ctx)
  (assert (parsectx? ctx))
  (peek-char (parsectx-in ctx)))


;; Read a character from textual input port (parsectx-in ctx)
;;
;; also updates (parsectx-pos ctx)
(define (parsectx-read-char ctx)
  (assert (parsectx? ctx))
  (let ((ch (read-char (parsectx-in ctx))))
    (parsectx-increment-pos ctx ch)
    ch))


;; Try to unread a character from textual input port (parsectx-in ctx)
;;
;; Raise condition if Chez Scheme (unread-char ch in) fails:
;; it will happen ch is different from last character read from input port,
;; or if attempting to unread multiple characters without reading them back first.
(define (parsectx-unread-char ctx ch)
  (let ((in (parsectx-in ctx)))
    (unread-char ch in)
    (assert (eqv? ch (peek-char in)))
    (parsectx-decrement-pos ctx ch)))


;; return #t if ch is a character and is <= ' '.
;; otherwise return #f
(define (is-whitespace-char? ch newline-is-whitespace?)
  (and (char? ch) (char<=? ch #\space)
       (or newline-is-whitespace? (not (char=? ch #\newline)))))


;; read and discard all initial whitespace in textual input port (parsectx-in ctx)
;; characters are considered whitespace if they are <= ' '
;;
;; also updates (parsectx-pos ctx)
(define (parsectx-skip-whitespace ctx newline-is-whitespace?)
  (while (is-whitespace-char? (parsectx-peek-char ctx) newline-is-whitespace?)
    (parsectx-read-char ctx)))


;; read and discard all characters in textual input port (parsectx-in ctx)
;; until the first occurrence of find-ch, which is discarded too and returned.
;;
;; if end-of-file is reached before the first occurence of find-ch,
;; return (eof-object)
;;
;; also updates (parsectx-pos ctx)
(define (parsectx-skip-until-char ctx find-ch)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char ctx)))
        (when (or (eqv? (eof-object) ch) (eqv? find-ch ch))
          (set! ret ch))))
    ret))


;; read and discard all characters in textual input port (parsectx-in ctx)
;; until the first #\newline, which is discarded too
;;
;; also updates (parsectx-pos ctx)
(define (parsectx-skip-line ctx)
  (parsectx-skip-until-char ctx #\newline)
  (void))


;; return truthy if ch is a character whose value is a number,
;; or an ASCII letter, or '_', or greater than (integer->char 127).
;; Otherwise return #f
(define (is-simple-identifier-char? ch)
  (and (char? ch)
       (or (and (char>=? ch #\0) (char<=? ch #\9))
           (and (char>=? ch #\A) (char<=? ch #\Z))
           (and (char>=? ch #\a) (char<=? ch #\z))
           (char=? ch #\_)
           (char>? ch #\delete))))



;; Try to read a parser directive #!... from textual input port (parsectx-in ctx)
;; Does NOT skip whitespace in input port.
;;
;; If port's first two characters are a parser directive #!
;; then read the symbol after it, and return such symbol.
;;
;; Otherwise do nothing and return #f i.e. do not consume any character or part of it
(define (try-read-parser-directive ctx)
  (let ((ret #f))
    (when (eqv? #\# (parsectx-peek-char ctx))
      (parsectx-read-char ctx)
      (if (eqv? #\! (parsectx-peek-char ctx))
        (let ((csp (charspan)))
          (charspan-reserve-back! csp 10)
          (parsectx-read-char ctx)
          (while (is-simple-identifier-char? (parsectx-peek-char ctx))
            (charspan-insert-back! csp (parsectx-read-char ctx)))
          (set! ret (string->symbol (charspan->string csp))))
        (parsectx-unread-char ctx #\#)))
    ret))


;; Raise a condition describing a syntax error
(define (syntax-errorf ctx who format-string . format-args)
  (raise
    (if (parsectx? ctx)
      (condition
        (make-lexical-violation)
        (make-i/o-read-error)
        (make-who-condition who)
        (make-format-condition)
        (make-i/o-port-error (parsectx-in ctx))
        (make-message-condition (string-append format-string " at line ~a, char ~a of ~a"))
        (make-irritants-condition
          (append format-args
            (list (fx1+ (cdr (parsectx-pos ctx)))
                  (car (parsectx-pos ctx))
                  (parsectx-in ctx)))))
      (condition
        (make-lexical-violation)
        (make-i/o-read-error)
        (make-who-condition who)
        (make-format-condition)
        (make-message-condition format-string)
        (make-irritants-condition format-args)))))


;; customize how "parser" objects are printed
(record-writer (record-type-descriptor parser)
  (lambda (obj port writer)
    (display "#<parser " port)
    (display (parser-name obj) port)
    (display ">" port)))

) ; close library