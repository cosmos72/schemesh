;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit parser (0 1))
  (export
    make-parsectx make-parsectx* parsectx? string->parsectx
    parsectx-in parsectx-current-pos parsectx-previous-pos parsectx-enabled-parsers

    make-parser parser?
    parser-name parser-parse-forms parser-parse-paren
    get-parser-or-false get-parser to-parser

    parsectx-peek-char parsectx-read-char parsectx-unread-char parsectx-skip-whitespace
    parsectx-skip-line parsectx-skip-until-char
    try-read-parser-directive

    syntax-errorf)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) fx1+ fx1- make-format-condition record-writer unread-char void)
    (only (schemesh bootstrap) assert* until while)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (schemesh containers span)
    (schemesh containers charspan))


;; parser is an object containing four procedures:
;;   parser-parse-forms will parse multiple forms, and return them
;;   parser-parse-paren will scan a list of forms and return matching parentheses/brackets/braces/quotes
(define-record-type
  (parser %make-parser parser?)
  (fields
    name
    parse-forms
    parse-paren)
  (nongenerative #{parser cd39kg38a9c4cnwzwhghs827-24}))


;; create a new parser
(define (make-parser name parse-forms parse-paren)
  (assert* 'make-parser (symbol?    name))
  (assert* 'make-parser (procedure? parse-forms))
  (assert* 'make-parser (procedure? parse-paren))
  (%make-parser name parse-forms parse-paren))


;; Find and return the parser corresponding to given parser-name (which must be a symbol)
;; in enabled-parsers.
;;
;; Argument pctx must be one of: #f, a parsectx or a hashtable symbol -> parser
;;
;; Return #f if not found.
(define (get-parser-or-false pctx parser-name)
  (let ((enabled-parsers
           (if (parsectx? pctx) (parsectx-enabled-parsers pctx) pctx)))
    (and enabled-parsers
         (hashtable-ref enabled-parsers parser-name #f))))


;; Find and return the parser corresponding to given parser-name (which must be a symbol)
;; in enabled-parsers.
;;
;; Argument pctx must be one of: #f, a parsectx or a hashtable symbol -> parser
;;
;; Raise (syntax-errorf pctx who ...) if not found.
(define (get-parser pctx parser-name who)
  (let ((parser (get-parser-or-false pctx parser-name)))
    (unless parser
      (syntax-errorf pctx who "no parser found for directive ~a"
        (string-append "#!" (symbol->string parser-name))))
    parser))


;; Convert a parser name to parser:
;; if p is a parser, return p
;; if p is a symbol, return (get-parser pctx p who)
;; otherwise raise (syntax-errorf pctx who ...)
;;
;; Argument pctx must be one of: #f, a parsectx or a hashtable name -> parser
(define (to-parser pctx p who)
  (if (parser? p)
    p
    (get-parser pctx p who)))


;; parsectx contains arguments common to most parsing procedures contained in a parser:
;;   parsectx-in is the textual input port to read from
;;   parsectx-pos is a pair (x . y) representing the current position in the input port
;;   parsectx-enabled-parsers is #f or an hashtable symbol -> parser containing enabled parsers,
;;     see (parsers) in parser/parser.ss
(define-record-type
  (parsectx %make-parsectx parsectx?)
  (fields
    in           ; textual input port to read from
    width        ; fixnum, screen width
    prompt-end-x ; fixnum, column where prompt ends
    pos          ; pair (x . y) containing two fixnums: current x and y position in the input port
    prev-pos     ; pair (x . y) containing two fixnums: previous x and y position in the input port
    pprev-pos    ; pair (x . y) containing two fixnums: previous previous x and y position in the input port
    enabled-parsers) ; #f or an hashtable symbol -> parser
  (nongenerative #{parsectx ghczmwc88jnt51nkrv9gaocnv-423}))


;; create a new parsectx. Arguments are
;;   in: mandatory, the textual input port to read from
;;   enabled-parsers: optional, #f or an hashtable name -> parser containing enabled parsers.
;;                    see (parsers) in parser/parser.ss
;;   width: optional, a fixnum representing screen width
;;   prompt-end-x: optional, a fixnum representing column where prompt ends
;;   x: optional, a fixnum representing the initial x position in the input port
;;   y: optional, a fixnum representing the initial y position in the input port
(define make-parsectx
  (case-lambda
    ((in)                                        (make-parsectx* in #f              (greatest-fixnum) 0 0 0))
    ((in enabled-parsers)                        (make-parsectx* in enabled-parsers (greatest-fixnum) 0 0 0))
    ((in enabled-parsers width)                  (make-parsectx* in enabled-parsers width 0 0 0))
    ((in enabled-parsers width prompt-end-x)     (make-parsectx* in enabled-parsers width prompt-end-x 0 0))
    ((in enabled-parsers width prompt-end-x x)   (make-parsectx* in enabled-parsers width prompt-end-x x 0))
    ((in enabled-parsers width prompt-end-x x y) (make-parsectx* in enabled-parsers width prompt-end-x x y))))


;; create a new parsectx. Arguments are the same as (make-parsectx)
;; with the difference that they are all mandatory
(define (make-parsectx* in enabled-parsers width prompt-end-x x y)
  (assert* 'make-parsectx* (input-port? in))
  (assert* 'make-parsectx* (textual-port? in))
  (assert* 'make-parsectx* (fixnum? width))
  (assert* 'make-parsectx* (fixnum? prompt-end-x))
  (assert* 'make-parsectx* (fixnum? x))
  (assert* 'make-parsectx* (fixnum? y))
  (assert* 'make-parsectx* (fx>? width 0))
  (assert* 'make-parsectx* (fx>=? prompt-end-x 0))
  (assert* 'make-parsectx* (fx>=? x 0))
  (assert* 'make-parsectx* (fx>=? y 0))
  (when enabled-parsers
    (hashtable-iterate enabled-parsers
      (lambda (cell)
        (let ((name  (car cell))
              (parser (cdr cell)))
          (assert* 'make-parsectx* (symbol? name))
          (assert* 'make-parsectx* (parser? parser))))))
  (%make-parsectx in width prompt-end-x (cons x y) (cons -1 -1) (cons -1 -1) enabled-parsers))


;; create a new parsectx. Arguments are
;;   str: mandatory, the string to read from
;;   enabled-parsers: optional, #f or an hashtable name -> parser containing enabled parsers.
;;                    see (parsers) in parser/parser.ss
(define string->parsectx
  (case-lambda
    ((str)                 (make-parsectx (open-string-input-port str)))
    ((str enabled-parsers) (make-parsectx (open-string-input-port str) enabled-parsers))))


;; return two values: parsectx current position x and y
(define (parsectx-current-pos pctx)
  (let ((pos (parsectx-pos pctx)))
    (values (car pos) (cdr pos))))


;; return two values: parsectx position x - delta and y,
;; i.e. delta character to the left of current position,
;; which may wrap around to previous lines.
;;
;; Implementation limit: delta must be 0, 1 or 2
(define (parsectx-previous-pos pctx delta)
  (let ((pair (case delta
                ((0) (parsectx-pos pctx))
                ((1) (parsectx-prev-pos pctx))
                ((2) (parsectx-pprev-pos pctx))
                (else (assert* 'parsectx-previous-pos (fx<=? 0 delta 2))))))
    (values (car pair) (cdr pair))))


(define (parsectx-width-at-y pctx y)
  (let ((width (parsectx-width pctx)))
    (if (fxzero? y)
      (fx- width (parsectx-prompt-end-x pctx))
      width)))

;; update parsectx position (x . y) after reading ch from textual input port
(define (parsectx-increment-pos pctx ch)
  (when (char? ch) ; do not advance after reading #!eof
    (let* ((pos (parsectx-pos pctx))
           (x   (car pos))
           (y   (cdr pos)))
      (cond
        ((or (char=? ch #\newline)
             (fx>=? (fx+ (fx1+ x) (if (fxzero? y) (parsectx-prompt-end-x pctx) 0))
                    (parsectx-width pctx)))
          ; newline or screen width wraparound -> set x to 0, increment y
          (parsectx-pos-set! pctx 0 (fx1+ y))
          (set-car! pos 0)
          (set-cdr! pos (fx1+ y)))
        (else
          (parsectx-pos-set! pctx (fx1+ x) y))))))


;; update parsectx position (x . y) after reading a character from textual input port
(define (parsectx-pos-set! pctx x y)
  (let ((pos  (parsectx-pos pctx))
        (prev (parsectx-prev-pos pctx))
        (pprev (parsectx-prev-pos pctx)))
    (set-car! pprev (car prev))
    (set-cdr! pprev (cdr prev))
    (set-car! prev (car pos))
    (set-cdr! prev (cdr pos))
    (set-car! pos x)
    (set-cdr! pos y)))


;; update parsectx position (x . y) after unreading ch from textual input port
(define (parsectx-decrement-pos pctx ch)
  (when (char? ch) ; do not rewind after reading #!eof
    (let ((pos  (parsectx-pos pctx))
          (prev (parsectx-prev-pos pctx))
          (pprev (parsectx-prev-pos pctx)))
      (set-car! pos (car prev))
      (set-cdr! pos (cdr prev))
      (set-car! prev (car pprev))
      (set-cdr! prev (cdr pprev))
      (set-car! pprev -1)
      (set-cdr! pprev -1))))


;; Peek a character from textual input port (parsectx-in pctx)
(define (parsectx-peek-char pctx)
  (assert* 'parsectx-peek-char (parsectx? pctx))
  (peek-char (parsectx-in pctx)))


;; Read a character from textual input port (parsectx-in pctx)
;;
;; also updates (parsectx-pos pctx)
(define (parsectx-read-char pctx)
  (assert* 'parsectx-read-char (parsectx? pctx))
  (let ((ch (read-char (parsectx-in pctx))))
    (parsectx-increment-pos pctx ch)
    ch))


;; Try to unread a character from textual input port (parsectx-in pctx)
;;
;; Raise condition if Chez Scheme (unread-char ch in) fails:
;; it will happen ch is different from last character read from input port,
;; or if attempting to unread multiple characters without reading them back first.
(define (parsectx-unread-char pctx ch)
  (let ((in (parsectx-in pctx)))
    (unread-char ch in)
    (assert* 'parsectx-unread-char (eqv? ch (peek-char in)))
    (parsectx-decrement-pos pctx ch)))


;; return #t if ch is a character and is <= ' '.
;; otherwise return #f
(define (is-whitespace-char? ch newline-is-whitespace?)
  (and (char? ch) (char<=? ch #\space)
       (or newline-is-whitespace? (not (char=? ch #\newline)))))


;; read and discard all initial whitespace in textual input port (parsectx-in pctx)
;; characters are considered whitespace if they are <= ' '
;;
;; also updates (parsectx-pos pctx)
(define (parsectx-skip-whitespace pctx newline-is-whitespace?)
  (while (is-whitespace-char? (parsectx-peek-char pctx) newline-is-whitespace?)
    (parsectx-read-char pctx)))


;; read and discard all characters in textual input port (parsectx-in pctx)
;; until the first occurrence of find-ch, which is discarded too and returned.
;;
;; if find-ch is found before end-of-file, return #t
;; otherwise return #f
;;
;; also updates (parsectx-pos pctx)
(define (parsectx-skip-until-char pctx find-ch)
  (let ((ret #f))
    (until ret
      (let ((ch (parsectx-read-char pctx)))
        (when (or (eqv? (eof-object) ch) (eqv? find-ch ch))
          (set! ret ch))))
    (char? ret)))


;; read and discard all characters in textual input port (parsectx-in pctx)
;; until the first #\newline, which is discarded too
;;
;; also updates (parsectx-pos pctx)
(define (parsectx-skip-line pctx)
  (parsectx-skip-until-char pctx #\newline)
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



;; Try to read a parser directive #!... from textual input port (parsectx-in pctx)
;; Does NOT skip whitespace in input port.
;;
;; If port's first two characters are a parser directive #!
;; then read the symbol after it, and return such symbol.
;;
;; Otherwise do nothing and return #f i.e. do not consume any character or part of it
(define (try-read-parser-directive pctx)
  (let ((ret #f))
    (when (eqv? #\# (parsectx-peek-char pctx))
      (parsectx-read-char pctx)
      (if (eqv? #\! (parsectx-peek-char pctx))
        (let ((csp (charspan)))
          (charspan-reserve-back! csp 10)
          (parsectx-read-char pctx)
          (while (is-simple-identifier-char? (parsectx-peek-char pctx))
            (charspan-insert-back! csp (parsectx-read-char pctx)))
          (set! ret (string->symbol (charspan->string csp))))
        (parsectx-unread-char pctx #\#)))
    ret))


;; Raise a condition describing a syntax error
(define (syntax-errorf pctx who format-string . format-args)
  (raise
    (if (parsectx? pctx)
      (condition
        (make-lexical-violation)
        (make-i/o-read-error)
        (make-who-condition who)
        (make-format-condition)
        (make-i/o-port-error (parsectx-in pctx))
        (make-message-condition (string-append format-string " at line ~a, char ~a of ~a"))
        (make-irritants-condition
          (append format-args
            (list (fx1+ (cdr (parsectx-pos pctx)))
                  (car (parsectx-pos pctx))
                  (parsectx-in pctx)))))
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


;; customize how "parsectx" objects are printed
(record-writer (record-type-descriptor parsectx)
  (lambda (obj port writer)
    (display "#<parsectx " port)
    (display (car (parsectx-pos obj)) port)
    (display " " port)
    (display (cdr (parsectx-pos obj)) port)
    (display ">" port)))

) ; close library
