;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser base (0 1))
  (export
    make-parens parens? parens-name parens-type
    parens-start-x parens-start-x-set! parens-start-y parens-start-y-set!
    parens-end-x   parens-end-x-set!   parens-end-y   parens-end-y-set!
    parens-inner   parens-inner-append!

    make-parse-ctx parse-ctx?
    parse-ctx-in parse-ctx-pos parse-ctx-enabled-parsers

    make-parser parser?
    parser-name parser-parse parser-parse* parser-parse-list parser-match-parens
    get-parser to-parser skip-whitespace try-unread-char try-read-parser-directive)
  (import
    (rnrs)
    (only (chezscheme) record-writer unread-char)
    (only (schemesh bootstrap) while)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (schemesh containers span)
    (schemesh containers charspan))


; parens is an object containing information about the matching parentheses/brackets/braces/quotes
; in some text to be parsed
(define-record-type
  (parens %make-parens parens?)
  (fields
    name ; symbol, name of parser that created this parens object (may differ in sub-objects)
    type ; symbol, one of: lparen lbrack lbrace backquote dollar+lparen dquote squote
    (mutable start-x) ; fixnum, x position of start parenthesis/bracket/brace/quote
    (mutable start-y) ; fixnum, y position of start parenthesis/bracket/brace/quote
    (mutable end-x)   ; fixnum, x position of end parenthesis/bracket/brace/quote
    (mutable end-y)   ; fixnum, y position of end parenthesis/bracket/brace/quote
    (mutable inner))  ; #f or span of nested parens appearing between start and end
  (nongenerative #{parens e1s38b5dr3myvj5mwxrpzkl27-400}))


(define (make-parens name type)
  (assert (symbol? name))
  (assert (symbol? type))
  (%make-parens name type 0 0 0 0 #f))


; append a list of nested parens to specified parens
(define (parens-inner-append! parens . nested-parens-list)
  (unless (null? nested-parens-list)
    (list-iterate nested-parens-list
      (lambda (nested-parens)
        (assert (parens? nested-parens))))
    (let ((inner (parens-inner parens)))
      (if (span? inner)
        (apply span-insert-back! inner nested-parens-list)
        (parens-inner-set! parens (list->span nested-parens-list))))))



; parser is an object containing four procedures:
;   parser-parse and parser-parse* will parse a single form,
;   parser-parse-list will parse a list of forms,
;   parser-match-parens will scan a list of forms and find matching parentheses/brackets/braces/quotes
(define-record-type
  (parser %make-parser parser?)
  (fields
    name
    parse
    parse*
    parse-list
    match-parens)
  (nongenerative #{parser cd39kg38a9c4cnwzwhghs827-24}))


; create a new parser
(define (make-parser name parse parse* parse-list match-parens)
  (assert (symbol?    name))
  (assert (procedure? parse))
  (assert (procedure? parse*))
  (assert (procedure? parse-list))
  (assert (procedure? match-parens))
  (%make-parser name parse parse* parse-list match-parens))


; Find and return the parser corresponding to given parser-name (which must be a symbol)
; in enabled-parsers.
; Raise (syntax-violation caller ...) if not found.
(define (get-parser parser-name enabled-parsers caller)
  (let ((parser (and enabled-parsers
                     (hashtable-ref enabled-parsers parser-name #f))))
    (unless parser
      (syntax-violation caller "no parser found for directive #!" parser-name))
    parser))


; Convert a parser name to parser:
; if p is a parser, return p
; if p is a symbol, return (get-parser p enabled-parsers caller)
; otherwise raise (syntax-violation caller ...)
(define (to-parser p enabled-parsers caller)
  (if (parser? p)
    p
    (get-parser p enabled-parsers caller)))


; parse-ctx contains arguments common to most parsing procedures contained in a parser:
;   parse-ctx-in is the textual input port to read from
;   parse-ctx-pos is a pair (x . y) representing the position in the input port
;   parse-ctx-enabled-parsers is #f or an hashtable name -> parser containing enabled parsers - see (parsers) in parser/parser.ss
(define-record-type
  (parse-ctx %make-parse-ctx parse-ctx?)
  (fields
    in  ; textual input port to read from
    pos ; pair (x . y) containing two fixnums: current x and y position in the input port
    enabled-parsers) ; #f or an hashtable name -> parser
  (nongenerative #{parse-ctx ghczmwc88jnt51nkrv9gaocnv-423}))


; create a new parse-ctx. Arguments are
;   in: the textual input port to read from
;   x: a fixnum representing the initial x position in the input port
;   y: a fixnum representing the initial y position in the input port
;   enabled-parsers: #f or an hashtable name -> parser containing enabled parsers - see (parsers) in parser/parser.ss
(define (make-parse-ctx in x y enabled-parsers)
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
  (%make-parse-ctx in (cons x y) enabled-parsers))


; return #t if ch is a character and is <= ' '.
; otherwise return #f
;/
(define (is-whitespace-char? ch newline-is-whitespace?)
  (and (char? ch) (char<=? ch #\space)
       (or newline-is-whitespace? (not (char=? ch #\newline)))))

;
; read and discard all initial whitespace in textual input port 'in'.
; characters are considered whitespace if they are <= ' '
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
