;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser (0 1))
  (export
    ; lineedit/parser.ss
    make-parsectx make-parsectx* parsectx?
    parsectx-skip-whitespace parsectx-unread-char try-read-parser-directive
    get-parser to-parser make-parser parser? parser-name parser-parse parser-parse* parser-parse-list

    ; r6rs.ss
    lex-r6rs parse-r6rs parse-r6rs* parser-r6rs

    ; scheme.ss
    lex-scheme parse-scheme parse-scheme* parser-scheme

    ; shell.ss
    read-shell-char lex-shell parse-shell-word
    parse-shell parse-shell* parse-shell-list parser-shell

    ; parser.ss
    parse-form parse-form* parse-forms
    parse-paren parse-paren-from-string make-parenmatcher
    parsers)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) reverse! void)
    (only (schemesh bootstrap) assert* debugf until while)
    (schemesh lineedit paren)
    (only (schemesh lineedit parenmatcher) make-custom-parenmatcher)
    (schemesh lineedit parser)
    (schemesh parser r6rs)
    (schemesh parser scheme)
    (schemesh parser shell))


;; Return mutable hashtable containing all known parsers.
(define parsers
  (let ((ret (make-eq-hashtable)))
    (hashtable-set! ret 'r6rs       (parser-r6rs))
    (hashtable-set! ret 'scheme     (parser-scheme))
    (hashtable-set! ret 'chezscheme (parser-scheme))
    (hashtable-set! ret 'shell      (parser-shell))
    (lambda ()
      ret)))


;; Call parse-scheme, parse-shell or whatever is the parser specified as initial-parser.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values: parsed form, and #t.
;; If end-of-file is reached, return (eof-object) and #f.
(define (parse-form pctx initial-parser)
  (let ((func (parser-parse (to-parser pctx initial-parser 'parse-form))))
    (func pctx)))


;; Call parse-scheme*, parse-shell* or whatever is the parser specified as initial-parser.
;;
;; Return parsed form.
;; Raise syntax-errorf if end-of-file is reached before completely reading a form.
(define (parse-form* pctx initial-parser)
  (let ((value (parse-form pctx initial-parser)))
    (when (eof-object? value)
      (syntax-errorf pctx 'parse-form* "unexpected end-of-file"))
    value))

;; Parse textual input port until eof, using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return two values.
;; First value is parsed forms: each element in the list is a parsed form.
;; Second value is updated parser to use.
(define (parse-forms pctx initial-parser)
  (let ((current-parser (to-parser pctx initial-parser 'parse-forms))
        (ret '())
        (again? #t))
    (while again?
      (let ((form (parse-form pctx current-parser)))
        (cond
          ((eof-object? form) (set! again? #f))
          ((parser? form)     (set! current-parser form))
          (else               (set! ret (cons form ret))))))
    (values
      (reverse! ret)
      current-parser)))


;; Parse textual input port (parsectx-in pctx) until closing token matching start-ch is found
;; (or until end-of-file if start-ch is #f) using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return a paren describing the ( [ { " ' ` | characters in input stream,
;; their positions, and the positions of their matching ) ] } " ' ` |
(define (parse-paren pctx start-ch initial-parser)
  (assert* 'parse-paren (parsectx? pctx))
  (let* ((current-parser (to-parser pctx initial-parser 'parse-paren))
         (current-parse-paren (parser-parse-paren current-parser)))
    (current-parse-paren pctx start-ch)))



;; Function stored by (make-parenmatcher) into created parenmatcher:
;;
;; parse textual input port (parsectx-in pctx) until end-of-file
;; for matching parenthesis and grouping tokens,
;; and return corresponding paren object
;;
;; Equivalent to (parse-paren pctx #f initial-parser)
(define (parse-paren-until-eof pctx initial-parser)
  (parse-paren pctx #f initial-parser))


;; Simple wrapper around parse-paren-until-eof, useful for testing
(define parse-paren-from-string
  (case-lambda
    ((str)                (parse-paren-until-eof (make-parsectx-from-string str (parsers)) 'scheme))
    ((str initial-parser) (parse-paren-until-eof (make-parsectx-from-string str (parsers)) initial-parser))))


;; Create a parenmatcher that uses parse-paren to find matching parenthesis and grouping tokens
(define (make-parenmatcher)
  (make-custom-parenmatcher parse-paren-until-eof))




) ; close library
