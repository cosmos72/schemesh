;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh parser (0 9 1))
  (export
    ; lisp.ss
    lex-lisp parse-lisp-forms parse-lisp-paren read-lisp-token

    ; parser.ss
    make-parsectx make-parsectx* parsectx?
    parsectx-skip-whitespace parsectx-unread-char parsectx-try-read-directive
    get-parser to-parser make-parser parser? parser-name
    parser-parse-forms parser-parse-paren

    ; r6rs.ss
    lex-r6rs parse-r6rs-forms parser-r6rs

    ; scheme.ss
    lex-scheme parse-scheme-forms1 parse-scheme-forms parser-scheme

    ; shell.ss
    read-shell-char lex-shell parse-shell-word parse-shell-form1
    parse-shell-forms parser-shell

    ; parser.ss
    parse-forms parse-forms1
    parse-paren string->paren make-parenmatcher
    parsers)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) reverse! void)
    (only (schemesh bootstrap) assert*)
    (schemesh lineedit paren)
    (only (schemesh lineedit parenmatcher) make-custom-parenmatcher)
    (schemesh lineedit parser)
    (schemesh parser lisp)
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


;; Parse textual input port until eof, using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return two values.
;; First value is list of parsed forms
;; Second value is updated parser to use.
(define (parse-forms pctx initial-parser)
  (let* ((parser (to-parser pctx initial-parser 'parse-forms))
         (func-parse-forms (parser-parse-forms parser)))
    (let-values (((form updated-parser) (func-parse-forms pctx 'eof)))
      (values form (or updated-parser parser)))))


;; Parse textual input port until eof, using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return list of parsed forms
(define (parse-forms1 pctx initial-parser)
  (let-values (((ret _) (parse-forms pctx initial-parser)))
    ret))


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
(define (parse-paren-all pctx initial-parser)
  (parse-paren pctx #f initial-parser))


;; Simple wrapper around parse-paren-all, useful for testing
(define string->paren
  (case-lambda
    ((str)                (parse-paren-all (string->parsectx str (parsers)) 'scheme))
    ((str initial-parser) (parse-paren-all (string->parsectx str (parsers)) initial-parser))))


;; Create a parenmatcher that uses parse-paren to find matching parenthesis and grouping tokens
(define (make-parenmatcher)
  (make-custom-parenmatcher parse-paren-all))




) ; close library
