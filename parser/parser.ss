;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh parser (0 1))
  (export
    ; base.ss
    make-parser parser? parser-name parser-parse parser-parse* parser-parse-list
    get-parser to-parser ctx-skip-whitespace ctx-unread-char try-read-parser-directive

    ; r6rs.ss
    lex-r6rs parse-r6rs parse-r6rs* parser-r6rs

    ; scheme.ss
    lex-scheme parse-scheme parse-scheme* parser-scheme

    ; shell.ss
    read-shell-char lex-shell parse-shell-word
    parse-shell parse-shell* parse-shell-list parser-shell

    ; parser.ss
    parse-form parse-form* parse-form-list parse-forms parse-parens parse-parens-from-string
    parsers)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) reverse! void)
    (only (schemesh bootstrap) until while)
    (schemesh parser base)
    (schemesh parser r6rs)
    (schemesh parser scheme)
    (schemesh parser shell))


;; Call parse-scheme, parse-shell or whatever is the parser specified as initial-parser.
;; Automatically change parser when directive #!... is found.
;;
;; Return two values: parsed form, and #t.
;; If end-of-file is reached, return (eof-object) and #f.
(define (parse-form ctx initial-parser)
  (let ((proc (parser-parse (to-parser ctx initial-parser 'parse-form))))
    (proc ctx)))


;; Call parse-scheme*, parse-shell* or whatever is the parser specified as initial-parser.
;;
;; Return parsed form.
;; Raise syntax-errorf if end-of-file is reached before completely reading a form.
(define (parse-form* ctx initial-parser)
  (let-values (((value ok) (parse-form ctx initial-parser)))
    (unless ok
      (syntax-errorf ctx 'parse-form* "unexpected end-of-file"))
    value))


;; Parse textual input port until the end of current list, using the parser specified by
;; initial-parser, and temporarily switching to other parsers if the directive #!...
;; is found in a (possibly nested) list being parsed.
;;
;; Return parsed list.
;; Raise syntax-errorf if mismatched end token is found, as for example ']' instead of ')'
(define (parse-form-list ctx begin-type already-parsed-reverse initial-parser)
  (let ((proc (parser-parse-list
                (to-parser ctx initial-parser 'parse-form-list))))
    (proc ctx begin-type already-parsed-reverse)))


;; Parse textual input port until eof, using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return two values.
;; First value is parsed forms: each element in the list is a parsed form.
;; Second value is updated parser to use.
(define (parse-forms ctx initial-parser)
  (let ((current-parser (to-parser ctx initial-parser 'parse-forms))
        (ret '())
        (again? #t))
    (while again?
      (let-values (((form ok) (parse-form ctx current-parser)))
        (if ok
          (if (parser? form)
            (set! current-parser form)
            (set! ret (cons form ret)))
          (set! again? #f))))
    (values
      (reverse! ret)
      current-parser)))


;; Parse textual input port until closing token matching start-token is found
;; (or until end-of-file if start-token is #f) using the parser specified by initial-parser,
;; and temporarily switching to other parsers every time the directive #!... is found
;; in a (possibly nested) list being parsed.
;;
;; Return a parens describing the ( [ { " ' ` | characters in input stream,
;; their position, and the position of their matching ) ] } " ' ` |
(define (parse-parens ctx start-token initial-parser)
  (let* ((current-parser (to-parser ctx initial-parser 'parse-parens))
         (current-parse-parens (parser-parse-parens current-parser)))
    (current-parse-parens ctx start-token)))


;; Simple wrapper around parse-parens, useful for testing
(define parse-parens-from-string
  (case-lambda
    ((str)                (parse-parens (make-parse-ctx-from-string str (parsers)) #f 'scheme))
    ((str initial-parser) (parse-parens (make-parse-ctx-from-string str (parsers)) #f initial-parser))))


;; Return mutable hashtable containing all known parsers.
(define parsers
  (let ((ret (make-eq-hashtable)))
    (hashtable-set! ret 'r6rs       (parser-r6rs))
    (hashtable-set! ret 'scheme     (parser-scheme))
    (hashtable-set! ret 'chezscheme (parser-scheme))
    (hashtable-set! ret 'shell      (parser-shell))
    (lambda ()
      ret)))

) ; close library
