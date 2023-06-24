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
    get-parser to-parser skip-whitespace try-unread-char try-read-parser-directive

    ; r6rs.ss
    lex-r6rs parse-r6rs parse-r6rs* parser-r6rs

    ; scheme.ss
    lex-scheme parse-scheme parse-scheme* parser-scheme

    ; shell.ss
    read-shell-char lex-shell parse-shell-word
    parse-shell parse-shell* parse-shell-list parser-shell

    ; parser.ss
    parse-form parse-form* parse-form-list parse-forms parsers)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) reverse! void)
    (only (schemesh bootstrap) while)
    (schemesh parser base)
    (schemesh parser r6rs)
    (schemesh parser scheme)
    (schemesh parser shell))


; Call parse-scheme, parse-shell or whatever is the parser specified as initial-parser.
; Automatically change parser when directive #!... is found.
;
; Return two values: parsed form, and #t.
; If end-of-file is reached, return (eof-object) and #f.
(define (parse-form ctx initial-parser)
  (let* ((enabled-parsers (parse-ctx-enabled-parsers ctx))
         (parser (to-parser initial-parser enabled-parsers 'parse-form))
         (proc (parser-parse parser)))
    (proc ctx)))


; Call parse-scheme*, parse-shell* or whatever is the parser specified as initial-parser.
;
; Return parsed form.
; Raise syntax-violation if end-of-file is reached before completely reading a form.
(define (parse-form* ctx initial-parser)
  (let-values (((value ok) (parse-form ctx initial-parser)))
    (unless ok
      (syntax-violation 'parse-form* "unexpected end-of-file" 'eof))
    value))


; Parse textual input port until the end of current list, using the parser specified by
; initial-parser, and temporarily switching to other parsers if the directive #!...
; is found in a (possibly nested) list being parsed.
;
; Return parsed list.
; Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
(define (parse-form-list ctx begin-type already-parsed-reverse initial-parser)
  (let ((proc (parser-parse-list
                (to-parser initial-parser (parse-ctx-enabled-parsers ctx) 'parse-form-list))))
    (proc ctx begin-type already-parsed-reverse)))


; Parse textual input port until eof, using the parser specified by initial-parser,
; and temporarily switching to other parsers every time the directive #!... is found
; in a (possibly nested) list being parsed.
;
; Return two values.
; First value is parsed forms: each element in the list is a parsed form.
; Second value is updated parser to use.
(define (parse-forms ctx initial-parser)
  (let ((current-parser (to-parser initial-parser (parse-ctx-enabled-parsers ctx) 'parse-forms))
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

; Return mutable hashtable containing all known parsers.
(define parsers
  (let ((ret (make-eq-hashtable)))
    (hashtable-set! ret 'r6rs       (parser-r6rs))
    (hashtable-set! ret 'scheme     (parser-scheme))
    (hashtable-set! ret 'chezscheme (parser-scheme))
    (hashtable-set! ret 'shell      (parser-shell))
    (lambda ()
      ret)))

) ; close library
