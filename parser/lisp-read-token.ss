;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh parser lisp read-token (0 7 2))
  (export
    read-token*)
  (import
    (rnrs)
    (only (chezscheme) read-token)
    (only (schemesh containers utf8b) integer->char*))

;; Extension of (read-token) that recognizes
;;   { as (values 'lbrace f ...)
;;   } as (values 'rbrace f ...)
;;
;; TODO: add support for character hexadecimal escape sequence #\x... corresponding to UTF-8b codepoints
;; and strings containing hexadecimal escape sequences \x...; corresponding to UTF-8b codepoints.
;;
;; Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
;; in pure R6RS.
(define (read-token* in)
  (let-values (((type value start end) (read-token in)))
    (if (eq? 'atomic type)
      (case value
        ;; replace (values '{ 'atomic) with (values #f 'lbrace)
        ;; and replace (values '} 'atomic) with (values #f 'rbrace)
        ;; because we use them to switch to shell parser. For example,
        ;;    {ls -l > log.txt}
        ;; is equivalent to
        ;;    (#!shell ls -l > log.txt)
        (({)  (values 'lbrace #f start end))
        ((})  (values 'rbrace #f start end))
        (else (values type value start end)))
      (values type value start end))))


) ; close library
