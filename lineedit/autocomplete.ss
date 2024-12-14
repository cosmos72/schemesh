;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh lineedit autocomplete (0 1))
  (export
    lineedit-r6rs-autocomplete
    lineedit-scheme-autocomplete
    lineedit-shell-autocomplete)
  (import
    (rnrs)
    (only (chezscheme) void)
    (schemesh containers span)
    (only (schemesh posix misc) directory-u8-list))

(define (lineedit-r6rs-autocomplete prefix-csp completions-span)
  (void))

(define (lineedit-scheme-autocomplete prefix-csp completions-span)
  (void))

(define (lineedit-shell-autocomplete prefix-csp completions-span)
  ; (unless (charspan-empty? stem)
  ;    (list-iterate (directory-u8-list #vu8(46) (charspan->string stem))
  ;      (lambda (elem)
  ;        (span-insert-back! completions
  ;          (string->charspan* (utf8->string (cdr elem)))))))))
  (void))

) ; close library
