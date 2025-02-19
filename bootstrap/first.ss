;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh bootstrap first (0 7 6))
  (export
      generate-pretty-temporaries generate-pretty-temporary gensym-pretty)
  (import
    (rnrs)
    (only (chezscheme) gensym))


(define (gensym-pretty x)
  (cond
    ((string? x) (gensym x))
    ((symbol? x) (gensym (symbol->string x)))
    ((number? x) (gensym (number->string x)))
    ((pair? x)
      (gensym-pretty
        (let ((a (car x))
              (b (cdr x)))
          (cond
            ((null? b)      a)
            ((eq? 'quote a) b)
            (else           a)))))
    (else
      (gensym))))


(define (generate-pretty-temporary stx)
  (datum->syntax #'stx (gensym-pretty (syntax->datum stx))))


(define (generate-pretty-temporaries l)
  (map generate-pretty-temporary l))


) ; close library
