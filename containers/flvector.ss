;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


#!r6rs

(library (schemesh containers flvector (0 9 1))
  (export
    flvector-native? flvector flvector? flvector-length flvector-ref flvector-set! make-flvector)
  (import
    (rnrs)
    (only (chezscheme) import library-exports meta-cond))


(meta-cond
  ((let ((exports (library-exports '(chezscheme))))
      (and (memq 'flvector?       exports)
           (memq 'flvector-length exports)
           (memq 'flvector-ref    exports)
           (memq 'flvector-set!   exports)
           (memq 'make-flvector   exports)))
    (import (prefix
                (only (chezscheme) flvector flvector? flvector-length flvector-ref flvector-set! make-flvector)
              chez:))
    ;; constant set to #t if native flvector functions are available
    ;; otherwise set to #f
    (define flvector-native? #t)
    (define flvector         chez:flvector)
    (define flvector?        chez:flvector?)
    (define flvector-length  chez:flvector-length)
    (define flvector-ref     chez:flvector-ref)
    (define flvector-set!    chez:flvector-set!)
    (define make-flvector    chez:make-flvector))

  (else
    (define flvector-native? #f)
    (define flvector         vector)
    (define (flvector? obj)  #f)
    (define flvector-length  vector-length)
    (define flvector-ref     vector-ref)
    (define flvector-set!    vector-set!)
    (define make-flvector    make-vector)))


) ; close library
