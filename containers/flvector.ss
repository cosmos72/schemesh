;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


#!r6rs

(library (schemesh containers flvector (0 9 2))
  (export
    flvector-native? flvector flvector? flvector-length flvector-ref flvector-set! make-flvector
    flvector-copy! in-flvector)
  (import
    (rnrs)
    (only (chezscheme) import library-exports meta-cond)
    (only (schemesh bootstrap) assert* fx<=?*)
    (only (schemesh containers vector) vector-copy!))


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


(meta-cond
  ;; flvector-copy! is defined only in Chez Scheme >= 10.2.0
  ((memq 'flvector-copy! (library-exports '(chezscheme)))
    (import (prefix
                (only (chezscheme) flvector-copy!)
              chez:))
    (define flvector-copy!   chez:flvector-copy!))

  (else
    ;; vector-copy! is defined only in Chez Scheme >= 10.3.0
    ;; => use vectory-copy! from (schemesh containers vector)
    (define flvector-copy!   vector-copy!)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional flvector functions    ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create and return a closure that iterates on elements of flvector v.
;; Requires Chez Scheme >= 10.0.0.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in flvector v and #t,
;; or (values #<unspecified> #f) if end of vector is reached.
(define in-flvector
  (case-lambda
    ((v start end step)
      (assert* 'in-flvector (fx<=?* 0 start end (flvector-length v)))
      (assert* 'in-flvector (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (flvector-ref v start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #f #f))))
    ((v start end)
      (in-flvector v start end 1))
    ((v)
      (in-flvector v 0 (flvector-length v) 1))))

) ; close library
