;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers flvector (0 9 3))
  (export
    flvector-native? flvector flvector? flvector-length flvector-ref flvector-set! make-flvector
    flvector-copy! for-flvector in-flvector)
  (import
    (rnrs)
    (only (chezscheme)         foreign-procedure import library-exports meta-cond fx1+ fx1-)
    (only (scheme2k bootstrap) assert* forever fx<=?* generate-pretty-temporaries with-while-until))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional flvector functions    ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (flvector-copy! src src-start dst dst-start n)
;;
;; Added in scheme2k 0.9.3
(meta-cond
  ;; flvector-copy! is defined only in Chez Scheme >= 10.2.0
  ((let ((exports (library-exports '(chezscheme))))
     (memq 'flvector-copy! exports))

    (import (prefix
                (only (chezscheme) flvector-copy!)
              chez:))
    (define flvector-copy!   chez:flvector-copy!))

  ((let ((exports (library-exports '(chezscheme))))
     (and (memq 'flvector?       exports)
          (memq 'flvector-length exports)
          (memq 'flvector-ref    exports)
          (memq 'flvector-set!   exports)))

    ;; flvector is a different type in Chez Scheme >= 10.0.0, cannot reuse (vectory-copy!)
    (define flvector-copy!
      (let ((c-flvector-copy! (foreign-procedure "c_flvector_copy" (ptr ptr ptr ptr ptr) void)))
        (lambda (src src-start dst dst-start n)
          (case n
            ((3)
              ;; copy may overlap
              (let ((e0 (flvector-ref src      src-start))
                    (e1 (flvector-ref src (fx1+ src-start)))
                    (e2 (flvector-ref src (fx+ 2 src-start))))
                (flvector-set! dst      dst-start e0)
                (flvector-set! dst (fx1+ dst-start) e1)
                (flvector-set! dst (fx+ 2 dst-start) e2)))
            ((2)
              ;; copy may overlap
              (let ((e0 (flvector-ref src      src-start))
                    (e1 (flvector-ref src (fx1+ src-start))))
                (flvector-set! dst      dst-start e0)
                (flvector-set! dst (fx1+ dst-start) e1)))
            ((1)
              (let ((e0 (flvector-ref src src-start)))
                (flvector-set! dst dst-start e0)))
            (else
              (assert* 'flvector-copy! (flvector? src))
              (assert* 'flvector-copy! (flvector? dst))
              (assert* 'flvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (flvector-length src)))
              (assert* 'flvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (flvector-length dst)))
              (unless (fxzero? n)
                (c-flvector-copy! src src-start dst dst-start n))))))))

  (else
    ;; flvector is not a different type, it is aliased to vector (see above) in Chez Scheme < 10.0.0
    (define (flvector-copy! src src-start dst dst-start n)
      (if (and (eq? src dst) (fx<? src-start dst-start))
        ;; copy backward
        (do ((i (fx1- n) (fx1- i)))
            ((fx<? i 0))
          (flvector-set! dst (fx+ i dst-start) (flvector-ref src (fx+ i src-start))))
        ;; copy forward
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n))
          (flvector-set! dst (fx+ i dst-start) (flvector-ref src (fx+ i src-start))))))))




;; Iterate in parallel on elements of given flvector(s) v ..., and evaluate body ... on each element.
;; Stop iterating when the shortest flvector is exhausted,
;; and return unspecified value.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect or modify the flvector(s) elements.
;;
;; It must NOT call any function that modifies the flvector(s) length, as for example (flvector-truncate!)
;;
;; If no flvector is specified, behaves as (forever body ...)
;;
;; Return unspecified value.
;;
;; Added in scheme2k 0.9.3
(define-syntax for-flvector
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((elem v) ...) body ...)
        (with-syntax (((tv ...) (generate-pretty-temporaries #'(v ...))))
          #'(let ((tv v) ...)
              (let %for-flvector ((i 0) (n (fxmin (flvector-length v) ...)))
                (when (fx<? i n)
                  (let ((elem (flvector-ref tv i)) ...)
                    (with-while-until
                      body ...
                      (%for-flvector (fx1+ i) n)))))))))))


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
