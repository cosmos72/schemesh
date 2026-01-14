;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers fxvector (0 9 3))
  (export
    fxsign fxvector<? fxvector<=? fxvector>? fxvector>=? fxvector=? fxvector-compare fxvector-copy! for-fxvector in-fxvector)
  (import
    (rnrs)
    (only (chezscheme)         foreign-procedure
                               fx1+ fx1- fxvector? fxvector-length fxvector-ref fxvector-set!
                               import meta-cond library-exports)
    (only (scheme2k bootstrap) assert* forever fx<=?* generate-pretty-temporaries with-while-until))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional fxvector functions    ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return  0 if fixnum n is 0
;; return -1 if fixnum n is < 0
;; return +1 if fixnum n is > 0
(define (fxsign n)
  (cond
    ((fx>? n 0)   1)
    ((fxzero? n)  0)
    (else        -1)))


;; (fxvector-copy! src src-start dst dst-start n)
;;
;; Added in scheme2k 0.9.3
(meta-cond
  ;; fxvector-copy! is defined only in Chez Scheme >= 10.2.0
  ((memq 'fxvector-copy! (library-exports '(chezscheme)))
    (import (prefix
                (only (chezscheme) fxvector-copy!)
              chez:))
    (define fxvector-copy!   chez:fxvector-copy!))

  (else
    ;; fxvector and vector are different types, cannot reuse (vector-copy!)
    (define fxvector-copy!
      (let ((c-fxvector-copy! (foreign-procedure "c_fxvector_copy" (ptr fixnum ptr fixnum fixnum) void)))
        (lambda (src src-start dst dst-start n)
          (case n
            ((3)
              ;; copy may overlap
              (let ((e0 (fxvector-ref src      src-start))
                    (e1 (fxvector-ref src (fx1+ src-start)))
                    (e2 (fxvector-ref src (fx+ 2 src-start))))
                (fxvector-set! dst      dst-start e0)
                (fxvector-set! dst (fx1+ dst-start) e1)
                (fxvector-set! dst (fx+ 2 dst-start) e2)))
            ((2)
              ;; copy may overlap
              (let ((e0 (fxvector-ref src      src-start))
                    (e1 (fxvector-ref src (fx1+ src-start))))
                (fxvector-set! dst      dst-start e0)
                (fxvector-set! dst (fx1+ dst-start) e1)))
            ((1)
              (let ((e0 (fxvector-ref src src-start)))
                (fxvector-set! dst dst-start e0)))
            (else
              (assert* 'fxvector-copy! (fxvector? src))
              (assert* 'fxvector-copy! (fxvector? dst))
              (assert* 'fxvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (fxvector-length src)))
              (assert* 'fxvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (fxvector-length dst)))
              (unless (fxzero? n)
                (c-fxvector-copy! src src-start dst dst-start n)))))))))


;; compare portions of two fxvectors.
;; return 0 if they are equal, -1 if first one is smaller, +1 if first one is larger.
(define fxvector-compare
  (let ((c-fxvector-compare (foreign-procedure "c_fxvector_compare" (ptr fixnum ptr fixnum fixnum) integer-8)))
    (case-lambda
      ((vec1 start1 vec2 start2 n)
        (assert* 'fxvector-compare (fx<=?* 0 start1 (fx+ start1 n) (fxvector-length vec1)))
        (assert* 'fxvector-compare (fx<=?* 0 start2 (fx+ start2 n) (fxvector-length vec2)))
        (if (or (fxzero? n) (and (eq? vec1 vec2) (fx=? start1 start2)))
          0
          (c-fxvector-compare vec1 start1 vec2 start2 n)))
      ((vec1 vec2)
        (let* ((n1  (fxvector-length vec1))
               (n2  (fxvector-length vec2))
               (cmp (fxvector-compare vec1 0 vec2 0 (fxmin n1 n2))))
          (if (fxzero? cmp)
            (fxsign (fx- n1 n2))
            cmp))))))


(define (fxvector<? vec1 vec2)
  (fx<? (fxvector-compare vec1 vec2) 0))

(define (fxvector<=? vec1 vec2)
  (fx<=? (fxvector-compare vec1 vec2) 0))

(define (fxvector>? vec1 vec2)
  (fx>? (fxvector-compare vec1 vec2) 0))

(define (fxvector>=? vec1 vec2)
  (fx>=? (fxvector-compare vec1 vec2) 0))

;; compare portions of two fxvectors.
;; return #t if they are equal, otherwise return #f
(define fxvector=?
  (let ((c-fxvector=? (foreign-procedure "c_fxvector_equal" (ptr fixnum ptr fixnum fixnum) ptr)))
    (case-lambda
      ((vec1 start1 vec2 start2 n)
        (assert* 'fxvector=? (fx<=?* 0 start1 (fx+ start1 n) (fxvector-length vec1)))
        (assert* 'fxvector=? (fx<=?* 0 start2 (fx+ start2 n) (fxvector-length vec2)))
        (or (fxzero? n)
            (and (eq? vec1 vec2) (fx=? start1 start2))
            (c-fxvector=? vec1 start1 vec2 start2 n)))
      ((vec1 vec2)
        (let ((n1 (fxvector-length vec1))
              (n2 (fxvector-length vec2)))
          (and (fx=? n1 n2)
               (fxvector=? vec1 0 vec2 0 n1)))))))

;; Iterate in parallel on elements of given fxvector(s) v ..., and evaluate body ... on each element.
;; Stop iterating when the shortest fxvector is exhausted,
;; and return unspecified value.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect or modify the fxvector(s) elements.
;;
;; It must NOT call any function that modifies the fxvector(s) length, as for example (fxvector-truncate!)
;;
;; If no flvector is specified, behaves as (forever body ...)
;;
;; Return unspecified value.
;;
;; Added in scheme2k 0.9.3
(define-syntax for-fxvector
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((elem v) ...) body ...)
        (with-syntax (((tv ...) (generate-pretty-temporaries #'(v ...))))
          #'(let ((tv v) ...)
              (let %for-fxvector ((i 0) (n (fxmin (fxvector-length v) ...)))
                (when (fx<? i n)
                  (let ((elem (fxvector-ref tv i)) ...)
                    (with-while-until
                      body ...
                      (%for-fxvector (fx1+ i) n)))))))))))


;; create and return a closure that iterates on elements of fxvector v.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in fxvector v and #t,
;; or (values #<unspecified> #f) if end of vector is reached.
(define in-fxvector
  (case-lambda
    ((v start end step)
      (assert* 'in-fxvector (fx<=?* 0 start end (fxvector-length v)))
      (assert* 'in-fxvector (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (fxvector-ref v start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #f #f))))
    ((v start end)
      (in-fxvector v start end 1))
    ((v)
      (in-fxvector v 0 (fxvector-length v) 1))))

) ; close library
