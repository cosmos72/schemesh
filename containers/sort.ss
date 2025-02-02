;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(library (schemesh containers sort (0 7 2))
  (export
    span-range-sort! span-sort! vector-range-sort!)
  (import
    (rnrs)
    (only (chezscheme) eval-when fx1+ fx1- fxarithmetic-shift-right logbit?
                       mutable-vector? optimize-level procedure-arity-mask random void)
    (only (schemesh bootstrap) assert* while)
    (schemesh containers span))


(eval-when (compile) (optimize-level 3) (debug-level 0))


(define (%vector-swap! v x0 x1)
  (unless (fx=? x0 x1)
    (let ((e0 (vector-ref v x0))
          (e1 (vector-ref v x1)))
      (vector-set! v x0 e1)
      (vector-set! v x1 e0))))


;; sort the two vector elements at x0, x0+1
(define (%vector-range-sort/2! is<? v x0)
    (let ((e0 (vector-ref v x0))
          (e1 (vector-ref v (fx1+ x0))))
      (when (is<? e1 e0)
        (vector-set! v x0        e1)
        (vector-set! v (fx1+ x0) e0))))



;; sort the three vector elements at x0, x0+1, x0+2
(define (%vector-range-sort/3! is<? v x0)
  (let* ((x1 (fx1+ x0))
         (x2 (fx+ x0 2))
         (e0 (vector-ref v x0))
         (e1 (vector-ref v x1))
         (e2 (vector-ref v x2)))
    (if (is<? e0 e1)
      (cond           ; order is e0 .. e1
        ((is<? e1 e2) ; order is e0 e1 e2
          (void))
        ((is<? e2 e0) ; order is e2 e0 e1
          (vector-set! v x0 e2)
          (vector-set! v x1 e0)
          (vector-set! v x2 e1))
        (#t           ; order is e0 e2 e1
          (vector-set! v x1 e2)
          (vector-set! v x2 e1)))
      (cond           ; order is e1 .. e0
        ((is<? e0 e2) ; order is e1 e0 e2
          (vector-set! v x0 e1)
          (vector-set! v x1 e0))
        ((is<? e2 e1) ; order is e2 e1 e0
          (vector-set! v x0 e2)
          (vector-set! v x2 e0))
        (#t           ; order is e1 e2 e0
          (vector-set! v x0 e1)
          (vector-set! v x1 e2)
          (vector-set! v x2 e0))))))


;; sort the two elements a b.
;; bind lo to the smaller value, and hi to the higher one
(define-syntax let-sorted2
  (syntax-rules ()
    ((_ (lo hi) (is<? a b) body ...)
       (let* ((cmp (is<? a b))
              (lo (if cmp a b))
              (hi (if cmp b a)))
         body ...))))


;; sort the four vector elements at x0, x0+1, x0+2, x0+3
(define (%vector-range-sort/4! is<? v x0)
  (let* ((x1 (fx1+ x0))
         (x2 (fx+ x0 2))
         (x3 (fx+ x0 3))
         (a (vector-ref v x0))
         (b (vector-ref v x1))
         (c (vector-ref v x2))
         (d (vector-ref v x3)))
    (let-sorted2 (lo1 hi1) (is<? a b)
      (let-sorted2 (lo2 hi2) (is<? c d)
        (let-sorted2 (e0 mid1) (is<? lo1 lo2)
          (let-sorted2 (mid2 e3) (is<? hi1 hi2)
            (let-sorted2 (e1 e2) (is<? mid1 mid2)
              (unless (eq? a e0)
                (vector-set! v x0 e0))
              (unless (eq? b e1)
                (vector-set! v x1 e1))
              (unless (eq? c e2)
                (vector-set! v x2 e2))
              (unless (eq? d e3)
                (vector-set! v x3 e3)))))))))


(define (%vector-partition is<? v start end)
  (let* ((pivot-i (fx+ start (fxarithmetic-shift-right (fx- end start) 1)))
         (pivot   (vector-ref v pivot-i))
         (out-i   start)
         (end-1   (fx- end 1))) ; pivot is at (fx1- end), no need to compare it against itself
    (%vector-swap! v pivot-i end-1)
    (do ((i start (fx1+ i)))
        ((fx>=? i end-1))
      (when (is<? (vector-ref v i) pivot)
        (%vector-swap! v i out-i)
        (set! out-i (fx1+ out-i))))
    (%vector-swap! v end-1 out-i)
    ; (debugf "%vector-partition start=~s end=~s out-i=~s v=~s" start end out-i v)
    out-i))


(define (%vector-range-sort! is<? v start end)
  ; (debugf "%vector-range-sort! start=~s end=~s v=~s" start end v)
  (case (fx- end start)
    ((0 1)
      (void))
    ((2)
      (%vector-range-sort/2! is<? v start))
    ((3)
      (%vector-range-sort/3! is<? v start))
    ((4)
      (%vector-range-sort/4! is<? v start))
    (else
      (let ((partition-i (%vector-partition is<? v start end)))
        (%vector-range-sort! is<? v start              partition-i)
        (%vector-range-sort! is<? v (fx1+ partition-i) end)))))


(define (vector-range-sort! is<? v start end)
  (assert* 'vector-range-sort! (procedure? is<?))
  (assert* 'vector-range-sort! (logbit? 2 (procedure-arity-mask is<?)))
  (assert* 'vector-range-sort! (vector? v))
  (assert* 'vector-range-sort! (mutable-vector? v))
  (assert* 'vector-range-sort! (fixnum? start))
  (assert* 'vector-range-sort! (fixnum? end))
  (assert* 'vector-range-sort! (fx<=? 0 start end (vector-length v)))
  (%vector-range-sort! is<? v start end))


(define (span-range-sort! is<? sp start end)
  (assert* 'span-range-sort! (procedure? is<?))
  (assert* 'span-range-sort! (logbit? 2 (procedure-arity-mask is<?)))
  (assert* 'span-range-sort! (span? sp))
  (assert* 'span-range-sort! (mutable-vector? (span-peek-data sp)))
  (assert* 'span-range-sort! (fixnum? start))
  (assert* 'span-range-sort! (fixnum? end))
  (assert* 'span-range-sort! (fx<=? 0 start end (span-length sp)))
  (let ((beg (span-peek-beg sp)))
    (%vector-range-sort! is<? (span-peek-data sp) (fx+ beg start) (fx+ beg end))))

#|
;; unnecessary, already defined in R6RS
(define (vector-sort! is<? v)
  (vector-range-sort! is<? v 0 (vector-length v)))
|#

(define (span-sort! is<? sp)
  (assert* 'span-sort! (span? sp))
  (span-range-sort! is<? sp 0 (span-length sp)))

) ; close library
