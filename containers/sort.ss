;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh containers sort (0 7 3))
  (export
    span-sort! vector-sort*!) ; R6RS already defines (vector-sort!)
  (import
    (rnrs)
    (only (chezscheme) eval-when format fx1+ fx1- fxarithmetic-shift-right logbit?
                       mutable-vector? optimize-level pariah procedure-arity-mask define void)
    ;;(only (schemesh bootstrap) assert*)
    (schemesh containers span))


(eval-when (compile) (optimize-level 3) (debug-level 0))

(define (raise-assert0 caller message)
  (raise
    (condition
      (make-assertion-violation)
      (make-non-continuable-violation)
      (make-who-condition caller)
      (make-message-condition message))))


;; optimized assert*, does not allocate. Error messages are less informative.
(define-syntax assert*
  (lambda (stx)
    (let ((message (format #f "assertion failed: ~s" (caddr (syntax->datum stx)))))
      (syntax-case stx ()
        ((_ caller expr)
          #`(if expr (void) (pariah (raise-assert0 caller #,message) (void))))))))


(define (%vector-swap! v x0 x1)
  (unless (fx=? x0 x1)
    (let ((e0 (vector-ref v x0))
          (e1 (vector-ref v x1)))
      (vector-set! v x0 e1)
      (vector-set! v x1 e0))))


;; sort the two vector elements at x0, x0+1
(define (%vector-sort/2! is<? v x0)
    (let ((e0 (vector-ref v x0))
          (e1 (vector-ref v (fx1+ x0))))
      (when (is<? e1 e0)
        (vector-set! v x0        e1)
        (vector-set! v (fx1+ x0) e0))))



;; sort the three vector elements at x0, x0+1, x0+2
(define (%vector-sort/3! is<? v x0)
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
(define (%vector-sort/4! is<? v x0)
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


;; sort the three vector elements at start, (avg start end), (fx1- end)
;; move them into positions start, (fx- end 2), (fx1- end)
;; and return middle value
(define (%vector-sort/pivot! is<? v start end)
  (let* ((x0 start)
         (x2 (fx1- end))
         (xp (fx1- x2))
         (x1 (fx+ x0 (fxarithmetic-shift-right (fx- xp x0) 1)))
         (e0 (vector-ref v x0))
         (e1 (vector-ref v x1))
         (e2 (vector-ref v x2))
         (ep (vector-ref v xp)))
    (if (is<? e0 e1)
      (cond           ; order is e0 .. e1
        ((is<? e1 e2) ; order is e0 e1 e2
          (vector-set! v x1 ep)
          (vector-set! v xp e1)
          e1)
        ((is<? e2 e0) ; order is e2 e0 e1
          (vector-set! v x0 e2)
          (vector-set! v x1 ep)
          (vector-set! v xp e0)
          (vector-set! v x2 e1)
          e0)
        (#t           ; order is e0 e2 e1
          (vector-set! v x1 ep)
          (vector-set! v xp e2)
          (vector-set! v x2 e1)
          e2))
      (cond           ; order is e1 .. e0
        ((is<? e0 e2) ; order is e1 e0 e2
          (vector-set! v x0 e1)
          (vector-set! v x1 ep)
          (vector-set! v xp e0)
          e0)
        ((is<? e2 e1) ; order is e2 e1 e0
          (vector-set! v x0 e2)
          (vector-set! v x1 ep)
          (vector-set! v xp e1)
          (vector-set! v x2 e0)
          e1)
        (#t           ; order is e1 e2 e0
          (vector-set! v x0 e1)
          (vector-set! v x1 ep)
          (vector-set! v xp e2)
          (vector-set! v x2 e0)
          e2)))))


(define (%vector-partition is<? v start end)
  ;; (%vector-sort/pivot!) already partitions first and last element
  (let ((pivot  (%vector-sort/pivot! is<? v start end))
        (end-2  (fx- end 2)))
    (let %loop ((lo (fx1+ start))
                (hi (fx1- end-2)))
      (if (fx<=? lo hi)
        (if (is<? (vector-ref v lo) pivot)
          (%loop (fx1+ lo) hi)
          (begin
            (%vector-swap! v lo hi)
            (%loop lo (fx1- hi))))
        (begin
          (%vector-swap! v lo end-2) ; put pivot after smaller elements
          lo)))))



(define (%vector-sort! is<? v start end)
  ; (debugf "%vector-sort! start=~s end=~s v=~s" start end v)
  (let ((n (fx- end start)))
    (cond
      ((fx>? n 4)
        (let ((partition-i (%vector-partition is<? v start end)))
          (%vector-sort! is<? v start              partition-i)
          (%vector-sort! is<? v (fx1+ partition-i) end)))
      ((fx=? n 4)
        (%vector-sort/4! is<? v start))
      ((fx=? n 3)
        (%vector-sort/3! is<? v start))
      ((fx=? n 2)
        (%vector-sort/2! is<? v start))
      (#t
        (void)))))


;; do not use the name (vector-sort!), R6RS already defines it
(define vector-sort*!
  (case-lambda
    ((is<? v)
      (assert* 'vector-sort*! (vector? v))
      (vector-sort*! is<? v 0 (vector-length v)))
    ((is<? v start end)
      (assert* 'vector-sort*! (procedure? is<?))
      (assert* 'vector-sort*! (logbit? 2 (procedure-arity-mask is<?)))
      (assert* 'vector-sort*! (vector? v))
      (assert* 'vector-sort*! (mutable-vector? v))
      (assert* 'vector-sort*! (fixnum? start))
      (assert* 'vector-sort*! (fixnum? end))
      (assert* 'vector-sort*! (fx<=? 0 start end (vector-length v)))
      (%vector-sort! is<? v start end))))


(define span-sort!
  (case-lambda
    ((is<? sp)
      (assert* 'span-sort! (span? sp))
      (span-sort! is<? sp 0 (span-length sp)))
    ((is<? sp start end)
      (assert* 'span-sort! (procedure? is<?))
      (assert* 'span-sort! (logbit? 2 (procedure-arity-mask is<?)))
      (assert* 'span-sort! (span? sp))
      (assert* 'span-sort! (mutable-vector? (span-peek-data sp)))
      (assert* 'span-sort! (fixnum? start))
      (assert* 'span-sort! (fixnum? end))
      (assert* 'span-sort! (fx<=? 0 start end (span-length sp)))
      (let ((beg (span-peek-beg sp)))
        (%vector-sort! is<? (span-peek-data sp) (fx+ beg start) (fx+ beg end))))))



) ; close library
