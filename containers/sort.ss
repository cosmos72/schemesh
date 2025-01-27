;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(library (schemesh containers sort (0 7 1))
  (export
    span-range-sort! span-sort! vector-range-sort!)
  (import
    (rnrs)
    (only (chezscheme) eval-when fx1+ fx1- fxarithmetic-shift-right logbit? optimize-level procedure-arity-mask random void)
    (only (schemesh bootstrap) assert* debugf)
    (schemesh containers span))


(eval-when (compile) (optimize-level 3) (debug-level 0))

(define (%vector-swap v i j)
  (unless (fx=? i j)
    (let ((e1 (vector-ref v i))
          (e2 (vector-ref v j)))
      (vector-set! v i e2)
      (vector-set! v j e1))))


(define (%vector-partition is<? v start end)
  (let* ((pivot-i (fx+ start (fxarithmetic-shift-right (fx- end start) 1)))
         (pivot   (vector-ref v pivot-i))
         (out-i   start))
    (%vector-swap v pivot-i (fx1- end))
    (do ((i start (fx1+ i)))
        ((fx>=? i end))
      (when (is<? (vector-ref v i) pivot)
        (%vector-swap v i out-i)
        (set! out-i (fx1+ out-i))))
    (%vector-swap v (fx1- end) out-i)
    ; (debugf "%vector-partition start=~s end=~s out-i=~s v=~s" start end out-i v)
    out-i))


(define (%vector-range-sort! is<? v start end)
  ; (debugf "%vector-range-sort! start=~s end=~s v=~s" start end v)
  (let ((n (fx- end start)))
    (cond
      ((fx<=? n 1) (void))
      ((fx=?  n 2)
        (let ((e1 (vector-ref v start))
              (e2 (vector-ref v (fx1+ start))))
          (when (is<? e2 e1)
            (vector-set! v start e2)
            (vector-set! v (fx1+ start) e1))))
      (#t
        (let ((partition-i (%vector-partition is<? v start end)))
          (%vector-range-sort! is<? v start              partition-i)
          (%vector-range-sort! is<? v (fx1+ partition-i) end))))))


(define (vector-range-sort! is<? v start end)
  (assert* 'vector-range-sort! (procedure? is<?))
  (assert* 'vector-range-sort! (logbit? 2 (procedure-arity-mask is<?)))
  (assert* 'vector-range-sort! (vector? v))
  (assert* 'vector-range-sort! (fixnum? start))
  (assert* 'vector-range-sort! (fixnum? end))
  (assert* 'vector-range-sort! (fx<=? 0 start end (vector-length v)))
  (%vector-range-sort! is<? v start end))


(define (span-range-sort! is<? sp start end)
  (assert* 'span-range-sort! (procedure? is<?))
  (assert* 'span-range-sort! (logbit? 2 (procedure-arity-mask is<?)))
  (assert* 'span-range-sort! (span? sp))
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
