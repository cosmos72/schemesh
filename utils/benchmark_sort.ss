
;; example file containing a benchmark for (vector-range-sort!) and (vector-sort!)
;; it is not read, compiled nor evaluated.

(library (schemesh benchmark sort (0 7 2))
  (export
    benchmark-make-vector benchmark-vector-range-sort! benchmark-vector-sort!)
  (import
    (rnrs)
    (only (chezscheme)           eval-when fx1+ fx1- random time vector-sort!)
    (only (schemesh bootstrap)     assert*)
    (only (schemesh containers misc) vector-copy!)
    (only (schemesh containers sort) vector-range-sort!))


(eval-when (compile) (optimize-level 3) (debug-level 0))

(define (benchmark-make-vector element-n)
  (let ((v (make-vector element-n)))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i element-n) v)
      (vector-set! v i (random (greatest-fixnum))))))


(define (benchmark-vector-range-sort! element-n run-n)
  (assert* 'benchmark-vector-range-sort! (fixnum? element-n))
  (assert* 'benchmark-vector-range-sort! (fx>=?   element-n 0))
  (assert* 'benchmark-vector-range-sort! (fixnum? run-n))
  (assert* 'benchmark-vector-range-sort! (fx>=?   run-n 0))
  (let ((v0 (benchmark-make-vector element-n))
        (v  (make-vector element-n)))
    (do ((i run-n (fx1- i)))
        ((fx<=? i 0))
      (vector-copy! v0 0 v 0 element-n)
      (vector-range-sort! fx<? v 0 element-n))))


(define (benchmark-vector-sort! element-n run-n)
  (assert* 'benchmark-vector-sort! (fixnum? element-n))
  (assert* 'benchmark-vector-sort! (fx>=?   element-n 0))
  (assert* 'benchmark-vector-sort! (fixnum? run-n))
  (assert* 'benchmark-vector-sort! (fx>=?   run-n 0))
  (let ((v0 (benchmark-make-vector element-n))
        (v  (make-vector element-n)))
    (do ((i run-n (fx1- i)))
        ((fx<=? i 0))
      (vector-copy! v0 0 v 0 element-n)
      (vector-sort! fx<? v))))


) ; close library

(import (schemesh benchmark sort))
