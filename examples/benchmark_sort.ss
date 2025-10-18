
;; example file containing a benchmark for (vector-sort!) and (subvector-sort!)
;; it is not read, compiled nor evaluated.

(library (schemesh benchmark sort (0 9 2))
  (export
    benchmark-make-vector benchmark-vector-sort! benchmark-subvector-sort! )
  (import
    (rnrs)
    (only (chezscheme)           eval-when fx1+ fx1- random time vector-sort!)
    (only (schemesh bootstrap)     assert*)
    (only (schemesh containers sort)   subvector-sort!)
    (only (schemesh containers vector) vector-copy!))


(eval-when (compile) (optimize-level 3) (debug-level 0))

(define (benchmark-make-vector element-n)
  (let ((v (make-vector element-n)))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i element-n) v)
      (vector-set! v i (random (greatest-fixnum))))))


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


(define (benchmark-subvector-sort! element-n run-n)
  (assert* 'benchmark-subvector-sort! (fixnum? element-n))
  (assert* 'benchmark-subvector-sort! (fx>=?   element-n 0))
  (assert* 'benchmark-subvector-sort! (fixnum? run-n))
  (assert* 'benchmark-subvector-sort! (fx>=?   run-n 0))
  (let ((v0 (benchmark-make-vector element-n))
        (v  (make-vector element-n)))
    (do ((i run-n (fx1- i)))
        ((fx<=? i 0))
      (vector-copy! v0 0 v 0 element-n)
      (subvector-sort! fx<? v))))


) ; close library

(import (schemesh benchmark sort))
