;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers sort (0 1))
  (export
    span-range-sort! vector-range-sort!)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- random void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers span))


(define (span-range-sort! sp start end is<?)
  (assert* 'span-range-sort! (span? sp))
  (assert* 'span-range-sort! (fx<=? 0 start end (span-length sp)))
  (assert* 'span-range-sort! (procedure? is<?))
  (let ((beg (span-peek-beg sp)))
    (%vector-range-sort! (span-peek-data sp) (fx+ beg start) (fx+ beg end) is<?)))


(define (vector-range-sort! v start end is<?)
  (assert* 'vector-range-sort! (vector? v))
  (assert* 'vector-range-sort! (fx<=? 0 start end (vector-length v)))
  (assert* 'vector-range-sort! (procedure? is<?))
  (%vector-range-sort! v start end is<?))


(define (%vector-range-sort! v start end is<?)
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
        (let ((partition-i (%vector-partition v start end is<?)))
          (%vector-range-sort! v start partition-i  is<?)
          (%vector-range-sort! v (fx1+ partition-i) end is<?))))))


(define (%vector-partition v start end is<?)
  (let* ((pivot-i (fx+ start (random (fx- end start))))
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


(define (%vector-swap v i j)
  (unless (fx=? i j)
    (let ((ei (vector-ref v i))
          (ej (vector-ref v j)))
      (vector-set! v i ej)
      (vector-set! v j ei))))

) ; close library
