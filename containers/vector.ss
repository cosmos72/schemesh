;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file containers/misc.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional vector functions    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; copy a portion of vector src into dst.
;; works even if src are the same vector and the two ranges overlap.
(define (vector-copy! src src-start dst dst-start n)
  (if (and (eq? src dst) (fx<? src-start dst-start))
    ; copy backward
    (do ((i (fx1- n) (fx1- i)))
        ((fx<? i 0))
      (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))
    ; copy forward
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))))


;; return a copy of vector vec containing only elements
;; in range [start, end) i.e. from start (inclusive) to end (exclusive)
(define (subvector vec start end)
  (assert* 'subvector (fx<=? 0 start end (vector-length vec)))
  (let* ((n (fx- end start))
         (dst (make-vector n)))
    (vector-copy! vec start dst 0 n)
    dst))

;; set elements in range [start, end) of vector vec specified value
(define (vector-fill-range! vec start end val)
  (assert* 'vector-fill-range! (fx<=? 0 start end (vector-length vec)))
  (do ((i start (fx1+ i)))
      ((fx>=? i end))
    (vector-set! vec i val)))

;; read elements from vector range [start, end) and copy them into a list.
;; return such list.
(define (vector-range->list vec start end)
  (let %again ((pos (fx1- end))
               (ret '()))
    (if (fx>=? pos start)
      (%again (fx1- pos) (cons (vector-ref vec pos) ret))
      ret)))


;; create and return a closure that iterates on elements of vector v.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vector v and #t,
;; or (values #<unspecified> #f) if end of vector is reached.
(define in-vector
  (case-lambda
    ((v start end step)
      (assert* 'in-vector (fx<=? 0 start end (vector-length v)))
      (assert* 'in-vector (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (vector-ref v start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #f #f))))
    ((v start end)
      (in-vector v start end 1))
    ((v)
      (in-vector v 0 (vector-length v) 1))))


;; (vector-iterate l proc) iterates on all elements of given vector vec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (vector-iterate vec proc)
  (do ((i 0 (fx1+ i))
       (n (vector-length vec)))
      ((or (fx>=? i n) (not (proc i (vector-ref vec i))))
       (fx>=? i n))))


;; (vector->hashtable! vec htable) iterates on all elements of given vector vec,
;; which must be cons cells, and inserts them into hashtable htable:
;; (car cell) is used as key, and (cdr cell) is used ad value.
;
;; Returns htable.
(define (vector->hashtable! vec htable)
  (vector-iterate vec
    (lambda (i cell)
      (hashtable-set! htable (car cell) (cdr cell))))
  htable)
