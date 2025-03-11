;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh containers vector (0 8 1))
  (export
    in-fxvector
    in-flvector ; requires Chez Scheme >= 10.0.0
    in-vector vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable! vector-range->list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         cflonum? cfl+ fl-make-rectangular
                               fx1+ fx1- fxvector-length fxvector-ref
                               import include meta-cond library-exports scheme-version)
    (only (schemesh bootstrap) assert* fx<=?* raise-errorf))


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
  (assert* 'subvector (fx<=?* 0 start end (vector-length vec)))
  (let* ((n (fx- end start))
         (dst (make-vector n)))
    (vector-copy! vec start dst 0 n)
    dst))

;; set elements in range [start, end) of vector vec specified value
(define (vector-fill-range! vec start end val)
  (assert* 'vector-fill-range! (fx<=?* 0 start end (vector-length vec)))
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
      (assert* 'in-vector (fx<=?* 0 start end (vector-length v)))
      (assert* 'in-vector (fx>=? step 0))
      (let ((%in-vector ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vector-ref v start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #f #f)))))
        %in-vector))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional fxvector functions    ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (meta-cond
    ((let ((exports (library-exports '(chezscheme))))
       (and (memq 'flvector-length exports)
            (memq 'flvector-ref    exports)))
      (case-lambda
        ((v start end step)
          (import (only (chezscheme) flvector-length flvector-ref))
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
          (import (only (chezscheme) flvector-length))
          (in-flvector v 0 (flvector-length v) 1))))
    (else
      (let ((raise-missing-flvector
              (lambda ()
                (raise-errorf 'in-flvector "flvector is only supported in Chez Scheme Version 10.0.0 or higher, found ~a" (scheme-version)))))
        (case-lambda
          ((v start end step)
            (raise-missing-flvector))
          ((v start end)
            (raise-missing-flvector))
          ((v)
            (raise-missing-flvector)))))))

) ; close library
