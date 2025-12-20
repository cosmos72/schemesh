;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers vector (0 9 2))
  (export
    for-vector in-vector vector-any vector-copy!
    subvector subvector-fill!
    vector-index vector-iterate vector->hashtable! subvector->list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         cflonum? cfl+ fl-make-rectangular
                               fx1+ fx1- fxvector-length fxvector-ref
                               import meta-cond library-exports)
    (only (scheme2k bootstrap) assert* forever fx<=?* raise-errorf generate-pretty-temporaries with-while-until))


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
(define (subvector-fill! vec start end val)
  (assert* 'subvector-fill! (fx<=?* 0 start end (vector-length vec)))
  (do ((i start (fx1+ i)))
      ((fx>=? i end))
    (vector-set! vec i val)))

;; read elements from vector range [start, end) and copy them into a list.
;; return such list.
(define (subvector->list vec start end)
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



;; Iterate in parallel on elements of given vector(s) v ..., and evaluate body ... on each element.
;; Stop iterating when the shortest vector is exhausted,
;; and return unspecified value.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect or modify the vectors elements.
;;
;; It must NOT call any function that modifies the vectors' length, as for example (vector-truncate!)
;;
;; Return unspecified value.
(define-syntax for-vector
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((elem v) ...) body ...)
        (with-syntax (((tv ...) (generate-pretty-temporaries #'(v ...))))
          #'(let ((tv v) ...)
              (let %for-vector ((i 0) (n (fxmin (vector-length v) ...)))
                (when (fx<? i n)
                  (let ((elem (vector-ref tv i)) ...)
                    (with-while-until
                      body ...
                      (%for-vector (fx1+ i) n)))))))))))


;; apply proc element-wise to the i-th element of each vector
(define (%apply-proc i proc vecs)
  (apply proc (map (lambda (vec) (vector-ref vec i)) vecs)))


;; apply proc element-wise to the elements of the vectors,
;; stop at the first truish value returned by (proc elem ...) and return such value.
;;
;; If all calls to (proc elem ...) return #f, then return #f.
;;
;; If not all vectors have the same length, iteration terminates when the end of shortest vector is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
(define vector-any
  (case-lambda
    ((proc vec)
      (let %vector-any ((i 0) (n (vector-length vec)) (proc proc) (vec vec))
        (if (fx>=? i n)
          #f
          (or (proc (vector-ref vec i))
              (%vector-any (fx1+ i) n proc vec)))))
    ((proc vec1 vec2)
      (let %vector-any ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2)))
                        (proc proc) (vec1 vec1) (vec2 vec2))
        (if (fx>=? i n)
          #f
          (or (proc (vector-ref vec1 i) (vector-ref vec2 i))
              (%vector-any (fx1+ i) n proc vec1 vec2)))))
    ((proc vec1 vec2 vec3)
      (let %vector-any ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3)))
                        (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3))
        (if (fx>=? i n)
          #f
          (or (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i))
              (%vector-any (fx1+ i) n proc vec1 vec2 vec3)))))
    ((proc vec1 vec2 vec3 vec4)
      (let %vector-any ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3) (vector-length vec4)))
                        (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3) (vec4 vec4))
        (if (fx>=? i n)
          #f
          (or (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i) (vector-ref vec4 i))
              (%vector-any (fx1+ i) n proc vec1 vec2 vec3 vec4)))))
    ((proc vec1 . vecs)
      (let %vector-any ((i 0) (n (apply fxmin (map vector-length (cons vec1 vecs))))
                        (proc proc) (vecs (cons vec1 vecs)))
        (if (fx>=? i n)
          #f
          (or (%apply-proc proc i vecs)
              (%vector-any (fx1+ i) n proc vecs)))))))


;; apply proc element-wise to the elements of the vectors,
;; stop at the first truish value returned by (proc elem ...) and return the index of such elements.
;;
;; If all calls to (proc elem ...) return #f, then return #f.
;;
;; If not all vectors have the same length, iteration terminates when the end of shortest vector is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
;;
;; Extension: if only one vector is specified and proc is not a procedure,
;; search for first element eqv? to proc.
(define vector-index
  (case-lambda
    ((proc vec)
      (let %vector-index ((i 0)
                          (n (vector-length vec))
                          (proc (if (procedure? proc) proc (lambda (elem) (eqv? proc elem))))
                          (vec vec))
        (if (fx>=? i n)
          #f
          (if (proc (vector-ref vec i))
            i
            (%vector-index (fx1+ i) n proc vec)))))
    ((proc vec1 vec2)
      (let %vector-index ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2)))
                        (proc proc) (vec1 vec1) (vec2 vec2))
        (if (fx>=? i n)
          #f
          (if (proc (vector-ref vec1 i) (vector-ref vec2 i))
            i
            (%vector-index (fx1+ i) n proc vec1 vec2)))))
    ((proc vec1 vec2 vec3)
      (let %vector-index ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3)))
                        (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3))
        (if (fx>=? i n)
          #f
          (if (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i))
            i
            (%vector-index (fx1+ i) n proc vec1 vec2 vec3)))))
    ((proc vec1 vec2 vec3 vec4)
      (let %vector-index ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3) (vector-length vec4)))
                        (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3) (vec4 vec4))
        (if (fx>=? i n)
          #f
          (if (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i) (vector-ref vec4 i))
            i
            (%vector-index (fx1+ i) n proc vec1 vec2 vec3 vec4)))))
    ((proc vec1 . vecs)
      (let %vector-index ((i 0) (n (apply fxmin (map vector-length (cons vec1 vecs))))
                        (proc proc) (vecs (cons vec1 vecs)))
        (if (fx>=? i n)
          #f
          (if (%apply-proc proc i vecs)
            i
            (%vector-index (fx1+ i) n proc vecs)))))))



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


(meta-cond
  ;; fxvector-copy! is defined only in Chez Scheme >= 10.2.0
  ((memq 'fxvector-copy! (library-exports '(chezscheme)))
    (import (prefix
                (only (chezscheme) fxvector-copy!)
              chez:))
    (define fxvector-copy!   chez:fxvector-copy!))

  (else
    (define fxvector-copy!   vector-copy!)))


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
