;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers vector (1 0 0))
  (export
    for-vector in-vector subvector subvector-fill!
    vector-any vector-copy! vector-every
    vector-index vector-iterate vector->hashtable! subvector->list)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         cflonum? cfl+ fl-make-rectangular fx1+ fx1- import meta-cond library-exports)
    (only (scheme2k bootstrap) assert* begin0 for fx<=?* raise-errorf generate-pretty-temporaries lambda0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional vector functions    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; copy a portion of vector src into dst.
;; works even if src are the same vector and the two ranges overlap.
;;
;; NOTE: arguments order is different from SRFI 43 function with the same name
(meta-cond
  ;; vector-copy! is defined only in Chez Scheme >= 10.3.0
  ((memq 'vector-copy! (library-exports '(chezscheme)))
    (import (prefix
                (only (chezscheme) vector-copy!)
              chez:))
    (define vector-copy! chez:vector-copy!))

  (else
    (define (vector-copy! src src-start dst dst-start n)
      (if (and (eq? src dst) (fx<? src-start dst-start))
        ; copy backward
        (do ((i (fx1- n) (fx1- i)))
            ((fx<? i 0))
          (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))
        ; copy forward
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n))
          (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))))))


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


;; (vector-iterate vec proc) iterates on all elements of given vector vec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; (proc index elem) can call directly or indirectly functions
;; that inspect the vector(s) elements, and can also call (vector-set! vec ...).
;;
;; It must NOT call any function that modifies the vector's length, as for example
;; (vector-truncate!)
;;
;; If no vector is specified, the loop finishes when body ... evaluates to #f
;;
;; Returns value of last call to (proc index elem), or #t if (proc index elem) was never called.
(define vector-iterate
  (case-lambda
    ((vec start end proc)
      (assert* 'vector-iterate (fx<=?* 0 start end (vector-length vec)))
      (assert* 'vector-iterate (procedure? proc))
      (let %vector-iterate ((vec vec) (proc proc) (ret #t) (i start) (n end))
        (if (fx<? i n)
          (let ((ret (proc i (vector-ref vec i))))
            (and ret (%vector-iterate vec proc ret (fx1+ i) n)))
          ret)))
    ((vec proc)
      (vector-iterate vec 0 (vector-length vec) proc))))


;; Iterate in parallel on elements of given vector(s) v ..., and evaluate body ... on each element.
;; Stop iterating when the shortest vector is exhausted, or when body ... evaluates to #f
;; If no vector is specified, the loop finishes when body ... evaluates to #f
;;
;; Returns value of last body ... evaluation, or #t if body .. was never evaluated.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect or modify the vector(s) elements.
;; It must NOT call any function that modifies the vector(s) length, as for example (vector-truncate!)
(define-syntax for-vector
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(for () body ...))
      ((_ elem v body ...)
        (identifier? #'elem)
        #'(vector-iterate v (lambda0 (_ elem) body ...)))
      ((_ ((elem v)) body ...)
        #'(vector-iterate v (lambda0 (_ elem) body ...)))
      ((_ ((elem v) ...) body ...)
        (with-syntax (((tv ...) (generate-pretty-temporaries #'(v ...))))
          #'(let ((tv v) ...)
              (let %for-vector ((i 0) (n (fxmin (vector-length tv) ...)) (ret #t))
                (if (fx<? i n)
                  (let ((elem (vector-ref tv i)) ...)
                    (let ((ret (begin0 body ...)))
                      (and ret (%for-vector (fx1+ i) n ret))))))))))))


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
;;
;; Conforms to SRFI 43 Vector Library
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
;; stop at the first #f value returned by (proc elem ...) and return #f.
;;
;; If all calls to (proc elem ...) return truish, then return the last of such values.
;;
;; If not all vectors have the same length, iteration terminates when the end of shortest vector is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
;;
;; Conforms to SRFI 43 Vector Library
(define vector-every
  (case-lambda
    ((proc vec)
      (let %vector-every ((i 0) (n (vector-length vec)) (proc proc) (vec vec) (lastval #t))
        (if (fx>=? i n)
          lastval
          (let ((val (proc (vector-ref vec i))))
            (if val
              (%vector-every (fx1+ i) n proc vec val)
              #f)))))
    ((proc vec1 vec2)
      (let %vector-every ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2)))
                          (proc proc) (vec1 vec1) (vec2 vec2) (lastval #t))
        (if (fx>=? i n)
          lastval
          (let ((val (proc (vector-ref vec1 i) (vector-ref vec2 i))))
            (if val
              (%vector-every (fx1+ i) n proc vec1 vec2 val)
              #f)))))
    ((proc vec1 vec2 vec3)
      (let %vector-every ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3)))
                          (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3) (lastval #t))
        (if (fx>=? i n)
          lastval
          (let ((val (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i))))
            (if val
              (%vector-every (fx1+ i) n proc vec1 vec2 vec3 val)
              #f)))))
    ((proc vec1 vec2 vec3 vec4)
      (let %vector-every ((i 0) (n (fxmin (vector-length vec1) (vector-length vec2) (vector-length vec3) (vector-length vec4)))
                                (proc proc) (vec1 vec1) (vec2 vec2) (vec3 vec3) (vec4 vec4) (lastval #t))
        (if (fx>=? i n)
          lastval
          (let ((val (proc (vector-ref vec1 i) (vector-ref vec2 i) (vector-ref vec3 i) (vector-ref vec4 i))))
            (if val
              (%vector-every (fx1+ i) n proc vec1 vec2 vec3 vec4 val)
              #f)))))
    ((proc vec1 . vecs)
      (let %vector-every ((i 0) (n (apply fxmin (map vector-length (cons vec1 vecs))))
                          (proc proc) (vecs (cons vec1 vecs)) (lastval #t))
        (if (fx>=? i n)
          lastval
          (let ((val (%apply-proc proc i vecs)))
            (if val
              (%vector-every (fx1+ i) n proc vecs val)
              #f)))))))


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
;;
;; Conforms to SRFI 43 Vector Library
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
;; which must be cons cells, and inserts them into caller-provided hashtable htable:
;; (car cell) is used as key, and (cdr cell) is used ad value.
;
;; Returns htable.
(define (vector->hashtable! vec htable)
  (vector-iterate vec
    (lambda (i cell)
      (hashtable-set! htable (car cell) (cdr cell))))
  htable)



) ; close library
