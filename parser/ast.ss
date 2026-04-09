;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (schemesh parser ast (1 0 0))
  (export
          ast-car ast-cdr ast-null? ast-pair? ast-unwrap ast-unwrap1 ast-wrap/2 ast-wrap-list2 ast-wrap-list ast-wrap-vector)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)                annotation? annotation-expression annotation-source annotation-stripped
                                      source-object-bfp source-object-column source-object-line)
    (only (scheme2k bootstrap)        assert*)
    (only (scheme2k containers list)  map*)
    (only (scheme2k lineedit parser)  make-parsectx-annotation))


(define (ast-unwrap value)
  (if (annotation? value) (annotation-stripped value) value))


(define (ast-unwrap1 obj)
  (if (annotation? obj) (annotation-expression obj) obj))


(define (ast-car obj)
  (let ((obj (ast-unwrap1 obj)))
    (assert* 'ast-car (pair? obj))
    (car obj)))


(define (ast-cdr obj)
  (let ((obj (ast-unwrap1 obj)))
    (assert* 'ast-cdr (pair? obj))
    (cdr obj)))


(define (ast-pair? obj)
  (pair? (ast-unwrap1 obj)))


(define (ast-null? obj)
  (null? (ast-unwrap1 obj)))


(define (ast-source obj)
  (let ((obj (if (annotation? obj)
               obj
               (and (ast-pair? obj) (ast-car obj)))))
    (and (annotation? obj) (annotation-source obj))))


;; return two values:
;;   token value, as an annotation object
;;   token type
(define (ast-wrap/2 value type ctx x y beg)
  (values (make-parsectx-annotation ctx (ast-unwrap1 value) (ast-unwrap value) x y beg) type))


;; create and return an annotation wrapping (list value1 value2)
(define (ast-wrap-list2 ctx value1 value2)
  (let ((src (ast-source value1)))
    (make-parsectx-annotation
      ctx
      (list value1 value2)
      (list (ast-unwrap value1) (ast-unwrap value2))
      (or (source-object-column src) 1)
      (or (source-object-line   src) 1)
      (or (source-object-bfp    src) 0))))


;; create and return an annotation wrapping list l
(define (ast-wrap-list ctx l)
  (let* ((l1  (ast-unwrap1 l))
         (src (and (ast-pair? l1) (ast-source (ast-car l1)))))
    (make-parsectx-annotation
      ctx
      l
      (map* ast-unwrap l)
      (if src (source-object-column src) 1)
      (if src (source-object-line   src) 1)
      (if src (source-object-bfp    src) 0))))


;; create and return an annotation wrapping vector v
(define (ast-wrap-vector ctx v vals)
  (let* ((vals1 (ast-unwrap1 vals))
         (src   (and (ast-pair? vals1) (ast-source (ast-car vals1)))))
    (make-parsectx-annotation
      ctx
      v
      v
      (if src (source-object-column src) 1)
      (if src (source-object-line   src) 1)
      (if src (source-object-bfp    src) 0))))


) ; close library
