;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;          define Scheme type "circular-buffer"        ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers circular-buffer (1 0 0))
  (export
    make-circular-buffer vector->circular-buffer*
    circular-buffer circular-buffer? circular-buffer-length circular-buffer-empty? circular-buffer-clear!
    circular-buffer-capacity       ; circular-buffer-ref circular-buffer-ref-right
    circular-buffer-delete-left!        circular-buffer-insert-right!
    ;; circular-buffer-delete-right!    circular-buffer-insert-left!
    )
  (import
    (rnrs)
    (only (chezscheme)                 fx1+ fx1- mutable-vector? record-writer)
    (only (scheme2k bootstrap)         assert* assert-not* fx<=?*))


(define-record-type (circular-buffer %make-circular-buffer circular-buffer?)
  (fields
     (mutable   beg)
     (mutable   len)
     (immutable vec))
  (nongenerative %circular-buffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; create a circular-buffer wrapping specified vector and containing its elements
(define (vector->circular-buffer* v)
  (assert* 'vector->circular-buffer* (vector? v))
  (assert* 'vector->circular-buffer* (mutable-vector? v))
  (%make-circular-buffer 0 (vector-length v) v))


;; create an empty circular-buffer, with capacity equal to n.
(define (make-circular-buffer n)
  (assert* 'make-circular-buffer* (fx>=? n 0))
  (%make-circular-buffer 0 n (make-vector n)))


(define (circular-buffer-capacity cbuf)
  (vector-length (circular-buffer-vec cbuf)))


(define (circular-buffer-clear! cbuf)
  (circular-buffer-beg-set! cbuf 0)
  (circular-buffer-len-set! cbuf 0))


(define circular-buffer-length circular-buffer-len)


(define (circular-buffer-empty? cbuf)
  (fxzero? (circular-buffer-length cbuf)))


;; insert one element, removing first element if full
(define (circular-buffer-insert-right! cbuf obj)
  (let* ((beg  (circular-buffer-beg cbuf))
         (len  (circular-buffer-len cbuf))
         (vec  (circular-buffer-vec cbuf))
         (cap  (vector-length cbuf))
         (pos0 (fx+ beg len))
         (pos  (if (fx<? pos0 cap) pos0 (fx- pos0 cap))))
    (vector-set! vec pos obj)
    (if (fx<? len cap)
      (circular-buffer-len-set! cbuf (fx1+ len))
      ;; circular-buffer is full
      (let ((beg+1 (fx1+ beg)))
        (circular-buffer-len-set! cbuf (if (fx<? beg+1 cap) beg+1 0))))))


;; remove and return first element
(define (circular-buffer-delete-left! cbuf)
  (assert-not* 'circular-buffer-delete-left! (circular-buffer-empty? cbuf))
  (let* ((pos   (circular-buffer-beg cbuf))
         (pos+1 (fx1+ pos))
         (len   (circular-buffer-len cbuf))
         (vec   (circular-buffer-vec cbuf))
         (cap   (vector-length cbuf))
         (obj   (vector-ref vec pos)))
    (circular-buffer-len-set! cbuf (fx1- len))
    (circular-buffer-beg-set! cbuf (if (fx<? pos+1 cap) pos+1 0))))


;; customize how circular-buffer objects are printed
(record-writer (record-type-descriptor circular-buffer)
  (lambda (sp port writer)
    (display "#<circular-buffer>" port)))


) ; close library
