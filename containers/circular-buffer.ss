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
    circular-buffer          circular-buffer?        make-circular-buffer    vector->circular-buffer*
    circular-buffer-length   circular-buffer-clear!  circular-buffer-empty?  circular-buffer-full?
    circular-buffer-capacity circular-buffer-ref     circular-buffer-ref-right
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


;; create a circular-buffer wrapping specified vector and containing the elements in range [start, end)
;; changes to either the vector elements or to the circular-buffer elements propagates to each other.
(define vector->circular-buffer*
  (case-lambda
    ((v start end)
      (assert* 'vector->circular-buffer* (vector? v))
      (assert* 'vector->circular-buffer* (mutable-vector? v))
      (assert* 'vector->circular-buffer* (fx<=? 0 start end (vector-length v)))
      (%make-circular-buffer start (fx- end start) v))
    ((v)
      (vector->circular-buffer* v 0 (vector-length v)))))


;; create an empty circular-buffer, with capacity equal to n.
(define (make-circular-buffer n)
  (assert* 'make-circular-buffer* (fx>=? n 0))
  (%make-circular-buffer 0 0 (make-vector n #f)))


(define (circular-buffer-capacity cbuf)
  (assert* 'circular-buffer-capacity (circular-buffer? cbuf))
  (vector-length (circular-buffer-vec cbuf)))


(define (circular-buffer-clear! cbuf)
  (assert* 'circular-buffer-clear! (circular-buffer? cbuf))
  (circular-buffer-beg-set! cbuf 0)
  (circular-buffer-len-set! cbuf 0))


(define (circular-buffer-length cbuf)
  (assert* 'circular-buffer-length (circular-buffer? cbuf))
  (circular-buffer-len cbuf))


(define (circular-buffer-empty? cbuf)
  (assert* 'circular-buffer-empty? (circular-buffer? cbuf))
  (fxzero? (circular-buffer-length cbuf)))


(define (circular-buffer-full? cbuf)
  (assert* 'circular-buffer-full? (circular-buffer? cbuf))
  (fx>=? (circular-buffer-length cbuf) (circular-buffer-capacity cbuf)))


;; return i-th element from the left
(define (circular-buffer-ref cbuf i)
  (assert* 'circular-buffer-ref (circular-buffer? cbuf))
  (let ((len-1 (fx1- (circular-buffer-len cbuf))))
    (assert* 'circular-buffer-ref (fx<=? 0 i len-1)))
  (let* ((vec  (circular-buffer-vec cbuf))
         (cap  (vector-length vec))
         (pos0 (fx+ i (circular-buffer-beg cbuf)))
         (pos  (if (fx<? pos0 cap) pos0 (fx- pos0 cap))))
    (vector-ref vec pos)))


;; return i-th element from the right
(define (circular-buffer-ref-right cbuf i)
  (assert* 'circular-buffer-ref-right (circular-buffer? cbuf))
  (let ((len-1 (fx1- (circular-buffer-len cbuf))))
    (assert* 'circular-buffer-ref (fx<=? 0 i len-1))
    (circular-buffer-ref cbuf (fx- len-1 i))))


;; append one element to the right
;; raise condition if full
(define (circular-buffer-insert-right! cbuf obj)
  (assert-not* 'circular-buffer-insert-right! (circular-buffer-full? cbuf))
  (let* ((len  (circular-buffer-len cbuf))
         (end0 (fx+ (circular-buffer-beg cbuf) len))
         (vec  (circular-buffer-vec cbuf))
         (cap  (vector-length vec))
         (end  (if (fx<? end0 cap) end0 (fx- end0 cap))))
    (vector-set! vec end obj)
    (circular-buffer-len-set! cbuf (fx1+ len))))


;; remove first element from the left and return it
;; raise condition if empty
(define (circular-buffer-delete-left! cbuf)
  (assert-not* 'circular-buffer-delete-left! (circular-buffer-empty? cbuf))
  (circular-buffer-len-set! cbuf (fx1- (circular-buffer-len cbuf)))
  (let* ((beg   (circular-buffer-beg cbuf))
         (beg+1 (fx1+ beg)))
    (circular-buffer-beg-set! cbuf
      (if (fx<? beg+1 (circular-buffer-capacity cbuf)) beg+1 0))
    (vector-ref (circular-buffer-vec cbuf) beg)))


;; customize how circular-buffer objects are printed
(record-writer (record-type-descriptor circular-buffer)
  (lambda (cbuf port writer)
    (put-string port "(vector->circular-buffer* (vector")
    (let ((len (circular-buffer-length cbuf)))
      (do ((i 0 (fx1+ i)))
          ((fx>=? i len))
        (put-char port #\space)
        (writer (circular-buffer-ref cbuf i) port))
      (do ((i len (fx1+ i))
           (cap (circular-buffer-capacity cbuf)))
          ((fx>=? i cap))
        (put-string port " #f"))
      (put-string port ") 0 ")
      (writer len port))
    (put-string port ")")))

) ; close library
