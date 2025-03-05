;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   define Scheme type "bitmap", a fixed size bit vector   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers bitmap (0 8 1))
  (export
    bitmap make-bitmap bitmap? bitmap-length bitmap-ref bitmap-set! bitmap-last-zero)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer void)
    (only (schemesh bootstrap) assert*))

(define-record-type
  (%bitmap %make-bitmap bitmap?)
  (fields
    (immutable data       bitmap-data)   ; bytevector
    ; unsigned fixnum, length in bits
    (mutable   length     bitmap-length     bitmap-length-set!)
    ; unsigned fixnum, position of last zero, or -1 if all bits are one.
    (mutable   last-zero  bitmap-last-zero  bitmap-last-zero-set!))
  (nongenerative #{%bitmap f7pgyor7q9839cgjbgqhv381w-0}))


;; create a zero-filled bitmap with specified bit length.
(define (make-bitmap bitlength)
  (let ((byte-n (fxarithmetic-shift-right (fx+ bitlength 7) 3)))
    (%make-bitmap
      (make-bytevector byte-n 0)
      bitlength
      (fx1- bitlength))))


;; create a bitmap containing specified values. each value must be 0 or 1
(define (bitmap . vals)
  (let* ((n (length vals))
         (b (make-bitmap n)))
    (do ((i 0 (fx1+ i))
         (tail vals (cdr tail)))
        ((fx>=? i n) b)
      (bitmap-set! b i (car tail)))))


;; get index-th element of bitmap. returns 0 or 1.
(define (bitmap-ref b index)
  (assert* 'bitmap-ref (fx<? -1 index (bitmap-length b)))
  (let ((byte (bytevector-u8-ref (bitmap-data b) (fxarithmetic-shift-right index 3))))
    (fxand 1 (fxarithmetic-shift-right byte (fxand index 7)))))


;; set index-th element of bitmap to zero or one
(define (bitmap-set! b index zero-or-one)
  (assert* 'bitmap-set! (fx<? -1 index (bitmap-length b)))
  (let* ((byte-index (fxarithmetic-shift-right index 3))
         (data       (bitmap-data b))
         (old-byte   (bytevector-u8-ref data byte-index))
         (bit        (fxarithmetic-shift-left 1 (fxand index 7)))
         (set-zero?  (fxzero? zero-or-one))
         (new-byte
           (if set-zero?
             (fxand old-byte (fxnot bit))
             (fxior old-byte bit))))
    (unless (fx=? old-byte new-byte)
      (bytevector-u8-set! data byte-index new-byte)
      (bitmap-last-zero-update! b index set-zero?))))


(define (bitmap-last-zero-update! b index set-zero?)
  (if set-zero?
    (when (fx>? index (bitmap-last-zero b))
      (bitmap-last-zero-set! b index))
    (when (fx=? index (bitmap-last-zero b))
      (do ((i (fx1- index) (fx1- i)))
          ((or (fx<? i 0)
               (fxzero? (bitmap-ref b i)))
           (bitmap-last-zero-set! b i))))))


; customize how "bitmap" objects are printed
(record-writer (record-type-descriptor %bitmap)
  (lambda (b port writer)
    (display "(bitmap" port)
    (do ((i 0 (fx1+ i))
         (n (bitmap-length b)))
        ((fx>=? i n))
      (display #\space port)
      (display (bitmap-ref b i) port))
    (display ")" port)))

) ; close library
