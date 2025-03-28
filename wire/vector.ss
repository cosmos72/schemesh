;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file wire/wire.ss

(define (len/vector pos obj)
  (let ((n (vector-length obj)))
    (let %len/vector ((i 0) (pos (vlen+ n (tag+ pos)))) ; n is encoded as vlen
      (if (and pos (fx<? i n))
        (%len/vector (fx1+ i) (len/any pos (vector-ref obj i)))
        pos))))

(define (put/vector bv pos obj)
  (let* ((n    (vector-length obj))
         (end0 (put/tag  bv pos tag-vector))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (let %put/vector ((i 0) (pos end1))
      (if (and pos (fx<? i n))
        (%put/vector (fx1+ i) (put/any bv pos (vector-ref obj i)))
        pos))))

(define (get/vector bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-vector n)))
        (let %get/vector ((i 0) (pos pos))
          ;; (debugf "...get/vector i=~s n=~s pos=~s end=~s" i n pos end)
          (cond
            ((or (not pos) (fx>? (fx- pos i) (fx- end n)))
              (values #f #f))
            ((fx>=? i n)
              (values ret pos))
            (else
              (let-values (((elem pos) (get/any bv pos end)))
                (vector-set! ret i elem)
                (%get/vector (fx1+ i) pos))))))
      (values #f #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (len/bytevector pos bv)
  (let ((n (bytevector-length bv)))
    (vlen+ n (tag+ pos) n)))

(define (put/bytevector bv pos obj)
  (let* ((n    (bytevector-length obj))
         (end0 (put/tag  bv pos tag-bytevector))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (if end1
      (begin
        (bytevector-copy! obj 0 bv end1 n)
        (fx+ end1 n))
      #f)))

(define (get/bytevector bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-bytevector n)))
        (bytevector-copy! bv pos ret 0 n)
        (values bv (fx+ pos n) end)))
    (values #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (len/fxvector pos obj)
  (let ((n (fxvector-length obj)))
    (let %len/fxvector ((i 0) (pos (vlen+ n (tag+ pos)))) ; n is encoded as clen
      (if (and pos (fx<? i n))
        (%len/fxvector (fx1+ i) (len/exact-sint pos (fxvector-ref obj i)))
        pos))))

(define (put/fxvector bv pos obj)
  (let* ((n (fxvector-length obj))
         (end0 (put/tag  bv pos tag-fxvector))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (let %put/vector ((i 0) (pos end1))
      (if (and pos (fx<? i n))
        (%put/vector (fx1+ i) (put/exact-sint bv pos (fxvector-ref obj i)))
        pos))))

(define (get/fxvector bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let %get/fxvector ((ret (make-fxvector n)) (i 0) (pos pos))
        (cond
          ((or (not pos) (fx>? (fx- pos i) (fx- end n)))
            (values #f #f))
          ((fx<? i n)
            (let-values (((elem pos) (%get/exact-int bv pos end)))
              ;; (debugf "...get/fxvector i=~s n=~s elem=~s pos=~s end=~s" i n elem pos end)
              (if (and (fixnum? elem) pos)
                (begin
                  (fxvector-set! ret i elem)
                  (%get/fxvector ret (fx1+ i) pos))
                (values #f #f))))
          (else
            (values ret pos))))
      (values #f #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (len/flvector pos obj)
  (let ((n (flvector-length obj)))
    (vlen+ n (tag+ pos) (fx* n len-flonum))))

(define (put/flvector bv pos obj)
  (let* ((n (flvector-length obj))
         (end0 (put/tag  bv pos tag-flvector))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos len-flonum)))
        ((fx>=? i n)
           pos)
      (bytevector-ieee-double-set! bv pos (flvector-ref obj i) endian))))

(define (get/flvector bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? (fx* n len-flonum) (fx- end pos)))
      (do ((ret (make-flvector n))
           (i 0 (fx1+ i))
           (pos pos (fx+ pos len-flonum)))
          ((fx>=? i n)
            (values ret pos))
        (flvector-set! ret i (bytevector-ieee-double-ref bv pos endian)))
      (values #f #f))))
