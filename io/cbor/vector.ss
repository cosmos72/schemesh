;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/cbor/cbor.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put/bytevector bsp obj)
  (let ((n (bytevector-length obj)))
    (put/count bsp n major-bytestring)
    (let ((pos (grow bsp n)))
      (bytevector-copy! obj 0 (bytes bsp) pos n))))

#|
(define (get/bytevector bsp end)
  (let-values (((n pos) (get/vlen bsp end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-bytevector n)))
        (bytevector-copy! bsp ret 0 n)
        (values ret (fx+ pos n)))
      (values #f #f))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put/string bsp obj)
  (put/count bsp (string->utf8b-length obj) major-utf8string)
  (bytespan-insert-right/string! bsp obj))

#|
(define (get/string bsp end)
  (let-values (((n pos) (get/vlen bsp end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-bytevector n)))
        (bytevector-copy! bsp ret 0 n)
        (values ret (fx+ pos n)))
      (values #f #f))))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put/flvector bsp obj)
  ;; TODO: put a flvector tag
  (let ((n (flvector-length obj)))
    (put/count bsp n major-array)
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (put/flonum bsp (flvector-ref obj i)))))

#|
(define (get/flvector bsp end)
  (let-values (((n pos) (get/vlen bsp end)))
    (if (and pos (fx<=? (fx* n len-flonum) (fx- end pos)))
      (do ((ret (make-flvector n))
           (i 0 (fx1+ i))
           (pos pos (fx+ pos len-flonum)))
          ((fx>=? i n)
            (values ret pos))
        (flvector-set! ret i (bytevector-ieee-double-ref bsp endian)))
      (values #f #f))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put/fxvector bsp obj)
  ;; TODO: put a fxvector tag
  (let ((n (fxvector-length obj)))
    (put/count bsp n major-array)
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (put/integer bsp (fxvector-ref obj i)))))

#|
(define (get/fxvector bsp end)
  (let-values (((n pos) (get/vlen bsp end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let %get/fxvector ((ret (make-fxvector n)) (i 0) (pos pos))
        (cond
          ((or (not pos) (fx>? (fx- pos i) (fx- end n)))
            (values #f #f))
          ((fx<? i n)
            (let-values (((elem pos) (%get/exact-int bsp end)))
              ;; (debugf "...get/fxvector i=~s n=~s elem=~s pos=~s end=~s" i n elem pos end)
              (if (and (fixnum? elem) pos)
                (begin
                  (fxvector-set! ret i elem)
                  (%get/fxvector ret (fx1+ i) pos))
                (values #f #f))))
          (else
            (values ret pos))))
      (values #f #f))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put/vector bsp obj)
  (let ((n (vector-length obj)))
    (put/count bsp n major-array)
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (put/any bsp (vector-ref obj i)))))

#|
(define (get/vector bsp end)
  (let-values (((n pos) (get/vlen bsp end)))
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
              (let-values (((elem pos) (get/any bsp end)))
                (vector-set! ret i elem)
                (%get/vector (fx1+ i) pos))))))
      (values #f #f))))
|#

