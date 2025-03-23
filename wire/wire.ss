;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


#!r6rs

;;; Wire serialization/deserialization format.
;;;
;;; magic bytes: #x0 #xFF w i r e #x0 #x0
;;;              the last two bytes are version-lo version-hi
;;;
;;; each message is: length in bytes (wlen), type tag (wtag), datum
;;;
;;; wlen: 3 bytes LO MID HI, max is 2^24 - 1
;;;       indicates the number of bytes occupied by wlen + wtag + datum
;;;
;;; wtag: either 0 bytes => datum is '()
;;;           or 1 byte   in the range #x0 ... #xFE
;;;           or 4 bytes: #xFF followed by 3 bytes LO MID HI, max is 2^24 -1
;;;
;;;       0 => datum is fixnum 0
;;;       1 => datum is fixnum 1
;;;       2 => datum is fixnum 2
;;;       3 => datum is fixnum -1
;;;       4 => datum is exact integer, signed, little endian, datum-byte-n is wlen-1 bytes
;;;       5 => datum is exact ratio, encoded as 2 messages: numerator, denominator
;;;       6 => datum is exact complex, encoded as 2 messages: real, imag
;;;       7 => datum is flonum, 8 byte IEEE float64 little-endian
;;;       8 => datum is cflonum, two 8 byte IEEE float64 little-endian: real, imag
;;;       9 => datum is character, datum-byte-n is wlen-1 bytes
;;;      10 => datum is #f
;;;      11 => datum is #t
;;;      12 => datum is (void)
;;;      13 => datum is (eof-object)
;;;      14 => datum is (bwp-object)
;;;      15 UNUSED.
;;;      16 => datum is box:         followed by message
;;;      17 => datum is cons:        encoded as 2 messages: car, cdr
;;;      18 => datum is vector:      n encoded as wlen, followed by n messages
;;;      19 => datum is bytevector:  n is wlen-1, followed by n raw bytes
;;;      20 => datum is string:      n encoded as wlen, followed by characters each encoded as wlen
;;;      21 => datum is fxvector:    n encoded as wlen, followed by n messages
;;;      22 => datum is flvector:    n is (wlen-1)/8, followed by n * 8 IEEE float64 little-endian
;;;      23 UNUSED. stencil-vector?
;;;      24 => datum is symbol:      n encoded as wlen, followed by characters each encoded as wlen
;;;      25 => datum is eq-hashtable:  n encoded as wlen, followed by 2 * n messages
;;;      26 => datum is eqv-hashtable: n encoded as wlen, followed by 2 * n messages
;;;      27 => datum is equal-hashtable: equal function name encoded as message - must be a symbol, checked against a whitelist
;;;                                      followed by n encoded as wlen, followed by 2 * n messages
;;
;;;      32 => datum is span:        n encoded as wlen, followed by n messages
;;;      33 => datum is bytespan:    n is wlen-1, followed by n raw bytes
;;;      34 => datum is charspan:    n encoded as wlen, followed by characters each encoded as wlen
;;;      35 => datum is gbuffer:     n encoded as wlen, followed by n messages
;;;      36 => datum is bytegbuffer: NOT IMPLEMENTED
;;;      37 => datum is chargbuffer: n encoded as wlen, followed by characters each encoded as wlen



(library (schemesh wire (0 8 1))
  (export  wire-length wire-get wire-put)
  (import
    (rnrs)
    (only (chezscheme) box box? bwp-object? fxsrl unbox
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       bytevector-sint-ref        bytevector-sint-set!
                       bytevector-u8-ref          bytevector-u8-set!
                       bytevector-u24-ref         bytevector-u24-set!
                       fx1+ fxsrl
                       fxvector? fxvector-length fxvector-ref fxvector-set! make-fxvector
                       integer-length
                       void)
    (schemesh bootstrap flvector))

(define (wlen+wtag n ret)
  (fx+ (fx+ n ret) 4)) ; wlen + wtag is 4 bytes

(define (wire-length/exact-integer obj ret)
  (case obj
    ((0 1 2 -1)
      (wlen+wtag 0 ret)) ; only wlen + wtag
    (else
      (if (and (fixnum? obj) (fx<=? -128 obj 127))
        (wlen+wtag 1 ret)) ; wlen + wtag + 1-byte datum

        ;; datum is sign-extended, two's complement little-endian bytes
        (let ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3))))
          (wlen+wtag datum-byte-n ret)))))


(define (wire-length/flonum obj ret)
  (wlen+wtag 8 ret)) ; wlen + wtag + 8 bytes


(define (wire-length/cflonum obj)
  (wlen+wtag 16 ret)) ; wlen + wtag + 16 bytes


(define (wire-length/number obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (wire-length/exact-integer obj)
        ;; exact ratio: encoded as wlen, wtag, (numerator, denominator)
        (wire-length/sum
          (fx1+ (fx+ (wire-length/number (numerator obj))
                     (wire-length/number (denominator obj)))))
      ;; exact complex
      (wire-length/number (imag-part obj) (wire-length/number (real-part obj) (fx1+ ret))))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (wire-length/flonum obj ret)
      (wire-length/cflonum obj ret))))


(define (wire-length/char obj ret)
  (cond
    ((char<=? obj #\xFF)   (fx+ ret 2))   ;; 1-byte wlen + 1-byte wtag + 1 byte datum
    ((char<=? obj #\xFFFF) (fx+ ret 2))   ;; 1-byte wlen + 1-byte wtag + 2 byte datum
    (else                  (fx+ ret 3)))) ;; 1-byte wlen + 1-byte wtag + 3 byte datum


(define (wire-length/pair obj ret)
  (let ((wtag-and-datum-byte-n
          (wire-length2 (cdr obj) (wire-length2 (car obj) 1))))
    (wire-length/wlen wtag-and-datum-byte-n (fx+ wtag-and-datum-byte-n ret))))


(define (wire-length/box obj ret)
  (let ((wtag-and-datum-byte-n
          (wire-length2 (unbox obj) 1)))
    (wire-length/wlen wtag-and-datum-byte-n (fx+ wtag-and-datum-byte-n ret))))


(define (wire-length/vector obj ret)
  (let ((n (vector-length obj)))
    (let %wire-length/vector ((v obj) (i 0) (n n) (datum-byte-n (wire-length/wlen n 0)))
      (if (fx<? i n)
        (wire-length/vector v (fx1+ i) n (wire-length2 (vector-ref v i ret) datum-byte-n))
        ret))))


(define (wire-length/bytevector obj ret)
  (let ((datum-byte-n (bytevector-length obj)))
    (wire-length/wlen (fx1+ datum-byte-n) (fx+ ret datum-byte-n))))


(define (wire-length/fxvector obj ret)
  (let ((n (fxvector-length obj)))
    (let %wire-length/fxvector ((v obj) (i 0) (n n) (ret (wire-length/wlen n (fx1+ ret))))
      (if (fx<? i n)
        (wire-length/fxvector v (fx1+ i) n (wire-length/exact-integer (fxvector-ref v i ret)))
        ret))))


(define (wire-length/flvector obj ret)
  (let ((datum-byte-n (8 * (flvector-length obj))))
    (wire-length/wlen (fx1+ datum-byte-n) (fx+ ret datum-byte-n))))


(define (wire-length/string obj ret)
  (let ((n (string-length obj)))
    (let %wire-length/string ((s obj) (i 0) (n n) (ret (wire-length/wlen n (fx1+ ret))))
      (if (fx<? i n)
        (wire-length/fxvector v (fx1+ i) n (wire-length/exact-integer (fxvector-ref v i ret)))
        ret))))



(define (wire-length/symbol obj ret)
  (wire-length/string (symbol->string obj) ret))

;; recursively traverse obj and return the number of bytes needed to serialize obj
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (wire-length obj)
  (case obj
    ((())             1) ;; just 1-byte wlen == 0
    ((0 1 2 -1 #f #t) 2) ;; 1-byte wlen + 1-byte wtag
    (else
      (cond
        ((fixnum? obj)     (wire-length/exact-integer obj))
        ((char?   obj)     (wire-length/char   obj))
        ((flonum? obj)     (wire-length/flonum obj))
        ((pair?   obj)     (wire-length/pair   obj))
        ((symbol? obj)     (wire-length/symbol obj))
        ((eq? (void) obj)  2) ;; 1-byte wlen + 1-byte wtag
        ((eof-object? obj) 2) ;; 1-byte wlen + 1-byte wtag
        ((bwp-object? obj) 2) ;; 1-byte wlen + 1-byte wtag
        ((procedure? obj) #f)
        ;; these are slower to check
        ((box?    obj)     (wire-length/box        obj))
        ((number? obj)     (wire-length/number     obj))
        ((vector? obj)     (wire-length/vector     obj))
        ((bytevector? obj) (wire-length/bytevector obj))
        ((fxvector? obj)   (wire-length/fxvector   obj))
        ((flvector? obj)   (wire-length/flvector   obj))
        ((string? obj)     (wire-length/string     obj))
        ((record? obj)     (wire-length/record     obj))
        (else              #f)))))


;; recursively traverse obj and return the number of bytes needed to serialize it.
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (wire-length obj)
  (wire-length2 obj 0))

) ; close library
