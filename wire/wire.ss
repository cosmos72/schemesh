;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


#!r6rs

;;; Wire serialization/deserialization format.
;;;
;;; magic bytes: #x6 #x0 #x0 #xFF w i r e #x0 #x0
;;;              the last two bytes are version-lo version-hi
;;;
;;; each message is: dlen + payload (tag + datum)
;;;
;;; dlen: 3 bytes LO MID HI, max is 2^24 - 1
;;;       indicates the number of bytes occupied by tag + datum
;;;       does NOT include the number of bytes occupied by dlen
;;;
;;; tag: 0 byte => datum is (void)
;;;      1 byte => indicates the type of datum and possibly its value
;;;       0 => datum is fixnum 0
;;;       1 => datum is fixnum 1
;;;       2 => datum is fixnum 2
;;;       3 => datum is fixnum 3
;;;       4 => datum is fixnum 4
;;;       5 => datum is fixnum 5
;;;       6 => datum is fixnum 6
;;;       7 => datum is fixnum 7
;;;       8 => datum is fixnum 8
;;;       9 => datum is fixnum 9
;;;      10 => datum is fixnum 10
;;;      11 => datum is fixnum -5
;;;      12 => datum is fixnum -4
;;;      13 => datum is fixnum -3
;;;      14 => datum is fixnum -2
;;;      15 => datum is fixnum -1
;;;      16 => datum is 1-byte signed exact integer
;;;      17 => datum is 2-byte signed exact integer, little endian
;;;      18 => datum is 3-byte signed exact integer, little endian
;;;      19 => datum is 4-byte signed exact integer, little endian
;;;      20 => datum is dlen followed by dlen bytes: signed exact integer, little endian
;;;      21 => datum is exact ratio, encoded as 2 tag+datum: numerator, denominator
;;;      22 => datum is exact complex, encoded as 2 tag+datum: real, imag
;;;      23 => datum is flonum, 8 byte IEEE float64 little-endian
;;;      24 => datum is cflonum, two 8 byte IEEE float64 little-endian: real, imag
;;;      25 => datum is #f
;;;      26 => datum is #t
;;;      27 => datum is '()
;;;      28 => datum is (void) needed inside a message, where 0-byte tag cannot be represented
;;;      29 => datum is (eof-object)
;;;      30 => datum is (bwp-object)
;;;      31 => datum is character:         1 byte
;;;      32 => datum is character:         2 byte, little endian
;;;      33 => datum is character:         3 bytes, little endian
;;;      34 => datum is box:               followed by unboxed tag+datum
;;;      35 => datum is pair:              encoded as 2 tag+datum: car, cdr
;;;      36 => datum is one-element list:  encoded as tag+datum: car
;;;      37 => datum is improper list:     n encoded as dlen, followed by n tag+datum: elements
;;;      38 => datum is proper list:       n encoded as dlen, followed by n tag+datum: elements
;;;      39 => datum is vector:        n encoded as dlen, followed by n tag+datum
;;;      40 => datum is bytevector:    n encoded as dlen, followed by n bytes
;;;      41 => datum is string8:       n encoded as dlen, followed by characters each encoded as 1 byte
;;;      42 => datum is string:        n encoded as dlen, followed by characters each encoded as 3 bytes
;;;      43 => datum is fxvector:      n encoded as dlen, followed by n tag+datum
;;;      44 => datum is flvector:      n encoded as dlen, followed by n IEEE float64 little-endian, each occupying 8 bytes
;;;      46 => datum is symbol8:       n encoded as dlen, followed by characters each encoded as 1 byte
;;;      46 => datum is symbol:        n encoded as dlen, followed by characters each encoded as 3 bytes
;;;      47 => datum is eq-hashtable:  n encoded as dlen, followed by 2 * n tag+datum
;;;      48 => datum is eqv-hashtable: n encoded as dlen, followed by 2 * n tag+datum
;;;      49 => datum is equal-hashtable: equal function name encoded as message - must be a symbol, checked against a whitelist
;;;                                      followed by n encoded as dlen, followed by 2 * n tag+datum
;;;
;;;      50 => datum is status:      followed by kind encoded as fixnum tag+datum, followed by values encoded as list tag+datum
;;;      51 => datum is span:        n encoded as dlen, followed by n tag+datum
;;;      52 => datum is bytespan:    n encoded as dlen, followed by n bytes
;;;      53 => datum is charspan:    n encoded as dlen, followed by n characters each encoded as dlen
;;;      54 => datum is gbuffer:     n encoded as dlen, followed by n tag+datum
;;;      55 => datum is bytegbuffer: NOT IMPLEMENTED
;;;      56 => datum is chargbuffer: n encoded as dlen, followed by n characters each encoded as dlen
;;;     254 => datum is magic string: bytes #\w #\i #\r #\e VERSION-LO VERSION-HI
;;;     255 => datum starts with extended tag


(library (schemesh wire (0 8 1))
  (export  wire-length wire-get wire-put wire-put->bytevector)
  (import
    (rnrs)
    (only (chezscheme) box box? bwp-object? fxsrl unbox
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       bytevector-sint-ref        bytevector-sint-set!
                       bytevector-s24-ref         bytevector-s24-set!
                       bytevector-u24-ref         bytevector-u24-set!
                       fx1+ fxsrl fxsll
                       fxvector? fxvector-length fxvector-ref fxvector-set! make-fxvector
                       integer-length meta-cond
                       void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers flvector)
    (schemesh containers bytespan))


(define dlen 3)     ; dlen is encoded as 3 bytes
(define dlen+tag 4) ; dlen+tag is encoded as 4 bytes

(define len-tag 1)  ; tag is encoded as 1 byte

(define max-len #xFFFFFF)  ; maximum length of tag + datum

(define tag-0         0)
(define tag-1         1)
(define tag-2         2)
(define tag-3         3)
(define tag-4         4)
(define tag-5         5)
(define tag-6         6)
(define tag-7         7)
(define tag-8         8)
(define tag-9         9)
(define tag-10       10)
(define tag--5       11)
(define tag--4       12)
(define tag--3       13)
(define tag--2       14)
(define tag--1       15)
(define tag-s8       16) ; exact integer, signed, 8 bit
(define tag-s16      17) ; exact integer, signed, 16 bit, little endian
(define tag-s24      18) ; exact integer, signed, 24 bit, little endian
(define tag-s32      19) ; exact integer, signed, 32 bit, little endian
(define tag-sint     20) ; dlen followed by: exact integer, signed, dlen bytes, little endian
(define tag-ratio    21) ; exact ratio
(define tag-complex  22) ; exact complex
(define tag-flonum   23)
(define tag-cflonum  24)
(define tag-f        25)
(define tag-t        26)
(define tag-nil      27)
(define tag-void     28)
(define tag-eof      29)
(define tag-bwp      30)
(define tag-char8    31) ; char, 8 bit
(define tag-char16   32) ; char, 16 bit, little endian
(define tag-char24   33) ; char, 24 bit, little endian
(define tag-box      34)
(define tag-pair     35)
(define tag-list1    36)
(define tag-list*    37)
(define tag-list     38)
(define tag-vector   39)
(define tag-bvector  40) ; bytevector
(define tag-string8  41)
(define tag-string   42)
(define tag-fxvector 43)
(define tag-flvector 44)
(define tag-symbol8  45)
(define tag-symbol   46)

(define max-len-char 3) ;; each character is encoded as <= 3 bytes
(define len-flonum   8) ;; each flonum is encoded as 8 bytes
(define len-cflonum 16) ;; each cflonum is encoded as 16 bytes

(define endian (endianness little))

(define (valid-payload-len? n)
  (and (fixnum? n) (fx<=? 0 n max-len)))

(define dlen+
  (case-lambda
    ((pos)
      (and (fixnum? pos) (fx+ pos dlen)))
    ((pos n)
      (and (fixnum? pos) (fixnum? n) (fx+ (fx+ pos n) dlen)))))

(define tag+
  (case-lambda
    ((pos)
      (and (fixnum? pos) (fx+ pos len-tag)))
    ((pos n)
      (and (fixnum? pos) (fixnum? n) (fx+ (fx+ pos n) len-tag)))))

;; write one signed byte into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s8 bv pos s8)
  (bytevector-s8-set! bv pos s8)
  (fx1+ pos))

;; write one byte into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u8 bv pos u8)
  (bytevector-u8-set! bv pos u8)
  (fx1+ pos))

;; write exact signed integer as 2 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s16 bv pos s16)
  (bytevector-s16-set! bv pos s16 endian)
  (fx+ pos 2))

;; write exact unsigned integer as 2 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u16 bv pos u16)
  (bytevector-u16-set! bv pos u16 endian)
  (fx+ pos 2))

;; write exact unsigned integer as 3 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s24 bv pos s24)
  (bytevector-s24-set! bv pos s24 endian)
  (fx+ pos 3))

;; write exact unsigned integer as 3 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u24 bv pos u24)
  (bytevector-u24-set! bv pos u24 endian)
  (fx+ pos 3))

;; write exact unsigned integer as 4 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u32 bv pos u32)
  (bytevector-u32-set! bv pos u32 endian)
  (fx+ pos 4))

;; write message header or dlen into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/dlen bv pos count)
  (put/u24 bv pos count))

;; write tag (1 byte) into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/tag bv pos tag)
  (put/u8 bv pos tag))

(define (len/exact-int pos obj)
  (cond
    ((not pos)
      pos)
    ((and (fixnum? obj) (fx<=? #x-800000 obj #x7fffff))
      (cond
        ((fx<=? -5 obj 10)
          (tag+ pos)) ; only tag, datum is 0 bytes
        ((fx<=? #x-80 obj #x7f)
          (tag+ pos 1)) ; 1-byte datum
        ((fx<=? #x-8000 obj #x7fff)
          (tag+ pos 2)) ; 2-byte datum
        (else
          (tag+ pos 3)))) ; 3-byte datum
    (else
      ;; datum is exact integer, sign-extended, two's complement little-endian
      (let ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3))))
          (tag+ pos datum-byte-n)))))


;; write header and exact integer into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/exact-int bv pos obj)
  (cond
    ((not pos)
      pos)
    ((and (fixnum? obj) (fx<=? #x-800000 obj #x7fffff))
      (cond
        ((fx<=? 0 obj 10)
          (put/tag bv pos (fx+ tag-0 obj))) ; only tag, datum is 0 bytes
        ((fx<=? -5 obj -1)
          (put/tag bv pos (fx+ tag--1 (fx1+ obj)))) ; only tag, datum is 0 bytes
        ((fx<=? #x-80 obj #x7f)
          (let ((pos (put/tag bv pos tag-s8)))
            (put/s8 bv pos obj)))   ; 1-byte datum
        ((fx<=? #x-8000 obj #x7fff)
          (let ((pos (put/tag bv pos tag-s16)))
            (put/s16 bv pos obj)))  ; 2-byte datum
        (else
          (let ((pos (put/tag bv pos tag-s24)))
            (put/s24 bv pos obj)))))  ; 3-byte datum
    (else
      ;; datum is exact integer, sign-extended, two's complement little-endian
      (let* ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3)))
             (pos (put/tag bv pos tag-sint)))
        (bytevector-sint-set! bv pos obj endian datum-byte-n)
        (fx+ pos datum-byte-n)))))


(define (len/flonum pos obj)
  (tag+ pos len-flonum))

(define (put/flonum bv pos obj)
  (let ((pos (put/tag bv pos tag-flonum)))
    (bytevector-ieee-double-set! bv pos obj endian)
    (fx+ pos len-flonum)))


(define (len/cflonum pos obj)
  (tag+ pos len-cflonum))

(define (put/cflonum bv pos obj)
  (let ((pos (put/tag bv pos tag-cflonum)))
    (bytevector-ieee-double-set! bv pos (real-part obj) endian)
    (let ((pos (fx+ pos len-flonum)))
      (bytevector-ieee-double-set! bv pos (imag-part obj) endian)
      (fx+ pos len-flonum))))


(define (len/number pos obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (len/exact-int pos obj)
        ;; exact ratio: encoded as tag, numerator, denominator
        (tag+
          (len/exact-int (denominator obj)
             (len/exact-int (numerator obj) pos))))
      ;; exact complex: encoded as tag, real-part, imag-part
      (tag+
        (len/number (imag-part obj)
          (len/number (real-part obj) pos))))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (len/flonum pos obj)
      (len/cflonum pos obj))))


(define (put/number bv pos obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (put/exact-int bv pos obj)
        ;; exact ratio: encoded as tag, (numerator, denominator)
        (let* ((end0 (put/tag bv pos tag-ratio))
               (end1 (put/exact-int bv end0 (numerator obj)))
               (end2 (put/exact-int bv end1 (denominator obj))))
          end2))
      ;; exact complex: encoded as tag, (real-part, imag-part)
      (let* ((end0 (put/tag bv pos tag-complex))
             (end1 (put/number bv end0 (real-part obj)))
             (end2 (put/number bv end1 (imag-part obj))))
        end2))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (put/flonum bv pos obj)
      (put/cflonum bv pos obj))))


(define (len/char pos obj)
  (tag+ pos
    (cond ((char<=? obj #\xFF)   1)
          ((char<=? obj #\xFFFF) 2)
          (else                  3))))

;; write header and one character into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/char bv pos obj)
  (cond
    ((char<=? obj #\xFF)
       (let ((pos (put/tag bv pos tag-char8)))
         (put/u8 bv pos (char->integer obj))))
    ((char<=? obj #\xFFFF)
       (let ((pos (put/tag bv pos tag-char16)))
         (put/u16 bv pos (char->integer obj))))
    (else
       (let ((pos (put/tag bv pos tag-char24)))
         (put/u24 bv pos (char->integer obj))))))


(define (len/box pos obj)
  (tag+
    (len/any pos (unbox obj))))

(define (put/box bv pos obj)
  (let* ((end0 (put/tag bv pos tag-box))
         (end1 (put/any bv end0 (unbox obj))))
    end1))


;; 1-element proper list
(define (len/list1 pos obj)
  (tag+ (len/any pos (car obj))))

;; 1-element proper list
(define (put/list1 bv pos obj)
  (let* ((end0 (put/tag bv pos tag-list1))
         (end1 (put/any bv end0 (car obj))))
    end1))

;; proper or improper list
(define (len/list pos obj)
  (let %len/list ((pos (dlen+ (tag+ pos))) (l obj)) ; n is encoded as dlen
    (cond
      ((not pos)
        pos)
      ((null? l)
        pos)
      ((pair? l)
        (%len/list (len/any pos (car l)) (cdr l)))
      (else
        ;; last element of an improper list
        (len/any pos l)))))


(define (put/list bv pos obj)
  (let ((end0 (tag+ pos)))
    (let %put/list ((end (dlen+ end0)) (n 0) (l obj)) ; n is encoded as dlen
      ;; (debugf "%put-list l=~s n=~s end=~s" l n end)
      (cond
        ((not end)
          end)
        ((null? l)
          (put/tag bv pos tag-list)
          (put/u24 bv end0 n) ; n is encoded as dlen
          end)
        ((pair? l)
          (let ((end (put/any bv end (car l))))
            (%put/list end (fx1+ n) (cdr l))))
        (else
          ;; last element of an improper list
          (let ((end (put/any bv end obj)))
            (put/tag bv pos tag-list*)
            (put/u24 bv end0 n) ; n is encoded as dlen
            end))))))


(define (len/pair pos obj)
  (let ((tail (cdr obj)))
    (cond
      ((null? tail)
        (len/list1 pos obj))
      ((pair? tail)
        (len/list pos obj))
      (else
        (tag+
          (len/any tail
            (len/any pos (car obj))))))))


(define (put/pair bv pos obj)
  (let ((tail (cdr obj)))
    (if (or (null? tail) (pair? tail))
      (put/list bv pos obj)
      (let* ((end0 (put/tag bv pos tag-pair))
             (end1 (put/any bv end0 (car obj)))
             (end2 (put/any bv end1 (cdr obj))))
        end2))))


(define (len/vector pos obj)
  (let ((n (vector-length obj))
        (v obj))
    (let %len/vector ((i 0) (pos (dlen+ (tag+ pos)))) ; n is encoded as dlen
      (if (and pos (fx<? i n))
        (%len/vector (fx1+ i) (len/any pos (vector-ref v i)))
        pos))))

(define (put/vector bv pos obj)
  (let* ((n (vector-length obj))
         (v obj)
         (end0 (put/tag  bv pos tag-vector))
         (end1 (put/dlen bv end0 n))) ; n is encoded as dlen
    (let %put/vector ((i 0) (pos end1))
      (if (and pos (fx<? i n))
        (%put/vector (fx1+ i) (put/any bv pos (vector-ref v i)))
        pos))))


(define (len/bytevector pos obj)
  (let ((n (bytevector-length obj)))
    (dlen+ (tag+ pos) n)))

(define (put/bytevector bv pos obj)
  (let* ((n     (bytevector-length obj))
         (end0 (put/tag  bv pos tag-bvector))
         (end1 (put/dlen bv end0 n))) ; n is encoded as dlen
    (bytevector-copy! obj 0 bv end1 n)
    (fx+ end1 n)))


(define (len/fxvector pos obj)
  (let ((n (fxvector-length obj))
        (v obj))
    (let %len/fxvector ((i 0) (pos (dlen+ (tag+ pos)))) ; n is encoded as dlen
      (if (and pos (fx<? i n))
        (%len/fxvector (fx1+ i) (len/exact-int pos (vector-ref v i)))
        pos)))) ; n is encoded as dlen

(define (put/fxvector bv pos obj)
  (let* ((n (fxvector-length obj))
         (v obj)
         (end0 (put/tag  bv pos tag-fxvector))
         (end1 (put/dlen bv end0 n))) ; n is encoded as dlen
    (let %put/vector ((i 0) (pos end1))
      (if (and pos (fx<? i n))
        (%put/vector (fx1+ i) (put/exact-int bv pos (fxvector-ref v i)))
        pos))))


(define (len/flvector pos obj)
  (let ((n (flvector-length obj)))
    (dlen+ (tag+ pos) (fx* n len-flonum))))

(define (put/flvector bv pos obj)
  (let* ((n (flvector-length obj))
         (v obj)
         (end0 (put/tag  bv pos tag-flvector))
         (end1 (put/dlen bv end0 n))) ; n is encoded as dlen
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos len-flonum)))
        ((fx>=? i n)
           pos)
      (bytevector-ieee-double-set! bv pos (flvector-ref v i) endian))))



(define (len/string pos obj)
  (let ((n (string-length obj)))
    ;; n is encoded as dlen
    ;; each character is max-len-char bytes
    (dlen+ (tag+ pos (fx* n max-len-char)))))

(define (put/string bv pos obj)
  (let* ((n (string-length obj))
         (v obj)
         (end0 (put/tag  bv pos tag-string))
         (end1 (put/dlen bv end0 n))) ; n is encoded as dlen
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos max-len-char)))
        ((fx>=? i n)
           pos)
      (put/u24 bv pos (char->integer (string-ref v i))))))


(define (len/symbol pos obj)
  (len/string pos (symbol->string obj)))

(define (put/symbol bv pos obj)
  (let ((end (put/string bv pos (symbol->string obj))))
    (put/tag bv pos tag-symbol)
    end))


(define (len/hashtable pos obj)
  #f) ;; TODO: implement

(define (put/hashtable bv pos obj)
  #f) ;; TODO: implement


(define (len/record pos obj)
  #f) ;; TODO: implement

(define (put/record bv pos obj)
  #f) ;; TODO: implement


;; recursively traverse obj and return the number of bytes needed to serialize obj
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (len/any pos obj)
  (case obj
    ((0 1 2 3 4 5 6 7 8 9 10 -5 -4 -3 -2 -1 #f #t ())
      (tag+ pos)) ; only tag, datum is 0 bytes
    (else
      (cond
        ((not pos)         #f)
        ((fixnum? obj)     (len/exact-int  pos obj))
        ((char?   obj)     (len/char       pos obj))
        ((flonum? obj)     (len/flonum     pos obj))
        ((pair?   obj)     (len/pair       pos obj))
        ((symbol? obj)     (len/symbol     pos obj))
        ((eq? (void) obj)  (tag+ pos)) ; only tag, datum is 0 bytes
        ((eof-object? obj) (tag+ pos)) ; only header, datum is 0 bytes
        ((bwp-object? obj) (tag+ pos)) ; only header, datum is 0 bytes
        ((procedure? obj)   #f)
        ;; these are slower to check
        ((box?    obj)     (len/box        pos obj))
        ((number? obj)     (len/number     pos obj))
        ((vector? obj)     (len/vector     pos obj))
        ((bytevector? obj) (len/bytevector pos obj))
        ((fxvector? obj)   (len/fxvector   pos obj))
        ((flvector? obj)   (len/flvector   pos obj))
        ((string? obj)     (len/string     pos obj))
        ((hashtable? obj)  (len/hashtable  pos obj))
        ((record? obj)     (len/record     pos obj))
        (else              #f)))))


;; recursively traverse obj, serialize it and write it into bytevector bv starting at position pos.
;; return updated position, or #f on errors.
(define (put/any bv pos obj)
  (case obj
    ((#f)
      (put/tag bv pos tag-f)) ; only tag, datum is 0 bytes
    ((#t)
      (put/tag bv pos tag-t)) ; only tag, datum is 0 bytes
    ((())
      (put/tag bv pos tag-nil)) ; only tag, datum is 0 bytes
    (else
      (cond
        ((not pos)         #f)
        ((fixnum? obj)     (put/exact-int  bv pos obj))
        ((char?   obj)     (put/char       bv pos obj))
        ((flonum? obj)     (put/flonum     bv pos obj))
        ((pair?   obj)     (put/pair       bv pos obj))
        ((symbol? obj)     (put/symbol     bv pos obj))
        ((eq? (void) obj)  (put/tag bv pos tag-void)) ; only tag, datum is 0 bytes
        ((eof-object? obj) (put/tag bv pos tag-eof))  ; only tag, datum is 0 bytes
        ((bwp-object? obj) (put/tag bv pos tag-bwp))  ; only tag, datum is 0 bytes
        ((procedure? obj)   #f)
        ;; these are slower to check
        ((box?    obj)     (put/box        bv pos obj))
        ((number? obj)     (put/number     bv pos obj))
        ((vector? obj)     (put/vector     bv pos obj))
        ((bytevector? obj) (put/bytevector bv pos obj))
        ((fxvector? obj)   (put/fxvector   bv pos obj))
        ((flvector? obj)   (put/flvector   bv pos obj))
        ((string? obj)     (put/string     bv pos obj))
        ((hashtable? obj)  (put/hashtable  bv pos obj))
        ((record? obj)     (put/record     bv pos obj))
        (else              #f)))))


;; recursively traverse obj and return the number of bytes needed to serialize it.
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (wire-length obj)
  (if (eq? (void) obj)
    dlen
    (let ((pos (len/any dlen obj))) ; header is dlen bytes
      (if (valid-payload-len? pos) pos #f))))


;; recursively traverse obj, serialize it and append it to bytespan bsp.
;; return number of written bytes, or #f on errors.
(define wire-put
  (case-lambda
    ((bsp obj message-wire-len)
      (assert* 'wire-put (bytespan? bsp))
      (if (and (fixnum? message-wire-len) (valid-payload-len? (fx- message-wire-len dlen)))
        (let ((payload-wire-len (fx- message-wire-len dlen))
              (len-before (bytespan-length bsp)))
          (bytespan-resize-right! bsp (fx+ len-before message-wire-len))
          (let* ((bv   (bytespan-peek-data bsp))
                 (pos  (fx+ len-before (bytespan-peek-beg bsp)))
                 (end0 (put/dlen bv pos payload-wire-len))
                 (end  (if (eq? (void) obj)
                          end0
                          (put/any bv end0 obj))))
            (assert* 'wire-put (fx=? end (bytespan-length bsp)))
            message-wire-len))
        #f))
    ((bsp obj)
      (wire-put bsp obj (wire-length obj)))))

;; recursively traverse obj, serialize it and return bytevector containing serialized bytes,
;; or #f on errors
(define (wire-put->bytevector obj)
  (let ((bsp (bytespan)))
    (if (wire-put bsp obj)
      (bytespan->bytevector*! bsp)
      #f)))


;; read byte range [start, end) from bytevector bv and deserialize an object from it.
;; Return two values:
;;   either object and updated start position in range [start, end)
;;   or #f #f
(define (wire-get bv start end)
  (values #f #f)) ; TODO: implement

) ; close library
