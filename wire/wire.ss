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
;;; each message is: header = dlen + tag
;;                   followed by datum
;;;
;;; dlen: 3 bytes LO MID HI, max is 2^24 - 1
;;;       indicates the number of bytes occupied by datum
;;;       does NOT include the number of bytes occupied by dlen + tag
;;;
;;; tag: 1 byte, indicates the type of datum
;;;       0 => datum is fixnum 0
;;;       1 => datum is fixnum 1
;;;       2 => datum is fixnum 2
;;;       3 => datum is fixnum -1
;;;       4 => datum is exact integer, signed, little endian, datum-byte-n is dlen bytes
;;;       5 => datum is exact ratio, encoded as 2 messages: numerator, denominator
;;;       6 => datum is exact complex, encoded as 2 messages: real, imag
;;;       7 => datum is flonum, 8 byte IEEE float64 little-endian
;;;       8 => datum is cflonum, two 8 byte IEEE float64 little-endian: real, imag
;;;       9 => datum is character, encoded as dlen i.e. 3 bytes
;;;      10 => datum is #f
;;;      11 => datum is #t
;;;      12 => datum is '()
;;;      13 => datum is (void)
;;;      14 => datum is (eof-object)
;;;      15 => datum is (bwp-object)
;;;      16 => datum is box:         followed by message
;;;      17 => datum is pair:        encoded as 2 messages: car, cdr
;;;      18 => datum is vector:      n encoded as dlen, followed by n messages
;;;      19 => datum is bytevector:  n is dlen, followed by n raw bytes
;;;      20 => datum is string:      n encoded as dlen, followed by characters each encoded as dlen
;;;      21 => datum is fxvector:    n encoded as dlen, followed by n messages
;;;      22 => datum is flvector:    n is dlen/8, followed by n * 8 IEEE float64 little-endian
;;;      23 UNUSED. stencil-vector?
;;;      24 => datum is symbol:      n encoded as dlen, followed by characters each encoded as dlen
;;;      25 => datum is eq-hashtable:  n encoded as dlen, followed by 2 * n messages
;;;      26 => datum is eqv-hashtable: n encoded as dlen, followed by 2 * n messages
;;;      27 => datum is equal-hashtable: equal function name encoded as message - must be a symbol, checked against a whitelist
;;;                                      followed by n encoded as dlen, followed by 2 * n messages
;;
;;;      32 => datum is span:        n encoded as dlen, followed by n messages
;;;      33 => datum is bytespan:    n is dlen, followed by n raw bytes
;;;      34 => datum is charspan:    n encoded as dlen, followed by characters each encoded as dlen
;;;      35 => datum is gbuffer:     n encoded as dlen, followed by n messages
;;;      36 => datum is bytegbuffer: NOT IMPLEMENTED
;;;      37 => datum is chargbuffer: n encoded as dlen, followed by characters each encoded as dlen


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
    (only (schemesh bootstrap) assert*)
    (schemesh containers flvector)
    (schemesh containers bytespan))


(define dlen 3)     ; dlen is encoded as 3 bytes
(define dlen+tag 4) ; dlen+tag is encoded as 4 bytes

(define min-len dlen+tag) ; minimum length of header + datum
(define max-len #xFFFFFF)  ; maximum length of header + datum

(define tag-0         0)
(define tag-1         1)
(define tag-2         2)
(define tag--1        3)
(define tag-int       4) ; exact integer
(define tag-ratio     5) ; exact ratio
(define tag-complex   6) ; exact complex
(define tag-flonum    7)
(define tag-cflonum   8)
(define tag-char      9)
(define tag-f        10)
(define tag-t        11)
(define tag-nil      12)
(define tag-void     13)
(define tag-eof      14)
(define tag-bwp      15)
(define tag-box      16)
(define tag-pair     17)
(define tag-list     18)
(define tag-vector   19)
(define tag-bvector  20) ; bytevector
(define tag-string   21)
(define tag-fxvector 22)
(define tag-flvector 23)
(define tag-symbol   25)

(define len-char     3) ;; each character is encoded as 3 bytes
(define len-flonum   8) ;; each flonum is encoded as 8 bytes
(define len-cflonum 16) ;; each cflonum is encoded as 16 bytes

(define endian (endianness little))

(define (valid-message-len? n)
  (and (fixnum? n) (fx<=? min-len n max-len)))

(define header+
  (case-lambda
    ((n)
      (and (fixnum? n) (fx+ n dlen+tag)))
    ((n pos)
      (and (fixnum? n) (fixnum? pos) (fx+ (fx+ n pos) dlen+tag)))))

;; write one byte into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u8 bv pos u8)
  (bytevector-u8-set! bv pos u8)
  (fx1+ pos))

;; write exact unsigned integer as 3 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u24 bv pos u24)
  (bytevector-u24-set! bv pos u24 endian)
  (fx+ pos 3))

;; write message header (dlen, tag) into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/header bv pos tag datum-byte-n)
  (let ((pos (put/u24 bv pos datum-byte-n)))
    (put/u8 bv pos tag)))


(define (len/exact-int obj pos)
  (case obj
    ((0 1 2 -1)
      (header+ pos)) ; only header, datum is 0 bytes
    (else
      (if (or (not (fixnum? pos))
              (and (fixnum? obj) (fx<=? -128 obj 127)))
        (header+ 1 pos)) ; 1-byte datum

        ;; datum is sign-extended, two's complement little-endian bytes
        (let ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3))))
          (header+ datum-byte-n pos)))))


;; write header and exact integer into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/exact-int bv pos obj)
  (case obj
    ((0)
      (put/header bv pos tag-0 0)) ; only header, datum is 0 bytes
    ((1)
      (put/header bv pos tag-1 0)) ; only header, datum is 0 bytes
    ((2)
      (put/header bv pos tag-2 0)) ; only header, datum is 0 bytes
    ((-1)
      (put/header bv pos tag--1 0)) ; only header, datum is 0 bytes
    (else
      (if (or (not (fixnum? pos))
              (and (fixnum? obj) (fx<=? -128 obj 127)))

        (let ((pos (put/header bv pos tag-int 1))) ; 1-byte datum
          (put/u8 bv pos (fxand obj #xFF)))

        ;; datum is sign-extended, two's complement little-endian bytes
        (let* ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3)))
               (pos (put/header bv pos tag-int datum-byte-n)))
          (bytevector-sint-set! bv pos obj endian datum-byte-n)
          (fx+ pos datum-byte-n))))))


(define (len/flonum obj pos)
  (header+ len-flonum pos))

(define (put/flonum bv obj pos)
  (let ((pos (put/header bv pos tag-flonum len-flonum)))
    (bytevector-ieee-double-set! bv pos obj endian)
    (fx+ pos len-flonum)))


(define (len/cflonum obj pos)
  (header+ (* 2 len-flonum) pos))

(define (put/cflonum bv obj pos)
  (let ((pos (put/header bv pos tag-cflonum len-cflonum)))
    (bytevector-ieee-double-set! bv pos (real-part obj) endian)
    (let ((pos (fx+ pos len-flonum)))
      (bytevector-ieee-double-set! bv pos (imag-part obj) endian)
      (fx+ pos len-flonum))))


(define (len/number obj pos)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (len/exact-int obj pos)
        ;; exact ratio: encoded as header, (numerator, denominator)
        (header+
          (len/exact-int (denominator obj)
             (len/exact-int (numerator obj) pos))))
      ;; exact complex
      (header+
        (len/number (imag-part obj)
          (len/number (real-part obj) pos))))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (len/flonum obj pos)
      (len/cflonum obj pos))))


(define (put/number bv obj pos)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (put/exact-int bv obj pos)
        ;; exact ratio: encoded as header, (numerator, denominator)
        (let* ((end0 (fx+ pos dlen+tag))
               (end1 (put/exact-int bv end0 (numerator obj)))
               (end2 (put/exact-int bv end1 (denominator obj))))
            (put/header bv pos tag-ratio (fx- end2 end0))
            end2))
      ;; exact complex: encoded as header, (real-part, imag-part)
      (let* ((end0 (fx+ pos dlen+tag))
             (end1 (put/number bv end0 (real-part obj)))
             (end2 (put/number bv end1 (imag-part obj))))
          (put/header bv pos tag-complex (fx- end2 end0))
          end2))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (put/flonum bv obj pos)
      (put/cflonum bv obj pos))))


(define (len/char obj pos)
  (header+ len-char pos))

;; write header and one character into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/char bv pos obj)
  (let ((pos (put/header bv pos tag-char len-char)))
    (put/u24 bv pos (char->integer obj))))


(define (len/box obj pos)
  (header+
    (len/any (unbox obj) pos)))

(define (put/box bv pos obj)
  (let* ((end0 (fx+ pos dlen+tag))
         (end1 (put/any bv end0 (unbox obj))))
    (put/header bv pos tag-box (fx- end1 end0))
    end1))


(define (len/list obj pos)
  (let %len/list ((l obj) (pos pos))
    (if (and pos (not (null? l)))
      (%len/list (cdr l) (len/any (car l) pos))
      (header+ dlen pos)))) ; n is encoded as dlen


(define (len/pair obj pos)
  (if #f ; (list? obj)
    (len/list obj pos)
    (header+
      (len/any (cdr obj)
        (len/any (car obj) pos)))))

(define (put/pair bv pos obj)
  (let* ((end0 (fx+ pos dlen+tag))
         (end1 (put/any bv end0 (car obj)))
         (end2 (put/any bv end1 (cdr obj))))
    (put/header bv pos tag-pair (fx- end2 end0))
    end2))


(define (len/vector obj pos)
  (let ((n (vector-length obj)))
    (let %len/vector ((v obj) (i 0) (n n) (pos pos))
      (if (and pos (fx<? i n))
        (%len/vector v (fx1+ i) n (len/any (vector-ref v i) pos))
        (header+ dlen pos))))) ; n is encoded as dlen

(define (put/vector bv pos obj)
  pos) ; TODO: implement


(define (len/bytevector obj pos)
  (let ((datum-byte-n (bytevector-length obj)))
    (header+ (fx+ dlen datum-byte-n) pos)))

(define (put/bytevector bv pos obj)
  (let ((datum-byte-n (bytevector-length obj))
        (end0         (fx+ pos dlen+tag)))
    (put/header bv pos tag-bvector datum-byte-n)
    (bytevector-copy! obj 0 bv end0 datum-byte-n)
    (fx+ end0 datum-byte-n)))


(define (len/fxvector obj pos)
  (let ((n (fxvector-length obj)))
    (let %len/fxvector ((v obj) (i 0) (n n) (pos pos))
      (if (and pos (fx<? i n))
        (%len/fxvector v (fx1+ i) n (len/exact-int (vector-ref v i) pos))
        (header+ dlen pos))))) ; n is encoded as dlen

(define (put/fxvector bv pos obj)
  pos) ; TODO: implement


(define (len/flvector obj pos)
  (let ((datum-byte-n (fx* len-flonum (flvector-length obj))))
    (header+ (fx+ dlen datum-byte-n) pos)))

(define (put/flvector bv pos obj)
  (let* ((n (flvector-length obj))
         (datum-byte-n (fx* len-flonum n)))
    (put/header bv pos tag-flvector datum-byte-n)
    (do ((i 0 (fx1+ i))
         (end (fx+ pos dlen+tag) (fx+ end len-flonum)))
        ((fx>=? i n)
           end)
      (bytevector-ieee-double-set! bv end (flvector-ref obj i) endian))))



(define (len/string obj pos)
  (let ((datum-byte-n (fx* len-char (string-length obj)))) ;; each character is 3 bytes
    (header+ (fx+ len-char datum-byte-n) pos)))

(define (put/string bv pos obj)
  pos) ; TODO: implement


(define (len/symbol obj pos)
  (len/string (symbol->string obj) pos))

(define (put/symbol bv pos obj)
  pos) ; TODO: implement


(define (len/hashtable obj pos)
  #f) ;; TODO: implement

(define (put/hashtable bv pos obj)
  #f) ;; TODO: implement


(define (len/record obj pos)
  #f) ;; TODO: implement

(define (put/record bv pos obj)
  #f) ;; TODO: implement


;; recursively traverse obj and return the number of bytes needed to serialize obj
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (len/any obj pos)
  (case obj
    ((0 1 2 -1 #f #t ())
      (header+ pos)) ; only header, datum is 0 bytes
    (else
      (cond
        ((not pos)         #f)
        ((fixnum? obj)     (len/exact-int  obj pos))
        ((char?   obj)     (len/char       obj pos))
        ((flonum? obj)     (len/flonum     obj pos))
        ((pair?   obj)     (len/pair       obj pos))
        ((symbol? obj)     (len/symbol     obj pos))
        ((eq? (void) obj)  (header+ pos)) ; only header, datum is 0 bytes
        ((eof-object? obj) (header+ pos)) ; only header, datum is 0 bytes
        ((bwp-object? obj) (header+ pos)) ; only header, datum is 0 bytes
        ((procedure? obj)   #f)
        ;; these are slower to check
        ((box?    obj)     (len/box        obj pos))
        ((number? obj)     (len/number     obj pos))
        ((vector? obj)     (len/vector     obj pos))
        ((bytevector? obj) (len/bytevector obj pos))
        ((fxvector? obj)   (len/fxvector   obj pos))
        ((flvector? obj)   (len/flvector   obj pos))
        ((string? obj)     (len/string     obj pos))
        ((hashtable? obj)  (len/hashtable  obj pos))
        ((record? obj)     (len/record     obj pos))
        (else              #f)))))


;; recursively traverse obj, serialize it and write it into bytevector bv starting at position pos.
;; return updated position, or #f on errors.
(define (put/any bv pos obj)
  (case obj
    ((0)
      (put/header bv pos tag-0 0)) ; only header, datum is 0 bytes
    ((1)
      (put/header bv pos tag-1 0)) ; only header, datum is 0 bytes
    ((2)
      (put/header bv pos tag-2 0)) ; only header, datum is 0 bytes
    ((-1)
      (put/header bv pos tag--1 0)) ; only header, datum is 0 bytes
    ((#f)
      (put/header bv pos tag-f 0)) ; only header, datum is 0 bytes
    ((#t)
      (put/header bv pos tag-t 0)) ; only header, datum is 0 bytes
    ((())
      (put/header bv pos tag-nil 0)) ; only header, datum is 0 bytes
    (else
      (cond
        ((not pos)         #f)
        ((fixnum? obj)     (put/exact-int  bv pos obj))
        ((char?   obj)     (put/char       bv pos obj))
        ((flonum? obj)     (put/flonum     bv pos obj))
        ((pair?   obj)     (put/pair       bv pos obj))
        ((symbol? obj)     (put/symbol     bv pos obj))
        ((eq? (void) obj)  (header+ pos)) ; only header, datum is 0 bytes
        ((eof-object? obj) (header+ pos)) ; only header, datum is 0 bytes
        ((bwp-object? obj) (header+ pos)) ; only header, datum is 0 bytes
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
  (let ((pos (len/any obj 0)))
    (if (valid-message-len? pos) pos #f)))


;; recursively traverse obj, serialize it and append it to bytespan bsp.
;; return number of written bytes, or #f on errors.
(define wire-put
  (case-lambda
    ((bsp obj message-wire-len)
      (assert* 'wire-put (bytespan? bsp))
      (if (valid-message-len? message-wire-len)
        (let ((pos (bytespan-length bsp)))
          (bytespan-resize-right! bsp (fx+ pos message-wire-len))
          (let ((end (put/any (bytespan-peek-data bsp)
                              (fx+ (bytespan-peek-beg bsp) pos)
                              obj)))
            (assert* 'wire-put (fx=? end (bytespan-length bsp)))
            message-wire-len))
        #f))
    ((bsp obj)
      (wire-put bsp obj (wire-length obj)))))


;; read byte range [start, end) from bytevector bv and deserialize an object from it.
;; Return two values:
;;   either object and updated start position in range [start, end)
;;   or #f #f
(define (wire-get bv start end)
  (values #f #f)) ; TODO: implement

) ; close library
