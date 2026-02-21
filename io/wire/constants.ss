;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/wire/wire.ss

(define min-len-vlen 1) ; vlen is encoded as 1 or 4 bytes
(define max-len-vlen 4) ; vlen is encoded as 1 or 4 bytes

;; maximum supported number of elements in a container (vector, string, hashtable ...)
(define max-vlen (fx- (min #x7fffffff (greatest-fixnum)) max-len-vlen))

;; maximum length of payload = tag + datum
(define max-len-payload max-vlen)

(define len-tag    1)  ; tag is encoded as 1 byte

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
(define tag-sint     20) ; vlen followed by: exact integer, signed, vlen bytes, little endian
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
(define tag-bytevector 40)
(define tag-string8    41)
(define tag-string16   42)
(define tag-string24   43)
(define tag-fxvector   44)
(define tag-flvector   45)
(define tag-symbol8    46)
(define tag-symbol16   47)
(define tag-symbol24   48)

(define tag-eq-hashtable  49)
(define tag-eqv-hashtable 50)
(define tag-hashtable     51)
(define tag-eq-ord-hash   52)
(define tag-eqv-ord-hash  53)
(define tag-ord-hash      54)

(define min-tag-to-allocate   89)
(define next-tag-to-allocate 238)
(define max-tag-to-allocate  253)

(define tag-process-entry 239) ; allocated in os/process.ss
(define tag-dir-entry     240)
(define tag-date          241)
(define tag-time          242)
(define tag-status        243) ; implemented in wire/status.ss
(define tag-span          244) ; n encoded as vlen, followed by n elements each encoded as tag+datum
(define tag-gbuffer       245) ; n encoded as vlen, followed by n elements each encoded as tag+datum
(define tag-bytespan      246) ; n encoded as vlen, followed by n bytes
(define tag-bytegbuffer   247) ; NOT IMPLEMENTED
(define tag-charspan8     248) ; n encoded as vlen, followed by n characters each encoded as u8
(define tag-charspan16    249) ; n encoded as vlen, followed by n characters each encoded as u16
(define tag-charspan24    250) ; n encoded as vlen, followed by n characters each encoded as u24
(define tag-chargbuffer8  251) ; n encoded as vlen, followed by n characters each encoded as u8
(define tag-chargbuffer16 252) ; n encoded as vlen, followed by n characters each encoded as u16
(define tag-chargbuffer24 253) ; n encoded as vlen, followed by n characters each encoded as u24
(define tag-magic-bytes   254)

;;; magic bytes: #x8 #x0 #x0 #x0 #xFF w i r e #x0 #x0 #x0
;;;              the last three bytes are version-lo version-mid version-hi

(define known-sym
  (eq-hashtable
    'boolean=? 55 ; UNUSED 56
    'bytevector=? 57 'bytevector-hash 58
    'cfl= 59 ; UNUSED 60
    'char=? 61 'char-ci=? 62 'char->integer 63 ; usable as hash function for char=?
    'enum-set=? 64  ; UNUSED 65
    'eq? 66 'eqv? 67 'equal? 68 'equal-hash 69
    'fl=? 70 ; UNUSED 71
    'fx=? 72 ; UNUSED 72
    'string=? 74 'string-ci=? 75 'string-hash 76 'string-ci-hash 77
    'symbol=? 78 'symbol-hash 79
    'time=? 80 'time-collector-cpu 81 'time-collector-real 82
    'time-duration 83 ; UNUSED 84
    'time-monotonic 85 'time-process 86 'time-thread 87 'time-utc 88))


(define max-len-char 3) ;; each character is encoded as <= 3 bytes
(define len-flonum   8) ;; each flonum is encoded as 8 bytes
(define len-cflonum 16) ;; each cflonum is encoded as 16 bytes

(define endian (endianness little))

(define tags-string (vector tag-string8 tag-string16 tag-string24))

(define known-cmp-sym
  (eq-hashtable
    boolean=? 'boolean=? ; boolean keys are not very useful in a hashtable...
    bytevector=? 'bytevector=? cfl= 'cfl= char=? 'char=? char-ci=? 'char-ci=?
    enum-set=? 'enum-set=? eq? 'eq? eqv? 'eqv? equal? 'equal? fl=? 'fl=? fx=? 'fx=?
    string=? 'string=? string-ci=? 'string-ci=? symbol=? 'symbol=? time=? 'time=?
    chez:char=? 'char=? chez:char-ci=? 'char-ci=?
    chez:string=? 'string=? chez:string-ci=? 'string-ci=?))

(define known-hash-sym
  (eq-hashtable
     bytevector-hash 'bytevector-hash
     char->integer 'char->integer ; usable as hash function for char=?
     equal-hash 'equal-hash
     string-hash 'string-hash string-ci-hash 'string-ci-hash
     symbol-hash 'symbol-hash))

(define (always-true x) #t)

(define known-cmp-key-type-validator
  (eq-hashtable
     'boolean=?       boolean?
     'bytevector=?    bytevector?
     'cfl=            cflonum?
     'char=?          char?
     'char-ci=?       char?
     'enum-set=?      enum-set?
     'eq?             always-true
     'eqv?            always-true
     'equal?          always-true
     'fl=?            flonum?
     'fx=?            fixnum?
     'string=?        string?
     'string-ci=?     string?
     'symbol=?        symbol?
     'time=?          time?))

(define known-hash-key-type-validator
  (eq-hashtable
     'bytevector-hash bytevector?
     'char->integer   char?
     'equal-hash      always-true
     'string-hash     string?
     'string-ci-hash  string?
     'symbol-hash     symbol?))
