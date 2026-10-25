;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/cbor/cbor.ss


(define-syntax endian (identifier-syntax (endianness big)))

(define major-unsigned   0)
(define major-negative   #x20)
(define major-bytestring #x40)
(define major-utf8string #x60)
(define major-array      #x80)
(define major-map        #xA0)
(define major-tag        #xC0)
(define major-misc       #xE0)


(define minor-1byte        24)
(define minor-2byte        25)
(define minor-4byte        26)
(define minor-8byte        27)
(define minor-indefinite   31)


#|
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

(define min-tag-to-allocate   95)
(define next-tag-to-allocate 240)
(define max-tag-to-allocate  253)

(define tag-disk-entry    241) ; implemented in os/disk.ss
(define tag-process-entry 242) ; implemented in os/process.ss
(define tag-rlimit        243)
(define tag-dir-entry     244)
(define tag-date          245)
(define tag-time          246)
(define tag-status        247) ; implemented in cbor/status.ss
(define tag-span          248) ; n encoded as vlen, followed by n elements each encoded as tag+datum
(define tag-gbuffer       249) ; n encoded as vlen, followed by n elements each encoded as tag+datum
(define tag-bytespan      250) ; n encoded as vlen, followed by n bytes
(define tag-charspan8     251) ; n encoded as vlen, followed by n characters each encoded as u8
(define tag-charspan16    252) ; n encoded as vlen, followed by n characters each encoded as u16
(define tag-charspan24    253) ; n encoded as vlen, followed by n characters each encoded as u24
(define tag-extended      254) ; followed by extended tag encoded as vlen
|#

;;; magic bytes: Self-Described CBOR i.e. #xd9 #xd9 #xf7
(define cbor-magic-bytes (bytevector->immutable-bytevector #vu8(#xd9 #xd9 #xf7)))

(define known-sym
  (eq-hashtable
    '<type> 55 'block-device 56 'boolean=? 57
    'bytevector=? 58 'bytevector-hash 59
    'cfl= 60
    'char=? 61 'char-ci=? 62 'char->integer 63 ; usable as hash function for char=?
    'char-device 64
    'date 65 'dir 66 'dir-entry 67
    'enum-set=? 68
    'eq? 69 'eqv? 70 'equal? 71 'equal-hash 72
    'file 73 'fifo 74 'fl=? 75 'fx=? 76
    'process-entry 77
    'socket 78
    'string=? 79 'string-ci=? 80 'string-hash 81 'string-ci-hash 82
    'symbol=? 83 'symbol-hash 84
    'symlink 85
    'time=? 86 'time-collector-cpu 87 'time-collector-real 88 'time-duration 89
    'time-monotonic 90 'time-process 91 'time-thread 92 'time-utc 93
    'unlimited 94))


;; (define tags-string (vector tag-string8 tag-string16 tag-string24))

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
