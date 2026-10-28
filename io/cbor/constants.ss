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


(define misc-eof    #xF2) ; major-misc + 18, unofficial
(define misc-bwp    #xF3) ; major-misc + 19, unofficial
(define misc-false  #xF4) ; major-misc + 20
(define misc-true   #xF5) ; major-misc + 21
(define misc-nil    #xF6) ; major-misc + 22
(define misc-void   #xF7) ; major-misc + 23, actually means "undefined"

;; for officially assigned CBOR tags, see https://www.iana.org/assignments/cbor-tags

(define tag-ratio    30) ;; see http://peteroupc.github.io/CBOR/rational.html
(define tag-symbol   39) ;; see https://github.com/lucas-clemente/cbor-specs/blob/master/id.md
;; an alternative is 280 ;; see https://github.com/mishoo/cbor.lisp/blob/e1428a5/clext-spec.txt

(define tag-box      22098) ;; see http://cbor.schmorp.de/indirection
   ;; an alternative is 128 ;; unofficial, in unassigned range 128.199

(define tag-complex  43000) ;; see https://www.iana.org/assignments/cbor-tags
   ;; an alternative is 129 ;; unofficial, in unassigned range 128.199

(define tag-list     281) ;; see https://github.com/mishoo/cbor.lisp/blob/e1428a5/clext-spec.txt
(define tag-char     282) ;; see https://github.com/mishoo/cbor.lisp/blob/e1428a5/clext-spec.txt
(define tag-object   283) ;; see https://github.com/mishoo/cbor.lisp/blob/e1428a5/clext-spec.txt

(define tag-time     1001) ;; see https://www.iana.org/go/rfc9581#section-3
(define tag-duration 1002) ;; see https://www.iana.org/go/rfc9581#section-4

#|
(define tag-eq-hashtable  49)
(define tag-eqv-hashtable 50)
(define tag-hashtable     51)
(define tag-eq-ord-hash   52)
(define tag-eqv-ord-hash  53)
(define tag-ord-hash      54)

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
    '<type> 0 'block-device 1 'boolean=? 2
    'bytevector=? 3 'bytevector-hash 4
    'cfl= 5
    'char=? 6 'char-ci=? 7 'char->integer 8 ; usable as hash function for char=?
    'char-device 9
    'date 10 'dir 1 'dir-entry 12
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
