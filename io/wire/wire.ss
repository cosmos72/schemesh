;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; Wire serialization/deserialization format.
;;;
;;; Hardcoded limits:
;;;
;;; Serialization format is message based, and length of each message
;;        can be at most 2^32 - 1 bytes or (greatest-fixnum) bytes,
;;;       whatever is smaller, including header (see below)
;;;
;;; Each message contains: header + payload (tag + datum)
;;;
;;; header: vlen, encoded either as 1 byte or 4 bytes little-endian,
;;        maximum is 2^31 - 5 or (greatest-fixnum) - 4, whatever is smaller.
;;;       Indicates the number of bytes occupied by payload (tag + datum)
;;;       does NOT include the number of bytes occupied by vlen
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
;;;      20 => datum is vlen followed by vlen bytes: signed exact integer, little endian
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
;;;      37 => datum is improper list:     n encoded as u32, followed by n tag+datum: elements
;;;      38 => datum is proper list:       n encoded as u32, followed by n tag+datum: elements
;;;      39 => datum is vector:        n encoded as vlen, followed by n tag+datum
;;;      40 => datum is bytevector:    n encoded as vlen, followed by n bytes
;;;      41 => datum is string8:       n encoded as vlen, followed by characters each encoded as 1 byte
;;;      42 => datum is string16:      n encoded as vlen, followed by characters each encoded as 2 bytes
;;;      43 => datum is string24:      n encoded as vlen, followed by characters each encoded as 3 bytes
;;;      44 => datum is fxvector:      n encoded as vlen, followed by n tag+datum
;;;      45 => datum is flvector:      n encoded as vlen, followed by n IEEE float64 little-endian, each occupying 8 bytes
;;;      46 => datum is symbol8:       n encoded as vlen, followed by characters each encoded as 1 byte
;;;      47 => datum is symbol16:      n encoded as vlen, followed by characters each encoded as 2 bytes
;;;      48 => datum is symbol24:      n encoded as vlen, followed by characters each encoded as 3 bytes
;;;      49 => datum is eq-hashtable:  n encoded as vlen, followed by 2 * n tag+datum
;;;      50 => datum is eqv-hashtable: n encoded as vlen, followed by 2 * n tag+datum
;;;      51 => datum is hashtable:     hash function name encoded as symbol, checked against a whitelist
;;;                                      followed by equal function name encoded as symbol, checked against a whitelist
;;;                                      followed by n encoded as vlen, followed by 2 * n tag+datum
;;;      52 => datum is eq-ordered-hash:  n encoded as vlen, followed by 2 * n tag+datum
;;;      53 => datum is eqv-ordered-hash: n encoded as vlen, followed by 2 * n tag+datum
;;;      54 => datum is ordered-hash:     hash function name encoded as symbol, checked against a whitelist
;;;                                         followed by equal function name encoded as symbol, checked against a whitelist
;;;                                         followed by n encoded as vlen, followed by 2 * n tag+datum
;;       55 ... 88  => datum is a known symbol
;;;      89 ... 242 => datum is a user-registered record type
;;;     243 ... 253 => datum is a pre-registered record type
;;;     254 => datum is magic string: #\w #\i #\r #\e VERSION-HI VERSION-LO
;;;     255 => datum starts with extended tag


(library (scheme2k io wire (0 9 3))
  (export datum->wire wire->datum datum->wire-length wire-get-from-bytevector wire-get-from-bytespan wire-put-to-bytespan
          wire-put-magic-to-bytespan wire-register-rtd  wire-register-rtd-reflect  wire-reserve-tag
          ;; internal functions, exported for types that want to define their own serializer/deserializer
          ;; and that cannot be handled by functions (wire-register-rtd...)
          (rename (len/any wire-inner-len)  (tag+    wire-inner-len-tag)
                  (get/any wire-inner-get)  ;; there is no (wire-inner-get-tag) because tag is consumed internally before invoking user functions
                  (put/any wire-inner-put)  (put/tag wire-inner-put-tag)))
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) box box? bwp-object? bytevector bytevector->immutable-bytevector
                       bytevector-ieee-double-ref bytevector-ieee-double-set!
                       bytevector-s24-ref         bytevector-s24-set!
                       bytevector-u24-ref         bytevector-u24-set!
                       cfl= cfl+ cflonum? current-time date-year date-month date-day date-hour date-minute
                       date-second date-nanosecond date-zone-offset enum-set? fl-make-rectangular
                       fx1+ fx1- fxsrl fxsll fxvector? fxvector-length fxvector-ref fxvector-set!
                       include integer-length logbit? make-fxvector make-time meta-cond procedure-arity-mask
                       reverse! time? time=? time-type time-second time-nanosecond unbox void)

    ;; these predicates are equivalent to their r6rs counterparts,
    ;; only extended to also accept 1 argument
    (prefix (only (chezscheme) char=? char-ci=? record-constructor string=? string-ci=?)
            chez:)

    (only (scheme2k bootstrap)               assert* bwp-object fx<=?*)
    (only (scheme2k containers bytespan)     bytespan bytespan? bytespan->bytevector*! bytespan-insert-right/bytevector!
                                             bytespan-length bytespan-peek-beg bytespan-peek-data
                                             bytespan-reserve-right! bytespan-resize-right! bytevector->bytespan*)
    (only (scheme2k containers bytevector)   bytevector-hash bytevector-sint-ref* bytevector-sint-set*!)
    (only (scheme2k containers charspan)     charspan charspan-length charspan-ref charspan-set! make-charspan)
    (only (scheme2k containers date)         date date-or-false)
    (only (scheme2k containers flvector)     flvector? flvector-length flvector-ref flvector-set! make-flvector)
    (only (scheme2k containers gbuffer)      gbuffer gbuffer-length gbuffer-ref gbuffer-set! make-gbuffer)
    (only (scheme2k containers list)         for-plist list-reverse*!)
    (only (scheme2k containers hashtable)    eq-hashtable for-hash hashtable-transpose plist->eq-hashtable)
    (only (scheme2k containers ordered-hash) for-ordered-hash make-eq-ordered-hash make-eqv-ordered-hash make-ordered-hash
                                             ordered-hash? ordered-hash-equivalence-function ordered-hash-hash-function
                                             ordered-hash-set! ordered-hash-size)
    (only (scheme2k containers span)         make-span span span-length span-ref span-set!)
    (only (scheme2k containers time)         duration)
    (only (scheme2k containers utf8b)        integer->char*)
    (only (scheme2k posix fs)                make-dir-entry dir-entry)
          (scheme2k posix status))


(include "io/wire/constants.ss")
(include "io/wire/common.ss")
(include "io/wire/record.ss")
(include "io/wire/vector.ss")
(include "io/wire/get.ss")
(include "io/wire/container.ss")
(include "io/wire/misc.ss")
(include "io/wire/status.ss")

(begin
  (wire-register-rtd (record-rtd (duration 0 0))     tag-time         len/time       get/time         put/time)
  (wire-register-rtd (record-rtd (date 1970 1 1 +0)) tag-date         len/date       get/date         put/date)
  (wire-register-rtd (record-rtd (span))             tag-span         len/span       get/span         put/span)
  (wire-register-rtd (record-rtd (gbuffer))          tag-gbuffer      len/gbuffer    get/gbuffer      put/gbuffer)
  (wire-register-rtd (record-rtd (bytespan))         tag-bytespan     len/bytespan   get/bytespan     put/bytespan)
  (wire-register-rtd (record-rtd (charspan))         tag-charspan24   len/charspan   get/charspan24   put/charspan)
  (wire-register-rtd (record-rtd (ok))               tag-status       len/status     get/status       put/status)

  (vector-set! known-tag tag-charspan8     get/charspan8)
  (vector-set! known-tag tag-charspan16    get/charspan16)

  ;; customize how to serialize/deserialize `dir-entry` objects
  (wire-register-rtd-reflect (record-type-descriptor dir-entry)   tag-dir-entry   make-dir-entry)


) ; close begin

) ; close library
