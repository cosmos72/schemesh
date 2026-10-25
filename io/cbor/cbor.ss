;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; CBOR serialization/deserialization format, compliant with IETF STD 94 (RFC 8949)
;;;
;;; Hardcoded limits:
;;;
;;; Serialization format is message based, and length of each message
;;        can be at most 2^32 - 1 bytes or (greatest-fixnum) bytes,
;;;       whatever is smaller.
;;;
;;; Each message consists of serialized CBOR data.
;;; For compatibility, there is no framing: message length is not encoded.
;;;
;;; The following tags are used in addition to the standard CBOR tags:
;;;
;;; [TO DO]

(library (scheme2k io cbor (1 0 2))
  (export make-cbor-reader cbor-reader cbor-reader?
          make-cbor-writer cbor-writer cbor-writer?

          ;datum->cbor cbor->datum cbor-get-from-bytevector cbor-get-from-bytespan cbor-put-to-bytespan
          ;cbor-register-rtd  cbor-register-rtd-reflect  cbor-reserve-tag
          ;; internal functions, exported for types that want to define their own serializer/deserializer
          ;; and that cannot be handled by functions (cbor-register-rtd...)
          ;(rename (get/any cbor-inner-get)
          ;        (put/any cbor-inner-put))
          )
  (import
    (rnrs)
    (only (chezscheme)       box box? bwp-object? bytevector bytevector->immutable-bytevector
                             bytevector-ieee-double-ref bytevector-ieee-double-set!
                             bytevector-s24-ref         bytevector-s24-set!
                             bytevector-u24-ref         bytevector-u24-set!
                             cfl= cfl+ cflonum? current-time date-year date-month date-day date-hour date-minute
                             date-second date-nanosecond date-zone-offset enum-set? fl-make-rectangular
                             fx1+ fx1- fxsrl fxsll fxvector? fxvector-length fxvector-ref fxvector-set!
                             include integer-length logbit? make-fxvector make-time meta-cond procedure-arity-mask
                             record-writer reverse! time? time=? time-type time-second time-nanosecond unbox void)

    ;; these predicates are equivalent to their r6rs counterparts,
    ;; only extended to also accept 1 argument
    (prefix (only (chezscheme) char=? char-ci=? record-constructor string=? string-ci=?)
            chez:)

    (only (scheme2k bootstrap)               assert* box-cas-strong! bwp-object check-interrupts fx<=?* raise-assertf raise-errorf)
    (only (scheme2k containers bytespan)     bytespan bytespan? bytespan->bytevector*! bytespan-clear! bytespan-delete-left!
                                             bytespan-insert-right/bytevector! bytespan-length
                                             bytespan-peek-beg bytespan-peek-data bytespan-peek-end
                                             bytespan-reserve-right! bytespan-resize-right! bytevector->bytespan*)
    (only (scheme2k containers bytevector)   bytevector-hash bytevector-sint-ref* bytevector-sint-set*! subbytevector=?)
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
    (only (scheme2k posix fd)                fd-close fd-read-insert-right! fd-write-all)
    (only (scheme2k posix fs)                make-dir-entry dir-entry)
    (only (scheme2k posix rlimit)            rlimit rlimit-type)
          (scheme2k posix status)
    (only (scheme2k io obj)                  reader reader-get reader-eof? reader-close reader-skip
                                             writer writer-put writer-eof? writer-close)
    (only (scheme2k io port)                 read-bytes-insert-right!))


(include "io/cbor/constants.ss")
(include "io/cbor/common.ss")
#|
(include "io/cbor/record.ss")
(include "io/cbor/vector.ss")
(include "io/cbor/get.ss")
(include "io/cbor/container.ss")
(include "io/cbor/misc.ss")
(include "io/cbor/status.ss")
|#
(include "io/cbor/reader.ss")
(include "io/cbor/writer.ss")


;; customize how "cbor-reader" objects are printed
(record-writer (record-type-descriptor cbor-reader)
  (lambda (rx port writer)
    (put-string port "#<cbor-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (unbox (cbor-reader-in-box rx)) port)
    (put-char port #\>)))


;; customize how "cbor-writer" objects are printed
(record-writer (record-type-descriptor cbor-writer)
  (lambda (tx port writer)
    (put-string port "#<cbor-writer")
    (put-string port (if (writer-eof? tx) " eof " " ok "))
    (writer (unbox (cbor-writer-out-box tx)) port)
    (put-char port #\>)))


#|
(begin
  (cbor-register-rtd (record-rtd (duration 0 0))     tag-time         len/time       get/time         put/time)
  (cbor-register-rtd (record-rtd (date 1970 1 1 +0)) tag-date         len/date       get/date         put/date)
  (cbor-register-rtd (record-rtd (span))             tag-span         len/span       get/span         put/span)
  (cbor-register-rtd (record-rtd (gbuffer))          tag-gbuffer      len/gbuffer    get/gbuffer      put/gbuffer)
  (cbor-register-rtd (record-rtd (bytespan))         tag-bytespan     len/bytespan   get/bytespan     put/bytespan)
  (cbor-register-rtd (record-rtd (charspan))         tag-charspan24   len/charspan   get/charspan24   put/charspan)
  (cbor-register-rtd (record-rtd (ok))               tag-status       len/status     get/status       put/status)

  (vector-set! known-tag tag-charspan8     get/charspan8)
  (vector-set! known-tag tag-charspan16    get/charspan16)

  ;; customize how to serialize/deserialize `dir-entry` objects
  (cbor-register-rtd-reflect (record-type-descriptor dir-entry)   tag-dir-entry   make-dir-entry)

    ;; customize how to serialize/deserialize `rlimit` objects
  (cbor-register-rtd-reflect (record-type-descriptor rlimit-type) tag-rlimit      rlimit)

) ; close begin
|#

) ; close library
