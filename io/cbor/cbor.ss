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
                             cfl= cflonum? enum-set?
                             fx1+ fx1- fxsra fxsrl fxsll fxvector? fxvector-length fxvector-ref fxvector-set!
                             include integer-length logbit? make-fxvector meta-cond procedure-arity-mask
                             record-writer time? time=? unbox void)

    ;; these predicates are equivalent to their r6rs counterparts,
    ;; only extended to also accept 1 argument
    (prefix (only (chezscheme) char=? char-ci=? record-constructor string=? string-ci=?)
            chez:)

    (only (scheme2k bootstrap)               assert* box-cas-strong! bwp-object check-interrupts fx<=?* raise-assertf raise-errorf)
    (only (scheme2k containers bytespan)     bytespan bytespan? bytespan-clear! bytespan-insert-right/bytevector! bytespan-length
                                             bytespan-peek-beg bytespan-peek-data bytespan-peek-end bytespan-resize-right!)
    (only (scheme2k containers bytevector)   bytevector-hash bytevector-uint-set*!)
    (only (scheme2k containers flvector)     flvector? flvector-length flvector-ref)
    (only (scheme2k containers hashtable)    eq-hashtable)
    (only (scheme2k containers utf8b)        bytespan-insert-right/string! string->utf8b-length)
    (only (scheme2k io obj)                  reader reader-close reader-eof?
                                             writer writer-close writer-eof?)
    (only (scheme2k io port)                 read-bytes-insert-right!)
    (only (scheme2k posix fd)                fd-close fd-read-insert-right! fd-write-all))


(include "io/cbor/constants.ss")
(include "io/cbor/common.ss")
;; (include "io/cbor/record.ss")
(include "io/cbor/vector.ss")
;; (include "io/cbor/get.ss")
;; (include "io/cbor/container.ss")
;; (include "io/cbor/misc.ss")
;; (include "io/cbor/status.ss")
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


(begin
  ;; (put/count) sanity check
  (assert* 'cbor-writer (not (fixnum? #x10000000000000000)))

#|
  (cbor-register-rtd (record-rtd (duration 0 0))     tag-time         get/time         put/time)
  (cbor-register-rtd (record-rtd (date 1970 1 1 +0)) tag-date         get/date         put/date)
  (cbor-register-rtd (record-rtd (span))             tag-span         get/span         put/span)
  (cbor-register-rtd (record-rtd (gbuffer))          tag-gbuffer      get/gbuffer      put/gbuffer)
  (cbor-register-rtd (record-rtd (bytespan))         tag-bytespan     get/bytespan     put/bytespan)
  (cbor-register-rtd (record-rtd (charspan))         tag-charspan24   get/charspan24   put/charspan)
  (cbor-register-rtd (record-rtd (ok))               tag-status       get/status       put/status)

  (vector-set! known-tag tag-charspan8     get/charspan8)
  (vector-set! known-tag tag-charspan16    get/charspan16)

  ;; customize how to serialize/deserialize `dir-entry` objects
  (cbor-register-rtd-reflect (record-type-descriptor dir-entry)   tag-dir-entry   make-dir-entry)

    ;; customize how to serialize/deserialize `rlimit` objects
  (cbor-register-rtd-reflect (record-type-descriptor rlimit-type) tag-rlimit      rlimit)
|#

) ; close begin

) ; close library
