;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/wire/wire.ss

;; customize how "status" objects are serialized/deserialized


;; copy-pasted from posix/status.ss
(define-record-type (%status %make-status %status?)
  (fields
    (immutable kind %status->kind) ; symbol
    (immutable val  %status->val))
  (nongenerative %status-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define known-kind (plist->eq-hashtable '(new 0 running 1 stopped 2 exception 3 failed 4 killed 5 ok 6)))

(define (kind->int kind)
  (hashtable-ref known-kind kind #f))

(define int->kind
  (let ((vec (make-vector (hashtable-size known-kind))))
    (for-hash ((kind int known-kind))
      (vector-set! vec int kind))
    (lambda (int)
      (if (fx<? -1 int (vector-length vec))
        (vector-ref vec int)
        #f))))

(define (len/status pos obj)
  (len/any (fx+ pos 2) (%status->val obj)))

;; tag was already read and consumed. only read serialized kind and val.
(define (get/status bv pos end)
  (let ((kind (int->kind (bytevector-u8-ref bv pos))))
    (if kind
      (let-values (((value pos) (get/any bv (fx1+ pos) end)))
        (if pos
          (values (%make-status kind value) pos)
          (values #f #f)))
    (values #f #f))))

(define (put/status bv pos obj)
  (let ((kind (%status->kind obj)))
    (bytevector-u8-set! bv pos tag-status)
    (bytevector-u8-set! bv (fx1+ pos) (kind->int kind))
    (put/any bv (fx+ pos 2) (%status->val obj))))

