;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k io json (0 9 3))
  (export make-json-reader json-reader json-reader? json-reader-depth
          json-read-token  json-read-value json-skip-token json-skip-value
          json-write-token json-write-value)
  (import
    (rename (rnrs)                        (fxarithmetic-shift-left fx<<))
    (only (chezscheme)                    fx1+ fx1- include record-writer reverse! void)
    (only (scheme2k bootstrap)            assert* assert-not* raise-errorf)
    (only (scheme2k containers bytespan)  bytespan bytespan-clear! bytespan-delete-right! bytespan-insert-right/u8!
                                          bytespan-length bytespan-ref-right/u8 bytespan-set/u8!)
    (only (scheme2k containers list)      for-plist plist? plist-add)
    (only (scheme2k containers span)      for-span span span? span-insert-right! span-length span-ref)
    (only (scheme2k containers utf8b)     bytespan-insert-right/char! utf8b-bytespan->string)
    (only (scheme2k io stdio)             sh-stdin))


(include "io/json/read.ss")
(include "io/json/write.ss")


(record-writer (record-type-descriptor json-reader)
  (lambda (r out writer)
    (display "#<json-reader>" out)))

) ; close library


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (make-json-reader) (json-read-token) and (json-write-token)
;;
#|
(define (json-copy-all bin-in txt-out)
  (let loop ((r (make-json-reader bin-in)))
    (let ((tok (json-read-token r)))
      (unless (eof-object? tok)
        (json-write-token tok txt-out)
        (loop r)))))

(json-copy-all
  (open-bytevector-input-port
    (string->utf8 "{\"a\": [1, true]}"))
  (current-output-port))
|#
