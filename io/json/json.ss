;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k io json (0 9 3))
  (export make-json-reader json-reader json-reader? json-reader-depth json-reader-restart
          make-json-writer json-writer json-writer?

          json-reader-get-token json-reader-get-value json-reader-skip-token json-reader-skip-value
          json-writer-put-token json-writer-put-value

          json-reflect-info-set!)
  (import
    (rename (rnrs)                        (fxarithmetic-shift-left fx<<))
    (only (chezscheme)                    fx1+ fx1- include logbit? make-time port-closed? procedure-arity-mask ratnum?
                                          record-writer reverse! string-truncate! time-type time-second time-nanosecond void)
    (only (scheme2k bootstrap)            assert* assert-not* for raise-errorf)
    (only (scheme2k containers bytespan)  bytespan bytespan? bytespan-clear! bytespan-delete-right!
                                          bytespan-display-right/integer! bytespan-display-right/unsigned-k-digits!
                                          bytespan-insert-right/bytevector! bytespan-insert-right/u8!
                                          bytespan-length bytespan-peek-beg bytespan-peek-data bytespan-peek-end
                                          bytespan-ref/u8 bytespan-ref-right/u8 bytespan-set/u8! bytespan-resize-right!)
    (only (scheme2k containers date)      date date->string string->date)
    (only (scheme2k containers hashtable) eq-hashtable)
    (only (scheme2k containers list)      for-plist plist? plist-add plist-ref)
    (only (scheme2k containers ordered-hash) in-ordered-hash ordered-hash-keys ordered-hash-set!)
    (only (scheme2k containers span)      for-span span span? span-insert-right! span-length span-ref)
    (only (scheme2k containers string)    string-index-right)
    (only (scheme2k containers utf8b)     bytespan-insert-right/char! bytespan-insert-right/string!
                                          utf8b-bytespan->string utf8b->string)
    (only (scheme2k io obj)               reader reader-eof? reader-close reader-get reader-skip
                                          writer writer-eof? writer-close writer-put)
    (only (scheme2k io base64)            put-base64 base64-string->bytevector)
    (only (scheme2k io stdio)             sh-stdin sh-stdout)
    (only (scheme2k posix fs)             dir-entry make-dir-entry)
    (scheme2k reflect))


(include "io/json/info.ss")
(include "io/json/reader.ss")
(include "io/json/writer.ss")


(record-writer (record-type-descriptor json-reader)
  (lambda (rx port writer)
    (put-string port "#<json-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (json-reader-in rx) port)
    (put-char port #\>)))


(record-writer (record-type-descriptor json-writer)
  (lambda (tx port writer)
    (put-string port "#<json-writer")
    (put-string port (if (writer-eof? tx) " eof " " ok "))
    (writer (json-writer-out tx) port)
    (put-char port #\>)))

) ; close library


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example usage for (make-json-reader) (json-reader-get-token) and (json-writer-put-token)
;;
#|
(define (json-copy-all bin-in txt-out)
  (let loop ((r (make-json-reader bin-in))
             (w (make-json-writer txt-out)))
    (let ((tok (json-reader-get-token r)))
      (unless (eof-object? tok)
        (json-writer-put-token w tok)
        (loop r w)))))

(json-copy-all
  (open-bytevector-input-port
    (string->utf8 "{\"a\": [1, true]}"))
  (current-output-port))
|#
