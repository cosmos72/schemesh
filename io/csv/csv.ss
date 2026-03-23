;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;; CSV reader and writer
;;;
(library (scheme2k io csv (1 0 0))
  (export make-csv-reader csv-reader csv-reader?
          make-csv-writer csv-writer csv-writer?)
  (import
    (rnrs)
    (only (chezscheme)                   box box-cas! format include record-writer reverse!
                                         time? time-nanosecond time-second unbox void)
    (only (scheme2k bootstrap)           assert* raise-errorf while)
    (only (scheme2k containers bytespan) bytespan bytespan-clear! bytespan-delete-left! bytespan-delete-right!
                                         bytespan-display-right/integer! bytespan-display-right/unsigned-k-digits!
                                         bytespan-empty? bytespan-insert-right/u8! bytespan-length
                                         bytespan-peek-beg bytespan-peek-data
                                         bytespan-ref/u8 bytespan-ref-right/u8 bytespan->real)
    (only (scheme2k containers string)   string-index-right)
    (only (scheme2k containers vector)   for-vector)
    (only (scheme2k containers utf8b)    bytespan-insert-right/string! utf8b-bytespan->string)
    (only (scheme2k io obj)              reader reader-eof? writer writer-eof?)
    (only (scheme2k reflect)             field field-names))


(include "io/csv/reader.ss")
(include "io/csv/writer.ss")


;; customize how "csv-reader" objects are printed
(record-writer (record-type-descriptor csv-reader)
  (lambda (rx port writer)
    (put-string port "#<csv-reader ")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (unbox (csv-reader-in-box rx)) port)
    (put-char port #\>)))


;; customize how "csv-writer" objects are printed
(record-writer (record-type-descriptor csv-writer)
  (lambda (rx port writer)
    (put-string port "#<csv-writer ")
    (put-string port (if (writer-eof? rx) " eof " " ok "))
    (writer (unbox (csv-writer-out-box rx)) port)
    (put-char port #\>)))

) ; close library
