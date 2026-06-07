;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;; CSV reader and writer
;;;
(library (scheme2k io text0 (1 0 0))
  (export make-text0-writer text0-writer text0-writer?)
  (import
    (rnrs)
    (only (chezscheme)                   box format fx1+ include record-writer reverse!
                                         time? time-nanosecond time-second unbox void)
    (only (scheme2k bootstrap)           assert* box-cas-strong! raise-errorf while)
    (only (scheme2k containers bytespan) bytespan bytespan-clear! bytespan-delete-left! bytespan-delete-right!
                                         bytespan-display-right/integer! bytespan-display-right/unsigned-k-digits!
                                         bytespan-empty? bytespan-insert-right/u8! bytespan-length
                                         bytespan-peek-beg bytespan-peek-data
                                         bytespan-ref/u8 bytespan-ref-right/u8 bytespan->real)
    (only (scheme2k containers string)   string-index string-index-right)
    (only (scheme2k containers vector)   for-vector)
    (only (scheme2k containers utf8b)    bytespan-insert-right/string! utf8b-bytespan->string)
    (only (scheme2k io obj)              reader reader-eof? writer writer-eof?)
    (only (scheme2k reflect)             field field-names))



;;; CSV writer
(define-record-type (text0-writer %make-text0-writer text0-writer?)
  (parent writer)
  (fields
    out-box             ; box containing either #f or binary output port
    close-out?          ; boolean, #t if closing the text0-writer must close out.
    (mutable cols)      ; #f or vector of symbols: the column names
    wbuf                ; bytespan, write buffer
    cache)              ; eq-hashtable, cache for (field) and (field-names)
  (protocol
    (lambda (args->new)
      (lambda (out close-out?)
        ((args->new text0-writer-put text0-writer-close)
          (box out) (and close-out? #t) #f (bytespan) (make-eq-hashtable)))))
  (nongenerative text0-writer-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define make-text0-writer
  (case-lambda
    ((out close-out?)
      (assert* 'make-text0-writer (output-port? out))
      (assert* 'make-text0-writer (binary-port? out))
      (%make-text0-writer out close-out?))
    ((out)
      (make-text0-writer out #f))))


(define (write/unquoted-string wbuf str)
  (bytespan-insert-right/string! wbuf str))


;; copy-pasted from io/json/writer.ss
(define (write/ratio wbuf num)
  (let* ((neg? (< num 0))
         (num  (if neg? (- num) num)))
    (when neg?
      (bytespan-insert-right/u8! wbuf 45)) ; #\-
    (let-values (((integer fraction) (div-and-mod num 1)))
      (bytespan-display-right/integer! wbuf integer)
      (unless (zero? fraction)
        (let ((fraction*1e16 (div (* fraction 10000000000000000) 1)))
          (unless (zero? fraction*1e16)
            (bytespan-insert-right/u8! wbuf 46) ; #\.
            (bytespan-display-right/unsigned-k-digits! wbuf fraction*1e16 16)
            ;; remove least significant zeroes
            (do ()
                ((not (fx=? 48 (bytespan-ref-right/u8 wbuf 0))))
              (bytespan-delete-right! wbuf 1))))))))


(define (write/real wbuf num)
  (let ((str (number->string num 10)))
    (write/unquoted-string wbuf str)
    (unless (string-index-right str #\e)
      (bytespan-insert-right/u8! wbuf 101)   ; #\e
      (bytespan-insert-right/u8! wbuf 48)))) ; #\0


(define (write/number wbuf num)
  (cond
    ((not (real? num))
      (write/unquoted-string wbuf (number->string num 10)))
    ((not (exact? num))
      (write/real wbuf num))
    ((not (integer? num))
      (write/ratio wbuf num))
    (else
      (bytespan-display-right/integer! wbuf num))))


(define (write/time wbuf obj)
  (write/ratio wbuf
    (+ (time-second obj) (/ (time-nanosecond obj) 1000000000))))


(define (write/field wbuf value)
  (cond
    ((eq? (void) value)
      (void))
    ((and (integer? value) (exact? value))
      (bytespan-display-right/integer! wbuf value))
    ((number? value)
      (write/number wbuf value))
    ((string? value)
      (write/unquoted-string wbuf value))
    ((time? value)
      (write/time wbuf value))
    (else
      (write/unquoted-string wbuf (format #f "~s" value)))))


(define (write/flush tx wbuf)
  (let ((out (unbox (text0-writer-out-box tx))))
    (unless out
      (raise-errorf 'text0-writer-put "not permitted on closed writer ~s" tx))
    (put-bytevector
      out
      (bytespan-peek-data wbuf)
      (bytespan-peek-beg  wbuf)
      (bytespan-length    wbuf))))


;; if column names are already known, return them.
;; otherwise extract field names from obj, store them in text0-writer-cols, write them to text0-writer-out, and and return them.
;; Column names are returned as a vector of symbols
(define (text0-writer-columns tx obj)
  (or (text0-writer-cols tx)
      (let ((cols (field-names obj (text0-writer-cache tx)))
            (wbuf (text0-writer-wbuf tx)))
        (text0-writer-cols-set! tx cols)
        cols)))


(define (text0-writer-put tx obj)
  (let ((cols  (text0-writer-columns tx obj))
        (cache (text0-writer-cache tx))
        (wbuf  (text0-writer-wbuf tx)))
    (bytespan-clear! wbuf)
    (for-vector ((col cols))
      (unless (eq? '<type> col)
        (write/field wbuf (field obj col cache))
        (bytespan-insert-right/u8! wbuf 0)))
    (write/flush tx wbuf)))


(define (text0-writer-close tx)
  (let* ((out-box (text0-writer-out-box tx))
         (out     (unbox out-box)))
    ;; store #f out box, instead of fd or port.
    ;; close fd or port only if wire-writer or wire-writer own them
    (when (and out (box-cas-strong! out-box out #f) (text0-writer-close-out? tx))
      (close-port out))))


;; customize how "text0-writer" objects are printed
(record-writer (record-type-descriptor text0-writer)
  (lambda (rx port writer)
    (put-string port "#<text0-writer ")
    (put-string port (if (writer-eof? rx) " eof " " ok "))
    (writer (unbox (text0-writer-out-box rx)) port)
    (put-char port #\>)))

) ; close library
