;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/csv/csv.ss

;;; CSV writer
(define-record-type (csv-writer %make-csv-writer csv-writer?)
  (parent writer)
  (fields
    out-box             ; box containing either #f or binary output port
    close-out?          ; boolean, #t if closing the csv-writer must close out.
    (mutable cols)      ; #f or vector of symbols: the column names
    wbuf                ; bytespan, write buffer
    cache)              ; eq-hashtable, cache for (field) and (field-names)
  (protocol
    (lambda (args->new)
      (lambda (out close-out?)
        ((args->new csv-writer-put csv-writer-close)
          (box out) (and close-out? #t) #f (bytespan) (make-eq-hashtable)))))
  (nongenerative csv-writer-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define make-csv-writer
  (case-lambda
    ((out close-out?)
      (assert* 'make-csv-writer (output-port? out))
      (assert* 'make-csv-writer (binary-port? out))
      (%make-csv-writer out close-out?))
    ((out)
      (make-csv-writer out #f))))


(define (write/newline wbuf)
  (bytespan-insert-right/u8! wbuf 13) ; #\return
  (bytespan-insert-right/u8! wbuf 10)) ; #\newline


(define (write/unquoted-string wbuf str)
  (bytespan-insert-right/string! wbuf str))


(define (write/quoted-string wbuf str)
  (bytespan-insert-right/u8! wbuf 34) ; #\"
  ;; TODO: escape each #\" in string
  (bytespan-insert-right/string! wbuf str)
  (bytespan-insert-right/u8! wbuf 34)) ; #\"


(define (write/string wbuf str)
  ;; TODO: quote only if str contains #\" or characters <= #\space
  (write/quoted-string wbuf str))


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
      (write/string wbuf value))
    ((time? value)
      (write/time wbuf value))
    (else
      (write/string wbuf (format #f "~s" value)))))


(define (write/flush tx wbuf)
  (let ((out (unbox (csv-writer-out-box tx))))
    (unless out
      (raise-errorf 'csv-writer-put "not permitted on closed writer ~s" tx))
    (put-bytevector
      out
      (bytespan-peek-data wbuf)
      (bytespan-peek-beg  wbuf)
      (bytespan-length    wbuf))))


;; if column names are already known, return them.
;; otherwise extract field names from obj, store them in csv-writer-cols, write them to csv-writer-out, and and return them.
;; Column names are returned as a vector of symbols
(define (csv-writer-columns tx obj)
  (or (csv-writer-cols tx)
      (let ((cols (field-names obj (csv-writer-cache tx)))
            (wbuf (csv-writer-wbuf tx)))
        (csv-writer-cols-set! tx cols)
        (bytespan-clear! wbuf)
        (for-vector ((col cols))
          (unless (bytespan-empty? wbuf)
            (bytespan-insert-right/u8! wbuf 44)) ; #\,
          (write/quoted-string wbuf (symbol->string col)))
        (write/newline wbuf)
        (write/flush tx wbuf)
        cols)))


(define (csv-writer-put tx obj)
  (let ((cols  (csv-writer-columns tx obj))
        (cache (csv-writer-cache tx))
        (wbuf  (csv-writer-wbuf tx)))
    (bytespan-clear! wbuf)
    (for-vector ((col cols))
      (unless (bytespan-empty? wbuf)
        (bytespan-insert-right/u8! wbuf 44)) ; #\,
      (write/field wbuf (field obj col cache)))
    (write/newline wbuf)
    (write/flush tx wbuf)))


(define (csv-writer-close tx)
  (let* ((out-box (csv-writer-out-box tx))
         (out     (unbox out-box)))
    ;; store #f out box, instead of fd or port.
    ;; close fd or port only if wire-writer or wire-writer own them
    (when (and out (box-cas! out-box out #f) (csv-writer-close-out? tx))
      (close-port out))))
