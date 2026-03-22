;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;; CSV reader
;;;
(library (scheme2k io csv (1 0 0))
  (export
    ;; obj/reader.ss
    make-csv-reader csv-reader csv-reader?)
  (import
    (rnrs)
    (only (chezscheme)                   box box-cas! record-writer reverse! unbox void)
    (only (scheme2k bootstrap)           assert* while)
    (only (scheme2k containers bytespan) bytespan bytespan-clear! bytespan-empty? bytespan-delete-left! bytespan-delete-right!
                                         bytespan-insert-right/u8! bytespan-is-signed-base10-integer? bytespan-ref/u8 bytespan-ref-right/u8)
    (only (scheme2k containers utf8b)    utf8b-bytespan->string)
    (only (scheme2k io obj)              reader reader-eof?))


(define-record-type (csv-reader %make-csv-reader csv-reader?)
  (parent reader)
  (fields
    in-box              ; box containing either #f or binary input port
    rbuf                ; bytespan, read buffer
    close-in?)          ; boolean, #t if closing the csv-reader must close in.
  (protocol
    (lambda (args->new)
      (lambda (in close-in?)
        ((args->new csv-reader-get #f csv-reader-close)
          (box in) (bytespan) (and close-in? #t)))))
  (nongenerative csv-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define make-csv-reader
  (case-lambda
    ((in close-in?)
      (assert* 'make-csv-reader (input-port? in))
      (assert* 'make-csv-reader (binary-port? in))
      (%make-csv-reader in close-in?))
    ((in)
      (make-csv-reader in #f))))


;; read next byte and return it, or #f on EOF
(define (read-byte in)
  (let ((b (get-u8 in)))
    (and (fixnum? b) b)))

;; peek next byte and return it, or #f on EOF
(define (peek-byte in)
  (let ((b (lookahead-u8 in)))
    (and (fixnum? b) b)))


(define (trim rbuf)
  (while (and (not (bytespan-empty? rbuf)) (fx<=? (bytespan-ref/u8 rbuf 0) 32))
    (bytespan-delete-left! rbuf 1))
  (while (and (not (bytespan-empty? rbuf)) (fx<=? (bytespan-ref-right/u8 rbuf) 32))
    (bytespan-delete-right! rbuf 1))
  rbuf)


;; convert rbuf to number if possible, otherwise convert it to string
(define (to-item rbuf)
  (if (bytespan-empty? rbuf)
    (void)
    ;; TODO: parse as exact number if no exponent
    (let ((str (utf8b-bytespan->string rbuf)))
      (or (string->number str) str))))


(define (csv-reader-get-quoted in rbuf)
  (read-byte in) ; consume #\"
  (bytespan-clear! rbuf)
  (let %loop ((b (peek-byte in)) (in in) (rbuf rbuf))
    (case b
      ((#f)
        (to-item rbuf))
      ((34)
        (read-byte in) ; consume #\"
        (if (eqv? 34 (peek-byte in))
          (begin
            (bytespan-insert-right/u8! rbuf b)
            (read-byte in) ; consume #\"
            (%loop (peek-byte in) in rbuf))
          (to-item rbuf)))
      (else
        (bytespan-insert-right/u8! rbuf b)
        (%loop (peek-byte in) in rbuf)))))


(define (csv-reader-get-unquoted in rbuf)
  (bytespan-clear! rbuf)
  (let %loop ((b (peek-byte in)) (in in) (rbuf rbuf))
    (case b
      ((#f 10 13 44) ; EOF #\newline #\return #\,
        (to-item (trim rbuf)))
      (else
        (bytespan-insert-right/u8! rbuf b)
        (read-byte in) ; consume b
        (%loop (peek-byte in) in rbuf)))))


(define (csv-reader-get-token in rbuf)
  (let ((u8 (lookahead-u8 in)))
    (if (eof-object? u8)
      'eof
      (case u8
        ((10) ; #\newline
          (get-u8 in)
          'nl)
        ((13) ; #\return
          (get-u8 in)
          (csv-reader-get-token in rbuf))
        ((34) ; #\"
          (csv-reader-get-quoted in rbuf))
        ((44) ; #\,
          (get-u8 in)
          'comma)
        (else
          (csv-reader-get-unquoted in rbuf))))))


(define (csv-reader-get rx)
  (let %csv-reader-get ((in     (unbox (csv-reader-in-box rx)))
                        (rbuf   (csv-reader-rbuf rx))
                        (ret    '())
                        (comma? #f))
    ;; TODO: parse first line as column names
    (let ((token (csv-reader-get-token in rbuf)))
      (case token
        ((nl)    (values (reverse! ret) #t))
        ((eof)   (values (reverse! ret) (not (null? ret))))
        ((comma) (%csv-reader-get in rbuf (if comma? ret (cons (void) (cons 'name ret))) #f))
        (else    (%csv-reader-get in rbuf (cons token (cons 'name ret))                  #t))))))


(define (csv-reader-close rx)
  (let* ((in-box (csv-reader-in-box rx))
         (in     (unbox in-box)))
    ;; store #f in box, instead of fd or port.
    ;; close fd or port only if wire-reader or wire-writer own them
    (when (and in (box-cas! in-box in #f) (csv-reader-close-in? rx))
      (close-port in))))


;; customize how "csv-reader" objects are printed
(record-writer (record-type-descriptor csv-reader)
  (lambda (rx port writer)
    (put-string port "#<csv-reader ")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (unbox (csv-reader-in-box rx)) port)
    (put-char port #\>)))

) ; close library
