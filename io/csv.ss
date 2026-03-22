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
    (only (scheme2k bootstrap)           assert* raise-errorf while)
    (only (scheme2k containers bytespan) bytespan bytespan-clear! bytespan-empty? bytespan-delete-left! bytespan-delete-right!
                                         bytespan-insert-right/u8! bytespan-is-signed-base10-integer? bytespan-ref/u8 bytespan-ref-right/u8
                                         bytespan->real)
    (only (scheme2k containers utf8b)    utf8b-bytespan->string)
    (only (scheme2k io obj)              reader reader-eof?))


(define-record-type (csv-reader %make-csv-reader csv-reader?)
  (parent reader)
  (fields
    in-box              ; box containing either #f or binary input port
    close-in?           ; boolean, #t if closing the csv-reader must close in.
    (mutable cols)      ; #f or list of column name
    rbuf)               ; bytespan, read buffer
  (protocol
    (lambda (args->new)
      (lambda (in close-in?)
        ((args->new csv-reader-get #f csv-reader-close)
          (box in) (and close-in? #t) #f (bytespan)))))
  (nongenerative csv-reader-7c46d04b-34f4-4046-b5c7-b63753c1be41))


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


(define (trim! rbuf)
  (while (and (not (bytespan-empty? rbuf)) (fx<=? (bytespan-ref/u8 rbuf 0) 32))
    (bytespan-delete-left! rbuf 1))
  (while (and (not (bytespan-empty? rbuf)) (fx<=? (bytespan-ref-right/u8 rbuf 0) 32))
    (bytespan-delete-right! rbuf 1)))


;; convert rbuf to real number if possible, otherwise convert it to string
(define (to-item rbuf header?)
  (if (bytespan-empty? rbuf)
    (if header? "name" (void))
    (if header?
      (utf8b-bytespan->string rbuf)
      (or (bytespan->real rbuf)
          (utf8b-bytespan->string rbuf)))))


(define (csv-reader-get-quoted in rbuf header?)
  (read-byte in) ; consume #\"
  (bytespan-clear! rbuf)
  (let %loop ((b (peek-byte in)))
    (case b
      ((#f)
        (to-item rbuf header?))
      ((34)
        (read-byte in) ; consume #\"
        (if (eqv? 34 (peek-byte in))
          (begin
            (bytespan-insert-right/u8! rbuf b)
            (read-byte in) ; consume #\"
            (%loop (peek-byte in)))
          (to-item rbuf header?)))
      (else
        (bytespan-insert-right/u8! rbuf b)
        (read-byte in) ; consume b
        (%loop (peek-byte in))))))


(define (csv-reader-get-unquoted in rbuf header?)
  (bytespan-clear! rbuf)
  (let %loop ((b (peek-byte in)))
    (case b
      ((#f 10 13 44) ; EOF #\newline #\return #\,
        ;; RFC 4180 states: Spaces are considered part of a field and should not be ignored.
        ;; We ignore it and trim unquoted fields
        (trim! rbuf)
        (to-item rbuf header?))
      ((34)
        (raise-errorf 'csv-reader-get "~s not allowed in unquoted field" #\"))
      (else
        (bytespan-insert-right/u8! rbuf b)
        (read-byte in) ; consume b
        (%loop (peek-byte in))))))


(define (csv-reader-get-token in rbuf header?)
  (let ((u8 (peek-byte in)))
    (case u8
      ((#f)
       'eof)
      ((10) ; #\newline
        (read-byte in)
        'nl)
      ((13) ; #\return
        (read-byte in)
        (csv-reader-get-token in rbuf header?))
      ((34) ; #\"
        (csv-reader-get-quoted in rbuf header?))
      ((44) ; #\,
        (read-byte in)
        'comma)
      (else
        (csv-reader-get-unquoted in rbuf header?)))))


;; parse column names and return them as a symbol list
(define (csv-reader-get-columns rx)
  (let ((in   (unbox (csv-reader-in-box rx)))
        (rbuf (csv-reader-rbuf rx)))
    (let %csv-reader-columns ((cols '()) (comma? #f))
      (let ((token (csv-reader-get-token in rbuf #t)))
        (case token
          ((nl eof) (reverse! cols))
          ((comma)  (%csv-reader-columns (if comma? cols (cons 'name cols)) #f))
          (else     (%csv-reader-columns (cons (string->symbol token) cols) #t)))))))
          

;; if column names are already parsed, return them.
;; otherwise parse them, store them in csv-reader-cols and and return them
(define (csv-reader-columns rx)
  (or (csv-reader-cols rx)
      (let ((cols (csv-reader-get-columns rx)))
        (csv-reader-cols-set! rx cols)
        cols)))


;; return (values plist #t) containing next CSV entry,
;; or (values #<unspecified> #f) on EOF
(define (csv-reader-get rx)
  (let ((in   (unbox (csv-reader-in-box rx)))
        (rbuf (csv-reader-rbuf rx)))
    (let %csv-reader-get ((ret '()) (comma? #f) (cols (csv-reader-columns rx)))
      (let ((token (csv-reader-get-token in rbuf #f)))
        (case token
          ((nl)    (values (reverse! ret) #t))
          ((eof)   (values (reverse! ret) (not (null? ret))))
          ((comma) (if comma?
                     (%csv-reader-get ret #f cols)
                     (let* ((col  (if (null? cols) 'name (car cols)))
                            (cols (if (null? cols) cols (cdr cols)))
                            (ret  (cons (void) (cons col ret))))
                       (%csv-reader-get ret #f cols))))
          (else     (let* ((col  (if (null? cols) 'name (car cols)))
                           (cols (if (null? cols) cols (cdr cols)))
                           (ret  (cons token (cons col ret))))
                      (%csv-reader-get ret #t cols))))))))


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
