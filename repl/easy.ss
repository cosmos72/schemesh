;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file repl/repl.ss


;; easy wrapper for (fd-read-all) (get-bytevector-all) (get-string-all) (reader->list)
(define (all obj)
  (cond
    ((fixnum? obj)      (fd-read-all obj))
    ((port? obj)        (if (binary-port? obj)
                          (get-bytevector-all obj)
                          (get-string-all obj)))
    ((obj-reader? obj)  (reader->list obj))))

  
;; easy wrapper for (fd-close) (close-port) (obj-reader-close) (obj-writer-close)
(define (close obj)
  (cond
    ((fixnum? obj)      (fd-close obj))
    ((port? obj)        (close-port obj))
    ((obj-reader? obj)  (obj-reader-close obj))
    ((obj-writer? obj)  (obj-writer-close obj))))


;; easy wrapper for (make-dir-reader)
(define dir
  (case-lambda
    ;; current directory charspan must NOT be modified => copy it
    (()     (make-dir-reader (charspan->string (sh-cwd))))
    ((path) (make-dir-reader path))))


;; easy wrapper for (port-eof?) (obj-reader-eof?) (obj-writer-eof?)
(define (eof? obj)
  (cond
    ((port? obj)        (port-eof? obj))
    ((obj-reader? obj)  (obj-reader-eof? obj))
    ((obj-writer? obj)  (obj-writer-eof? obj))))


;; easy wrapper for (fd-read) (get-bytevector-some) (get-line) (obj-reader-get)
;; always returns two values:
;;   either (values elem #t)
;;   or (values #<unspecified> #f) on eof
(define (get from)
  (cond
    ((fixnum? from)
      (let* ((bv (make-bytevector 4096 0))
             (n  (fd-read from bv)))
        (cond
          ((fxzero? n)
            (values #f #f))
          (else
            (bytevector-truncate! bv n)
            (values bv #t)))))
    ((port? from)
      (let ((got (if (binary-port? from)
                   (get-bytevector-some from)
                   (get-line from))))
        (values got (not (eof-object? got)))))
    ((obj-reader? from)
      (obj-reader-get from))
    (else
      (raise-errorf 'get "unsupported reader: ~s" from))))


;; easy wrapper for (fd-write-all) (put-bytevector) (put-string) (obj-writer-put)
;; returns unspecified value
(define (put to datum)
  (cond
    ((fixnum? to)
      (assert* 'put (bytevector? datum))
      (fd-write-all to datum))
    ((port? to)
      (cond
        ((binary-port? to)
          (assert* 'put (bytevector? datum))
          (put-bytevector to datum))
        (else
          (assert* 'put (string? datum))
          (put-string to datum))))
    ((obj-writer? to)
      (obj-writer-put to datum))
    (else
      (raise-errorf 'put "unsupported writer: ~s" to))))


;; iterate (get from) then (put to) until from is exhausted
(define (copy from to)
  (let-values (((datum ok?) (get from)))
    (when ok?
      (put to datum)
      (copy from to))))


;; easy wrapper for (make-json-reader)
(define from-json
  (case-lambda
    ((in)
      (make-json-reader in))
    (()
      (make-json-reader (sh-port #f 0 'binary)))))


;; easy wrapper for (make-queue-reader)
;; q must be a queue-writer
(define (from-queue q)
  (make-queue-reader q))


;; easy wrapper for (make-wire-reader)
(define from-wire
  (case-lambda
    ((in)
      (make-json-reader in))
    (()
      (make-json-reader (sh-port #f 0 'binary)))))


;; easy wrapper for (make-wire-writer)
(define to-json
  (case-lambda
    ((out)
      (make-json-writer out))
    (()
      (make-json-writer (sh-port #f 1 'textual)))))


;; easy wrapper for (make-queue-writer)
(define (to-queue)
  (make-queue-writer))


;; easy wrapper for (make-wire-writer)
(define to-wire
  (case-lambda
    ((out)
      (make-wire-writer out))
    (()
      (make-wire-writer (sh-port #f 1 'binary)))))


;; lazily create a reader that autodetects protocol upon the first call to (obj-reader-get)
;;
;; TODO: this must be a thread parameter.
;; further calls must return the same reader until current job changes
(define (from-stdin)
  (from-json))


;; TODO: choose writer protocol depending on optional arguments or stdout fd type:
;;   tty    => make-tabular-writer
;;   socket => make-wire-writer
;;   else   => make-json-writer
;;
;; TODO: this must be a thread parameter.
;; further calls must return the same writer until current job changes
(define (to-stdout)
  (to-json))
