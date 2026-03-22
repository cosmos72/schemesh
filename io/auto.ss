;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


#!r6rs


(library (scheme2k io auto (1 0 0))
  (export
    auto-reader auto-reader? make-auto-reader)
  (import
    (rnrs)
    (only (chezscheme)                 fx1+ record-writer)
    (only (scheme2k bootstrap)         assert*)
    (only (scheme2k io json)           make-json-reader)
    (only (scheme2k io wire)           make-wire-reader)
    (only (scheme2k io obj)            empty-reader
                                       nested-reader nested-reader-inner nested-reader-inner-set!
                                       reader-eof? reader-get reader-skip reader-close))


;; Reader that autodetects protocol upon the first call to (reader-get) or (reader-skip)
(define-record-type (auto-reader %make-auto-reader auto-reader?)
  (parent nested-reader)
  (fields
    (mutable in)  ;; input port, or #f if protocol was already autodetected
    close-in?)    ;; flag passed to wrapped reader constructor
  (protocol
    (lambda (args->new)
      (lambda (in close-in?)
         ((args->new auto-reader-get auto-reader-skip (and close-in? auto-reader-close) #f)
            in close-in?))))
  (nongenerative %auto-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; Create and return a reader that autodetects protocol upon the first call to (reader-get) or (reader-skip)
(define make-auto-reader
  (case-lambda
    ((in close-in?)
      (assert* 'make-auto-reader (input-port? in))
      (assert* 'make-auto-reader (binary-port? in))
      (%make-auto-reader in (and close-in? #t)))
    ((in)
      (make-auto-reader in #f))))


;; Autodetect protocol if it's the first call to (reader-get) or (reader-skip),
;; otherwise just call (reader-get) on the inner reader
(define (auto-reader-get rx)
  (reader-get (auto-reader-inner rx)))


;; Autodetect protocol if it's the first call to (reader-get) or (reader-skip),
;; otherwise just call (reader-skip) on the inner reader
(define (auto-reader-skip rx)
  (reader-skip (auto-reader-inner rx)))


;; called only if close-in? passed to constructor was truish
(define (auto-reader-close rx)
  (let ((inner (nested-reader-inner rx)))
    (when inner
      (reader-close inner)))
  (let ((in (auto-reader-in rx)))
    (when in
      (auto-reader-in-set! rx #f)
      (close-port in))))


;; Autodetect protocol if it's the first call to (reader-get) or (reader-skip),
;; otherwise just return inner reader
(define (auto-reader-inner rx)
  (let ((inner (nested-reader-inner rx)))
    (or inner
        (let ((inner (auto-reader-make-inner rx)))
          (auto-reader-in-set! rx #f)
          (nested-reader-inner-set! rx inner)
          inner))))


;; Autodetect protocol and return an appropriate reader
(define (auto-reader-make-inner rx)
  (let* ((close-in? (auto-reader-close-in? rx))
         (in        (auto-reader-in rx))
         (u8        (and in (lookahead-u8 in))))
    (cond
      ((not (fixnum? u8))
        (empty-reader)) ; port is #f, or EOF reading from it
      ((fx=? 7 u8)
        (make-wire-reader in close-in?))
      (else
        (make-json-reader in close-in?)))))


;; customize how "auto-reader" objects are printed
(record-writer (record-type-descriptor auto-reader)
  (lambda (rx port writer)
    (put-string port "#<auto-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (or (nested-reader-inner rx) (auto-reader-in rx)) port)
    (put-char port #\>)))


) ; close library
