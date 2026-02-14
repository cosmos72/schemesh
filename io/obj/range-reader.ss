;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file io/obj/obj.ss


;; Reader that wraps another "inner" reader
;; and discards the first skip-n elements, then passes get-n elements, then closes the inner reader.
(define-record-type (range-reader %make-range-reader range-reader?)
  (parent nested-reader)
  (fields
    (mutable skip-n) ; number of elements still to skip
    (mutable get-n)) ; number of elements still to get, or #t if unlimited
  (protocol
    (lambda (args->new)
      (lambda (inner skip-n get-n close-inner?)
         ((args->new %range-reader-get %range-reader-skip (and close-inner? nested-reader-inner-close) inner)
            skip-n get-n))))
  (nongenerative %range-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; Create and return a range-reader that wraps another "inner" reader.
;; Mandatory arguments:
;;   inner - the reader to wrap
;;
;; Optional arguments:
;;   skip-n - the number of initial elements to skip.
;;            must be an exact integer >= 0, and defaults to 0
;;   get-n  - the number of elements to get after skipping skip-n ones.
;;            must an exact integer >= 0, or #t to indicate unlimited elements. defaults to #t
;;   close-inner? - if truish, closing the returned range-reader will close the inner reader.
;;                  defaults to #f
;;
;; At each call to (obj-reader-get) or (range-reader-get)
;; reads one element from the wrapped reader, then:
;;   if skip-n is > 0, decreases skip-n by one and retries
;;   if get-n is #t, returns the element
;;   if get-n is > 0, decreases get-n by one and returns the element
;;   otherwise returns end-of-reader
;;
;; Note: as per obj-reader contract, by default closing a range-reader does NOT close
;; the wrapped reader, because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a range-reader should take ownership of the wrapped reader passed to the constructor,
;; then pass a truish value as the optional argument close-inner?
(define make-range-reader
  (case-lambda
    ((inner skip-n get-n close-inner?)
      (assert* 'make-range-reader (obj-reader? inner))
      (assert* 'make-range-reader (integer? skip-n))
      (assert* 'make-range-reader (exact? skip-n))
      (assert* 'make-range-reader (>= skip-n 0))
      (unless (eq? #t get-n)
        (assert* 'make-range-reader (integer? get-n))
        (assert* 'make-range-reader (exact? get-n))
        (assert* 'make-range-reader (>= get-n 0)))
      (%make-range-reader inner (and (not (zero? skip-n)) skip-n) get-n close-inner?))
    ((inner skip-n get-n)
      (make-range-reader inner skip-n get-n #f))
    ((inner skip-n)
      (make-range-reader inner skip-n #t #f))
    ((inner)
      (make-range-reader inner 0 #t #f))))


(define (range-reader-eof? rx)
  (assert* 'range-reader-eof? (range-reader? rx))
  (obj-reader-eof? rx))


(define (range-reader-close rx)
  (assert* 'range-reader-close (range-reader? rx))
  (obj-reader-close rx))


;; return the wrapped, "inner" reader
(define (range-reader-inner rx)
  (assert* 'range-reader-inner (range-reader? rx))
  (nested-reader-inner rx))


(define (range-reader-get rx)
  (assert* 'range-reader-get (range-reader? rx))
  (obj-reader-get rx))


(define (range-reader-skip rx)
  (assert* 'range-reader-skip (range-reader? rx))
  (obj-reader-skip rx))


(define (skip-n-dec! rx skip-n)
  (let ((n (- skip-n 1)))
    (range-reader-skip-n-set! rx (and (not (zero? skip-n)) skip-n))))


(define (get-n-dec! rx get-n)
  (unless (eq? #t get-n)
    (let ((n (- get-n 1)))
      (range-reader-skip-n-set! rx (and (not (zero? get-n)) get-n)))))


;; called by (range-reader-get) and (obj-reader-get)
(define (%range-reader-get rx)
  (let ((skip-n (range-reader-skip-n rx))
        (get-n  (range-reader-get-n  rx)))
    (cond
      (skip-n
        (let ((ok? (nested-reader-inner-skip rx)))
          (skip-n-dec! rx skip-n)
          (if ok?
            (%range-reader-get rx) ; skipped one element, iterate
            (values #f #f))))      ; inner reader is exhausted
      (get-n
        (let-values (((obj ok?) (nested-reader-inner-get rx)))
          (get-n-dec! rx get-n)
          (values obj ok?)))
      (else
        (values #f #f)))))


;; called by (range-reader-skip) (obj-reader-skip)
(define (%range-reader-skip rx)
  (let ((ok?    (nested-reader-inner-skip rx))
        (skip-n (range-reader-skip-n rx))
        (get-n  (range-reader-get-n rx)))
    (cond
      ((not ok?)
        #f) ; inner reader is exhausted
      (skip-n
        (skip-n-dec! rx skip-n)
        (%range-reader-skip rx)) ; skipped one element, iterate
      (get-n
        (get-n-dec! rx get-n) ; consumed one element
        ok?)
      (else
        ok?))))
