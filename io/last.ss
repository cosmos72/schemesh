;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (scheme2k io last (1 0 0))
  (export
    make-last-reader last-reader last-reader?)
  (import
    (rnrs)
    (only (scheme2k bootstrap)                  assert* while)
    (only (scheme2k io obj)                     nested-reader nested-reader-inner-close nested-reader-inner-get reader?)
    (only (scheme2k containers circular-buffer) make-circular-buffer circular-buffer-delete-left!
                                                circular-buffer-empty? circular-buffer-full? circular-buffer-insert-right!))


;; Reader that wraps another "inner" reader
;; and returns only the last n elements of inner reader, then closes the inner reader.
(define-record-type (last-reader %make-last-reader last-reader?)
  (parent nested-reader)
  (fields
    circular-buffer)  ; #f or circular buffer containing up to n last elements
  (protocol
    (lambda (args->new)
      (lambda (inner n close-inner?)
         ((args->new (if (fxzero? n) %last-reader-get0 %last-reader-get)
                     #f (and close-inner? nested-reader-inner-close) inner)
          (if (fxzero? n)
              #f
              (make-circular-buffer n))))))
  (nongenerative %last-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; Create and return a last-reader that wraps another "inner" reader
;; and returns the last n elements of inner reader.
;;
;; Mandatory arguments:
;;   inner - the reader to wrap
;;
;; Optional arguments:
;;   n            - the number of elements to get. must an exact integer >= 0. defaults to 1
;;   close-inner? - if truish, closing the returned last-reader will close the inner reader.
;;                  defaults to #f
;;
;; Note: as per reader contract, by default closing a last-reader does NOT close
;; the wrapped reader, because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a last-reader should take ownership of the wrapped reader passed to the constructor,
;; then pass a truish value as the optional argument close-inner?
(define make-last-reader
  (case-lambda
    ((inner n close-inner?)
      (assert* 'make-last-reader (reader? inner))
      (assert* 'make-last-reader (fixnum? n))
      (assert* 'make-last-reader (fx>=? n 0))
      (%make-last-reader inner n close-inner?))
    ((inner n)
      (make-last-reader inner n #f))
    ((inner)
      (make-last-reader inner 1 #f))))


;; called by (last-reader-get) and (reader-get)
(define (%last-reader-get rx)
  (let-values (((obj ok?) (nested-reader-inner-get rx)))
    (let ((cbuf (last-reader-circular-buffer rx)))
      (cond
        (ok?
          (when (circular-buffer-full? cbuf)
            (circular-buffer-delete-left! cbuf))
          (circular-buffer-insert-right! cbuf obj)
          (%last-reader-get rx))
        ((circular-buffer-empty? cbuf)
          (values #f #f))
        (else
          (values
            (circular-buffer-delete-left! cbuf)
            #t))))))


(define (%last-reader-get0 rx)
  (values #f #f))

) ; close library
