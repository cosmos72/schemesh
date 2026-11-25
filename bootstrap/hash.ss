;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;; if this file is loaded multiple times, only the first one has any effect.
;; implementation note:
;; this is done by setting the top-level symbols sh-current-environment and sh-current-eval
;; only if they are not bound yet.

#!chezscheme

;; this file should be included only by file bootstrap/bootstrap.ss


(define-syntax fnv-prime
  (meta-cond ((fixnum? #xffffffff) (identifier-syntax #x100000001b3))
             (else                 (identifier-syntax #x1000193))))

(define-syntax fnv-offset
  ;; adapted to fit Chez Scheme fixnums
  (meta-cond ((fixnum? #xffffffff) (identifier-syntax #xbf29ce484222325))
             (else                 (identifier-syntax #x111c9dc5))))

(define (fxhash-init)
  fnv-offset)

(define (fxhash-update h key)
  (let ((h (fxlogxor key (#3%fx* h fnv-prime))))
    (fxlogxor h (fxsrl h 11))))

(define (fxhash-finish h)
  (fxand h (greatest-fixnum)))
