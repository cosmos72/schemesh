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

#!r6rs

;; this file should be included only by file bootstrap/bootstrap.ss


;; convention: #f is less than #t
(define (boolean-compare a b)
  (cond ((eq? a b) 0)
        (a         1)
        (else     -1)))


(define (char-compare a b)
  (cond ((char=? a b) 0)
        ((char>? a b) 1)
        (else         -1)))


;; compare two fixnums.
;; return 0 if (fx=? a b)
;; return -1 if (fx<? a b)
;; return +1 if (fx>? a b)
(define (fxcompare a b)
  (cond ((fx=? a b) 0)
        ((fx<? a b) -1)
        (else       +1)))


(define (number-compare a b)
  (cond
    ((or (eq? a b) (= a b))
      0)
    ((and (real? a) (real? b))
      (cond ((> a b) 1)
            ((< a b) -1)
            ;; (= a b) already checked above
            (else #f))) ;; not-a-numbers are unordered
    (else
      #f))) ;; complex numbers are unordered


(define (string-compare a b)
  (cond ((string=? a b) 0)
        ((string>? a b) 1)
        (else           -1)))


;; version of fx<=? that does not allocate up to 6 arguments
(define fx<=?*
  (case-lambda
    ((a b)         (fx<=? a b))
    ((a b c)       (fx<=? a b c))
    ((a b c d)     (and (fx<=? a b c) (fx<=? c d)))
    ((a b c d e)   (and (fx<=? a b c) (fx<=? c d e)))
    ((a b c d e f) (and (fx<=? a b c) (fx<=? c d e) (fx<=? e f)))))
