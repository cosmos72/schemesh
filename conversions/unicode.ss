;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k conversions unicode (1 0 0))
  (export char-display-width charspan-display-width string-display-width)
  (import
    (rnrs)
    (only (chezscheme)                   fx1+ include)
    (only (scheme2k bootstrap)           assert* fx<=?*)
    (only (scheme2k containers charspan) charspan-length charspan-peek-beg charspan-peek-data))


(define wide-table (make-eqv-hashtable))

(define (wide-set! lo hi)
  (do ((i lo (fx1+ i)))
      ((fx>? i hi))
    (hashtable-set! wide-table (integer->char i) #t)))


(define wide-lowest  (integer->char (include "conversions/gen-unicode-wide-lowest.ss")))
(define wide-highest (integer->char (include "conversions/gen-unicode-wide-highest.ss")))

(define (char-display-width ch)
  (cond
    ((or (char<? ch #\space) (char=? ch #\x7f))
      0)
    ((and (char<=? wide-lowest ch wide-highest)
          (or
            (include "conversions/gen-unicode-wide-default.ss")
            (hashtable-ref wide-table ch #f)))
      2)
    (else
      1)))

(define string-display-width
  (case-lambda
    ((str start end)
      (assert* 'string-display-width (fx<=?* 0 start end (string-length str)))
      (let %string-display-width ((pos start) (width 0))
        (if (fx<? pos end)
          (%string-display-width (fx1+ pos) (fx+ width (char-display-width (string-ref str pos))))
          width)))
    ((str)
      (string-display-width str 0 (string-length str)))))

(define charspan-display-width
  (case-lambda
    ((csp start end)
      (assert* 'charspan-display-width (fx<=?* 0 start end (charspan-length csp)))
      (let ((offset (charspan-peek-beg csp)))
        (string-display-width (charspan-peek-data csp) (fx+ offset start) (fx+ offset end))))
    ((csp)
      (charspan-display-width csp 0 (charspan-length csp)))))

(include "conversions/gen-unicode-wide-set.ss")

) ; close library
