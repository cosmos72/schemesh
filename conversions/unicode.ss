;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k conversions unicode (0 9 2))
  (export char-display-wide?)
  (import
    (rnrs)
    (only (chezscheme) fx1+ include))


(define wide-table (make-eqv-hashtable))

(define (wide-set! lo hi)
  (do ((i lo (fx1+ i)))
      ((fx>? i hi))
    (hashtable-set! wide-table (integer->char i) #t)))


(define wide-lowest  (integer->char (include "conversions/gen-unicode-wide-lowest.ss")))
(define wide-highest (integer->char (include "conversions/gen-unicode-wide-highest.ss")))

(define (char-display-wide? ch)
  (and
    (char<=? wide-lowest ch wide-highest)
    (or
      (include "conversions/gen-unicode-wide-default.ss")
      (hashtable-ref wide-table ch #f))))

(include "conversions/gen-unicode-wide-set.ss")

) ; close library
