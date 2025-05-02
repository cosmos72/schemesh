;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit ansi (0 9 0))
  (export
    ansi-text ansi-text? ansi-text-bytes ansi-text-clear! ansi-text-visible-length make-ansi-text
    string+ color
    black  red  green  yellow  blue  magenta  cyan  white
    black+ red+ green+ yellow+ blue+ magenta+ cyan+ white+)
  (import
    (rnrs)
    (only (chezscheme)                      record-writer)
    (only (schemesh bootstrap)              assert*)
    (only (schemesh containers bytespan)    make-bytespan bytespan? bytespan-clear!)
    (only (schemesh containers utf8b)       utf8b-bytespan->string)
    (only (schemesh containers utf8b utils) bytespan-insert-right/string!))


(define-record-type (ansi-text %make-ansi-text ansi-text?)
  (fields
     (mutable bytes) ; bytespan
     (mutable visible-length)) ; unsigned fixnum
  (nongenerative %ansi-text-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-ansi-text
  (case-lambda
    (()
      (%make-ansi-text (make-bytespan 0) 0))
    ((bytes visible-length)
      (assert* 'make-ansi-text (bytespan? bytes))
      (assert* 'make-ansi-text (fixnum? visible-length))
      (assert* 'make-ansi-text (fx>=? visible-length 0))
      (%make-ansi-text bytes visible-length))))


(define (ansi-text-clear! a)
  (bytespan-clear! (ansi-text-bytes a))
  (ansi-text-visible-length-set! a 0))

(define string+
  (case-lambda
    ((a str visible-length)
      (bytespan-insert-right/string! (ansi-text-bytes a) str)
      (ansi-text-visible-length-set! a (fx+ visible-length (ansi-text-visible-length a)))
      a)
    ((a str)
      (string+ a str (string-length str)))))


(define (color a col-seq str)
  (string+ a "\x1b;[" 0)
  (string+ a col-seq 0)
  (string+ a "m" 0)
  (string+ a str (string-length str))
  (string+ a "\x1b;[m" 0)
  a)


(define (black    a str) (color a "30" str))
(define (red      a str) (color a "31" str))
(define (green    a str) (color a "32" str))
(define (yellow   a str) (color a "33" str))
(define (blue     a str) (color a "34" str))
(define (magenta  a str) (color a "35" str))
(define (cyan     a str) (color a "36" str))
(define (white    a str) (color a "37" str))

(define (black+   a str) (color a "1;30" str))
(define (red+     a str) (color a "1;31" str))
(define (green+   a str) (color a "1;32" str))
(define (yellow+  a str) (color a "1;33" str))
(define (blue+    a str) (color a "1;34" str))
(define (magenta+ a str) (color a "1;35" str))
(define (cyan+    a str) (color a "1;36" str))
(define (white+   a str) (color a "1;37" str))


;; customize how "ansi-text" objects are printed
(record-writer (record-type-descriptor ansi-text)
  (lambda (a port writer)
    (display "(make-ansi-text (bytevector->bytespan* (string->utf8b " port)
    (write   (utf8b-bytespan->string (ansi-text-bytes a)) port)
    (display ")) " port)
    (display (ansi-text-visible-length a) port)
    (display ")" port)))

) ; close library
