;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charlines (0 1))
  (export
    charlines charlines? assert-charlines?
    charlines-iterate charlines-empty? charlines-length charlines-ref
    charlines-clear! charlines-copy-on-write charlines-erase-at/cline! charlines-insert-at/cline!
    charlines-dirty-y-start charlines-dirty-y-end charlines-dirty-xy-unset!)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (schemesh containers misc) ;; list-iterate
    (schemesh containers span)
    (schemesh containers charline)
    (schemesh containers gbuffer))


;; copy-pasted from containers/gbuffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left  gbuffer-left  gbuffer-left-set!)
     (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))

;; type charlines is a gap-buffer, containing charline elements
(define-record-type
  (%charlines %make-charlines charlines?)
  (parent %gbuffer)
  (fields
    (mutable dirty-y-start charlines-dirty-y-start charlines-dirty-y-start-set!)
    (mutable dirty-y-end   charlines-dirty-y-end   charlines-dirty-y-end-set!))
  (nongenerative #{%charlines lf2lr8d65f8atnffcpi1ja7l0-439}))


(define (assert-charlines? who lines)
  (unless (charlines? lines)
    (assertion-violation who "not a charlines" lines)))

;; create a charlines from zero or more charline
(define (charlines . vals)
  (list-iterate vals (lambda (val) (assert-charline? 'charlines val)))
  (%make-charlines (span) (list->span vals) (greatest-fixnum) 0))

(define charlines-iterate    gbuffer-iterate)
(define charlines-empty?     gbuffer-empty?)
(define charlines-length     gbuffer-length)

(define (charlines-dirty-y-add! lines start end)
  (charlines-dirty-y-start-set! lines (fxmin (charlines-dirty-y-start lines)))
  (charlines-dirty-y-end-set!   lines (fxmax (charlines-dirty-y-end   lines))))

(define (charlines-dirty-xy-unset! lines)
  (charlines-dirty-y-start-set! lines (greatest-fixnum))
  (charlines-dirty-y-end-set!   lines 0)
  (charlines-iterate lines
    (lambda (i line)
      (charline-dirty-x-unset! line))))

(define (charlines-clear! lines)
  (charlines-dirty-y-add! lines 0 (charlines-length lines))
  (gbuffer-clear! lines))

; erase n charline from lines, beginning from position start
(define (charlines-erase-at/cline! lines start n)
  (when (fx>? n 0)
    (charlines-dirty-y-add! lines start (charlines-length lines))
    (gbuffer-erase-at! start n)))

; insert a charline into lines at position idx
(define (charlines-insert-at/cline! lines idx line)
  (assert-charline? 'charlines-insert-at/cline! line)
  (charlines-dirty-y-add! lines idx (fx1+ (charlines-length lines)))
  (gbuffer-insert-at! lines idx line))

(define charlines-ref   gbuffer-ref)

; replace a charline in lines at position idx
(define (charlines-set/cline! lines idx line)
  (assert-charline?  'charlines-set/cline! line)
  (charlines-dirty-y-add! idx (fx1+ idx))
  (gbuffer-set! lines idx line))

;; Return a copy-on-write clone of charlines.
;; Also calls (charline-copy-on-write) on each line.
(define (charlines-copy-on-write lines)
  (let ((dst (make-span (charlines-length lines))))
    (charlines-iterate lines
      (lambda (i line)
        (span-set! dst i (charline-copy-on-write line))))
    (%make-charlines (span) dst
      (charlines-dirty-y-start lines)
      (charlines-dirty-y-end   lines))))


;; customize how "charlines" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (sp port writer)
    (display "(charlines" port)
    (charlines-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
