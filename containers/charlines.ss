;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charlines (0 1))
  (export
    charlines charlines? strings->charlines strings->charlines*
    assert-charlines? charlines-copy-on-write charlines-iterate
    charlines-empty? charlines-length charlines-ref charlines-set/cline! charlines-clear!
    charlines-dirty-y-start charlines-dirty-y-end charlines-dirty-y-add! charlines-dirty-xy-unset!
    charlines-erase-at/cline! charlines-insert-at/cline! charlines-insert-at/ch!)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (schemesh bootstrap)       ;; while
    (schemesh containers misc) ;; list-iterate
    (schemesh containers span)
    (schemesh containers charline)
    (schemesh containers gbuffer))


;; copy-pasted from containers/gbuffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left)
     (mutable right))
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
(define (charlines . cline)
  (list-iterate cline (lambda (val) (assert-charline? 'charlines val)))
  (%make-charlines (span) (list->span cline) (greatest-fixnum) 0))

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

; get n-th line
(define charlines-ref   gbuffer-ref)

; get char in lines at position x and y.
; if lines are empty, return #f
; y is clamped to the available range [0, length of charlines - 1]
; x is clamped to the available range [0, length of y-th charline]
; if clamped x is == length of y-th charline:
;   if an implicit newline is present, return #\newline
;   otherwise if line is not empty, return the last char of the line
;   otherwise return #f
(define (charlines-ref/xy lines x y)
  (if (charlines-empty? lines)
    #f
    (let* ((yn   (charlines-length lines))
           (y    (fxmax 0 (fxmin y (fx1- yn))))
           (line (charline-ref lines y))
           (xn   (charline-length line))
           (x    (fxmax 0 (fxmin x xn))))
      (cond
        ((fx<? x xn)         (charline-ref line x))
        ((charline-nl? line) #\newline)
        ((fx>? xn 0)         (charline-ref line (fx1- xn)))
        (#t                  #f)))))


; replace a charline in lines at y
(define (charlines-set/cline! lines y line)
  (assert-charline? 'charlines-set/cline! line)
  (charlines-dirty-y-add! lines y (fx1+ y))
  (gbuffer-set! lines y line))

(define (charlines-clear! lines)
  (charlines-dirty-y-add! lines 0 (charlines-length lines))
  (gbuffer-clear! lines))

; erase a charline from lines at y
(define (charlines-erase-at/cline! lines y)
  (let ((yn (charlines-length lines)))
    (when (fx<? -1 y yn)
      (charlines-dirty-y-add! lines y yn)
      (gbuffer-erase-at! lines y 1))))

; insert a charline into lines at y
(define (charlines-insert-at/cline! lines y line)
  (assert-charline? 'charlines-insert-at/cline! line)
  (gbuffer-insert-at! lines y line)
  (charlines-dirty-y-add! lines y (fx1+ (charlines-length lines))))

; insert a char into lines at specified x and y
(define (charlines-insert-at/ch! lines x y ch)
  (let ((line (charlines-ref lines y)))
    (charline-insert-at! line x ch)))

;; make a copy of strings str and store them into a newly created charlines
;; return the created charlines
(define (strings->charlines . str)
  (apply charlines (map string->charline str)))

;; store references to strings str into a newly created charlines
;; return the created charlines
(define (strings->charlines* . str)
  (apply charlines (map string->charline* str)))

;; customize how "charlines" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (sp port writer)
    (display "(strings->charlines*" port)
    (charlines-iterate sp
      (lambda (i cline)
        (display #\space port)
        (write (charline->string cline) port)))
    (display ")" port)))

) ; close library
