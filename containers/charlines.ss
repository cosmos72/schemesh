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
    charlines-dirty-y-start charlines-dirty-y-end charlines-dirty-xy-unset!
    charlines-erase-left! charlines-erase-right!
    charlines-insert-at! charlines-insert-at/cspan! charlines-insert-at/cline!
    charlines-merge-line!)

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

; replace a charline in lines at y
(define (charlines-set/cline! lines y line)
  (assert-charline? 'charlines-set/cline! line)
  (charlines-dirty-y-add! lines y (fx1+ y))
  (gbuffer-set! lines y line))

(define (charlines-clear! lines)
  (charlines-dirty-y-add! lines 0 (charlines-length lines))
  (gbuffer-clear! lines))

; merge the two lines at y and y+1
(define (charlines-merge-line! lines y)
  (let ((ny (charlines-length lines)))
    (when (fx<? -1 y ny)
      (let* ((y+1 (fx1+ y))
             (line1 (charlines-ref lines y))
             (line2 (charlines-ref lines y+1)))
        (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 (charline-length line2))
        (charline-nl?-set! line1 (charline-nl? line2))
        (gbuffer-erase-at! lines y+1 1)
        (charlines-dirty-y-add! lines y+1 ny)))))


; erase one char to the left, starting from specified x and y
; if x = 0 and line at y - 1 ends with implicit newline,
; remove the implicit newline and merge the two lines at y and y - 1
; return two values: updated x and updated y
(define (charlines-erase-left! lines x y)
  (when (fx<? -1 y (charlines-length lines))
    (let* ((line1 (charlines-ref lines y))
           (len1 (charline-length line1)))
      (cond
        ((fx>? x 0)
          (let ((x-1 (fx1- x)))
            (charline-erase-at! line1 x-1 1)
            (values x-1 y)))
        ((fx>? y 0)
          (let* ((y-1 (fx1- y))
                 (line0 (charlines-ref lines y-1))
                 (len0  (charline-length line0)))
            (if (charline-nl? line0)
              (begin
                (charlines-merge-line! lines y-1)
                (values len0 y-1))
              (charlines-erase-left! lines len0 y-1))))
        (#t
          (values x y))))))

; erase one char to the right, starting from specified x and y.
; if line at y has length <= x and ends with implicit newline,
; remove the implicit newline and merge the two lines at y and y + 1
(define (charlines-erase-right! lines x y)
  (when (fx<? -1 y (charlines-length lines))
    (let* ((line (charlines-ref lines y))
           (len (charline-length line)))
      (cond
        ((fx<? x len)
          (charline-erase-at! line x 1))
        ((charline-nl? line)
          (charlines-merge-line! lines y))
        (#t
          (charlines-erase-right! lines 0 (fx1+ y)))))))

; insert a char into lines at specified x and y
(define (charlines-insert-at! lines x y ch)
  (let ((line (charlines-ref lines y)))
    (charline-insert-at! line x ch)))

; insert a charspan into lines starting at specified x and y
(define (charlines-insert-at/cspan! lines x y csp)
  (let ((line (charlines-ref lines y)))
    (charline-insert-at/cspan! line x csp)))

; insert a charline into lines at y
(define (charlines-insert-at/cline! lines y line)
  (assert-charline? 'charlines-insert-at/cline! line)
  (gbuffer-insert-at! lines y line)
  (charlines-dirty-y-add! lines y (fx1+ (charlines-length lines))))

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
