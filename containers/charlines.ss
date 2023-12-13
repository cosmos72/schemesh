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
    charlines-count-left charlines-count-right
    charlines-dirty-y-start charlines-dirty-y-end charlines-dirty-y-add! charlines-dirty-xy-unset!
    charlines-erase-at/cline! charlines-insert-at/cline! charlines-insert-at/ch!
    write-charlines)

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

;; get n-th line
(define charlines-ref   gbuffer-ref)

;; replace a charline in lines at y
(define (charlines-set/cline! lines y line)
  (assert-charline? 'charlines-set/cline! line)
  (charlines-dirty-y-add! lines y (fx1+ y))
  (gbuffer-set! lines y line))

(define (charlines-clear! lines)
  (charlines-dirty-y-add! lines 0 (charlines-length lines))
  (gbuffer-clear! lines))

;; erase a charline from lines at y
(define (charlines-erase-at/cline! lines y)
  (let ((yn (charlines-length lines)))
    (when (fx<? -1 y yn)
      (charlines-dirty-y-add! lines y yn)
      (gbuffer-erase-at! lines y 1))))

;; insert a charline into lines at y
(define (charlines-insert-at/cline! lines y line)
  (assert-charline? 'charlines-insert-at/cline! line)
  (gbuffer-insert-at! lines y line)
  (charlines-dirty-y-add! lines y (fx1+ (charlines-length lines))))

;; insert a char into lines at specified x and y
(define (charlines-insert-at/ch! lines x y ch)
  (let ((line (charlines-ref lines y)))
    (charline-insert-at! line x ch)))


;; search leftward starting from one character left of specified x and y,
;; find first character that satisfies (pred ch)
;; and return number of characters to skip until such character.
;; return #f if no character satisfies (pred ch)
;;
;; notes: if y < 0 returns #f
;;        if y >= (charlines-length lines)
;;                it is truncated to charlines-length - 1
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-count-left lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmin y (fx1- ny)))
    (while (fx>=? y 0)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (charline-find-left line xx pred)))
        (if pos
          (begin ;; match found
            (set! ret (fx+ ret (fx- xx pos)))
            (set! y -1)) ; finishes (while) loop
          (begin ;; match not found yet
            (set! x (greatest-fixnum))
            (set! y (fx1- y))
            (if (fx>=? y 0)
              (set! ret (fx+ ret xx))
              (set! ret #f)))))) ; (while) loop finished, no char found
      ret))


;; search rightward starting from specified x and y,
;; find first character that satisfies (pred ch)
;; and return number of characters to skip until such character.
;; return #f if no character satisfies (pred ch)
;;
;; notes: if y < 0 it is truncated to 0
;;        if y >= (charlines-length lines) returns #f
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-count-right lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmax 0 y))
    (while (fx<? y ny)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (charline-find-right line xx pred)))
        (if pos
          (begin ;; match found
            (set! ret (fx+ ret (fx- pos xx)))
            (set! y ny)) ; finishes (while) loop
          (begin ;; match not found yet
            (set! x 0)
            (set! y (fx1+ y))
            (if (fx<? y ny)
              (set! ret (fx+ ret (fx- len xx)))
              (set! ret #f)))))) ; (while) loop finished, no char found
      ret))



;; make a copy of strings str and store them into a newly created charlines
;; return the created charlines
(define (strings->charlines . str)
  (apply charlines (map string->charline str)))

;; store references to strings str into a newly created charlines
;; return the created charlines
(define (strings->charlines* . str)
  (apply charlines (map string->charline* str)))

;; write a textual representation of charlines to output port
(define (write-charlines lines port)
  (assert-charlines? "write-charlines" lines)
  (display "(strings->charlines*" port)
  (charlines-iterate lines
    (lambda (i line)
      (display #\space port)
      (write (charline->string line) port)))
  (display ")" port))

;; customize how "charlines" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (lines port writer)
    (write-charlines lines port)))

) ; close library
