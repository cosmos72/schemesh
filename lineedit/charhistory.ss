;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit charhistory (0 1))
  (export
    charhistory charhistory? make-charhistory
    charhistory-empty? charhistory-length charhistory-cow-ref charhistory-set*!
    charhistory-path charhistory-path-set!)
  (import
    (rnrs)
    (only (chezscheme)               fx1+ fx1- record-writer)
    (only (schemesh bootstrap)       raise-assertf while)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh containers span) span make-span list->span)
    (schemesh containers gbuffer)
    (only (schemesh containers charline) charline-empty?)
    (schemesh containers charlines))

;; copy-pasted from containers/gbuffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
    (mutable left  gbuffer-left  gbuffer-left-set!)
    (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))


;; type charhistory is a gbuffer containing charlines elements (the history itself)
(define-record-type
  (%charhistory %make-charhistory charhistory?)
  (parent %gbuffer)
  (fields
    (mutable path charhistory-path %charhistory-path-set!)) ; #f or string path where to load/save history
  (nongenerative #{%charhistory db0fmss41lc1voqx7uww5xnhu-28}))


(define (charhistory . vals)
  (list-iterate vals (lambda (val) (assert-charlines? 'charhistory val)))
  (%make-charhistory (span) (list->span vals) #f))


(define (make-charhistory n)
  ; optimization: (charhistory-cow-ref) returns a copy-on-write clone of i-th charline,
  ; thus we can reuse the same empty (charlines) for all elements
  (%make-charhistory (span) (make-span n (charlines)) #f))


(define (charhistory-path-set! hist path)
  (when (and path (not (string? path)))
    (raise-assertf 'charhistory-path-set! "~s is not #f or string" path))
  (%charhistory-path-set! hist path))


(define charhistory-empty? gbuffer-empty?)
(define charhistory-length gbuffer-length)

;; return a copy-on-write clone of i-th charlines in history
(define (charhistory-cow-ref hist idx)
  (charlines-copy-on-write (gbuffer-ref hist idx)))

;; if i is in range, set i-th charlines in history to a shallow copy of lines.
;; otherwise append shallow copy of lines to history.
;; do NOT insert lines in history if they are equal to another charlines at a nearby index.
;; Then remove consecutive empty charlines at indexes < i and update indexes accordingly.
;; returns two values:
;;   the inserted shallow copy of lines - or the existing lines that prevented insertion
;;   the index where lines were actually stored - or the index of existing lines that prevented insertion
(define (charhistory-set*! hist idx lines)
  (assert-charlines? 'charhistory-set*! lines)
  (let* ((len       (gbuffer-length hist))
         (idx-clamp (fxmax 0 (fxmin idx len)))
         (idx-eq    (charlines-find-nearby hist idx-clamp lines))
         (idx       (or idx-eq idx-clamp))
         (lines (if idx-eq
                  (gbuffer-ref hist idx-eq)
                  ; make a shallow copy of lines. Also helps in case lines is not
                  ; a charlines but a subclass of it, for example a vscreen
                  (charlines-shallow-copy lines))))
    (unless idx-eq
      (if (fx>=? idx len)
        (gbuffer-insert-at! hist idx lines)
        (gbuffer-set! hist idx lines)))

    ; remove empty lines at indexes < idx
    (let ((i (fx1- idx)))
      (while (and (fx>=? i 0) (%charlines-empty? (gbuffer-ref hist i)))
        (gbuffer-erase-at! i 1)
        (set! i (fx1- i))
        (set! idx (fx1- idx)))) ; also decrement idx

    (values lines idx)))


(define (%charlines-empty? lines)
  (or (charlines-empty? lines)
      (and (fx=? 1 (charlines-length lines))
           (charline-empty? (charlines-ref lines 0)))))


;; search for charlines equal to lines
;; at charhistory indexes (fx1- idx) idx (fx1+ idx)
;;
;; return index of equal charlines if found,
;; otherwise return #f
(define (charlines-find-nearby hist idx lines)
  (let* ((len   (gbuffer-length hist))
         (start (fxmax 0 (fx1- idx)))
         (end   (fxmin len (fx+ 2 idx)))
         (ret   #f))
    (do ((i start (fx1+ i)))
        ((or ret (fx>=? i end)) ret)
      (when (charlines-equal? (gbuffer-ref hist i) lines)
        (set! ret i)))))


;; customize how "charhistory" objects are printed
(record-writer (record-type-descriptor %charhistory)
  (lambda (hist port writer)
    (display "(charhistory" port)
    (gbuffer-iterate hist
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
