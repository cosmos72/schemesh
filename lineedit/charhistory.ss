;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit charhistory (0 1))
  (export
    charhistory charhistory? make-charhistory
    charhistory-empty? charhistory-length charhistory-cow-ref charhistory-set*!)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer)
    (only (schemesh containers misc) list-iterate)
    (schemesh containers span)
    (schemesh containers charlines))

;; copy-pasted from containers/span.ss
(define-record-type
  (%span %make-span %span?)
  (fields
     (mutable beg span-beg span-beg-set!)
     (mutable end span-end span-end-set!)
     (mutable vec span-vec span-vec-set!))
  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))


;; type charhistory is a span containing charlines elements (the history itself)
(define-record-type
  (%charhistory %make-charhistory charhistory?)
  (parent %span)
  (nongenerative #{%charhistory db0fmss41lc1voqx7uww5xnhu-28}))

(define (charhistory . vals)
  (list-iterate vals (lambda (val) (assert-charlines? 'charhistory val)))
  (let ((vec (list->vector vals)))
    (%make-charhistory 0 (vector-length vec) vec)))

(define (make-charhistory n)
  ;; optimization: (charhistory-cow-ref) returns a copy-on-write clone of i-th
  ; charline, thus we can reuse the same empty (charlines) for all elements
  (%make-charhistory 0 n (make-vector n (charlines))))

(define charhistory-empty? span-empty?)
(define charhistory-length span-length)

;; return a copy-on-write clone of i-th charlines in history
(define (charhistory-cow-ref hist idx)
  (charlines-copy-on-write (span-ref hist idx)))

;; set i-th charlines in history to a shallow copy of lines, and return such copy
;; resizes history if needed.
(define (charhistory-set*! hist idx lines)
  (assert-charlines? 'charhistory-set*! lines)
  (let ((len (span-length hist)))
    (when (fx>=? idx len)
      (span-resize-back! hist (fx1+ idx))
      ; optimization: (charhistory-cow-ref) returns a copy-on-write clone of i-th
      ; charlines, thus we can reuse the same empty (charlines) for all elements we add
      (let ((empty-lines (charlines)))
        (do ((i len (fx1+ i)))
            ((fx>=? i idx))
          (span-set! hist i empty-lines)))))
  ;; make a shallow copy of lines. Also helps in case
  ;; lines is a subclass of charlines - for example a vscreen
  (let ((lines (charlines-shallow-copy lines)))
    (span-set! hist idx lines)
    lines))


;; customize how "charhistory" objects are printed
(record-writer (record-type-descriptor %charhistory)
  (lambda (hist port writer)
    (display "(charhistory" port)
    (span-iterate hist
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
