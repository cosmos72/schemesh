;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell status (0 7 5))
  (export
    sh-status sh-status? sh-status-kind sh-status-result sh-status-results
)
  (import
    (rnrs)
    (only (chezscheme)           record-writer  void))


;; Represent status of a job.
;; Kind can be of: 'new 'running 'stopped 'ok 'failed 'killed 'exception
;; Result possible values depend on kind:
;;   if kind is 'new       -> result is 0
;;   if kind is 'running   -> result is #f or job-id
;;   if kind is 'ok        -> results are a list of arbitrary Scheme values
;;   if kind is 'failed    -> result is an 8-bit C exit status or an arbitrary Scheme value
;;   if kind is 'stopped   -> result is signal-name represented as a symbol
;;   if kind is 'killed    -> result is signal-name represented as a symbol
;;   if kind is 'exception -> result is a Scheme condition object
(define-record-type
  (status %make-status sh-status?)
  (fields
    (immutable kind sh-status-kind)
    (immutable results sh-status-results))
  (nongenerative #{job csm8lk7y0b1pt73ged4aijp8x-12}))


;; create an sh-status
(define (sh-status kind . results)
  (%make-status kind results))


;; return the first result stored in a sh-status,
;; or (void) if sh-status contains zero results.
(define (sh-status-result s)
  (let ((results (sh-status-results s)))
    (if (pair? results)
      (car results)
      (void))))


;; customize how "sh-status" objects are printed
(record-writer (record-type-descriptor status)
  (lambda (s port writer)
    (put-char  port #\()
    (put-datum port (sh-status-kind s))
    (do ((l (sh-status-results s) (cdr l)))
        ((null? l))
      (put-char  port #\space)
      (put-datum port (car l)))
    (put-char  port #\))))

)
