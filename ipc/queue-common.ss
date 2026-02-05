;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/queue-thread.ss or ipc/queue-nothread.ss


(define-record-type (queue-reader %make-queue-reader queue-reader?)
  (parent obj-reader)
  (fields
    (mutable head)
    mutex
    changed)
  (protocol
    (lambda (args->new)
      (lambda (head mutex changed)
        ((args->new %queue-reader-get #f #f) head mutex changed))))
  (nongenerative queue-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define-record-type (queue-writer %make-queue-writer queue-writer?)
  (parent obj-writer)
  (fields
    (mutable tail)
    mutex
    changed)
  (protocol
    (lambda (args->new)
      (lambda (mutex condition)
        ((args->new %queue-writer-put %queue-writer-close)
           (cons #f '()) mutex condition))))
  (nongenerative queue-writer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; convert one of:
;; * an exact or inexact real, indicating the number of seconds
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration, which is copied
;;
;; to a time object with type 'time-duration
(define (make-time-duration duration)
  (cond
    ((real? duration)
      (let* ((seconds (exact (floor duration)))
             (ns      (exact (round (* 1e9 (- duration seconds))))))
        (make-time 'time-duration ns seconds)))
    ((pair? duration)
      (make-time 'time-duration (cdr duration) (car duration)))
    (else
      (assert* 'make-time-duration (time? duration))
      (assert* 'make-time-duration (eq? 'time-duration (time-type duration)))
      (make-time 'time-duration (time-nanosecond duration) (time-second duration)))))


;; create and return a closure that iterates on data received by queue-reader rx.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values datum #t) i.e. the next datum received from queue-reader and #t,
;; or (values #<unspecified> #f) if queue-reader reached end-of-file.
;;
;; note: (in-reader rx) is equivalent and also accepts other obj-reader types
(define (in-queue-reader rx)
  (assert* 'in-queue-reader (queue-reader? rx))
  (lambda ()
    (queue-reader-get rx)))
