;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/fifo-thread.ss or ipc/fifo-nothread.ss


(define-record-type (fifo-sender %make-fifo-sender fifo-sender?)
  (fields
    (mutable tail)
    mutex
    changed)
  (nongenerative fifo-sender-7c46d04b-34f4-4046-b5c7-b63753c1be39))



(define-record-type (fifo-receiver %make-fifo-receiver fifo-receiver?)
  (fields
    (mutable head)
    (mutable eof?)
    mutex
    changed)
  (nongenerative fifo-receiver-7c46d04b-34f4-4046-b5c7-b63753c1be39))


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


;; create and return a closure that iterates on data recreived by fifo-receiver c.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values datum #t) i.e. the next datum received from fifo-receiver and #t,
;; or (values #<unspecified> #f) if fifo-receiver reached end-of-file.
(define (in-fifo-receiver c)
  (assert* 'in-fifo-receiver (fifo-receiver? c))
  (lambda ()
    (fifo-receiver-get c)))
