;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; inter-thread communication library:
;;;
;;; exchanges arbitrary objects through thread-safe FIFO
;;;
(library (scheme2k ipc fifo (0 9 3))
  (export make-fifo-sender fifo-sender? fifo-sender-close fifo-sender-name fifo-sender-put
          make-fifo-receiver fifo-receiver? fifo-receiver-get fifo-receiver-eof? fifo-receiver-timed-get fifo-receiver-try-get in-fifo-receiver)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            include make-time record-writer time? time-type time-second time-nanosecond)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k posix signal) countdown))


(include "ipc/fifo-common.ss")

;; create and return a fifo-sender.
(define make-fifo-sender
  (case-lambda
    (()
      (make-fifo-sender #f))
    ((name)
      (%make-fifo-sender (cons #f '()) name #f))))


(define (fifo-sender-name p)
  (fifo-sender-mutex p))


;; Close specified fifo-sender.
;; Notifies all attached consumers that no more data can be received.
;; Each attached fifo-receiver will still receive any pending data.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (fifo-sender-close p)
  (set-cdr! (fifo-sender-tail p) #f))


;; put a datum into the fifo-sender, which will be visible to all
;; consumers attached *before* this call to (fifo-sender-put).
;;
;; raises exception if fifo-sender is closed
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (fifo-sender-put p obj)
  (let ((old-tail (fifo-sender-tail p)))
    (unless (null? (cdr old-tail))
      (raise-errorf 'fifo-sender-put "~s is already closed" p))
    (set-car! old-tail obj)
    (let ((new-tail (cons #f '())))
      (set-cdr! old-tail new-tail)
      (fifo-sender-tail-set! p new-tail))))





;; create a fifo-receiver attached to specified fifo-sender, and return it.
;; multiple consumers can be attached to the same fifo-sender, and each fifo-receiver
;; receives in order all data put to the fifo-sender *after* the fifo-receiver was created.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (make-fifo-receiver p)
  (%make-fifo-receiver (fifo-sender-tail p) #f (fifo-sender-mutex p) (fifo-sender-changed p)))


(define (fifo-receiver-name c)
  (fifo-receiver-mutex c))

(define huge-timeout (* 86400 365))

(define (fifo-receiver-timed-get-once c timeout)
  (check-interrupts)
  (let* ((head (fifo-receiver-head c))
         (tail (cdr head)))
    (cond
      ((not tail)
        (fifo-receiver-eof?-set! c #t)
        (values #f 'eof))
      ((null? tail)
        (if (eqv? 0 timeout)
          (values #f 'timeout)
          (begin
            (countdown timeout)
            (fifo-receiver-timed-get-once c 0))))
      ((pair? tail)
        (fifo-receiver-head-set! c tail)
        (values (car head) 'ok)))))


;; block until a datum is received from fifo-sender, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if fifo-sender has been closed and all data has been received.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (fifo-receiver-get c)
  (if (fifo-receiver-eof? c)
    (values #f #f)
    (let %fifo-receiver-get ((c c))
      (let-values (((datum flag) (fifo-receiver-timed-get-once c huge-timeout)))
        (if (eq? flag 'timeout)
          (%fifo-receiver-get c)
          (values datum (eq? flag 'ok)))))))


;; block with timeout until a datum is received from fifo-sender, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if fifo-sender has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (fifo-receiver-timed-get c timeout)
  (if (fifo-receiver-eof? c)
    (values #f 'eof)
    (fifo-receiver-timed-get-once c timeout)))


;; non-blockingly try to receive a datum from fifo-sender, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if fifo-sender has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (fifo-receiver-try-get c)
  (if (fifo-receiver-eof? c)
    (values #f 'eof)
    (fifo-receiver-timed-get-once c 0)))


;; customize how "fifo-sender" objects are printed
(record-writer (record-type-descriptor fifo-sender)
  (lambda (p port writer)
    (let ((name (fifo-sender-name p)))
      (if name
        (begin
          (display "#<fifo-sender " port)
          (display name port)
          (display ">" port))
        (display "#<fifo-sender>" port)))))


;; customize how "fifo-receiver" objects are printed
(record-writer (record-type-descriptor fifo-receiver)
  (lambda (c port writer)
    (let ((name (fifo-receiver-name c)))
      (if name
        (begin
          (display "#<fifo-receiver " port)
          (display name port)
          (display ">" port))
        (display "#<fifo-receiver>" port)))))


) ; close library
