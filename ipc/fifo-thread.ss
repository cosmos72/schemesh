;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

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
    (only (chezscheme)         condition-broadcast condition-wait include
                               make-condition make-mutex mutex-name make-time record-writer
                               time<=? time? time-difference! time-type time-second time-nanosecond
                               with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap) assert* check-interrupts raise-errorf))


(include "ipc/fifo-common.ss")


;; create and return a fifo-sender.
(define make-fifo-sender
  (case-lambda
    (()
      (make-fifo-sender #f))
    ((name)
      (%make-fifo-sender (cons #f '()) (make-mutex name) (make-condition name)))))


(define (fifo-sender-name p)
  (mutex-name (fifo-sender-mutex p)))


;; Close specified fifo-sender.
;; Notifies all attached consumers that no more data can be received.
;; Each attached fifo-receiver will still receive any pending data.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-sender-close) on the same or different producers.
(define (fifo-sender-close p)
  (with-mutex (fifo-sender-mutex p)
    (set-cdr! (fifo-sender-tail p) #f))
  (condition-broadcast (fifo-sender-changed p)))


;; put a datum into the fifo-sender, which will be visible to all
;; consumers attached *before* this call to (fifo-sender-put).
;;
;; raises exception if fifo-sender is closed
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-sender-put) on the same or different producers.
(define (fifo-sender-put p obj)
  (let ((new-tail (cons #f '())))
    (with-mutex (fifo-sender-mutex p)
      (let ((old-tail (fifo-sender-tail p)))
        (unless (null? (cdr old-tail))
          (raise-errorf 'fifo-sender-put "~s is already closed" p))
        (set-car! old-tail obj)
        (set-cdr! old-tail new-tail)
        (fifo-sender-tail-set! p new-tail))))
  (condition-broadcast (fifo-sender-changed p)))


;; create and return a fifo-receiver that receives data put into the fifo-sender.
;; multiple consumers can be attached to the same fifo-sender, and each fifo-receiver
;; receives in order each datum put to the fifo-sender *after* the fifo-receiver was created.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (make-fifo-receiver) on the same or different producers.
(define (make-fifo-receiver p)
  (with-mutex (fifo-sender-mutex p)
    (%make-fifo-receiver (fifo-sender-tail p) #f (fifo-sender-mutex p) (fifo-sender-changed p))))


(define (fifo-receiver-name c)
  (mutex-name (fifo-receiver-mutex c)))


(define short-timeout (make-time 'time-duration 500000000 0))
(define zero-timeout  (make-time 'time-duration 0 0))


(define (fifo-receiver-timed-get-once c timeout)
  (check-interrupts)
  (with-mutex (fifo-receiver-mutex c)
    (with-interrupts-disabled
      (let* ((head (fifo-receiver-head c))
             (tail (cdr head)))
        (cond
          ((not tail)
            (fifo-receiver-eof?-set! c #t)
            (values #f 'eof))
          ((null? tail)
            (unless (eqv? 0 timeout)
              ;; (condition-wait) is somewhat bugged at least on Linux:
              ;; if CTRL+C is pressed once, it does nothing.
              ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
              (condition-wait (fifo-receiver-changed c) (fifo-receiver-mutex c) timeout))
            (values #f 'timeout))
          ((pair? tail)
            (fifo-receiver-head-set! c tail)
            (values (car head) 'ok)))))))



;; block until a datum is received from fifo-sender, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if fifo-sender has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-receiver-get) (fifo-receiver-timed-get) or (fifo-receiver-try-get)
;; on the same or different consumers.
(define (fifo-receiver-get c)
  (if (fifo-receiver-eof? c)
    (values #f #f)
    (let %fifo-receiver-get ((c c))
      (let-values (((datum flag) (fifo-receiver-timed-get-once c short-timeout)))
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
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-receiver-get) (fifo-receiver-timed-get) or (fifo-receiver-try-get)
;; on the same or different consumers.
(define (fifo-receiver-timed-get c timeout)
  (let ((timeout (make-time-duration timeout)))
    (cond
      ((fifo-receiver-eof? c)
        (values #f 'eof))
      ((time<=? timeout zero-timeout)
        (fifo-receiver-timed-get-once c 0))
      (else
        (let %fifo-receiver-get ((c c) (timeout timeout))
          (let ((tiny-timeout? (time<=? timeout short-timeout)))
            (let-values (((datum flag) (fifo-receiver-timed-get-once c
                                         (if tiny-timeout? timeout short-timeout))))
              (if (and (eq? flag 'timeout) (not tiny-timeout?))
                (%fifo-receiver-get c (time-difference! timeout short-timeout))
                (values datum flag)))))))))


;; non-blockingly try to receive a datum from fifo-sender, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if fifo-sender has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-receiver-get) (fifo-receiver-timed-get) or (fifo-receiver-try-get)
;; on the same or different consumers.
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
