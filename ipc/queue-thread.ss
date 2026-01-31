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
(library (scheme2k ipc queue (0 9 3))
  (export make-queue-writer queue-writer? queue-writer-close queue-writer-name queue-writer-put
          make-queue-reader queue-reader? queue-reader-get queue-reader-eof?
          queue-reader-timed-get queue-reader-try-get in-queue-reader)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         condition-broadcast condition-wait include
                               make-condition make-mutex mutex-name make-time record-writer
                               time<=? time? time-difference! time-type time-second time-nanosecond
                               with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap) assert* check-interrupts raise-errorf))


(include "ipc/queue-common.ss")


;; create and return a queue-writer.
(define make-queue-writer
  (case-lambda
    (()
      (make-queue-writer #f))
    ((name)
      (%make-queue-writer (cons #f '()) (make-mutex name) (make-condition name)))))


(define (queue-writer-name p)
  (mutex-name (queue-writer-mutex p)))


;; Close specified queue-writer.
;; Notifies all attached consumers that no more data can be received.
;; Each attached queue-reader will still receive any pending data.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-close) on the same or different producers.
(define (queue-writer-close p)
  (with-mutex (queue-writer-mutex p)
    (set-cdr! (queue-writer-tail p) #f))
  (condition-broadcast (queue-writer-changed p)))


;; put a datum into the queue-writer, which will be visible to all
;; consumers attached *before* this call to (queue-writer-put).
;;
;; raises exception if queue-writer is closed
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-put) on the same or different producers.
(define (queue-writer-put p obj)
  (let ((new-tail (cons #f '())))
    (with-mutex (queue-writer-mutex p)
      (let ((old-tail (queue-writer-tail p)))
        (unless (null? (cdr old-tail))
          (raise-errorf 'queue-writer-put "~s is already closed" p))
        (set-car! old-tail obj)
        (set-cdr! old-tail new-tail)
        (queue-writer-tail-set! p new-tail))))
  (condition-broadcast (queue-writer-changed p)))


;; create and return a queue-reader that receives data put into the queue-writer.
;; multiple consumers can be attached to the same queue-writer, and each queue-reader
;; receives in order each datum put to the queue-writer *after* the queue-reader was created.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (make-queue-reader) on the same or different producers.
(define (make-queue-reader p)
  (with-mutex (queue-writer-mutex p)
    (%make-queue-reader (queue-writer-tail p) #f (queue-writer-mutex p) (queue-writer-changed p))))


(define (queue-reader-name c)
  (mutex-name (queue-reader-mutex c)))


(define short-timeout (make-time 'time-duration 500000000 0))
(define zero-timeout  (make-time 'time-duration 0 0))


(define (queue-reader-timed-get-once c timeout)
  (check-interrupts)
  (with-mutex (queue-reader-mutex c)
    (with-interrupts-disabled
      (let* ((head (queue-reader-head c))
             (tail (cdr head)))
        (cond
          ((not tail)
            (queue-reader-eof?-set! c #t)
            (values #f 'eof))
          ((null? tail)
            (unless (eqv? 0 timeout)
              ;; (condition-wait) is somewhat bugged at least on Linux:
              ;; if CTRL+C is pressed once, it does nothing.
              ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
              (condition-wait (queue-reader-changed c) (queue-reader-mutex c) timeout))
            (values #f 'timeout))
          ((pair? tail)
            (queue-reader-head-set! c tail)
            (values (car head) 'ok)))))))



;; block until a datum is received from queue-writer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if queue-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-reader-get) (queue-reader-timed-get) or (queue-reader-try-get)
;; on the same or different consumers.
(define (queue-reader-get c)
  (if (queue-reader-eof? c)
    (values #f #f)
    (let %queue-reader-get ((c c))
      (let-values (((datum flag) (queue-reader-timed-get-once c short-timeout)))
        (if (eq? flag 'timeout)
          (%queue-reader-get c)
          (values datum (eq? flag 'ok)))))))


;; block with timeout until a datum is received from queue-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if queue-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-reader-get) (queue-reader-timed-get) or (queue-reader-try-get)
;; on the same or different consumers.
(define (queue-reader-timed-get c timeout)
  (let ((timeout (make-time-duration timeout)))
    (cond
      ((queue-reader-eof? c)
        (values #f 'eof))
      ((time<=? timeout zero-timeout)
        (queue-reader-timed-get-once c 0))
      (else
        (let %queue-reader-get ((c c) (timeout timeout))
          (let ((tiny-timeout? (time<=? timeout short-timeout)))
            (let-values (((datum flag) (queue-reader-timed-get-once c
                                         (if tiny-timeout? timeout short-timeout))))
              (if (and (eq? flag 'timeout) (not tiny-timeout?))
                (%queue-reader-get c (time-difference! timeout short-timeout))
                (values datum flag)))))))))


;; non-blockingly try to receive a datum from queue-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if queue-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-reader-get) (queue-reader-timed-get) or (queue-reader-try-get)
;; on the same or different consumers.
(define (queue-reader-try-get c)
  (if (queue-reader-eof? c)
    (values #f 'eof)
    (queue-reader-timed-get-once c 0)))


;; customize how "queue-writer" objects are printed
(record-writer (record-type-descriptor queue-writer)
  (lambda (p port writer)
    (let ((name (queue-writer-name p)))
      (if name
        (begin
          (display "#<queue-writer " port)
          (display name port)
          (display ">" port))
        (display "#<queue-writer>" port)))))


;; customize how "queue-reader" objects are printed
(record-writer (record-type-descriptor queue-reader)
  (lambda (c port writer)
    (let ((name (queue-reader-name c)))
      (if name
        (begin
          (display "#<queue-reader " port)
          (display name port)
          (display ">" port))
        (display "#<queue-reader>" port)))))


) ; close library
