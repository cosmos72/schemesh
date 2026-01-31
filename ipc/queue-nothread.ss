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
(library (scheme2k ipc queue (0 9 3))
  (export make-queue-writer queue-writer? queue-writer-close queue-writer-name queue-writer-put
          make-queue-reader queue-reader? queue-reader-get queue-reader-eof? queue-reader-timed-get queue-reader-try-get in-queue-reader)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            include make-time record-writer time? time-type time-second time-nanosecond)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k posix signal) countdown))


(include "ipc/queue-common.ss")

;; create and return a queue-writer.
(define make-queue-writer
  (case-lambda
    (()
      (make-queue-writer #f))
    ((name)
      (%make-queue-writer (cons #f '()) name #f))))


(define (queue-writer-name p)
  (queue-writer-mutex p))


;; Close specified queue-writer.
;; Notifies all attached consumers that no more data can be received.
;; Each attached queue-reader will still receive any pending data.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (queue-writer-close p)
  (set-cdr! (queue-writer-tail p) #f))


;; put a datum into the queue-writer, which will be visible to all
;; consumers attached *before* this call to (queue-writer-put).
;;
;; raises exception if queue-writer is closed
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (queue-writer-put p obj)
  (let ((old-tail (queue-writer-tail p)))
    (unless (null? (cdr old-tail))
      (raise-errorf 'queue-writer-put "~s is already closed" p))
    (set-car! old-tail obj)
    (let ((new-tail (cons #f '())))
      (set-cdr! old-tail new-tail)
      (queue-writer-tail-set! p new-tail))))





;; create a queue-reader attached to specified queue-writer, and return it.
;; multiple consumers can be attached to the same queue-writer, and each queue-reader
;; receives in order all data put to the queue-writer *after* the queue-reader was created.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (make-queue-reader p)
  (%make-queue-reader (queue-writer-tail p) #f (queue-writer-mutex p) (queue-writer-changed p)))


(define (queue-reader-name c)
  (queue-reader-mutex c))

(define huge-timeout (* 86400 365))

(define (queue-reader-timed-get-once c timeout)
  (check-interrupts)
  (let* ((head (queue-reader-head c))
         (tail (cdr head)))
    (cond
      ((not tail)
        (queue-reader-eof?-set! c #t)
        (values #f 'eof))
      ((null? tail)
        (if (eqv? 0 timeout)
          (values #f 'timeout)
          (begin
            (countdown timeout)
            (queue-reader-timed-get-once c 0))))
      ((pair? tail)
        (queue-reader-head-set! c tail)
        (values (car head) 'ok)))))


;; block until a datum is received from queue-writer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if queue-writer has been closed and all data has been received.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (queue-reader-get c)
  (if (queue-reader-eof? c)
    (values #f #f)
    (let %queue-reader-get ((c c))
      (let-values (((datum flag) (queue-reader-timed-get-once c huge-timeout)))
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
;; This procedure is for non-threaded build of Chez Scheme.
(define (queue-reader-timed-get c timeout)
  (if (queue-reader-eof? c)
    (values #f 'eof)
    (queue-reader-timed-get-once c timeout)))


;; non-blockingly try to receive a datum from queue-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if queue-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is for non-threaded build of Chez Scheme.
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
