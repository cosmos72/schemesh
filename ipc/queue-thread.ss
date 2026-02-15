;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; inter-thread communication library:
;;;
;;; exchanges arbitrary Scheme data through thread-safe in-memory queues
;;;
(library (scheme2k ipc queue (0 9 3))
  (export make-queue-pair
          make-queue-reader queue-reader queue-reader? queue-reader-name queue-reader-close queue-reader-eof? queue-reader-get queue-reader-skip
          make-queue-writer queue-writer queue-writer? queue-writer-name queue-writer-close queue-writer-eof? queue-writer-put
          queue-reader-timed-get queue-reader-try-get in-queue-reader
          thread==> thread-queue-reader)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            condition-broadcast condition-wait fx1+ include list-copy list-head
                                  make-condition make-mutex mutex-name make-time record-writer
                                  time<=? time? time-difference! time-type time-second time-nanosecond
                                  void with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k io obj)       obj-reader obj-reader? obj-reader-close obj-reader-eof? obj-reader-get obj-reader-skip
                                  obj-writer obj-writer? obj-writer-close obj-writer-eof? obj-writer-put)
    (only (scheme2k posix thread) make-thread thread-start!))


(include "ipc/queue-common.ss")


;; Create and return a queue-writer, which is a subtype of obj-writer,
;; and writes arbitrary datum to a thread-safe, in-memory unlimited queue.
;;
;; Not used often, most of the time (make-queue-pair) is a better choice.
;;
;; Optional argument must be #f or a symbol. Defaults to #f
(define make-queue-writer
  (case-lambda
    ((name)
      (%make-queue-writer (make-mutex name) (make-condition name)))
    (()
      (%make-queue-writer (make-mutex #f) (make-condition #f)))))


(define (queue-writer-name tx)
  (mutex-name (queue-writer-mutex tx)))


;; Return #t if specified queue-writer is closed, otherwise return #f
(define (queue-writer-eof? tx)
  (assert* 'queue-writer-eof? (queue-writer? tx))
  (obj-writer-eof? tx))


;; Close specified queue-writer.
;; Notifies all attached queue-readers that no more data can be received.
;; Each attached queue-reader will still receive any pending data.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-close) and (queue-writer-put) on the same or different queue-writers.
(define (queue-writer-close tx)
  (assert* 'queue-writer-close (queue-writer? tx))
  (obj-writer-close tx))


;; called by (queue-writer-close) and (obj-writer-close)
(define (%queue-writer-close tx)
  (with-mutex (queue-writer-mutex tx)
    (set-cdr! (queue-writer-tail tx) #f))
  (condition-broadcast (queue-writer-changed tx)))


;; put a datum into the queue-writer, which will be visible to all
;; queue-readers attached *before* this call to (queue-writer-put).
;;
;; raises exception if queue-writer is closed
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-close) and (queue-writer-put) on the same or different queue-writers.
(define (queue-writer-put tx obj)
  (assert* 'queue-writer-put (queue-writer? tx))
  (obj-writer-put tx obj))


;; called by (queue-writer-put) and (obj-writer-put)
(define (%queue-writer-put tx obj)
  (let ((new-tail (cons #f '())))
    (with-mutex (queue-writer-mutex tx)
      (let ((old-tail (queue-writer-tail tx)))
        (unless (null? (cdr old-tail))
          (raise-errorf 'queue-writer-put "~s is already closed" tx))
        (set-car! old-tail obj)
        (set-cdr! old-tail new-tail)
        (queue-writer-tail-set! tx new-tail))))
  (condition-broadcast (queue-writer-changed tx)))


;; Create and return a queue-reader, which is a subtype of obj-writer.
;; Connects to specified queue-writer, and receives in order each datum
;; put to the queue-writer *after* the queue-reader was created.
;;
;; Multiple queue-readers can be attached to the same queue-writer, and each queue-reader
;; receives in order each datum put to the queue-writer *after* that queue-reader was created,
;; in publish-and-subscribe style.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (make-queue-reader) and connect to the same or different queue-writers.
;;
;; Not used often, most of the time (make-queue-pair) is a better choice.
(define (make-queue-reader tx)
  (with-mutex (queue-writer-mutex tx)
    (%make-queue-reader (queue-writer-tail tx) (queue-writer-mutex tx) (queue-writer-changed tx))))


(define (queue-reader-name rx)
  (mutex-name (queue-reader-mutex rx)))


(define (queue-reader-eof? rx)
  (assert* 'queue-reader-eof? (queue-reader? rx))
  (obj-reader-eof? rx))


(define (queue-reader-close rx)
  (assert* 'queue-reader-close (queue-reader? rx))
  (obj-reader-close rx))


(define short-timeout (make-time 'time-duration 500000000 0))
(define zero-timeout  (make-time 'time-duration 0 0))


(define (queue-reader-timed-get-once rx timeout)
  (check-interrupts)
  (with-mutex (queue-reader-mutex rx)
    (with-interrupts-disabled
      (let* ((head (queue-reader-head rx))
             (tail (cdr head)))
        (cond
          ((queue-reader-eof? rx)
            ;; this queue-reader is already closed
            (values #f 'eof))
          ((not tail)
            ;; connected queue-writer was closed, and we reached eof
            (queue-reader-close rx)
            (values #f 'eof))
          ((null? tail)
            (unless (eqv? 0 timeout)
              ;; (condition-wait) is somewhat bugged at least on Linux:
              ;; if CTRL+C is pressed once, it does nothing.
              ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
              (condition-wait (queue-reader-changed rx) (queue-reader-mutex rx) timeout))
            (values #f 'timeout))
          ((pair? tail)
            (queue-reader-head-set! rx tail)
            (values (car head) 'ok)))))))


;; block until a datum is received from the connected queue-writer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if queue-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (queue-reader-get) (queue-reader-timed-get) (queue-reader-try-get) (queue-reader-skip) and (queue-reader-close)
;; on the same or different queue-readers.
(define (queue-reader-get rx)
  (assert* 'queue-reader-get (queue-reader? rx))
  (obj-reader-get rx))


;; called by (queue-reader-get) and (obj-reader-get)
(define (%queue-reader-get rx)
  (let-values (((datum flag) (queue-reader-timed-get-once rx short-timeout)))
    (if (eq? flag 'timeout)
      (%queue-reader-get rx) ;; timeout, retry
      (values datum (eq? flag 'ok)))))


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
;; This procedure is thread safe: multiple threads can concurrently call
;; (queue-reader-get) (queue-reader-timed-get) (queue-reader-try-get) (queue-reader-skip) and (queue-reader-close)
;; on the same or different queue-readers.
(define (queue-reader-timed-get rx timeout)
  (assert* 'queue-reader-timed-get (queue-reader? rx))
  (let ((timeout (make-time-duration timeout)))
    (cond
      ((queue-reader-eof? rx)
        (values #f 'eof))
      ((time<=? timeout zero-timeout)
        (queue-reader-timed-get-once rx 0))
      (else
        (let %queue-reader-timed-get ((rx rx) (timeout timeout))
          (let ((tiny-timeout? (time<=? timeout short-timeout)))
            (let-values (((datum flag) (queue-reader-timed-get-once rx
                                         (if tiny-timeout? timeout short-timeout))))
              (if (and (eq? flag 'timeout) (not tiny-timeout?))
                (%queue-reader-timed-get rx (time-difference! timeout short-timeout))
                (values datum flag)))))))))


;; non-blockingly try to receive a datum from queue-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if queue-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (queue-reader-get) (queue-reader-timed-get) (queue-reader-try-get) (queue-reader-skip) and (queue-reader-close)
;; on the same or different queue-readers.
(define (queue-reader-try-get rx)
  (assert* 'queue-reader-try-get (queue-reader? rx))
  (if (queue-reader-eof? rx)
    (values #f 'eof)
    (queue-reader-timed-get-once rx 0)))


(include "ipc/queue-util.ss")

) ; close library
