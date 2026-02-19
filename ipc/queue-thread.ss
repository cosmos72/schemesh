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
          make-queue-reader queue-reader queue-reader? queue-reader-name queue-reader-timed-get queue-reader-try-get
          make-queue-writer queue-writer queue-writer? queue-writer-name)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            condition-broadcast condition-wait fx1+ include
                                  make-condition make-mutex mutex-name make-time record-writer
                                  time<=? time? time-difference! time-type time-second time-nanosecond
                                  void with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k io obj)       reader reader? reader-close reader-eof? reader-get reader-skip
                                  writer writer? writer-close writer-eof? writer-put))


(include "ipc/queue-common.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-reader


(define (queue-reader-name rx)
  (mutex-name (queue-reader-mutex rx)))


(define (queue-reader-timed-get-once-locked rx timeout)
  (let* ((head (queue-reader-head rx))
         (tail (cdr head)))
    (cond
      ((reader-eof? rx)
        ;; this queue-reader is already closed
        (values #f 'eof))
      ((not tail)
        ;; connected queue-writer was closed, and we reached eof
        (reader-close rx)
        (values #f 'eof))
      ((null? tail)
        (if (eqv? 0 timeout)
          (values #f 'timeout)
          (begin
            ;; (condition-wait) is somewhat bugged at least on Linux:
            ;; if CTRL+C is pressed once, it does nothing.
            ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
            (condition-wait (queue-reader-changed rx) (queue-reader-mutex rx) timeout)
            ;; try again with zero timeout
            (queue-reader-timed-get-once-locked rx 0))))
      ((pair? tail)
        (queue-reader-head-set! rx tail)
        (values (car head) 'ok)))))


(define (queue-reader-timed-get-once rx timeout)
  (check-interrupts)
  (with-mutex (queue-reader-mutex rx)
    (with-interrupts-disabled
      (queue-reader-timed-get-once-locked rx timeout))))


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
;; (reader-get) (reader-skip) (reader-close) (queue-reader-timed-get) and (queue-reader-try-get)
;; on the same or different queue-readers.
(define (queue-reader-timed-get rx timeout)
  (assert* 'queue-reader-timed-get (queue-reader? rx))
  (let ((timeout (to-duration timeout)))
    (cond
      ((reader-eof? rx)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-writer


;; Create and return a queue-writer, which is a subtype of writer,
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


;; called by (writer-close)
(define (%queue-writer-close tx)
  (with-mutex (queue-writer-mutex tx)
    (set-cdr! (queue-writer-tail tx) #f))
  (condition-broadcast (queue-writer-changed tx)))


;; called by (writer-put)
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


(include "ipc/queue-util.ss")

) ; close library
