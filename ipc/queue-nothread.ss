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
;;; This implementation is for non-threaded build of Chez Scheme.
;;;
(library (scheme2k ipc queue (0 9 3))
  (export make-queue-pair
          make-queue-reader queue-reader queue-reader? queue-reader-name queue-reader-close queue-reader-eof? queue-reader-get queue-reader-skip
          make-queue-writer queue-writer queue-writer? queue-writer-name queue-writer-close queue-writer-eof? queue-writer-put
          queue-reader-timed-get queue-reader-try-get in-queue-reader)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            fx1+ include make-time record-writer time? time-type time-second time-nanosecond void)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k posix signal) countdown)
    (only (scheme2k io obj)       obj-reader obj-reader? obj-reader-close obj-reader-eof? obj-reader-get obj-reader-skip
                                  obj-writer obj-writer? obj-writer-close obj-writer-eof? obj-writer-put))


(include "ipc/queue-common.ss")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-reader


(define (queue-reader-name rx)
  (queue-reader-mutex rx))


(define huge-timeout (* 86400 365))

(define (queue-reader-timed-get-once rx timeout)
  (check-interrupts)
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
        (if (eqv? 0 timeout)
          (values #f 'timeout)
          (begin
            (countdown timeout)
            (queue-reader-timed-get-once rx 0))))
      ((pair? tail)
        (queue-reader-head-set! rx tail)
        (values (car head) 'ok)))))


;; called by (queue-reader-get) and (obj-reader-get)
(define (%queue-reader-get rx)
  (let-values (((datum flag) (queue-reader-timed-get-once rx huge-timeout)))
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
;; This procedure is for non-threaded build of Chez Scheme.
(define (queue-reader-timed-get rx timeout)
  (assert* 'queue-reader-timed-get (queue-reader? rx))
  (if (queue-reader-eof? rx)
    (values #f 'eof)
    (queue-reader-timed-get-once rx timeout)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-writer


;; Create and return a queue-writer, which is a subtype of obj-writer,
;; and writes arbitrary datum to a thread-safe, in-memory unlimited queue.
;;
;; Not used often, most of the time (make-queue-pair) is a better choice.
;;
;; Optional argument must be #f or a symbol. Defaults to #f
(define make-queue-writer
  (case-lambda
    (()
      (%make-queue-writer #f #f))
    ((name)
      (%make-queue-writer name #f))))


(define (queue-writer-name tx)
  (queue-writer-mutex tx))


;; called by (queue-writer-close) and (obj-writer-close)
(define (%queue-writer-close tx)
  (set-cdr! (queue-writer-tail tx) #f))


;; called by (queue-writer-put) and (obj-writer-put)
(define (%queue-writer-put tx obj)
  (let ((old-tail (queue-writer-tail tx)))
    (unless (null? (cdr old-tail))
      (raise-errorf 'queue-writer-put "~s is already closed" tx))
    (set-car! old-tail obj)
    (let ((new-tail (cons #f '())))
      (set-cdr! old-tail new-tail)
      (queue-writer-tail-set! tx new-tail))))


(include "ipc/queue-util.ss")

) ; close library
