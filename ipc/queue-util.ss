;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/queue-thread.ss or ipc/queue-nothread.ss


;; Create and return a thread-safe queue-reader and queue-writer, connected to each other
;; and with unlimited queue capacity.
;;
;; Each datum put to the queue-writer can be received by the queue-reader,
;; and if needed more queue-readers can be connected to the same queue-writer
;; by calling (make-queue-reader).
;;
;; Every queue-reader connected to the same queue-writer receives in order
;; each datum put to the queue-writer *after* that queue-reader was created,
;; in publish-and-subscribe style.
;;
;; A single queue-reader can also be used simultaneously from different threads, in dispatch style:
;; each datum put to the queue-writer will be received only by one thread.
(define make-queue-pair
  (case-lambda
    ((name)
      (let* ((tx (make-queue-writer name))
             (rx (make-queue-reader tx)))
        (values rx tx)))
    (()
      (make-queue-pair #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-reader


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


(define (queue-reader-close rx)
  (assert* 'queue-reader-close (queue-reader? rx))
  (obj-reader-close rx))


(define (queue-reader-eof? rx)
  (assert* 'queue-reader-eof? (queue-reader? rx))
  (obj-reader-eof? rx))


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


;; block until a datum is received from the connected queue-writer, and return one value:
;;   #t if successful, or #f if queue-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (queue-reader-get) (queue-reader-timed-get) (queue-reader-try-get) (queue-reader-skip) and (queue-reader-close)
;; on the same or different queue-readers.
(define (queue-reader-skip rx)
  (assert* 'queue-reader-skip (queue-reader? rx))
  (obj-reader-skip rx))


;; called by (queue-reader-get) and (obj-reader-get)
(define (%queue-reader-get rx)
  (let-values (((datum flag) (queue-reader-timed-get-once rx short-timeout)))
    (if (eq? flag 'timeout)
      (%queue-reader-get rx) ;; timeout, retry
      (values datum (eq? flag 'ok)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; queue-writer


;; Close specified queue-writer.
;; Notifies all attached queue-readers that no more data can be received.
;; Each attached queue-reader will still receive any pending data.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-close) and (queue-writer-put) on the same or different queue-writers.
(define (queue-writer-close tx)
  (assert* 'queue-writer-close (queue-writer? tx))
  (obj-writer-close tx))


;; Return #t if specified queue-writer is closed, otherwise return #f
(define (queue-writer-eof? tx)
  (assert* 'queue-writer-eof? (queue-writer? tx))
  (obj-writer-eof? tx))


;; put a datum into the queue-writer, which will be visible to all
;; queue-readers attached *before* this call to (queue-writer-put).
;;
;; raises exception if queue-writer is closed.
;;
;; expected to block at most for a short time, because queue-writer has unbounded capacity:
;; no need for functions (queue-writer-timed-put) and (queue-writer-try-put).
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (queue-writer-close) and (queue-writer-put) on the same or different queue-writers.
(define (queue-writer-put tx obj)
  (assert* 'queue-writer-put (queue-writer? tx))
  (obj-writer-put tx obj))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; customize how "queue-reader" objects are printed
(record-writer (record-type-descriptor queue-reader)
  (lambda (rx port writer)
    (put-string port "#<queue-reader")
    (put-string port (if (obj-reader-eof? rx) " eof" " ok"))
    (let ((name (queue-reader-name rx)))
      (when name
        (put-char port #\space)
        (display name port)))
    (put-char port #\>)))


;; customize how "queue-writer" objects are printed
(record-writer (record-type-descriptor queue-writer)
  (lambda (tx port writer)
    (put-string port "#<queue-writer")
    (put-string port (if (obj-writer-eof? tx) " eof" " ok"))
    (let ((name (queue-writer-name tx)))
      (when name
        (put-char port #\space)
        (display name port)))
    (put-char port #\>)))
