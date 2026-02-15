;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/fifo-thread.ss or ipc/fifo-nothread.ss


(define-record-type (fifo-handle %make-fifo-handle fifo-handle?)
  (fields
    vec             ; vector, used as circular buffer
    (mutable size)  ; number of elements in vec
    (mutable start) ; index of first element in vec
    (mutable end)   ; 1 + index of last element in vec
    (mutable eof?)  ; boolean, if #t no more elements can be added
    mutex           ; thread-mutex
    may-get         ; thread-condition, notified when vec becomes non-empty
    may-put)        ; thread-condition, notified when vec becomes non-full
  (nongenerative fifo-handle-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define-record-type (fifo-reader %make-fifo-reader fifo-reader?)
  (parent obj-reader)
  (fields
    handle)
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %fifo-reader-get #f %fifo-reader-close) handle))))
  (nongenerative fifo-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define-record-type (fifo-writer %make-fifo-writer fifo-writer?)
  (parent obj-writer)
  (fields
    handle)
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %fifo-writer-put %fifo-writer-close) handle))))
  (nongenerative fifo-writer-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define fifo-default-capacity
  (let ((cap 256))
    (case-lambda
      (()
        cap)
      ((capacity)
        (assert* 'fifo-default-capacity (fixnum? capacity))
        (assert* 'fifo-default-capacity (fx>? capacity 0))
        (set! cap capacity)))))


;; Create and return two values: a fifo-reader and a fifo-writer,
;; connected together and having bounded capacity.
;;
;; Caller can put arbitrary datum to the fifo-writer, which can be get back in the same order from the fifo-reader.
;;
;; both are thread-safe, fifo-reader is a subtype of obj-writer and fifo-writer is a subtype of obj-writer.
;;
;; Optional argument capacity must be a fixnum > 0. Defaults to (fifo-default-capacity)
(define make-fifo-pair
  (case-lambda
    ((capacity)
      (let ((h (make-fifo-handle capacity)))
        (values (%make-fifo-reader h)
                (%make-fifo-writer h))))
    (()
      (make-fifo-pair (fifo-default-capacity)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fifo-reader


;; Close specified fifo-reader. Also closes the connected fifo-writer.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-reader-close) and (fifo-reader-get) on the same or different fifo-readers.
(define (fifo-reader-close rx)
  (assert* 'fifo-reader-close (fifo-reader? rx))
  (obj-reader-close rx))


;; Return #t if specified fifo-reader is closed, otherwise return #f
(define (fifo-reader-eof? rx)
  (assert* 'fifo-reader-eof? (fifo-reader? rx))
  (obj-reader-eof? rx))


;; block until a datum is received from the connected fifo-writer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if fifo-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-reader-get) (fifo-reader-timed-get) (fifo-reader-try-get) (fifo-reader-skip) and (fifo-reader-close)
;; on the same or different fifo-readers.
(define (fifo-reader-get rx)
  (assert* 'fifo-reader-get (fifo-reader? rx))
  (obj-reader-get rx))


;; block until a datum is received from the connected fifo-writer, discard it, and return one value:
;;   #t if successful, or #f if fifo-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-reader-get) (fifo-reader-timed-get) (fifo-reader-try-get) (fifo-reader-skip) and (fifo-reader-close)
;; on the same or different fifo-readers.
(define (fifo-reader-skip rx)
  (assert* 'fifo-reader-skip (fifo-reader? rx))
  (obj-reader-skip rx))


;; called by (fifo-reader-close) and (obj-reader-close)
(define (%fifo-reader-close rx)
  (fifo-handle-close (fifo-reader-handle rx)))


;; called by (fifo-reader-get) and (obj-reader-get)
(define (%fifo-reader-get rx)
  (fifo-handle-get (fifo-reader-handle rx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fifo-writer


;; Close specified fifo-writer. Also closes the connected fifo-reader.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-writer-put) (fifo-writer-timed-put) (fifo-writer-try-put) and (fifo-writer-close)
;; on the same or different fifo-writers.
(define (fifo-writer-close tx)
  (assert* 'fifo-writer-close (fifo-writer? tx))
  (obj-writer-close tx))


;; Return #t if specified fifo-writer is closed, otherwise return #f
(define (fifo-writer-eof? tx)
  (assert* 'fifo-writer-eof? (fifo-writer? tx))
  (obj-writer-eof? tx))


;; put a datum to specified fifo-writer. returns unspecified value.
;; raises condition if fifo-writer is closed.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (fifo-writer-put) (fifo-writer-timed-put) (fifo-writer-try-put) and (fifo-writer-close)
;; on the same or different fifo-writers.
(define (fifo-writer-put tx datum)
  (assert* 'fifo-writer-close (fifo-writer? tx))
  (obj-writer-put tx datum))


;; called by (fifo-writer-close) and (obj-writer-close)
(define (%fifo-writer-close tx)
  (fifo-handle-close (fifo-writer-handle tx)))


;; called by (fifo-writer-put) and (obj-writer-put)
(define (%fifo-writer-put tx datum)
  (fifo-handle-put tx datum))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create and return a closure that iterates on data received by fifo-reader rx.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values datum #t) i.e. the next datum received from fifo-reader and #t,
;; or (values #<unspecified> #f) if fifo-reader reached end-of-file.
;;
;; note: (in-reader rx) is equivalent and also accepts other obj-reader types
(define (in-fifo-reader rx)
  (assert* 'in-fifo-reader (fifo-reader? rx))
  (lambda ()
    (fifo-reader-get rx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; convert one of:
;; * an exact or inexact real, indicating the number of seconds
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration, which is copied
;;
;; to a time object with type 'time-duration
(define (to-duration duration)
  (cond
    ((real? duration)
      (let* ((seconds (exact (floor duration)))
             (ns      (exact (round (* 1e9 (- duration seconds))))))
        (make-time 'time-duration ns seconds)))
    ((pair? duration)
      (make-time 'time-duration (cdr duration) (car duration)))
    (else
      (assert* 'to-duration (time? duration))
      (assert* 'to-duration (eq? 'time-duration (time-type duration)))
      (make-time 'time-duration (time-nanosecond duration) (time-second duration)))))
