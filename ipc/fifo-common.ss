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
  (parent reader)
  (fields
    handle)
  (protocol
    (lambda (args->new)
      (lambda (handle)
        ((args->new %fifo-reader-get #f %fifo-reader-close) handle))))
  (nongenerative fifo-reader-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define-record-type (fifo-writer %make-fifo-writer fifo-writer?)
  (parent writer)
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
;; Both are thread-safe, fifo-reader is a subtype of writer and fifo-writer is a subtype of writer.
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
;; fifo-handle

(define (fifo-handle-pop-left h size)
  (let* ((pos   (fifo-handle-start h))
         (vec   (fifo-handle-vec   h))
         (cap   (vector-length vec))
         (datum (vector-ref    vec pos)))
    ;; help the gc
    (vector-set! vec pos #f)
    (let ((pos+1 (fx1+ pos)))
      (fifo-handle-start-set! h (if (fx>=? pos+1 cap) 0 pos+1)))
    (fifo-handle-size-set! h (fx1- size))
    (values datum 'ok)))


(define (fifo-handle-push-right h datum vec size cap)
  (let ((pos (fifo-handle-end h)))
    (vector-set! vec pos datum)
    (let ((pos+1 (fx1+ pos)))
      (fifo-handle-end-set! h (if (fx>=? pos+1 cap) 0 pos+1)))
    (fifo-handle-size-set! h (fx1+ size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fifo-reader


;; called by (reader-close)
(define (%fifo-reader-close rx)
  (fifo-handle-close (fifo-reader-handle rx)))


;; called by (reader-get)
(define (%fifo-reader-get rx)
  (fifo-handle-get (fifo-reader-handle rx)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fifo-writer


;; called by (writer-close)
(define (%fifo-writer-close tx)
  (fifo-handle-close (fifo-writer-handle tx)))


;; called by (writer-put)
(define (%fifo-writer-put tx datum)
  (fifo-handle-put tx datum))


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
