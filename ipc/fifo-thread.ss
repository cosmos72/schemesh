;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; inter-thread communication library:
;;;
;;; exchanges arbitrary objects through thread-safe FIFO
;;;
(library (schemesh ipc fifo (0 8 3))
  (export make-producer producer? producer-close producer-name producer-put
          make-consumer consumer? consumer-get consumer-eof? consumer-timed-get consumer-try-get)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         condition-broadcast condition-wait include
                               make-condition make-mutex mutex-name make-time record-writer
                               time<=? time? time-difference! time-type time-second time-nanosecond
                               with-interrupts-disabled with-mutex)
    (only (schemesh bootstrap) assert* check-interrupts raise-errorf))


(include "ipc/fifo-common.ss")


;; create and return a producer.
(define make-producer
  (case-lambda
    (()
      (make-producer #f))
    ((name)
      (%make-producer (cons #f '()) (make-mutex name) (make-condition name)))))


(define (producer-name p)
  (mutex-name (producer-mutex p)))


;; Close specified producer.
;; Notifies all attached consumers that no more data can be received.
;; Each attached consumer will still receive any pending data.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (producer-close) on the same or different producers.
(define (producer-close p)
  (with-mutex (producer-mutex p)
    (set-cdr! (producer-tail p) #f))
  (condition-broadcast (producer-changed p)))


;; put a datum into the producer, which will be visible to all
;; consumers attached *before* this call to (producer-put).
;;
;; raises exception if producer is closed
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (producer-put) on the same or different producers.
(define (producer-put p obj)
  (let ((new-tail (cons #f '())))
    (with-mutex (producer-mutex p)
      (let ((old-tail (producer-tail p)))
        (unless (null? (cdr old-tail))
          (raise-errorf 'producer-put "~s is already closed" p))
        (set-car! old-tail obj)
        (set-cdr! old-tail new-tail)
        (producer-tail-set! p new-tail))))
  (condition-broadcast (producer-changed p)))


;; create and return a consumer that receives data put into the producer.
;; multiple consumers can be attached to the same producer, and each consumer
;; receives in order each datum put to the producer *after* the consumer was created.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (make-consumer) on the same or different producers.
(define (make-consumer p)
  (with-mutex (producer-mutex p)
    (%make-consumer (producer-tail p) #f (producer-mutex p) (producer-changed p))))


(define (consumer-name c)
  (mutex-name (consumer-mutex c)))


(define short-timeout (make-time 'time-duration 500000000 0))
(define zero-timeout  (make-time 'time-duration 0 0))


(define (consumer-timed-get-once c timeout)
  (check-interrupts)
  (with-mutex (consumer-mutex c)
    (with-interrupts-disabled
      (let* ((head (consumer-head c))
             (tail (cdr head)))
        (cond
          ((not tail)
            (consumer-eof?-set! c #t)
            (values #f 'eof))
          ((null? tail)
            (unless (eqv? 0 timeout)
              ;; (condition-wait) is somewhat bugged at least on Linux:
              ;; if CTRL+C is pressed once, it does nothing.
              ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
              (condition-wait (consumer-changed c) (consumer-mutex c) timeout))
            (values #f 'timeout))
          ((pair? tail)
            (consumer-head-set! c tail)
            (values (car head) 'ok)))))))



;; block until a datum is received from producer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if producer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (consumer-get) (consumer-timed-get) or (consumer-try-get)
;; on the same or different consumers.
(define (consumer-get c)
  (if (consumer-eof? c)
    (values #f #f)
    (let %consumer-get ((c c))
      (let-values (((datum flag) (consumer-timed-get-once c short-timeout)))
        (if (eq? flag 'timeout)
          (%consumer-get c)
          (values datum (eq? flag 'ok)))))))


;; block with timeout until a datum is received from producer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if producer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (consumer-get) (consumer-timed-get) or (consumer-try-get)
;; on the same or different consumers.
(define (consumer-timed-get c timeout)
  (let ((timeout (make-time-duration timeout)))
    (cond
      ((consumer-eof? c)
        (values #f 'eof))
      ((time<=? timeout zero-timeout)
        (consumer-timed-get-once c 0))
      (else
        (let %consumer-get ((c c) (timeout timeout))
          (let ((tiny-timeout? (time<=? timeout short-timeout)))
            (let-values (((datum flag) (consumer-timed-get-once c
                                         (if tiny-timeout? timeout short-timeout))))
              (if (and (eq? flag 'timeout) (not tiny-timeout?))
                (%consumer-get c (time-difference! timeout short-timeout))
                (values datum flag)))))))))


;; non-blockingly try to receive a datum from producer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if producer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is thread safe: multiple threads can concurrently
;; call (consumer-get) (consumer-timed-get) or (consumer-try-get)
;; on the same or different consumers.
(define (consumer-try-get c)
  (if (consumer-eof? c)
    (values #f 'eof)
    (consumer-timed-get-once c 0)))


;; customize how "producer" objects are printed
(record-writer (record-type-descriptor producer)
  (lambda (p port writer)
    (let ((name (producer-name p)))
      (if name
        (begin
          (display "#<producer " port)
          (display name port)
          (display ">" port))
        (display "#<producer>" port)))))


;; customize how "consumer" objects are printed
(record-writer (record-type-descriptor consumer)
  (lambda (c port writer)
    (let ((name (consumer-name c)))
      (if name
        (begin
          (display "#<consumer " port)
          (display name port)
          (display ">" port))
        (display "#<consumer>" port)))))


) ; close library
