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
          make-consumer consumer? consumer-get consumer-eof? consumer-try-get)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            record-writer)
    (only (schemesh bootstrap)    raise-errorf)
    (only (schemesh posix signal) countdown))


(define-record-type (producer %make-producer producer?)
  (fields
    (mutable tail)
    mutex
    changed)
  (nongenerative producer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; create and return a producer.
(define make-producer
  (case-lambda
    (()
      (make-producer #f))
    ((name)
      (%make-producer (cons #f '()) name #f))))


(define (producer-name p)
  (producer-mutex p))


;; Close specified producer.
;; Notifies all attached consumers that no more data can be received.
;; Each attached consumer will still receive any pending data.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (producer-close p)
  (set-cdr! (producer-tail p) #f))


;; put a datum into the producer, which will be visible to all
;; consumers attached *before* this call to (producer-put).
;;
;; raises exception if producer is closed
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (producer-put p obj)
  (let ((old-tail (producer-tail p)))
    (unless (null? (cdr old-tail))
      (raise-errorf 'producer-put "~s is already closed" p))
    (set-car! old-tail obj)
    (let ((new-tail (cons #f '())))
      (set-cdr! old-tail new-tail)
      (producer-tail-set! p new-tail))))


(define-record-type (consumer %make-consumer consumer?)
  (fields
    (mutable head)
    (mutable eof?)
    mutex
    changed)
  (nongenerative consumer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; create a consumer attached to specified producer, and return it.
;; multiple consumers can be attached to the same producer, and each consumer
;; receives in order all data put to the producer *after* the consumer was created.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (make-consumer p)
  (%make-consumer (producer-tail p) #f (producer-mutex p) (producer-changed p)))


(define (consumer-name c)
  (consumer-mutex c))

(define huge-timeout (* 86400 365))

(define (consumer-timed-get-once c timeout)
  (let* ((head (consumer-head c))
         (tail (cdr head)))
    (cond
      ((not tail)
        (consumer-eof?-set! c #t)
        (values #f 'eof))
      ((null? tail)
        (unless (eqv? 0 timeout)
          (countdown timeout))
        (values #f 'timeout))
      ((pair? tail)
        (consumer-head-set! c tail)
        (values (car head) 'ok)))))


;; block until a datum is received from producer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if producer has been closed and all data has been received.
;;
;; This procedure is for non-threaded build of Chez Scheme.
(define (consumer-get c)
  (if (consumer-eof? c)
    (values #f #f)
    (let %consumer-get ((c c))
      (let-values (((datum flag) (consumer-timed-get-once c huge-timeout)))
        (if (eq? flag 'timeout)
          (%consumer-get c)
          (values datum (eq? flag 'ok)))))))


;; non-blockingly try to receive a datum from producer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if producer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is for non-threaded build of Chez Scheme.
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
