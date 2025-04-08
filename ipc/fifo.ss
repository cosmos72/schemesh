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
    (only (chezscheme)         condition-broadcast condition-wait
                               make-condition make-mutex mutex-name make-time
                               record-writer with-interrupts-disabled with-mutex)
    (only (schemesh bootstrap) check-interrupts raise-errorf))


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
      (%make-producer (cons #f '()) (make-mutex name) (make-condition name)))))


(define (producer-name p)
  (mutex-name (producer-mutex p)))


(define (producer-close p)
  (with-mutex (producer-mutex p)
    (set-cdr! (producer-tail p) #f))
  (condition-broadcast (producer-changed p)))


;; raises exception if producer is closed
(define (producer-put p obj)
  (let ((new-tail (cons obj '())))
    (with-mutex (producer-mutex p)
      (let ((old-tail (producer-tail p)))
        (unless (null? (cdr old-tail))
          (raise-errorf 'producer-put "~s is already closed" p))
        (set-cdr! old-tail new-tail)
        (producer-tail-set! p new-tail))))
  (condition-broadcast (producer-changed p)))


(define-record-type (consumer %make-consumer consumer?)
  (fields
    (mutable head)
    (mutable eof?)
    mutex
    changed)
  (nongenerative consumer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; create and return a consumer that receives data put into the producer.
;; multiple consumers can be attached to the same producer, and each consumer
;; receives in order each datum put to the producer *after* the consumer was created.
(define (make-consumer p)
  (%make-consumer (producer-tail p) #f (producer-mutex p) (producer-changed p)))


(define (consumer-name c)
  (mutex-name (consumer-mutex c)))


(define default-timeout (make-time 'time-duration 500000000 0))


(define (consumer-timed-get-once c timeout)
  (check-interrupts)
  (with-interrupts-disabled
    (with-mutex (consumer-mutex c)
      (let ((obj (cdr (consumer-head c))))
        (cond
          ((not obj)
            (consumer-eof?-set! c #t)
            (values #f 'eof))
          ((null? obj)
            (cond
              ((eqv? 0 timeout)
                (values #f 'timeout))
              (else
                (condition-wait (consumer-changed c) (consumer-mutex c) timeout)
                (values #f 'timeout))))
          ((pair? obj)
            (consumer-head-set! c obj)
            (values (car obj) 'ok)))))))



;; block until a datum is received from producer, and return two values:
;;   datum and #t
;;   or <unspecified> and #f if producer has been closed and all data has been received.
(define (consumer-get c)
  (if (consumer-eof? c)
    (values #f #f)
    (let %consumer-get ((c c))
      (let-values (((datum flag) (consumer-timed-get-once c default-timeout)))
        (if (eq? flag 'timeout)
          (%consumer-get c)
          (values datum (eq? flag 'ok)))))))


;; non-blockingly try to receive a datum from producer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if producer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
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
