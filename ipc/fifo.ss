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
         )
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         condition-broadcast make-condition make-mutex mutex-name record-writer with-mutex)
    (only (schemesh bootstrap) assert* raise-errorf))

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



) ; close library
