;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


(define (add-time-rtd-cache cache)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! cache rtd
      (eq-hashtable
        (void)       '#(\x40;type value)
        '\x40;type   time-type
        'value       (lambda (obj) (fl+ (inexact (time-second obj)) (fl/ (time-nanosecond obj) 1e9)))))))


(define (add-date-rtd-cache cache)
  (void))


(define (make-initial-rtd-cache)
  (let ((cache (make-eq-hashtable 16)))
    (add-time-rtd-cache cache)
    (add-date-rtd-cache cache)))


(define default-rtd-cache (make-initial-rtd-cache))


(define (json-field-names obj rtd-cache)
  (let ((default-accessors (hashtable-ref default-rtd-cache (record-rtd obj) #f)))
    (if default-accessors
      (hashtable-ref default-accessors (void) '#())
      (field-names obj rtd-cache))))
