;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


; (define type-sym (begin '\x40;type))

(define-syntax type-sym (identifier-syntax '\x40;type))

(define (construct-time plist)
  (let ((type  (string->symbol (plist-ref plist type-sym)))
        (value (plist-ref plist 'value)))
    (let-values (((second nanosecond) (div-and-mod value 1)))
      (make-time type (exact (round (* nanosecond 1000000000)))
                      (exact second)))))


(define (construct-date plist)
  (let ((value (plist-ref plist 'value)))
    (or (and (string? value)
             (string->date value))
        (raise-errorf 'json-reader-get "invalid RFC 3339 date string: ~s" value))))


(define (add-time-info cache)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! cache rtd
      (make-record-info #f #f
        type-sym  time-type
        'value    (lambda (obj) (+ (time-second obj)
                                   (/ (time-nanosecond obj) 1000000000))))))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> constructor
  (hashtable-set! cache 'time-duration construct-time)
  (hashtable-set! cache 'time-monotonic construct-time)
  (hashtable-set! cache 'time-utc        construct-time)
  (hashtable-set! cache 'time-process     construct-time)
  (hashtable-set! cache 'time-thread       construct-time)
  (hashtable-set! cache 'time-collector-cpu construct-time)
  (hashtable-set! cache 'time-collector-real construct-time)
  cache)


(define (add-date-info cache)
  (let ((rtd (record-rtd (date 1970 1 1  0 0 0  0 0))))
    (hashtable-set! cache rtd
      (make-record-info #f #f
        type-sym  (lambda (obj) 'date)
        'value    date->string)))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> constructor
  (hashtable-set! cache 'date construct-date)
  cache)


(define record-info-table
  (add-date-info
    (add-time-info
      (make-eq-hashtable))))


(define (json-field-cursor obj rtd-cache)
  (let ((info (hashtable-ref record-info-table (record-rtd obj) #f)))
    (if info
      (ordered-hash-cursor info)
      (field-cursor obj rtd-cache))))
