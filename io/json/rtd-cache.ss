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

(define (deserialize-time plist)
  (let ((type  (string->symbol (plist-ref plist type-sym)))
        (value (plist-ref plist 'value)))
    (let-values (((second nanosecond) (div-and-mod value 1)))
      (make-time type (exact (round (* nanosecond 1000000000)))
                      (exact second)))))


(define (add-time-info cache)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! cache rtd
      (field-custom-info deserialize-time
        type-sym  time-type
        'value    (lambda (obj) (fl+ (inexact (time-second obj))
                                     (fl/ (inexact (time-nanosecond obj)) 1e9))))))
  cache)


(define (add-date-info cache)
  cache)


(define rtd-cache-override
  (add-date-info
    (add-time-info
      (make-eq-hashtable))))


(define (json-field-cursor obj rtd-cache)
  (let ((info (hashtable-ref rtd-cache-override (record-rtd obj) #f)))
    (if info
      (ordered-hash-cursor info)
      (field-cursor obj rtd-cache))))
