;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


; (define _type (begin '\x40;type))

(define-syntax _type (identifier-syntax '\x40;type))

(define (construct-time plist)
  (let ((type  (string->symbol (plist-ref plist _type)))
        (value (plist-ref plist 'value)))
    (let-values (((second nanosecond) (div-and-mod value 1)))
      (make-time type (exact (truncate (* nanosecond 1000000000)))
                      (exact second)))))


(define (construct-date plist)
  (let ((value (plist-ref plist 'value)))
    (or (and (string? value)
             (string->date value))
        (raise-errorf 'json-reader-get "invalid RFC 3339 date string: ~s" value))))


(define (add-time-info table)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! table rtd
      (make-record-info
        (list
          _type  time-type
          'value    (lambda (obj) (+ (time-second obj)
                                     (/ (time-nanosecond obj) 1000000000)))))))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> constructor
  (hashtable-set! table 'time-duration construct-time)
  (hashtable-set! table 'time-monotonic construct-time)
  (hashtable-set! table 'time-utc        construct-time)
  (hashtable-set! table 'time-process     construct-time)
  (hashtable-set! table 'time-thread       construct-time)
  (hashtable-set! table 'time-collector-cpu construct-time)
  (hashtable-set! table 'time-collector-real construct-time)
  table)


(define (add-date-info table)
  (let ((rtd (record-rtd (date 1970 1 1  0 0 0  0 0))))
    (hashtable-set! table rtd
      (make-record-info
        (list
          _type  (lambda (obj) 'date)
          'value    date->string))))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> constructor
  (hashtable-set! table 'date construct-date)
  table)


;; hashtable rtd -> record-info and symbol -> constructor,
;;
;; describes how to serialize/deserialize records from/to json
;; and overrides fields autodiscovery via reflection with (field-cursor)
(define json-record-table
  (add-date-info
    (add-time-info
      (make-eq-hashtable))))


;; add a record-info entry to json-record-table
;; for serializing/deserializing objects with specified rtd.
;;
;; lets external code define how to serialize/deserialize custom types from/to json.
;;
;; Mandatory arguments:
;;   type-symbol - the symbolic name of the type, will be stored in json under the key "@type"
;;   constructor - a one-argument procedure, will be called with a plist of deserialized key/value pairs
;;                 and must create and return an object having specified rtd
;;   field-names-and-accessors - a plist containing alternating field-name and accessor.
;;                               It must NOT contain the key '@type, because it's added automatically.
;;                               If empty, it will be autodetected via reflection.
(define (json-record-info-set! rtd type-symbol constructor field-names-and-accessors)
  (assert* 'json-record-info-set! (record-type-descriptor? rtd))
  (assert* 'json-record-info-set! (symbol? type-symbol))
  (assert* 'json-record-info-set! (procedure? constructor))
  (assert* 'json-record-info-set! (logbit? 1 (procedure-arity-mask constructor)))
  ;; (plist? field-names-and-accessors) is already checked by (make-record-info)
  (let ((table json-record-table)
        (info (if (null? field-names-and-accessors)
                (make-record-info/autodetect rtd type-symbol)
                (make-record-info
                  (cons _type (cons (lambda (obj) type-symbol) field-names-and-accessors))))))
    (hashtable-set! table rtd info)
    ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> constructor
    (hashtable-set! table type-symbol constructor)))


(define (make-record-info/autodetect rtd type-symbol)
  (let ((info (make-record-info '())))
    ;; _type -> (lambda (obj) type-symbol) must be inserted as first
    (ordered-hash-set! info _type (lambda (obj) type-symbol))
    ;; followed by field names and accessors detected via reflection
    (record-info-fill! info rtd)
    info))



;; search for obj's rtd in json-record-table and if a record-info is found, return a cursor on it.
;; otherwise return a cursor on obj's reflect fields via (field-cursor obj rtd-cache)
(define (record-json-field-cursor obj rtd-cache)
  (let ((info (hashtable-ref json-record-table (record-rtd obj) #f)))
    (if info
      (ordered-hash-cursor info)
      (field-cursor obj rtd-cache))))
