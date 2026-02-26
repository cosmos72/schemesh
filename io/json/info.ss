;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/json/json.ss


(define-syntax _type (identifier-syntax '<type>))


;; construct a bytevector from json deserialized base64
(define (deserialize-base64 plist)
  (let ((value (plist-ref plist 'value)))
    (or (and (string? value)
             (base64-string->bytevector value))
        (raise-errorf 'json-reader-get "invalid base64 string: ~s" value))))


;; construct a `date` from json deserialized plist
(define (deserialize-date plist)
  (let ((value (plist-ref plist 'value)))
    (or (and (string? value)
             (string->date value))
        (raise-errorf 'json-reader-get "invalid RFC 3339 date string: ~s" value))))


;; construct a `time` from json deserialized plist
(define (deserialize-time plist)
  (let ((type  (string->symbol (plist-ref plist _type)))
        (value (plist-ref plist 'value)))
    (let-values (((second nanosecond) (div-and-mod value 1)))
      (make-time type (exact (truncate (* nanosecond 1000000000)))
                      (exact second)))))


;; customize how `base64` data is deserialized from json
(define (add-base64-info table)
  (hashtable-set! table 'base64 deserialize-base64)
  table)


;; customize how `date` objects are serialized to / deserialized from json
(define (add-date-info table)
  (let ((rtd (record-rtd (date 1970 1 1  +0))))
    (hashtable-set! table rtd
      (make-reflect-info 'date (list 'value date->string))))
  ;; hack: put in the same eq-hashtable both rtd -> reflect-info and symbol -> deserializer
  (hashtable-set! table 'date deserialize-date)
  table)


;; customize how `time` objects are serialized to / deserialized from json
(define (add-time-info table)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! table rtd
      (make-reflect-info time-type
        (list
          'value    (lambda (obj) (+ (time-second obj)
                                     (/ (time-nanosecond obj) 1000000000)))))))
  ;; hack: put in the same eq-hashtable both rtd -> reflect-info and symbol -> deserializer
  (hashtable-set! table 'time-duration deserialize-time)
  (hashtable-set! table 'time-monotonic deserialize-time)
  (hashtable-set! table 'time-utc        deserialize-time)
  (hashtable-set! table 'time-process     deserialize-time)
  (hashtable-set! table 'time-thread       deserialize-time)
  (hashtable-set! table 'time-collector-cpu deserialize-time)
  (hashtable-set! table 'time-collector-real deserialize-time)
  table)


;; hashtable rtd -> reflect-info and symbol -> deserializer,
;;
;; describes how to serialize/deserialize records from/to json
;; and overrides fields autodiscovery via reflection i.e. (field) (field-names) (in-fields)
(define json-reflect-infos
  (add-base64-info
    (add-date-info
      (add-time-info
        (make-eq-hashtable)))))


;; add a reflect-info entry to json-reflect-infos
;; for serializing/deserializing objects with specified rtd.
;;
;; lets external code define how to serialize/deserialize custom types from/to json.
;;
;; Mandatory arguments:
;;   type-symbol - the symbolic name of the type, will be stored in json under the key "<type>"
;;   deserializer - Either #f or a one-argument procedure, will be called with a plist
;;                  of deserialized key/value pairs and must create and return an object having specified rtd
;;   constructor - Ignored if deserializer is truthy.
;;                 Otherwise must be a procedure, will be called with N arguments - the deserialized fields -
;;                 and must create and return an object having specified rtd
;;   field-names-and-accessors - a plist containing alternating field-name and accessor.
;;                               It must NOT contain the key '<type>, because it's added automatically.
;;                               If empty, it will be autodetected via reflection.
(define (json-reflect-info-set! rtd type-symbol deserializer constructor field-names-and-accessors)
  (assert* 'json-reflect-info-set! (record-type-descriptor? rtd))
  (assert* 'json-reflect-info-set! (symbol? type-symbol))
  (if deserializer
    (begin
      (assert* 'json-reflect-info-set! (procedure? deserializer))
      (assert* 'json-reflect-info-set! (logbit? 1 (procedure-arity-mask deserializer))))
    (assert* 'json-reflect-info-set! (procedure? constructor)))
  ;; (plist? field-names-and-accessors) is already checked by (make-reflect-info)
  (let ((table json-reflect-infos)
        (info (if (null? field-names-and-accessors)
                (make-reflect-info-autodetect rtd type-symbol)
                (make-reflect-info                type-symbol field-names-and-accessors))))
    (hashtable-set! table rtd info)
    ;; hack: put in the same eq-hashtable both rtd -> reflect-info and symbol -> deserializer
    (hashtable-set! table type-symbol (or deserializer (make-reflect-deserializer constructor info)))))


;; search for obj's rtd in json-reflect-infos and if a reflect-info is found,
;;   return an iterator that generates obj's fields and values according to the reflect-info.
;; otherwise return an iterator generates obj's fields and values
;;   according to reflection i.e. (in-fields obj cache)
(define (json-in-fields obj cache)
  (let ((info (hashtable-ref json-reflect-infos (record-rtd obj) #f)))
    (if info
      (let ((seq (in-ordered-hash info)))
        (lambda ()
           (let-values (((name accessor ok?) (seq)))
             (values name (and ok? (accessor obj)) ok?))))
      (in-fields obj cache))))
