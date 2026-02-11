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


;; create and return a deserializer that extracts field values from json deserialized plist
;; and passes them to specified constructor.
(define (make-deserializer constructor info)
  (let ((keys (ordered-hash-keys info)))
    (lambda (plist)
      (let %deserialize ((i (fx1- (vector-length keys)))
                         (args '()))
        (if (fx<? i 0)
          (apply constructor args)
          (%deserialize (fx1- i)
            (let ((key (vector-ref keys i)))
              (if (eq? key _type)
                args
                (cons (plist-ref plist key (void)) args)))))))))


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


;; customize how `date` objects are serialized to / deserialized from json
(define (add-date-info table)
  (let ((rtd (record-rtd (date 1970 1 1  0 0 0  0 0))))
    (hashtable-set! table rtd
      (make-record-info 'date
        (list
          'value    date->string))))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> deserializer
  (hashtable-set! table 'date deserialize-date)
  table)


;; customize how `dir-entry` objects are serialized to / deserialized from json
(define (add-dir-entry-info table)
  (let* ((rtd  (record-type-descriptor dir-entry))
         (info (make-record-info-autodetect rtd 'dir-entry)))
    (hashtable-set! table rtd info)
    ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> deserializer
    (hashtable-set! table 'dir-entry (make-deserializer make-dir-entry info)))
  table)


;; customize how `time` objects are serialized to / deserialized from json
(define (add-time-info table)
  (let ((rtd (record-rtd (make-time 'time-duration 0 0))))
    (hashtable-set! table rtd
      (make-record-info time-type
        (list
          'value    (lambda (obj) (+ (time-second obj)
                                     (/ (time-nanosecond obj) 1000000000)))))))
  ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> deserializer
  (hashtable-set! table 'time-duration deserialize-time)
  (hashtable-set! table 'time-monotonic deserialize-time)
  (hashtable-set! table 'time-utc        deserialize-time)
  (hashtable-set! table 'time-process     deserialize-time)
  (hashtable-set! table 'time-thread       deserialize-time)
  (hashtable-set! table 'time-collector-cpu deserialize-time)
  (hashtable-set! table 'time-collector-real deserialize-time)
  table)


;; hashtable rtd -> record-info and symbol -> deserializer,
;;
;; describes how to serialize/deserialize records from/to json
;; and overrides fields autodiscovery via reflection with (field-cursor)
(define json-record-infos
  (add-date-info
    (add-dir-entry-info
      (add-time-info
        (make-eq-hashtable)))))


;; add a record-info entry to json-record-infos
;; for serializing/deserializing objects with specified rtd.
;;
;; lets external code define how to serialize/deserialize custom types from/to json.
;;
;; Mandatory arguments:
;;   type-symbol - the symbolic name of the type, will be stored in json under the key "@type"
;;   deserializer - Either #f or a one-argument procedure, will be called with a plist
;;                  of deserialized key/value pairs and must create and return an object having specified rtd
;;   constructor - Ignored if deserializer is truthy.
;;                 Otherwise must be a procedure, will be called with N arguments - the deserialized fields -
;;                 and must create and return an object having specified rtd
;;   field-names-and-accessors - a plist containing alternating field-name and accessor.
;;                               It must NOT contain the key '@type, because it's added automatically.
;;                               If empty, it will be autodetected via reflection.
(define (json-record-info-set! rtd type-symbol deserializer constructor field-names-and-accessors)
  (assert* 'json-record-info-set! (record-type-descriptor? rtd))
  (assert* 'json-record-info-set! (symbol? type-symbol))
  (if deserializer
    (begin
      (assert* 'json-record-info-set! (procedure? deserializer))
      (assert* 'json-record-info-set! (logbit? 1 (procedure-arity-mask deserializer))))
    (assert* 'json-record-info-set! (procedure? constructor)))
  ;; (plist? field-names-and-accessors) is already checked by (make-record-info)
  (let ((table json-record-infos)
        (info (if (null? field-names-and-accessors)
                (make-record-info-autodetect rtd type-symbol)
                (make-record-info                type-symbol field-names-and-accessors))))
    (hashtable-set! table rtd info)
    ;; hack: put in the same eq-hashtable both rtd -> record-info and symbol -> deserializer
    (hashtable-set! table type-symbol (or deserializer (make-deserializer constructor info)))))


;; search for obj's rtd in json-record-infos and if a record-info is found, return a cursor on it.
;; otherwise return a cursor on obj's reflect fields via (field-cursor obj cache)
(define (json-record-info-cursor obj cache)
  (let ((info (hashtable-ref json-record-infos (record-rtd obj) #f)))
    (if info
      (ordered-hash-cursor info)
      (field-cursor obj cache))))
