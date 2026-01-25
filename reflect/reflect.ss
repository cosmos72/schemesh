;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; Simple reflection on scheme records, hashtables, plists or vectors
;;;

(library (scheme2k reflect (0 9 3))
  (export field field-names)
  (import
    (rnrs)
    (only (chezscheme)                fx1+ fx/ void)
    (only (scheme2k containers list)  plist? plist-ref)
    (only (scheme2k containers span)  span span-insert-left/vector! span->vector))


;; find first element in vector that is eq? to key,
;; and return its position in 0 ... (fx1- (vector-length vec))
;;
;; if no element is eq? to key, return #f
(define (vector-index/eq vec key)
  (let %scan ((pos 0) (len (vector-length vec)))
    (cond
      ((fx>=? pos len)
        #f)
      ((eq? key (vector-ref vec pos))
        pos)
      (else
        (%scan (fx1+ pos) len)))))


;; find field in record obj that has name eq? to field-name, and return its value.
;; Search first in specified record-type-descriptor, then recurse to parent record-type-descriptors.
;;
;; return field's value, or default if not found.
(define (uncached-record-field obj field-name default rtd)
  (if rtd
    (let* ((field-names (record-type-field-names rtd))
           (i (and (symbol? field-name)
                   (vector-index/eq field-names field-name))))
      (if i
        ((record-accessor rtd i) obj)
        ;; field name not found in rtd => search in parent rtd
        (uncached-record-field obj field-name default (record-type-parent rtd))))
    ;; no rtd => cannot access fields
    default))


;; collect all accessors for fields in specified record-type-descriptor
;; and add them to accessors.
;;
;; also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field in a child record-type-descriptor.
(define (cache-record-accessors* accessors rtd field-names)
  (do ((i   0 (fx1+ i))
       (len (vector-length field-names)))
      ((fx>=? i len))
    (let ((field-name (vector-ref field-names i)))
      (unless (hashtable-contains? accessors field-name)
        (hashtable-set! accessors field-name (record-accessor rtd i)))))
  (let ((parent-rtd (record-type-parent rtd)))
    (if parent-rtd
      ;; also collect fields in parent record-type-descriptors
      (cache-record-accessors* accessors parent-rtd (record-type-field-names parent-rtd))
      accessors)))


;; collect all accessors for fields in specified record-type-descriptor
;; and add them to rtd-cache.
;;
;; also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field name in a child record-type-descriptor.
(define (cache-record-accessors rtd-cache rtd)
  (let* ((field-names   (record-type-field-names rtd))
         (accessors (make-eq-hashtable  (vector-length field-names))))
    (hashtable-set! rtd-cache rtd accessors)
    (cache-record-accessors* accessors rtd field-names)))


;; implementation of (field) for record types.
;; returns value of specified field name in obj, or default
(define (cached-record-field obj field-name rtd-cache default rtd)
  (if rtd
    (let* ((accessors (or (hashtable-ref rtd-cache rtd #f)
                          (cache-record-accessors rtd-cache rtd)))
           (accessor  (hashtable-ref accessors field-name #f)))
      (if accessor
        (accessor obj)
        ;; all fields of rtd and its parents are present in cached accessors
        ;; => requested field-name is not present
        default))
    ;; no rtd => cannot access fields
    default))


;; find the value of specified field name in obj.
;; obj must be a record, hashtable or plist.
;; name must be a symbol.
;;
;; return the value of specified field, or default if not found.
;; if default is not specified, it defaults to (void)
(define field
  (case-lambda
    ((obj field-name rtd-cache default)
      (cond
        ;; in Chez Scheme hashtable is a record => must checked for (hashtable?) before (record?)
        ((hashtable? obj)
          ;; FIXME: can raise condition if hashtable-hash-function or hashtable-equivalence-function
          ;; do not allow field's type and raise a condition
          (hashtable-ref obj field-name default))
        ((record? obj)
          (let ((rtd (record-rtd obj)))
            (if rtd-cache
              (cached-record-field obj field-name rtd-cache default rtd)
              (uncached-record-field obj field-name default rtd))))
        ((plist? obj)
          (plist-ref obj field-name default))
        (else
          default)))
  ((obj field-name rtd-cache)
    (field obj field-name rtd-cache (void)))
  ((obj field-name)
    (field obj field-name #f (void)))))


;; return a vector containing the field names of obj, in natural order.
;; each field name is represented as a symbol.
(define (field-names obj)
  (cond
    ;; in Chez Scheme hashtable is a record => must checked for (hashtable?) before (record?)
    ((hashtable? obj)
      (hashtable-keys obj))
    ((record? obj)
      (let %loop-record-field-names ((sp (span)) (rtd (record-rtd obj)))
        (if rtd
          (begin
            ;; insert fields from this record-type-descriptor
            (span-insert-left/vector! sp (record-type-field-names rtd))
            ;; then iterate on parent record-type-descriptor
            (%loop-record-field-names sp (record-type-parent rtd)))
          (span->vector sp))))
    ((list? obj)
      (let* ((len (length obj))
             (n   (fx/ len 2)))
        (if (even? len)
          (let %loop-plist-field-names ((i 0) (n n) (v (make-vector n)) (l obj))
            (if (fx<? i n)
              (begin
                (vector-set! v i (car l))
                (%loop-plist-field-names (fx1+ i) n v (cddr l)))
              v))
          '#())))
    (else
      '#())))


) ; close library
