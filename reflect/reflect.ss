;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; Simple reflection on scheme records, hashtables, plists and vector-like containers
;;;

(library (scheme2k reflect (0 9 3))
  (export     array?     array-accessor     array-length
          chararray? chararray-accessor chararray-length
             htable?     htable-cursor     htable-size
          field field-cursor field-cursor-next! field-names
          make-record-info record-info record-info? record-info-serializer record-info-field-names)
  (import
    (rnrs)
    (only (chezscheme)                       fx1+ fx/ logbit? procedure-arity-mask void)
    (only (scheme2k bootstrap)               assert*)
    (only (scheme2k containers charspan)     charspan? charspan-length charspan-ref)
    (only (scheme2k containers gbuffer)      gbuffer? gbuffer-length gbuffer-ref)
    (only (scheme2k containers hashtable)    hash-cursor hash-cursor-next!)
    (only (scheme2k containers list)         plist? plist-ref)
          (scheme2k containers ordered-hash)
    (only (scheme2k containers span)         span span? span-insert-left/vector! span-insert-right/vector! span-length
                                             span-ref span->vector)
    (only (scheme2k containers vector)       vector-every))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on hashtable-like containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return #t if obj is an associative container for arbitrary values.
;; currently returns #t for: hashtable, ordered-hash
(define (htable? obj)
  (or (hashtable? obj) (ordered-hash? obj)))


;; if obj is an associative container for arbitrary values,
;; return its size. otherwise return #f
(define (htable-size obj)
  (cond
    ((hashtable? obj)    (hashtable-size obj))
    ((ordered-hash? obj) (ordered-hash-size obj))
    (else           #f)))


;; if obj is an associative container for arbitrary values,
;; return two values:
;;   a cursor object,
;;   and a procedure that accepts such cursor and returns the next entry in obj as a pair (key . value),
;;     or #f if cursor reached the end of obj.
;; otherwise return (values #f #f)
(define (htable-cursor obj)
  (cond
    ((hashtable? obj)     (values (hash-cursor obj) hash-cursor-next!))
    ((ordered-hash? obj)  (values (ordered-hash-cursor obj) ordered-hash-cursor-next!))
    (else                 (values #f #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on vector-like containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return #t if obj is a random-access container for arbitrary values.
;; currently returns #t for: vector, span, gbuffer
(define (array? obj)
  (or (vector? obj) (span? obj) (gbuffer? obj)))


;; if obj is a random-access container for arbitrary values,
;; return its length. otherwise return #f
(define (array-length obj)
  (cond
    ((vector? obj)  (vector-length obj))
    ((span? obj)    (span-length obj))
    ((gbuffer? obj) (gbuffer-length obj))
    (else           #f)))


;; if obj is a random-access container for arbitrary values,
;; return a procedure that accepts two arguments: obj and a fixnum,
;;   and returns the element of obj at such index.
;; otherwise return #f
(define (array-accessor obj)
  (cond
    ((vector? obj)  vector-ref)
    ((span? obj)    span-ref)
    ((gbuffer? obj) gbuffer-ref)
    (else           #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on string-like containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return #t if obj is a random-access container for characters.
;; currently returns #t for: string, charspan
(define (chararray? obj)
  (or (string? obj) (charspan? obj)))


;; if obj is a random-access container for characters,
;; return its length. otherwise return #f
(define (chararray-length obj)
  (cond
    ((string? obj)   (string-length obj))
    ((charspan? obj) (charspan-length obj))
    (else            #f)))


;; if obj is a random-access container for characters,
;; return a procedure that accepts two arguments: obj and a fixnum,
;;   and returns the character of obj at such index.
;; otherwise return #f
(define (chararray-accessor obj)
  (cond
    ((vector? obj)   string-ref)
    ((charspan? obj) charspan-ref)
    (else           #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; caching


(define-record-type (record-info %make-record-info record-info?)
  (parent ordered-hash-type)
  (fields
    (immutable serializer   record-info-serializer)                                 ; #f or procedure
    (mutable   field-names  record-info-field-names %record-info-field-names-set!)) ; vector of symbols
  (nongenerative %record-info-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; create and return a record-info containing caller-specified serializer and fields
(define (make-record-info serializer . names-and-accessors)
  (when serializer
    ;; serializer will be called with two arguments: obj-writer and the object to serialize
    (assert* 'make-record-info (procedure? serializer))
    (assert* 'make-record-info (logbit? 2 (procedure-arity-mask serializer))))
  (assert* 'make-record-info (plist? names-and-accessors))
  (let* ((len   (fx/ (length names-and-accessors) 2))
         (names (make-vector len))
         (info  (%make-record-info (make-eq-hashtable) #f #f serializer names)))
    (do ((i 0 (fx1+ i))
         (l names-and-accessors (cddr l)))
        ((null? l) info)
      (let ((name     (car l))
            (accessor (cadr l)))
        (assert* 'make-record-info (symbol? name))
        (assert* 'make-record-info (procedure? accessor))
        (assert* 'make-record-info (logbit? 1 (procedure-arity-mask accessor)))
        (ordered-hash-set! info name accessor)))))


;; collect all accessors for fields in specified record-type-descriptor
;; and add them to info.
;; Return unspecified value.
;;
;; Also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field in a child record-type-descriptor.
(define (fill-record-info info rtd)
  (let ((parent-rtd (record-type-parent rtd)))
    (when parent-rtd
      ;; first, collect fields from parent record-type-descriptors
      (fill-record-info info parent-rtd)))
  (let ((this-field-names (record-type-field-names rtd)))
    (span-insert-right/vector! (record-info-field-names info) this-field-names)
    (do ((i   0 (fx1+ i))
         (len (vector-length this-field-names)))
        ((fx>=? i len))
      (let ((field-name (vector-ref this-field-names i)))
        ;; field-name may conflict with some eq? field name from parents rtd
        (ordered-hash-delete! info field-name)
        (ordered-hash-set!    info field-name (record-accessor rtd i))))))


;; collect all accessors for fields in specified record-type-descriptor,
;; add them to cache, and return them as a record-info
;;
;; also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field name in a child record-type-descriptor.
;;
;; finally, also collect field names from specified record-type-descriptor and its parents,
;; and add them to the returned accessors hashtable with the key (void)
(define (make-record-info/reflect cache rtd)
  (let ((info (%make-record-info (make-eq-hashtable) #f #f #f (span))))
    (fill-record-info info rtd)
    ;; convert field names span -> vector
    (%record-info-field-names-set! info (span->vector (record-info-field-names info)))
    (hashtable-set! cache rtd info)
    info))


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



;; implementation of (field) for record types.
;; returns value of specified field name in obj, or default
(define (cached-record-field obj field-name cache default rtd)
  (if rtd
    (let* ((info     (or (hashtable-ref cache rtd #f) (make-record-info/reflect cache rtd)))
           (accessor (ordered-hash-ref info field-name #f)))
      (if accessor
        (accessor obj)
        ;; all fields of rtd and its parents are present in cached info
        ;; => requested field-name is not present
        default))
    ;; no rtd => cannot access fields
    default))


(define (cached-record-field-names obj cache rtd)
 (record-info-field-names (or (hashtable-ref cache rtd #f) (make-record-info/reflect cache rtd))))


(define (uncached-record-field-names obj sp rtd)
  (if rtd
    (begin
      ;; insert fields from this record-type-descriptor *before* the subtypes field names
      (span-insert-left/vector! sp (record-type-field-names rtd))
      ;; then iterate on parent record-type-descriptor
      (uncached-record-field-names obj sp (record-type-parent rtd)))
    ;; no more parent record-type-descriptors, convert filled span to vector
    (span->vector sp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; field

;; find the value of specified field name in obj.
;; obj must be a record, hashtable, ordered-hash or plist.
;; name must be a symbol.
;;
;; return the value of specified field, or default if not found.
;; if default is not specified, it defaults to (void)
(define field
  (case-lambda
    ((obj field-name cache default)
      (cond
        ;; in Chez Scheme, hashtable is a record type
        ;; => must check for it before (record?)
        ((hashtable? obj)
          ;; FIXME: can raise condition if hashtable-hash-function or hashtable-equivalence-function
          ;; do not allow field-name's type and raise a condition
          (hashtable-ref obj field-name default))
        ((ordered-hash? obj)
          ;; FIXME: can raise condition if ordered-hash-hash-function or ordered-hash-equivalence-function
          ;; do not allow field-name's type and raise a condition
          (ordered-hash-ref obj field-name default))
        ((record? obj)
          (let ((rtd (record-rtd obj)))
            (if cache
              (cached-record-field   obj field-name cache default rtd)
              (uncached-record-field obj field-name default rtd))))
        ((plist? obj)
          (plist-ref obj field-name default))
        (else
          default)))
  ((obj field-name cache)
    (field obj field-name cache (void)))
  ((obj field-name)
    (field obj field-name #f (void)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; field-cursor


;; return a cursor that iterates on all field names of of obj, in natural order.
(define (field-cursor obj cache)
  (let ((rtd (record-rtd obj)))
    (if rtd
      (let ((info (or (hashtable-ref cache rtd #f) (make-record-info/reflect cache rtd))))
        (ordered-hash-cursor info))
      (ordered-hash-cursor-empty))))


;; return next pair (field-name . accessor) of specified field cursor,
;; or #f if cursor reached the end of fields.
(define field-cursor-next! ordered-hash-cursor-next!)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; field-names

;; return a vector containing the field names of obj, in natural order.
;; each field name is represented as a symbol.
;;
;; if obj is a hashtable, return its keys.
;;   if all the keys are symbols, they are are returned in lexicographic order.
;;
;; if obj is a ordered-hash, returns its keys in insertion order.
;;
;; if obj is a list, assume it is a plist and return its keys,
;; i.e. the 1st, 3rd, 5th ... element of the list.
;;
;; if obj is a record, returns its field names.
;;   the returned vector contains as **last** ones the field names
;;   of its record-rtd, preceded by the fields names of its parent-rtd,
;;   preceded by the fields names of its parent's parent-rtd, and so on.
;;
;; if field names cannot be retrieved, return an empty vector.
;;
;; do NOT modify the returned vector, because it may be cached in cache.
(define field-names
  (case-lambda
    ((obj cache)
      (cond
        ;; in Chez Scheme, hashtable and ordered-hash are record types
        ;; => must checked for them before (record?)
        ((hashtable? obj)
          (let ((v (hashtable-keys obj)))
            (when (vector-every symbol? v)
              (vector-sort! (lambda (sym1 sym2) (string<? (symbol->string sym1) (symbol->string sym2))) v))
            v))
        ((ordered-hash? obj)
          (ordered-hash-keys obj))
        ((record? obj)
          (let ((rtd (record-rtd obj)))
            (cond
              ((not rtd)
                '#())
              (cache
                (cached-record-field-names obj cache rtd))
              (else
                (uncached-record-field-names obj (span) rtd)))))
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
    ((obj)
      (field-names obj #f))))


) ; close library
