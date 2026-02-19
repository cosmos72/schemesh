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
  (export array? array-accessor array-length chararray? chararray-accessor chararray-length htable? htable-size in-htable
          compare compare-type-and-value equiv? greater-equiv? greater? less? less-equiv? unordered? reflect-compare-functions-set!
          field field-names fields->plist in-fields
          make-reflect-info make-reflect-info-autodetect make-reflect-deserializer
          reflect-info reflect-info? reflect-info-deserializer reflect-info-fill! reflect-info-set! reflect-info-set-autodetect! reflect-infos)
  (import
    (rnrs)
    (only (chezscheme)                       date? date-year date-month date-day date-hour date-minute date-second date-nanosecond date-zone-offset
                                             fx1+ fx1- fx/ list-copy logbit? make-date make-time procedure-arity-mask reverse!
                                             time? time-type time-second time-nanosecond void)
    (only (scheme2k bootstrap)               assert* for let-macro)
    (only (scheme2k containers charspan)     charspan? charspan-length charspan-ref)
    (only (scheme2k containers date)         date date-compare date-equiv?)
    (only (scheme2k containers gbuffer)      gbuffer? gbuffer-length gbuffer-ref)
    (only (scheme2k containers hashtable)    eq-hashtable hash-cursor hash-cursor-next! in-hash)
    (only (scheme2k containers list)         plist? plist-ref in-plist)
          (scheme2k containers ordered-hash)
    (only (scheme2k containers time)         make-time-utc time-compare time-equiv?)
    (only (scheme2k containers span)         span span? span-insert-left! span-insert-left/vector! span-insert-right/vector!
                                             span-length span-ref span->vector)
    (only (scheme2k containers vector)       vector-every))


;; an exhausted iterator
(define (empty-iterator)
  (values #f #f #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compare value-like objects: booleans, characters, numbers, strings, symbols, times, dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define reflect-compare-table
  (eq-hashtable (record-rtd (date 1970 1 1 0))   (vector date-compare date-equiv? 0)
                (record-rtd (make-time-utc 0 0)) (vector time-compare time-equiv? 1)))


;; retrieve the function for comparing with (compare) records having specified rtd
(define (reflect-compare-function rtd)
  (vector-ref (hashtable-ref reflect-compare-table rtd '#(#f #f #f))
              0))


;; retrieve the function for comparing with (equiv?) records having specified rtd
(define (reflect-equiv-function rtd)
  (vector-ref (hashtable-ref reflect-compare-table rtd '#(#f #f #f))
              1))


;; set the functions for comparing with (compare) and (equiv?) records having specified rtd
(define reflect-compare-functions-set!
  (case-lambda
    ((rtd compare-proc equiv-proc)
      (assert* 'reflect-compare-function (record-type-descriptor? rtd))
      (assert* 'reflect-compare-function (procedure? compare-proc))
      (assert* 'reflect-compare-function (logbit? 2 (procedure-arity-mask compare-proc)))
      (assert* 'reflect-compare-function (procedure? equiv-proc))
      (assert* 'reflect-compare-function (logbit? 2 (procedure-arity-mask equiv-proc)))
      (let ((ht reflect-compare-table))
        (hashtable-set! ht rtd (vector compare-proc equiv-proc (hashtable-size ht)))))
    ((rtd compare-proc)
      (reflect-compare-functions-set! rtd compare-proc
        (let ((%reflect-equiv-proc ;; name shown when displaying the closure
                (lambda (a b) (eqv? 0 (compare-proc a b)))))
            %reflect-equiv-proc)))))


;; convention: #f is less than #t
(define (boolean-compare a b)
  (cond ((eq? a b) 0)
        (a         1)
        (else     -1)))


(define (char-compare a b)
  (cond ((char=? a b) 0)
        ((char>? a b) 1)
        (else         -1)))


(define (number-compare a b)
  (cond
    ((or (eq? a b) (= a b))
      0)
    ((and (real? a) (real? b))
      (cond ((> a b) 1)
            ((< a b) -1)
            ;; (= a b) already checked above
            (else #f))) ;; not-a-numbers are unordered
    (else
      #f))) ;; complex numbers are unordered


(define (string-compare a b)
  (cond ((string=? a b) 0)
        ((string>? a b) 1)
        (else           -1)))


(define (symbol-compare a b)
  ;; compare symbol names
  (string-compare (symbol->string a) (symbol->string b)))


(define (symbol-equiv? a b)
  ;; compare symbol names
  (string=? (symbol->string a) (symbol->string b)))


(define (record-compare a b)
  (let* ((rtd (record-rtd a))
         (cmp (reflect-compare-function rtd)))
    (and cmp
         (eq? rtd (record-rtd b))
         (cmp a b))))


(define (record-equiv? a b)
  (let* ((rtd   (record-rtd a))
         (equiv (reflect-equiv-function rtd)))
    (and equiv
         (eq? rtd (record-rtd b))
         (equiv a b))))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return -1 if a is smaller than b
;;   return 0  if a and b are equivalent
;;   return 1  if a is greater than b
;;   return #f if they are not ordered
;;             (examples: times with different time-type, complex numbers, not-a-number)
;;
;; should never raise condition
(define (compare a b)
  (cond
    ((eq? a b)      0) ; also catches (void) and (eof-object)
    ((boolean? a)   (and (boolean? b) (boolean-compare a b)))
    ((char?   a)    (and (char?   b)  (char-compare    a b)))
    ((number? a)    (and (number? b)  (number-compare  a b)))
    ((symbol? a)    (and (symbol? b)  (symbol-compare  a b)))
    ((string? a)    (and (string? b)  (string-compare  a b)))
    ((record? a)    (and (record? b)  (record-compare  a b)))
    (else #f)))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return #t if they are equivalent
;;   return #f if they are different or not comparable
;;
;; same semantic as (eqv? 0 (compare a b)), but often faster
;; should never raise condition
(define (equiv? a b)
  (cond
    ((eq? a b)      #t) ; also catches (void) (eof-object) and equal booleans
    ((boolean? a)   #f)
    ((char? a)      (and (char? b)    (char=? a b)))
    ((number? a)    (and (number? b)  (= a b)))
    ((symbol? a)    (and (symbol? b)  (symbol-equiv? a b)))
    ((string? a)    (and (string? b)  (string=? a b)))
    ((record? a)    (and (record? b)  (record-equiv? a b)))
    (else #f)))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return #t if a is less than b
;;   return #f in all other cases: a is equivalent to or greater than b,
;;                                 or they are not ordered
;;
;; same semantic as (eqv? -1 (compare a b))
;; should never raise condition
(define (less? a b)
  (eqv? -1 (compare a b)))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return #t if a is less than or equivalent to b
;;   return #f in all other cases: a is greater than b,
;;                                 or they are not ordered
;;
;; same semantic as (memv (compare a b) '(-1 0))
;; should never raise condition
(define (less-equiv? a b)
  (and (memv (compare a b) '(-1 0)) #t))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return #t if a is greater than or equivalent to b
;;   return #f in all other cases: a is less than b,
;;                                 or they are not ordered
;;
;; same semantic as (memv (compare a b) '(0 1))
;; should never raise condition
(define (greater-equiv? a b)
  (and (memv (compare a b) '(0 1)) #t))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #f.
;; otherwise compare them according to comparison for their type, and
;;   return #t if a is greater than b
;;   return #f in all other cases: a is less than or equivalent to b,
;;                                 or they are not ordered
;;
;; same semantic as (eqv? 1 (compare a b))
;; should never raise condition
(define (greater? a b)
  (eqv? 1 (compare a b)))


;; compare two arbitrary datum a and b.
;;
;; if a and b have different type, return #t.
;; otherwise compare them according to comparison for their type, and
;;   return #t if they are or not ordered
;;   return #f in all other cases: a is less than, or equivalent to, or greater than b
;;
;; same semantic as (not (compare a b)) i.e. (eq? #f (compare a b))
;; should never raise condition
(define (unordered? a b)
  (not (compare a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compare types of objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; retrieve the type-id of specified rtd, if available.
;; used to order data having different types
(define (record-type-id rtd)
  (vector-ref (hashtable-ref reflect-compare-table rtd '#(#f #f #f)) 2))


;; retrieve the type-id of specified object, if available.
;; used to order data having different types
(define (reflect-type-id a)
  (cond
    ((boolean? a)    -5)
    ((char?   a)     -4)
    ((number? a)     -3)
    ((symbol? a)     -2)
    ((string? a)     -1)
    ((record? a)     (record-type-id (record-rtd a)))
    ((eq? a (void))  (fx1- (greatest-fixnum)))
    ((eof-object? a) (greatest-fixnum))
    (else            #f)))


;; compare the type of two arbitrary datum a and b.
;;
;; return 0  if a and b have same type.
;; return -1 if a's type is smaller than b's type
;; return 1  if a's type is greater than b's type
;; return #f if a's type and b's type are not ordered
;;
;; should never raise condition
(define (compare-type-of a b)
  (let ((ta (reflect-type-id a))
        (tb (reflect-type-id b)))
    (cond
      ((not ta)     (if tb 1 #f))
      ((not tb)     -1)
      ((fx<? ta tb) -1)
      ((fx>? ta tb)  1)
      (else          0))))


;; compare the type, then the value of two arbitrary datum a and b.
;;
;; return 0  if a and b have same type and equivalent values.
;; return -1 if a's type is smaller than b's type, or they have the same type and a's value is smaller than b's value
;; return 1  if a's type is greater than b's type, or they have the same type and a's value is greater than b's value
;; return #f if a's type and b's type are not ordered, or they have the same type and a's value and b's type are not ordered
;;
;; should never raise condition
(define (compare-type-and-value a b)
  ;; shortcut: compare values first.
  ;; if a and b have different types, their values are always unordered.
  (or (compare a b)
      (compare-type-of a b)))


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
    ((string? obj)   string-ref)
    ((charspan? obj) charspan-ref)
    (else            #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on hashtable-like containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return #t if obj is an associative container for arbitrary values.
;; currently returns #t for: hashtable, ordered-hash
(define (htable? obj)
  (or (hashtable? obj) (ordered-hash? obj)))


;; if obj is an associative container for arbitrary values,
;; return its size. otherwise return #f
;;
;; supported associative containers are:
;;   hashtable
;;   ordered-hash
(define (htable-size obj)
  (cond
    ((hashtable? obj)    (hashtable-size obj))
    ((ordered-hash? obj) (ordered-hash-size obj))
    (else           #f)))


;; if obj is an associative container for arbitrary values, return a iterator on it.
;; supported associative containers are:
;;   hashtable
;;   ordered-hash
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in associative container and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of associative container is reached.
(define (in-htable obj)
  (cond
    ((hashtable? obj)     (in-hash obj))
    ((ordered-hash? obj)  (in-ordered-hash obj))
    (else                 empty-iterator)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflection on records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; reflect-info


(define-syntax _type (identifier-syntax '<type>))


(define-record-type (reflect-info %make-reflect-info reflect-info?)
  (parent ordered-hash-type)
  (fields
    (mutable   field-names  reflect-info-field-names %reflect-info-field-names-set!)) ; vector of symbols
  (nongenerative %reflect-info-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; insert '<type> -> type-symbol-or-proc into specified reflect-info
(define (reflect-info-insert-type! info type-symbol-or-proc)
  (ordered-hash-set! info _type
    (cond
      ((symbol? type-symbol-or-proc)
        (let ((%reflect-type-proc ;; name shown when displaying the closure
                (lambda (obj) type-symbol-or-proc)))
          %reflect-type-proc))
      (else
        (assert* 'make-reflect-info (procedure? type-symbol-or-proc))
        (assert* 'make-reflect-info (logbit? 1 (procedure-arity-mask type-symbol-or-proc)))
        type-symbol-or-proc))))


;; create and return a reflect-info containing caller-specified type-symbol, field names and their accessors.
;; names-and-accessors must be a plist alternating field-name and accessor.
(define (make-reflect-info type-symbol-or-proc names-and-accessors)
  (assert* 'make-reflect-info (plist? names-and-accessors))
  (let* ((len   (fx/ (length names-and-accessors) 2))
         (names (make-vector (fx1+ len)))
         (info  (%make-reflect-info (make-eq-hashtable) #f #f names)))
    ;; insert '<type> -> type-symbol-or-proc as first entry in ordered-hash
    (reflect-info-insert-type! info type-symbol-or-proc)
    ;; and insert '<type> as first field name
    (vector-set! names 0 _type)
    ;; followed by field names and accessors
    (do ((i 1 (fx1+ i))
         (l names-and-accessors (cddr l)))
        ((null? l) info)
      (let ((name     (car l))
            (accessor (cadr l)))
        (assert* 'make-reflect-info (symbol? name))
        (assert* 'make-reflect-info (procedure? accessor))
        (assert* 'make-reflect-info (logbit? 1 (procedure-arity-mask accessor)))
        ;; insert name into field names
        (vector-set! names i name)
        ;; insert name -> accessor into info's ordered-hash
        (ordered-hash-set! info name accessor)))))


;; create and return a reflect-info describing the fields of objects with specified rtd.
;; uses reflection to obtain field names and accessors.
(define make-reflect-info-autodetect
  (case-lambda
    ((rtd type-symbol-or-proc)
      (let ((info (%make-reflect-info (make-eq-hashtable) #f #f #f)))
        ;; insert '<type> -> type-symbol-or-proc as first entry in ordered-hash
        (reflect-info-insert-type! info type-symbol-or-proc)
        ;; followed by field names and accessors detected via reflection
        (reflect-info-fill! info rtd)
        info))
    ((rtd)
      (make-reflect-info-autodetect rtd (record-type-name rtd)))))


;; recursive implementation of (reflect-info-fill!)
(define (%reflect-info-fill! info rtd)
  (let ((parent-rtd (record-type-parent rtd)))
    (when parent-rtd
      ;; first, collect fields from parent record-type-descriptors
      (%reflect-info-fill! info parent-rtd)))
  (let ((this-field-names (record-type-field-names rtd)))
    (do ((i   0 (fx1+ i))
         (len (vector-length this-field-names)))
        ((fx>=? i len))
      (let ((field-name (vector-ref this-field-names i)))
        ;; field-name may conflict with some eq? field name from parents rtd
        ;; remove any existing field-name from its position
        (ordered-hash-delete! info field-name)
        ;; and insert the field-name at the end: ordered-hash remembers insertion order
        (ordered-hash-set!    info field-name (record-accessor rtd i))))))


;; collect via reflection accessors for all fields in specified record-type-descriptor,
;; and add them to info.
;; Return unspecified value.
;;
;; Also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field in a child record-type-descriptor.
(define (reflect-info-fill! info rtd)
  (%reflect-info-fill! info rtd)
  ;; create and store vector of field-names
  (%reflect-info-field-names-set! info (ordered-hash-keys info)))


;; collect all accessors for fields in specified record-type-descriptor,
;; add them to cache, and return them as a reflect-info
;;
;; also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field name in a child record-type-descriptor.
;;
;; finally, also collect field names from specified record-type-descriptor and its parents,
;; and add them to the returned accessors hashtable with the key (void)
(define (make-reflect-info/reflect cache-or-false rtd)
  (let ((info (make-reflect-info-autodetect rtd)))
    (when cache-or-false
      (hashtable-set! cache-or-false rtd info))
    info))


;; create and return a deserializer that scans a plist,
;; extracts the field values corresponding to fields contained in info,
;; and passes them to specified constructor, which is supposed to create and return an object
(define (make-reflect-deserializer constructor info)
  (let* ((keys (ordered-hash-keys info))
         (%reflect-deserialize ;; name name shown when displaying the closure
           (lambda (plist)
             (let %reflect-deserialize-loop ((i (fx1- (vector-length keys)))
                                             (args '()))
               (if (fx<? i 0)
                 (apply constructor args)
                 (%reflect-deserialize-loop
                   (fx1- i)
                   (let ((key (vector-ref keys i)))
                     (if (eq? key _type)
                       args
                       (cons (plist-ref plist key (void)) args)))))))))
    %reflect-deserialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reflect-infos


;; customize visible fields and deserializer for `date` objects
(define (add-date-info table)
  (let* ((rtd  (record-rtd (date 1970 1 1 +0)))
         (info (make-reflect-info 'date
                 (list 'year date-year 'month date-month   'day date-day
                       'hour date-hour 'minute date-minute 'second date-second
                       'nanosecond date-nanosecond 'zone-offset date-zone-offset))))
    (hashtable-set! table rtd info)
    (hashtable-set! table 'date
      (make-reflect-deserializer make-date info)))
  table)


;; construct a `time` objects from a plist
(define (deserialize-time plist)
  (let ((type (plist-ref plist _type)))
    (make-time (if (symbol? type) type (string->symbol type))
               (plist-ref plist 'nanosecond)
               (plist-ref plist 'second))))


;; customize visible fields and deserializer for `time` objects
(define (add-time-info table)
  (let* ((rtd  (record-rtd (make-time 'time-duration 0 0)))
         (info (make-reflect-info time-type ;; lambda obj -> type-symbol
                 ;; customize visible fields
                 (list 'second time-second 'nanosecond time-nanosecond))))
    (hashtable-set! table rtd info))
  (do ((l '(time-duration time-monotonic time-utc time-process time-thread time-collector-cpu time-collector-real)
           (cdr l)))
      ((null? l) table)
    (hashtable-set! table (car l) deserialize-time)))


;; global table reflect-infos, contains user-provided reflect-info and deserializer
;; for customizing the visible fields and deserializer of objects
(define reflect-infos
  (add-date-info
    (add-time-info
      (make-eq-hashtable))))


;; customize visible fields and deserializer of objects having specified rtd:
;; stores info and user-specified deserializer into global table reflect-infos.
(define reflect-info-set!
  (case-lambda
    ((rtd info type-symbol deserializer)
      (assert* 'reflect-info-set! (record-type-descriptor? rtd))
      (assert* 'reflect-info-set! (reflect-info? info))
      (when (or type-symbol deserializer)
        (assert* 'reflect-info-set! (symbol? type-symbol))
        (assert* 'reflect-info-set! (procedure? deserializer))
        (assert* 'reflect-info-set! (logbit? 1 (procedure-arity-mask deserializer)))
        ;; put in reflect-infos both rtd -> info and type-symbol -> deserializer
        (hashtable-set! reflect-infos type-symbol deserializer))
      (hashtable-set! reflect-infos rtd info))

    ((rtd info)
      (reflect-info-set! rtd info #f #f))))


;; return user-added deserializer for specified type-symbol, or #f if not found
(define (reflect-info-deserializer type-symbol)
   (assert* 'reflect-info-deserializer (symbol? type-symbol))
   (hashtable-ref reflect-infos type-symbol #f))


;; customize visible fields and deserializer of objects having specified rtd:
;; autodetect via reflection info and record-type-name from rtd,
;; create a deserializer from info and caller-specified constructor,
;; and store all of them into global table reflect-infos.
(define (reflect-info-set-autodetect! rtd constructor)
  (let ((info (make-reflect-info-autodetect rtd)))
    (reflect-info-set! rtd info (record-type-name rtd)
      (make-reflect-deserializer constructor info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(define (uncached-reflect-field obj field-name default rtd)
  (cond
    ((not rtd) ;; no rtd => cannot access fields
      default)
    ((eq? field-name _type) ;; synthetic field '<type> always has value = record type name
      (record-type-name rtd))
    (else
      (let* ((field-names (record-type-field-names rtd))
             (i (and (symbol? field-name)
                     (vector-index/eq field-names field-name))))
        (if i
          ((record-accessor rtd i) obj)
          ;; field name not found in rtd => search in parent rtd
          (uncached-reflect-field obj field-name default (record-type-parent rtd)))))))


;; implementation of (field) for record types.
;; returns value of specified field name in obj, or default
(define (cached-reflect-field obj field-name cache default rtd)
  (cond
    ((not rtd) ;; no rtd => cannot access fields
      default)
    ((eq? field-name _type) ;; synthetic field '<type> always has value = record type name
      (record-type-name rtd))
    (else
      (let* ((info     (or (hashtable-ref cache rtd #f) (make-reflect-info/reflect cache rtd)))
             (accessor (ordered-hash-ref info field-name #f)))
        (if accessor
          (accessor obj)
          ;; all fields of rtd and its parents are present in cached info
          ;; => requested field-name is not present
          default)))))


(define (cached-reflect-field-names obj cache rtd)
 (reflect-info-field-names (or (hashtable-ref cache rtd #f) (make-reflect-info/reflect cache rtd))))


(define (%uncached-reflect-field-names obj sp rtd)
  (when rtd
    ;; insert fields from this record-type-descriptor *before* the subtypes field names
    (span-insert-left/vector! sp (record-type-field-names rtd))
    ;; then iterate on parent record-type-descriptor
    (%uncached-reflect-field-names obj sp (record-type-parent rtd))))


(define (uncached-reflect-field-names obj sp rtd)
  (%uncached-reflect-field-names obj sp rtd)
  ;; insert '<type> as synthetic first field name
  (span-insert-left! sp _type)
  (span->vector sp))


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
        ((or (null? obj) (pair? obj))
          (plist-ref obj field-name default))
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
          (let* ((rtd   (record-rtd obj))
                 (xinfo (and rtd (hashtable-ref reflect-infos rtd #f))))
            (cond
              (xinfo ; found override in reflect-infos, use it
                (let ((accessor (ordered-hash-ref xinfo field-name #f)))
                  (if accessor
                    (accessor obj)
                    default)))
              (cache ; search in cache, autogenerating and storing a reflect-info for future calls if not present
                (cached-reflect-field obj field-name cache default rtd))
              (else
                (uncached-reflect-field obj field-name default rtd)))))
        (else
          default)))
  ((obj field-name cache)
    (field obj field-name cache (void)))
  ((obj field-name)
    (field obj field-name #f (void)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; in-fields


;; return an iterator that generates all field names and values of obj, in natural order.
;;
;; The returned closure accepts no arguments, and each call to it returns three values:
;;   either (values field-name field-value #t)
;;   or (values #<unspecified> #<unspecified> #f) if end of fields is reached.
;;
;; if obj is a hashtable, returned closure generates its keys and values.
;;
;; if obj is a ordered-hash, returned closure generates its keys and values in insertion order.
;;
;; if obj is '() or a pair, it must be a plist. returned closure generates its keys and values.
;;
;; if obj is a record type, returned closure generates its field names and values.
;;
(define in-fields
  (case-lambda
    ((obj cache)
      (cond
        ((or (null? obj) (pair? obj))
          (assert* 'in-fields (plist? obj))
          (in-plist obj))
        ;; in Chez Scheme, hashtable is a record type
        ;; => must check for it before (record?)
        ((hashtable? obj)
          (in-hash obj))
        ((ordered-hash? obj)
          (in-ordered-hash obj))
        ((record? obj)
          (let ((rtd (record-rtd obj)))
            (if rtd
              ;; first search for an override in reflect-infos, then search in cache if not found,
              ;; finally autogenerate reflect-info via reflection if both searches failed.
              (let* ((iter (ordered-hash-cursor
                             (or (hashtable-ref reflect-infos rtd #f)
                                 (and cache (hashtable-ref cache rtd #f))
                                 (make-reflect-info/reflect cache rtd))))
                     (%in-fields ;; name shown when displaying the closure
                       (lambda ()
                         (let ((cell (ordered-hash-cursor-next! iter)))
                           (if cell
                             (values (car cell) ((cdr cell) obj) #t)
                             (values #f #f #f))))))
                %in-fields)
              ;; no rtd, return an empty iterator
              empty-iterator)))
        (else
          empty-iterator)))
    ((obj)
      (in-fields obj #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; field-names


;; return a vector containing the field names of obj, in natural order.
;; each field name is represented as a symbol.
;;
;; if obj is a hashtable, return its keys.
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
          (hashtable-keys obj))
        ((ordered-hash? obj)
          (ordered-hash-keys obj))
        ((record? obj)
          (let* ((rtd   (record-rtd obj))
                 (xinfo (and rtd (hashtable-ref reflect-infos rtd #f))))
            (cond
              ((not rtd)
                '#())
              (xinfo ; found override in reflect-infos, use it
                (reflect-info-field-names xinfo))
              (cache ; search in cache, autogenerating a reflect-info for future calls if not found
                (cached-reflect-field-names obj cache rtd))
              (else
                (uncached-reflect-field-names obj (span) rtd)))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fields->plist

;; return a freshly allocated plist containing alternating field names and field values of obj, in natural order.
;; each field name is represented as a symbol.
;;
;; if obj is a hashtable, return its keys and values, interleaved.
;;
;; if obj is a ordered-hash, returns its keys and values in insertion order, interleaved.
;;
;; if obj is a list, assume it is a plist and return a copy of it.
;;
;; if obj is a record, return its field names and values, interleaved.
;;   the returned vector contains as **last** ones the field names
;;   of its record-rtd, preceded by the fields names of its parent-rtd,
;;   preceded by the fields names of its parent's parent-rtd, and so on.
;;
;; if field names cannot be retrieved, return an empty plist i.e. '().
(define fields->plist
  (case-lambda
    ((obj cache)
      (if (or (null? obj) (pair? obj))
        (list-copy obj)
        (let ((l '()))
          (for ((k v (in-fields obj cache)))
            (set! l (cons v (cons k l))))
          (reverse! l))))
    ((obj)
      (fields->plist obj #f))))


) ; close library
