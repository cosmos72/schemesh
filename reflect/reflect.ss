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
  (export array? array-accessor array-length chararray? chararray-accessor chararray-length htable? htable-cursor htable-size
          compare  equiv? greater-equiv? greater? less? less-equiv? unordered? record-compare-functions
          field field-cursor field-cursor-next! field-names fields->plist
          make-record-info make-record-info-autodetect make-record-deserializer
          record-info record-info? record-info-deserializer record-info-fill! record-info-set! record-infos)
  (import
    (rnrs)
    (only (chezscheme)                       date? date-year date-month date-day date-hour date-minute date-second date-nanosecond date-zone-offset 
                                             fx1+ fx1- fx/ list-copy logbit? make-date make-time procedure-arity-mask reverse!
                                             time? time-type time-second time-nanosecond void)
    (only (scheme2k bootstrap)               assert* let-macro)
    (only (scheme2k containers charspan)     charspan? charspan-length charspan-ref)
    (only (scheme2k containers date)         date date-compare date-equiv?)
    (only (scheme2k containers gbuffer)      gbuffer? gbuffer-length gbuffer-ref)
    (only (scheme2k containers hashtable)    eq-hashtable hash-cursor hash-cursor-next!)
    (only (scheme2k containers list)         plist? plist-ref)
          (scheme2k containers ordered-hash)
    (only (scheme2k containers time)         make-time-utc time-compare time-equiv?)
    (only (scheme2k containers span)         span span? span-insert-left! span-insert-left/vector! span-insert-right/vector!
                                             span-length span-ref span->vector)
    (only (scheme2k containers vector)       vector-every)
    (only (scheme2k posix fs)                dir-entry make-dir-entry))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compare value-like objects: booleans, characters, numbers, strings, symbols, times, dates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; retrieve or set the functions for comparing with (compare) and (equiv?) records having specified rtd
(define record-compare-functions
  (let ((ht (eq-hashtable (record-rtd (date 1970 1 1 0))   (cons date-compare date-equiv?)
                          (record-rtd (make-time-utc 0 0)) (cons time-compare time-equiv?))))
    (case-lambda
      ((rtd)
        (let ((pair (hashtable-ref ht rtd #f)))
          (and pair (cons (car pair) (cdr pair))))) ; make a copy
      ((rtd compare-proc)
        (record-compare-functions rtd compare-proc
          (lambda (a b) (eqv? 0 (compare-proc a b)))))
      ((rtd compare-proc equiv-proc)
        (assert* 'record-compare-function (record-type-descriptor? rtd))
        (assert* 'record-compare-function (procedure? compare-proc))
        (assert* 'record-compare-function (logbit? 2 (procedure-arity-mask compare-proc)))
        (assert* 'record-compare-function (procedure? equiv-proc))
        (assert* 'record-compare-function (logbit? 2 (procedure-arity-mask equiv-proc)))
        (hashtable-set! ht rtd (cons compare-proc equiv-proc))))))


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
    ((= a b)
      0)
    ((and (real? a) (real? b))
      (cond ((> a b) 1)
            ((< a b) -1)
            (else #f))) ;; not-a-numbers are unordered
    (else
      #f))) ;; complex numbers are unordered


(define (string-compare a b)
  (cond ((string=? a b) 0)
        ((string>? a b) 1)
        (else           -1)))


(define (symbol-compare a b)
  (if (eq? a b)
    0
    ;; compare symbol names
    (string-compare (symbol->string a) (symbol->string b))))


(define (symbol-equiv? a b)
  (eqv? 0 (symbol-compare a b)))


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
    ((boolean? a)   (and (boolean? b) (boolean-compare a b)))
    ((char?   a)    (and (char?   b)  (char-compare    a b)))
    ((number? a)    (and (number? b)  (number-compare  a b)))
    ((symbol? a)    (and (symbol? b)  (symbol-compare  a b)))
    ((string? a)    (and (string? b)  (string-compare  a b)))
    (else
      (let* ((rtd  (record-rtd a))
             (pair (record-compare-functions rtd)))
        (and pair
             (eq? rtd (record-rtd b))
             ((car pair) a b))))))


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
    ((boolean? a)   (eq? a b))
    ((char? a)      (and (char? b)    (char=? a b)))
    ((number? a)    (and (number? b)  (= a b)))
    ((symbol? a)    (and (symbol? b)  (symbol-equiv? a b)))
    ((string? a)    (and (string? b)  (string=? a b)))
    (else
      (let* ((rtd  (record-rtd a))
             (pair (record-compare-functions rtd)))
        (and pair
             (eq? rtd (record-rtd b))
             ((cdr pair) a b))))))


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
    (else           #f)))


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
;;; reflection on records
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; record-info


(define-syntax _type (identifier-syntax '<type>))


(define-record-type (record-info %make-record-info record-info?)
  (parent ordered-hash-type)
  (fields
    (mutable   field-names  record-info-field-names %record-info-field-names-set!)) ; vector of symbols
  (nongenerative %record-info-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; insert '<type> -> type-symbol-or-proc into specified record-info
(define (record-info-insert-type! info type-symbol-or-proc)
  (ordered-hash-set! info _type
    (cond
      ((symbol? type-symbol-or-proc)
        (lambda (obj) type-symbol-or-proc))
      (else
        (assert* 'make-record-info (procedure? type-symbol-or-proc))
        (assert* 'make-record-info (logbit? 1 (procedure-arity-mask type-symbol-or-proc)))
        type-symbol-or-proc))))


;; create and return a record-info containing caller-specified type-symbol, field names and their accessors.
;; names-and-accessors must be a plist alternating field-name and accessor.
(define (make-record-info type-symbol-or-proc names-and-accessors)
  (assert* 'make-record-info (plist? names-and-accessors))
  (let* ((len   (fx/ (length names-and-accessors) 2))
         (names (make-vector (fx1+ len)))
         (info  (%make-record-info (make-eq-hashtable) #f #f names)))
    ;; insert '<type> -> type-symbol-or-proc as first entry in ordered-hash
    (record-info-insert-type! info type-symbol-or-proc)
    ;; and insert '<type> as first field name
    (vector-set! names 0 _type)
    ;; followed by field names and accessors
    (do ((i 1 (fx1+ i))
         (l names-and-accessors (cddr l)))
        ((null? l) info)
      (let ((name     (car l))
            (accessor (cadr l)))
        (assert* 'make-record-info (symbol? name))
        (assert* 'make-record-info (procedure? accessor))
        (assert* 'make-record-info (logbit? 1 (procedure-arity-mask accessor)))
        ;; insert name into field names
        (vector-set! names i name)
        ;; insert name -> accessor into info's ordered-hash
        (ordered-hash-set! info name accessor)))))


;; create and return a record-info describing the fields of objects with specified rtd.
;; uses reflection to obtain field names and accessors.
(define make-record-info-autodetect
  (case-lambda
    ((rtd type-symbol-or-proc)
      (let ((info (%make-record-info (make-eq-hashtable) #f #f #f)))
        ;; insert '<type> -> type-symbol-or-proc as first entry in ordered-hash
        (record-info-insert-type! info type-symbol-or-proc)
        ;; followed by field names and accessors detected via reflection
        (record-info-fill! info rtd)
        info))
    ((rtd)
      (make-record-info-autodetect rtd (record-type-name rtd)))))


;; recursive implementation of (record-info-fill!)
(define (%record-info-fill! info rtd)
  (let ((parent-rtd (record-type-parent rtd)))
    (when parent-rtd
      ;; first, collect fields from parent record-type-descriptors
      (%record-info-fill! info parent-rtd)))
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
(define (record-info-fill! info rtd)
  (%record-info-fill! info rtd)
  ;; create and store vector of field-names
  (%record-info-field-names-set! info (ordered-hash-keys info)))


;; collect all accessors for fields in specified record-type-descriptor,
;; add them to cache, and return them as a record-info
;;
;; also collect accessors for fields in parent record-type-descriptors,
;; unless they conflict with a field name in a child record-type-descriptor.
;;
;; finally, also collect field names from specified record-type-descriptor and its parents,
;; and add them to the returned accessors hashtable with the key (void)
(define (make-record-info/reflect cache rtd)
  (let ((info (make-record-info-autodetect rtd)))
    (record-info-fill! info rtd)
    (hashtable-set! cache rtd info)
    info))


;; create and return a deserializer that scans a plist,
;; extracts the field values corresponding to fields contained in info,
;; and passes them to specified constructor, which is supposed to create and return an object
(define (make-record-deserializer constructor info)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record-infos


;; customize visible fields and deserializer for `date` objects
(define (add-date-info table)
  (let* ((rtd  (record-rtd (date 1970 1 1 +0)))
         (info (make-record-info 'date
                 (list 'year date-year 'month date-month   'day date-day
                       'hour date-hour 'minute date-minute 'second date-second
                       'nanosecond date-nanosecond 'zone-offset date-zone-offset))))
    (hashtable-set! table rtd info)
    (hashtable-set! table 'date
      (make-record-deserializer make-date info)))
  table)


;; customize visible fields and deserializer for `dir-entry` objects
(define (add-dir-entry-info table)
  (let* ((rtd  (record-type-descriptor dir-entry))
         (info (make-record-info-autodetect rtd)))
    (hashtable-set! table rtd info)
    (hashtable-set! table 'dir-entry
      (make-record-deserializer make-dir-entry info)))
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
         (info (make-record-info time-type ;; lambda obj -> type-symbol
                 ;; customize visible fields
                 (list 'second time-second 'nanosecond time-nanosecond))))
    (hashtable-set! table rtd info))
  (do ((l '(time-duration time-monotonic time-utc time-process time-thread time-collector-cpu time-collector-real)
           (cdr l)))
      ((null? l) table)
    (hashtable-set! table (car l) deserialize-time)))


;; global table record-infos, contains user-provided record-info and deserializer
;; that customize how objects having user-provided rtd are serialized / deserialized
(define record-infos
  (add-date-info
    (add-dir-entry-info
      (add-time-info
        (make-eq-hashtable)))))


;; customize visible fields and deserializer of objects having specified rtd:
;; stores info and user-specified deserializer into global table record-infos.
(define (record-info-set! rtd info deserializer)
  (let ((type-symbol (ordered-hash-ref info _type)))
    (assert* 'record-info-set! (symbol? type-symbol))
    (assert* 'record-info-set! (procedure? deserializer))
    (assert* 'record-info-set! (logbit? 1 (procedure-arity-mask deserializer)))
    ;; put in record-infos both rtd -> info and type-symbol -> deserializer
    (hashtable-set! record-infos rtd info)
    (hashtable-set! record-infos type-symbol deserializer)))


;; return user-added deserializer for specified type-symbol, or #f if not found
(define (record-info-deserializer type-symbol)
   (assert* 'record-info-deserializer (symbol? type-symbol))
   (hashtable-ref record-infos type-symbol #f))


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
(define (uncached-record-field obj field-name default rtd)
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
          (uncached-record-field obj field-name default (record-type-parent rtd)))))))


;; implementation of (field) for record types.
;; returns value of specified field name in obj, or default
(define (cached-record-field obj field-name cache default rtd)
  (cond
    ((not rtd) ;; no rtd => cannot access fields
      default)
    ((eq? field-name _type) ;; synthetic field '<type> always has value = record type name
      (record-type-name rtd))
    (else
      (let* ((info     (or (hashtable-ref cache rtd #f) (make-record-info/reflect cache rtd)))
             (accessor (ordered-hash-ref info field-name #f)))
        (if accessor
          (accessor obj)
          ;; all fields of rtd and its parents are present in cached info
          ;; => requested field-name is not present
          default)))))


(define (cached-record-field-names obj cache rtd)
 (record-info-field-names (or (hashtable-ref cache rtd #f) (make-record-info/reflect cache rtd))))


(define (%uncached-record-field-names obj sp rtd)
  (when rtd
    ;; insert fields from this record-type-descriptor *before* the subtypes field names
    (span-insert-left/vector! sp (record-type-field-names rtd))
    ;; then iterate on parent record-type-descriptor
    (%uncached-record-field-names obj sp (record-type-parent rtd))))


(define (uncached-record-field-names obj sp rtd)
  (%uncached-record-field-names obj sp rtd)
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
                 (xinfo (and rtd (hashtable-ref record-infos rtd #f))))
            (cond
              (xinfo ; found override in record-infos, use it
                (let ((accessor (ordered-hash-ref xinfo field-name #f)))
                  (if accessor
                    (accessor obj)
                    default)))
              (cache ; search in cache, autogenerating and storing a record-info for future calls if not present
                (cached-record-field obj field-name cache default rtd))
              (else
                (uncached-record-field obj field-name default rtd)))))
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
;; obj must be a record type.
;; Caller must repeatedly invoke (field-cursor-next!) on returned cursor - see its description.
(define (field-cursor obj cache)
  (let ((rtd   (record-rtd obj)))
    (if rtd
      ;; first search in override in record-infos, then seacrh in cache if not found,
      ;; finally autogenerate via reflection if both searches failed.
      (let ((info (or (hashtable-ref record-infos rtd #f)
                      (hashtable-ref cache        rtd #f) 
                      (make-record-info/reflect cache rtd))))
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
          (hashtable-keys obj))
        ((ordered-hash? obj)
          (ordered-hash-keys obj))
        ((record? obj)
          (let* ((rtd   (record-rtd obj))
                 (xinfo (and rtd (hashtable-ref record-infos rtd #f))))
            (cond
              ((not rtd)
                '#())
              (xinfo ; found override in record-infos, use it
                (record-info-field-names xinfo))
              (cache ; search in cache, autogenerating a record-info for future calls if not found
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


;; return a freshly allocated plist containing alternating field names and field values of obj, in natural order.
;; each field name is represented as a symbol.
;;
;; if obj is a hashtable, return its keys.
;;   if all the keys are symbols, they are are returned in lexicographic order.
;;
;; if obj is a ordered-hash, returns its keys in insertion order.
;;
;; if obj is a list, assume it is a plist and return a copy of it.
;;
;; if obj is a record, return its field names and field values, interleaved.
;;   the returned vector contains as **last** ones the field names
;;   of its record-rtd, preceded by the fields names of its parent-rtd,
;;   preceded by the fields names of its parent's parent-rtd, and so on.
;;
;; if field names cannot be retrieved, return an empty plist i.e. '().
(define fields->plist
  (case-lambda
    ((obj cache)
      (cond
        ((list? obj)
          (list-copy obj))
        (else
          (let ((names (field-names obj)))
            (let %field-names->plist ((i (fx1- (vector-length names)))
                                      (plist '()))
              (if (fx<? i 0)
                plist
                (let ((name (vector-ref names i)))
                  (%field-names->plist (fx1- i) (cons name (cons (field obj name) plist))))))))))
    ((obj)
      (fields->plist obj #f))))


) ; close library
