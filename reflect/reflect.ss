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
  (export reflect-field)
  (import
    (rnrs)
    (only (chezscheme)                   fx1+ fxvector? fxvector-length fxvector-ref meta-cond)
    (only (scheme2k bootstrap)           assert*)
    (only (scheme2k containers list)     plist? plist-ref)
    (only (scheme2k containers flvector) flvector? flvector-length flvector-native? flvector-ref))


(define (cache-record-accessors ht rtd namevec)
  ;; TODO: also cache accessors of parents record-type-descriptors
  (let* ((len (vector-length namevec))
         (accessors (make-eqv-hashtable (fx* 2 len))))
    (hashtable-set! ht rtd accessors)
    (do ((i 0 (fx1+ i)))
        ((fx>=? i len) accessors)
      (let ((accessor (record-accessor rtd i)))
        (hashtable-set! accessors i accessor)
        (hashtable-set! accessors (vector-ref namevec i) accessor)))))


(define (reflect-record-field-uncached obj field default rtd namevec)
  ;; TODO: also search in parents record-type-descriptors
  (let ((i (cond
             ((fixnum? field)
               (and (fx<? -1 field (vector-length namevec)) field))
             ((symbol? field)
               (let %scan ((pos 0) (len (vector-length namevec)))
                 (cond
                   ((fx>=? pos len)
                     #f)
                   ((eq? field (vector-ref namevec pos))
                     pos)
                   (else
                     (%scan (fx1+ pos) len)))))
             (else
               #f))))
    (if i
     ((record-accessor rtd i) obj)
     default)))




;; implementation of (reflect-field) for record types.
;; returns value of field in obj, or default
(define (reflect-record-field obj field default rtd-cache)
  (let ((rtd (record-rtd obj)))
    (if rtd
      (let* ((accessors (and rtd-cache (hashtable-ref rtd-cache rtd #f)))
             (accessor  (and accessors (hashtable-ref accessors field #f))))
        (cond
          (accessor
            (accessor obj))
          (accessors
            ;; all fields of rtd are present in cached accessors => requested field is not present
            default)
          (else
            ;; no cached accessors, list them manually
            (let ((namevec (record-type-field-names rtd)))
              (if rtd-cache
                ;; cache all accessors, then lookup field in cached accessors
                (let* ((accessors (cache-record-accessors rtd-cache rtd namevec))
                       (accessor  (hashtable-ref accessors field #f)))
                  (if accessor
                    (accessor obj)
                    default))
                ;; work without a cache
                (reflect-record-field-uncached obj field default rtd namevec))))))

      ;; cannot retrieve rtd of object => cannot access its fields
      default)))


;; find the value of specified field in obj.
;; obj must be a record, flvector, fxvector, hashtable, plist or vector.
;; field must be a symbol or an unsigned fixnum.
;;
;; return the value of specified field, or default if not found
(define reflect-field
  (let ((missing (cons #f #f)))
    (case-lambda
      ((obj field default rtd-cache)
        (cond
          ;; in Chez Scheme hashtable is a record => must checked for (hashtable?) before (record?)
          ((hashtable? obj)
            ;; FIXME: can raise condition if hashtable-hash-function or hashtable-equivalence-function
            ;; do not allow field's type and raise a condition
            (let ((value (hashtable-ref obj field missing)))
              (if (eq? value missing) default value)))
          ((record? obj)
            (reflect-record-field obj field default rtd-cache))
          ((vector? obj)
            (if (and (fixnum? field)
                     (fx<? -1 field (vector-length obj)))
              (vector-ref obj field)
              default))
          ((fxvector? obj)
            (if (and (fixnum? field)
                     (fx<? -1 field (fxvector-length obj)))
              (fxvector-ref obj field)
              default))
          ((meta-cond (flvector-native? (flvector? obj))
                      (else             #f))
            (if (and (fixnum? field)
                     (fx<? -1 field (flvector-length obj)))
              (flvector-ref obj field)
              default))
          ((null? obj)
            default)
          ((plist? obj)
            (let ((value (plist-ref obj field missing)))
              (if (eq? value missing) default value)))
          (else
            default)))
    ((obj field default)
      (reflect-field obj field default #f)))))



) ; close library
