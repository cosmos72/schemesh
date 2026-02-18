;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(library (scheme2k io sort (0 9 3))
  (export make-sort-reader sort-reader sort-reader?
          sort-reader-get sort-reader-eof? sort-reader-close sort-reader-skip
          sort-reader-inner)
  (import
    (rnrs)
    (only (chezscheme)                 fx1+ fx1- record-writer void)
    (only (scheme2k bootstrap)         assert*)
    (only (scheme2k containers list)   symbol-list?)
    (only (scheme2k containers span)   span span-clear! span-delete-left! span-empty? span-insert-right! span-length span-ref)
    (only (scheme2k containers sort)   span-sort-by!)
          (scheme2k io obj)
    (only (scheme2k reflect)           compare-type-and-value field))


;; Reader that wraps another "inner" reader, accumulates all elements, closes the inner reader,
;; sorts them and finally then passes the sorted elements to caller.
(define-record-type (sort-reader %make-sort-reader sort-reader?)
  (parent nested-reader)
  (fields
    (mutable compare-proc)  ; #f or closure that compares elements
    span                    ; span of accumulated elements
    close-inner?)           ; boolean
  (protocol
    (lambda (args->new)
      (lambda (inner compare-proc close-inner?)
         ((args->new %sort-reader-get #f %sort-reader-close inner)
            compare-proc (span) close-inner?))))
  (nongenerative %sort-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


(define make-compare-proc
  (case-lambda
    ((field-names cache)
      (when cache
        (assert* 'make-compare-proc (hashtable? cache))
        (assert* 'make-compare-proc (eq? eq? (hashtable-equivalence-function cache))))
      (letrec* ((%cache (or cache (make-eq-hashtable)))
                ;; compare a single field in obj1 and obj2
                (compare-field-proc
                  (lambda (obj1 obj2 field-name)
                    (compare-type-and-value (field obj1 field-name %cache)
                                             (field obj2 field-name %cache))))
                ;; compare a list of fields in obj1 and obj2
                (compare-fields-proc
                  (lambda (obj1 obj2 field-names)
                    (if (null? field-names)
                      0
                      (let ((cmp (compare-field-proc obj1 obj2 (car field-names))))
                        (if (eqv? 0 cmp)
                          ;; obj1 and obj2 have equivalent field => compare the remaining fields
                          (compare-fields-proc obj1 obj2 (cdr field-names))
                          cmp))))))
        ;; WARNING: not a total order, obj1 and obj2 may compare as unordered
        (lambda (obj1 obj2)
          (compare-fields-proc obj1 obj2 field-names))))
    ((field-names)
      (make-compare-proc field-names #f))))


;; Create and return a sort-reader that wraps another "inner" reader.
;; Mandatory arguments:
;;   inner - the reader to wrap
;;
;; Optional arguments:
;;   field-names - a list of field names to sort on
;;
;; Note: as per obj-reader contract, by default closing a sort-reader does NOT close
;; the wrapped reader, because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a sort-reader should take ownership of the wrapped reader passed to the constructor,
;; then pass a truish value as the optional argument close-inner?
(define make-sort-reader
  (case-lambda
    ((inner field-names close-inner?)
      (assert* 'make-sort-reader (obj-reader? inner))
      (assert* 'make-sort-reader (symbol-list? field-names))
      (let ((compare-proc (make-compare-proc field-names)))
        (%make-sort-reader inner compare-proc close-inner?)))
    ((inner field-names)
      (make-sort-reader inner field-names #f))))


(define (sort-reader-eof? rx)
  (assert* 'sort-reader-eof? (sort-reader? rx))
  (obj-reader-eof? rx))


(define (sort-reader-close rx)
  (assert* 'sort-reader-close (sort-reader? rx))
  (obj-reader-close rx))


;; return the wrapped, "inner" reader
(define (sort-reader-inner rx)
  (assert* 'sort-reader-inner (sort-reader? rx))
  (nested-reader-inner rx))


(define (sort-reader-get rx)
  (assert* 'sort-reader-get (sort-reader? rx))
  (obj-reader-get rx))


(define (sort-reader-skip rx)
  (assert* 'sort-reader-skip (sort-reader? rx))
  (obj-reader-skip rx))


;; called by (sort-reader-close) and (obj-reader-close)
(define (%sort-reader-close rx)
  (when (sort-reader-close-inner? rx)
    (nested-reader-inner-close rx))
  (sort-reader-compare-proc-set! rx #f)
  (span-clear! (sort-reader-span rx)))


;; called by (sort-reader-get) and (obj-reader-get)
(define (%sort-reader-get rx)
  (let ((compare-proc (sort-reader-compare-proc rx))
        (sp           (sort-reader-span rx)))
    (cond
      (compare-proc
        ;; accumulate ALL element generated by inner reader
        (let-values (((obj ok?) (nested-reader-inner-get rx)))
          (if ok?
            (span-insert-right! (sort-reader-span rx) obj)
            (begin
              ;; compare-proc is not necessarily a total order: elements can be grouped
              ;; in one or more "connected component" that are internally totally ordered,
              ;; but elements from different connected components are always unordered.
              ;; => use (span-sort-by!) that handles such situation
              (span-sort-by! compare-proc sp)
              (sort-reader-compare-proc-set! rx #f)))

          ;; iterate: either get more elements from inner reader,
          ;; or return one of them if inner reader is exhausted
          (%sort-reader-get rx)))

      ;; we got all elements from inner reader, and sorted them.
      ;; now return them one by one
      ((span-empty? sp)
        (values #f #f))
      (else
        (let ((obj (span-ref sp 0)))
          (span-delete-left! sp 1)
          (values obj #t))))))


) ; close library
