;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(library (scheme2k io sort (0 9 3))
  (export make-sort-reader sort-reader sort-reader?)
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


(define (%validate-field-name caller field-name)
  (unless (symbol? field-name)
    (assert* caller (list? field-name))
    (assert* caller (fx=? 2 (length field-name)))
    (assert* caller (memq (car field-name) '(+ -)))
    (assert* caller (symbol? (cadr field-name)))))


(define (%validate-field-names caller field-names)
  (do ((l field-names (cdr l)))
      ((null? l))
    (%validate-field-name caller (car l))))


;; create and return a closure that compares field-name in two objects
(define (make-compare-proc1 field-name cache)
  (let ((name    (if (symbol? field-name) field-name (cadr field-name)))
        (ascend? (or (symbol? field-name) (eq? '+ (car field-name)))))
    (if ascend?
      (lambda (obj1 obj2)
        (compare-type-and-value (field obj1 name cache)
                                (field obj2 name cache)))
      (lambda (obj1 obj2)
        (let ((cmp (compare-type-and-value (field obj1 name cache)
                                           (field obj2 name cache))))
          (and cmp (fx- cmp))))))) ; found (- field-name) => invert comparison


;; create and return a closure that compares field-names in two objects
(define make-compare-proc
  (case-lambda
    ((caller field-names cache)
      (%validate-field-names caller field-names)
      (when cache
        (assert* caller (hashtable? cache))
        (assert* caller (eq? eq? (hashtable-equivalence-function cache))))
      (let* ((cache         (or cache (make-eq-hashtable)))
             (compare-procs (map (lambda (field-name) (make-compare-proc1 field-name cache))
                                 field-names)))
        ;; compare field-names in obj1 and obj2
        ;; WARNING: not a total order, obj1 and obj2 may compare as unordered
        (lambda (obj1 obj2)
          (let compare-fields ((obj1 obj1) (obj2 obj2) (procs compare-procs))
            (if (null? procs)
              0
              (let ((cmp ((car procs) obj1 obj2)))
                (if (eqv? 0 cmp)
                  ;; obj1 and obj2 have equivalent field => compare the remaining fields
                  (compare-fields obj1 obj2 (cdr procs))
                  cmp)))))))

    ((caller field-names)
      (make-compare-proc caller field-names #f))))


;; Create and return a sort-reader that wraps another "inner" reader.
;; Mandatory arguments:
;;   inner - the reader to wrap
;;
;; Optional arguments:
;;   field-names - a list of field names to sort on
;;
;; Note: as per reader contract, by default closing a sort-reader does NOT close
;; the wrapped reader, because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a sort-reader should take ownership of the wrapped reader passed to the constructor,
;; then pass a truish value as the optional argument close-inner?
(define make-sort-reader
  (case-lambda
    ((inner field-names close-inner?)
      (assert* 'make-sort-reader (reader? inner))
      (let ((compare-proc (make-compare-proc 'make-sort-reader field-names)))
        (%make-sort-reader inner compare-proc close-inner?)))
    ((inner field-names)
      (make-sort-reader inner field-names #f))))


;; called by (reader-close)
(define (%sort-reader-close rx)
  (when (sort-reader-close-inner? rx)
    (nested-reader-inner-close rx))
  (sort-reader-compare-proc-set! rx #f)
  (span-clear! (sort-reader-span rx)))


;; called by (reader-get)
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
