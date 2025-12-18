;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; Chez Scheme hashtables can be iterated only by copying their WHOLE contents to one or more vectors,
;; or with (hash-table-for-each) that ONLY works on eq-hashtables.
;;
;; Define functions for accessing hashtables' internal bucket vector
;; and let hash-iterator iterate on them.

(define (record-accessor-byname rtd name)
  (and rtd
       (let ((index (vector-index name (record-type-field-names rtd))))
         (and index
              (record-accessor rtd index)))))

(define (record-accessor-byname/recursive rtd name)
  (or (record-accessor-byname rtd name)
      (record-accessor-byname/recursive (record-type-parent rtd) name)))


(define %eqv-hashtable->eq-hashtable
  (let ((%eqv-hashtable-rtd (record-rtd (make-eqv-hashtable))))
    (record-accessor-byname %eqv-hashtable-rtd 'eqht)))

(define %eqv-hashtable->gen-hashtable
  (let ((%eqv-hashtable-rtd (record-rtd (make-eqv-hashtable))))
    (record-accessor-byname %eqv-hashtable-rtd 'genht)))

(define %hashtable->vector
  (let ((%gen-hashtable-rtd (record-rtd (make-hashtable equal-hash equal?))))
    (record-accessor-byname/recursive %gen-hashtable-rtd 'vec)))
