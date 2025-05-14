;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh containers hashtable (0 9 1))
  (export
    make-hash-iterator hash-iterator? hash-iterator-copy hash-iterator-pair hash-iterator-next!

    for-hash for-hash-keys for-hash-pairs for-hash-values
    hash-for-each hash-for-each-key hash-for-each-pair hash-for-each-value
    in-hash in-hash-keys in-hash-pairs in-hash-values

    eq-hashtable eqv-hashtable hashtable
    alist->eq-hashtable alist->eqv-hashtable alist->hashtable
    plist->eq-hashtable plist->eqv-hashtable plist->hashtable
    hashtable-transpose)
  (import
    (rnrs)
    (only (chezscheme)                 $primitive fx1+ fx/ include meta record-writer)
    (only (schemesh bootstrap)         assert* generate-pretty-temporaries with-while-until)
    (only (schemesh containers list)   for-list for-plist)
    (only (schemesh containers vector) vector-index))


;; NOTE: (hash-table-for-each) exported by Chez Scheme
;; is not suitable for implementing iterators, (for-hash) or (in-hash)
;; because at least up to Chez Scheme v10.0.0 it works *only* on eq-hashtable:s.


(include "containers/hashtable-accessors.ss")


;; Note: eqv hashtables contain two inner hashtables:
;; one for keys comparable with eq, and one for all other keys.
;; We must retrieve both vectors from them and iterate on both.
(define-record-type (%hash-iterator %make-iter hash-iterator?)
  (fields
    (mutable index  iter-index  iter-index-set!)
    (mutable bucket iter-bucket iter-bucket-set!)
    (mutable vec1   iter-vec1   iter-vec1-set!)
    (mutable vec2   iter-vec2   iter-vec2-set!))
  (nongenerative %hash-iterator-7c46d04b-34f4-4046-b5c7-b63753c1be39)
  (sealed #t))

(define (bucket-valid? bucket)
  (or (pair? bucket) (($primitive 3 $tlc?) bucket)))

(define (bucket-keyval bucket)
  (cond
    ((pair? bucket)
      (car bucket))
    ((($primitive 3 $tlc?) bucket)
      (($primitive 3 $tlc-keyval) bucket))
    (else
      #f)))

(define (bucket-next bucket)
  (cond
    ((pair? bucket)
      (cdr bucket))
    ((($primitive 3 $tlc?) bucket)
      (($primitive 3 $tlc-next) bucket))
    (else
      #f)))

;; make a copy of specified hash-iterator
(define (hash-iterator-copy iter)
  (%make-iter (iter-index iter) (iter-bucket iter) (iter-vec1 iter) (iter-vec2 iter)))


;; return hashtable element (key . val) corresponding to current position
;; of hash-iterator, or #f if end of hashtable is reached
;;
;; setting the cdr of returned element propagates back to the hashtable,
;; i.e. it is equivalent to setting the value associated to key in the hashtable
;;
;; NEVER set or modify in any way the car of returned element!
(define (hash-iterator-pair iter)
  (bucket-keyval (iter-bucket iter)))


;; modify hash-iterator in place to point to next hashtable element.
;; return next hashtable element (key . val) if more elements are available,
;; otherwise return #f
;;
;; as (hash-iterator-pair), setting the cdr of returned element propagates back
;; to the hashtable.
(define (hash-iterator-next! iter)
  (let* ((index  (iter-index  iter))
         (bucket (bucket-next (iter-bucket iter)))
         (vec1   (iter-vec1   iter))
         (vlen   (vector-length vec1)))

    ; iterate on vec1 until we find a cell
    (do ()
      ((or (bucket-valid? bucket) (fx>=? index vlen)))
      (set! index (fx1+ index))
      (when (fx<? index vlen)
        (set! bucket (vector-ref vec1 index))))
    (iter-index-set!  iter index)
    (iter-bucket-set! iter bucket)

    (let ((vec2   (iter-vec2 iter)))
      (if (or (bucket-valid? bucket) (fxzero? (vector-length vec2)))
        ; either we found a cell, or vec2 is empty and we reached end of vec1
        (bucket-keyval  bucket)
        ; no cell found, but vec2 is non-empty: switch to it and retry
        (begin
          (iter-index-set!  iter -1)
          (iter-bucket-set! iter #f)
          (iter-vec1-set!   iter vec2)
          (iter-vec2-set!   iter (vector))
          (hash-iterator-next! iter))))))


;; return hash-iterator to first element in hashtable
(define (make-hash-iterator h)
  (if (fxzero? (hashtable-size h))
    ; hashtable is empty, return empty iterator
    (%make-iter 0 #f (vector) (vector))
    ; hashtable is not empty, seek to first bucket
    (let* ((is-eqv (and (not (hashtable-hash-function h))
                        (eq? eqv? (hashtable-equivalence-function h))))
           (vec1 (%hashtable->vector
                   (if is-eqv (%eqv-hashtable->eq-hashtable h) h)))
           (vec2 (if is-eqv
                   (%hashtable->vector (%eqv-hashtable->gen-hashtable h))
                   '#()))
           (iter (%make-iter -1 #f vec1 vec2)))
      ; advance iterator to first bucket
      (hash-iterator-next! iter)
      iter)))


;; create and return a closure that iterates on elements of hashtable t.
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in hashtable t and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of hashtable is reached.
(define (in-hash htable)
  (let* ((iter (make-hash-iterator htable))
         (next (hash-iterator-pair iter)))
     (lambda ()
       (if (pair? next)
         (let ((cell next))
           (set! next (hash-iterator-next! iter))
           (values (car cell) (cdr cell) #t))
         (values #f #f #f)))))


;; create and return a closure that iterates on keys of hashtable htable.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values key #t) i.e. the next key in hashtable and #t,
;; or (values #<unspecified> #f) if end of hashtable is reached.
(define (in-hash-keys htable)
  (let* ((iter (make-hash-iterator htable))
         (next (hash-iterator-pair iter)))
     (lambda ()
       (if (pair? next)
         (let ((cell next))
           (set! next (hash-iterator-next! iter))
           (values (car cell) #t))
         (values #f #f)))))


;; create and return a closure that iterates on each pair containing (key . value) of htable.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values pair #t) i.e. the next pair containing (key . value) in hashtable and #t,
;; or (values #<unspecified> #f) if end of hashtable is reached.
;;
;; Assigning the (cdr) of a pair propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any pair!
(define (in-hash-pairs htable)
  (let* ((iter (make-hash-iterator htable))
         (next (hash-iterator-pair iter)))
     (lambda ()
       (if (pair? next)
         (let ((cell next))
           (set! next (hash-iterator-next! iter))
           (values cell #t))
         (values #f #f)))))


;; create and return a closure that iterates on values of hashtable t.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values key #t) i.e. the next key in hashtable t and #t,
;; or (values #<unspecified> #f) if end of hashtable is reached.
(define (in-hash-values htable)
  (let* ((iter (make-hash-iterator htable))
         (next (hash-iterator-pair iter)))
     (lambda ()
       (if (pair? next)
         (let ((cell next))
           (set! next (hash-iterator-next! iter))
           (values (cdr cell) #t))
         (values #f #f)))))


;; Iterate on elements of given hashtables htable, and call (proc key value) on each key and value.
;; Return unspecified value.
(define (hash-for-each proc htable)
  (assert* 'hash-for-each (procedure? proc))
  (hash-for-each-pair (lambda (cell) (proc (car cell) (cdr cell)))
                      htable))


;; Iterate on elements of given hashtables htable, and call (proc key) on each key.
;; Return unspecified value.
(define (hash-for-each-key proc htable)
  (assert* 'hash-for-each-key (procedure? proc))
  (hash-for-each-pair (lambda (cell) (proc (car cell)))
                      htable))


;; Iterate on elements of given hashtables htable, and call (proc pair) on each pair (key . value).
;; Return unspecified value.
;;
;; Do NOT modify the (car) of any pair!
(define (hash-for-each-pair proc htable)
  (assert* 'hash-for-each-pair (procedure? proc))
  (let ((iter (make-hash-iterator htable)))
    (do ((cell (hash-iterator-pair iter) (hash-iterator-next! iter)))
        ((not cell))
      (proc cell))))


;; Iterate on elements of given hashtables htable, and call (proc value) on each value.
;; Return unspecified value.
(define (hash-for-each-value proc htable)
  (assert* 'hash-for-each-value (procedure? proc))
  (hash-for-each-pair (lambda (cell) (proc (cdr cell)))
                      htable))

;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each key and value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Note: body ... must evaluate to a single value.
(define-syntax for-hash
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((key val htable) ...) body1 body2 ...)
        (not (null? #'(htable ...)))
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          (with-syntax (((cell ...) (generate-pretty-temporaries #'(htable ...))))
            #'(let ((iter (make-hash-iterator htable)) ...)
                (let %for-hash ((cell (hash-iterator-pair iter)) ...)
                  (when (and cell ...)
                    (let ((key (car cell)) ...
                          (val (cdr cell)) ...)
                      (with-while-until
                        body1 body2 ...
                        (%for-hash (hash-iterator-next! iter) ...))))))))))))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each key.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Note: body ... must evaluate to a single value.
(define-syntax for-hash-keys
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((key htable) ...) body1 body2 ...)
        (not (null? #'(htable ...)))
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          (with-syntax (((cell ...) (generate-pretty-temporaries #'(htable ...))))
            #'(let ((iter (make-hash-iterator htable)) ...)
                (let %for-hash-keys ((cell (hash-iterator-pair iter)) ...)
                  (when (and cell ...)
                    (let ((key (car cell)) ...)
                      (with-while-until
                        body1 body2 ...
                        (%for-hash-keys (hash-iterator-next! iter) ...))))))))))))


;; Iterate in parallel on elements of given hashtables ht ...,
;; and evaluate body ... on each pair containing (key . value).
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Assigning the (cdr) of a pair propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any pair!
;;
;; Note: body ... must evaluate to a single value.
(define-syntax for-hash-pairs
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((pair htable) ...) body1 body2 ...)
        (not (null? #'(htable ...)))
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (make-hash-iterator htable)) ...)
              (let %for-hash-pairs ((pair (hash-iterator-pair iter)) ...)
                (when (and pair ...)
                  (with-while-until
                    body1 body2 ...
                    (%for-hash-pairs (hash-iterator-next! iter) ...))))))))))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Note: body ... must evaluate to a single value.
(define-syntax for-hash-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((val htable) ...) body1 body2 ...)
        (not (null? #'(htable ...)))
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          (with-syntax (((cell ...) (generate-pretty-temporaries #'(htable ...))))
            #'(let ((iter (make-hash-iterator htable)) ...)
                (let %for-hash-keys ((cell (hash-iterator-pair iter)) ...)
                  (when (and cell ...)
                    (let ((val (cdr cell)) ...)
                      (with-while-until
                        body1 body2 ...
                        (%for-hash-keys (hash-iterator-next! iter) ...))))))))))))


;; (hashtable-transpose src dst) iterates on all (key . value) elements of hashtable src,
;; and inserts each of them into hashtable dst as transposed (value . key)
;
;; Returns dst.
(define (hashtable-transpose src dst)
  (for-hash ((key val src))
    (hashtable-set! dst val key))
  dst)


;; (alist->eq-hashtable l) iterates on all (key . value) elements of list l
;; and inserts each of them into a new hashtable created with
;;   (make-eq-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (alist->eq-hashtable l)
  (let ((dst (make-eq-hashtable (length l))))
    (for-list ((cell l))
      (hashtable-set! dst (car cell) (cdr cell)))
    dst))


;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (alist->eqv-hashtable l)
  (let ((dst (make-eqv-hashtable (length l))))
    (for-list ((cell l))
      (hashtable-set! dst (car cell) (cdr cell)))
    dst))

;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (length pairs)).
;
;; Returns the created hashtable.
(define (alist->hashtable hash-proc eq-proc l)
  (let ((dst (make-hashtable hash-proc eq-proc (length l))))
    (for-list ((cell l))
      (hashtable-set! dst (car cell) (cdr cell)))
    dst))


;; iterate on all key value elements of plist l,
;; and inserts each of them into a new hashtable created with
;; (make-eq-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (plist->eq-hashtable plist)
  (let ((dst (make-eq-hashtable (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (hashtable-set! dst key value))
    dst))


;; iterate on all key value elements of plist l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (plist->eqv-hashtable plist)
  (let ((dst (make-eqv-hashtable (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (hashtable-set! dst key value))
    dst))


;; iterate on all key value elements of plist l,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (length pairs)).
;
;; Returns the created hashtable.
(define (plist->hashtable hash-proc eq-proc plist)
  (let ((dst (make-hashtable hash-proc eq-proc (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (hashtable-set! dst key value))
    dst))

;; iterates on all key value elements of plist,
;; and inserts each of them into a new hashtable created with
;;   (make-eq-hashtable (fx/ (length plist) 2)).
;;
;; Returns the created hashtable.
(define (eq-hashtable . plist)
  (plist->eq-hashtable plist))


;; iterate on all key value elements of plist,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (fx/ (length plist) 2)).
;;
;; Returns the created hashtable.
(define (eqv-hashtable . plist)
  (plist->eqv-hashtable plist))


;; iterate on all key value elements of plist,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (fx/ (length plist) 2)).
;
;; Returns the created hashtable.
(define (hashtable hash-proc eq-proc . plist)
  (plist->hashtable hash-proc eq-proc plist))


;; customize how "hash-iterator" objects are printed
(record-writer (record-type-descriptor %hash-iterator)
  (lambda (iter port writer)
    (display "#<hash-iterator>" port)))

) ; close library
