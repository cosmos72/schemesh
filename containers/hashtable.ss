;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers hashtable (0 9 3))
  (export
    hash-cursor hash-cursor? hash-cursor-copy hash-cursor-cell hash-cursor-next!

    for-hash for-hash-keys for-hash-cells for-hash-values
    hash-for-each hash-for-each-cell hash-for-each-key hash-for-each-value
    in-hash in-hash-keys in-hash-cells in-hash-values
    hash-reader

    eq-hashtable eqv-hashtable hashtable
    alist->eq-hashtable alist->eqv-hashtable alist->hashtable
    plist->eq-hashtable plist->eqv-hashtable plist->hashtable
    hashtable-transpose)
  (import
    (rnrs)
    (only (chezscheme)                 $primitive format fx1+ fx1- fx/ include meta record-writer)
    (only (scheme2k bootstrap)         assert* forever generate-pretty-temporaries with-while-until)
    (only (scheme2k io obj)            make-obj-reader)
    (only (scheme2k containers list)   for-alist for-plist)
    (only (scheme2k containers string) string-prefix? string-suffix? display-procedure-name)
    (only (scheme2k containers vector) vector-index))


;; NOTE: (hash-table-for-each) exported by Chez Scheme
;; is not suitable for implementing iterators, (for-hash) or (in-hash)
;; because at least up to Chez Scheme v10.0.0 it works *only* on eq-hashtable:s.


(include "containers/hashtable-accessors.ss")


;; Note: eqv hashtables contain two inner hashtables:
;; one for keys comparable with eq, and one for all other keys.
;; We must retrieve both vectors from them and iterate on both.
(define-record-type (%hash-cursor %make-iter hash-cursor?)
  (fields
    (mutable index  iter-index  iter-index-set!)
    (mutable bucket iter-bucket iter-bucket-set!)
    (mutable vec1   iter-vec1   iter-vec1-set!)
    (mutable vec2   iter-vec2   iter-vec2-set!))
  (nongenerative %hash-cursor-7c46d04b-34f4-4046-b5c7-b63753c1be39)
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

;; make a copy of specified hash-cursor
(define (hash-cursor-copy iter)
  (%make-iter (iter-index iter) (iter-bucket iter) (iter-vec1 iter) (iter-vec2 iter)))


;; return hashtable element (key . val) corresponding to current position
;; of hash-cursor, or #f if end of hashtable is reached
;;
;; setting the cdr of returned element propagates back to the hashtable,
;; i.e. it is equivalent to setting the value associated to key in the hashtable
;;
;; NEVER set or modify in any way the car of returned element!
(define (hash-cursor-cell iter)
  (bucket-keyval (iter-bucket iter)))


;; return current hashtable element (key . val) if more elements are available,
;; otherwise return #f
;;
;; as a side effect, modifies hash-cursor in place to point to next hashtable element.
;;
;; as (hash-cursor-cell), setting the cdr of returned element propagates back
;; to the hashtable.
(define (hash-cursor-next! iter)
  (let* ((bucket  (bucket-next (iter-bucket iter)))
         (index   (iter-index  iter))
         (vec1    (iter-vec1   iter))
         (vlen    (vector-length vec1)))

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
        ;; either we found a cell, or vec2 is empty and we reached end of vec1
        ;; in both cases, stop iterating and return keyval
        (bucket-keyval  bucket)
        ; no cell found, but vec2 is non-empty: switch to it and retry
        (begin
          (iter-index-set!  iter -1)
          (iter-bucket-set! iter #f)
          (iter-vec1-set!   iter vec2)
          (iter-vec2-set!   iter (vector))
          (hash-cursor-next! iter))))))


;; return hash-cursor to first element in hashtable
(define (hash-cursor h)
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
      iter)))



;; create and return a obj-reader that generates the cells of specified hashtable.
;; each call to (obj-reader-get p) will return two values:
;;  either (values cell truish) i.e. the next cell the hashtable, where cell is a pair (key . value)
;;  or (values #<unspecified> #f) when the hashtable is exhausted or after (obj-reader-close p) is called.
;;
;; note: assigning the cdr of a returned pair propagates to the hashtable.
;; do NOT modify the car of any returned pair!
(define (hash-reader ht)
  (let* ((iter (hash-cursor ht))
         (%hash-reader ;; name shown when displaying the closure
           (lambda (p)
             (let ((cell (hash-cursor-next! iter)))
               (values cell (and cell #t))))))
    (make-obj-reader %hash-reader #f)))


;; create and return a closure that iterates on elements of hashtable t.
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in hashtable t and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of hashtable is reached.
(define (in-hash htable)
  (let ((iter (hash-cursor htable)))
    (lambda ()
      (let ((cell (hash-cursor-next! iter)))
        (if cell
          (values (car cell) (cdr cell) #t)
          (values #f #f #f))))))


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
(define (in-hash-cells htable)
  (let ((iter (hash-cursor htable)))
    (lambda ()
      (let ((cell (hash-cursor-next! iter)))
        (if cell
          (values cell #t)
          (values #f #f))))))


;; create and return a closure that iterates on keys of hashtable htable.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values key #t) i.e. the next key in hashtable and #t,
;; or (values #<unspecified> #f) if end of hashtable is reached.
(define (in-hash-keys htable)
  (let ((iter (hash-cursor htable)))
    (lambda ()
      (let ((cell (hash-cursor-next! iter)))
        (if cell
         (values (car cell) #t)
         (values #f #f))))))


;; create and return a closure that iterates on values of hashtable t.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values key #t) i.e. the next key in hashtable t and #t,
;; or (values #<unspecified> #f) if end of hashtable is reached.
(define (in-hash-values htable)
  (let ((iter (hash-cursor htable)))
    (lambda ()
      (let ((cell (hash-cursor-next! iter)))
        (if cell
         (values (cdr cell) #t)
         (values #f #f))))))


;; Iterate on elements of given hashtables htable, and call (proc key value) on each key and value.
;; Return unspecified value.
(define (hash-for-each htable proc)
  (assert* 'hash-for-each (procedure? proc))
  (hash-for-each-cell htable (lambda (cell) (proc (car cell) (cdr cell)))))


;; Iterate on elements of given hashtables htable, and call (proc key) on each key.
;; Return unspecified value.
(define (hash-for-each-key htable proc)
  (assert* 'hash-for-each-key (procedure? proc))
  (hash-for-each-cell htable (lambda (cell) (proc (car cell)))))


;; Iterate on elements of given hashtables htable, and call (proc pair) on each pair (key . value).
;; Return unspecified value.
;;
;; Do NOT modify the (car) of any pair!
(define (hash-for-each-cell htable proc)
  (assert* 'hash-for-each-cell (procedure? proc))
  (let ((iter (hash-cursor htable)))
    (do ((cell (hash-cursor-next! iter) (hash-cursor-next! iter)))
        ((not cell))
      (proc cell))))


;; Iterate on elements of given hashtables htable, and call (proc value) on each value.
;; Return unspecified value.
(define (hash-for-each-value htable proc)
  (assert* 'hash-for-each-value (procedure? proc))
  (hash-for-each-cell htable (lambda (cell) (proc (cdr cell)))))

;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each key and value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
(define-syntax for-hash
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((key val htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (hash-cursor htable)) ...)
              (let %for-hash ((cell (hash-cursor-next! iter)) ...)
                (when (and cell ...)
                  (let ((key (car cell)) ...
                        (val (cdr cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash (hash-cursor-next! iter) ...)))))))))))


;; Iterate in parallel on elements of given hashtables ht ...,
;; and evaluate body ... on each pair containing (key . value).
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Assigning the (cdr) of a pair propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any pair!
(define-syntax for-hash-cells
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((cell htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (hash-cursor htable)) ...)
              (let %for-hash-cells ((cell (hash-cursor-next! iter)) ...)
                (when (and cell ...)
                  (with-while-until
                    body ...
                    (%for-hash-cells (hash-cursor-next! iter) ...))))))))))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each key.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
(define-syntax for-hash-keys
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((key htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (hash-cursor htable)) ...)
              (let %for-hash-keys ((cell (hash-cursor-next! iter)) ...)
                (when (and cell ...)
                  (let ((key (car cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash-keys (hash-cursor-next! iter) ...)))))))))))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
(define-syntax for-hash-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((val htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (hash-cursor htable)) ...)
              (let %for-hash-keys ((cell (hash-cursor-next! iter)) ...)
                (when (and cell ...)
                  (let ((val (cdr cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash-keys (hash-cursor-next! iter) ...)))))))))))


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
    (for-alist ((key value l))
      (hashtable-set! dst key value))
    dst))


;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (alist->eqv-hashtable l)
  (let ((dst (make-eqv-hashtable (length l))))
    (for-alist ((key value l))
      (hashtable-set! dst key value))
    dst))


;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (length pairs)).
;
;; Returns the created hashtable.
(define (alist->hashtable hash-proc eq-proc l)
  (let ((dst (make-hashtable hash-proc eq-proc (length l))))
    (for-alist ((key value l))
      (hashtable-set! dst key value))
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


(define (display-hashtable-content htable out writer)
  (for-hash ((key val htable))
    (put-char out #\space)
    (writer key out)
    (put-char out #\space)
    (writer val out)))


;; customize how "hash-cursor" objects are printed
(record-writer (record-type-descriptor %hash-cursor)
  (lambda (iter out writer)
    (put-string out "#<hash-cursor>" out)))


;; customize how eq-hashtable objects are printed
(record-writer %eq-hashtable-rtd
  (lambda (htable out writer)
    (put-string out "(eq-hashtable")
    (display-hashtable-content htable out writer)
    (put-string out ")")))


;; customize how eqv-hashtable objects are printed
(record-writer %eqv-hashtable-rtd
  (lambda (htable out writer)
    (put-string out "(eqv-hashtable")
    (display-hashtable-content htable out writer)
    (put-string out ")")))


;; customize how hashtable objects are printed
(record-writer %gen-hashtable-rtd
  (lambda (htable out writer)
    (put-string out "(hashtable ")
    (display-procedure-name (hashtable-hash-function htable) out)
    (put-char out #\space)
    (display-procedure-name (hashtable-equivalence-function htable) out)
    (display-hashtable-content htable out writer)
    (put-string out ")")))

) ; close library
