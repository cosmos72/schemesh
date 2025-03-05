;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers hashtable (0 8 1))
  (export
    make-hash-iterator hash-iterator? hash-iterator-copy hash-iterator-cell hash-iterator-next!
    in-hashtable hashtable-iterate hashtable-transpose
    eq-hashtable eqv-hashtable (rename (%hashtable hashtable))
    alist->eq-hashtable alist->eqv-hashtable alist->hashtable)
  (import
    (rnrs)
    (only (chezscheme) $primitive fx1+ include record-writer)
    (only (schemesh containers list) list-iterate))


;; NOTE: (hash-table-for-each) exported by Chez Scheme at least up to version 10.0.0
;; is not suitable for implementing (hashtable-iterate) because it only works on eq-hashtable:s.


(include "containers/hashtable-types.ss")


; Note: eqv hashtables contain two inner hashtables:
; one for keys comparable with eq, and one for all other keys.
; We must retrieve both vectors from them and iterate on both.
(define-record-type
  (%hash-iterator %make-iter hash-iterator?)
  (fields
    (mutable index  iter-index  iter-index-set!)
    (mutable bucket iter-bucket iter-bucket-set!)
    (mutable vec1   iter-vec1   iter-vec1-set!)
    (mutable vec2   iter-vec2   iter-vec2-set!))
  (nongenerative #{%hash-iterator lq4zmtggul3p4izcxd4jinmdw-0})
  (sealed #t))

(define (bucket-valid? bucket)
  (or (pair? bucket) (#3%$tlc? bucket)))

(define (bucket-keyval bucket)
  (cond
    ((pair? bucket)    (car bucket))
    ((#3%$tlc? bucket) (#3%$tlc-keyval bucket))
    (else              #f)))

(define (bucket-next bucket)
  (cond
    ((pair? bucket)    (cdr bucket))
    ((#3%$tlc? bucket) (#3%$tlc-next bucket))
    (else              #f)))

; make a copy of specified hash-iterator
(define (hash-iterator-copy iter)
  (%make-iter (iter-index iter) (iter-bucket iter) (iter-vec1 iter) (iter-vec2 iter)))


; return hashtable element (key . val) corresponding to current position
; of hash-iterator, or #f if end of hashtable is reached
;
; setting the cdr of returned element propagates back to the hashtable,
; i.e. it is equivalent to setting the value associated to key in the hashtable
;
; NEVER set or modify in any way the car of returned element!
(define (hash-iterator-cell iter)
  (bucket-keyval (iter-bucket iter)))


; modify hash-iterator in place to point to next hashtable element.
; return next hashtable element (key . val) if more elements are available,
; otherwise return #f
;
; as (hash-iterator-cell), setting the cdr of returned element propagates back
; to the hashtable.
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


; return hash-iterator to first element in hashtable
(define (make-hash-iterator h)
  (if (fxzero? (hashtable-size h))
    ; hashtable is empty, return empty iterator
    (%make-iter 0 #f (vector) (vector))
    ; hashtable is not empty, seek to first bucket
    (let* ((is-eqv (eqv-ht? h))
           (vec1 (if is-eqv (ht-vec (eqv-ht-eqht h))  (ht-vec h)))
           (vec2 (if is-eqv (ht-vec (eqv-ht-genht h)) (vector)))
           (iter (%make-iter -1 #f vec1 vec2)))
      ; advance iterator to first bucket
      (hash-iterator-next! iter)
      iter)))


;; create and return a closure that iterates on elements of hashtable t.
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in hashtable t and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of hashtable is reached.
(define (in-hashtable htable)
  (let* ((iter (make-hash-iterator htable))
         (next (hash-iterator-cell iter)))
     (lambda ()
       (if (pair? next)
         (let ((cell next))
           (set! next (hash-iterator-next! iter))
           (values (car cell) (cdr cell) #t))
         (values #f #f #f)))))


;; iterate on all elements of given hashtable, and call (proc (cons key value))
;; for each element. stop iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc ...) returned truish,
;; otherwise returns #f.
;;
;; Assigning the (cdr) of an element propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any element!
(define (hashtable-iterate htable proc)
  (let ((iter (make-hash-iterator htable)))
    (do ((cell (hash-iterator-cell iter) (hash-iterator-next! iter)))
        ((not (and cell (proc cell)))
         (not cell)))))


; (hashtable-transpose src dst) iterates on all (key . value) elements of hashtable src,
; and inserts each of them into hashtable dst as transposed (value . key)
;
; Returns dst.
(define (hashtable-transpose src dst)
  (hashtable-iterate src
    (lambda (cell)
      (hashtable-set! dst (cdr cell) (car cell))))
  dst)


;; (alist->eq-hashtable l) iterates on all (key . value) elements of list l
;; and inserts each of them into a new hashtable created with
;;   (make-eq-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (alist->eq-hashtable l)
  (let ((dst (make-eq-hashtable (length l))))
    (list-iterate l
      (lambda (cell)
        (hashtable-set! dst (car cell) (cdr cell))))
    dst))


;; (alist->eqv-hashtable l) iterates on all (key . value) elements of list l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (alist->eqv-hashtable l)
  (let ((dst (make-eqv-hashtable (length l))))
    (list-iterate l
      (lambda (cell)
        (hashtable-set! dst (car cell) (cdr cell))))
    dst))


; (alist->hashtable hash-proc eq-proc l) iterates on all (key . value) elements of list l,
; and inserts each of them into a new hashtable created with
;   (make-hashtable hash-proc eq-proc (length pairs)).
;
; Returns the created hashtable.
(define (alist->hashtable hash-proc eq-proc l)
  (let ((dst (make-hashtable hash-proc eq-proc (length l))))
    (list-iterate l
      (lambda (cell)
        (hashtable-set! dst (car cell) (cdr cell))))
    dst))


;; (eq-hashtable . pairs) iterates on all (key . value) elements of pairs,
;; and inserts each of them into a new hashtable created with
;;   (make-eq-hashtable (length pairs)).
;;
;; Returns the created hashtable.
(define (eq-hashtable . pairs)
  (alist->eq-hashtable pairs))


;; (eqv-hashtable . pairs) iterates on all (key . value) elements of pairs,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length pairs)).
;;
;; Returns the created hashtable.
(define (eqv-hashtable . pairs)
  (alist->eqv-hashtable pairs))


; (hashtable hash-proc eq-proc l) iterates on all (key . value) elements of list l,
; and inserts each of them into a new hashtable created with
;   (make-hashtable hash-proc eq-proc (length pairs)).
;
; Returns the created hashtable.
(define (%hashtable hash-proc eq-proc . pairs)
  (alist->hashtable hash-proc eq-proc pairs))


; customize how "hash-iterator" objects are printed
(record-writer (record-type-descriptor %hash-iterator)
  (lambda (iter port writer)
    (display "#<hash-iterator>" port)))

) ; close library
