;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


(library (scheme2k containers ordered-hash (0 9 3))
  (export
     make-eq-ordered-hash make-eqv-ordered-hash make-equal-ordered-hash make-ordered-hash

     eq-ordered-hash eqv-ordered-hash ordered-hash
     alist->eq-ordered-hash alist->eqv-ordered-hash alist->ordered-hash
     plist->eq-ordered-hash plist->eqv-ordered-hash plist->ordered-hash

     ordered-hash? ordered-hash-contains? ordered-hash-empty? ordered-hash-size ordered-hash-copy
     ordered-hash-ref ordered-hash-set! ordered-hash-delete! ordered-hash-clear!

     ordered-hash-for-each ordered-hash-cells ordered-hash-keys ordered-hash-values ordered-hash-entries

     for-ordered-hash for-ordered-hash-cells for-ordered-hash-keys for-ordered-hash-values

     ordered-hash-iterator (rename (iterator ordered-hash-iterator?))
     ordered-hash-iterator-cell ordered-hash-iterator-next!) 
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)                 fx1+ fx/ record-type-descriptor record-writer)
    (only (scheme2k bootstrap)         generate-pretty-temporaries forever with-while-until)
    (only (scheme2k containers list)   for-alist for-plist)
    (only (scheme2k containers string) display-procedure-name))


;; An ordered-hash preserves the order in which distinct keys are inserted.
;; Updating the value of an existing key does not affect iteration order.
;; Removing a key removes it from the order.
;; Reinserting a previously removed key inserts it at the end of the order.
;; All iteration procedures enumerate entries in this order.
(define-record-type (ord-hash %make-ordered-hash ordered-hash?)
  (fields
    table            ; hashtable key -> node
    (mutable head)   ; #f or node
    (mutable tail))  ; #f or node
  (nongenerative %ordered-hash-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define-record-type iterator
  (fields
    (mutable node))  ; #f or current node
  (nongenerative %ordered-hash-iterator-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define-record-type node
  (fields
    cell             ; (key . value)
    (mutable prev)   ; #f or previous node in insertion order
    (mutable next))  ; #f or next node in insertion order
  (nongenerative %ordered-hash-node-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define make-eq-ordered-hash
  (case-lambda
    (()
      (%make-ordered-hash (make-eq-hashtable) #f #f))
    ((size)
      (%make-ordered-hash (make-eq-hashtable size) #f #f))))


(define make-eqv-ordered-hash
  (case-lambda
    (()
      (%make-ordered-hash (make-eqv-hashtable) #f #f))
    ((size)
      (%make-ordered-hash (make-eqv-hashtable size) #f #f))))


(define make-equal-ordered-hash
  (case-lambda
    (()
      (%make-ordered-hash (make-hashtable equal-hash equal?) #f #f))
    ((size)
      (%make-ordered-hash (make-hashtable equal-hash equal? size) #f #f))))


(define make-ordered-hash
  (case-lambda
    ((hash equiv?)
      (%make-ordered-hash (make-hashtable hash equiv?) #f #f))
    ((hash equiv? size)
      (%make-ordered-hash (make-hashtable hash equiv? size) #f #f))))


;; (alist->eq-ordered-hash l) iterates on all (key . value) elements of list l
;; and inserts each of them into a new hashtable created with
;;   (make-eq-ordered-hash (length l)).
;;
;; Returns the new hashtable.
(define (alist->eq-ordered-hash l)
  (let ((dst (make-eq-ordered-hash (length l))))
    (for-alist ((key value l))
      (ordered-hash-set! dst key value))
    dst))


;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-ordered-hash (length l)).
;;
;; Returns the new hashtable.
(define (alist->eqv-ordered-hash l)
  (let ((dst (make-eqv-ordered-hash (length l))))
    (for-alist ((key value l))
      (ordered-hash-set! dst key value))
    dst))


;; iterate on all (key . value) elements of alist l,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (length pairs)).
;
;; Returns the created hashtable.
(define (alist->ordered-hash hash-proc eq-proc l)
  (let ((dst (make-ordered-hash hash-proc eq-proc (length l))))
    (for-alist ((key value l))
      (ordered-hash-set! dst key value))
    dst))


;; iterate on all key, value elements of plist l,
;; and inserts each of them into a new hashtable created with
;; (make-eq-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (plist->eq-ordered-hash plist)
  (let ((dst (make-eq-ordered-hash (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (ordered-hash-set! dst key value))
    dst))


;; iterate on all key, value elements of plist l,
;; and inserts each of them into a new hashtable created with
;; (make-eqv-hashtable (length l)).
;;
;; Returns the new hashtable.
(define (plist->eqv-ordered-hash plist)
  (let ((dst (make-eqv-ordered-hash (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (ordered-hash-set! dst key value))
    dst))


;; iterate on all key, value elements of plist l,
;; and inserts each of them into a new hashtable created with
;;   (make-hashtable hash-proc eq-proc (length pairs)).
;
;; Returns the created hashtable.
(define (plist->ordered-hash hash-proc eq-proc plist)
  (let ((dst (make-ordered-hash hash-proc eq-proc (fx/ (length plist) 2))))
    (for-plist ((key value plist))
      (ordered-hash-set! dst key value))
    dst))


;; iterates on all key, value elements of plist,
;; and inserts each of them into a new created ordered-hash.
;;
;; Returns the created hashtable.
(define (eq-ordered-hash . plist)
  (plist->eq-ordered-hash plist))


;; iterates on all key, value elements of plist,
;; and inserts each of them into a new created ordered-hash.
;;
;; Returns the created hashtable.
(define (eqv-ordered-hash . plist)
  (plist->eqv-ordered-hash plist))


;; iterate on all key value elements of plist,
;; and inserts each of them into a new hashtable created with
;;   (make-ordered-hash hash-proc eq-proc (fx/ (length plist) 2)).
;
;; Returns the created ordered-hash.
(define (ordered-hash hash-proc eq-proc . plist)
  (plist->ordered-hash hash-proc eq-proc plist))


;; lookup key in ordered-hash and return #t if present, or #f if not present.
;; Always O(1)
(define (ordered-hash-contains? oht key)
  (if (hashtable-ref (ord-hash-table oht) key #f)
    #t
    #f))


;; Always O(1)
(define (ordered-hash-size oht)
  (hashtable-size (ord-hash-table oht)))


;; Always O(1)
(define (ordered-hash-empty? oht)
  (fxzero? (ordered-hash-size oht)))


(define (node-key node)
  (car (node-cell node)))


(define (node-value node)
  (cdr (node-cell node)))


(define (node-value-set! node value)
  (set-cdr! (node-cell node) value))


;; lookup key in ordered-hash and return is associated value,
;; or default if not present. Always O(1)
(define (ordered-hash-ref oht key default)
  (let ((node (hashtable-ref (ord-hash-table oht) key #f)))
    (if node
        (node-value node)
        default)))


;; insert or overwrite key and value in ordered-hash.
;; Amortized O(1)
(define (ordered-hash-set! oht key value)
  (let* ((table (ord-hash-table oht))
         (node (hashtable-ref table key #f)))
    (if node
      ;; update existing (order unchanged)
      (node-value-set! node value)
      ;; new insertion
      (let ((node (make-node (cons key value) #f #f))
            (tail (ord-hash-tail oht)))
        (if tail
          (begin
            ;; append new node at the end of linked list
            (node-next-set! tail node)
            (node-prev-set! node tail)
            (ord-hash-tail-set! oht node))
          (begin
            ;; table was empty, set new node as the only element
            (ord-hash-head-set! oht node)
            (ord-hash-tail-set! oht node)))
        (hashtable-set! table key node)))))


;; delete key and its associated value from ordered-hash.
;; Always O(1)
(define (ordered-hash-delete! oht key)
  (let* ((table (ord-hash-table oht))
         (node  (hashtable-ref table key #f)))
    (when node
      (let ((prev (node-prev node))
            (next (node-next node)))
        (if prev
            (node-next-set! prev next)
            (ord-hash-head-set! oht next)) ; deleting the first node
        (if next
            (node-prev-set! next prev)
            (ord-hash-tail-set! oht prev))) ; deleting the last node
      (hashtable-delete! table key))))


(define (ordered-hash-clear! oht)
  (hashtable-clear! (ord-hash-table oht))
  (ord-hash-head-set! oht #f)
  (ord-hash-tail-set! oht #f))


;; call (proc key value) on each entry in ord-hash-table, in insertion order.
;; Mutating an ordered hashtable during iteration results in unspecified behavior.
;; Always O(n)
(define (ordered-hash-for-each oht proc)
  (let %loop ((node (ord-hash-head oht)))
    (when node
      (proc (node-key node) (node-value node))
      (%loop (node-next node)))))


;; return an iterator positioned at the first element.
(define (ordered-hash-iterator oht)
  (make-iterator (ord-hash-head oht)))


;; return hashtable element (key . val) corresponding to current position
;; of iterator, or #f if end of hashtable is reached
;;
;; setting the cdr of returned element propagates back to the hashtable,
;; i.e. it is equivalent to setting the value associated to key in the hashtable
;;
;; NEVER set or modify in any way the car of returned element!
(define (ordered-hash-iterator-cell iter)
  (let ((node (iterator-node iter)))
    (and node (node-cell node))))


;; return current ordered-hash element (key . val) if more elements are available,
;; otherwise return #f
;;
;; as a side effect, modifies iterator in place to point to next ordered-hash element.
;;
;; setting the cdr of returned element propagates back to the ordered-hash.
;;
;; NEVER set or modify in any way the car of returned element!
(define (ordered-hash-iterator-next! iter)
  (let ((node (iterator-node iter)))
    (if node
      (begin
        (iterator-node-set! iter (node-next node))
        (node-cell node))
      #f)))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each key and value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
(define-syntax for-ordered-hash
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((key val ohtable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(ohtable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(ohtable ...))))
          #'(let ((iter (ordered-hash-iterator ohtable)) ...)
              (let %for-hash ((cell (ordered-hash-iterator-next! iter)) ...)
                (when (and cell ...)
                  (let ((key (car cell)) ...
                        (val (cdr cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash (ordered-hash-iterator-next! iter) ...)))))))))))


;; Iterate in parallel on elements of given hashtables ht ...,
;; and evaluate body ... on each pair containing (key . value).
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
;;
;; Assigning the (cdr) of a pair propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any pair!
(define-syntax for-ordered-hash-cells
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((cell htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (ordered-hash-iterator htable)) ...)
              (let %for-hash-cells ((cell (ordered-hash-iterator-next! iter)) ...)
                (when (and cell ...)
                  (with-while-until
                    body ...
                    (%for-hash-cells (ordered-hash-iterator-next! iter) ...))))))))))


;; Iterate in parallel on elements of given ordered-hashs ht ..., and evaluate body ... on each key.
;; Stop iterating when the smallest ordered-hash is exhausted,
;; and return unspecified value.
(define-syntax for-ordered-hash-keys
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((key htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (ordered-hash-iterator htable)) ...)
              (let %for-hash-keys ((cell (ordered-hash-iterator-next! iter)) ...)
                (when (and cell ...)
                  (let ((key (car cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash-keys (ordered-hash-iterator-next! iter) ...)))))))))))


;; Iterate in parallel on elements of given hashtables ht ..., and evaluate body ... on each value.
;; Stop iterating when the smallest hashtable is exhausted,
;; and return unspecified value.
(define-syntax for-ordered-hash-values
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((val htable) ...) body ...)
        (with-syntax (((iter ...) (generate-pretty-temporaries #'(htable ...)))
                      ((cell ...) (generate-pretty-temporaries #'(htable ...))))
          #'(let ((iter (ordered-hash-iterator htable)) ...)
              (let %for-hash-keys ((cell (ordered-hash-iterator-next! iter)) ...)
                (when (and cell ...)
                  (let ((val (cdr cell)) ...)
                    (with-while-until
                      body ...
                      (%for-hash-keys (ordered-hash-iterator-next! iter) ...)))))))))))


;; return a freshly allocated, mutable vector containing pairs (key . val) in insertion order
;; Always O(n)
;;
;; Assigning the (cdr) of a pair propagates to the hashtable,
;; i.e. changes the value associated to key in hashtable.
;;
;; Do NOT modify the (car) of any pair!
(define ordered-hash-cells
  (case-lambda
    ((oht size)
      (let %loop ((i 0)
                  (size size)
                  (node (ord-hash-head oht))
                  (vec  (make-vector size)))
        (if (and node (fx<? i size))
          (begin
            (vector-set! vec i (node-cell node))
            (%loop (fx1+ i) size (node-next node) vec))
          vec)))
    ((oht)
      (ordered-hash-cells oht (ordered-hash-size oht)))))


;; return a freshly allocated, mutable vector containing keys in insertion order
;; Always O(n)
(define ordered-hash-keys
  (case-lambda
    ((oht size)
      (let %loop ((i 0)
                  (size size)
                  (node (ord-hash-head oht))
                  (vec  (make-vector size)))
        (if (and node (fx<? i size))
          (begin
            (vector-set! vec i (node-key node))
            (%loop (fx1+ i) size (node-next node) vec))
          vec)))
    ((oht)
      (ordered-hash-keys oht (ordered-hash-size oht)))))


;; return a freshly allocated, mutable vector containing values in insertion order
;; Always O(n)
(define ordered-hash-values
  (case-lambda
    ((oht size)
      (let %loop ((i 0)
                  (size size)
                  (node (ord-hash-head oht))
                  (vec  (make-vector size)))
        (if (and node (fx<? i size))
          (begin
            (vector-set! vec i (node-value node))
            (%loop (fx1+ i) size (node-next node) vec))
          vec)))
    ((oht)
      (ordered-hash-values oht (ordered-hash-size oht)))))


;; return two values:
;;   a freshly allocated, mutable vector containing keys in insertion order
;;   a freshly allocated, mutable vector containing values in insertion order
;; Always O(n)
(define ordered-hash-entries
  (case-lambda
    ((oht size)
      (let %loop ((i 0)
                  (size size)
                  (node (ord-hash-head oht))
                  (keys (make-vector size))
                  (vals (make-vector size)))
        (if (and node (fx<? i size))
          (let ((cell (node-cell node)))
            (vector-set! keys i (car cell))
            (vector-set! vals i (cdr cell))
            (%loop (fx1+ i) size (node-next node) keys vals))
          (values keys vals))))
    ((oht)
      (ordered-hash-entries oht (ordered-hash-size oht)))))


(define (ordered-hash-copy oht)
  ;; Create a new empty ordered-hash with the same hash/equiv
  (let* ((orig-table (ord-hash-table oht))
         (eq-proc (hashtable-equivalence-function orig-table))
         (copy (cond
                 ((eq? eq?  eq-proc)
                   (make-eq-ordered-hash))
                 ((eq? eqv? eq-proc)
                   (make-eqv-ordered-hash))
                 (else
                   (make-ordered-hash (hashtable-hash-function orig-table) eq-proc)))))
    ;; Traverse original in insertion order and copy entries
    (ordered-hash-for-each oht
      (lambda (k v)
        (ordered-hash-set! copy k v)))
    copy))


;; customize how ordered-hash objects are printed
(record-writer (record-type-descriptor ord-hash)
  (lambda (oht port writer)
    (let* ((table   (ord-hash-table oht))
           (eq-proc (hashtable-equivalence-function table)))
       (cond
         ((eq? eq?  eq-proc)
           (put-string port "(eq-ordered-hash"))
         ((eq? eqv? eq-proc)
           (put-string port "(eqv-ordered-hash"))
         (else
           (display "(ordered-hash " port)
           (display-procedure-name (hashtable-hash-function table) port)
           (put-char port #\space)
           (display-procedure-name (hashtable-equivalence-function table) port))))
    (ordered-hash-for-each oht
      (lambda (k v)
        (put-char port #\space)
        (writer k port)
        (put-char port #\space)
        (writer v port)))
    #| ( |# ; help vscode
    (put-char port #\))))


) ; close library
