/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

void define_array(void) {
  eval("(define (vectory-copy! src src-start dst dst-start n)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>= i n))\n"
       "    (let ((elem (vector-ref src (fx+ i src-start))))\n"
       "      (vector-set! dst (fx+ i dst-start) elem))))\n");

  eval("(define (subvector-fill! vec start n obj)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>= i n))\n"
       "    (vector-set! vec (fx+ i start) obj))))\n");

  /** array is a resizeable vector */
  eval("(define-record-type\n"
       "  (array %make-array array?)\n"
       "  (fields\n"
       "     (mutable len  array-length %array-len-set!)\n"
       "     (mutable vec  %array-vec   %array-vec-set!)))\n");

  eval("(define (make-array n . obj)\n"
       "  (%make-array n (apply make-vector n obj)))\n");

  eval("(define (array . objs)\n"
       "  (let ((vec (apply vector objs)))\n"
       "    (%make-array (vector-length vec) vec)))\n");

  eval("(define (array-capacity arr)\n"
       "  (vector-length (%array-vec arr)))\n");

  eval("(define (array-ref arr n)\n"
       "  (assert (fx< n (array-length arr)))\n"
       "  (vector-ref (%array-vec arr) n))\n");

  eval("(define (array-set! arr n obj)\n"
       "  (assert (fx< n (array-length arr)))\n"
       "  (vector-set! (%array-vec arr) n obj))\n");

  eval("(define (array-fill! arr obj)\n"
       /* no optimized function to fill only up to array-length,
        * so fill up to array-capacity */
       "  (vector-fill! (%array-vec arr) obj))\n");

  eval("(define (subarray-fill! arr start n obj)\n"
       "  (assert (fx<= (fx+ src n) (array-length arr)))\n"
       "  (subvector-fill! (%array-vec arr) start n obj))\n");

  eval("(define (array-copy! src src-start dst dst-start n)\n"
       "  (assert (fx<= (fx+ src-start n) (array-length src)))\n"
       "  (assert (fx<= (fx+ dst-start n) (array-length dst)))\n"
       "  (vectory-copy! (%array-vec src) src-start (%array-vec dst) dst-start n))\n");

  eval("(define (array-capacity-set! arr n)\n"
       "  (assert (fx>= n (array-length arr)))\n"
       "  (unless (fx= n (array-capacity arr))\n"
       "    (let* ((len (array-length arr))\n"
       "           (old-vec (%array-vec arr))\n"
       "           (nev-vec (make-vector n)))\n"
       "      (vector-copy! old-vec 0 new-vec 0 len)\n"
       "      (%array-vec-set! arr new-vec))))\n");

  eval("(define (array-length-set! arr n)\n"
       "  (when (fx> n (array-capacity arr))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (array-capacity arr)))))\n"
       "      (array-capacity-set! arr new-cap)))\n"
       "  (%array-len-set arr n))\n");
}

void define_array_iterate(void) {

  eval("(define (array-iterate arr proc)\n"
       "  (let ((vec (%array-vec arr))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (array-capacity arr)))))\n"
       "      (array-capacity-set! arr new-cap)))\n"
       "  (%array-len-set arr n))\n");

  /** customize how "array" objects are printed */
  eval("(record-writer (record-type-descriptor array)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(array \" port)\n"
       "    (writer (job-start-func obj) port)\n"
       "    (display #\\) port)))\n");
}

void define_list_iterate(void) {
  /**
   * (list-iterate l proc) iterates on all elements of given list l,
   * and calls (proc elem) on each element. stops iterating if (proc ...) returns #f
   */
  eval("(define (list-iterate l proc)\n"
       "  (do ((tail l (cdr tail)))\n"
       "      ((or (null? tail) (not (proc (car tail)))))))\n");
}

void define_vector_iterate(void) {
  /**
   * (vector-iterate l proc) iterates on all elements of given vector v,
   * and calls (proc elem) on each element. stops iterating if (proc ...) returns #f
   */
  eval("(define (vector-iterate v proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (vector-length v)))\n"
       "      ((or (fx>= i n) (not (proc (vector-ref v i))))))))\n");
}

void define_hash_iterate(void) {
  eval("(begin\n"
       /** return hash-iterator to first element in hashtable */
       "  (define make-hash-iterator)\n"
       "  \n"
       /** return #t if argument is an hash-iterator, otherwise return #f */
       "  (define hash-iterator?)\n"
       "  \n"
       /** make a copy of specified hash-iterator */
       "  (define hash-iterator-copy)\n"
       "  \n"
       /**
        * return hashtable element (key . val) corresponding to current position
        * of hash-iterator, or #f if end of hashtable is reached
        *
        * setting the cdr of returned element propagates back to the hashtable,
        * i.e. it is equivalent to setting the value associated to key in the hashtable
        *
        * NEVER set or modify in any way the car of returned element!
        */
       "  (define hash-iterator-cell))\n"
       "  \n"
       /**
        * modify hash-iterator in place to point to next hashtable element.
        * return next hashtable element (key . val) if more elements are available,
        * otherwise return #f
        *
        * as (hash-iterator-cell), setting the cdr of returned element propagates back
        * to the hashtable.
        */
       "  (define hash-iterator-next!)\n"
       "  \n"
       /**
        * iterate on all elements of given hashtable, and call (proc (cons key value))
        * on each element. stop iterating if (proc ...) returns #f
        */
       "  (define hashtable-iterate)\n"
       "  \n"
       ")\n");

  eval("(let ()\n"
       "\n"
       ";;; hashtable-types.ss\n"
       ";;; Copyright 1984-2017 Cisco Systems, Inc.\n"
       ";;; \n"
       ";;; Licensed under the Apache License, Version 2.0 (the \"License\");\n"
       ";;; you may not use this file except in compliance with the License.\n"
       ";;; You may obtain a copy of the License at\n"
       ";;; \n"
       ";;; http://www.apache.org/licenses/LICENSE-2.0\n"
       ";;; \n"
       ";;; Unless required by applicable law or agreed to in writing, software\n"
       ";;; distributed under the License is distributed on an \"AS IS\" BASIS,\n"
       ";;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\n"
       ";;; See the License for the specific language governing permissions and\n"
       ";;; limitations under the License.\n"
       "\n"
       "(define-record-type (hashtable make-xht xht?)\n"
       "  (fields (immutable type xht-type) (immutable mutable? xht-mutable?))\n"
       "  (nongenerative #{hashtable bu811z2onf9o6tfc-0}))\n"
       "\n"
       "(define-record-type ht\n"
       "  (parent hashtable)\n"
       "  (fields (mutable vec) (mutable minlen) (mutable size))\n"
       "  (nongenerative #{ht bu811z2onf9o6tfc-6}))\n"
       "\n"
       "(define-record-type eq-ht\n"
       "  (parent ht)\n"
       "  (fields (immutable subtype)) ; eq-hashtable-subtype-{normal,weak,ephemeron}\n"
       "  (nongenerative #{eq-ht icguu8mlhm1y7ywsairxck-0})\n"
       "  (sealed #t))\n"
       "\n"
       "(define-record-type symbol-ht\n"
       "  (parent ht)\n"
       "  (fields (immutable equiv?))\n"
       "  (nongenerative #{symbol-ht bu811z2onf9o6tfc-8})\n"
       "  (sealed #t))\n"
       "\n"
       "(define-record-type gen-ht\n"
       "  (parent ht)\n"
       "  (fields (immutable hash) (immutable equiv?))\n"
       "  (nongenerative #{gen-ht bu811z2onf9o6tfc-7})\n"
       "  (sealed #t))\n"
       "\n"
       "(define-record-type eqv-ht\n"
       "  (parent hashtable)\n"
       "  (fields (immutable eqht) (immutable genht))\n"
       "  (nongenerative #{eqv-ht bu811z2onf9o6tfc-4})\n"
       "  (sealed #t))\n"
       "\n"
       ";;; \n"
       ";;; end of hashtable-types.ss\n"
       ";;; \n"
       ";;; The following code belongs to schemesh\n"
       ";;; \n"
       "\n"
       /**
        * Note: eqv hashtables contain two inner hashtables:
        * one for keys comparable with eq, and one for all other keys.
        * We must retrieve both vectors from them and iterate on both.
        */
       "(define-record-type %hash-iterator\n"
       "  (fields (mutable index) (mutable bucket) (mutable vec1) (mutable vec2))\n"
       "  (nongenerative #{%hash-iterator lq4zmtggul3p4izcxd4jinmdw-0})\n"
       "  (sealed #t))\n"
       "\n"
       "(define (hash-bucket-valid? bucket)\n"
       "  (or (pair? bucket) (#3%$tlc? bucket)))\n"
       "\n"
       "(define (hash-bucket-keyval bucket)\n"
       "  (cond\n"
       "    ((pair? bucket)    (car bucket))\n"
       "    ((#3%$tlc? bucket) (#3%$tlc-keyval bucket))\n"
       "    (else              #f)))\n"
       "\n"
       "(define (hash-bucket-next bucket)\n"
       "  (cond\n"
       "    ((pair? bucket)    (cdr bucket))\n"
       "    ((#3%$tlc? bucket) (#3%$tlc-next bucket))\n"
       "    (else              #f)))\n"
       "\n"
       "(set! hash-iterator?\n"
       "  (lambda (iter)\n"
       "    (%hash-iterator? iter)))\n"
       "\n"
       "(set! hash-iterator-copy\n"
       "  (lambda (iter)\n"
       "    (make-%hash-iterator\n"
       "      (%hash-iterator-index  iter)\n"
       "      (%hash-iterator-bucket iter)\n"
       "      (%hash-iterator-vec1   iter)\n"
       "      (%hash-iterator-vec2   iter))))\n"
       "\n"
       "(set! hash-iterator-cell\n"
       "  (lambda (iter)\n"
       "    (hash-bucket-keyval (%hash-iterator-bucket iter))))\n"
       "\n"
       "(set! make-hash-iterator\n"
       "  (lambda (h)\n"
       "    (if (fxzero? (hashtable-size h))\n"
       "      ; hashtable is empty, return empty iterator\n"
       "      (make-%hash-iterator 0 #f (vector) (vector))\n"
       "      ; hashtable is not empty, seek to first bucket\n"
       "      (let* ((is-eqv (eqv-ht? h))\n"
       "             (vec1 (if is-eqv (ht-vec (eqv-ht-eqht h))  (ht-vec h)))\n"
       "             (vec2 (if is-eqv (ht-vec (eqv-ht-genht h)) (vector)))\n"
       "             (iter (make-%hash-iterator -1 #f vec1 vec2)))\n"
       "        (hash-iterator-next! iter)\n"
       "        iter))))\n"
       "\n"
       "(set! hash-iterator-next!\n"
       "  (lambda (iter)\n"
       "    (let* ((index  (%hash-iterator-index  iter))\n"
       "           (bucket (%hash-iterator-bucket iter))\n"
       "           (vec1   (%hash-iterator-vec1   iter))\n"
       "           (vlen   (vector-length vec1)))\n"
       "      (set! bucket (hash-bucket-next bucket))\n"
       "      \n"
       "      ; iterate on vec1 until we find a cell\n"
       "      (do ()\n"
       "        ((or (hash-bucket-valid? bucket) (fx>=? index vlen)))\n"
       "        (set! index (fx1+ index))\n"
       "        (when (fx<? index vlen)\n"
       "          (set! bucket (vector-ref vec1 index))))\n"
       "      (%hash-iterator-index-set!  iter index)\n"
       "      (%hash-iterator-bucket-set! iter bucket)\n"
       "      \n"
       "      (let ((keyval (hash-bucket-keyval  bucket))\n"
       "            (vec2   (%hash-iterator-vec2 iter)))\n"
       "        (if (or keyval (fxzero? (vector-length vec2)))\n"
       "          ; either we found a cell, or vec2 is empty and we reached end of vec1\n"
       "          keyval\n"
       "          ; no cell found, but vec2 is non-empty: switch to it and retry\n"
       "          (begin\n"
       "            (%hash-iterator-index-set!  iter -1)\n"
       "            (%hash-iterator-bucket-set! iter #f)\n"
       "            (%hash-iterator-vec1-set!   iter vec2)\n"
       "            (%hash-iterator-vec2-set!   iter (vector))\n"
       "            (hash-iterator-next! iter)))))))\n"
       "\n"
       "(set! hashtable-iterate\n"
       "  (lambda (htable proc)\n"
       "    (let ((iter (make-hash-iterator htable)))\n"
       "      (do ((cell (hash-iterator-cell iter) (hash-iterator-next! iter)))\n"
       "          ((or (not cell) (not (proc cell))))))))\n"
       "\n"
       ")\n");
}
