/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <string.h> /* memmove() */

#include "eval.h"

/** C backend implementation of (vectory-copy!) */
static void c_vector_copy(ptr src, iptr src_start, ptr dst, iptr dst_start, iptr n) {
#if 0 /* redundant, already checked by Scheme function (vector-copy!) */
  if (Svectorp(src) && Svectorp(dst) && src_start >= 0 && dst_start >= 0 && n > 0 &&
      src_start <= Svector_length(src) && dst_start <= Svector_length(dst) &&
      n <= Svector_length(src) - src_start && n <= Svector_length(dst) - dst_start)
#endif
  {

    ptr* src_ptr = &Svector_ref(src, src_start);
    ptr* dst_ptr = &Svector_ref(dst, dst_start);
    if (src_ptr != dst_ptr) {
      memmove(dst_ptr, src_ptr, n * sizeof(ptr));
    }
  }
}

static void define_library_containers_misc(void) {

#define SCHEMESH_LIBRARY_CONTAINERS_MISC_EXPORT                                                    \
  "list-iterate vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable "       \
  "list->bytevector bytevector-utf8-ref bytevector-utf8-set! char->utf8-length "                   \
  "subbytevector bytevector-fill-range! bytevector-iterate string-fill-range! "

  Sregister_symbol("c_vector_copy", &c_vector_copy);

  eval("(library (schemesh containers misc (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_MISC_EXPORT ")\n"
       "  (import\n"
       "    (rename (rnrs)\n"
       "      (fxarithmetic-shift-left  fxshl)\n"
       "      (fxarithmetic-shift-right fxshr))\n"
       "    (rnrs mutable-strings)\n"
       "    (only (chezscheme) bytevector foreign-procedure fx1+))\n"
       "\n"
       /**
        * (list-iterate l proc) iterates on all elements of given list l,
        * and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
        */
       "(define (list-iterate l proc)\n"
       "  (do ((tail l (cdr tail)))\n"
       "      ((or (null? tail) (not (proc (car tail)))))))\n"
       "\n"
       /**
        * copy a portion of vector src into dst.
        * works even if src are the same vector and the two ranges overlap.
        */
       "(define vector-copy!\n"
       "  (let ((c-vector-copy (foreign-procedure \"c_vector_copy\"\n"
       "     (scheme-object fixnum scheme-object fixnum fixnum)"
       "     void)))\n"
       "    (lambda (src src-start dst dst-start n)\n"
       "      (if (fx=? n 1)\n"
       "        (vector-set! dst dst-start (vector-ref src src-start))\n"
       "        (begin\n"
       "          (assert (and (vector? src) (vector? dst)))\n"
       "          (assert (and (fixnum? src-start) (fixnum? dst-start) (fixnum? n)))\n"
       "          (assert (and (fx>=? src-start 0) (fx>=? dst-start 0) (fx>=? n 0)))\n"
       "          (assert (fx<=? src-start (vector-length src)))\n"
       "          (assert (fx<=? dst-start (vector-length dst)))\n"
       "          (assert (fx<=? n (fx- (vector-length src) src-start)))\n"
       "          (assert (fx<=? n (fx- (vector-length dst) dst-start)))\n"
       "          (if (fx>=? n 2)\n"
       "            (c-vector-copy src src-start dst dst-start n))\n"
       "            (assert (fx>=? n 0)))))))\n"
       "\n"
       /**
        * return a copy of vector vec containing only elements
        * from start (inclusive) to end (exclusive)
        */
       "(define (subvector vec start end)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? end 0))\n"
       "  (assert (fx<=? start end))\n"
       "  (let* ((n (fx- end start))\n"
       "         (dst (make-vector n)))\n"
       "    (vector-copy! vec start dst 0 n)\n"
       "    dst))\n"
       "\n"
       /**
        * set n elements of vector from offset = start with specified value
        */
       "(define (vector-fill-range! vec start n val)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (vector-set! vec (fx+ i start) val)))\n"
       "\n"
       /**
        * (vector-iterate l proc) iterates on all elements of given vector vec,
        * and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
        */
       "(define (vector-iterate vec proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (vector-length vec)))\n"
       "      ((or (fx>=? i n) (not (proc i (vector-ref vec i)))))))\n"
       "\n"
       /**
        * (vector->hashtable vec htable) iterates on all elements of given vector vec,
        * which must be cons cells, and inserts them into hashtable htable:
        * (car cell) is used as key, and (cdr cell) is used ad value.
        *
        * Returns htable.
        */
       "(define (vector->hashtable vec htable)\n"
       "  (vector-iterate vec\n"
       "    (lambda (i cell)\n"
       "      (hashtable-set! htable (car cell) (cdr cell))))\n"
       "  htable)\n"
       "\n"
       /****************************************************************************/
       /************  define some additional bytevector functions  *****************/
       /****************************************************************************/
       "(define (list->bytevector l)\n"
       "  (apply bytevector l))\n"
       "\n"
       /**
        * interpret two bytes as UTF-8 sequence and return corresponding char.
        * b0 is assumed to be in the range #xc0 <= b0 < #xe0
        */
       "(define (utf8-pair->char b0 b1)\n"
       "  (if (fx=? #x80 (fxand #xc0 b1))\n" /* is b1 valid continuation byte ? */
       "    (let ((n (fxior\n"
       "               (fxshl (fxand #x1f b0) 6)\n"
       "               (fxshl (fxand #x3f b1) 0))))\n"
       "      (if (fx<=? #x80 n #x7ff)\n"
       "        (integer->char n)\n"
       "        #f))\n" /* overlong UTF-8 sequence */
       "    #f))"       /* invalid continuation byte b1 */
       "\n"
       /**
        * interpret three bytes as UTF-8 sequence and return corresponding char.
        * b0 is assumed to be in the range #xe0 <= b0 < #xf0
        */
       "(define (utf8-triplet->char b0 b1 b2)\n"
       "  (if (and (fx=? #x80 (fxand #xc0 b1))\n"  /* is b1 valid continuation byte ? */
       "           (fx=? #x80 (fxand #xc0 b2)))\n" /* is b2 valid continuation byte ? */
       "    (let ((n (fxior\n"
       "               (fxshl (fxand #x0f b0) 12)\n"
       "               (fxshl (fxand #x3f b1)  6)\n"
       "               (fxshl (fxand #x3f b2)  0))))\n"
       "      (if (or (fx<=? #x800 n #xd7ff) (fx<=? #xe000 n #xffff))\n"
       "        (integer->char n)\n"
       "        #f))\n" /* surrogate half, or overlong UTF-8 sequence */
       "    #f))"       /* invalid continuation byte b0 or b1 */
       "\n"
       /**
        * interpret four bytes as UTF-8 sequence and return corresponding char.
        * b0 is assumed to be in the range #xf0 <= b0 < #xf5
        */
       "(define (utf8-quadruplet->char b0 b1 b2 b3)\n"
       "  (if (and (fx=? #x80 (fxand #xc0 b1))\n"  /* is b1 valid continuation byte ? */
       "           (fx=? #x80 (fxand #xc0 b2))\n"  /* is b2 valid continuation byte ? */
       "           (fx=? #x80 (fxand #xc0 b3)))\n" /* is b3 valid continuation byte ? */
       "    (let ((n (fxior\n"
       "               (fxshl (fxand #x07 b0) 18)\n"
       "               (fxshl (fxand #x3f b1) 12)\n"
       "               (fxshl (fxand #x3f b2)  6)\n"
       "               (fxshl (fxand #x3f b3)  0))))\n"
       "      (if (fx<=? #x10000 n #x10ffff)\n"
       "        (integer->char n)\n"
       "        #f))\n" /* overlong UTF-8 sequence, or beyond #x10ffff */
       "    #f))"       /* invalid continuation byte b0, b1 or b2 */
       "\n"
       /**
        * read up to max-n bytes from bytevector at offset start, interpret
        * them as UTF-8 sequence and convert them to the corresponding char.
        *
        * Returns two values: converted char, and length in bytes of UTF-8 sequence.
        * If UTF-8 sequence is incomplete, return #t instead of converted char.
        * If UTF-8 sequence is invalid, return #f instead of converted char.
        */
       "(define (bytevector-utf8-ref vec start max-n)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? max-n 0))\n"
       "  (assert (fx<=? start (bytevector-length vec)))\n"
       "  (let* ((len (bytevector-length vec))\n"
       "         (max-n (fxmin max-n (fx- len start)))\n"
       "         (b0  (if (fx>? max-n 0) (bytevector-u8-ref vec start) -1)))\n"
       "    (cond\n"
       "      ((fx<? b0    0) (values #t 0))\n" /* 0 bytes available */
       "      ((fx<? b0 #x80) (values (integer->char b0) 1))\n"
       "      ((fx<? b0 #xc0) (values #f 1))\n"
       "      ((fx<? b0 #xe0)\n"
       "        (if (fx>? max-n 1)\n"
       "          (let ((b1 (bytevector-u8-ref vec (fx1+ start))))\n"
       "            (values (utf8-pair->char b0 b1) 2))\n"
       "          (values #t 1)))\n" /* < 2 bytes available */
       "      ((fx<? b0 #xf0)\n"
       "        (if (fx>? max-n 2)\n"
       "          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))\n"
       "                (b2 (bytevector-u8-ref vec (fx+ 2 start))))\n"
       "            (values (utf8-triplet->char b0 b1 b2) 3))\n"
       "          (values #t (fxmin 2 max-n))))\n" /* < 3 bytes available */
       "      ((fx<? b0 #xf5)\n"
       "        (if (fx>? max-n 3)\n"
       "          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))\n"
       "                (b2 (bytevector-u8-ref vec (fx+ 2 start)))\n"
       "                (b3 (bytevector-u8-ref vec (fx+ 3 start))))\n"
       "            (values (utf8-quadruplet->char b0 b1 b2 b3) 4))\n"
       "          (values #t (fxmin 3 max-n))))\n" /* < 4 bytes available */
       "      (#t (values #f 1)))))\n"
       "\n"
       /**
        * convert char to 2-byte UTF-8 sequence and return two values: the two converted bytes.
        * ch is assumed to be in the range #x80 <= ch < #x800
        */
       "(define (char->utf8-pair ch)\n"
       "  (let ((n (char->integer ch)))\n"
       "    (values\n"
       "      (fxior #xc0 (fxand #x3f (fxshr n 6)))\n"
       "      (fxior #x80 (fxand #x3f n)))))\n"
       "\n"
       /**
        * convert char to 3-byte UTF-8 sequence and return three values: the three converted bytes.
        * ch is assumed to be in the range #x800 <= ch < #x1000
        */
       "(define (char->utf8-triplet ch)\n"
       "  (let ((n (char->integer ch)))\n"
       "    (values\n"
       "      (fxior #xe0 (fxand #x0f (fxshr n 12)))\n"
       "      (fxior #x80 (fxand #x3f (fxshr n 6)))\n"
       "      (fxior #x80 (fxand #x3f n)))))\n"
       "\n"
       /**
        * convert char to 4-byte UTF-8 sequence and return four values: the four converted bytes.
        * ch is assumed to be in the range #x1000 <= ch < #x110000
        */
       "(define (char->utf8-quadruplet ch)\n"
       "  (let ((n (char->integer ch)))\n"
       "    (values\n"
       "      (fxior #xf0 (fxand #x07 (fxshr n 18)))\n"
       "      (fxior #x80 (fxand #x3f (fxshr n 12)))\n"
       "      (fxior #x80 (fxand #x3f (fxshr n 6)))\n"
       "      (fxior #x80 (fxand #x3f n)))))\n"
       "\n"
       /**
        * convert a char to UTF-8 sequence and write it into given bytevector.
        * Returns one value: the length in bytes of UTF-8 sequence.
        * Raises condition if (fx- (bytevector-length) start) is smaller
        * than length in bytes of UTF-8 sequence
        */
       "(define (bytevector-utf8-set! vec start ch)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx<?  start (bytevector-length vec)))\n"
       "  (let ((n (char->integer ch)))\n"
       "    (cond\n"
       "      ((fx<? n 0) 0)\n" /* should not happen */
       "      ((fx<? n #x80)\n"
       "        (bytevector-u8-set! vec start n)\n"
       "        1)\n"
       "      ((fx<? n #x800)\n"
       "        (let-values (((b0 b1) (char->utf8-pair ch)))\n"
       "          (bytevector-u8-set! vec start b0)\n"
       "          (bytevector-u8-set! vec (fx1+ start) b1))\n"
       "        2)\n"
       "      ((fx<? n #x10000)\n"
       "        (let-values (((b0 b1 b2) (char->utf8-triplet ch)))\n"
       "          (bytevector-u8-set! vec start b0)\n"
       "          (bytevector-u8-set! vec (fx+ 1 start) b1)\n"
       "          (bytevector-u8-set! vec (fx+ 2 start) b2))\n"
       "        3)\n"
       "      ((fx<? n #x110000)\n"
       "        (let-values (((b0 b1 b2 b3) (char->utf8-quadruplet ch)))\n"
       "          (bytevector-u8-set! vec start b0)\n"
       "          (bytevector-u8-set! vec (fx+ 1 start) b1)\n"
       "          (bytevector-u8-set! vec (fx+ 2 start) b2)\n"
       "          (bytevector-u8-set! vec (fx+ 3 start) b3))\n"
       "        4)\n"
       "      (#t 0))))\n" /* should not happen */
       "\n"
       /* convert a char to UTF-8 sequence and return the length in bytes of UTF-8 sequence. */
       "(define (char->utf8-length ch)\n"
       "  (let ((n (char->integer ch)))\n"
       "    (cond\n"
       "      ((fx<? n   0) 0)\n" /* should not happen */
       "      ((fx<? n #x80) 1)\n"
       "      ((fx<? n #x800) 2)\n"
       "      ((fx<? n #x10000) 3)\n"
       "      ((fx<? n #x110000) 4)\n"
       "      (#t 0))))\n" /* should not happen */
       "\n"
       /**
        * return a copy of bytevector vec containing only elements
        * from start (inclusive) to end (exclusive)
        */
       "(define (subbytevector vec start end)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? end 0))\n"
       "  (assert (fx<=? start end))\n"
       "  (let* ((n (fx- end start))\n"
       "         (dst (make-bytevector n)))\n"
       "    (bytevector-copy! vec start dst 0 n)\n"
       "    dst))\n"
       "\n"
       "(define (bytevector-fill-range! bvec start n val)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (bytevector-u8-set! bvec (fx+ i start) val)))\n"
       "\n"
       /**
        * (bytevector-iterate l proc) iterates on all elements of given bytevector vec,
        * and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
        */
       "(define (bytevector-iterate bvec proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (bytevector-length bvec)))\n"
       "      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i)))))))\n"
       "\n"
       /**
        * set n elements of string from offset = start with specified value
        */
       "(define (string-fill-range! str start n val)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (string-set! str (fx+ i start) val)))\n"
       "\n"
       ")\n"); /* close library */
}

/****************************************************************************/
/* define Scheme type "hash-iterator" and functions operating on it:        */
/* they implement traversing hashtable contents without allocating          */
/****************************************************************************/
static void define_library_containers_hashtable(void) {

#define SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT_1                                             \
  "make-hash-iterator hash-iterator? hash-iterator-copy hash-iterator-cell hash-iterator-next! "   \
  "hashtable-iterate hashtable-transpose eq-hashtable eqv-hashtable "
#define SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT_2 "(rename (%hashtable hashtable))"

#define SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT                                               \
  SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT_1 "hashtable "

  eval("(library (schemesh containers hashtable (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT_1 /*                             */
           /*    */ SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT_2 ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) $primitive fx1+ record-writer)\n"
       "    (schemesh containers misc))\n"
       "\n"
       /* =================================================================== */
       "; start of hashtable-types.ss: the following code belongs to Chez Scheme\n"
       /* =================================================================== */
       ";\n"
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
       /* =================================================================== */
       "; end of hashtable-types.ss: the following code no longer belongs to Chez Scheme\n"
       /* =================================================================== */
       "\n"
       /**
        * Note: eqv hashtables contain two inner hashtables:
        * one for keys comparable with eq, and one for all other keys.
        * We must retrieve both vectors from them and iterate on both.
        */
       "(define-record-type\n"
       "  (%hash-iterator %make-iter hash-iterator?)\n"
       "  (fields\n"
       "    (mutable index  iter-index  iter-index-set!)\n"
       "    (mutable bucket iter-bucket iter-bucket-set!)\n"
       "    (mutable vec1   iter-vec1   iter-vec1-set!)\n"
       "    (mutable vec2   iter-vec2   iter-vec2-set!))\n"
       "  (nongenerative #{%hash-iterator lq4zmtggul3p4izcxd4jinmdw-0})\n"
       "  (sealed #t))\n"
       "\n"
       "(define (bucket-valid? bucket)\n"
       "  (or (pair? bucket) (#3%$tlc? bucket)))\n"
       "\n"
       "(define (bucket-keyval bucket)\n"
       "  (cond\n"
       "    ((pair? bucket)    (car bucket))\n"
       "    ((#3%$tlc? bucket) (#3%$tlc-keyval bucket))\n"
       "    (else              #f)))\n"
       "\n"
       "(define (bucket-next bucket)\n"
       "  (cond\n"
       "    ((pair? bucket)    (cdr bucket))\n"
       "    ((#3%$tlc? bucket) (#3%$tlc-next bucket))\n"
       "    (else              #f)))\n"
       "\n"
       /** make a copy of specified hash-iterator */
       "(define (hash-iterator-copy iter)\n"
       "  (%make-iter (iter-index iter) (iter-bucket iter) (iter-vec1 iter) (iter-vec2 iter)))\n"
       "\n"
       /**
        * return hashtable element (key . val) corresponding to current position
        * of hash-iterator, or #f if end of hashtable is reached
        *
        * setting the cdr of returned element propagates back to the hashtable,
        * i.e. it is equivalent to setting the value associated to key in the hashtable
        *
        * NEVER set or modify in any way the car of returned element!
        */
       "(define (hash-iterator-cell iter)\n"
       "  (bucket-keyval (iter-bucket iter)))\n"
       "\n"
       /**
        * modify hash-iterator in place to point to next hashtable element.
        * return next hashtable element (key . val) if more elements are available,
        * otherwise return #f
        *
        * as (hash-iterator-cell), setting the cdr of returned element propagates back
        * to the hashtable.
        */
       "(define (hash-iterator-next! iter)\n"
       "  (let* ((index  (iter-index  iter))\n"
       "         (bucket (bucket-next (iter-bucket iter)))\n"
       "         (vec1   (iter-vec1   iter))\n"
       "         (vlen   (vector-length vec1)))\n"
       "    \n"
       /*   ; iterate on vec1 until we find a cell */
       "    (do ()\n"
       "      ((or (bucket-valid? bucket) (fx>=? index vlen)))\n"
       "      (set! index (fx1+ index))\n"
       "      (when (fx<? index vlen)\n"
       "        (set! bucket (vector-ref vec1 index))))\n"
       "    (iter-index-set!  iter index)\n"
       "    (iter-bucket-set! iter bucket)\n"
       "    \n"
       "    (let ((vec2   (iter-vec2 iter)))\n"
       "      (if (or (bucket-valid? bucket) (fxzero? (vector-length vec2)))\n"
       /*       ; either we found a cell, or vec2 is empty and we reached end of vec1 */
       "        (bucket-keyval  bucket)\n"
       /*       ; no cell found, but vec2 is non-empty: switch to it and retry */
       "        (begin\n"
       "          (iter-index-set!  iter -1)\n"
       "          (iter-bucket-set! iter #f)\n"
       "          (iter-vec1-set!   iter vec2)\n"
       "          (iter-vec2-set!   iter (vector))\n"
       "          (hash-iterator-next! iter))))))\n"
       "\n"
       /** return hash-iterator to first element in hashtable */
       "(define (make-hash-iterator h)\n"
       "  (if (fxzero? (hashtable-size h))\n"
       /*   ; hashtable is empty, return empty iterator */
       "    (%make-iter 0 #f (vector) (vector))\n"
       /*   ; hashtable is not empty, seek to first bucket */
       "    (let* ((is-eqv (eqv-ht? h))\n"
       "           (vec1 (if is-eqv (ht-vec (eqv-ht-eqht h))  (ht-vec h)))\n"
       "           (vec2 (if is-eqv (ht-vec (eqv-ht-genht h)) (vector)))\n"
       "           (iter (%make-iter -1 #f vec1 vec2)))\n"
       /*     ; advance iterator to first bucket */
       "      (hash-iterator-next! iter)\n"
       "      iter)))\n"
       "\n"
       /**
        * iterate on all elements of given hashtable, and call (proc (cons key value))
        * for each element. stop iterating if (proc ...) returns #f
        *
        * Assigning the (cdr) of an element propagates to the hashtable,
        * i.e. changes the value associated to key in hashtable.
        *
        * Do NOT modify the (car) of any element!
        */
       "(define (hashtable-iterate htable proc)\n"
       "  (let ((iter (make-hash-iterator htable)))\n"
       "    (do ((cell (hash-iterator-cell iter) (hash-iterator-next! iter)))\n"
       "        ((or (not cell) (not (proc cell)))))))\n"
       "\n"
       /**
        * (hashtable-transpose src dst) iterates on all (key . value) elements of hashtable src,
        * and inserts each of them into hashtable dst as transposed (value . key)
        *
        * Returns dst.
        */
       "(define (hashtable-transpose src dst)\n"
       "  (hashtable-iterate src\n"
       "    (lambda (cell)\n"
       "      (hashtable-set! dst (cdr cell) (car cell))))\n"
       "  dst)\n"
       "\n"
       /**
        * (eq-hashtable . pairs) iterates on all (key . value) elements of pairs,
        * and inserts each of them into a new hashtable created with (make-eq-hashtable (length
        * pairs)).
        *
        * Returns the new hashtable.
        */
       "(define (eq-hashtable . pairs)\n"
       "  (let ((dst (make-eq-hashtable (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n"
       "\n"
       /**
        * (eqv-hashtable . pairs) iterates on all (key . value) elements of pairs,
        * and inserts each of them into a new hashtable created with (make-eqv-hashtable (length
        * pairs)).
        *
        * Returns the new hashtable.
        */
       "(define (eqv-hashtable . pairs)\n"
       "  (let ((dst (make-eqv-hashtable (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n"
       "\n"
       /**
        * (hashtable hash-proc eq-proc . pairs) iterates on all (key . value) elements of pairs,
        * and inserts each of them into a new hashtable created with
        *   (make-hashtable hash-proc eq-proc (length pairs)).
        *
        * Returns the new hashtable.
        */
       "(define (%hashtable hash-proc eq-proc . pairs)\n"
       "  (let ((dst (make-hashtable hash-proc eq-proc (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n"
       "\n"
       /** customize how "hash-iterator" objects are printed */
       "(record-writer (record-type-descriptor %hash-iterator)\n"
       "  (lambda (iter port writer)\n"
       "    (display \"#<hash-iterator>\" port)))\n"
       "\n"
       ")\n"); /* close library */
}

/** define Scheme type "span", a resizeable vector */
static void define_library_containers_span(void) {

#define SCHEMESH_LIBRARY_CONTAINERS_SPAN_EXPORT                                                    \
  "list->span vector->span vector->span* make-span span->vector span span? "                       \
  "span-length span-empty? span-clear! span-capacity span-capacity-front span-capacity-back "      \
  "span-ref span-back span-set! span-fill! span-fill-range! span-copy span-copy! "                 \
  "span-reserve-front! span-reserve-back! span-resize-front! span-resize-back! "                   \
  "span-insert-front! span-insert-back! span-sp-insert-front! span-sp-insert-back! "               \
  "span-erase-front! span-erase-back! span-iterate span-find "                                     \
  "span-peek-beg span-peek-end span-peek-data "

  eval("(library (schemesh containers span (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_SPAN_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) fx1+ fx1- record-writer vector-copy void)\n"
       "    (schemesh containers misc))\n"
       "\n"
       "(define-record-type\n"
       "  (%span %make-span span?)\n"
       "  (fields\n"
       "     (mutable beg span-beg span-beg-set!)\n"
       "     (mutable end span-end span-end-set!)\n"
       "     (mutable vec span-vec span-vec-set!))\n"
       "  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))\n"
       "\n"
       "(define (span-peek-beg sp)\n"
       "  (span-beg sp))\n"
       "\n"
       "(define (span-peek-end sp)\n"
       "  (span-end sp))\n"
       "\n"
       "(define (span-peek-data sp)\n"
       "  (span-vec sp))\n"
       "\n"
       "(define (list->span l)\n"
       "  (let ((vec (list->vector l)))\n"
       "    (%make-span 0 (vector-length vec) vec)))\n"
       "\n"
       /* create span copying contents of specified vector */
       "(define (vector->span vec)\n"
       "  (%make-span 0 (vector-length vec) (vector-copy vec)))\n"
       "\n"
       /* view existing vector as span */
       "(define (vector->span* vec)\n"
       "  (%make-span 0 (vector-length vec) vec))\n"
       "\n"
       "(define (make-span n . val)\n"
       "  (%make-span 0 n (apply make-vector n val)))\n"
       "\n"
       "(define (span->vector sp)\n"
       "  (subvector (span-vec sp) (span-beg sp) (span-end sp)))\n"
       "\n"
       "(define (span . vals)\n"
       "  (list->span vals))\n"
       "\n"
       "(define (span-length sp)\n"
       "  (fx- (span-end sp) (span-beg sp)))\n"
       "\n"
       /* return length of internal vector, i.e. maximum number of elements
        * that can be stored without reallocating */
       "(define (span-capacity sp)\n"
       "  (vector-length (span-vec sp)))\n"
       "\n"
       "(define (span-empty? sp)\n"
       "  (fx>=? (span-beg sp) (span-end sp)))\n"
       "\n"
       "(define (span-clear! sp)\n"
       "  (span-beg-set! sp 0)\n"
       "  (span-end-set! sp 0))\n"
       "\n"
       "(define (span-ref sp idx)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (span-length sp)))\n"
       "  (vector-ref (span-vec sp) (fx+ idx (span-beg sp))))\n"
       "\n"
       "(define (span-back sp)\n"
       "  (assert (not (span-empty? sp)))\n"
       "  (vector-ref (span-vec sp) (fx1- (span-end sp))))\n"
       "\n"
       "(define (span-set! sp idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (span-length sp)))\n"
       "  (vector-set! (span-vec sp) (fx+ idx (span-beg sp)) val))\n"
       "\n"
       "(define (span-fill! sp val)\n"
       "  (vector-fill-range! (span-vec sp) (span-beg sp) (span-length sp) val))\n"
       "\n"
       "(define (span-fill-range! sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (span-length sp)))\n"
       "  (vector-fill-range! (span-vec sp) (fx+ start (span-beg sp)) n val))\n"
       "\n"
       "(define (span-copy src)\n"
       "  (let* ((n (span-length src))\n"
       "         (dst (make-span n)))\n"
       "    (vector-copy! (span-vec src) (span-beg src)\n"
       "                  (span-vec dst) (span-beg dst) n)\n"
       "    dst))\n"
       "\n"
       "(define (span-copy! src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (span-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (span-length dst)))\n"
       "  (vector-copy! (span-vec src) (fx+ src-start (span-beg src))\n"
       "                (span-vec dst) (fx+ dst-start (span-beg dst)) n))\n"
       "\n"
       "(define (span-reallocate-front! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (span-length sp)))\n"
       "        (old-vec (span-vec sp))\n"
       "        (new-vec (make-vector cap))\n"
       "        (new-beg (fx- cap len)))\n"
       "    (vector-copy! old-vec (span-beg sp) new-vec new-beg copy-len)\n"
       "    (span-beg-set! sp new-beg)\n"
       "    (span-end-set! sp cap)\n"
       "    (span-vec-set! sp new-vec)))\n"
       "\n"
       "(define (span-reallocate-back! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (span-length sp)))\n"
       "        (old-vec (span-vec sp))\n"
       "        (new-vec (make-vector cap)))\n"
       "    (vector-copy! old-vec (span-beg sp) new-vec 0 copy-len)\n"
       "    (span-beg-set! sp 0)\n"
       "    (span-end-set! sp len)\n"
       "    (span-vec-set! sp new-vec)))\n"
       "\n"
       /* return distance between begin of internal vector and last element */
       "(define (span-capacity-front sp)\n"
       "  (span-end sp))\n"
       "\n"
       /* return distance between first element and end of internal vector */
       "(define (span-capacity-back sp)\n"
       "  (fx- (vector-length (span-vec sp)) (span-beg sp)))\n"
       "\n"
       /* ensure distance between begin of internal vector and last element is >= n.
        * does NOT change the length */
       "(define (span-reserve-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (span-vec sp))\n"
       "        (cap-front (span-capacity-front sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (vector-length vec))\n"
       /*       vector is large enough, move elements to the back */
       "        (let* ((cap (span-capacity sp))\n"
       "               (old-len (span-length sp))\n"
       "               (new-beg (fx- cap old-len)))\n"
       "          (vector-copy! vec (span-beg sp) vec new-beg old-len)\n"
       "          (span-beg-set! sp new-beg)\n"
       "          (span-end-set! sp cap)))\n"
       "      (#t\n"
       /*       vector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))\n"
       "         (span-reallocate-front! sp (span-length sp) new-cap))))))\n"
       "\n"
       /* ensure distance between first element and end of internal vector is >= n.
        * does NOT change the length */
       "(define (span-reserve-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (span-vec sp))\n"
       "        (cap-back (span-capacity-back sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (vector-length vec))\n"
       /*       vector is large enough, move elements to the front */
       "        (let ((len (span-length sp)))\n"
       "          (vector-copy! vec (span-beg sp) vec 0 len)\n"
       "          (span-beg-set! sp 0)\n"
       "          (span-end-set! sp len)))\n"
       "      (#t\n"
       /*       vector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "         (span-reallocate-back! sp (span-length sp) new-cap))))))\n"
       "\n"
       /* grow or shrink span on the left (front), set length to n */
       "(define (span-resize-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-front! sp len)\n"
       "  (assert (fx>=? (span-capacity-front sp) len))\n"
       "  (span-beg-set! sp (fx- (span-end sp) len)))\n"
       "\n"
       /* grow or shrink span on the right (back), set length to n */
       "(define (span-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-back! sp len)\n"
       "  (assert (fx>=? (span-capacity-back sp) len))\n"
       "  (span-end-set! sp (fx+ len (span-beg sp))))\n"
       "\n"
       "(define (span-insert-front! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((pos 0)\n"
       "          (new-len (fx+ (span-length sp) (length vals))))\n"
       "      (span-resize-front! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (span-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       "(define (span-insert-back! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let* ((pos (span-length sp))\n"
       "           (new-len (fx+ pos (length vals))))\n"
       "      (span-resize-back! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (span-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       /* prefix a portion of another span to this span */
       "(define (span-sp-insert-front! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((len (span-length sp-dst)))\n"
       "      (span-resize-front! sp-dst (fx+ len src-n))\n"
       "      (span-copy! sp-src src-start sp-dst 0 src-n))))\n"
       "\n"
       /* append a portion of another span to this span */
       "(define (span-sp-insert-back! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (span-length sp-dst)))\n"
       "      (span-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (span-copy! sp-src src-start sp-dst pos src-n))))\n"
       "\n"
       /* erase n elements at the left (front) of span */
       "(define (span-erase-front! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (span-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (span-beg-set! sp (fx+ n (span-beg sp)))))\n"
       "\n"
       /* erase n elements at the right (back) of span */
       "(define (span-erase-back! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (span-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (span-end-set! sp (fx- (span-end sp) n))))\n"
       "\n"
       "(define (span-iterate sp proc)\n"
       "  (do ((i (span-beg sp) (fx1+ i))\n"
       "       (n (span-end sp))\n"
       "       (v (span-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (vector-ref v i)))))))\n"
       "\n"
       /**
        * (span-find) iterates on span elements from start to (fxmin (fx+ start n) (span-length
        * sp)), and returns the index of first span element that causes (predicate elem) to return
        * non-#f. Returns #f if no such element is found.
        */
       "(define (span-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (span-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (span-ref sp i))\n"
       "        (set! ret i)))))\n"
       "\n"
       /** customize how "span" objects are printed */
       "(record-writer (record-type-descriptor %span)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(span\" port)\n"
       "    (span-iterate sp"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
};

/** define Scheme type "bytespan", a resizeable bytevector */
static void define_library_containers_bytespan(void) {

#define SCHEMESH_LIBRARY_CONTAINERS_BYTESPAN_EXPORT                                                \
  "list->bytespan bytevector->bytespan bytevector->bytespan* make-bytespan bytespan->bytevector "  \
  "bytespan bytespan? bytespan-length bytespan-empty? bytespan-clear! "                            \
  "bytespan-capacity bytespan-capacity-front bytespan-capacity-back "                              \
  "bytespan-u8-ref bytespan-u8-back bytespan-u8-set! bytespan-fill! bytespan-fill-range! "         \
  "bytespan-copy bytespan-copy! bytespan-reserve-front! bytespan-reserve-back! "                   \
  "bytespan-resize-front! bytespan-resize-back! "                                                  \
  "bytespan-u8-insert-front! bytespan-u8-insert-back! "                                            \
  "bytespan-bsp-insert-front! bytespan-bsp-insert-back! "                                          \
  "bytespan-bv-insert-front! bytespan-bv-insert-back! "                                            \
  "bytespan-erase-front! bytespan-erase-back! bytespan-iterate bytespan-u8-find "                  \
  "bytespan-peek-beg bytespan-peek-end bytespan-peek-data "

  eval("(library (schemesh containers bytespan (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_BYTESPAN_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) fx1+ fx1- record-writer void)\n"
       "    (schemesh containers misc))\n"
       "\n"
       "(define-record-type\n"
       "  (%bytespan %make-bytespan bytespan?)\n"
       "  (fields\n"
       "     (mutable beg bytespan-beg bytespan-beg-set!)\n"
       "     (mutable end bytespan-end bytespan-end-set!)\n"
       "     (mutable vec bytespan-vec bytespan-vec-set!))\n"
       "  (nongenerative #{%bytespan 1j9oboeqc5j4db1bamcd28yz-0}))\n"
       "\n"
       "(define (bytespan-peek-beg sp)\n"
       "  (bytespan-beg sp))\n"
       "\n"
       "(define (bytespan-peek-end sp)\n"
       "  (bytespan-end sp))\n"
       "\n"
       "(define (bytespan-peek-data sp)\n"
       "  (bytespan-vec sp))\n"
       "\n"
       "(define (list->bytespan l)\n"
       "  (let ((vec (list->bytevector l)))\n"
       "    (%make-bytespan 0 (bytevector-length vec) vec)))\n"
       "\n"
       /* create bytespan copying contents of specified bytevector */
       "(define (bytevector->bytespan vec)\n"
       "  (%make-bytespan 0 (bytevector-length vec) (bytevector-copy vec)))\n"
       "\n"
       /* view existing bytevector as bytespan */
       "(define (bytevector->bytespan* vec)\n"
       "  (%make-bytespan 0 (bytevector-length vec) vec))\n"
       "\n"
       "(define (make-bytespan n . val)\n"
       "  (%make-bytespan 0 n (apply make-bytevector n val)))\n"
       "\n"
       "(define (bytespan->bytevector sp)\n"
       "  (subbytevector (bytespan-vec sp) (bytespan-beg sp) (bytespan-end sp)))\n"
       "\n"
       "(define (bytespan . vals)\n"
       "  (list->bytespan vals))\n"
       "\n"
       "(define (bytespan-length sp)\n"
       "  (fx- (bytespan-end sp) (bytespan-beg sp)))\n"
       "\n"
       /* return length of internal bytevector, i.e. maximum number of elements
        * that can be stored without reallocating */
       "(define (bytespan-capacity sp)\n"
       "  (bytevector-length (bytespan-vec sp)))\n"
       "\n"
       "(define (bytespan-empty? sp)\n"
       "  (fx>=? (bytespan-beg sp) (bytespan-end sp)))\n"
       "\n"
       "(define (bytespan-clear! sp)\n"
       "  (bytespan-beg-set! sp 0)\n"
       "  (bytespan-end-set! sp 0))\n"
       "\n"
       "(define (bytespan-u8-ref sp idx)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (bytespan-length sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx+ idx (bytespan-beg sp))))\n"
       "\n"
       "(define (bytespan-u8-back sp)\n"
       "  (assert (not (bytespan-empty? sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-end sp))))\n"
       "\n"
       "(define (bytespan-u8-set! sp idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (bytespan-length sp)))\n"
       "  (bytevector-u8-set! (bytespan-vec sp) (fx+ idx (bytespan-beg sp)) val))\n"
       "\n"
       "(define (bytespan-fill! sp val)\n"
       "  (bytevector-fill-range! (bytespan-vec sp) (bytespan-beg sp)\n"
       "                          (bytespan-length sp) val))\n"
       "\n"
       "(define (bytespan-fill-range! sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (bytespan-length sp)))\n"
       "  (bytevector-fill-range! (bytespan-vec sp) (fx+ start (bytespan-beg sp)) n val))\n"
       "\n"
       "(define (bytespan-copy src)\n"
       "  (let* ((n (bytespan-length src))\n"
       "         (dst (make-bytespan n)))\n"
       "    (bytevector-copy! (bytespan-vec src) (bytespan-beg src)\n"
       "                  (bytespan-vec dst) (bytespan-beg dst) n)\n"
       "    dst))\n"
       "\n"
       "(define (bytespan-copy! src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (bytespan-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (bytespan-length dst)))\n"
       "  (bytevector-copy! (bytespan-vec src) (fx+ src-start (bytespan-beg src))\n"
       "                (bytespan-vec dst) (fx+ dst-start (bytespan-beg dst)) n))\n"
       "\n"
       "(define (bytespan-reallocate-front! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (bytespan-length sp)))\n"
       "        (old-vec (bytespan-vec sp))\n"
       "        (new-vec (make-bytevector cap))\n"
       "        (new-beg (fx- cap len)))\n"
       "    (bytevector-copy! old-vec (bytespan-beg sp) new-vec new-beg copy-len)\n"
       "    (bytespan-beg-set! sp new-beg)\n"
       "    (bytespan-end-set! sp cap)\n"
       "    (bytespan-vec-set! sp new-vec)))\n"
       "\n"
       "(define (bytespan-reallocate-back! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (bytespan-length sp)))\n"
       "        (old-vec (bytespan-vec sp))\n"
       "        (new-vec (make-bytevector cap)))\n"
       "    (bytevector-copy! old-vec (bytespan-beg sp) new-vec 0 copy-len)\n"
       "    (bytespan-beg-set! sp 0)\n"
       "    (bytespan-end-set! sp len)\n"
       "    (bytespan-vec-set! sp new-vec)))\n"
       "\n"
       /* return distance between begin of internal bytevector and last element */
       "(define (bytespan-capacity-front sp)\n"
       "  (bytespan-end sp))\n"
       "\n"
       /* return distance between first element and end of internal bytevector */
       "(define (bytespan-capacity-back sp)\n"
       "  (fx- (bytevector-length (bytespan-vec sp)) (bytespan-beg sp)))\n"
       "\n"
       /* ensure distance between begin of internal bytevector and last element is >= n.
        * does NOT change the length */
       "(define (bytespan-reserve-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (bytespan-vec sp))\n"
       "        (cap-front (bytespan-capacity-front sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (bytevector-length vec))\n"
       /*       bytevector is large enough, move elements to the back */
       "        (let* ((cap (bytespan-capacity sp))\n"
       "               (old-len (bytespan-length sp))\n"
       "               (new-beg (fx- cap old-len)))\n"
       "          (bytevector-copy! vec (bytespan-beg sp) vec new-beg old-len)\n"
       "          (bytespan-beg-set! sp new-beg)\n"
       "          (bytespan-end-set! sp cap)))\n"
       "      (#t\n"
       /*       bytevector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))\n"
       "         (bytespan-reallocate-front! sp (bytespan-length sp) new-cap))))))\n"
       "\n"
       /* ensure distance between first element and end of internal bytevector is >= n.
        * does NOT change the length */
       "(define (bytespan-reserve-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (bytespan-vec sp))\n"
       "        (cap-back (bytespan-capacity-back sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (bytevector-length vec))\n"
       /*       bytevector is large enough, move elements to the front */
       "        (let ((len (bytespan-length sp)))\n"
       "          (bytevector-copy! vec (bytespan-beg sp) vec 0 len)\n"
       "          (bytespan-beg-set! sp 0)\n"
       "          (bytespan-end-set! sp len)))\n"
       "      (#t\n"
       /*       bytevector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "         (bytespan-reallocate-back! sp (bytespan-length sp) new-cap))))))\n"
       "\n"
       /* grow or shrink bytespan on the left (front), set length to n */
       "(define (bytespan-resize-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-front! sp len)\n"
       "  (assert (fx>=? (bytespan-capacity-front sp) len))\n"
       "  (bytespan-beg-set! sp (fx- (bytespan-end sp) len)))\n"
       "\n"
       /* grow or shrink bytespan on the right (back), set length to n */
       "(define (bytespan-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-back! sp len)\n"
       "  (assert (fx>=? (bytespan-capacity-back sp) len))\n"
       "  (bytespan-end-set! sp (fx+ len (bytespan-beg sp))))\n"
       "\n"
       "(define (bytespan-u8-insert-front! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((pos 0)\n"
       "          (new-len (fx+ (bytespan-length sp) (length vals))))\n"
       "      (bytespan-resize-front! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (bytespan-u8-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       "(define (bytespan-u8-insert-back! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let* ((pos (bytespan-length sp))\n"
       "           (new-len (fx+ pos (length vals))))\n"
       "      (bytespan-resize-back! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (bytespan-u8-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       /* prefix a portion of another bytespan to this bytespan */
       "(define (bytespan-bsp-insert-front! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((len (bytespan-length sp-dst)))\n"
       "      (bytespan-resize-front! sp-dst (fx+ len src-n))\n"
       "      (bytespan-copy! sp-src src-start sp-dst 0 src-n))))\n"
       "\n"
       /* prefix a portion of a bytevector to this bytespan */
       "(define (bytespan-bv-insert-front! sp-dst bv-src src-start src-n)\n"
       "  (unless (fxzero? src-n)\n"
       "    (bytespan-bsp-insert-front! sp-dst (bytevector->bytespan* bv-src) src-start src-n)))\n"
       "\n"
       /* append a portion of another bytespan to this bytespan */
       "(define (bytespan-bsp-insert-back! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (bytespan-length sp-dst)))\n"
       "      (bytespan-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (bytespan-copy! sp-src src-start sp-dst pos src-n))))\n"
       "\n"
       /* append a portion of another bytespan to this bytespan */
       "(define (bytespan-bv-insert-back! sp-dst bv-src src-start src-n)\n"
       "  (unless (fxzero? src-n)\n"
       "    (bytespan-bsp-insert-back! sp-dst (bytevector->bytespan* bv-src) src-start src-n)))\n"
       "\n"
       /* erase n elements at the left (front) of bytespan */
       "(define (bytespan-erase-front! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (bytespan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (bytespan-beg-set! sp (fx+ n (bytespan-beg sp)))))\n"
       "\n"
       /* erase n elements at the right (back) of bytespan */
       "(define (bytespan-erase-back! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (bytespan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (bytespan-end-set! sp (fx- (bytespan-end sp) n))))\n"
       "\n"
       "(define (bytespan-iterate sp proc)\n"
       "  (do ((i (bytespan-beg sp) (fx1+ i))\n"
       "       (n (bytespan-end sp))\n"
       "       (v (bytespan-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (bytevector-u8-ref v i)))))))\n"
       "\n"
       /**
        * (bytespan-find) iterates on bytespan elements from start to (fxmin (fx+ start n)
        * (bytespan-length sp)), and returns the index of first bytespan element that causes
        * (predicate elem) to return non-#f. Returns #f if no such element is found.
        */
       "(define (bytespan-u8-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (bytespan-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (bytespan-u8-ref sp i))\n"
       "        (set! ret i)))))\n"
       "\n"
       /** customize how "bytespan" objects are printed */
       "(record-writer (record-type-descriptor %bytespan)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(bytespan\" port)\n"
       "    (bytespan-iterate sp"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_containers_charspan(void) {

  /****************************************************************************/
  /** Define Scheme type "charspan", a resizeable string */
  /****************************************************************************/

#define SCHEMESH_LIBRARY_CONTAINERS_CHARSPAN_EXPORT                                                \
  "list->charspan string->charspan string->charspan* make-charspan charspan->string "              \
  "charspan charspan? charspan-length charspan-empty? charspan-clear! charspan-capacity "          \
  "charspan-capacity-front charspan-capacity-back charspan-ref charspan-back charspan-set! "       \
  "charspan-fill! charspan-fill-range! charspan-copy charspan-copy! "                              \
  "charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back! "   \
  "charspan-insert-front! charspan-insert-back!  "                                                 \
  "charspan-csp-insert-front! charspan-csp-insert-back! "                                          \
  "charspan-erase-front! charspan-erase-back! charspan-iterate charspan-find "                     \
  "charspan-peek-data charspan-peek-beg charspan-peek-end "

  eval("(library (schemesh containers charspan (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_CHARSPAN_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-strings)\n"
       "    (only (chezscheme) fx1+ fx1- record-writer string-copy! void)\n"
       "    (schemesh containers misc))\n"
       "\n"
       "(define-record-type\n"
       "  (%charspan %make-charspan charspan?)\n"
       "  (fields\n"
       "     (mutable beg charspan-beg charspan-beg-set!)\n"
       "     (mutable end charspan-end charspan-end-set!)\n"
       "     (mutable vec charspan-vec charspan-vec-set!))\n"
       "  (nongenerative #{%charspan b847ikzm9lftljwelbq0cknyh-0}))\n"
       "\n"
       "(define (charspan-peek-beg sp)\n"
       "  (charspan-beg sp))\n"
       "\n"
       "(define (charspan-peek-end sp)\n"
       "  (charspan-end sp))\n"
       "\n"
       "(define (charspan-peek-data sp)\n"
       "  (charspan-vec sp))\n"
       "\n"
       "(define (list->charspan l)\n"
       "  (let ((vec (list->string l)))\n"
       "    (%make-charspan 0 (string-length vec) vec)))\n"
       "\n"
       /* create charspan copying contents of specified string */
       "(define (string->charspan vec)\n"
       "  (%make-charspan 0 (string-length vec) (string-copy vec)))\n"
       "\n"
       /* view existing string as charspan */
       "(define (string->charspan* vec)\n"
       "  (%make-charspan 0 (string-length vec) vec))\n"
       "\n"
       "(define (make-charspan n . val)\n"
       "  (%make-charspan 0 n (apply make-string n val)))\n"
       "\n"
       "(define (charspan->string sp)\n"
       "  (substring (charspan-vec sp) (charspan-beg sp) (charspan-end sp)))\n"
       "\n"
       "(define (charspan . vals)\n"
       "  (list->charspan vals))\n"
       "\n"
       "(define (charspan-length sp)\n"
       "  (fx- (charspan-end sp) (charspan-beg sp)))\n"
       "\n"
       /* return length of internal string, i.e. maximum number of elements
        * that can be stored without reallocating */
       "(define (charspan-capacity sp)\n"
       "  (string-length (charspan-vec sp)))\n"
       "\n"
       "(define (charspan-empty? sp)\n"
       "  (fx>=? (charspan-beg sp) (charspan-end sp)))\n"
       "\n"
       "(define (charspan-clear! sp)\n"
       "  (charspan-beg-set! sp 0)\n"
       "  (charspan-end-set! sp 0))\n"
       "\n"
       "(define (charspan-ref sp idx)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (charspan-length sp)))\n"
       "  (string-ref (charspan-vec sp) (fx+ idx (charspan-beg sp))))\n"
       "\n"
       "(define (charspan-back sp)\n"
       "  (assert (not (charspan-empty? sp)))\n"
       "  (string-ref (charspan-vec sp) (fx1- (charspan-end sp))))\n"
       "\n"
       "(define (charspan-set! sp idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (charspan-length sp)))\n"
       "  (string-set! (charspan-vec sp) (fx+ idx (charspan-beg sp)) val))\n"
       "\n"
       "(define (charspan-fill! sp val)\n"
       "  (string-fill-range! (charspan-vec sp) (charspan-beg sp) (charspan-length sp) val))\n"
       "\n"
       "(define (charspan-fill-range! sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (charspan-length sp)))\n"
       "  (string-fill-range! (charspan-vec sp) (fx+ start (charspan-beg sp)) n val))\n"
       "\n"
       "(define (charspan-copy src)\n"
       "  (let* ((n (charspan-length src))\n"
       "         (dst (make-charspan n)))\n"
       "    (string-copy! (charspan-vec src) (charspan-beg src)\n"
       "                  (charspan-vec dst) (charspan-beg dst) n)\n"
       "    dst))\n"
       "\n"
       "(define (charspan-copy! src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (charspan-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (charspan-length dst)))\n"
       "  (string-copy! (charspan-vec src) (fx+ src-start (charspan-beg src))\n"
       "                (charspan-vec dst) (fx+ dst-start (charspan-beg dst)) n))\n"
       "\n"
       "(define (charspan-reallocate-front! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (charspan-length sp)))\n"
       "        (old-vec (charspan-vec sp))\n"
       "        (new-vec (make-string cap))\n"
       "        (new-beg (fx- cap len)))\n"
       "    (string-copy! old-vec (charspan-beg sp) new-vec new-beg copy-len)\n"
       "    (charspan-beg-set! sp new-beg)\n"
       "    (charspan-end-set! sp cap)\n"
       "    (charspan-vec-set! sp new-vec)))\n"
       "\n"
       "(define (charspan-reallocate-back! sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (charspan-length sp)))\n"
       "        (old-vec (charspan-vec sp))\n"
       "        (new-vec (make-string cap)))\n"
       "    (string-copy! old-vec (charspan-beg sp) new-vec 0 copy-len)\n"
       "    (charspan-beg-set! sp 0)\n"
       "    (charspan-end-set! sp len)\n"
       "    (charspan-vec-set! sp new-vec)))\n"
       "\n"
       /* return distance between begin of internal string and last element */
       "(define (charspan-capacity-front sp)\n"
       "  (charspan-end sp))\n"
       "\n"
       /* return distance between first element and end of internal string */
       "(define (charspan-capacity-back sp)\n"
       "  (fx- (string-length (charspan-vec sp)) (charspan-beg sp)))\n"
       "\n"
       /* ensure distance between begin of internal string and last element is >= n.
        * does NOT change the length */
       "(define (charspan-reserve-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (charspan-vec sp))\n"
       "        (cap-front (charspan-capacity-front sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (string-length vec))\n"
       /*       string is large enough, move elements to the back */
       "        (let* ((cap (charspan-capacity sp))\n"
       "               (old-len (charspan-length sp))\n"
       "               (new-beg (fx- cap old-len)))\n"
       "          (string-copy! vec (charspan-beg sp) vec new-beg old-len)\n"
       "          (charspan-beg-set! sp new-beg)\n"
       "          (charspan-end-set! sp cap)))\n"
       "      (#t\n"
       /*       string is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))\n"
       "         (charspan-reallocate-front! sp (charspan-length sp) new-cap))))))\n"
       "\n"
       /* ensure distance between first element and end of internal string is >= n.
        * does NOT change the length */
       "(define (charspan-reserve-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (charspan-vec sp))\n"
       "        (cap-back (charspan-capacity-back sp)))\n"
       "    (cond\n"
       "      ((fx<=? len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<=? len (string-length vec))\n"
       /*       string is large enough, move elements to the front */
       "        (let ((len (charspan-length sp)))\n"
       "          (string-copy! vec (charspan-beg sp) vec 0 len)\n"
       "          (charspan-beg-set! sp 0)\n"
       "          (charspan-end-set! sp len)))\n"
       "      (#t\n"
       /*       string is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "         (charspan-reallocate-back! sp (charspan-length sp) new-cap))))))\n"
       "\n"
       /* grow or shrink charspan on the left (front), set length to n */
       "(define (charspan-resize-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (charspan-reserve-front! sp len)\n"
       "  (assert (fx>=? (charspan-capacity-front sp) len))\n"
       "  (charspan-beg-set! sp (fx- (charspan-end sp) len)))\n"
       "\n"
       /* grow or shrink charspan on the right (back), set length to n */
       "(define (charspan-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (charspan-reserve-back! sp len)\n"
       "  (assert (fx>=? (charspan-capacity-back sp) len))\n"
       "  (charspan-end-set! sp (fx+ len (charspan-beg sp))))\n"
       "\n"
       "(define (charspan-insert-front! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((pos 0)\n"
       "          (new-len (fx+ (charspan-length sp) (length vals))))\n"
       "      (charspan-resize-front! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (charspan-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       "(define (charspan-insert-back! sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let* ((pos (charspan-length sp))\n"
       "           (new-len (fx+ pos (length vals))))\n"
       "      (charspan-resize-back! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (charspan-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos)))))))\n"
       "\n"
       /* prefix a portion of another charspan to this charspan */
       "(define (charspan-csp-insert-front! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((len (charspan-length sp-dst)))\n"
       "      (charspan-resize-front! sp-dst (fx+ len src-n))\n"
       "      (charspan-copy! sp-src src-start sp-dst 0 src-n))))\n"
       "\n"
       /* append a portion of another charspan to this charspan */
       "(define (charspan-csp-insert-back! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (charspan-length sp-dst)))\n"
       "      (charspan-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (charspan-copy! sp-src src-start sp-dst pos src-n))))\n"
       "\n"
       /* erase n elements at the left (front) of charspan */
       "(define (charspan-erase-front! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (charspan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (charspan-beg-set! sp (fx+ n (charspan-beg sp)))))\n"
       "\n"
       /* erase n elements at the right (back) of charspan */
       "(define (charspan-erase-back! sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (charspan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (charspan-end-set! sp (fx- (charspan-end sp) n))))\n"
       "\n"
       "(define (charspan-iterate sp proc)\n"
       "  (do ((i (charspan-beg sp) (fx1+ i))\n"
       "       (n (charspan-end sp))\n"
       "       (v (charspan-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (string-ref v i)))))))\n"
       "\n"
       /**
        * (charspan-find) iterates on charspan elements from start to (fxmin (fx+ start n)
        * (charspan-length sp)), and returns the index of first charspan element that causes
        * (predicate elem) to return non-#f. Returns #f if no such element is found.
        */
       "(define (charspan-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (charspan-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (charspan-ref sp i))\n"
       "        (set! ret i)))))\n"
       "\n"
       /** customize how "charspan" objects are printed */
       "(record-writer (record-type-descriptor %charspan)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(string->charspan* \" port)\n"
       "    (write (charspan->string sp) port)\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_containers_gbuffer(void) {

  /****************************************************************************/
  /** Define Scheme type "gbuffer", a gap buffer. */
  /** Implementation: contains two spans, a "left" and a "right" ones */
  /****************************************************************************/

#define SCHEMESH_LIBRARY_CONTAINERS_GBUFFER_EXPORT                                                 \
  "list->gbuffer vector->gbuffer vector->gbuffer* span->gbuffer span->gbuffer* "                   \
  "gbuffer->vector gbuffer->span make-gbuffer gbuffer gbuffer? "                                   \
  "gbuffer-length gbuffer-empty? gbuffer-ref gbuffer-set! gbuffer-clear! gbuffer-split-at! "       \
  "gbuffer-insert-at! gbuffer-erase-at! gbuffer-iterate "

  eval("(library (schemesh containers gbuffer (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_GBUFFER_EXPORT ")\n"
       "  (import\n"
       "   (rnrs)\n"
       "   (only (chezscheme) fx1+ record-writer void)\n"
       "   (schemesh containers misc)\n"
       "   (schemesh containers span))\n"
       "\n"
       "(define-record-type\n"
       "  (%gbuffer %make-gbuffer gbuffer?)\n"
       "  (fields\n"
       "     (mutable left  gbuffer-left  gbuffer-left-set!)\n"
       "     (mutable right gbuffer-right gbuffer-right-set!))\n"
       "  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))\n"
       "\n"
       "(define (list->gbuffer l)\n"
       "  (%make-gbuffer (make-span 0) (list->span l)))\n"
       "\n"
       "(define (vector->gbuffer str)\n"
       "  (%make-gbuffer (make-span 0) (vector->span str)))\n"
       "\n"
       /* view a vector as gbuffer */
       "(define (vector->gbuffer* str)\n"
       "  (%make-gbuffer (make-span 0) (vector->span* str)))\n"
       "\n"
       "(define (span->gbuffer sp)\n"
       "  (%make-gbuffer (make-span 0) (span-copy sp)))\n"
       "\n"
       /* view a span as gbuffer */
       "(define (span->gbuffer* sp)\n"
       "  (%make-gbuffer (make-span 0) sp))\n"
       "\n"
       "(define (gbuffer->vector gb)\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (left-n  (span-length left))\n"
       "         (right-n (span-length right))\n"
       "         (dst (make-vector (fx+ left-n right-n))))\n"
       "    (vector-copy! (span-peek-data left)  (span-peek-beg left)  dst 0 left-n)\n"
       "    (vector-copy! (span-peek-data right) (span-peek-beg right) dst left-n right-n)\n"
       "    dst))\n"
       "\n"
       "(define (gbuffer->span gb)\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (left-n  (span-length left))\n"
       "         (right-n (span-length right))\n"
       "         (dst (make-span (fx+ left-n right-n))))\n"
       "    (span-copy! left  0 dst 0 left-n)\n"
       "    (span-copy! right 0 dst left-n right-n)\n"
       "    dst))\n"
       "\n"
       "(define (make-gbuffer n . val)\n"
       "  (%make-gbuffer (make-span 0) (apply make-span n val)))\n"
       "\n"
       "(define (gbuffer . vals)\n"
       "  (list->gbuffer vals))\n"
       "\n"
       "(define (gbuffer-length gb)\n"
       "  (fx+ (span-length (gbuffer-left gb)) (span-length (gbuffer-right gb))))\n"
       "\n"
       "(define (gbuffer-empty? gb)\n"
       "  (and (span-empty? (gbuffer-left gb)) (span-empty? (gbuffer-right gb))))\n"
       "\n"
       "(define (gbuffer-ref gb n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<? n (gbuffer-length gb)))\n"
       "  (let ((left-n (span-length (gbuffer-left gb))))\n"
       "    (if (fx<? n left-n)\n"
       "      (span-ref (gbuffer-left  gb) n)\n"
       "      (span-ref (gbuffer-right gb) (fx- n left-n)))))\n"
       "\n"
       "(define (gbuffer-set! gb idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (gbuffer-length gb)))\n"
       "  (let ((left-n (span-length (gbuffer-left gb))))\n"
       "    (if (fx<? idx left-n)\n"
       "      (span-set! (gbuffer-left  gb) idx val)\n"
       "      (span-set! (gbuffer-right gb) (fx- idx left-n) val))))\n"
       "\n"
       "(define (gbuffer-clear! gb)\n"
       "  (span-clear! (gbuffer-left  gb) 0)\n"
       "  (span-clear! (gbuffer-right gb) 0))\n"
       "\n"
       "(define (gbuffer-split-at! gb idx)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (gbuffer-length gb)))\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (delta (fx- idx (span-length left))))\n"
       "    (cond\n"
       "      ((fx>? delta 0)\n"
       "        (span-sp-insert-back! left right 0 delta)\n"
       "        (span-erase-front! right delta))\n"
       "      ((fx<? delta 0)\n"
       "        (span-sp-insert-front! right left idx (fx- delta))\n"
       "        (span-erase-back! left (fx- delta))))))\n"
       "\n"
       /** insert val into gbuffer at position idx */
       "(define (gbuffer-insert-at! gb idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (gbuffer-length gb)))\n"
       "  (let* ((left   (gbuffer-left  gb))\n"
       "         (right  (gbuffer-right gb))\n"
       "         (left-n (span-length left))\n"
       "         (delta  (fx- idx left-n)))\n"
       "    (cond\n"
       "      ((fxzero? idx)\n"
       "        (span-insert-front! left val))\n"
       "      ((fx=? idx (gbuffer-length gb))\n"
       "        (span-insert-back! right val))\n"
       "      (#t\n"
       "        (gbuffer-split-at! gb idx)\n"
       "        (span-insert-back! left val)))))\n"
       "\n"
       /**
        * read src-n elements from span sp-src starting from src-start
        * and insert them into gbuffer at position idx
        */
       "(define (gbuffer-sp-insert-at! gb idx sp-src src-start src-n)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (gbuffer-length gb)))\n"
       "  (let* ((left   (gbuffer-left  gb))\n"
       "         (right  (gbuffer-right gb))\n"
       "         (left-n (span-length left))\n"
       "         (delta  (fx- idx left-n)))\n"
       "    (cond\n"
       "      ((fxzero? src-n)\n" /* nothing to do */
       "        (assert (fx>=? src-start 0))\n"
       "        (assert (fx<=? src-start (span-length sp-src))))\n"
       "      ((fxzero? idx)\n"
       "        (span-sp-insert-front! left sp-src src-start src-n))\n"
       "      ((fx=? idx (gbuffer-length gb))\n"
       "        (span-sp-insert-back! right sp-src src-start src-n))\n"
       "      (#t\n"
       "        (gbuffer-split-at! gb idx)\n"
       "        (span-sp-insert-back! left sp-src src-start src-n)))))\n"
       "\n"
       /* remove n elements from gbuffer starting at start */
       "(define (gbuffer-erase-at! gb start n)\n"
       "  (let* ((left    (gbuffer-left  gb))\n"
       "         (right   (gbuffer-right gb))\n"
       "         (left-n  (span-length left))\n"
       "         (right-n (span-length right))\n"
       "         (len     (fx+ left-n right-n))\n"
       "         (end     (fx+ start n)))\n"
       "    (assert (fx>=? start 0))\n"
       "    (assert (fx<=? start len))\n"
       "    (assert (fx>=? n 0))\n"
       "    (assert (fx<=? n (fx- len start)))\n"
       "    (cond\n"
       "      ((fxzero? n) (void))\n" /* nothing to do */
       "      ((fxzero? start)\n"
       "        (let ((head (fxmin n left-n)))"
       "          (span-erase-front! left head)\n"
       "          (span-erase-front! right (fx- n head))))\n"
       "      ((fx=? end left-n)\n"
       "        (span-erase-back! left n))\n"
       "      ((fx=? start left-n)\n"
       "        (span-erase-front! right n))\n"
       "      ((fx=? end len)\n"
       "        (let ((tail (fxmin n right-n)))"
       "          (span-erase-back! right tail)\n"
       "          (span-erase-back! left (fx- n tail))))\n"
       "      (#t\n"
       "        (gbuffer-split-at! gb end)\n"
       "        (span-erase-back! left n)))))\n"
       "\n"
       "(define (gbuffer-iterate gb proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (gbuffer-length gb)))\n"
       "    ((or (fx>=? i n) (not (proc i (gbuffer-ref gb i)))))))\n"
       "\n"
       /** customize how "gbuffer" objects are printed */
       "(record-writer (record-type-descriptor %gbuffer)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(gbuffer\" port)\n"
       "    (gbuffer-iterate sp"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_containers_chargbuffer(void) {

  /****************************************************************************/
  /** Define Scheme type "chargbuffer", a gap buffer containing chars. */
  /** Implementation: contains two bytespans, a "left" and a "right" ones */
  /****************************************************************************/

#define SCHEMESH_LIBRARY_CONTAINERS_CHARGBUFFER_EXPORT                                             \
  "list->chargbuffer string->chargbuffer string->chargbuffer* "                                    \
  "charspan->chargbuffer charspan->chargbuffer* make-chargbuffer "                                 \
  "chargbuffer->charspan chargbuffer->string chargbuffer chargbuffer? chargbuffer-length "         \
  "chargbuffer-empty? "                                                                            \
  "chargbuffer-ref chargbuffer-set! chargbuffer-clear! chargbuffer-split-at! "                     \
  "chargbuffer-insert-at! chargbuffer-erase-at! chargbuffer-iterate "

  eval("(library (schemesh containers chargbuffer (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_CHARGBUFFER_EXPORT ")\n"
       "  (import\n"
       "   (rnrs)\n"
       "   (only (chezscheme) fx1+ record-writer string-copy! void)\n"
       "   (schemesh containers charspan))\n"
       "\n"
       "(define-record-type\n"
       "  (%chargbuffer %make-chargbuffer chargbuffer?)\n"
       "  (fields\n"
       "     (mutable left  chargbuffer-left  chargbuffer-left-set!)\n"
       "     (mutable right chargbuffer-right chargbuffer-right-set!))\n"
       "  (nongenerative #{%chargbuffer itah4n3k0nl66ucaakkpqk55m-16}))\n"
       "\n"
       "(define (list->chargbuffer l)\n"
       "  (%make-chargbuffer (make-charspan 0) (list->charspan l)))\n"
       "\n"
       "(define (string->chargbuffer str)\n"
       "  (%make-chargbuffer (make-charspan 0) (string->charspan str)))\n"
       "\n"
       /* view a string as chargbuffer */
       "(define (string->chargbuffer* str)\n"
       "  (%make-chargbuffer (make-charspan 0) (string->charspan* str)))\n"
       "\n"
       "(define (charspan->chargbuffer sp)\n"
       "  (%make-chargbuffer (make-charspan 0) (charspan-copy sp)))\n"
       "\n"
       /* view a charspan as chargbuffer */
       "(define (charspan->chargbuffer* sp)\n"
       "  (%make-chargbuffer (make-charspan 0) sp))\n"
       "\n"
       "(define (make-chargbuffer n . val)\n"
       "  (%make-chargbuffer (make-charspan 0) (apply make-charspan n val)))\n"
       "\n"
       "(define (chargbuffer->string gb)\n"
       "  (let* ((left    (chargbuffer-left  gb))\n"
       "         (right   (chargbuffer-right gb))\n"
       "         (left-n  (charspan-length left))\n"
       "         (right-n (charspan-length right))\n"
       "         (dst (make-string (fx+ left-n right-n))))\n"
       "    (string-copy! (charspan-peek-data left)  (charspan-peek-beg left)\n"
       "                  dst 0 left-n)\n"
       "    (string-copy! (charspan-peek-data right) (charspan-peek-beg right)\n"
       "                  dst left-n right-n)\n"
       "    dst))\n"
       "\n"
       "(define (chargbuffer->charspan gb)\n"
       "  (let* ((left  (chargbuffer-left  gb))\n"
       "         (right (chargbuffer-right gb))\n"
       "         (left-n  (charspan-length left))\n"
       "         (right-n (charspan-length right))\n"
       "         (dst (make-charspan (fx+ left-n right-n))))\n"
       "    (charspan-copy! left  0 dst 0 left-n)\n"
       "    (charspan-copy! right 0 dst left-n right-n)\n"
       "    dst))\n"
       "\n"
       "(define (chargbuffer . vals)\n"
       "  (list->chargbuffer vals))\n"
       "\n"
       "(define (chargbuffer-length gb)\n"
       "  (fx+ (charspan-length (chargbuffer-left gb)) (charspan-length (chargbuffer-right gb))))\n"
       "\n"
       "(define (chargbuffer-empty? gb)\n"
       "  (and (charspan-empty? (chargbuffer-left gb)) (charspan-empty? (chargbuffer-right gb))))\n"
       "\n"
       "(define (chargbuffer-ref gb n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<? n (chargbuffer-length gb)))\n"
       "  (let ((left-n (charspan-length (chargbuffer-left gb))))\n"
       "    (if (fx<? n left-n)\n"
       "      (charspan-ref (chargbuffer-left  gb) n)\n"
       "      (charspan-ref (chargbuffer-right gb) (fx- n left-n)))))\n"
       "\n"
       "(define (chargbuffer-set! gb idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<? idx (chargbuffer-length gb)))\n"
       "  (let ((left-n (charspan-length (chargbuffer-left gb))))\n"
       "    (if (fx<? idx left-n)\n"
       "      (charspan-set! (chargbuffer-left  gb) idx val)\n"
       "      (charspan-set! (chargbuffer-right gb) (fx- idx left-n) val))))\n"
       "\n"
       "(define (chargbuffer-clear! gb)\n"
       "  (charspan-clear! (chargbuffer-left  gb) 0)\n"
       "  (charspan-clear! (chargbuffer-right gb) 0))\n"
       "\n"
       "(define (chargbuffer-split-at! gb idx)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (chargbuffer-length gb)))\n"
       "  (let* ((left  (chargbuffer-left  gb))\n"
       "         (right (chargbuffer-right gb))\n"
       "         (delta (fx- idx (charspan-length left))))\n"
       "    (cond\n"
       "      ((fx>? delta 0)\n"
       "        (charspan-csp-insert-back! left right 0 delta)\n"
       "        (charspan-erase-front! right delta))\n"
       "      ((fx<? delta 0)\n"
       "        (charspan-csp-insert-front! right left idx (fx- delta))\n"
       "        (charspan-erase-back! left (fx- delta))))))\n"
       "\n"
       /** insert val into chargbuffer at position idx */
       "(define (chargbuffer-insert-at! gb idx val)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (chargbuffer-length gb)))\n"
       "  (let* ((left   (chargbuffer-left  gb))\n"
       "         (right  (chargbuffer-right gb))\n"
       "         (left-n (charspan-length left))\n"
       "         (delta  (fx- idx left-n)))\n"
       "    (cond\n"
       "      ((fxzero? idx)\n"
       "        (charspan-insert-front! left val))\n"
       "      ((fx=? idx (chargbuffer-length gb))\n"
       "        (charspan-insert-back! right val))\n"
       "      (#t\n"
       "        (chargbuffer-split-at! gb idx)\n"
       "        (charspan-insert-back! left val)))))\n"
       "\n"
       /**
        * read src-n elements from charspan sp-src starting from src-start
        * and insert them into chargbuffer at position idx
        */
       "(define (chargbuffer-sp-insert-at! gb idx sp-src src-start src-n)\n"
       "  (assert (fx>=? idx 0))\n"
       "  (assert (fx<=? idx (chargbuffer-length gb)))\n"
       "  (let* ((left   (chargbuffer-left  gb))\n"
       "         (right  (chargbuffer-right gb))\n"
       "         (left-n (charspan-length left))\n"
       "         (delta  (fx- idx left-n)))\n"
       "    (cond\n"
       "      ((fxzero? src-n)\n" /* nothing to do */
       "        (assert (fx>=? src-start 0))\n"
       "        (assert (fx<=? src-start (charspan-length sp-src))))\n"
       "      ((fxzero? idx)\n"
       "        (charspan-csp-insert-front! left sp-src src-start src-n))\n"
       "      ((fx=? idx (chargbuffer-length gb))\n"
       "        (charspan-csp-insert-back! right sp-src src-start src-n))\n"
       "      (#t\n"
       "        (chargbuffer-split-at! gb idx)\n"
       "        (charspan-csp-insert-back! left sp-src src-start src-n)))))\n"
       "\n"
       /* remove n elements from chargbuffer starting at start */
       "(define (chargbuffer-erase-at! gb start n)\n"
       "  (let* ((left    (chargbuffer-left  gb))\n"
       "         (right   (chargbuffer-right gb))\n"
       "         (left-n  (charspan-length left))\n"
       "         (right-n (charspan-length right))\n"
       "         (len     (fx+ left-n right-n))\n"
       "         (end     (fx+ start n)))\n"
       "    (assert (fx>=? start 0))\n"
       "    (assert (fx<=? start len))\n"
       "    (assert (fx>=? n 0))\n"
       "    (assert (fx<=? n (fx- len start)))\n"
       "    (cond\n"
       "      ((fxzero? n) (void))\n" /* nothing to do */
       "      ((fxzero? start)\n"
       "        (let ((head (fxmin n left-n)))"
       "          (charspan-erase-front! left head)\n"
       "          (charspan-erase-front! right (fx- n head))))\n"
       "      ((fx=? end left-n)\n"
       "        (charspan-erase-back! left n))\n"
       "      ((fx=? start left-n)\n"
       "        (charspan-erase-front! right n))\n"
       "      ((fx=? end len)\n"
       "        (let ((tail (fxmin n right-n)))"
       "          (charspan-erase-back! right tail)\n"
       "          (charspan-erase-back! left (fx- n tail))))\n"
       "      (#t\n"
       "        (chargbuffer-split-at! gb end)\n"
       "        (charspan-erase-back! left n)))))\n"
       "\n"
       "(define (chargbuffer-iterate gb proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (chargbuffer-length gb)))\n"
       "    ((or (fx>=? i n) (not (proc i (chargbuffer-ref gb i)))))))\n"
       "\n"
       /** customize how "chargbuffer" objects are printed */
       "(record-writer (record-type-descriptor %chargbuffer)\n"
       "  (lambda (gb port writer)\n"
       "    (display \"(string->chargbuffer* \" port)\n"
       "    (write (chargbuffer->string gb) port)\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
}

void define_library_containers(void) {
  define_library_containers_misc();
  define_library_containers_hashtable();
  define_library_containers_span();
  define_library_containers_bytespan();
  define_library_containers_charspan();
  define_library_containers_gbuffer();
  define_library_containers_chargbuffer();

  eval("(library (schemesh containers (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_MISC_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_HASHTABLE_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_SPAN_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_BYTESPAN_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_CHARSPAN_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_GBUFFER_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_CONTAINERS_CHARGBUFFER_EXPORT ")\n"
       "  (import (schemesh containers misc)\n"
       "          (schemesh containers hashtable)\n"
       "          (schemesh containers span)\n"
       "          (schemesh containers bytespan)\n"
       "          (schemesh containers charspan)\n"
       "          (schemesh containers gbuffer)\n"
       "          (schemesh containers chargbuffer)))\n");

  eval("(import (schemesh containers))\n");
}
