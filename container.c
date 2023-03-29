/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

/** define some additional vector functions */
static void define_vector_functions(void) {
  /** copy a portion of vector src into dst */
  eval("(define (vector-copy! src src-start dst dst-start n)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (let ((elem (vector-ref src (fx+ i src-start))))\n"
       "      (vector-set! dst (fx+ i dst-start) elem))))\n");

  /**
   * return a copy of vector vec containing only elements
   * from start (inclusive) to end (exclusive)
   */
  eval("(define (subvector vec start end)\n"
       "  (let* ((n (fx- end start))\n"
       "         (dst (make-vector n)))\n"
       "    (vector-copy! vec start dst 0 n)\n"
       "    dst))\n");

  eval("(define (vector-fill-range! vec start n val)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (vector-set! vec (fx+ i start) obj))))\n");

  /**
   * (vector-iterate l proc) iterates on all elements of given vector vec,
   * and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
   */
  eval("(define (vector-iterate vec proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (vector-length vec)))\n"
       "      ((or (fx>=? i n) (not (proc i (vector-ref vec i))))))))\n");

  /**
   * (vector->hashtable vec htable) iterates on all elements of given vector vec,
   * which must be cons cells, and inserts them into hashtable htable:
   * (car cell) is used as key, and (cdr cell) is used ad value.
   *
   * Returns htable.
   */
  eval("(define (vector->hashtable vec htable)\n"
       "  (vector-iterate vec\n"
       "    (lambda (i cell)\n"
       "      (hashtable-set! htable (car cell) (cdr cell))))\n"
       "  htable)\n");
}

/** define some additional bytevector functions */
static void define_bytevector_functions(void) {
  eval("(define (bytevector-fill-range! bvec start n val)\n"
       "  (do ((i 0 (fx1+ i)))\n"
       "      ((fx>=? i n))\n"
       "    (bytevector-u8-set! bvec (fx+ i start) val))))\n");

  /**
   * (bytevector-iterate l proc) iterates on all elements of given bytevector vec,
   * and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
   */
  eval("(define (bytevector-iterate bvec proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (bytevector-length bvec)))\n"
       "      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i))))))))\n");
}

/** Define Scheme type "array", a resizeable vector */
static void define_array_functions(void) {
  eval("(begin\n"
       "  (define list->array)\n"
       "  (define vector->array)\n"
       "  (define make-array)\n"
       "  (define array->vector)\n"
       "  (define array)\n"
       "  (define array?)\n"
       "  (define array-length)\n"
       "  (define array-capacity)\n"
       "  (define array-empty?)\n"
       "  (define array-ref)\n"
       "  (define array-set!)\n"
       "  (define array-last)\n"
       "  (define array-fill!)\n"
       "  (define array-fill-range!)\n"
       "  (define array-copy)\n"
       "  (define array-copy!)\n"
       "  (define array-capacity-set!)\n"
       "  (define array-length-set!)\n"
       "  (define array-append!)\n"
       "  (define array-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%array %make-array %array?)\n"
       "  (fields\n"
       "     (mutable len array-len array-len-set!)\n"
       "     (mutable vec array-vec array-vec-set!))\n"
       "  (nongenerative #{%array ddartshmtuppqpm7bv5l7h7jm-0}))\n"
       "\n"
       "(set! list->array (lambda (l)\n"
       "  (let ((vec (list->vector l)))\n"
       "    (%make-array (vector-length vec) vec))))\n"
       "\n"
       "(set! vector->array (lambda (vec)\n"
       "  (%make-array (vector-length vec) (vector-copy vec))))\n"
       "\n"
       "(set! make-array (lambda (n . obj)\n"
       "  (%make-array n (apply make-vector n obj))))\n"
       "\n"
       "(set! array->vector (lambda (obj)\n"
       "  (subvector (array-vec obj) (array-len obj))))\n"
       "\n"
       "(set! array (lambda objs\n"
       "  (list->array objs)))\n"
       "\n"
       "(set! array? (lambda (obj)\n"
       "  (%array? obj)))\n"
       "\n"
       "(set! array-length (lambda (arr)\n"
       "  (array-len arr)))\n"
       "\n"
       "(set! array-capacity (lambda (arr)\n"
       "  (vector-length (array-vec arr))))\n"
       "\n"
       "(set! array-empty? (lambda (arr)\n"
       "  (fxzero? (array-len arr))))\n"
       "\n"
       "(set! array-ref (lambda (arr n)\n"
       "  (assert (fx<? n (array-len arr)))\n"
       "  (vector-ref (array-vec arr) n)))\n"
       "\n"
       "(set! array-set! (lambda (arr n obj)\n"
       "  (assert (fx<? n (array-len arr)))\n"
       "  (vector-set! (array-vec arr) n obj)))\n"
       "\n"
       "(set! array-last (lambda (arr)\n"
       "  (assert (not (array-empty? arr)))\n"
       "  (vector-ref (array-vec arr) (fx1- (array-len arr)))))\n"
       "\n"
       "(set! array-fill! (lambda (arr obj)\n"
       /* no optimized function to fill only up to array-len,
        * so fill up to array-capacity */
       "  (vector-fill! (array-vec arr) obj)))\n"
       "\n"
       "(set! array-fill-range! (lambda (arr start n obj)\n"
       "  (assert (fx<=? (fx+ src n) (array-len arr)))\n"
       "  (vector-fill-range! (array-vec arr) start n obj)))\n"
       "\n"
       "(set! array-copy (lambda (src)\n"
       "  (let* ((n (array-length src))\n"
       "         (dst (make-array n)))\n"
       "    (vector-copy! (array-vec src) 0 (array-vec dst) 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! array-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx<=? (fx+ src-start n) (array-len src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (array-len dst)))\n"
       "  (vector-copy! (array-vec src) src-start (array-vec dst) dst-start n)))\n"
       "\n"
       "(set! array-capacity-set! (lambda (arr n)\n"
       "  (assert (fx>=? n (array-len arr)))\n"
       "  (unless (fx=? n (array-capacity arr))\n"
       "    (let* ((len (array-len arr))\n"
       "           (old-vec (array-vec arr))\n"
       "           (new-vec (make-vector n)))\n"
       "      (vector-copy! old-vec 0 new-vec 0 len)\n"
       "      (array-vec-set! arr new-vec)))))\n"
       "\n"
       "(set! array-length-set! (lambda (arr n)\n"
       "  (when (fx>? n (array-capacity arr))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (array-capacity arr)))))\n"
       "      (array-capacity-set! arr new-cap)))\n"
       "  (array-len-set! arr n)))\n"
       "\n"
       "(set! array-append! (lambda (arr . elements)\n"
       "  (unless (null? elements)\n"
       "    (let ((elem-n (length elements))\n"
       "          (pos    (array-len arr)))\n"
       "      (array-length-set! arr (fx+ pos elem-n))\n"
       "      (list-iterate elements\n"
       "        (lambda (elem)\n"
       "          (array-set! arr pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! array-iterate (lambda (arr proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (array-len arr))\n"
       "       (v (array-vec arr)))\n"
       "    ((or (fx>=? i n) (not (proc i (vector-ref v i))))))))\n"
       "\n"
       /** customize how "array" objects are printed */
       "(record-writer (record-type-descriptor %array)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(array\" port)\n"
       "    (array-iterate obj"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       ")\n");

  /**
   * (array-find) iterates on array elements from start to (fxmin (fx+ start n) (array-length arr)),
   * and returns the index of first array element that causes (predicate elem)
   * to return non-#f.
   * Returns #f if no such element is found.
   */
  eval("(define (array-find arr start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (array-length arr))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (array-ref arr i))\n"
       "        (set! ret i)))))\n");
}

/** Define Scheme type "bytearray", a resizeable bytevector */
static void define_bytearray_functions(void) {
  eval("(begin\n"
       "  (define bytevector->bytearray)\n"
       "  (define list->bytearray)\n"
       "  (define make-bytearray)\n"
       "  (define bytearray->bytevector)\n"
       "  (define bytearray)\n"
       "  (define bytearray?)\n"
       "  (define bytearray-length)\n"
       "  (define bytearray-capacity)\n"
       "  (define bytearray-underlying)\n" /* return underlying bytevector */
       "  (define bytearray-empty?)\n"
       "  (define bytearray-u8-ref)\n"
       "  (define bytearray-u8-set!)\n"
       "  (define bytearray-u8-last)\n"
       "  (define bytearray-fill!)\n"
       "  (define bytearray-fill-range!)\n"
       "  (define bytearray-copy)\n"
       "  (define bytearray-copy!)\n"
       "  (define bytearray-capacity-set!)\n"
       "  (define bytearray-length-set!)\n"
       "  (define bytearray-u8-append!)\n"
       "  (define bytearray-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%bytearray %make-bytearray %bytearray?)\n"
       "  (fields\n"
       "     (mutable len bytearray-len bytearray-len-set!)\n"
       "     (mutable vec bytearray-vec bytearray-vec-set!))\n"
       "  (nongenerative #{%bytearray mu5r2go07bretxssmue01kt46-0}))\n"
       "\n"
       "(set! bytevector->bytearray (lambda (vec)\n"
       "  (%make-bytearray (bytevector-length vec) (bytevector-copy vec))))\n"
       "(set! list->bytearray (lambda (l)\n"
       "  (let ((vec (apply bytevector l)))\n"
       "    (%make-bytearray (bytevector-length vec) vec))))\n"
       "\n"
       "(set! make-bytearray (lambda (n . val)\n"
       "  (%make-bytearray n (apply make-bytevector n val))))\n"
       "\n"
       "(set! bytearray->bytevector (lambda (obj)\n"
       "  (let* ((n (bytearray-len obj))\n"
       "         (dst (make-bytevector n)))\n"
       "    (bytevector-copy! (bytearray-vec obj) 0 dst 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! bytearray (lambda objs\n"
       "  (list->bytearray objs)))\n"
       "\n"
       "(set! bytearray? (lambda (obj)\n"
       "   (%bytearray? obj)))\n"
       "\n"
       "(set! bytearray-length (lambda (arr)\n"
       "  (bytearray-len arr)))\n"
       "\n"
       "(set! bytearray-capacity (lambda (arr)\n"
       "  (bytevector-length (bytearray-vec arr))))\n"
       "\n"
       "(set! bytearray-underlying (lambda (arr)\n"
       "  (bytearray-vec arr)))\n"
       "\n"
       "(set! bytearray-empty? (lambda (arr)\n"
       "  (fxzero? (bytearray-len arr))))\n"
       "\n"
       "(set! bytearray-u8-ref (lambda (arr n)\n"
       "  (assert (fx<? n (bytearray-len arr)))\n"
       "  (bytevector-u8-ref (bytearray-vec arr) n)))\n"
       "\n"
       "(set! bytearray-u8-set! (lambda (arr n obj)\n"
       "  (assert (fx<? n (bytearray-len arr)))\n"
       "  (bytevector-u8-set! (bytearray-vec arr) n obj)))\n"
       "\n"
       "(set! bytearray-u8-last (lambda (arr)\n"
       "  (assert (not (bytearray-empty? arr)))\n"
       "  (bytevector-u8-ref (bytearray-vec arr) (fx1- (bytearray-len arr)))))\n"
       "\n"
       "(set! bytearray-fill! (lambda (arr obj)\n"
       /* no optimized function to fill only up to bytearray-len,
        * so fill up to bytearray-capacity */
       "  (bytevector-fill! (bytearray-vec arr) obj)))\n"
       "\n"
       "(set! bytearray-fill-range! (lambda (arr start n obj)\n"
       "  (assert (fx<=? (fx+ src n) (bytearray-len arr)))\n"
       "  (bytevector-fill-range! (bytearray-vec arr) start n obj)))\n"
       "\n"
       "(set! bytearray-copy (lambda (src)\n"
       "  (let* ((n (bytearray-length src))\n"
       "         (dst (make-bytearray n)))\n"
       "    (bytevector-copy! (bytearray-vec src) 0 (bytearray-vec dst) 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! bytearray-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx<=? (fx+ src-start n) (bytearray-len src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (bytearray-len dst)))\n"
       "  (bytevector-copy! (bytearray-vec src) src-start (bytearray-vec dst) dst-start n)))\n"
       "\n"
       "(set! bytearray-capacity-set! (lambda (arr n)\n"
       "  (assert (fx>=? n (bytearray-len arr)))\n"
       "  (unless (fx=? n (bytearray-capacity arr))\n"
       "    (let* ((len (bytearray-len arr))\n"
       "           (old-vec (bytearray-vec arr))\n"
       "           (new-vec (make-bytevector n)))\n"
       "      (bytevector-copy! old-vec 0 new-vec 0 len)\n"
       "      (bytearray-vec-set! arr new-vec)))))\n"
       "\n"
       "(set! bytearray-length-set! (lambda (arr n)\n"
       "  (when (fx>? n (bytearray-capacity arr))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (bytearray-capacity arr)))))\n"
       "      (bytearray-capacity-set! arr new-cap)))\n"
       "  (bytearray-len-set! arr n)))\n"
       "\n"
       "(set! bytearray-u8-append! (lambda (arr . elements)\n"
       "  (unless (null? elements)\n"
       "    (let ((elem-n (length elements))\n"
       "          (pos    (bytearray-len arr)))\n"
       "      (bytearray-length-set! arr (fx+ pos elem-n))\n"
       "      (list-iterate elements\n"
       "        (lambda (elem)\n"
       "          (bytearray-u8-set! arr pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! bytearray-iterate (lambda (arr proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (bytearray-len arr))\n"
       "       (v (bytearray-vec arr)))\n"
       "    ((or (fx>=? i n) (not (proc i (bytevector-u8-ref v i))))))))\n"
       "\n"
       /** customize how "bytearray" objects are printed */
       "(record-writer (record-type-descriptor %bytearray)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(bytearray\" port)\n"
       "    (bytearray-iterate obj"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       ")\n");

  /**
   * (bytearray-u8-find) iterates on bytearray elements from start to (fxmin (fx+ start n)
   * (bytearray-length arr)), and returns the index of first bytearray element that causes
   * (predicate elem) to return non-#f. Returns #f if no such element is found.
   */
  eval("(define (bytearray-u8-find arr start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (bytearray-length arr))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (bytearray-u8-ref arr i))\n"
       "        (set! ret i)))))\n");
}

/** Define Scheme type "chararray", a resizeable string */
static void define_chararray_functions(void) {
  eval("(begin\n"
       "  (define list->chararray)\n"
       "  (define string->chararray)\n"
       "  (define make-chararray)\n"
       "  (define chararray->string)\n"
       "  (define chararray)\n"
       "  (define chararray?)\n"
       "  (define chararray-length)\n"
       "  (define chararray-capacity)\n"
       "  (define chararray-empty?)\n"
       "  (define chararray-ref)\n"
       "  (define chararray-set!)\n"
       "  (define chararray-last)\n"
       "  (define chararray-fill!)\n"
       "  (define chararray-fill-range!)\n"
       "  (define chararray-copy)\n"
       "  (define chararray-copy!)\n"
       "  (define chararray-capacity-set!)\n"
       "  (define chararray-length-set!)\n"
       "  (define chararray-append!)\n"
       "  (define chararray-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%chararray %make-chararray %chararray?)\n"
       "  (fields\n"
       "     (mutable len chararray-len chararray-len-set!)\n"
       "     (mutable vec chararray-vec chararray-vec-set!))\n"
       "  (nongenerative #{%chararray g8i3d8unk70r0jnvh6n2uil2a-0}))\n"
       "\n"
       "(set! list->chararray (lambda (l)\n"
       "  (let ((vec (apply string l)))\n"
       "    (%make-chararray (string-length vec) vec))))\n"
       "\n"
       "(set! string->chararray (lambda (vec)\n"
       "  (%make-chararray (string-length vec) (string-copy vec))))\n"
       "\n"
       "(set! make-chararray (lambda (len . fillchar)\n"
       "  (%make-chararray len (apply make-string len fillchar))))\n"
       "\n"
       "(set! chararray->string (lambda (obj)\n"
       "  (substring (chararray-vec obj) 0 (chararray-len obj))))\n"
       "\n"
       "(set! chararray (lambda objs\n"
       "  (list->chararray objs)))\n"
       "\n"
       "(set! chararray? (lambda (obj)\n"
       "  (%chararray? obj)))\n"
       "\n"
       "(set! chararray-length (lambda (arr)\n"
       "  (chararray-len arr)))\n"
       "\n"
       "(set! chararray-capacity (lambda (arr)\n"
       "  (string-length (chararray-vec arr))))\n"
       "\n"
       "(set! chararray-empty? (lambda (arr)\n"
       "  (fxzero? (chararray-len arr))))\n"
       "\n"
       "(set! chararray-ref (lambda (arr n)\n"
       "  (assert (fx<? n (chararray-len arr)))\n"
       "  (string-ref (chararray-vec arr) n)))\n"
       "\n"
       "(set! chararray-set! (lambda (arr n obj)\n"
       "  (assert (fx<? n (chararray-len arr)))\n"
       "  (string-set! (chararray-vec arr) n obj)))\n"
       "\n"
       "(set! chararray-last (lambda (arr)\n"
       "  (assert (not (chararray-empty? arr)))\n"
       "  (string-ref (chararray-vec arr) (fx1- (chararray-len arr)))))\n"
       "\n"
       "(set! chararray-fill! (lambda (arr obj)\n"
       /* no optimized function to fill only up to chararray-len,
        * so fill up to chararray-capacity */
       "  (string-fill! (chararray-vec arr) obj)))\n"
       "\n"
       "(set! chararray-fill-range! (lambda (arr start n obj)\n"
       "  (assert (fx<=? (fx+ src n) (chararray-len arr)))\n"
       "  (substring-fill! (chararray-vec arr) start n obj)))\n"
       "\n"
       "(set! chararray-copy (lambda (src)\n"
       "  (let* ((n (chararray-length src))\n"
       "         (dst (make-chararray n)))\n"
       "    (string-copy! (chararray-vec src) 0 (chararray-vec dst) 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! chararray-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx<=? (fx+ src-start n) (chararray-len src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (chararray-len dst)))\n"
       "  (string-copy! (chararray-vec src) src-start (chararray-vec dst) dst-start n)))\n"
       "\n"
       "(set! chararray-capacity-set! (lambda (arr n)\n"
       "  (assert (fx>=? n (chararray-len arr)))\n"
       "  (unless (fx=? n (chararray-capacity arr))\n"
       "    (let* ((len (chararray-len arr))\n"
       "           (old-vec (chararray-vec arr))\n"
       "           (new-vec (make-string n)))\n"
       "      (string-copy! old-vec 0 new-vec 0 len)\n"
       "      (chararray-vec-set! arr new-vec)))))\n"
       "\n"
       "(set! chararray-length-set! (lambda (arr n)\n"
       "  (when (fx>? n (chararray-capacity arr))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (chararray-capacity arr)))))\n"
       "      (chararray-capacity-set! arr new-cap)))\n"
       "  (chararray-len-set! arr n)))\n"
       "\n"
       "(set! chararray-append! (lambda (arr . elements)\n"
       "  (unless (null? elements)\n"
       "    (let ((elem-n (length elements))\n"
       "          (pos    (chararray-len arr)))\n"
       "      (chararray-length-set! arr (fx+ pos elem-n))\n"
       "      (list-iterate elements\n"
       "        (lambda (elem)\n"
       "          (chararray-set! arr pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! chararray-iterate (lambda (arr proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (chararray-len arr))\n"
       "       (v (chararray-vec arr)))\n"
       "    ((or (fx>=? i n) (not (proc i (string-ref v i))))))))\n"
       "\n"
       /** customize how "chararray" objects are printed */
       "(record-writer (record-type-descriptor %chararray)\n"
       "  (lambda (obj port writer)\n"
       "    (display \"(string->chararray \" port)\n"
       "    (write (substring (chararray-vec obj) 0 (chararray-len obj)) port)\n"
       "    (display #\\) port)))\n"
       ")\n");

  /**
   * (chararray-find) iterates on chararray elements from start to (fxmin (fx+ start n)
   * (chararray-length arr)), and returns the index of first chararray element that causes
   * (predicate elem) to return non-#f. Returns #f if no such element is found.
   */
  eval("(define (chararray-find arr start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (chararray-length arr))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (chararray-ref arr i))\n"
       "        (set! ret i)))))\n");
}

static void define_list_functions(void) {
  /**
   * (list-iterate l proc) iterates on all elements of given list l,
   * and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
   */
  eval("(define (list-iterate l proc)\n"
       "  (do ((tail l (cdr tail)))\n"
       "      ((or (null? tail) (not (proc (car tail)))))))\n");
}

static void define_hash_functions(void) {
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
        *
        * Assigning the (cdr) of an element propagates to the hashtable,
        * i.e. changes the value associated to key in hashtable.
        *
        * Do NOT modify the (car) of an element!
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
       /*     ; hashtable is empty, return empty iterator */
       "      (make-%hash-iterator 0 #f (vector) (vector))\n"
       /*     ; hashtable is not empty, seek to first bucket */
       "      (let* ((is-eqv (eqv-ht? h))\n"
       "             (vec1 (if is-eqv (ht-vec (eqv-ht-eqht h))  (ht-vec h)))\n"
       "             (vec2 (if is-eqv (ht-vec (eqv-ht-genht h)) (vector)))\n"
       "             (iter (make-%hash-iterator -1 #f vec1 vec2)))\n"
       /*       ; advance iterator to first bucket */
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
       /*     ; iterate on vec1 until we find a cell */
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
       /*         ; either we found a cell, or vec2 is empty and we reached end of vec1 */
       "          keyval\n"
       /*         ; no cell found, but vec2 is non-empty: switch to it and retry */
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

  /**
   * (hashtable-transpose src dst) iterates on all (key . value) elements of hashtable src,
   * and inserts each of them into hashtable dst as transposed (value . key)
   *
   * Returns dst.
   */
  eval("(define (hashtable-transpose src dst)\n"
       "  (hashtable-iterate src\n"
       "    (lambda (cell)\n"
       "      (hashtable-set! dst (cdr cell) (car cell))))\n"
       "  dst)\n");

  /**
   * (eq-hashtable . pairs) iterates on all (key . value) elements of pairs,
   * and inserts each of them into a new hashtable created with (make-eq-hashtable (length pairs)).
   *
   * Returns the new hashtable.
   */
  eval("(define (eq-hashtable . pairs)\n"
       "  (let ((dst (make-eq-hashtable (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n");

  /**
   * (eqv-hashtable . pairs) iterates on all (key . value) elements of pairs,
   * and inserts each of them into a new hashtable created with (make-eqv-hashtable (length pairs)).
   *
   * Returns the new hashtable.
   */
  eval("(define (eqv-hashtable . pairs)\n"
       "  (let ((dst (make-eqv-hashtable (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n");

  /**
   * (hashtable hash-proc eq-proc . pairs) iterates on all (key . value) elements of pairs,
   * and inserts each of them into a new hashtable created with
   *   (make-hashtable hash-proc eq-proc (length pairs)).
   *
   * Returns the new hashtable.
   */
  eval("(define (hashtable hash-proc eq-proc . pairs)\n"
       "  (let ((dst (make-hashtable hash-proc eq-proc (length pairs))))\n"
       "    (list-iterate pairs\n"
       "      (lambda (cell)\n"
       "        (hashtable-set! dst (car cell) (cdr cell))))\n"
       "    dst))\n");
}

void define_container_functions(void) {
  define_vector_functions();
  define_array_functions();
  define_bytevector_functions();
  define_bytearray_functions();
  define_chararray_functions();
  define_hash_functions();
  define_list_functions();
}
