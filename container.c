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
       "    (vector-set! vec (fx+ i start) val))))\n");

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

/** Define Scheme type "span", a resizeable vector */
static void define_span_functions(void) {
  eval("(begin\n"
       "  (define list->span)\n"
       "  (define vector->span)\n"
       "  (define make-span)\n"
       "  (define span->vector)\n"
       "  (define span)\n"
       "  (define span?)\n"
       "  (define span-length)\n"
       "  (define span-capacity)\n"
       "  (define span-empty?)\n"
       "  (define span-ref)\n"
       "  (define span-set!)\n"
       "  (define span-last)\n"
       "  (define span-fill!)\n"
       "  (define span-fill-range!)\n"
       "  (define span-copy)\n"
       "  (define span-copy!)\n"
       "  (define span-capacity-set!)\n"
       "  (define span-length-set!)\n"
       "  (define span-append!)\n"
       "  (define span-erase-front!)\n"
       "  (define span-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%span %make-span %span?)\n"
       "  (fields\n"
       "     (mutable beg span-beg span-beg-set!)\n"
       "     (mutable end span-end span-end-set!)\n"
       "     (mutable vec span-vec span-vec-set!))\n"
       "  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))\n"
       "\n"
       "(set! list->span (lambda (l)\n"
       "  (let ((vec (list->vector l)))\n"
       "    (%make-span 0 (vector-length vec) vec))))\n"
       "\n"
       "(set! vector->span (lambda (vec)\n"
       "  (%make-span 0 (vector-length vec) (vector-copy vec))))\n"
       "\n"
       "(set! make-span (lambda (n . val)\n"
       "  (%make-span 0 n (apply make-vector n val))))\n"
       "\n"
       "(set! span->vector (lambda (sp)\n"
       "  (subvector (span-vec sp) (span-beg sp) (span-end sp))))\n"
       "\n"
       "(set! span (lambda vals\n"
       "  (list->span vals)))\n"
       "\n"
       "(set! span? (lambda (sp)\n"
       "  (%span? sp)))\n"
       "\n"
       "(set! span-length (lambda (sp)\n"
       "  (fx- (span-end sp) (span-beg sp))))\n"
       "\n"
       "(set! span-capacity (lambda (sp)\n"
       "  (vector-length (span-vec sp))))\n"
       "\n"
       "(set! span-empty? (lambda (sp)\n"
       "  (fx>= (span-beg sp) (span-end sp))))\n"
       "\n"
       "(set! span-ref (lambda (sp n)\n"
       "  (assert (fx<? n (span-length sp)))\n"
       "  (vector-ref (span-vec sp) (fx+ n (span-beg sp)))))\n"
       "\n"
       "(set! span-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (span-end sp)))\n"
       "  (vector-set! (span-vec sp) (fx+ n (span-beg sp)) val)))\n"
       "\n"
       "(set! span-last (lambda (sp)\n"
       "  (assert (not (span-empty? sp)))\n"
       "  (vector-ref (span-vec sp) (fx1- (span-end sp)))))\n"
       "\n"
       "(set! span-fill! (lambda (sp val)\n"
       /* no optimized function to fill only between span-beg and span-end,
        * so fill the whole vector */
       "  (vector-fill! (span-vec sp) val)))\n"
       "\n"
       "(set! span-fill-range! (lambda (sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (span-length sp)))\n"
       "  (vector-fill-range! (span-vec sp) (fx+ start (span-beg sp)) n val)))\n"
       "\n"
       "(set! span-copy (lambda (src)\n"
       "  (let* ((n (span-length src))\n"
       "         (dst (make-span n)))\n"
       "    (vector-copy! (span-vec src) (span-beg src)\n"
       "                  (span-vec dst) (span-beg dst) n)\n"
       "    dst)))\n"
       "\n"
       "(set! span-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (span-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (span-length dst)))\n"
       "  (vector-copy! (span-vec src) (fx+ src-start (span-beg src))\n"
       "                (span-vec dst) (fx+ dst-start (span-beg dst)) n)))\n"
       "\n"
       "(set! span-capacity-set! (lambda (sp n)\n"
       "  (assert (fx>=? n (span-length sp)))\n"
       "  (unless (fx=? n (span-capacity sp))\n"
       "    (let* ((len (span-length sp))\n"
       "           (old-vec (span-vec sp))\n"
       "           (new-vec (make-vector n)))\n"
       "      (vector-copy! old-vec (span-beg sp) new-vec 0 len)\n"
       "      (span-beg-set! sp 0)\n"
       "      (span-end-set! sp len)\n"
       "      (span-vec-set! sp new-vec)))))\n"
       "\n"
       "(set! span-length-set! (lambda (sp n)\n"
       "  (when (fx>? n (span-capacity sp))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (span-capacity sp)))))\n"
       "      (span-capacity-set! sp new-cap)))\n"
       "  (span-end-set! sp (fx+ n (span-beg sp)))))\n"
       "\n"
       "(set! span-append! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((n   (length vals))\n"
       "          (pos (span-length sp)))\n"
       "      (span-length-set! sp (fx+ pos n))\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (span-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       /* erase n elements at the front of span */
       "(set! span-erase-front! (lambda (sp n)\n"
       "  (assert (fx>= n 0))\n"
       "  (assert (fx<= n (span-length sp)))\n"
       "  (cond\n"
       "    ((fx=? n 0))\n"
       "    ((fx<? n (span-length sp))\n"
       "      (span-beg-set! sp (fx+ n (span-beg sp))))\n"
       "    (#t\n"
       "      (span-beg-set! sp 0)\n"
       "      (span-end-set! sp 0)))))\n"
       "\n"
       "(set! span-iterate (lambda (sp proc)\n"
       "  (do ((i (span-beg sp) (fx1+ i))\n"
       "       (n (span-end sp))\n"
       "       (v (span-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (vector-ref v i))))))))\n"
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
       ")\n");

  /**
   * (span-find) iterates on span elements from start to (fxmin (fx+ start n) (span-length sp)),
   * and returns the index of first span element that causes (predicate elem)
   * to return non-#f.
   * Returns #f if no such element is found.
   */
  eval("(define (span-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (span-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (span-ref sp i))\n"
       "        (set! ret i)))))\n");
}

/** Define Scheme type "bytespan", a resizeable bytevector */
static void define_bytespan_functions(void) {
  eval("(begin\n"
       "  (define bytevector->bytespan)\n"
       "  (define list->bytespan)\n"
       "  (define make-bytespan)\n"
       "  (define bytespan->bytevector)\n"
       "  (define bytespan)\n"
       "  (define bytespan?)\n"
       "  (define bytespan-length)\n"
       "  (define bytespan-capacity)\n"
       "  (define bytespan-underlying)\n" /* return underlying bytevector */
       "  (define bytespan-empty?)\n"
       "  (define bytespan-u8-ref)\n"
       "  (define bytespan-u8-set!)\n"
       "  (define bytespan-u8-last)\n"
       "  (define bytespan-fill!)\n"
       "  (define bytespan-fill-range!)\n"
       "  (define bytespan-copy)\n"
       "  (define bytespan-copy!)\n"
       "  (define bytespan-capacity-set!)\n"
       "  (define bytespan-length-set!)\n"
       "  (define bytespan-u8-append!)\n"
       "  (define bytespan-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%bytespan %make-bytespan %bytespan?)\n"
       "  (fields\n"
       "     (mutable len bytespan-len bytespan-len-set!)\n"
       "     (mutable vec bytespan-vec bytespan-vec-set!))\n"
       "  (nongenerative #{%bytespan mu5r2go07bretxssmue01kt46-0}))\n"
       "\n"
       "(set! bytevector->bytespan (lambda (vec)\n"
       "  (%make-bytespan (bytevector-length vec) (bytevector-copy vec))))\n"
       "\n"
       "(set! list->bytespan (lambda (l)\n"
       "  (let ((vec (apply bytevector l)))\n"
       "    (%make-bytespan (bytevector-length vec) vec))))\n"
       "\n"
       "(set! make-bytespan (lambda (n . val)\n"
       "  (%make-bytespan n (apply make-bytevector n val))))\n"
       "\n"
       "(set! bytespan->bytevector (lambda (sp)\n"
       "  (let* ((n (bytespan-len sp))\n"
       "         (dst (make-bytevector n)))\n"
       "    (bytevector-copy! (bytespan-vec sp) 0 dst 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! bytespan (lambda vals\n"
       "  (list->bytespan vals)))\n"
       "\n"
       "(set! bytespan? (lambda (sp)\n"
       "   (%bytespan? sp)))\n"
       "\n"
       "(set! bytespan-length (lambda (sp)\n"
       "  (bytespan-len sp)))\n"
       "\n"
       "(set! bytespan-capacity (lambda (sp)\n"
       "  (bytevector-length (bytespan-vec sp))))\n"
       "\n"
       "(set! bytespan-underlying (lambda (sp)\n"
       "  (bytespan-vec sp)))\n"
       "\n"
       "(set! bytespan-empty? (lambda (sp)\n"
       "  (fxzero? (bytespan-len sp))))\n"
       "\n"
       "(set! bytespan-u8-ref (lambda (sp n)\n"
       "  (assert (fx<? n (bytespan-len sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) n)))\n"
       "\n"
       "(set! bytespan-u8-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (bytespan-len sp)))\n"
       "  (bytevector-u8-set! (bytespan-vec sp) n val)))\n"
       "\n"
       "(set! bytespan-u8-last (lambda (sp)\n"
       "  (assert (not (bytespan-empty? sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-len sp)))))\n"
       "\n"
       "(set! bytespan-fill! (lambda (sp val)\n"
       /* no optimized function to fill only up to bytespan-len,
        * so fill up to bytespan-capacity */
       "  (bytevector-fill! (bytespan-vec sp) val)))\n"
       "\n"
       "(set! bytespan-fill-range! (lambda (sp start n val)\n"
       "  (assert (fx<=? (fx+ src n) (bytespan-len sp)))\n"
       "  (bytevector-fill-range! (bytespan-vec sp) start n val)))\n"
       "\n"
       "(set! bytespan-copy (lambda (src)\n"
       "  (let* ((n (bytespan-length src))\n"
       "         (dst (make-bytespan n)))\n"
       "    (bytevector-copy! (bytespan-vec src) 0 (bytespan-vec dst) 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! bytespan-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx<=? (fx+ src-start n) (bytespan-len src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (bytespan-len dst)))\n"
       "  (bytevector-copy! (bytespan-vec src) src-start (bytespan-vec dst) dst-start n)))\n"
       "\n"
       "(set! bytespan-capacity-set! (lambda (sp n)\n"
       "  (assert (fx>=? n (bytespan-len sp)))\n"
       "  (unless (fx=? n (bytespan-capacity sp))\n"
       "    (let* ((len (bytespan-len sp))\n"
       "           (old-vec (bytespan-vec sp))\n"
       "           (new-vec (make-bytevector n)))\n"
       "      (bytevector-copy! old-vec 0 new-vec 0 len)\n"
       "      (bytespan-vec-set! sp new-vec)))))\n"
       "\n"
       "(set! bytespan-length-set! (lambda (sp n)\n"
       "  (when (fx>? n (bytespan-capacity sp))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (bytespan-capacity sp)))))\n"
       "      (bytespan-capacity-set! sp new-cap)))\n"
       "  (bytespan-len-set! sp n)))\n"
       "\n"
       "(set! bytespan-u8-append! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((n (length vals))\n"
       "          (pos    (bytespan-len sp)))\n"
       "      (bytespan-length-set! sp (fx+ pos n))\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (bytespan-u8-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! bytespan-iterate (lambda (sp proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (bytespan-len sp))\n"
       "       (v (bytespan-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (bytevector-u8-ref v i))))))))\n"
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
       ")\n");

  /**
   * (bytespan-u8-find) iterates on bytespan elements from start to (fxmin (fx+ start n)
   * (bytespan-length sp)), and returns the index of first bytespan element that causes
   * (predicate elem) to return non-#f. Returns #f if no such element is found.
   */
  eval("(define (bytespan-u8-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (bytespan-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (bytespan-u8-ref sp i))\n"
       "        (set! ret i)))))\n");
}

/** Define Scheme type "charspan", a resizeable string */
static void define_charspan_functions(void) {
  eval("(begin\n"
       "  (define list->charspan)\n"
       "  (define string->charspan)\n"
       "  (define make-charspan)\n"
       "  (define charspan->string)\n"
       "  (define charspan)\n"
       "  (define charspan?)\n"
       "  (define charspan-length)\n"
       "  (define charspan-capacity)\n"
       "  (define charspan-empty?)\n"
       "  (define charspan-ref)\n"
       "  (define charspan-set!)\n"
       "  (define charspan-last)\n"
       "  (define charspan-fill!)\n"
       "  (define charspan-fill-range!)\n"
       "  (define charspan-copy)\n"
       "  (define charspan-copy!)\n"
       "  (define charspan-capacity-set!)\n"
       "  (define charspan-length-set!)\n"
       "  (define charspan-append!)\n"
       "  (define charspan-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%charspan %make-charspan %charspan?)\n"
       "  (fields\n"
       "     (mutable len charspan-len charspan-len-set!)\n"
       "     (mutable vec charspan-vec charspan-vec-set!))\n"
       "  (nongenerative #{%charspan g8i3d8unk70r0jnvh6n2uil2a-0}))\n"
       "\n"
       "(set! list->charspan (lambda (l)\n"
       "  (let ((vec (apply string l)))\n"
       "    (%make-charspan (string-length vec) vec))))\n"
       "\n"
       "(set! string->charspan (lambda (vec)\n"
       "  (%make-charspan (string-length vec) (string-copy vec))))\n"
       "\n"
       "(set! make-charspan (lambda (len . fillchar)\n"
       "  (%make-charspan len (apply make-string len fillchar))))\n"
       "\n"
       "(set! charspan->string (lambda (sp)\n"
       "  (substring (charspan-vec sp) 0 (charspan-len sp))))\n"
       "\n"
       "(set! charspan (lambda vals\n"
       "  (list->charspan vals)))\n"
       "\n"
       "(set! charspan? (lambda (sp)\n"
       "  (%charspan? sp)))\n"
       "\n"
       "(set! charspan-length (lambda (sp)\n"
       "  (charspan-len sp)))\n"
       "\n"
       "(set! charspan-capacity (lambda (sp)\n"
       "  (string-length (charspan-vec sp))))\n"
       "\n"
       "(set! charspan-empty? (lambda (sp)\n"
       "  (fxzero? (charspan-len sp))))\n"
       "\n"
       "(set! charspan-ref (lambda (sp n)\n"
       "  (assert (fx<? n (charspan-len sp)))\n"
       "  (string-ref (charspan-vec sp) n)))\n"
       "\n"
       "(set! charspan-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (charspan-len sp)))\n"
       "  (string-set! (charspan-vec sp) n val)))\n"
       "\n"
       "(set! charspan-last (lambda (sp)\n"
       "  (assert (not (charspan-empty? sp)))\n"
       "  (string-ref (charspan-vec sp) (fx1- (charspan-len sp)))))\n"
       "\n"
       "(set! charspan-fill! (lambda (sp val)\n"
       /* no optimized function to fill only up to charspan-len,
        * so fill up to charspan-capacity */
       "  (string-fill! (charspan-vec sp) val)))\n"
       "\n"
       "(set! charspan-fill-range! (lambda (sp start n val)\n"
       "  (assert (fx<=? (fx+ src n) (charspan-len sp)))\n"
       "  (substring-fill! (charspan-vec sp) start n val)))\n"
       "\n"
       "(set! charspan-copy (lambda (src)\n"
       "  (let* ((n (charspan-length src))\n"
       "         (dst (make-charspan n)))\n"
       "    (string-copy! (charspan-vec src) 0 (charspan-vec dst) 0 n)\n"
       "    dst)))\n"
       "\n"
       "(set! charspan-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx<=? (fx+ src-start n) (charspan-len src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (charspan-len dst)))\n"
       "  (string-copy! (charspan-vec src) src-start (charspan-vec dst) dst-start n)))\n"
       "\n"
       "(set! charspan-capacity-set! (lambda (sp n)\n"
       "  (assert (fx>=? n (charspan-len sp)))\n"
       "  (unless (fx=? n (charspan-capacity sp))\n"
       "    (let* ((len (charspan-len sp))\n"
       "           (old-vec (charspan-vec sp))\n"
       "           (new-vec (make-string n)))\n"
       "      (string-copy! old-vec 0 new-vec 0 len)\n"
       "      (charspan-vec-set! sp new-vec)))))\n"
       "\n"
       "(set! charspan-length-set! (lambda (sp n)\n"
       "  (when (fx>? n (charspan-capacity sp))\n"
       "    (let ((new-cap (fxmax 8 n (fx* 2 (charspan-capacity sp)))))\n"
       "      (charspan-capacity-set! sp new-cap)))\n"
       "  (charspan-len-set! sp n)))\n"
       "\n"
       "(set! charspan-append! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((n (length vals))\n"
       "          (pos    (charspan-len sp)))\n"
       "      (charspan-length-set! sp (fx+ pos n))\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (charspan-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! charspan-iterate (lambda (sp proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (charspan-len sp))\n"
       "       (v (charspan-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (string-ref v i))))))))\n"
       "\n"
       /** customize how "charspan" objects are printed */
       "(record-writer (record-type-descriptor %charspan)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(string->charspan \" port)\n"
       "    (write (substring (charspan-vec sp) 0 (charspan-len sp)) port)\n"
       "    (display #\\) port)))\n"
       ")\n");

  /**
   * (charspan-find) iterates on charspan elements from start to (fxmin (fx+ start n)
   * (charspan-length sp)), and returns the index of first charspan element that causes
   * (predicate elem) to return non-#f. Returns #f if no such element is found.
   */
  eval("(define (charspan-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (charspan-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (charspan-ref sp i))\n"
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
  define_span_functions();
  define_bytevector_functions();
  define_bytespan_functions();
  define_charspan_functions();
  define_hash_functions();
  define_list_functions();
}
