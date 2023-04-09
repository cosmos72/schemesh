/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <string.h> // memmove()

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

/** define some additional vector functions */
static void define_vector_functions(void) {
  Sregister_symbol("c_vector_copy", &c_vector_copy);

  /**
   * copy a portion of vector src into dst.
   * works even if src are the same vector and the two ranges overlap.
   */
  eval("(begin\n"
       "\n"
       "(define vector-copy!)\n"
       "\n"
       "(parameterize ((optimize-level 3))\n"
       "\n"
       "(set! vector-copy!\n"
       "  (let ((c-vector-copy (foreign-procedure \"c_vector_copy\"\n"
       "     (scheme-object fixnum scheme-object fixnum fixnum)"
       "     void)))\n"
       "    (lambda (src src-start dst dst-start n)\n"
       "      (assert (and (vector? src) (vector? dst)))\n"
       "      (assert (and (fixnum? src-start) (fixnum? dst-start) (fixnum? n)))\n"
       "      (assert (and (fx>=? src-start 0) (fx>=? dst-start 0) (fx>=? n 0)))\n"
       "      (assert (fx<=? src-start (vector-length src)))\n"
       "      (assert (fx<=? dst-start (vector-length dst)))\n"
       "      (assert (fx<=? n (fx- (vector-length src) src-start)))\n"
       "      (assert (fx<=? n (fx- (vector-length dst) dst-start)))\n"
       "      (cond\n"
       "       ((fx>=? n 2)\n"
       "         (c-vector-copy src src-start dst dst-start n))\n"
       "       ((fx=? n 1)\n"
       "         (let ((elem (vector-ref src src-start)))\n"
       "           (vector-set! dst dst-start elem)))\n"
       "       (#t\n"
       "         (assert (fx>=? n 0)))))))\n"
       "))\n");

  /**
   * return a copy of vector vec containing only elements
   * from start (inclusive) to end (exclusive)
   */
  eval("(define (subvector vec start end)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? end 0))\n"
       "  (assert (fx<=? start end))\n"
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
  eval("(define (list->bytevector l)\n"
       "  (apply bytevector l))\n");

  /**
   * return a copy of bytevector vec containing only elements
   * from start (inclusive) to end (exclusive)
   */
  eval("(define (subbytevector vec start end)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? end 0))\n"
       "  (assert (fx<=? start end))\n"
       "  (let* ((n (fx- end start))\n"
       "         (dst (make-bytevector n)))\n"
       "    (bytevector-copy! vec start dst 0 n)\n"
       "    dst))\n");

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
       /* return length of internal vector, i.e. maximum number of elements
        * that can be stored without reallocating */
       "  (define span-capacity)\n"
       "  (define span-empty?)\n"
       "  (define span-ref)\n"
       "  (define span-set!)\n"
       "  (define span-back)\n"
       "  (define span-fill!)\n"
       "  (define span-fill-range!)\n"
       "  (define span-copy)\n"
       "  (define span-copy!)\n"
       /* return distance between begin of internal vector and last element */
       "  (define span-capacity-front)\n"
       /* return distance between first element and end of internal vector */
       "  (define span-capacity-back)\n"
       /* ensure distance between begin of internal vector and last element is >= n.
        * does NOT change the length */
       "  (define span-reserve-front!)\n"
       /* ensure distance between first element and end of internal vector is >= n.
        * does NOT change the length */
       "  (define span-reserve-back!)\n"
       /* grow or shrink span on the left (front), set length to n */
       "  (define span-resize-front!)\n"
       /* grow or shrink span on the right (back), set length to n */
       "  (define span-resize-back!)\n"
       "  (define span-insert-front!)\n"
       "  (define span-insert-back!)\n"
       /* prefix a portion of another span to this span */
       "  (define span-sp-insert-front!)\n"
       /* append a portion of another span to this span */
       "  (define span-sp-insert-back!)\n"
       /* erase n elements at the left (front) of span */
       "  (define span-erase-front!)\n"
       /* erase n elements at the right (back) of span */
       "  (define span-erase-back!)\n"
       "  (define span-iterate))\n");

  eval("(let ((span-reallocate-front! (void))\n"
       "      (span-reallocate-back! (void)))\n"
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
       "  (fx>=? (span-beg sp) (span-end sp))))\n"
       "\n"
       "(set! span-ref (lambda (sp n)\n"
       "  (assert (fx<? n (span-length sp)))\n"
       "  (vector-ref (span-vec sp) (fx+ n (span-beg sp)))))\n"
       "\n"
       "(set! span-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (span-end sp)))\n"
       "  (vector-set! (span-vec sp) (fx+ n (span-beg sp)) val)))\n"
       "\n"
       "(set! span-back (lambda (sp)\n"
       "  (assert (not (span-empty? sp)))\n"
       "  (vector-ref (span-vec sp) (fx1- (span-end sp)))))\n"
       "\n"
       "(set! span-fill! (lambda (sp val)\n"
       "  (vector-fill-range! (span-vec sp) (span-beg sp) (span-length sp) val)))\n"
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
       "(set! span-reallocate-front! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (span-length sp)))\n"
       "        (old-vec (span-vec sp))\n"
       "        (new-vec (make-vector cap))\n"
       "        (new-beg (fx- cap len)))\n"
       "    (vector-copy! old-vec (span-beg sp) new-vec new-beg copy-len)\n"
       "    (span-beg-set! sp new-beg)\n"
       "    (span-end-set! sp cap)\n"
       "    (span-vec-set! sp new-vec))))\n"
       "\n"
       "(set! span-reallocate-back! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (span-length sp)))\n"
       "        (old-vec (span-vec sp))\n"
       "        (new-vec (make-vector cap)))\n"
       "    (vector-copy! old-vec (span-beg sp) new-vec 0 copy-len)\n"
       "    (span-beg-set! sp 0)\n"
       "    (span-end-set! sp len)\n"
       "    (span-vec-set! sp new-vec))))\n"
       "\n"
       "(set! span-capacity-front (lambda (sp)\n"
       "  (span-end sp)))\n"
       "\n"
       "(set! span-capacity-back (lambda (sp)\n"
       "  (fx- (vector-length (span-vec sp)) (span-beg sp))))\n"
       "\n"
       "(set! span-reserve-front! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (span-vec sp))\n"
       "        (cap-front (span-capacity-front sp)))\n"
       "    (cond\n"
       "      ((fx<= len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (vector-length vec))\n"
       /*       vector is large enough, move elements to the back */
       "        (let* ((cap (span-capacity sp))\n"
       "               (len (span-length sp))\n"
       "               (new-beg (fx- cap old-len)))\n"
       "          (vector-copy! vec (span-beg sp) vec new-beg old-len)\n"
       "          (span-beg-set! sp new-beg)\n"
       "          (span-end-set! sp cap)))\n"
       "      (#t\n"
       /*       vector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))\n"
       "         (span-reallocate-front! sp (span-length sp) new-cap)))))))\n"
       "\n"
       "(set! span-reserve-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (span-vec sp))\n"
       "        (cap-back (span-capacity-back sp)))\n"
       "    (cond\n"
       "      ((fx<= len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (vector-length vec))\n"
       /*       vector is large enough, move elements to the front */
       "        (let ((len (span-length sp)))\n"
       "          (vector-copy! vec (span-beg sp) vec 0 len)\n"
       "          (span-beg-set! sp 0)\n"
       "          (span-end-set! sp len)))\n"
       "      (#t\n"
       /*       vector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "         (span-reallocate-back! sp (span-length sp) new-cap)))))))\n"
       "\n"
       "(set! span-resize-front! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-front! sp len)\n"
       "  (assert (fx>= (span-capacity-front sp) len))\n"
       "  (span-beg-set! sp (fx- (span-end sp) len))))\n"
       "\n"
       "(set! span-resize-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-back! sp len)\n"
       "  (assert (fx>= (span-capacity-back sp) len))\n"
       "  (span-end-set! sp (fx+ len (span-beg sp)))))\n"
       "\n"
       "(set! span-insert-front! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((pos 0)\n"
       "          (new-len (fx+ (span-length sp) (length vals))))\n"
       "      (span-resize-front! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (span-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! span-insert-back! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let* ((pos (span-length sp))\n"
       "           (new-len (fx+ pos (length vals))))\n"
       "      (span-resize-back! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (span-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! span-sp-insert-front! (lambda (sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((len (span-length sp-dst)))\n"
       "      (span-resize-front! sp-dst (fx+ len src-n))\n"
       "      (span-copy! sp-src src-start sp-dst 0 src-n)))))\n"
       "\n"
       "(set! span-sp-insert-back! (lambda (sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (span-length sp-dst)))\n"
       "      (span-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (span-copy! sp-src src-start sp-dst pos src-n)))))\n"
       "\n"
       "(set! span-erase-front! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (span-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (span-beg-set! sp (fx+ n (span-beg sp))))))\n"
       "\n"
       "(set! span-erase-back! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (span-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (span-end-set! sp (fx- (span-end sp) n)))))\n"
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
       "  (define list->bytespan)\n"
       "  (define bytevector->bytespan)\n"
       "  (define make-bytespan)\n"
       "  (define bytespan->bytevector)\n"
       "  (define bytespan)\n"
       "  (define bytespan?)\n"
       "  (define bytespan-length)\n"
       "  (define bytespan-empty?)\n"
       "  (define bytespan-peek-beg)\n"  /* return start offset into internal bytevector */
       "  (define bytespan-peek-end)\n"  /* return end offset into internal bytevector */
       "  (define bytespan-peek-data)\n" /* return internal bytevector */
       "  (define bytespan-u8-ref)\n"
       "  (define bytespan-u8-set!)\n"
       "  (define bytespan-u8-back)\n"
       "  (define bytespan-fill!)\n"
       "  (define bytespan-fill-range!)\n"
       "  (define bytespan-copy)\n"
       "  (define bytespan-copy!)\n"
       /* return distance between begin of internal bytevector and last element */
       "  (define bytespan-capacity-front)\n"
       /* return distance between first element and end of internal bytevector */
       "  (define bytespan-capacity-back)\n"
       /* ensure distance between begin of internal bytevector and last element is >= n.
        * does NOT change the length */
       "  (define bytespan-reserve-front!)\n"
       /* ensure distance between first element and end of internal bytevector is >= n.
        * does NOT change the length */
       "  (define bytespan-reserve-back!)\n"
       /* grow or shrink bytespan on the left (front), set length to n */
       "  (define bytespan-resize-front!)\n"
       /* grow or shrink bytespan on the right (back), set length to n */
       "  (define bytespan-resize-back!)\n"
       "  (define bytespan-u8-insert-front!)\n"
       "  (define bytespan-u8-insert-back!)\n"
       /* prefix a portion of another bytespan to this bytespan */
       "  (define bytespan-bsp-insert-front!)\n"
       /* append a portion of another bytespan to this bytespan */
       "  (define bytespan-bsp-insert-back!)\n"
       /* erase n elements at the left (front) of bytespan */
       "  (define bytespan-erase-front!)\n"
       /* erase n elements at the right (back) of bytespan */
       "  (define bytespan-erase-back!)\n"
       "  (define bytespan-iterate))\n");

  eval("(let ((bytespan-reallocate-front! (void))\n"
       "      (bytespan-reallocate-back! (void)))\n"
       "\n"
       "(define-record-type\n"
       "  (%bytespan %make-bytespan %bytespan?)\n"
       "  (fields\n"
       "     (mutable beg bytespan-beg bytespan-beg-set!)\n"
       "     (mutable end bytespan-end bytespan-end-set!)\n"
       "     (mutable vec bytespan-vec bytespan-vec-set!))\n"
       "  (nongenerative #{%bytespan 1j9oboeqc5j4db1bamcd28yz-0}))\n"
       "\n"
       "(set! list->bytespan (lambda (l)\n"
       "  (let ((vec (list->bytevector l)))\n"
       "    (%make-bytespan 0 (bytevector-length vec) vec))))\n"
       "\n"
       "(set! bytevector->bytespan (lambda (vec)\n"
       "  (%make-bytespan 0 (bytevector-length vec) (bytevector-copy vec))))\n"
       "\n"
       "(set! make-bytespan (lambda (n . val)\n"
       "  (%make-bytespan 0 n (apply make-bytevector n val))))\n"
       "\n"
       "(set! bytespan->bytevector (lambda (sp)\n"
       "  (subbytevector (bytespan-vec sp) (bytespan-beg sp) (bytespan-end sp))))\n"
       "\n"
       "(set! bytespan (lambda vals\n"
       "  (list->bytespan vals)))\n"
       "\n"
       "(set! bytespan? (lambda (sp)\n"
       "  (%bytespan? sp)))\n"
       "\n"
       "(set! bytespan-length (lambda (sp)\n"
       "  (fx- (bytespan-end sp) (bytespan-beg sp))))\n"
       "\n"
       "(set! bytespan-empty? (lambda (sp)\n"
       "  (fx>=? (bytespan-beg sp) (bytespan-end sp))))\n"
       "\n"
       "(set! bytespan-peek-beg (lambda (sp)\n"
       "  (bytespan-beg sp)))\n"
       "\n"
       "(set! bytespan-peek-end (lambda (sp)\n"
       "  (bytespan-end sp)))\n"
       "\n"
       "(set! bytespan-peek-data (lambda (sp)\n"
       "  (bytespan-vec sp)))\n"
       "\n"
       "(set! bytespan-u8-ref (lambda (sp n)\n"
       "  (assert (fx<? n (bytespan-length sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx+ n (bytespan-beg sp)))))\n"
       "\n"
       "(set! bytespan-u8-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (bytespan-end sp)))\n"
       "  (bytevector-u8-set! (bytespan-vec sp) (fx+ n (bytespan-beg sp)) val)))\n"
       "\n"
       "(set! bytespan-u8-back (lambda (sp)\n"
       "  (assert (not (bytespan-empty? sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-end sp)))))\n"
       "\n"
       "(set! bytespan-fill! (lambda (sp val)\n"
       "  (bytevector-fill-range! (bytespan-vec sp) (bytespan-beg sp) (bytespan-length sp) val)))\n"
       "\n"
       "(set! bytespan-fill-range! (lambda (sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (bytespan-length sp)))\n"
       "  (bytevector-fill-range! (bytespan-vec sp) (fx+ start (bytespan-beg sp)) n val)))\n"
       "\n"
       "(set! bytespan-copy (lambda (src)\n"
       "  (let* ((n (bytespan-length src))\n"
       "         (dst (make-bytespan n)))\n"
       "    (bytevector-copy! (bytespan-vec src) (bytespan-beg src)\n"
       "                      (bytespan-vec dst) (bytespan-beg dst) n)\n"
       "    dst)))\n"
       "\n"
       "(set! bytespan-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (bytespan-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (bytespan-length dst)))\n"
       "  (bytevector-copy! (bytespan-vec src) (fx+ src-start (bytespan-beg src))\n"
       "                    (bytespan-vec dst) (fx+ dst-start (bytespan-beg dst)) n)))\n"
       "\n"
       "(set! bytespan-reallocate-front! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (bytespan-length sp)))\n"
       "        (old-vec (bytespan-vec sp))\n"
       "        (new-vec (make-bytevector cap))\n"
       "        (new-beg (fx- cap len)))\n"
       "    (bytevector-copy! old-vec (bytespan-beg sp) new-vec new-beg copy-len)\n"
       "    (bytespan-beg-set! sp new-beg)\n"
       "    (bytespan-end-set! sp cap)\n"
       "    (bytespan-vec-set! sp new-vec))))\n"
       "\n"
       "(set! bytespan-reallocate-back! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (bytespan-length sp)))\n"
       "        (old-vec (bytespan-vec sp))\n"
       "        (new-vec (make-bytevector cap)))\n"
       "    (bytevector-copy! old-vec (bytespan-beg sp) new-vec 0 copy-len)\n"
       "    (bytespan-beg-set! sp 0)\n"
       "    (bytespan-end-set! sp len)\n"
       "    (bytespan-vec-set! sp new-vec))))\n"
       "\n"
       "(set! bytespan-capacity-front (lambda (sp)\n"
       "  (bytespan-end sp)))\n"
       "\n"
       "(set! bytespan-capacity-back (lambda (sp)\n"
       "  (fx- (bytevector-length (bytespan-vec sp)) (bytespan-beg sp))))\n"
       "\n"
       "(set! bytespan-reserve-front! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (bytespan-vec sp))\n"
       "        (cap-front (bytespan-capacity-front sp)))\n"
       "    (cond\n"
       "      ((fx<= len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (bytevector-length vec))\n"
       /*       bytevector is large enough, move elements to the back */
       "        (let* ((cap (bytespan-capacity sp))\n"
       "               (len (bytespan-length sp))\n"
       "               (new-beg (fx- cap old-len)))\n"
       "          (bytevector-copy! vec (bytespan-beg sp) vec new-beg old-len)\n"
       "          (bytespan-beg-set! sp new-beg)\n"
       "          (bytespan-end-set! sp cap)))\n"
       "      (#t\n"
       /*       bytevector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))\n"
       "         (bytespan-reallocate-front! sp (bytespan-length sp) new-cap)))))))\n"
       "\n"
       "(set! bytespan-reserve-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((vec (bytespan-vec sp))\n"
       "        (cap-back (bytespan-capacity-back sp)))\n"
       "    (cond\n"
       "      ((fx<= len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (bytevector-length vec))\n"
       /*       bytevector is large enough, move elements to the front */
       "        (let ((len (bytespan-length sp)))\n"
       "          (bytevector-copy! vec (bytespan-beg sp) vec 0 len)\n"
       "          (bytespan-beg-set! sp 0)\n"
       "          (bytespan-end-set! sp len)))\n"
       "      (#t\n"
       /*       bytevector is too small, reallocate it */
       "       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "         (bytespan-reallocate-back! sp (bytespan-length sp) new-cap)))))))\n"
       "\n"
       "(set! bytespan-resize-front! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-front! sp len)\n"
       "  (assert (fx>= (bytespan-capacity-front sp) len))\n"
       "  (bytespan-beg-set! sp (fx- (bytespan-end sp) len))))\n"
       "\n"
       "(set! bytespan-resize-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-back! sp len)\n"
       "  (assert (fx>= (bytespan-capacity-back sp) len))\n"
       "  (bytespan-end-set! sp (fx+ len (bytespan-beg sp)))))\n"
       "\n"
       "(set! bytespan-u8-insert-front! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((pos 0)\n"
       "          (new-len (fx+ (bytespan-length sp) (length vals))))\n"
       "      (bytespan-resize-front! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (bytespan-u8-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! bytespan-u8-insert-back! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let* ((pos (bytespan-length sp))\n"
       "           (new-len (fx+ pos (length vals))))\n"
       "      (bytespan-resize-back! sp new-len)\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (bytespan-u8-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! bytespan-bsp-insert-front! (lambda (sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((len (bytespan-length sp-dst)))\n"
       "      (bytespan-resize-front! sp-dst (fx+ len src-n))\n"
       "      (bytespan-copy! sp-src src-start sp-dst 0 src-n)))))\n"
       "\n"
       "(set! bytespan-bsp-insert-back! (lambda (sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (bytespan-length sp-dst)))\n"
       "      (bytespan-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (bytespan-copy! sp-src src-start sp-dst pos src-n)))))\n"
       "\n"
       "(set! bytespan-erase-front! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (bytespan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (bytespan-beg-set! sp (fx+ n (bytespan-beg sp))))))\n"
       "\n"
       "(set! bytespan-erase-back! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (bytespan-length sp)))\n"
       "  (unless (fxzero? n)\n"
       "    (bytespan-end-set! sp (fx- (bytespan-end sp) n)))))\n"
       "\n"
       "(set! bytespan-iterate (lambda (sp proc)\n"
       "  (do ((i (bytespan-beg sp) (fx1+ i))\n"
       "       (n (bytespan-end sp))\n"
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
   * (bytespan-find) iterates on bytespan elements from start to (fxmin (fx+ start n)
   * (bytespan-length sp)), and returns the index of first bytespan element that causes (predicate
   * elem) to return non-#f. Returns #f if no such element is found.
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
       "  (define charspan-empty?)\n"
       "  (define charspan-ref)\n"
       "  (define charspan-set!)\n"
       "  (define charspan-back)\n"
       "  (define charspan-fill!)\n"
       "  (define charspan-fill-range!)\n"
       "  (define charspan-copy)\n"
       "  (define charspan-copy!)\n"
       /* return distance between first element and end of internal string */
       "  (define charspan-capacity-back)\n"
       /* ensure distance between first element and end of internal string is >= n */
       "  (define charspan-reserve-back!)\n"
       /* grow or shrink span on the right (back), set length to n */
       "  (define charspan-resize-back!)\n"
       "  (define charspan-insert-back!)\n"
       /* append a portion of another charspan to this charspan */
       "  (define charspan-csp-insert-back!)\n"
       "  (define charspan-erase-front!)\n"
       "  (define charspan-iterate))\n");

  eval("(let ((charspan-reallocate! (void)))\n"
       "\n"
       "(define-record-type\n"
       "  (%charspan %make-charspan %charspan?)\n"
       "  (fields\n"
       "     (mutable beg charspan-beg charspan-beg-set!)\n"
       "     (mutable end charspan-end charspan-end-set!)\n"
       "     (mutable vec charspan-vec charspan-vec-set!))\n"
       "  (nongenerative #{%charspan b847ikzm9lftljwelbq0cknyh-0}))\n"
       "\n"
       "(set! list->charspan (lambda (l)\n"
       "  (let ((vec (list->string l)))\n"
       "    (%make-charspan 0 (string-length vec) vec))))\n"
       "\n"
       "(set! string->charspan (lambda (vec)\n"
       "  (%make-charspan 0 (string-length vec) (string-copy vec))))\n"
       "\n"
       "(set! make-charspan (lambda (n . val)\n"
       "  (%make-charspan 0 n (apply make-string n val))))\n"
       "\n"
       "(set! charspan->string (lambda (sp)\n"
       "  (substring (charspan-vec sp) (charspan-beg sp) (charspan-end sp))))\n"
       "\n"
       "(set! charspan (lambda vals\n"
       "  (list->charspan vals)))\n"
       "\n"
       "(set! charspan? (lambda (sp)\n"
       "  (%charspan? sp)))\n"
       "\n"
       "(set! charspan-length (lambda (sp)\n"
       "  (fx- (charspan-end sp) (charspan-beg sp))))\n"
       "\n"
       "(set! charspan-empty? (lambda (sp)\n"
       "  (fx>=? (charspan-beg sp) (charspan-end sp))))\n"
       "\n"
       "(set! charspan-ref (lambda (sp n)\n"
       "  (assert (fx<? n (charspan-length sp)))\n"
       "  (string-ref (charspan-vec sp) (fx+ n (charspan-beg sp)))))\n"
       "\n"
       "(set! charspan-set! (lambda (sp n val)\n"
       "  (assert (fx<? n (charspan-end sp)))\n"
       "  (string-set! (charspan-vec sp) (fx+ n (charspan-beg sp)) val)))\n"
       "\n"
       "(set! charspan-back (lambda (sp)\n"
       "  (assert (not (charspan-empty? sp)))\n"
       "  (string-ref (charspan-vec sp) (fx1- (charspan-end sp)))))\n"
       "\n"
       "(set! charspan-fill! (lambda (sp val)\n"
       /* no optimized function to fill only between charspan-beg and charspan-end,
        * so fill the whole string */
       "  (string-fill! (charspan-vec sp) val)))\n"
       "\n"
       "(set! charspan-fill-range! (lambda (sp start n val)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (charspan-length sp)))\n"
       "  (string-fill-range! (charspan-vec sp) (fx+ start (charspan-beg sp)) n val)))\n"
       "\n"
       "(set! charspan-copy (lambda (src)\n"
       "  (let* ((n (charspan-length src))\n"
       "         (dst (make-charspan n)))\n"
       "    (string-copy! (charspan-vec src) (charspan-beg src)\n"
       "                  (charspan-vec dst) (charspan-beg dst) n)\n"
       "    dst)))\n"
       "\n"
       "(set! charspan-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (charspan-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (charspan-length dst)))\n"
       "  (string-copy! (charspan-vec src) (fx+ src-start (charspan-beg src))\n"
       "                (charspan-vec dst) (fx+ dst-start (charspan-beg dst)) n)))\n"
       "\n"
       "(set! charspan-reallocate-back! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (charspan-length sp)))\n"
       "        (old-vec (charspan-vec sp))\n"
       "        (new-vec (make-string cap)))\n"
       "    (string-copy! old-vec (charspan-beg sp) new-vec 0 copy-len)\n"
       "    (charspan-beg-set! sp 0)\n"
       "    (charspan-end-set! sp len)\n"
       "    (charspan-vec-set! sp new-vec))))\n"
       "\n"
       "(set! charspan-capacity-back (lambda (sp)\n"
       "  (fx- (string-length (charspan-vec sp)) (charspan-beg sp))))\n"
       "\n"
       "(set! charspan-reserve-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((cap-back (charspan-capacity-back sp)))\n"
       "    (when (fx> len cap-back)\n"
       "      (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "        (charspan-reallocate-back! sp (charspan-length sp) new-cap))))))\n"
       "\n"
       "(set! charspan-resize-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (charspan-reserve-back! sp len)\n"
       "  (charspan-end-set! sp (fx+ len (charspan-beg sp)))))\n"
       "\n"
       "(set! charspan-insert-back! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((n   (length vals))\n"
       "          (pos (charspan-length sp)))\n"
       "      (charspan-resize-back! sp (fx+ pos n))\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (charspan-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       "(set! charspan-csp-insert-back! (lambda (sp-dst sp-src src-start src-n)\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (charspan-length sp-dst)))\n"
       "      (charspan-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (charspan-copy! sp-src src-start sp-dst pos src-n)))))\n"
       "\n"
       /* erase n elements at the front of charspan */
       "(set! charspan-erase-front! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (charspan-length sp)))\n"
       "  (cond\n"
       "    ((fx=? n 0))\n"
       "    ((fx<? n (charspan-length sp))\n"
       "      (charspan-beg-set! sp (fx+ n (charspan-beg sp))))\n"
       "    (#t\n"
       "      (charspan-beg-set! sp 0)\n"
       "      (charspan-end-set! sp 0)))))\n"
       "\n"
       "(set! charspan-iterate (lambda (sp proc)\n"
       "  (do ((i (charspan-beg sp) (fx1+ i))\n"
       "       (n (charspan-end sp))\n"
       "       (v (charspan-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (string-ref v i))))))))\n"
       "\n"
       /** customize how "charspan" objects are printed */
       "(record-writer (record-type-descriptor %charspan)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(string->charspan \" port)\n"
       "    (write (substring (charspan-vec sp) (charspan-beg sp) (charspan-end sp)) port)\n"
       "    (display #\\) port)))\n"
       ")\n");

  /**
   * (charspan-find) iterates on charspan elements from start to (fxmin (fx+ start n)
   * (charspan-length sp)), and returns the index of first charspan element that causes (predicate
   * elem) to return non-#f. Returns #f if no such element is found.
   */
  eval("(define (charspan-find sp start n predicate)\n"
       "  (let ((ret #f))\n"
       "    (do ((i   start (fx1+ i))\n"
       "         (end (fxmin (fx+ start n) (charspan-length sp))))\n"
       "        ((or ret (fx>=? i end)) ret)\n"
       "      (when (predicate (charspan-ref sp i))\n"
       "        (set! ret i)))))\n");
}

/**
 * Define Scheme type "gbuffer", a gap buffer.
 * Implementation: contains two spans, a "left" and a "right" ones
 */
static void define_gbuffer_functions(void) {
  eval("(begin\n"
       "  (define list->gbuffer)\n"
       "  (define span->gbuffer)\n"
       "  (define make-gbuffer)\n"
       "  (define gbuffer->span)\n"
       "  (define gbuffer)\n"
       "  (define gbuffer?)\n"
       "  (define gbuffer-length)\n"
       "  (define gbuffer-empty?)\n"
       "  (define gbuffer-ref)\n"
       "  (define gbuffer-split-at!)\n"
       "  (define gbuffer-insert-at!)\n"
       "  (define gbuffer-erase-at!)\n"
       "  (define gbuffer-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%gbuffer %make-gbuffer %gbuffer?)\n"
       "  (fields\n"
       "     (mutable left  gbuffer-left  gbuffer-left-set!)\n"
       "     (mutable right gbuffer-right gbuffer-right-set!))\n"
       /*  "  (nongenerative #{%gbuffer ng1h8vurkk5k61p0jsryrbk99-0})" */ ")\n"
       "\n"
       "(set! list->gbuffer (lambda (l)\n"
       "  (%make-gbuffer (make-span 0) (list->span vec))))\n"
       "\n"
       "(set! span->gbuffer (lambda (sp)\n"
       "  (%make-gbuffer (make-span 0) (copy-span sp))))\n"
       "\n"
       "(set! make-gbuffer (lambda (n . val)\n"
       "  (%make-gbuffer (make-span 0) (apply make-span n val))))\n"
       "\n"
       "(set! gbuffer->span (lambda (gb)\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (left-n  (span-length left))\n"
       "         (right-n (span-length right))\n"
       "         (dst (make-span (fx+ left-n right-n))))\n"
       "    (span-copy! left  0 dst 0 left-n)\n"
       "    (span-copy! right 0 dst left-n right-n))))\n"
       "\n"
       "(set! gbuffer (lambda vals\n"
       "  (list->gbuffer vals)))\n"
       "\n"
       "(set! gbuffer? (lambda (gb)\n"
       "  (%gbuffer? gb)))\n"
       "\n"
       "(set! gbuffer-length (lambda (gb)\n"
       "  (fx+ (span-length (gbuffer-left gb)) (span-length (gbuffer-right gb)))))\n"
       "\n"
       "(set! gbuffer-empty? (lambda (gb)\n"
       "  (and (span-empty? (gbuffer-left gb)) (span-empty? (gbuffer-right gb)))))\n"
       "\n"
       "(set! gbuffer-ref (lambda (gb n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<? n (gbuffer-length gb)))\n"
       "  (let ((left-n (span-length (gbuffer-left gb))))\n"
       "    (if (fx<? n left-n)\n"
       "      (span-ref (gbuffer-left  gb) n)\n"
       "      (span-ref (gbuffer-right gb) (fx- n left-n))))))\n"
       "\n"
       "(set! gbuffer-split-at! (lambda (gb n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (gbuffer-length gb)))\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (left-n  (span-length left))\n"
       "         (right-n (span-length right)))\n"
       "    (vector-copy! (gbuffer-vec src) (gbuffer-beg src)\n"
       "                  (gbuffer-vec dst) (gbuffer-beg dst) n)\n"
       "    dst)))\n"
       "\n"
       "(set! gbuffer-insert-at! (lambda (gb n val)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (gbuffer-length gb)))\n"
       "  (let* ((n (gbuffer-length src))\n"
       "         (dst (make-gbuffer n)))\n"
       "    (vector-copy! (gbuffer-vec src) (gbuffer-beg src)\n"
       "                  (gbuffer-vec dst) (gbuffer-beg dst) n)\n"
       "    dst)))\n"
       "\n"
       "(set! gbuffer-copy! (lambda (src src-start dst dst-start n)\n"
       "  (assert (fx>=? src-start 0))\n"
       "  (assert (fx>=? dst-start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ src-start n) (gbuffer-length src)))\n"
       "  (assert (fx<=? (fx+ dst-start n) (gbuffer-length dst)))\n"
       "  (vector-copy! (gbuffer-vec src) (fx+ src-start (gbuffer-beg src))\n"
       "                (gbuffer-vec dst) (fx+ dst-start (gbuffer-beg dst)) n)))\n"
       "\n"
       "(set! gbuffer-reallocate-back! (lambda (sp len cap)\n"
       "  (assert (fx>=? len 0))\n"
       "  (assert (fx>=? cap len))\n"
       "  (let ((copy-len (fxmin len (gbuffer-length sp)))\n"
       "        (old-vec (gbuffer-vec sp))\n"
       "        (new-vec (make-vector cap)))\n"
       "    (vector-copy! old-vec (gbuffer-beg sp) new-vec 0 copy-len)\n"
       "    (gbuffer-beg-set! sp 0)\n"
       "    (gbuffer-end-set! sp len)\n"
       "    (gbuffer-vec-set! sp new-vec))))\n"
       "\n"
       "(set! gbuffer-capacity-back (lambda (sp)\n"
       "  (fx- (vector-length (gbuffer-vec sp)) (gbuffer-beg sp))))\n"
       "\n"
       "(set! gbuffer-reserve-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (let ((cap-back (gbuffer-capacity-back sp)))\n"
       "    (when (fx> len cap-back)\n"
       "      (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))\n"
       "        (gbuffer-reallocate-back! sp (gbuffer-length sp) new-cap))))))\n"
       "\n"
       "(set! gbuffer-resize-back! (lambda (sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (gbuffer-reserve-back! sp len)\n"
       "  (gbuffer-end-set! sp (fx+ len (gbuffer-beg sp)))))\n"
       "\n"
       "(set! gbuffer-insert-back! (lambda (sp . vals)\n"
       "  (unless (null? vals)\n"
       "    (let ((n   (length vals))\n"
       "          (pos (gbuffer-length sp)))\n"
       "      (gbuffer-resize-back! sp (fx+ pos n))\n"
       "      (list-iterate vals\n"
       "        (lambda (elem)\n"
       "          (gbuffer-set! sp pos elem)\n"
       "          (set! pos (fx1+ pos))))))))\n"
       "\n"
       /* erase n elements at the front of gbuffer */
       "(set! gbuffer-erase-front! (lambda (sp n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (gbuffer-length sp)))\n"
       "  (cond\n"
       "    ((fx=? n 0))\n"
       "    ((fx<? n (gbuffer-length sp))\n"
       "      (gbuffer-beg-set! sp (fx+ n (gbuffer-beg sp))))\n"
       "    (#t\n"
       "      (gbuffer-beg-set! sp 0)\n"
       "      (gbuffer-end-set! sp 0)))))\n"
       "\n"
       "(set! gbuffer-iterate (lambda (sp proc)\n"
       "  (do ((i (gbuffer-beg sp) (fx1+ i))\n"
       "       (n (gbuffer-end sp))\n"
       "       (v (gbuffer-vec sp)))\n"
       "    ((or (fx>=? i n) (not (proc i (vector-ref v i))))))))\n"
       "\n"
#if 0
       /** customize how "gbuffer" objects are printed */
       "(record-writer (record-type-descriptor %gbuffer)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(gbuffer\" port)\n"
       "    (gbuffer-iterate sp"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
#endif
       ")\n");
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
