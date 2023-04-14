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
  "list->bytevector subbytevector bytevector-fill-range! bytevector-iterate "                      \
  "string-fill-range! "

  Sregister_symbol("c_vector_copy", &c_vector_copy);

  eval("(library (schemesh containers misc (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_MISC_EXPORT ")\n"
       "  (import (chezscheme))\n"
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

/** define Scheme type "span", a resizeable vector */
static void define_library_containers_span(void) {

#define SCHEMESH_LIBRARY_CONTAINERS_SPAN_EXPORT                                                    \
  "list->span vector->span vector->span* make-span span->vector span span? "                       \
  "span-length span-empty? span-capacity span-capacity-front span-capacity-back "                  \
  "span-ref span-back span-set! span-fill! span-fill-range! span-copy span-copy! "                 \
  "span-reserve-front! span-reserve-back! span-resize-front! span-resize-back! "                   \
  "span-insert-front! span-insert-back! span-sp-insert-front! span-sp-insert-back! "               \
  "span-erase-front! span-erase-back! span-iterate span-find "

  eval("(library (schemesh containers span (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_SPAN_EXPORT ")\n"
       "  (import (chezscheme) (schemesh containers misc))\n"
       "\n"
       "(define-record-type\n"
       "  (%span %make-span span?)\n"
       "  (fields\n"
       "     (mutable beg span-beg span-beg-set!)\n"
       "     (mutable end span-end span-end-set!)\n"
       "     (mutable vec span-vec span-vec-set!))\n"
       "  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))\n"
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
       "(define (span-ref sp n)\n"
       "  (assert (fx<? n (span-length sp)))\n"
       "  (vector-ref (span-vec sp) (fx+ n (span-beg sp))))\n"
       "\n"
       "(define (span-back sp)\n"
       "  (assert (not (span-empty? sp)))\n"
       "  (vector-ref (span-vec sp) (fx1- (span-end sp))))\n"
       "\n"
       "(define (span-set! sp n val)\n"
       "  (assert (fx<? n (span-end sp)))\n"
       "  (vector-set! (span-vec sp) (fx+ n (span-beg sp)) val))\n"
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
       "      ((fx<= len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (vector-length vec))\n"
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
       "         (span-reallocate-back! sp (span-length sp) new-cap))))))\n"
       "\n"
       /* grow or shrink span on the left (front), set length to n */
       "(define (span-resize-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-front! sp len)\n"
       "  (assert (fx>= (span-capacity-front sp) len))\n"
       "  (span-beg-set! sp (fx- (span-end sp) len)))\n"
       "\n"
       /* grow or shrink span on the right (back), set length to n */
       "(define (span-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (span-reserve-back! sp len)\n"
       "  (assert (fx>= (span-capacity-back sp) len))\n"
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
  "bytespan bytespan? bytespan-length bytespan-empty? "                                            \
  "bytespan-capacity bytespan-capacity-front bytespan-capacity-back "                              \
  "bytespan-u8-ref bytespan-u8-back bytespan-u8-set! bytespan-fill! bytespan-fill-range! "         \
  "bytespan-copy bytespan-copy! bytespan-reserve-front! bytespan-reserve-back! "                   \
  "bytespan-resize-front! bytespan-resize-back! "                                                  \
  "bytespan-u8-insert-front! bytespan-u8-insert-back! "                                            \
  "bytespan-bsp-insert-front! bytespan-bsp-insert-back! "                                          \
  "bytespan-erase-front! bytespan-erase-back! bytespan-iterate bytespan-u8-find "                  \
  "bytespan-peek-beg bytespan-peek-end bytespan-peek-data "

  eval("(library (schemesh containers bytespan (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_BYTESPAN_EXPORT ")\n"
       "  (import (chezscheme) (schemesh containers misc))\n"
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
       "(define (bytespan-u8-ref sp n)\n"
       "  (assert (fx<? n (bytespan-length sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx+ n (bytespan-beg sp))))\n"
       "\n"
       "(define (bytespan-u8-back sp)\n"
       "  (assert (not (bytespan-empty? sp)))\n"
       "  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-end sp))))\n"
       "\n"
       "(define (bytespan-u8-set! sp n val)\n"
       "  (assert (fx<? n (bytespan-end sp)))\n"
       "  (bytevector-u8-set! (bytespan-vec sp) (fx+ n (bytespan-beg sp)) val))\n"
       "\n"
       "(define (bytespan-fill! sp val)\n"
       "  (bytevector-fill-range! (bytespan-vec sp) (bytespan-beg sp) (bytespan-length sp) "
       "val))\n"
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
       "      ((fx<= len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (bytevector-length vec))\n"
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
       "         (bytespan-reallocate-back! sp (bytespan-length sp) new-cap))))))\n"
       "\n"
       /* grow or shrink bytespan on the left (front), set length to n */
       "(define (bytespan-resize-front! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-front! sp len)\n"
       "  (assert (fx>= (bytespan-capacity-front sp) len))\n"
       "  (bytespan-beg-set! sp (fx- (bytespan-end sp) len)))\n"
       "\n"
       /* grow or shrink bytespan on the right (back), set length to n */
       "(define (bytespan-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (bytespan-reserve-back! sp len)\n"
       "  (assert (fx>= (bytespan-capacity-back sp) len))\n"
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
       /* append a portion of another bytespan to this bytespan */
       "(define (bytespan-bsp-insert-back! sp-dst sp-src src-start src-n)\n"
       "  (assert (not (eq? sp-dst sp-src)))\n"
       "  (unless (fxzero? src-n)\n"
       "    (let ((pos (bytespan-length sp-dst)))\n"
       "      (bytespan-resize-back! sp-dst (fx+ pos src-n))\n"
       "      (bytespan-copy! sp-src src-start sp-dst pos src-n))))\n"
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
  "charspan charspan? charspan-length charspan-empty? charspan-capacity "                          \
  "charspan-capacity-front charspan-capacity-back charspan-ref charspan-back charspan-set! "       \
  "charspan-fill! charspan-fill-range! charspan-copy charspan-copy! "                              \
  "charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back! "   \
  "charspan-insert-front! charspan-insert-back!  "                                                 \
  "charspan-csp-insert-front! charspan-csp-insert-back! "                                          \
  "charspan-erase-front! charspan-erase-back! charspan-iterate charspan-find "

  eval("(library (schemesh containers charspan (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_CONTAINERS_CHARSPAN_EXPORT ")\n"
       "  (import (chezscheme) (schemesh containers misc))\n"
       "\n"
       "(define-record-type\n"
       "  (%charspan %make-charspan charspan?)\n"
       "  (fields\n"
       "     (mutable beg charspan-beg charspan-beg-set!)\n"
       "     (mutable end charspan-end charspan-end-set!)\n"
       "     (mutable vec charspan-vec charspan-vec-set!))\n"
       "  (nongenerative #{%charspan b847ikzm9lftljwelbq0cknyh-0}))\n"
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
       "(define (charspan-ref sp n)\n"
       "  (assert (fx<? n (charspan-length sp)))\n"
       "  (string-ref (charspan-vec sp) (fx+ n (charspan-beg sp))))\n"
       "\n"
       "(define (charspan-back sp)\n"
       "  (assert (not (charspan-empty? sp)))\n"
       "  (string-ref (charspan-vec sp) (fx1- (charspan-end sp))))\n"
       "\n"
       "(define (charspan-set! sp n val)\n"
       "  (assert (fx<? n (charspan-end sp)))\n"
       "  (string-set! (charspan-vec sp) (fx+ n (charspan-beg sp)) val))\n"
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
       "      ((fx<= len cap-front)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (string-length vec))\n"
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
       "      ((fx<= len cap-back)\n"
       /*      nothing to do */
       "       (void))\n"
       "      ((fx<= len (string-length vec))\n"
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
       "  (assert (fx>= (charspan-capacity-front sp) len))\n"
       "  (charspan-beg-set! sp (fx- (charspan-end sp) len)))\n"
       "\n"
       /* grow or shrink charspan on the right (back), set length to n */
       "(define (charspan-resize-back! sp len)\n"
       "  (assert (fx>=? len 0))\n"
       "  (charspan-reserve-back! sp len)\n"
       "  (assert (fx>= (charspan-capacity-back sp) len))\n"
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

  eval("(begin\n"
       "\n"
       "(import (schemesh containers misc)\n"
       "        (schemesh containers span)\n"
       "        (schemesh containers bytespan)\n"
       "        (schemesh containers charspan))\n"
       "\n"
       "(define list->gbuffer)\n"
       "(define span->gbuffer)\n"
       "(define span->gbuffer*)\n" /* view two spans as gbuffer */
       "(define make-gbuffer)\n"
       "(define gbuffer->span)\n"
       "(define gbuffer)\n"
       "(define gbuffer?)\n"
       "(define gbuffer-length)\n"
       "(define gbuffer-empty?)\n"
       "(define gbuffer-ref)\n"
       "(define gbuffer-set!)\n"
       "(define gbuffer-split-at!)\n"
       "(define gbuffer-insert-at!)\n"
       "(define gbuffer-erase-at!)\n"
       "(define gbuffer-iterate))\n");

  eval("(let ()\n"
       "\n"
       "(define-record-type\n"
       "  (%gbuffer %make-gbuffer %gbuffer?)\n"
       "  (fields\n"
       "     (mutable left  gbuffer-left  gbuffer-left-set!)\n"
       "     (mutable right gbuffer-right gbuffer-right-set!))\n"
       "  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))\n"
       "\n"
       "(set! list->gbuffer (lambda (l)\n"
       "  (%make-gbuffer (make-span 0) (list->span l))))\n"
       "\n"
       "(set! span->gbuffer (lambda (sp)\n"
       "  (%make-gbuffer (make-span 0) (copy-span sp))))\n"
       "\n"
       "(set! span->gbuffer* (lambda (sp1 sp2)\n"
       "  (%make-gbuffer sp1 sp2)))\n"
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
       "(set! gbuffer-set! (lambda (gb n val)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<? n (gbuffer-length gb)))\n"
       "  (let ((left-n (span-length (gbuffer-left gb))))\n"
       "    (if (fx<? n left-n)\n"
       "      (span-set! (gbuffer-left  gb) n val)\n"
       "      (span-set! (gbuffer-right gb) (fx- n left-n) val)))))\n"
       "\n"
       "(set! gbuffer-split-at! (lambda (gb n)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (gbuffer-length gb)))\n"
       "  (let* ((left  (gbuffer-left  gb))\n"
       "         (right (gbuffer-right gb))\n"
       "         (delta (fx- n (span-length left))))\n"
       "    (cond\n"
       "      ((fx>? delta 0)\n"
       "        (span-sp-insert-back! left right 0 delta)\n"
       "        (span-erase-front! right delta))\n"
       "      ((fx<? delta 0)\n"
       "        (span-sp-insert-front! right left n (fx- delta))\n"
       "        (span-erase-back! left (fx- delta)))))))\n"
       "\n"
       "(set! gbuffer-insert-at! (lambda (gb n val)\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? n (gbuffer-length gb)))\n"
       "  (let* ((left   (gbuffer-left  gb))\n"
       "         (right  (gbuffer-right gb))\n"
       "         (left-n (span-length left))\n"
       "         (delta  (fx- n left-n)))\n"
       "    (cond\n"
       "      ((fx=? n 0)\n"
       "        (span-insert-front! left val))\n"
       "      ((fx=? n (gbuffer-length gb))\n"
       "        (span-insert-back! right val))\n"
       "      (#t\n"
       "        (gbuffer-split-at! gb n)\n"
       "        (span-insert-back! left val))))))\n"
       "\n"
       "(set! gbuffer-iterate (lambda (gb proc)\n"
       "  (do ((i 0 (fx1+ i))\n"
       "       (n (gbuffer-length gb)))\n"
       "    ((or (fx>=? i n) (not (proc i (gbuffer-ref gb i))))))))\n"
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
       ")\n");

  /****************************************************************************/
}

static void define_library_containers_hashtable(void) {

  eval("(begin\n"
       /** return hash-iterator to first element in hashtable */
       "(define make-hash-iterator)\n"
       "  \n"
       /** return #t if argument is an hash-iterator, otherwise return #f */
       "(define hash-iterator?)\n"
       "  \n"
       /** make a copy of specified hash-iterator */
       "(define hash-iterator-copy)\n"
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
       "(define hash-iterator-cell))\n"
       "  \n"
       /**
        * modify hash-iterator in place to point to next hashtable element.
        * return next hashtable element (key . val) if more elements are available,
        * otherwise return #f
        *
        * as (hash-iterator-cell), setting the cdr of returned element propagates back
        * to the hashtable.
        */
       "(define hash-iterator-next!)\n"
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
       "(define hashtable-iterate)\n"
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

void define_library_containers(void) {
  define_library_containers_misc();
  define_library_containers_span();
  define_library_containers_bytespan();
  define_library_containers_charspan();
  define_library_containers_gbuffer();
  define_library_containers_hashtable();
}
