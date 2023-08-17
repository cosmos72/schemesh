;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  define Scheme type "bytespan", a resizeable bytevector  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers bytespan (0 1))
  (export
    list->bytespan bytevector->bytespan bytevector->bytespan* make-bytespan bytespan->bytevector
    bytespan bytespan? bytespan-length bytespan-empty? bytespan-clear!
    bytespan-capacity bytespan-capacity-front bytespan-capacity-back
    bytespan-u8-ref bytespan-u8-back bytespan-u8-set!
    bytespan-fill! bytespan-fill-range! bytespan-copy bytespan-copy! bytespan=?
    bytespan-reserve-front! bytespan-reserve-back! bytespan-resize-front! bytespan-resize-back!
    bytespan-insert-front/u8! bytespan-insert-back/u8!
    bytespan-insert-front/bspan! bytespan-insert-back/bspan!
    bytespan-insert-front/bvector! bytespan-insert-back/bvector!
    bytespan-erase-front! bytespan-erase-back! bytespan-iterate bytespan-u8-find
    bytespan-peek-beg bytespan-peek-end bytespan-peek-data)
  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer void)
    (schemesh containers misc))

(define-record-type
  (%bytespan %make-bytespan bytespan?)
  (fields
     (mutable beg bytespan-beg bytespan-beg-set!)
     (mutable end bytespan-end bytespan-end-set!)
     (mutable vec bytespan-vec bytespan-vec-set!))
  (nongenerative #{%bytespan 1j9oboeqc5j4db1bamcd28yz-0}))

(define bytespan-peek-beg bytespan-beg)
(define bytespan-peek-end bytespan-end)
(define bytespan-peek-data bytespan-vec)

(define (list->bytespan l)
  (let ((vec (list->bytevector l)))
    (%make-bytespan 0 (bytevector-length vec) vec)))

;  create bytespan copying contents of specified bytevector
(define (bytevector->bytespan vec)
  (%make-bytespan 0 (bytevector-length vec) (bytevector-copy vec)))

;  view existing bytevector as bytespan
(define (bytevector->bytespan* vec)
  (%make-bytespan 0 (bytevector-length vec) vec))

(define make-bytespan
  (case-lambda
    ((n)      (%make-bytespan 0 n (make-bytevector n)))
    ((n fill) (%make-bytespan 0 n (make-bytevector n fill)))))

(define (bytespan->bytevector sp)
  (let ((beg (bytespan-beg sp))
        (end (bytespan-end sp)))
    (if (fx>=? beg end)
      #vu8()
      (subbytevector (bytespan-vec sp) beg end))))

(define (bytespan . vals)
  (list->bytespan vals))

(define (bytespan-length sp)
  (fx- (bytespan-end sp) (bytespan-beg sp)))

;  return length of internal bytevector, i.e. maximum number of elements
; that can be stored without reallocating
(define (bytespan-capacity sp)
  (bytevector-length (bytespan-vec sp)))

(define (bytespan-empty? sp)
  (fx>=? (bytespan-beg sp) (bytespan-end sp)))

(define (bytespan-clear! sp)
  (bytespan-beg-set! sp 0)
  (bytespan-end-set! sp 0))

(define (bytespan-u8-ref sp idx)
  (assert (fx>=? idx 0))
  (assert (fx<? idx (bytespan-length sp)))
  (bytevector-u8-ref (bytespan-vec sp) (fx+ idx (bytespan-beg sp))))

(define (bytespan-u8-back sp)
  (assert (not (bytespan-empty? sp)))
  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-end sp))))

(define (bytespan-u8-set! sp idx val)
  (assert (fx>=? idx 0))
  (assert (fx<? idx (bytespan-length sp)))
  (bytevector-u8-set! (bytespan-vec sp) (fx+ idx (bytespan-beg sp)) val))

(define (bytespan-fill! sp val)
  (bytevector-fill-range! (bytespan-vec sp) (bytespan-beg sp)
                          (bytespan-length sp) val))

(define (bytespan-fill-range! sp start n val)
  (assert (fx>=? start 0))
  (assert (fx>=? n 0))
  (assert (fx<=? (fx+ start n) (bytespan-length sp)))
  (bytevector-fill-range! (bytespan-vec sp) (fx+ start (bytespan-beg sp)) n val))

(define (bytespan-copy src)
  (let* ((n (bytespan-length src))
         (dst (make-bytespan n)))
    (bytevector-copy! (bytespan-vec src) (bytespan-beg src)
                  (bytespan-vec dst) (bytespan-beg dst) n)
    dst))

(define (bytespan-copy! src src-start dst dst-start n)
  (assert (fx<=? 0 src-start (fx+ src-start n) (bytespan-length src)))
  (assert (fx<=? 0 dst-start (fx+ dst-start n) (bytespan-length dst)))
  (bytevector-copy! (bytespan-vec src) (fx+ src-start (bytespan-beg src))
                (bytespan-vec dst) (fx+ dst-start (bytespan-beg dst)) n))

(define (bytespan=? left right)
  (or (and (eq?  (bytespan-vec left) (bytespan-vec right))
           (fx=? (bytespan-beg left) (bytespan-beg right))
           (fx=? (bytespan-end left) (bytespan-end right)))
    (let* ((n1 (bytespan-length left))
           (n2 (bytespan-length right))
           (equal (fx=? n1 n2)))
      (do ((i 0 (fx1+ i)))
          ((or (not equal) (fx>=? i n1)) equal)
        (set! equal (fx=? (bytespan-u8-ref left i) (bytespan-u8-ref right i)))))))

(define (bytespan-reallocate-front! sp len cap)
  (assert (fx>=? len 0))
  (assert (fx>=? cap len))
  (let ((copy-len (fxmin len (bytespan-length sp)))
        (old-vec (bytespan-vec sp))
        (new-vec (make-bytevector cap))
        (new-beg (fx- cap len)))
    (bytevector-copy! old-vec (bytespan-beg sp) new-vec new-beg copy-len)
    (bytespan-beg-set! sp new-beg)
    (bytespan-end-set! sp cap)
    (bytespan-vec-set! sp new-vec)))

(define (bytespan-reallocate-back! sp len cap)
  (assert (fx>=? len 0))
  (assert (fx>=? cap len))
  (let ((copy-len (fxmin len (bytespan-length sp)))
        (old-vec (bytespan-vec sp))
        (new-vec (make-bytevector cap)))
    (bytevector-copy! old-vec (bytespan-beg sp) new-vec 0 copy-len)
    (bytespan-beg-set! sp 0)
    (bytespan-end-set! sp len)
    (bytespan-vec-set! sp new-vec)))

;  return distance between begin of internal bytevector and last element
(define (bytespan-capacity-front sp)
  (bytespan-end sp))

;  return distance between first element and end of internal bytevector
(define (bytespan-capacity-back sp)
  (fx- (bytevector-length (bytespan-vec sp)) (bytespan-beg sp)))

;  ensure distance between begin of internal bytevector and last element is >= n.
; does NOT change the length
(define (bytespan-reserve-front! sp len)
  (assert (fx>=? len 0))
  (let ((vec (bytespan-vec sp))
        (cap-front (bytespan-capacity-front sp)))
    (cond
      ((fx<=? len cap-front)
;       nothing to do
       (void))
      ((fx<=? len (bytevector-length vec))
;        bytevector is large enough, move elements to the back
        (let* ((cap (bytespan-capacity sp))
               (old-len (bytespan-length sp))
               (new-beg (fx- cap old-len)))
          (bytevector-copy! vec (bytespan-beg sp) vec new-beg old-len)
          (bytespan-beg-set! sp new-beg)
          (bytespan-end-set! sp cap)))
      (#t
;        bytevector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))
         (bytespan-reallocate-front! sp (bytespan-length sp) new-cap))))))

;  ensure distance between first element and end of internal bytevector is >= n.
; does NOT change the length
(define (bytespan-reserve-back! sp len)
  (assert (fx>=? len 0))
  (let ((vec (bytespan-vec sp))
        (cap-back (bytespan-capacity-back sp)))
    (cond
      ((fx<=? len cap-back)
;       nothing to do
       (void))
      ((fx<=? len (bytevector-length vec))
;        bytevector is large enough, move elements to the front
        (let ((len (bytespan-length sp)))
          (bytevector-copy! vec (bytespan-beg sp) vec 0 len)
          (bytespan-beg-set! sp 0)
          (bytespan-end-set! sp len)))
      (#t
;        bytevector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))
         (bytespan-reallocate-back! sp (bytespan-length sp) new-cap))))))

;  grow or shrink bytespan on the left (front), set length to n
(define (bytespan-resize-front! sp len)
  (assert (fx>=? len 0))
  (bytespan-reserve-front! sp len)
  (assert (fx>=? (bytespan-capacity-front sp) len))
  (bytespan-beg-set! sp (fx- (bytespan-end sp) len)))

;  grow or shrink bytespan on the right (back), set length to n
(define (bytespan-resize-back! sp len)
  (assert (fx>=? len 0))
  (bytespan-reserve-back! sp len)
  (assert (fx>=? (bytespan-capacity-back sp) len))
  (bytespan-end-set! sp (fx+ len (bytespan-beg sp))))

(define (bytespan-insert-front/u8! sp . vals)
  (unless (null? vals)
    (let ((pos 0)
          (new-len (fx+ (bytespan-length sp) (length vals))))
      (bytespan-resize-front! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (bytespan-u8-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

(define (bytespan-insert-back/u8! sp . vals)
  (unless (null? vals)
    (let* ((pos (bytespan-length sp))
           (new-len (fx+ pos (length vals))))
      (bytespan-resize-back! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (bytespan-u8-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

;  prefix a portion of another bytespan to this bytespan
(define (bytespan-insert-front/bspan! sp-dst sp-src src-start src-n)
  (assert (not (eq? (bytespan-vec sp-dst) (bytespan-vec sp-src))))
  (assert (fx<=? 0 src-start (fx+ src-start src-n) (bytespan-length sp-src)))
  (unless (fxzero? src-n)
    (let ((len (bytespan-length sp-dst)))
      (bytespan-resize-front! sp-dst (fx+ len src-n))
      (bytespan-copy! sp-src src-start sp-dst 0 src-n))))

;  prefix a portion of a bytevector to this bytespan
(define (bytespan-insert-front/bvector! sp-dst bv-src src-start src-n)
  (assert (not (eq? (bytespan-vec sp-dst) bv-src)))
  (assert (fx<=? 0 src-start (fx+ src-start src-n) (bytevector-length bv-src)))
  (unless (fxzero? src-n)
    (bytespan-insert-front/bspan! sp-dst (bytevector->bytespan* bv-src) src-start src-n)))

;  append a portion of another bytespan to this bytespan
(define (bytespan-insert-back/bspan! sp-dst sp-src src-start src-n)
  (assert (not (eq? (bytespan-vec sp-dst) (bytespan-vec sp-src))))
  (assert (fx<=? 0 src-start (fx+ src-start src-n) (bytespan-length sp-src)))
  (unless (fxzero? src-n)
    (let ((pos (bytespan-length sp-dst)))
      (bytespan-resize-back! sp-dst (fx+ pos src-n))
      (bytespan-copy! sp-src src-start sp-dst pos src-n))))

;  append a portion of a bytevector to this bytespan
(define (bytespan-insert-back/bvector! sp-dst bv-src src-start src-n)
  (unless (fxzero? src-n)
    (bytespan-insert-back/bspan! sp-dst (bytevector->bytespan* bv-src) src-start src-n)))

;  erase n elements at the left (front) of bytespan
(define (bytespan-erase-front! sp n)
  (assert (fx<=? 0 n (bytespan-length sp)))
  (unless (fxzero? n)
    (bytespan-beg-set! sp (fx+ n (bytespan-beg sp)))))

;  erase n elements at the right (back) of bytespan
(define (bytespan-erase-back! sp n)
  (assert (fx<=? 0 n (bytespan-length sp)))
  (unless (fxzero? n)
    (bytespan-end-set! sp (fx- (bytespan-end sp) n))))

(define (bytespan-iterate sp proc)
  (do ((i (bytespan-beg sp) (fx1+ i))
       (n (bytespan-end sp))
       (v (bytespan-vec sp)))
    ((or (fx>=? i n) (not (proc i (bytevector-u8-ref v i)))))))

; (bytespan-u8-find) iterates on bytespan elements from start to (fxmin (fx+ start n)
; (bytespan-length sp)), and returns the index of first bytespan element that causes
; (predicate elem) to return non-#f. Returns #f if no such element is found.
(define (bytespan-u8-find sp start n predicate)
  (let ((ret #f))
    (do ((i   start (fx1+ i))
         (end (fxmin (fx+ start n) (bytespan-length sp))))
        ((or ret (fx>=? i end)) ret)
      (when (predicate (bytespan-u8-ref sp i))
        (set! ret i)))))

; * customize how "bytespan" objects are printed
(record-writer (record-type-descriptor %bytespan)
  (lambda (sp port writer)
    (display "(bytespan" port)
    (bytespan-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display #\) port)))

) ; close library
