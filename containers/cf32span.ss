;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define Scheme type "cf32span", a resizeable vector of complex-float-32 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers cf32span (0 9 3))
  (export
    list->cf32span bytevector->cf32span bytevector->cf32span* make-cf32span
    cf32span->bytevector cf32span->bytevector*!
    cf32span cf32span? cf32span-length cf32span-empty? cf32span-clear!
    cf32span-capacity cf32span-capacity-left cf32span-capacity-right
    cf32span-ref cf32span-ref-right cf32span-set!
    cf32span-fill! cf32span-copy cf32span-copy! cf32span=?
    cf32span-reserve-left! cf32span-reserve-right! cf32span-resize-left! cf32span-resize-right!
    cf32span-insert-left! cf32span-insert-right!
    cf32span-insert-left/cf32span! cf32span-insert-right/cf32span!
    cf32span-delete-left! cf32span-delete-right!
    in-cf32span cf32span-iterate
    cf32span-peek-beg cf32span-peek-end cf32span-peek-data)
  (import
    (rnrs)
    (only (chezscheme)         bytevector-truncate! cfl-real-part cfl-imag-part cflonum?
                               fl-make-rectangular fx1+ fx1- record-writer void)
    (only (scheme2k bootstrap) assert* assert-not* fx<=?*)
    (only (scheme2k containers list) for-list)
    (scheme2k containers bytevector))

(define-record-type (%cf32span %make-cf32span cf32span?)
  (fields
     (mutable beg cf32span-beg cf32span-beg-set!)  ;; cf32 index
     (mutable end cf32span-end cf32span-end-set!)  ;; cf32 index
     (mutable vec cf32span-vec cf32span-vec-set!)) ;; bytevector
  (nongenerative %cf32span-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define (bytepos cf32-index)
  (fxarithmetic-shift-left cf32-index 3))

;; given byte-index of real part, return byte-index of imaginary part
(define (bytepos+imag byte-index)
  (fx+ byte-index 4))

(define (cf32pos byte-index)
  (fxarithmetic-shift-right byte-index 3))

(define cf32span-peek-beg cf32span-beg)
(define cf32span-peek-end cf32span-end)
(define cf32span-peek-data cf32span-vec)


;; convert a list of numbers to cf32span
(define (list->cf32span l)
  (let* ((n  (length l))
         (sp (%make-cf32span 0 n (make-bytevector (bytepos n)))))
    (do ((tail l (cdr tail))
         (i    0 (fx1+ i)))
        ((null? tail) sp)
      (cf32span-set! sp i (car tail)))))

;; create cf32span copying contents of specified bytevector
(define (bytevector->cf32span vec)
  (%make-cf32span 0 (cf32pos (bytevector-length vec)) (bytevector-copy vec)))

;; view existing bytevector as cf32span
(define (bytevector->cf32span* vec)
  (%make-cf32span 0 (cf32pos (bytevector-length vec)) vec))

;; create cf32span with specified length and optional fill value, which must be a cflonum
;; If fill value is not specified, it defaults to 0.0+0.0i.
(define make-cf32span
  (case-lambda
    ((n fill)
      (assert* 'make-cf32span (cflonum? fill))
      (let ((sp (%make-cf32span 0 n (make-bytevector (bytepos n)))))
        (cf32span-fill! sp 0 n fill)
        sp))
    ((n)
      (make-cf32span n 0.0+0.0i))))

;; convert a cf32span to bytevector
(define (cf32span->bytevector sp)
  (let ((beg (cf32span-beg sp))
        (end (cf32span-end sp)))
    (if (fx>=? beg end)
      #vu8()
      (subbytevector (cf32span-vec sp) (bytepos beg) (bytepos end)))))

;; if possible, truncate cf32span to its length and view it as a bytevector.
;; otherwise convert it to bytevector as (cf32span->bytevector) does.
(define (cf32span->bytevector*! sp)
  (if (or (cf32span-empty? sp) (not (fxzero? (cf32span-beg sp))))
    (cf32span->bytevector sp)
    (let ((bv (cf32span-vec sp)))
      (bytevector-truncate! bv (bytepos (cf32span-end sp)))
      bv)))

;; create cf32span containing specified numbers
(define (cf32span . numbers)
  (list->cf32span numbers))

;; return current number of elements
(define (cf32span-length sp)
  (fx- (cf32span-end sp) (cf32span-beg sp)))

;; return maximum number of elements that can be stored without reallocating
(define (cf32span-capacity sp)
  (cf32pos (bytevector-length (cf32span-vec sp))))

(define (cf32span-empty? sp)
  (fx>=? (cf32span-beg sp) (cf32span-end sp)))

;; set cf32span to empty. return (void)
(define (cf32span-clear! sp)
  (cf32span-beg-set! sp 0)
  (cf32span-end-set! sp 0))

;; return i-th element of cf32span
(define (cf32span-ref sp idx)
  (assert* 'cf32span-ref (fx<? -1 idx (cf32span-length sp)))
  (let ((bv  (cf32span-vec sp))
        (pos (bytepos (fx+ idx (cf32span-beg sp)))))
    (fl-make-rectangular
      (bytevector-ieee-single-native-ref bv pos)
      (bytevector-ieee-single-native-ref bv (bytepos+imag pos)))))

(define (cf32span-ref-right sp)
  (assert* 'cf32span-ref-right (not (cf32span-empty? sp)))
  (cf32span-ref sp (fx1- (cf32span-length sp))))

;; set i-th element of cf32span to cfloat, which must be a cflonum
(define (cf32span-set! sp idx cfloat)
  (assert* 'cf32span-set! (fx<? -1 idx (cf32span-length sp)))
  (assert* 'cf32span-set! (cflonum? cfloat))
  (let ((bv  (cf32span-vec sp))
        (pos (bytepos (fx+ idx (cf32span-beg sp)))))
    (bytevector-ieee-single-native-set! bv pos                (cfl-real-part cfloat))
    (bytevector-ieee-single-native-set! bv (bytepos+imag pos) (cfl-imag-part cfloat))))

;; set all cf32span elements in range [start...end) to fill, which must be a cflonum
(define cf32span-fill!
  (case-lambda
    ((sp start end fill)
      (assert* 'cf32span-fill! (cflonum? fill))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (cf32span-set! sp i fill)))
    ((sp fill)
      (cf32span-fill! sp 0 (cf32span-length sp) fill))))

(define (cf32span-copy src)
  (let* ((n   (cf32span-length src))
         (dst (make-cf32span n)))
    (bytevector-copy! (cf32span-vec src) (bytepos (cf32span-beg src))
                      (cf32span-vec dst) (bytepos (cf32span-beg dst)) (bytepos n))
    dst))

(define (cf32span-copy! src src-start dst dst-start n)
  (assert* 'cf32span-copy! (fx<=?* 0 src-start (fx+ src-start n) (cf32span-length src)))
  (assert* 'cf32span-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (cf32span-length dst)))
  (bytevector-copy! (cf32span-vec src) (bytepos (fx+ src-start (cf32span-beg src)))
                    (cf32span-vec dst) (bytepos (fx+ dst-start (cf32span-beg dst))) (bytepos n)))

(define (cf32span=? left right)
  (or
    (eq? left right)
    (and (eq?  (cf32span-vec left) (cf32span-vec right))
         (fx=? (cf32span-beg left) (cf32span-beg right))
         (fx=? (cf32span-end left) (cf32span-end right)))
    (let ((n1 (cf32span-length left))
          (n2 (cf32span-length right)))
      (and (fx=? n1 n2)
           (subbytevector=? (cf32span-vec left)  (bytepos (cf32span-beg left))
                            (cf32span-vec right) (bytepos (cf32span-beg right))
                            (bytepos n1))))))

(define (cf32span-reallocate-left! sp len cap)
  (assert* 'cf32span-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (cf32span-length sp)))
        (old-vec  (cf32span-vec sp))
        (new-vec  (make-bytevector (bytepos cap)))
        (new-beg  (fx- cap len)))
    (bytevector-copy! old-vec (bytepos (cf32span-beg sp)) new-vec (bytepos new-beg) (bytepos copy-len))
    (cf32span-beg-set! sp new-beg)
    (cf32span-end-set! sp cap)
    (cf32span-vec-set! sp new-vec)))

(define (cf32span-reallocate-right! sp len cap)
  (assert* 'cf32span-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (cf32span-length sp)))
        (old-vec  (cf32span-vec sp))
        (new-vec  (make-bytevector (bytepos cap))))
    (bytevector-copy! old-vec (bytepos (cf32span-beg sp)) new-vec 0 (bytepos copy-len))
    (cf32span-beg-set! sp 0)
    (cf32span-end-set! sp len)
    (cf32span-vec-set! sp new-vec)))

;; return distance between begin of internal bytevector and last element
(define (cf32span-capacity-left sp)
  (cf32span-end sp))

;; return distance between first element and end of internal bytevector
(define (cf32span-capacity-right sp)
  (fx- (cf32pos (bytevector-length (cf32span-vec sp))) (cf32span-beg sp)))

;; ensure distance between begin of internal bytevector and last element is >= n.
;; does NOT change the length
(define (cf32span-reserve-left! sp n)
  (assert* 'cf32span-reserve-left! (fx>=? n 0))
  (let ((vec      (cf32span-vec sp))
        (cap-left (cf32span-capacity-left sp)))
    (cond
      ((fx<=? n cap-left)
       ;; nothing to do
       (void))
      ((fx<=? n (cf32pos (bytevector-length vec)))
        ;; bytevector is large enough, move elements to the back
        (let* ((cap     (cf32span-capacity sp))
               (old-len (cf32span-length sp))
               (new-beg (fx- cap old-len)))
          (bytevector-copy! vec (bytepos (cf32span-beg sp)) vec (bytepos new-beg) (bytepos old-len))
          (cf32span-beg-set! sp new-beg)
          (cf32span-end-set! sp cap)))
      (else
        ;; bytevector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-left))))
          (cf32span-reallocate-left! sp (cf32span-length sp) new-cap))))))

;; ensure distance between first element and end of internal bytevector is >= n.
;; does NOT change the length
(define (cf32span-reserve-right! sp n)
  (assert* 'cf32span-reserve-right! (fx>=? n 0))
  (let ((vec       (cf32span-vec sp))
        (cap-right (cf32span-capacity-right sp)))
    (cond
      ((fx<=? n cap-right)
       ;; nothing to do
       (void))
      ((fx<=? n (cf32pos (bytevector-length vec)))
        ;; bytevector is large enough, move elements to the front
        (let ((old-len (cf32span-length sp)))
          (bytevector-copy! vec (bytepos (cf32span-beg sp)) vec 0 (bytepos old-len))
          (cf32span-beg-set! sp 0)
          (cf32span-end-set! sp n)))
      (else
        ;; bytevector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-right))))
          (cf32span-reallocate-right! sp (cf32span-length sp) new-cap))))))

;; grow or shrink cf32span on the left (front), set length to len
(define (cf32span-resize-left! sp len)
  (assert* 'cf32span-resize-left! (fx>=? len 0))
  (cf32span-reserve-left! sp len)
  (assert* 'cf32span-resize-left! (fx>=? (cf32span-capacity-left sp) len))
  (cf32span-beg-set! sp (fx- (cf32span-end sp) len)))

;; grow or shrink cf32span on the right (back), set length to len
(define (cf32span-resize-right! sp len)
  (assert* 'cf32span-resize-right! (fx>=? len 0))
  (cf32span-reserve-right! sp len)
  (assert* 'cf32span-resize-right! (fx>=? (cf32span-capacity-right sp) len))
  (cf32span-end-set! sp (fx+ len (cf32span-beg sp))))

(define (cf32span-insert-left! sp cfloat)
  (assert* 'cf32span-insert-left! (cflonum? cfloat))
  (cf32span-resize-left! sp (fx1+ (cf32span-length sp)))
  (cf32span-set! sp 0 cfloat))

(define (cf32span-insert-right! sp cfloat)
  (assert* 'cf32span-insert-right! (cflonum? cfloat))
  (let ((pos (cf32span-length sp)))
    (cf32span-resize-right! sp (fx1+ pos))
    (cf32span-set! sp pos cfloat)))

;; insert range [src-start, src-end) of cf32span bv-src
;; at the beginning of cf32span sp-dst
(define cf32span-insert-left/cf32span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'cf32span-insert-left/cf32span! (eq? sp-dst sp-src))
      (assert*     'cf32span-insert-left/cf32span! (fx<=?* 0 src-start src-end (cf32span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'cf32span-insert-left/cf32span! (eq? (cf32span-vec sp-dst) (cf32span-vec sp-src)))
        (let ((len    (cf32span-length sp-dst))
              (src-n  (fx- src-end src-start)))
          (cf32span-resize-left! sp-dst (fx+ len src-n))
          (cf32span-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (cf32span-insert-left/cf32span! sp-dst sp-src 0 (cf32span-length sp-src)))))

;; append a portion of another cf32span to this cf32span
(define cf32span-insert-right/cf32span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'cf32span-insert-right/cf32span! (eq? sp-dst sp-src))
      (assert*     'cf32span-insert-right/cf32span! (fx<=?* 0 src-start src-end src-end (cf32span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'cf32span-insert-right/cf32span! (eq? (cf32span-vec sp-dst) (cf32span-vec sp-src)))
        (let ((pos   (cf32span-length sp-dst))
              (src-n (fx- src-end src-start)))
          (cf32span-resize-right! sp-dst (fx+ pos src-n))
          (cf32span-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (cf32span-insert-right/cf32span! sp-dst sp-src 0 (cf32span-length sp-src)))))

;; erase n elements at the left i.e. front of cf32span
(define (cf32span-delete-left! sp n)
  (assert* 'cf32span-delete-left! (fx<=? 0 n (cf32span-length sp)))
  (unless (fxzero? n)
    (cf32span-beg-set! sp (fx+ n (cf32span-beg sp)))))

;; erase n elements at the right i.e. back of cf32span
(define (cf32span-delete-right! sp n)
  (assert* 'cf32span-delete-right! (fx<=? 0 n (cf32span-length sp)))
  (unless (fxzero? n)
    (cf32span-end-set! sp (fx- (cf32span-end sp) n))))


;; create and return a closure that iterates on elements of cf32span sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in cf32span sp and #t,
;; or (values #<unspecified> #f) if end of cf32span is reached.
(define in-cf32span
  (case-lambda
    ((sp start end step)
      (assert* 'in-cf32span (fx<=?* 0 start end (cf32span-length sp)))
      (assert* 'in-cf32span (fx>=? step 0))
      (let ((pos start))
        (lambda ()
          (if (fx<? pos end)
            (let ((ret (cf32span-ref sp pos)))
              (set! pos (fx+ pos step))
              (values ret #t))
            (values #f #f)))))
    ((sp start end)
      (in-cf32span sp start end 1))
    ((sp)
      (in-cf32span sp 0 (cf32span-length sp) 1))))

;; (cf32span-iterate sp proc) iterates on all elements of given cf32span sp,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (cf32span-iterate sp proc)
  (do ((len (cf32span-length sp))
       (i   0 (fx1+ i)))
    ((or (fx>=? i len) (not (proc i (cf32span-ref sp i))))
     (fx>=? i len))))


;; customize how "cf32span" objects are printed
(record-writer (record-type-descriptor %cf32span)
  (lambda (sp out writer)
    (display "(cf32span" out)
    (cf32span-iterate sp
      (lambda (i elem)
        (display #\space out)
        (writer elem out)))
    (display ")" out)))

) ; close library
