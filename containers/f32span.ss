;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define Scheme type "f32span", a resizeable vector of float-32 ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers f32span (0 9 3))
  (export
    list->f32span bytevector->f32span bytevector->f32span* make-f32span
    f32span->bytevector f32span->bytevector*!
    f32span f32span? f32span-length f32span-empty? f32span-clear!
    f32span-capacity f32span-capacity-left f32span-capacity-right
    f32span-ref f32span-ref-right f32span-set!
    f32span-fill! f32span-copy f32span-copy! f32span=?
    f32span-reserve-left! f32span-reserve-right! f32span-resize-left! f32span-resize-right!
    f32span-insert-left! f32span-insert-right!
    f32span-insert-left/f32span! f32span-insert-right/f32span!
    f32span-delete-left! f32span-delete-right!
    in-f32span f32span-iterate
    f32span-peek-beg f32span-peek-end f32span-peek-data
    port->f32span f32span->port)
  (import
    (rnrs)
    (only (chezscheme)         bytevector-truncate! exact->inexact
                               fx1+ fx1- read-token record-writer void)
    (only (scheme2k bootstrap) assert* assert-not* fx<=?*)
    (only (scheme2k containers list) for-list)
    (scheme2k containers bytevector))

(define-record-type (%f32span %make-f32span f32span?)
  (fields
     (mutable beg f32span-beg f32span-beg-set!)  ;; unsigned fixnum, min offset in f32 elements
     (mutable end f32span-end f32span-end-set!)  ;; unsigned fixnum, max offset in f32 elements
     (mutable vec f32span-vec f32span-vec-set!)) ;; bytevector
  (nongenerative %f32span-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define (bytepos f32-index)
  (fxarithmetic-shift-left f32-index 2))

(define (f32pos byte-index)
  (fxarithmetic-shift-right byte-index 2))

(define f32span-peek-beg f32span-beg)
(define f32span-peek-end f32span-end)
(define f32span-peek-data f32span-vec)


;; convert a list of inexact numbers to f32span
(define (list->f32span l)
  (let* ((n  (length l))
         (sp (%make-f32span 0 n (make-bytevector (bytepos n)))))
    (do ((tail l (cdr tail))
         (i    0 (fx1+ i)))
        ((null? tail) sp)
      (f32span-set! sp i (car tail)))))

;; create f32span copying contents of specified bytevector
(define (bytevector->f32span vec)
  (%make-f32span 0 (f32pos (bytevector-length vec)) (bytevector-copy vec)))

;; view existing bytevector as f32span
(define (bytevector->f32span* vec)
  (%make-f32span 0 (f32pos (bytevector-length vec)) vec))

;; create f32span with specified length and optional fill value, which must be a cflonum
;; If fill value is not specified, it defaults to 0.0+0.0i.
(define make-f32span
  (case-lambda
    ((n fill)
      (assert* 'make-f32span (flonum? fill))
      (let ((sp (%make-f32span 0 n (make-bytevector (bytepos n)))))
        (f32span-fill! sp 0 n fill)
        sp))
    ((n)
      (make-f32span n 0.0+0.0i))))

;; convert a f32span to bytevector
(define (f32span->bytevector sp)
  (let ((beg (f32span-beg sp))
        (end (f32span-end sp)))
    (if (fx>=? beg end)
      #vu8()
      (subbytevector (f32span-vec sp) (bytepos beg) (bytepos end)))))

;; if possible, truncate f32span to its length and view it as a bytevector.
;; otherwise convert it to bytevector as (f32span->bytevector) does.
(define (f32span->bytevector*! sp)
  (if (or (f32span-empty? sp) (not (fxzero? (f32span-beg sp))))
    (f32span->bytevector sp)
    (let ((bv (f32span-vec sp)))
      (bytevector-truncate! bv (bytepos (f32span-end sp)))
      bv)))

;; create f32span containing specified inexact numbers
(define (f32span . numbers)
  (list->f32span numbers))

;; return current number of elements in specified f32span
(define (f32span-length sp)
  (fx- (f32span-end sp) (f32span-beg sp)))

;; return maximum number of elements that can be stored in specified f32span without reallocating 
(define (f32span-capacity sp)
  (f32pos (bytevector-length (f32span-vec sp))))

(define (f32span-empty? sp)
  (fx>=? (f32span-beg sp) (f32span-end sp)))

;; set f32span to empty. return (void)
(define (f32span-clear! sp)
  (f32span-beg-set! sp 0)
  (f32span-end-set! sp 0))

;; return i-th element of f32span
(define (f32span-ref sp idx)
  (assert* 'f32span-ref (fx<? -1 idx (f32span-length sp)))
  (let ((bv  (f32span-vec sp))
        (pos (bytepos (fx+ idx (f32span-beg sp)))))
    (bytevector-ieee-single-native-ref bv pos)))

(define (f32span-ref-right sp)
  (assert* 'f32span-ref-right (not (f32span-empty? sp)))
  (f32span-ref sp (fx1- (f32span-length sp))))

;; set i-th element of f32span to float, which must be a flonum
(define (f32span-set! sp idx float)
  (assert* 'f32span-set! (fx<? -1 idx (f32span-length sp)))
  (assert* 'f32span-set! (flonum? float))
  (let ((bv  (f32span-vec sp))
        (pos (bytepos (fx+ idx (f32span-beg sp)))))
    (bytevector-ieee-single-native-set! bv pos float)))

;; set all f32span elements in range [start...end) to fill, which must be a cflonum
(define f32span-fill!
  (case-lambda
    ((sp start end fill)
      (assert* 'f32span-fill! (flonum? fill))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (f32span-set! sp i fill)))
    ((sp fill)
      (f32span-fill! sp 0 (f32span-length sp) fill))))

(define (f32span-copy src)
  (let* ((n   (f32span-length src))
         (dst (make-f32span n)))
    (bytevector-copy! (f32span-vec src) (bytepos (f32span-beg src))
                      (f32span-vec dst) (bytepos (f32span-beg dst)) (bytepos n))
    dst))

(define (f32span-copy! src src-start dst dst-start n)
  (assert* 'f32span-copy! (fx<=?* 0 src-start (fx+ src-start n) (f32span-length src)))
  (assert* 'f32span-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (f32span-length dst)))
  (bytevector-copy! (f32span-vec src) (bytepos (fx+ src-start (f32span-beg src)))
                    (f32span-vec dst) (bytepos (fx+ dst-start (f32span-beg dst))) (bytepos n)))

(define (f32span=? left right)
  (or
    (eq? left right)
    (and (eq?  (f32span-vec left) (f32span-vec right))
         (fx=? (f32span-beg left) (f32span-beg right))
         (fx=? (f32span-end left) (f32span-end right)))
    (let ((n1 (f32span-length left))
          (n2 (f32span-length right)))
      (and (fx=? n1 n2)
           (subbytevector=? (f32span-vec left)  (bytepos (f32span-beg left))
                            (f32span-vec right) (bytepos (f32span-beg right))
                            (bytepos n1))))))

(define (f32span-reallocate-left! sp len cap)
  (assert* 'f32span-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (f32span-length sp)))
        (old-vec  (f32span-vec sp))
        (new-vec  (make-bytevector (bytepos cap)))
        (new-beg  (fx- cap len)))
    (bytevector-copy! old-vec (bytepos (f32span-beg sp)) new-vec (bytepos new-beg) (bytepos copy-len))
    (f32span-beg-set! sp new-beg)
    (f32span-end-set! sp cap)
    (f32span-vec-set! sp new-vec)))

(define (f32span-reallocate-right! sp len cap)
  (assert* 'f32span-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (f32span-length sp)))
        (old-vec  (f32span-vec sp))
        (new-vec  (make-bytevector (bytepos cap))))
    (bytevector-copy! old-vec (bytepos (f32span-beg sp)) new-vec 0 (bytepos copy-len))
    (f32span-beg-set! sp 0)
    (f32span-end-set! sp len)
    (f32span-vec-set! sp new-vec)))

;; return distance between begin of internal bytevector and last element
(define (f32span-capacity-left sp)
  (f32span-end sp))

;; return distance between first element and end of internal bytevector
(define (f32span-capacity-right sp)
  (fx- (f32pos (bytevector-length (f32span-vec sp))) (f32span-beg sp)))

;; ensure distance between begin of internal bytevector and last element is >= n.
;; does NOT change the length
(define (f32span-reserve-left! sp n)
  (assert* 'f32span-reserve-left! (fx>=? n 0))
  (let ((vec      (f32span-vec sp))
        (cap-left (f32span-capacity-left sp)))
    (cond
      ((fx<=? n cap-left)
       ;; nothing to do
       (void))
      ((fx<=? n (f32pos (bytevector-length vec)))
        ;; bytevector is large enough, move elements to the back
        (let* ((cap     (f32span-capacity sp))
               (old-len (f32span-length sp))
               (new-beg (fx- cap old-len)))
          (bytevector-copy! vec (bytepos (f32span-beg sp)) vec (bytepos new-beg) (bytepos old-len))
          (f32span-beg-set! sp new-beg)
          (f32span-end-set! sp cap)))
      (else
        ;; bytevector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-left))))
          (f32span-reallocate-left! sp (f32span-length sp) new-cap))))))

;; ensure distance between first element and end of internal bytevector is >= n.
;; does NOT change the length
(define (f32span-reserve-right! sp n)
  (assert* 'f32span-reserve-right! (fx>=? n 0))
  (let ((vec       (f32span-vec sp))
        (cap-right (f32span-capacity-right sp)))
    (cond
      ((fx<=? n cap-right)
       ;; nothing to do
       (void))
      ((fx<=? n (f32pos (bytevector-length vec)))
        ;; bytevector is large enough, move elements to the front
        (let ((old-len (f32span-length sp)))
          (bytevector-copy! vec (bytepos (f32span-beg sp)) vec 0 (bytepos old-len))
          (f32span-beg-set! sp 0)
          (f32span-end-set! sp n)))
      (else
        ;; bytevector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-right))))
          (f32span-reallocate-right! sp (f32span-length sp) new-cap))))))

;; grow or shrink f32span on the left (front), set length to len
(define (f32span-resize-left! sp len)
  (assert* 'f32span-resize-left! (fx>=? len 0))
  (f32span-reserve-left! sp len)
  (assert* 'f32span-resize-left! (fx>=? (f32span-capacity-left sp) len))
  (f32span-beg-set! sp (fx- (f32span-end sp) len)))

;; grow or shrink f32span on the right (back), set length to len
(define (f32span-resize-right! sp len)
  (assert* 'f32span-resize-right! (fx>=? len 0))
  (f32span-reserve-right! sp len)
  (assert* 'f32span-resize-right! (fx>=? (f32span-capacity-right sp) len))
  (f32span-end-set! sp (fx+ len (f32span-beg sp))))

;; insert one inexact number to the left.
(define (f32span-insert-left! sp float)
  (assert* 'f32span-insert-left! (flonum? float))
  (f32span-resize-left! sp (fx1+ (f32span-length sp)))
  (f32span-set! sp 0 float))

;; insert one inexact number to the right.
(define (f32span-insert-right! sp float)
  (assert* 'f32span-insert-right! (flonum? float))
  (let ((pos (f32span-length sp)))
    (f32span-resize-right! sp (fx1+ pos))
    (f32span-set! sp pos float)))

;; insert range [src-start, src-end) of f32span sp-src
;; at the beginning of f32span sp-dst
(define f32span-insert-left/f32span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'f32span-insert-left/f32span! (eq? sp-dst sp-src))
      (assert*     'f32span-insert-left/f32span! (fx<=?* 0 src-start src-end (f32span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'f32span-insert-left/f32span! (eq? (f32span-vec sp-dst) (f32span-vec sp-src)))
        (let ((len    (f32span-length sp-dst))
              (src-n  (fx- src-end src-start)))
          (f32span-resize-left! sp-dst (fx+ len src-n))
          (f32span-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (f32span-insert-left/f32span! sp-dst sp-src 0 (f32span-length sp-src)))))

;; append a portion of another f32span to this f32span
(define f32span-insert-right/f32span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'f32span-insert-right/f32span! (eq? sp-dst sp-src))
      (assert*     'f32span-insert-right/f32span! (fx<=?* 0 src-start src-end src-end (f32span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'f32span-insert-right/f32span! (eq? (f32span-vec sp-dst) (f32span-vec sp-src)))
        (let ((pos   (f32span-length sp-dst))
              (src-n (fx- src-end src-start)))
          (f32span-resize-right! sp-dst (fx+ pos src-n))
          (f32span-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (f32span-insert-right/f32span! sp-dst sp-src 0 (f32span-length sp-src)))))

;; erase n elements at the left i.e. front of f32span
(define (f32span-delete-left! sp n)
  (assert* 'f32span-delete-left! (fx<=? 0 n (f32span-length sp)))
  (unless (fxzero? n)
    (f32span-beg-set! sp (fx+ n (f32span-beg sp)))))

;; erase n elements at the right i.e. back of f32span
(define (f32span-delete-right! sp n)
  (assert* 'f32span-delete-right! (fx<=? 0 n (f32span-length sp)))
  (unless (fxzero? n)
    (f32span-end-set! sp (fx- (f32span-end sp) n))))


;; create and return a closure that iterates on elements of f32span sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in f32span sp and #t,
;; or (values #<unspecified> #f) if end of f32span is reached.
(define in-f32span
  (case-lambda
    ((sp start end step)
      (assert* 'in-f32span (fx<=?* 0 start end (f32span-length sp)))
      (assert* 'in-f32span (fx>=? step 0))
      (let ((pos start))
        (lambda ()
          (if (fx<? pos end)
            (let ((ret (f32span-ref sp pos)))
              (set! pos (fx+ pos step))
              (values ret #t))
            (values #f #f)))))
    ((sp start end)
      (in-f32span sp start end 1))
    ((sp)
      (in-f32span sp 0 (f32span-length sp) 1))))

;; (f32span-iterate sp proc) iterates on all elements of given f32span sp,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (f32span-iterate sp proc)
  (do ((len (f32span-length sp))
       (i   0 (fx1+ i)))
    ((or (fx>=? i len) (not (proc i (f32span-ref sp i))))
     (fx>=? i len))))

(define port->f32span
  (case-lambda
    ((in)
      (let %loop ((in in) (ret (make-f32span 0)))
        (let-values (((_a float _b _c) (read-token in)))
          (if (eof-object? float)
            ret
            (begin
              (f32span-insert-right! ret (exact->inexact float))
              (%loop in ret))))))
    (()
      (port->f32span (current-input-port)))))

(define f32span->port
  (case-lambda
    ((sp out)
      (do ((i 0 (fx1+ i))
           (n (f32span-length sp)))
          ((fx>=? i n))
        (write (f32span-ref sp i) out)
        (newline out)))
    ((sp)
      (f32span->port sp (current-output-port)))))

;; customize how "f32span" objects are printed
(record-writer (record-type-descriptor %f32span)
  (lambda (sp out writer)
    (display "(f32span" out)
    (f32span-iterate sp
      (lambda (i elem)
        (display #\space out)
        (writer elem out)))
    (display ")" out)))

) ; close library
