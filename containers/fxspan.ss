;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; define Scheme type "fxspan", a resizeable vector of fixnum ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers fxspan (0 9 3))
  (export
    list->fxspan fxvector->fxspan fxvector->fxspan* make-fxspan
    fxspan->fxvector fxspan->fxvector*!
    fxspan fxspan? fxspan-length fxspan-empty? fxspan-clear!
    fxspan-capacity fxspan-capacity-left fxspan-capacity-right
    fxspan-ref fxspan-ref-right fxspan-set!
    fxspan-fill! fxspan-copy fxspan-copy!
    fxspan<? fxspan<=? fxspan>? fxspan>=? fxspan=? fxspan-compare
    fxspan-reserve-left! fxspan-reserve-right! fxspan-resize-left! fxspan-resize-right!
    fxspan-insert-left! fxspan-insert-right!
    fxspan-insert-left/fxspan! fxspan-insert-right/fxspan!
    fxspan-delete-left! fxspan-delete-right!
    in-fxspan fxspan-iterate
    fxspan-peek-beg fxspan-peek-end fxspan-peek-data
    port->fxspan fxspan->port)
  (import
    (rnrs)
    (only (chezscheme)               fx1+ fx1- fxvector-copy fxvector-length fxvector-ref
                                     fxvector-set! make-fxvector read-token record-writer void)
    (only (scheme2k bootstrap)       assert* assert-not* fx<=?*)
    (only (scheme2k containers list) for-list)
    (scheme2k containers fxvector))

(define-record-type (%fxspan %make-fxspan fxspan?)
  (fields
     (mutable beg fxspan-beg fxspan-beg-set!)  ;; unsigned fixnum, min offset
     (mutable end fxspan-end fxspan-end-set!)  ;; unsigned fixnum, max offset
     (mutable vec fxspan-vec fxspan-vec-set!)) ;; fxvector
  (nongenerative %fxspan-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define fxspan-peek-beg fxspan-beg)
(define fxspan-peek-end fxspan-end)
(define fxspan-peek-data fxspan-vec)


;; convert a list of fixnums to fxspan
(define (list->fxspan l)
  (let* ((n  (length l))
         (v  (make-fxvector n)))
    (do ((tail l (cdr tail))
         (i    0 (fx1+ i)))
        ((null? tail) (%make-fxspan 0 n v))
      (fxvector-set! v i (car tail)))))

;; create fxspan copying contents of specified fxvector
(define (fxvector->fxspan vec)
  (%make-fxspan 0 (fxvector-length vec) (fxvector-copy vec)))

;; view existing fxvector as fxspan
(define (fxvector->fxspan* vec)
  (%make-fxspan 0 (fxvector-length vec) vec))

;; create fxspan with specified length and optional fill value, which must be a fixnum
;; If fill value is not specified, fxspan contents are unspecified
(define make-fxspan
  (case-lambda
    ((n fill)
      (%make-fxspan 0 n (make-fxvector n fill)))
    ((n)
      (%make-fxspan 0 n (make-fxvector n)))))

;; convert a fxspan to fxvector
(define (fxspan->fxvector sp)
  (let* ((n   (fxspan-length sp))
         (ret (make-fxvector n)))
    (fxvector-copy! (fxspan-vec sp) (fxspan-beg sp) ret 0 n)
    ret))

;; if possible, truncate fxspan to its length and view it as a fxvector.
;; otherwise convert it to fxvector as (fxspan->fxvector) does.
(define (fxspan->fxvector*! sp)
  (if (and (fxzero? (fxspan-beg sp)) (fxzero? (fxspan-end sp)))
    (fxspan-vec sp)
    (fxspan->fxvector sp)))

;; create fxspan containing specified inexact numbers
(define (fxspan . numbers)
  (list->fxspan numbers))

;; return current number of elements in specified fxspan
(define (fxspan-length sp)
  (fx- (fxspan-end sp) (fxspan-beg sp)))

;; return maximum number of elements that can be stored in specified fxspan without reallocating 
(define (fxspan-capacity sp)
  (fxvector-length (fxspan-vec sp)))

(define (fxspan-empty? sp)
  (fx>=? (fxspan-beg sp) (fxspan-end sp)))

;; set fxspan to empty. return (void)
(define (fxspan-clear! sp)
  (fxspan-beg-set! sp 0)
  (fxspan-end-set! sp 0))

;; return i-th element of fxspan
(define (fxspan-ref sp idx)
  (assert* 'fxspan-ref (fx<? -1 idx (fxspan-length sp)))
  (fxvector-ref (fxspan-vec sp) (fx+ idx (fxspan-beg sp))))

(define (fxspan-ref-right sp)
  (assert* 'fxspan-ref-right (not (fxspan-empty? sp)))
  (fxspan-ref sp (fx1- (fxspan-length sp))))

;; set i-th element of fxspan to value, which must be a fixnum
(define (fxspan-set! sp idx value)
  (assert* 'fxspan-set! (fx<? -1 idx (fxspan-length sp)))
  (assert* 'fxspan-set! (fixnum? value))
  (fxvector-set! (fxspan-vec sp) (fx+ idx (fxspan-beg sp)) value))

;; set all fxspan elements in range [start...end) to fill, which must be a fixnum
(define fxspan-fill!
  (case-lambda
    ((sp start end fill)
      (assert* 'fxspan-fill! (fixnum? fill))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (fxspan-set! sp i fill)))
    ((sp fill)
      (fxspan-fill! sp 0 (fxspan-length sp) fill))))

(define (fxspan-copy src)
  (let* ((n   (fxspan-length src))
         (dst (make-fxspan n)))
    (fxvector-copy! (fxspan-vec src) (fxspan-beg src)
                    (fxspan-vec dst) (fxspan-beg dst) n)
    dst))

(define (fxspan-copy! src src-start dst dst-start n)
  (assert* 'fxspan-copy! (fx<=?* 0 src-start (fx+ src-start n) (fxspan-length src)))
  (assert* 'fxspan-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (fxspan-length dst)))
  (fxvector-copy! (fxspan-vec src) (fx+ src-start (fxspan-beg src))
                  (fxspan-vec dst) (fx+ dst-start (fxspan-beg dst)) n))

(define (fxspan-compare left right)
  (let* ((n1  (fxspan-length left))
         (n2  (fxspan-length right))
         (cmp (fxvector-compare (fxspan-vec left)  (fxspan-beg left)
                                (fxspan-vec right) (fxspan-beg right) (fxmin n1 n2))))
    (if (fxzero? cmp)
      (fxsign (fx- n1 n2))
      cmp)))

(define (fxspan<? left right)
  (fx<? (fxspan-compare left right) 0))

(define (fxspan<=? left right)
  (fx<=? (fxspan-compare left right) 0))

(define (fxspan>? left right)
  (fx>? (fxspan-compare left right) 0))

(define (fxspan>=? left right)
  (fx>=? (fxspan-compare left right) 0))

(define (fxspan=? left right)
  (or
    (eq? left right)
    (and (eq?  (fxspan-vec left) (fxspan-vec right))
         (fx=? (fxspan-beg left) (fxspan-beg right))
         (fx=? (fxspan-end left) (fxspan-end right)))
    (let ((n1 (fxspan-length left))
          (n2 (fxspan-length right)))
      (and (fx=? n1 n2)
           (fxvector=? (fxspan-vec left)  (fxspan-beg left)
                       (fxspan-vec right) (fxspan-beg right) n1)))))

(define (fxspan-reallocate-left! sp len cap)
  (assert* 'fxspan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (fxspan-length sp)))
        (old-vec  (fxspan-vec sp))
        (new-vec  (make-fxvector cap))
        (new-beg  (fx- cap len)))
    (fxvector-copy! old-vec (fxspan-beg sp) new-vec new-beg copy-len)
    (fxspan-beg-set! sp new-beg)
    (fxspan-end-set! sp cap)
    (fxspan-vec-set! sp new-vec)))

(define (fxspan-reallocate-right! sp len cap)
  (assert* 'fxspan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (fxspan-length sp)))
        (old-vec  (fxspan-vec sp))
        (new-vec  (make-fxvector cap)))
    (fxvector-copy! old-vec (fxspan-beg sp) new-vec 0 copy-len)
    (fxspan-beg-set! sp 0)
    (fxspan-end-set! sp len)
    (fxspan-vec-set! sp new-vec)))

;; return distance between begin of internal fxvector and last element
(define (fxspan-capacity-left sp)
  (fxspan-end sp))

;; return distance between first element and end of internal fxvector
(define (fxspan-capacity-right sp)
  (fx- (fxvector-length (fxspan-vec sp)) (fxspan-beg sp)))

;; ensure distance between begin of internal fxvector and last element is >= n.
;; does NOT change the length
(define (fxspan-reserve-left! sp n)
  (assert* 'fxspan-reserve-left! (fx>=? n 0))
  (let ((vec      (fxspan-vec sp))
        (cap-left (fxspan-capacity-left sp)))
    (cond
      ((fx<=? n cap-left)
       ;; nothing to do
       (void))
      ((fx<=? n (fxvector-length vec))
        ;; fxvector is large enough, move elements to the back
        (let* ((cap     (fxspan-capacity sp))
               (old-len (fxspan-length sp))
               (new-beg (fx- cap old-len)))
          (fxvector-copy! vec (fxspan-beg sp) vec new-beg old-len)
          (fxspan-beg-set! sp new-beg)
          (fxspan-end-set! sp cap)))
      (else
        ;; fxvector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-left))))
          (fxspan-reallocate-left! sp (fxspan-length sp) new-cap))))))

;; ensure distance between first element and end of internal fxvector is >= n.
;; does NOT change the length
(define (fxspan-reserve-right! sp n)
  (assert* 'fxspan-reserve-right! (fx>=? n 0))
  (let ((vec       (fxspan-vec sp))
        (cap-right (fxspan-capacity-right sp)))
    (cond
      ((fx<=? n cap-right)
       ;; nothing to do
       (void))
      ((fx<=? n (fxvector-length vec))
        ;; fxvector is large enough, move elements to the front
        (let ((old-len (fxspan-length sp)))
          (fxvector-copy! vec (fxspan-beg sp) vec 0 old-len)
          (fxspan-beg-set! sp 0)
          (fxspan-end-set! sp n)))
      (else
        ;; fxvector is too small, reallocate it
        (let ((new-cap (fxmax 8 n (fx* 2 cap-right))))
          (fxspan-reallocate-right! sp (fxspan-length sp) new-cap))))))

;; grow or shrink fxspan on the left (front), set length to len
(define (fxspan-resize-left! sp len)
  (assert* 'fxspan-resize-left! (fx>=? len 0))
  (fxspan-reserve-left! sp len)
  (assert* 'fxspan-resize-left! (fx>=? (fxspan-capacity-left sp) len))
  (fxspan-beg-set! sp (fx- (fxspan-end sp) len)))

;; grow or shrink fxspan on the right (back), set length to len
(define (fxspan-resize-right! sp len)
  (assert* 'fxspan-resize-right! (fx>=? len 0))
  (fxspan-reserve-right! sp len)
  (assert* 'fxspan-resize-right! (fx>=? (fxspan-capacity-right sp) len))
  (fxspan-end-set! sp (fx+ len (fxspan-beg sp))))

;; insert one inexact number to the left.
(define (fxspan-insert-left! sp value)
  (assert* 'fxspan-insert-left! (fixnum? value))
  (fxspan-resize-left! sp (fx1+ (fxspan-length sp)))
  (fxspan-set! sp 0 value))

;; insert one inexact number to the right.
(define (fxspan-insert-right! sp value)
  (assert* 'fxspan-insert-right! (fixnum? value))
  (let ((pos (fxspan-length sp)))
    (fxspan-resize-right! sp (fx1+ pos))
    (fxspan-set! sp pos value)))

;; insert range [src-start, src-end) of fxspan sp-src
;; at the beginning of fxspan sp-dst
(define fxspan-insert-left/fxspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'fxspan-insert-left/fxspan! (eq? sp-dst sp-src))
      (assert*     'fxspan-insert-left/fxspan! (fx<=?* 0 src-start src-end (fxspan-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty fxvector is a common optimization of Scheme compilers
        (assert-not* 'fxspan-insert-left/fxspan! (eq? (fxspan-vec sp-dst) (fxspan-vec sp-src)))
        (let ((len    (fxspan-length sp-dst))
              (src-n  (fx- src-end src-start)))
          (fxspan-resize-left! sp-dst (fx+ len src-n))
          (fxspan-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (fxspan-insert-left/fxspan! sp-dst sp-src 0 (fxspan-length sp-src)))))

;; append a portion of another fxspan to this fxspan
(define fxspan-insert-right/fxspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'fxspan-insert-right/fxspan! (eq? sp-dst sp-src))
      (assert*     'fxspan-insert-right/fxspan! (fx<=?* 0 src-start src-end src-end (fxspan-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty fxvector is a common optimization of Scheme compilers
        (assert-not* 'fxspan-insert-right/fxspan! (eq? (fxspan-vec sp-dst) (fxspan-vec sp-src)))
        (let ((pos   (fxspan-length sp-dst))
              (src-n (fx- src-end src-start)))
          (fxspan-resize-right! sp-dst (fx+ pos src-n))
          (fxspan-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (fxspan-insert-right/fxspan! sp-dst sp-src 0 (fxspan-length sp-src)))))

;; erase n elements at the left i.e. front of fxspan
(define (fxspan-delete-left! sp n)
  (assert* 'fxspan-delete-left! (fx<=? 0 n (fxspan-length sp)))
  (unless (fxzero? n)
    (fxspan-beg-set! sp (fx+ n (fxspan-beg sp)))))

;; erase n elements at the right i.e. back of fxspan
(define (fxspan-delete-right! sp n)
  (assert* 'fxspan-delete-right! (fx<=? 0 n (fxspan-length sp)))
  (unless (fxzero? n)
    (fxspan-end-set! sp (fx- (fxspan-end sp) n))))


;; create and return a closure that iterates on elements of fxspan sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in fxspan sp and #t,
;; or (values #<unspecified> #f) if end of fxspan is reached.
(define in-fxspan
  (case-lambda
    ((sp start end step)
      (assert* 'in-fxspan (fx<=?* 0 start end (fxspan-length sp)))
      (assert* 'in-fxspan (fx>=? step 0))
      (let ((pos start))
        (lambda ()
          (if (fx<? pos end)
            (let ((ret (fxspan-ref sp pos)))
              (set! pos (fx+ pos step))
              (values ret #t))
            (values #f #f)))))
    ((sp start end)
      (in-fxspan sp start end 1))
    ((sp)
      (in-fxspan sp 0 (fxspan-length sp) 1))))

;; (fxspan-iterate sp proc) iterates on all elements of given fxspan sp,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (fxspan-iterate sp proc)
  (do ((len (fxspan-length sp))
       (i   0 (fx1+ i)))
    ((or (fx>=? i len) (not (proc i (fxspan-ref sp i))))
     (fx>=? i len))))

(define port->fxspan
  (case-lambda
    ((in)
      (let %loop ((in in) (ret (make-fxspan 0)))
        (let-values (((_a value _b _c) (read-token in)))
          (if (eof-object? value)
            ret
            (begin
              (fxspan-insert-right! ret value)
              (%loop in ret))))))
    (()
      (port->fxspan (current-input-port)))))

(define fxspan->port
  (case-lambda
    ((sp out)
      (do ((i 0 (fx1+ i))
           (n (fxspan-length sp)))
          ((fx>=? i n))
        (write (fxspan-ref sp i) out)
        (newline out)))
    ((sp)
      (fxspan->port sp (current-output-port)))))

;; customize how "fxspan" objects are printed
(record-writer (record-type-descriptor %fxspan)
  (lambda (sp out writer)
    (display "(fxspan" out)
    (fxspan-iterate sp
      (lambda (i elem)
        (display #\space out)
        (writer elem out)))
    (display ")" out)))

) ; close library
