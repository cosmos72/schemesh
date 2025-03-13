;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; define Scheme type "bytespan", a resizeable bytevector  ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers bytespan (0 8 1))
  (export
    list->bytespan bytevector->bytespan bytevector->bytespan* make-bytespan
    bytespan->bytevector bytespan->bytevector*!
    bytespan bytespan? bytespan-length bytespan-empty? bytespan-clear!
    bytespan-capacity bytespan-capacity-left bytespan-capacity-right
    bytespan-ref/u8 bytespan-ref-right/u8 bytespan-set/u8!
    bytespan-fill! bytespan-fill-range! bytespan-copy bytespan-copy! bytespan=?
    bytespan-reserve-left! bytespan-reserve-right! bytespan-resize-left! bytespan-resize-right!
    bytespan-insert-left/u8! bytespan-insert-right/u8!
    bytespan-insert-left/bspan! bytespan-insert-right/bspan!
    bytespan-insert-left/bvector! bytespan-insert-right/bvector!
    bytespan-erase-left! bytespan-erase-right! bytespan-index/u8
    in-bytespan bytespan-iterate
    bytespan-peek-beg bytespan-peek-end bytespan-peek-data)
  (import
    (rnrs)
    (only (chezscheme)         bytevector-truncate! fx1+ fx1- record-writer void)
    (only (schemesh bootstrap) assert* assert-not* fx<=?*)
    (only (schemesh containers list) for-list)
    (schemesh containers bytevector))

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

;; create bytespan copying contents of specified bytevector
(define (bytevector->bytespan vec)
  (%make-bytespan 0 (bytevector-length vec) (bytevector-copy vec)))

;; view existing bytevector as bytespan
(define (bytevector->bytespan* vec)
  (%make-bytespan 0 (bytevector-length vec) vec))

(define make-bytespan
  (case-lambda
    ((n)      (%make-bytespan 0 n (make-bytevector n)))
    ((n fill) (%make-bytespan 0 n (make-bytevector n fill)))))

;; convert a bytespan to bytevector
(define (bytespan->bytevector sp)
  (let ((beg (bytespan-beg sp))
        (end (bytespan-end sp)))
    (if (fx>=? beg end)
      #vu8()
      (subbytevector (bytespan-vec sp) beg end))))

;; if possible, truncate bytespan to its length and view it as a bytevector.
;; otherwise convert it to bytevector as (bytespan->bytevector) does.
(define (bytespan->bytevector*! sp)
  (if (or (bytespan-empty? sp) (not (fxzero? (bytespan-beg sp))))
    (bytespan->bytevector sp)
    (let ((bv (bytespan-vec sp)))
      (bytevector-truncate! bv (bytespan-end sp))
      bv)))

(define (bytespan . u8vals)
  (list->bytespan u8vals))

(define (bytespan-length sp)
  (fx- (bytespan-end sp) (bytespan-beg sp)))

;; return length of internal bytevector, i.e. maximum number of elements
;; that can be stored without reallocating
(define (bytespan-capacity sp)
  (bytevector-length (bytespan-vec sp)))

(define (bytespan-empty? sp)
  (fx>=? (bytespan-beg sp) (bytespan-end sp)))

(define (bytespan-clear! sp)
  (bytespan-beg-set! sp 0)
  (bytespan-end-set! sp 0))

(define (bytespan-ref/u8 sp idx)
  (assert* 'bytespan-ref/u8 (fx<? -1 idx (bytespan-length sp)))
  (bytevector-u8-ref (bytespan-vec sp) (fx+ idx (bytespan-beg sp))))

(define (bytespan-ref-right/u8 sp)
  (assert* 'bytespan-ref-right/u8 (not (bytespan-empty? sp)))
  (bytevector-u8-ref (bytespan-vec sp) (fx1- (bytespan-end sp))))

(define (bytespan-set/u8! sp idx u8)
  (assert* 'bytespan-set/u8! (fx<? -1 idx (bytespan-length sp)))
  (bytevector-u8-set! (bytespan-vec sp) (fx+ idx (bytespan-beg sp)) u8))

(define (bytespan-fill! sp u8)
  (bytevector-fill-range! (bytespan-vec sp) (bytespan-beg sp)
                          (bytespan-end sp) u8))

(define (bytespan-fill-range! sp start end u8)
  (assert* 'bytespan-fill-range! (fx<=?* 0 start end (bytespan-length sp)))
  (let ((offset (bytespan-beg sp)))
    (bytevector-fill-range! (bytespan-vec sp) (fx+ start offset) (fx+ end offset) u8)))

(define (bytespan-copy src)
  (let* ((n (bytespan-length src))
         (dst (make-bytespan n)))
    (bytevector-copy! (bytespan-vec src) (bytespan-beg src)
                      (bytespan-vec dst) (bytespan-beg dst) n)
    dst))

(define (bytespan-copy! src src-start dst dst-start n)
  (assert* 'bytespan-copy! (fx<=?* 0 src-start (fx+ src-start n) (bytespan-length src)))
  (assert* 'bytespan-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (bytespan-length dst)))
  (bytevector-copy! (bytespan-vec src) (fx+ src-start (bytespan-beg src))
                (bytespan-vec dst) (fx+ dst-start (bytespan-beg dst)) n))

(define (bytespan=? left right)
  (or
    (eq? left right)
    (and (eq?  (bytespan-vec left) (bytespan-vec right))
         (fx=? (bytespan-beg left) (bytespan-beg right))
         (fx=? (bytespan-end left) (bytespan-end right)))
    (let* ((n1 (bytespan-length left))
           (n2 (bytespan-length right))
           (equal (fx=? n1 n2)))
      (do ((i 0 (fx1+ i)))
          ((or (not equal) (fx>=? i n1)) equal)
        (set! equal (fx=? (bytespan-ref/u8 left i) (bytespan-ref/u8 right i)))))))

(define (bytespan-reallocate-left! sp len cap)
  (assert* 'bytespan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (bytespan-length sp)))
        (old-vec (bytespan-vec sp))
        (new-vec (make-bytevector cap))
        (new-beg (fx- cap len)))
    (bytevector-copy! old-vec (bytespan-beg sp) new-vec new-beg copy-len)
    (bytespan-beg-set! sp new-beg)
    (bytespan-end-set! sp cap)
    (bytespan-vec-set! sp new-vec)))

(define (bytespan-reallocate-right! sp len cap)
  (assert* 'bytespan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (bytespan-length sp)))
        (old-vec (bytespan-vec sp))
        (new-vec (make-bytevector cap)))
    (bytevector-copy! old-vec (bytespan-beg sp) new-vec 0 copy-len)
    (bytespan-beg-set! sp 0)
    (bytespan-end-set! sp len)
    (bytespan-vec-set! sp new-vec)))

;; return distance between begin of internal bytevector and last element
(define (bytespan-capacity-left sp)
  (bytespan-end sp))

;; return distance between first element and end of internal bytevector
(define (bytespan-capacity-right sp)
  (fx- (bytevector-length (bytespan-vec sp)) (bytespan-beg sp)))

;; ensure distance between begin of internal bytevector and last element is >= n.
;; does NOT change the length
(define (bytespan-reserve-left! sp len)
  (assert* 'bytespan-reserve-left! (fx>=? len 0))
  (let ((vec (bytespan-vec sp))
        (cap-left (bytespan-capacity-left sp)))
    (cond
      ((fx<=? len cap-left)
       ;; nothing to do
       (void))
      ((fx<=? len (bytevector-length vec))
        ;; bytevector is large enough, move elements to the back
        (let* ((cap (bytespan-capacity sp))
               (old-len (bytespan-length sp))
               (new-beg (fx- cap old-len)))
          (bytevector-copy! vec (bytespan-beg sp) vec new-beg old-len)
          (bytespan-beg-set! sp new-beg)
          (bytespan-end-set! sp cap)))
      (else
       ;; bytevector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
         (bytespan-reallocate-left! sp (bytespan-length sp) new-cap))))))

;; ensure distance between first element and end of internal bytevector is >= n.
;; does NOT change the length
(define (bytespan-reserve-right! sp len)
  (assert* 'bytespan-reserve-right! (fx>=? len 0))
  (let ((vec (bytespan-vec sp))
        (cap-right (bytespan-capacity-right sp)))
    (cond
      ((fx<=? len cap-right)
       ;; nothing to do
       (void))
      ((fx<=? len (bytevector-length vec))
        ;; bytevector is large enough, move elements to the front
        (let ((len (bytespan-length sp)))
          (bytevector-copy! vec (bytespan-beg sp) vec 0 len)
          (bytespan-beg-set! sp 0)
          (bytespan-end-set! sp len)))
      (else
       ;; bytevector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
         (bytespan-reallocate-right! sp (bytespan-length sp) new-cap))))))

;; grow or shrink bytespan on the left (front), set length to n
(define (bytespan-resize-left! sp len)
  (assert* 'bytespan-resize-left! (fx>=? len 0))
  (bytespan-reserve-left! sp len)
  (assert* 'bytespan-resize-left! (fx>=? (bytespan-capacity-left sp) len))
  (bytespan-beg-set! sp (fx- (bytespan-end sp) len)))

;; grow or shrink bytespan on the right (back), set length to n
(define (bytespan-resize-right! sp len)
  (assert* 'bytespan-resize-right! (fx>=? len 0))
  (bytespan-reserve-right! sp len)
  (assert* 'bytespan-resize-right! (fx>=? (bytespan-capacity-right sp) len))
  (bytespan-end-set! sp (fx+ len (bytespan-beg sp))))

(define (bytespan-insert-left/u8! sp . u8vals)
  (unless (null? u8vals)
    (let ((pos 0)
          (new-len (fx+ (bytespan-length sp) (length u8vals))))
      (bytespan-resize-left! sp new-len)
      (for-list ((elem u8vals))
        (bytespan-set/u8! sp pos elem)
        (set! pos (fx1+ pos))))))

(define (bytespan-insert-right/u8! sp . u8vals)
  (unless (null? u8vals)
    (let* ((pos (bytespan-length sp))
           (new-len (fx+ pos (length u8vals))))
      (bytespan-resize-right! sp new-len)
      (for-list ((elem u8vals))
        (bytespan-set/u8! sp pos elem)
        (set! pos (fx1+ pos))))))

;; insert range [src-start, src-end) of bytespan bv-src
;; at the beginning of bytespan sp-dst
(define bytespan-insert-left/bspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'bytespan-insert-left/bspan! (eq? sp-dst sp-src))
      (assert*     'bytespan-insert-left/bspan! (fx<=?* 0 src-start src-end (bytespan-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'bytespan-insert-left/bspan! (eq? (bytespan-vec sp-dst) (bytespan-vec sp-src)))
        (let ((len    (bytespan-length sp-dst))
              (src-n  (fx- src-end src-start)))
          (bytespan-resize-left! sp-dst (fx+ len src-n))
          (bytespan-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (bytespan-insert-left/bspan! sp-dst sp-src 0 (bytespan-length sp-src)))))

;; insert range [src-start, src-end) of bytevector bv-src
;; at the beginning of bytespan sp-dst
(define bytespan-insert-left/bvector!
  (case-lambda
    ((sp-dst bv-src src-start src-end)
      (assert*     'bytespan-insert-left/bvector! (fx<=?* 0 src-start src-end (bytevector-length bv-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'bytespan-insert-left/bvector! (eq? (bytespan-vec sp-dst) bv-src))
        (bytespan-insert-left/bspan! sp-dst (bytevector->bytespan* bv-src) src-start src-end)))
    ((sp-dst bv-src)
      (bytespan-insert-left/bvector! sp-dst bv-src 0 (bytevector-length bv-src)))))

;; append a portion of another bytespan to this bytespan
(define bytespan-insert-right/bspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert-not* 'bytespan-insert-right/bspan! (eq? sp-dst sp-src))
      (assert*     'bytespan-insert-right/bspan! (fx<=?* 0 src-start src-end src-end (bytespan-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty bytevector is a common optimization of Scheme compilers
        (assert-not* 'bytespan-insert-right/bspan! (eq? (bytespan-vec sp-dst) (bytespan-vec sp-src)))
        (let ((pos   (bytespan-length sp-dst))
              (src-n (fx- src-end src-start)))
          (bytespan-resize-right! sp-dst (fx+ pos src-n))
          (bytespan-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (bytespan-insert-right/bspan! sp-dst sp-src 0 (bytespan-length sp-src)))))

;; append a portion of a bytevector to this bytespan
(define bytespan-insert-right/bvector!
  (case-lambda
    ((sp-dst bv-src src-start src-end)
      (unless (fx=? src-start src-end)
        ; call bytespan-insert... accepting a bytespan second argument
        (bytespan-insert-right/bspan! sp-dst (bytevector->bytespan* bv-src) src-start src-end)))
    ((sp-dst bv-src)
      (bytespan-insert-right/bvector! sp-dst bv-src 0 (bytevector-length bv-src)))))

;; erase n elements at the left (front) of bytespan
(define (bytespan-erase-left! sp n)
  (assert* 'bytespan-erase-left! (fx<=? 0 n (bytespan-length sp)))
  (unless (fxzero? n)
    (bytespan-beg-set! sp (fx+ n (bytespan-beg sp)))))

;; erase n elements at the right (back) of bytespan
(define (bytespan-erase-right! sp n)
  (assert* 'bytespan-erase-right! (fx<=? 0 n (bytespan-length sp)))
  (unless (fxzero? n)
    (bytespan-end-set! sp (fx- (bytespan-end sp) n))))


;; create and return a closure that iterates on elements of bytespan sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in bytespan sp and #t,
;; or (values #<unspecified> #f) if end of bytespan is reached.
(define in-bytespan
  (case-lambda
    ((sp start end step)
      (assert* 'in-bytespan (fx<=?* 0 start end (bytespan-length sp)))
      (assert* 'in-bytespan (fx>=? step 0))
      (let ((offset (bytespan-beg sp)))
        (in-bytevector (bytespan-vec sp) (fx+ start offset) (fx+ end offset) step)))
    ((sp start end)
      (in-bytespan sp start end 1))
    ((sp)
      (in-bytespan sp 0 (bytespan-length sp) 1))))

;; (bytespan-iterate l proc) iterates on all elements of given bytespan sp,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (bytespan-iterate sp proc)
  (let ((start (bytespan-beg sp))
        (end   (bytespan-end sp))
        (bv    (bytespan-vec sp)))
    (do ((i start (fx1+ i)))
      ((or (fx>=? i end) (not (proc (fx- i start) (bytevector-u8-ref bv i))))
       (fx>=? i end)))))

;; (bytespan-index/u8) iterates on bytespan u8 elements in range [start, end)
;; and returns the index of first bytespan u8 element that causes
;; (predicate elem) to return truish. Returns #f if no such element is found.
(define bytespan-index/u8
  (case-lambda
    ((sp start end predicate)
      (assert* 'bytespan-index/u8 (fx<=?* 0 start end (bytespan-length sp)))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (bytespan-ref/u8 sp i)))
           (if (fx>=? i end) #f i))))
    ((sp predicate)
      (bytespan-index/u8 sp 0 (bytespan-length sp) predicate))))

;; customize how "bytespan" objects are printed
(record-writer (record-type-descriptor %bytespan)
  (lambda (sp port writer)
    (display "(bytespan" port)
    (bytespan-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
