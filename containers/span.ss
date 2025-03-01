;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers span (0 7 7))
  (export
    list->span vector->span vector->span* make-span span->list span->vector span span?
    span-length span-empty? span-clear! span-capacity span-capacity-left span-capacity-right
    span-ref span-ref-right span-set! span-fill! span-fill-range! span-range->span* span-copy span-copy!
    span-reserve-left! span-reserve-right! span-resize-left! span-resize-right!
    span-insert-left! span-insert-right! span-insert-left/span! span-insert-right/span!
    span-erase-left! span-erase-right! span-index span-index-right
    in-span span-any span-iterate
    span-peek-beg span-peek-end span-peek-data)
  (import
    (rnrs)
    (only (chezscheme) break fx1+ fx1- record-writer reverse! vector-copy void)
    (only (schemesh bootstrap)         assert* assert-not*)
    (only (schemesh containers list)   list-iterate)
    (only (schemesh containers misc)   subvector vector-copy! vector-fill-range! vector-range->list))

(define-record-type
  (%span %make-span span?)
  (fields
     (mutable beg span-beg span-beg-set!)
     (mutable end span-end span-end-set!)
     (mutable vec span-vec span-vec-set!))
  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))

(define span-peek-beg span-beg)
(define span-peek-end span-end)
(define span-peek-data span-vec)

(define (list->span l)
  (let ((vec (list->vector l)))
    (%make-span 0 (vector-length vec) vec)))

;; create span copying contents of specified vector
(define (vector->span vec)
  (%make-span 0 (vector-length vec) (vector-copy vec)))

;; view existing vector as span
(define (vector->span* vec)
  (%make-span 0 (vector-length vec) vec))

(define make-span
  (case-lambda
    ((n)      (%make-span 0 n (make-vector n)))
    ((n fill) (%make-span 0 n (make-vector n fill)))))

(define (span->list sp)
  (vector-range->list (span-vec sp) (span-beg sp) (span-end sp)))

(define (span->vector sp)
  (let ((beg (span-beg sp))
        (end (span-end sp)))
    (if (fx<? beg end)
      (subvector (span-vec sp) beg end)
      '#())))

(define (span . vals)
  (list->span vals))

(define (span-length sp)
  (fx- (span-end sp) (span-beg sp)))

;; return length of internal vector, i.e. maximum number of elements
; that can be stored without reallocating
(define (span-capacity sp)
  (vector-length (span-vec sp)))

(define (span-empty? sp)
  (fx>=? (span-beg sp) (span-end sp)))

(define (span-clear! sp)
  (span-fill! sp 0) ; slower, but helps GC
  (span-beg-set! sp 0)
  (span-end-set! sp 0))

(define (span-ref sp idx)
  (assert* 'span-ref (fx<? -1 idx (span-length sp)))
  (vector-ref (span-vec sp) (fx+ idx (span-beg sp))))

(define (span-ref-right sp)
  (assert* 'span-ref-right (not (span-empty? sp)))
  (vector-ref (span-vec sp) (fx1- (span-end sp))))

(define (span-set! sp idx val)
  (assert* 'span-set! (fx<? -1 idx (span-length sp)))
  (vector-set! (span-vec sp) (fx+ idx (span-beg sp)) val))

(define (span-fill! sp val)
  (vector-fill-range! (span-vec sp) (span-beg sp) (span-end sp) val))

(define (span-fill-range! sp start end val)
  (assert* 'span-fill-range! (fx<=? 0 start end (span-length sp)))
  (let ((offset (span-beg sp)))
    (vector-fill-range! (span-vec sp) (fx+ start offset) (fx+ end offset) val)))


;; view the range [start, end) of span sp as a new span, and return it.
;; note: modifying the content of either span may propagate to the other.
(define (span-range->span* sp start end)
  (assert* 'span-range->span* (fx<=? 0 start end (span-length sp)))
  (%make-span (fx+ start (span-beg sp)) (fx+ end (span-beg sp)) (span-vec sp)))


;; copy of a range of elements from span src to a new span,
;; and return the new span.
(define span-copy
  (case-lambda
    ((src start end)
      (assert* 'span-copy (fx<=? 0 start end (span-length src)))
      (let* ((n (fx- end start))
             (dst (make-span n)))
        (vector-copy! (span-vec src) (fx+ (span-beg src) start)
                      (span-vec dst) (span-beg dst) n)
        dst))
    ((src)
      (span-copy src 0 (span-length src)))))


;; copy a range of elements from span src to span dst.
(define (span-copy! src src-start dst dst-start n)
  (assert* 'span-copy! (fx<=? 0 src-start (fx+ src-start n) (span-length src)))
  (assert* 'span-copy! (fx<=? 0 dst-start (fx+ dst-start n) (span-length dst)))
  (vector-copy! (span-vec src) (fx+ src-start (span-beg src))
                (span-vec dst) (fx+ dst-start (span-beg dst)) n))


(define (span-reallocate-left! sp len cap)
  (assert* 'span-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (span-length sp)))
        (old-vec (span-vec sp))
        (new-vec (make-vector cap))
        (new-beg (fx- cap len)))
    (vector-copy! old-vec (span-beg sp) new-vec new-beg copy-len)
    (span-beg-set! sp new-beg)
    (span-end-set! sp cap)
    (span-vec-set! sp new-vec)))

(define (span-reallocate-right! sp len cap)
  (assert* 'span-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (span-length sp)))
        (old-vec (span-vec sp))
        (new-vec (make-vector cap)))
    (vector-copy! old-vec (span-beg sp) new-vec 0 copy-len)
    (span-beg-set! sp 0)
    (span-end-set! sp len)
    (span-vec-set! sp new-vec)))

;; return distance between begin of internal vector and last element
(define (span-capacity-left sp)
  (span-end sp))

;; return distance between first element and end of internal vector
(define (span-capacity-right sp)
  (fx- (vector-length (span-vec sp)) (span-beg sp)))

;; ensure distance between begin of internal vector and last element is >= n.
;; does NOT change the length
(define (span-reserve-left! sp len)
  (assert* 'span-reserve-left! (fx>=? len 0))
  (let ((vec (span-vec sp))
        (cap-left (span-capacity-left sp)))
    (cond
      ((fx<=? len cap-left)
       ; nothing to do
       (void))
      ((fx<=? len (vector-length vec))
        ; vector is large enough, move elements to the back
        (let* ((cap (span-capacity sp))
               (old-len (span-length sp))
               (new-beg (fx- cap old-len)))
          (vector-copy! vec (span-beg sp) vec new-beg old-len)
          (span-beg-set! sp new-beg)
          (span-end-set! sp cap)))
      (else
       ; vector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
         (span-reallocate-left! sp (span-length sp) new-cap))))))

;; ensure distance between first element and end of internal vector is >= n.
;; does NOT change the length
(define (span-reserve-right! sp len)
  (assert* 'span-reserve-right! (fx>=? len 0))
  (let ((vec (span-vec sp))
        (cap-right (span-capacity-right sp)))
    (cond
      ((fx<=? len cap-right)
       ; nothing to do
       (void))
      ((fx<=? len (vector-length vec))
        ; vector is large enough, move elements to the front
        (let ((len (span-length sp)))
          (vector-copy! vec (span-beg sp) vec 0 len)
          (span-beg-set! sp 0)
          (span-end-set! sp len)))
      (else
       ; vector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
         (span-reallocate-right! sp (span-length sp) new-cap))))))

;; grow or shrink span on the left (front), set length to n
(define (span-resize-left! sp len)
  (assert* 'span-resize-left! (fx>=? len 0))
  (span-reserve-left! sp len)
  (assert* 'span-resize-left! (fx>=? (span-capacity-left sp) len))
  (span-beg-set! sp (fx- (span-end sp) len)))

;; grow or shrink span on the right (back), set length to n
(define (span-resize-right! sp len)
  (assert* 'span-resize-right! (fx>=? len 0))
  (span-reserve-right! sp len)
  (assert* 'span-resize-right! (fx>=? (span-capacity-right sp) len))
  (span-end-set! sp (fx+ len (span-beg sp))))

(define (span-insert-left! sp . vals)
  (unless (null? vals)
    (let ((pos 0)
          (new-len (fx+ (span-length sp) (length vals))))
      (span-resize-left! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (span-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

(define (span-insert-right! sp . vals)
  (unless (null? vals)
    (let* ((pos (span-length sp))
           (new-len (fx+ pos (length vals))))
      (span-resize-right! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (span-set! sp pos elem)
          (set! pos (fx1+ pos)))))))


;; prefix range [src-start, src-end) of another span into this span
(define span-insert-left/span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert*     'span-insert-left/span! (fx<=? 0 src-start src-end (span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty vector is a common optimization of Scheme compilers
        (assert-not* 'span-insert-left/span! (eq? sp-dst sp-src))
        (assert-not* 'span-insert-left/span! (eq? (span-peek-data sp-dst) (span-peek-data sp-src)))
        (let ((len   (span-length sp-dst))
              (src-n (fx- src-end src-start)))
          (span-resize-left! sp-dst (fx+ len src-n))
          (span-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (span-insert-left/span! sp-dst sp-src 0 (span-length sp-src)))))


;; append a portion of another span to this span
(define span-insert-right/span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert*     'span-insert-right/span! (fx<=? 0 src-start src-end (span-length sp-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty vector is a common optimization of Scheme compilers
        (assert-not* 'span-insert-right/span! (eq? sp-dst sp-src))
        (assert-not* 'span-insert-right/span! (eq? (span-peek-data sp-dst) (span-peek-data sp-src)))
        (let ((pos   (span-length sp-dst))
              (src-n (fx- src-end src-start)))
          (span-resize-right! sp-dst (fx+ pos src-n))
          (span-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (span-insert-right/span! sp-dst sp-src 0 (span-length sp-src)))))


;; erase n elements at the left (front) of span
(define (span-erase-left! sp n)
  (assert* 'span-erase-left! (fx<=? 0 n (span-length sp)))
  (unless (fxzero? n)
    ; TODO: zero-fill erased range? Helps GC
    (span-beg-set! sp (fx+ n (span-beg sp)))))


;; erase n elements at the right (back) of span
(define (span-erase-right! sp n)
  (assert* 'span-erase-right! (fx<=? 0 n (span-length sp)))
  (unless (fxzero? n)
    ; TODO: zero-fill erased range? Helps GC
    (span-end-set! sp (fx- (span-end sp) n))))


;; create and return a closure that iterates on elements of span sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in span sp and #t,
;; or (values #<unspecified> #f) if end of span is reached.
(define in-span
  (case-lambda
    ((sp start end step)
      (assert* 'in-span (fx<=? 0 start end (span-length sp)))
      (assert* 'in-span (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (span-ref sp start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #f #f))))
    ((sp start end)
      (in-span sp start end 1))
    ((sp)
      (in-span sp 0 (span-length sp) 1))))


;; iterate on span elements, and call (proc i elem) on each one.
;; if (proc ...) evaluates to truish, stop iterating and return such value.
;;
;; Returns #f if all calls to (proc i elem) evaluated to #f.
;;
;; The implementation of (proc ...) can call directly or indirectly functions
;; that inspect the span without modifying it, and can also call (span-set! sp ...).
;;
;; It must NOT call any other function that modifies the span (insert or erase elements,
;; change the span size or capacity, etc).
(define span-any
  (case-lambda
    ((sp proc start end)
      (assert* 'span-any (fx<=? 0 start end (span-length sp)))
      (assert* 'span-any (procedure? proc))
      (let %any ((i start) (v (span-vec sp)))
        (if (fx<? i end)
          (or (proc (fx- i start) (vector-ref v i))
              (%any (fx1+ i) v))
          #f)))
    ((sp proc)
      (span-any sp proc 0 (span-length sp)))))


;; iterate on span elements, and call (proc i elem) on each one.
;; Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc i elem) returned truish,
;; otherwise returns #f.
;;
;; The implementation of (proc ...) can call directly or indirectly functions
;; that inspect the span without modifying it, and can also call (span-set! sp ...).
;;
;; It must NOT call any other function that modifies the span (insert or erase elements,
;; change the span size or capacity, etc).
(define span-iterate
  (case-lambda
    ((sp proc start end)
      (assert* 'span-iterate (fx<=? 0 start end (span-length sp)))
      (assert* 'span-iterate (procedure? proc))
      (do ((i start (fx1+ i))
           (offset (span-beg sp))
           (v      (span-vec sp)))
        ((or (fx>=? i end) (not (proc i (vector-ref v (fx+ i offset)))))
          (fx>=? i end))))
    ((sp proc)
      (span-iterate sp proc 0 (span-length sp)))))


;; (span-index) iterates forward on span elements from start
;; up to (fxmin end (span-length sp)),
;; and returns the index of first span element that causes (predicate elem) to return truish.
;; Returned index is always in the range [start, end)
;; Returns #f if no such element is found.
(define (span-index sp start end predicate)
  (let ((end (fxmin end (span-length sp)))
        (ret #f))
    (do ((i start (fx1+ i)))
        ((or ret (fx>=? i end)) ret)
      (when (predicate (span-ref sp i))
        (set! ret i)))))

;; (span-index) iterates backward on span elements
;; from (fx1- (fxmin end (span-length sp))) down to start
;; and returns the index of last span element that causes (predicate elem) to return truish.
;; Returned index is always in the range [start, end)
;; Returns #f if no such element is found.
(define (span-index-right sp start end predicate)
  (let ((end (fxmin end (span-length sp)))
        (ret #f))
    (do ((i (fx1- end) (fx1- i)))
        ((or ret (fx<? i start)) ret)
      (when (predicate (span-ref sp i))
        (set! ret i)))))


;;  customize how "span" objects are printed
(record-writer (record-type-descriptor %span)
  (lambda (sp port writer)
    (display "(span" port)
    (span-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
