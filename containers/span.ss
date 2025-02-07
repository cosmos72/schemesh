;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers span (0 7 3))
  (export
    list->span vector->span vector->span* make-span span->list span->vector span span?
    span-length span-empty? span-clear! span-capacity span-capacity-front span-capacity-back
    span-ref span-back span-set! span-fill! span-fill-range! span-range->span* span-copy span-copy!
    span-reserve-front! span-reserve-back! span-resize-front! span-resize-back!
    span-insert-front! span-insert-back! span-insert-front/span! span-insert-back/span!
    span-erase-front! span-erase-back! span-iterate span-find span-rfind
    span-peek-beg span-peek-end span-peek-data)
  (import
    (rnrs)
    (only (chezscheme) break fx1+ fx1- record-writer reverse! vector-copy void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers misc))

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

(define (span-back sp)
  (assert* 'span-back (not (span-empty? sp)))
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
    ((src)
      (span-copy src 0 (span-length src)))
    ((src start end)
      (assert* 'span-copy (fx<=? 0 start end (span-length src)))
      (let* ((n (fx- end start))
             (dst (make-span n)))
        (vector-copy! (span-vec src) (fx+ (span-beg src) start)
                      (span-vec dst) (span-beg dst) n)
        dst))))


;; copy a range of elements from span src to span dst.
(define (span-copy! src src-start dst dst-start n)
  (assert* 'span-copy! (fx>=? src-start 0))
  (assert* 'span-copy! (fx>=? dst-start 0))
  (assert* 'span-copy! (fx>=? n 0))
  (assert* 'span-copy! (fx<=? (fx+ src-start n) (span-length src)))
  (assert* 'span-copy! (fx<=? (fx+ dst-start n) (span-length dst)))
  (vector-copy! (span-vec src) (fx+ src-start (span-beg src))
                (span-vec dst) (fx+ dst-start (span-beg dst)) n))


(define (span-reallocate-front! sp len cap)
  (assert* 'span-reallocate-front! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (span-length sp)))
        (old-vec (span-vec sp))
        (new-vec (make-vector cap))
        (new-beg (fx- cap len)))
    (vector-copy! old-vec (span-beg sp) new-vec new-beg copy-len)
    (span-beg-set! sp new-beg)
    (span-end-set! sp cap)
    (span-vec-set! sp new-vec)))

(define (span-reallocate-back! sp len cap)
  (assert* 'span-reallocate-back! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (span-length sp)))
        (old-vec (span-vec sp))
        (new-vec (make-vector cap)))
    (vector-copy! old-vec (span-beg sp) new-vec 0 copy-len)
    (span-beg-set! sp 0)
    (span-end-set! sp len)
    (span-vec-set! sp new-vec)))

;; return distance between begin of internal vector and last element
(define (span-capacity-front sp)
  (span-end sp))

;; return distance between first element and end of internal vector
(define (span-capacity-back sp)
  (fx- (vector-length (span-vec sp)) (span-beg sp)))

;; ensure distance between begin of internal vector and last element is >= n.
;; does NOT change the length
(define (span-reserve-front! sp len)
  (assert* 'span-reserve-front! (fx>=? len 0))
  (let ((vec (span-vec sp))
        (cap-front (span-capacity-front sp)))
    (cond
      ((fx<=? len cap-front)
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
      (#t
       ; vector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))
         (span-reallocate-front! sp (span-length sp) new-cap))))))

;; ensure distance between first element and end of internal vector is >= n.
;; does NOT change the length
(define (span-reserve-back! sp len)
  (assert* 'span-reserve-back! (fx>=? len 0))
  (let ((vec (span-vec sp))
        (cap-back (span-capacity-back sp)))
    (cond
      ((fx<=? len cap-back)
       ; nothing to do
       (void))
      ((fx<=? len (vector-length vec))
        ; vector is large enough, move elements to the front
        (let ((len (span-length sp)))
          (vector-copy! vec (span-beg sp) vec 0 len)
          (span-beg-set! sp 0)
          (span-end-set! sp len)))
      (#t
       ; vector is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))
         (span-reallocate-back! sp (span-length sp) new-cap))))))

;; grow or shrink span on the left (front), set length to n
(define (span-resize-front! sp len)
  (assert* 'span-resize-front! (fx>=? len 0))
  (span-reserve-front! sp len)
  (assert* 'span-resize-front! (fx>=? (span-capacity-front sp) len))
  (span-beg-set! sp (fx- (span-end sp) len)))

;; grow or shrink span on the right (back), set length to n
(define (span-resize-back! sp len)
  (assert* 'span-resize-back! (fx>=? len 0))
  (span-reserve-back! sp len)
  (assert* 'span-resize-back! (fx>=? (span-capacity-back sp) len))
  (span-end-set! sp (fx+ len (span-beg sp))))

(define (span-insert-front! sp . vals)
  (unless (null? vals)
    (let ((pos 0)
          (new-len (fx+ (span-length sp) (length vals))))
      (span-resize-front! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (span-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

(define (span-insert-back! sp . vals)
  (unless (null? vals)
    (let* ((pos (span-length sp))
           (new-len (fx+ pos (length vals))))
      (span-resize-back! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (span-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

;; prefix range [src-start, src-end) of another span into this span
(define (span-insert-front/span! sp-dst sp-src src-start src-end)
  (assert* 'span-insert-front/span! (not (eq? sp-dst sp-src)))
  (assert* 'span-insert-front/span! (fx<=? 0 src-start src-end (span-length sp-src)))
  (when (fx<? src-start src-end)
    (let ((len   (span-length sp-dst))
          (src-n (fx- src-end src-start)))
      (span-resize-front! sp-dst (fx+ len src-n))
      (span-copy! sp-src src-start sp-dst 0 src-n))))

;; append a portion of another span to this span
(define (span-insert-back/span! sp-dst sp-src src-start src-end)
  (assert* 'span-insert-back/span! (not (eq? sp-dst sp-src)))
  (assert* 'span-insert-back/span! (fx<=? 0 src-start src-end (span-length sp-src)))
  (when (fx<? src-start src-end)
    (let ((pos   (span-length sp-dst))
          (src-n (fx- src-end src-start)))
      (span-resize-back! sp-dst (fx+ pos src-n))
      (span-copy! sp-src src-start sp-dst pos src-n))))

;; erase n elements at the left (front) of span
(define (span-erase-front! sp n)
  (assert* 'span-erase-front! (fx<=? 0 n (span-length sp)))
  (unless (fxzero? n)
    ; TODO: zero-fill erased range? Helps GC
    (span-beg-set! sp (fx+ n (span-beg sp)))))

;; erase n elements at the right (back) of span
(define (span-erase-back! sp n)
  (assert* 'span-erase-back! (fx<=? 0 n (span-length sp)))
  (unless (fxzero? n)
    ; TODO: zero-fill erased range? Helps GC
    (span-end-set! sp (fx- (span-end sp) n))))

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
(define (span-iterate sp proc)
  (do ((i (span-beg sp) (fx1+ i))
       (n (span-end sp))
       (v (span-vec sp)))
    ((or (fx>=? i n) (not (proc i (vector-ref v i))))
     (fx>=? i n))))


;; (span-find) iterates forward on span elements from start
;; up to (fxmin end (span-length sp)),
;; and returns the index of first span element that causes (predicate elem) to return truish.
;; Returned index is always in the range [start, end)
;; Returns #f if no such element is found.
(define (span-find sp start end predicate)
  (let ((end (fxmin end (span-length sp)))
        (ret #f))
    (do ((i start (fx1+ i)))
        ((or ret (fx>=? i end)) ret)
      (when (predicate (span-ref sp i))
        (set! ret i)))))

;; (span-find) iterates backward on span elements
;; from (fx1- (fxmin end (span-length sp))) down to start
;; and returns the index of last span element that causes (predicate elem) to return truish.
;; Returned index is always in the range [start, end)
;; Returns #f if no such element is found.
(define (span-rfind sp start end predicate)
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
