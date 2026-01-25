;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;  define Scheme type "span", a resizeable vector  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers span (0 9 3))
  (export
    list->span vector->span vector->span* make-span span->list span->vector span span?
    span-length span-empty? span-clear! span-capacity span-capacity-left span-capacity-right
    span-ref span-ref-right span-set! span-fill! subspan/shared span-copy span-copy!
    span-reserve-left! span-reserve-right! span-resize-left! span-resize-right!
    span-insert-left! span-insert-right! span-insert-left/span! span-insert-right/span!
    span-insert-left/vector! span-insert-right/vector!
    span-delete-left! span-delete-right! span-index span-index-right
    for-span in-span span-iterate span-iterate-any
    span-peek-beg span-peek-end span-peek-data)
  (import
    (rnrs)
    (only (chezscheme) break fx1+ fx1- record-writer reverse! vector-copy void)
    (only (scheme2k bootstrap)         assert* assert-not* forever fx<=?* generate-pretty-temporaries with-while-until)
    (only (scheme2k containers list)   for-list)
    (only (scheme2k containers vector) subvector vector-copy! subvector-fill! subvector->list))

(define-record-type (%span %make-span span?)
  (fields
     (mutable beg span-beg span-beg-set!)
     (mutable end span-end span-end-set!)
     (mutable vec span-vec span-vec-set!))
  (nongenerative %span-7c46d04b-34f4-4046-b5c7-b63753c1be39))

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
  (subvector->list (span-vec sp) (span-beg sp) (span-end sp)))

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
;; that can be stored without reallocating
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

(define span-fill!
  (case-lambda
    ((sp val)
      (subvector-fill! (span-vec sp) (span-beg sp) (span-end sp) val))
    ((sp start end val)
      (assert* 'span-fill! (fx<=?* 0 start end (span-length sp)))
      (let ((offset (span-beg sp)))
        (subvector-fill! (span-vec sp) (fx+ start offset) (fx+ end offset) val)))))


;; view the range [start, end) of span sp as a new span, and return it.
;; note: setting the elements of either span will propagate to the other
;; (provided they are in range of both spans) until one of the spans is reallocated.
(define (subspan/shared sp start end)
  (assert* 'subspan/shared (fx<=?* 0 start end (span-length sp)))
  (%make-span (fx+ start (span-beg sp)) (fx+ end (span-beg sp)) (span-vec sp)))


;; copy of a range of elements from span src to a new span,
;; and return the new span.
(define span-copy
  (case-lambda
    ((src start end)
      (assert* 'span-copy (fx<=?* 0 start end (span-length src)))
      (let* ((n (fx- end start))
             (dst (make-span n)))
        (vector-copy! (span-vec src) (fx+ (span-beg src) start)
                      (span-vec dst) (span-beg dst) n)
        dst))
    ((src)
      (span-copy src 0 (span-length src)))))


;; copy a range of elements from span src to span dst.
(define (span-copy! src src-start dst dst-start n)
  (assert* 'span-copy! (fx<=?* 0 src-start (fx+ src-start n) (span-length src)))
  (assert* 'span-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (span-length dst)))
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

(define (span-copy-list! sp pos l)
  (do ((tail l   (cdr tail))
       (pos  pos (fx1+ pos)))
      ((null? tail))
    (span-set! sp pos (car tail))))

(define (span-insert-left! sp . vals)
  (unless (null? vals)
    (let ((new-len (fx+ (span-length sp) (length vals))))
      (span-resize-left! sp new-len))
    (span-copy-list! sp 0 vals)))

(define (span-insert-right! sp . vals)
  (unless (null? vals)
    (let* ((pos (span-length sp))
           (new-len (fx+ pos (length vals))))
      (span-resize-right! sp new-len)
      (span-copy-list! sp pos vals))))

;; prefix range [src-start, src-end) of another span into this span
(define span-insert-left/span!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert*       'span-insert-left/span! (fx<=?* 0 src-start src-end (span-length sp-src)))
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
      (assert*       'span-insert-right/span! (fx<=?* 0 src-start src-end (span-length sp-src)))
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


;; prefix range [start, end) of a vector into this span
(define span-insert-left/vector!
  (case-lambda
    ((sp vec start end)
      (assert*       'span-insert-left/vector! (span? sp))
      (assert*       'span-insert-left/vector! (fx<=?* 0 start end (vector-length vec)))
      (when (fx<? start end)
        ;; check for (not (eq? dst vec)) only if vec is non-empty,
        ;; because reusing the empty vector is a common optimization of Scheme compilers
        (assert-not* 'span-insert-left/vector! (eq? (span-peek-data sp) vec))
        (let ((len (span-length sp))
              (n   (fx- end start)))
          (span-resize-left! sp (fx+ len n))
          (vector-copy! vec start
                        (span-peek-data sp) (span-peek-beg sp)
                        n))))
    ((sp vec)
      (span-insert-left/vector! sp vec 0 (vector-length vec)))))


;; append a portion of a vector to this span
(define span-insert-right/vector!
  (case-lambda
    ((sp vec start end)
      (assert*       'span-insert-right/vector! (span? sp))
      (assert*       'span-insert-right/vector! (fx<=?* 0 start end (vector-length vec)))
      (when (fx<? start end)
        ;; check for (not (eq? dst vec)) only if vec is non-empty,
        ;; because reusing the empty vector is a common optimization of Scheme compilers
        (assert-not* 'span-insert-right/vector! (eq? (span-peek-data sp) vec))
        (let ((pos   (span-length sp))
              (n     (fx- end start)))
          (span-resize-right! sp (fx+ pos n))
          (vector-copy! vec start
                        (span-peek-data sp) (fx- (span-end sp) n)
                        n))))
    ((sp vec)
      (span-insert-right/vector! sp vec 0 (vector-length vec)))))


;; erase n elements at the left (front) of span
(define (span-delete-left! sp n)
  (assert* 'span-delete-left! (fx<=? 0 n (span-length sp)))
  (unless (fxzero? n)
    ; TODO: zero-fill erased range? Helps GC
    (span-beg-set! sp (fx+ n (span-beg sp)))))


;; erase n elements at the right (back) of span
(define (span-delete-right! sp n)
  (assert* 'span-delete-right! (fx<=? 0 n (span-length sp)))
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
      (assert* 'in-span (fx<=?* 0 start end (span-length sp)))
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


;; Iterate in parallel on elements of given spans sp ..., and evaluate body ... on each element.
;; Stop iterating when the shortest span is exhausted,
;; and return unspecified value.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect the spans without modifying them, and can also call (span-set! ...).
;;
;; It must NOT call any other function that modify the spans (insert or erase elements,
;; change any span size or capacity, etc).
;;
;; Return unspecified value.
(define-syntax for-span
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((elem sp) ...) body ...)
        (with-syntax (((tsp ...) (generate-pretty-temporaries #'(sp ...))))
          #'(let ((tsp sp) ...)
              (let %for-span ((i 0) (n (fxmin (span-length sp) ...)))
                (when (fx<? i n)
                  (let ((elem (span-ref tsp i)) ...)
                    (with-while-until
                      body ...
                      (%for-span (fx1+ i) n)))))))))))


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
(define span-iterate-any
  (case-lambda
    ((sp start end proc)
      (assert* 'span-iterate-any (fx<=?* 0 start end (span-length sp)))
      (assert* 'span-iterate-any (procedure? proc))
      (let ((offset (span-beg sp))
            (v (span-vec sp)))
        (let %any ((i start))
          (if (fx<? i end)
            (or (proc i (vector-ref v (fx+ i offset)))
                (%any (fx1+ i)))
            #f))))
    ((sp proc)
      (span-iterate-any sp 0 (span-length sp) proc))))


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
    ((sp start end proc)
      (assert* 'span-iterate (fx<=?* 0 start end (span-length sp)))
      (assert* 'span-iterate (procedure? proc))
      (do ((i start (fx1+ i))
           (offset (span-beg sp))
           (v      (span-vec sp)))
        ((or (fx>=? i end) (not (proc i (vector-ref v (fx+ i offset)))))
          (fx>=? i end))))
    ((sp proc)
      (span-iterate sp 0 (span-length sp) proc))))


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
