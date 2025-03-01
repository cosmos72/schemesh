;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  define Scheme type "gbuffer", a gap buffer  ;;;;;;;;;;;;;;;;;;;
;;;;;;; Implementation: contains two spans, a "left" and a "right" ones ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers gbuffer (0 7 7))
  (export
    list->gbuffer vector->gbuffer vector->gbuffer* span->gbuffer span->gbuffer*
    make-gbuffer gbuffer gbuffer? gbuffer->vector gbuffer->span
    gbuffer-length gbuffer-empty? gbuffer-ref gbuffer-set! gbuffer-clear! gbuffer-split-at!
    gbuffer-insert-at! gbuffer-erase-range! in-gbuffer gbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer void)
    (only (schemesh bootstrap)       assert* assert-not* -> ^)
    (only (schemesh containers misc) vector-copy!)
    (schemesh containers span))

(define-record-type
  (%gbuffer %make-gbuffer gbuffer?)
  (fields
    (mutable left  gbuffer-left  gbuffer-left-set!)
    (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))

(define (list->gbuffer l)
  (%make-gbuffer (span) (list->span l)))

(define (vector->gbuffer str)
  (%make-gbuffer (span) (vector->span str)))

; view a vector as gbuffer
(define (vector->gbuffer* str)
  (%make-gbuffer (span) (vector->span* str)))

(define (span->gbuffer sp)
  (%make-gbuffer (span) (span-copy sp)))

; view a span as gbuffer
(define (span->gbuffer* sp)
  (%make-gbuffer (span) sp))

(define (gbuffer->vector gb)
  (let* ((left  (gbuffer-left  gb))
         (right (gbuffer-right gb))
         (left-n  (span-length left))
         (right-n (span-length right))
         (dst (make-vector (fx+ left-n right-n))))
    (vector-copy! (span-peek-data left)  (span-peek-beg left)  dst 0 left-n)
    (vector-copy! (span-peek-data right) (span-peek-beg right) dst left-n right-n)
    dst))

(define (gbuffer->span gb)
  (vector->span* (gbuffer->vector gb)))

(define make-gbuffer
  (case-lambda
    ((n)      (%make-gbuffer (span) (make-span n)))
    ((n fill) (%make-gbuffer (span) (make-span n fill)))))

(define (gbuffer . vals)
  (list->gbuffer vals))

(define (gbuffer-length gb)
  (fx+ (-> gb gbuffer-left span-length) (-> gb gbuffer-right span-length)))

(define (gbuffer-empty? gb)
  (and (-> gb gbuffer-left span-empty?) (-> gb gbuffer-right span-empty?)))

(define (gbuffer-ref gb n)
  (assert* 'gbuffer-ref (fx<? -1 n (gbuffer-length gb)))
  (let ((left-n (-> gb gbuffer-left span-length)))
    (if (fx<? n left-n)
      (-> gb gbuffer-left  (span-ref ^ n))
      (-> gb gbuffer-right (span-ref ^ (fx- n left-n))))))

(define (gbuffer-set! gb idx val)
  (assert* 'gbuffer-set! (fx<? -1 idx (gbuffer-length gb)))
  (let ((left-n (-> gb gbuffer-left span-length)))
    (if (fx<? idx left-n)
      (-> gb gbuffer-left  (span-set! ^ idx val))
      (-> gb gbuffer-right (span-set! ^ (fx- idx left-n) val)))))

(define (gbuffer-clear! gb)
  (-> gb gbuffer-left  span-clear!)
  (-> gb gbuffer-right span-clear!))

(define (gbuffer-split-at! gb idx)
  (assert* 'gbuffer-split-at! (fx<=? 0 idx (gbuffer-length gb)))
  (let* ((left   (gbuffer-left  gb))
         (right  (gbuffer-right gb))
         (left-n (span-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fx>? delta 0)
        (span-insert-right/span! left right 0 delta)
        (span-erase-left! right delta))
      ((fx<? delta 0)
        (span-insert-left/span! right left idx left-n)
        (span-erase-right! left (fx- delta))))))

;; insert val into gbuffer at position idx
;; prerequisite: (fx<=? 0 idx (gbuffer-length gb))
(define (gbuffer-insert-at! gb idx val)
  (assert* 'gbuffer-insert-at! (fx<=? 0 idx (gbuffer-length gb)))
  (let ((left   (gbuffer-left  gb))
        (right  (gbuffer-right gb)))
    (cond
      ((fxzero? idx)
        (span-insert-left! left val))
      ((fx=? idx (gbuffer-length gb))
        (span-insert-right! right val))
      (else
        (gbuffer-split-at! gb idx)
        (span-insert-right! left val)))))

; read elements in range [src-start, src-end) from span sp-src
; and insert them into gbuffer at position idx
(define gbuffer-insert-at/span!
  (case-lambda
    ((gb idx sp-src src-start src-end)
      (assert* 'gbuffer-insert-at/span! (fx<=? 0 idx (gbuffer-length gb)))
      (assert* 'gbuffer-insert-at/span! (fx<=? 0 src-start src-end (span-length sp-src)))
      (when (fx<? src-start src-end)
        (let ((left   (gbuffer-left  gb))
              (right  (gbuffer-right gb)))
          (assert-not* 'gbuffer-insert-at/span! (eq? left sp-src))
          (assert-not* 'gbuffer-insert-at/span! (eq? right sp-src))
          (cond
            ((fxzero? idx)
              (span-insert-left/span! left sp-src src-start src-end))
            ((fx=? idx (gbuffer-length gb))
              (span-insert-right/span! right sp-src src-start src-end))
            (else
              (gbuffer-split-at! gb idx)
              (span-insert-right/span! left sp-src src-start src-end))))))
    ((gb idx sp-src)
      (gbuffer-insert-at/span! gb idx sp-src 0 (span-length sp-src)))))


; remove elements in range [start, end) from gbuffer gb
(define (gbuffer-erase-range! gb start end)
  (let* ((left    (gbuffer-left  gb))
         (right   (gbuffer-right gb))
         (left-n  (span-length left))
         (right-n (span-length right))
         (len     (fx+ left-n right-n))
         (n       (fx- end start)))
    (assert* 'gbuffer-erase-range! (fx<=? 0 start end len))
    (cond
      ((fxzero? n) (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (span-erase-left! left head)
          (span-erase-left! right (fx- n head))))
      ((fx=? end left-n)
        (span-erase-right! left n))
      ((fx=? start left-n)
        (span-erase-left! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (span-erase-right! right tail)
          (span-erase-right! left (fx- n tail))))
      (else
        (gbuffer-split-at! gb end)
        (span-erase-right! left n)))))


;; create and return a closure that iterates on elements of gbuffer gb.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in gbuffer gb and #t,
;; or (values #<unspecified> #f) if end of gbuffer is reached.
(define in-gbuffer
  (case-lambda
    ((gb start end step)
      (assert* 'in-gbuffer (fx<=? 0 start end (gbuffer-length gb)))
      (assert* 'in-gbuffer (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (gbuffer-ref gb start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #f #f))))
    ((gb start end)
      (in-gbuffer gb start end 1))
    ((gb)
      (in-gbuffer gb 0 (gbuffer-length gb) 1))))


;; iterate on gbuffer elements, and call (proc i elem) on each one.
;; Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc i elem) returned truish,
;; otherwise returns #f.
;;
;; The implementation of (proc ...) can call directly or indirectly functions
;; that inspect the gbuffer without modifying it, and can also call (gbuffer-set! sp ...).
;;
;; It must NOT call any other function that modifies the gbuffer (insert or erase elements,
;; change the gbuffer size or capacity, etc).
(define (gbuffer-iterate gb proc)
  (do ((i 0 (fx1+ i))
       (n (gbuffer-length gb)))
    ((or (fx>=? i n) (not (proc i (gbuffer-ref gb i))))
     (fx>=? i n))))


; customize how "gbuffer" objects are printed
(record-writer (record-type-descriptor %gbuffer)
  (lambda (sp port writer)
    (display "(gbuffer" port)
    (gbuffer-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display ")" port)))

) ; close library
