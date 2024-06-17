;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  define Scheme type "gbuffer", a gap buffer  ;;;;;;;;;;;;;;;;;;;
;;;;;;; Implementation: contains two spans, a "left" and a "right" ones ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers gbuffer (0 1))
  (export
    list->gbuffer vector->gbuffer vector->gbuffer* span->gbuffer span->gbuffer*
    make-gbuffer gbuffer gbuffer? gbuffer->vector gbuffer->span
    gbuffer-length gbuffer-empty? gbuffer-ref gbuffer-set! gbuffer-clear! gbuffer-split-at!
    gbuffer-insert-at! gbuffer-erase-at! gbuffer-iterate)
  (import
    (rnrs)
    (only (chezscheme) fx1+ record-writer void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers misc)
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
  (fx+ (span-length (gbuffer-left gb)) (span-length (gbuffer-right gb))))

(define (gbuffer-empty? gb)
  (and (span-empty? (gbuffer-left gb)) (span-empty? (gbuffer-right gb))))

(define (gbuffer-ref gb n)
  (assert* 'gbuffer-ref (fx<? -1 n (gbuffer-length gb)))
  (let ((left-n (span-length (gbuffer-left gb))))
    (if (fx<? n left-n)
      (span-ref (gbuffer-left  gb) n)
      (span-ref (gbuffer-right gb) (fx- n left-n)))))

(define (gbuffer-set! gb idx val)
  (assert* 'gbuffer-set! (fx<? -1 idx (gbuffer-length gb)))
  (let ((left-n (span-length (gbuffer-left gb))))
    (if (fx<? idx left-n)
      (span-set! (gbuffer-left  gb) idx val)
      (span-set! (gbuffer-right gb) (fx- idx left-n) val))))

(define (gbuffer-clear! gb)
  (span-clear! (gbuffer-left  gb))
  (span-clear! (gbuffer-right gb)))

(define (gbuffer-split-at! gb idx)
  (assert* 'gbuffer-split-at! (fx<=? 0 idx (gbuffer-length gb)))
  (let* ((left  (gbuffer-left  gb))
         (right (gbuffer-right gb))
         (delta (fx- idx (span-length left))))
    (cond
      ((fx>? delta 0)
        (span-insert-back/span! left right 0 delta)
        (span-erase-front! right delta))
      ((fx<? delta 0)
        (span-insert-front/span! right left idx (fx- delta))
        (span-erase-back! left (fx- delta))))))

;; insert val into gbuffer at position idx
;; prerequisite: (fx<=? 0 idx (gbuffer-length gb))
(define (gbuffer-insert-at! gb idx val)
  (assert* 'gbuffer-insert-at! (fx<=? 0 idx (gbuffer-length gb)))
  (let* ((left   (gbuffer-left  gb))
         (right  (gbuffer-right gb))
         (left-n (span-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? idx)
        (span-insert-front! left val))
      ((fx=? idx (gbuffer-length gb))
        (span-insert-back! right val))
      (#t
        (gbuffer-split-at! gb idx)
        (span-insert-back! left val)))))

; read src-n elements from span sp-src starting from src-start
; and insert them into gbuffer at position idx
(define (gbuffer-insert-at/span! gb idx sp-src src-start src-n)
  (assert* 'gbuffer-insert-at/span! (fx<=? 0 idx (gbuffer-length gb)))
  (let* ((left   (gbuffer-left  gb))
         (right  (gbuffer-right gb))
         (left-n (span-length left))
         (delta  (fx- idx left-n)))
    (cond
      ((fxzero? src-n) ; nothing to do
        (assert* 'gbuffer-insert-at/span! (fx<=? 0 src-start (span-length sp-src))))
      ((fxzero? idx)
        (span-insert-front/span! left sp-src src-start src-n))
      ((fx=? idx (gbuffer-length gb))
        (span-insert-back/span! right sp-src src-start src-n))
      (#t
        (gbuffer-split-at! gb idx)
        (span-insert-back/span! left sp-src src-start src-n)))))

; remove n elements from gbuffer starting at start
(define (gbuffer-erase-at! gb start n)
  (let* ((left    (gbuffer-left  gb))
         (right   (gbuffer-right gb))
         (left-n  (span-length left))
         (right-n (span-length right))
         (len     (fx+ left-n right-n))
         (end     (fx+ start n)))
    (assert* 'gbuffer-erase-at! (fx<=? 0 start len))
    (assert* 'gbuffer-erase-at! (fx<=? 0 n (fx- len start)))
    (cond
      ((fxzero? n) (void)) ; nothing to do
      ((fxzero? start)
        (let ((head (fxmin n left-n)))
          (span-erase-front! left head)
          (span-erase-front! right (fx- n head))))
      ((fx=? end left-n)
        (span-erase-back! left n))
      ((fx=? start left-n)
        (span-erase-front! right n))
      ((fx=? end len)
        (let ((tail (fxmin n right-n)))
          (span-erase-back! right tail)
          (span-erase-back! left (fx- n tail))))
      (#t
        (gbuffer-split-at! gb end)
        (span-erase-back! left n)))))

(define (gbuffer-iterate gb proc)
  (do ((i 0 (fx1+ i))
       (n (gbuffer-length gb)))
    ((or (fx>=? i n) (not (proc i (gbuffer-ref gb i)))))))

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
