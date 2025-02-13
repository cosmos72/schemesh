;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file containers/misc.ss


(define (list->bytevector l)
  (apply bytevector l))


;; return a copy of bytevector bvec containing only elements
;; from start (inclusive) to end (exclusive)
(define (subbytevector bvec start end)
  (assert* 'subbytevector (fx<=? 0 start end (bytevector-length bvec)))
  (let* ((n (fx- end start))
         (dst (make-bytevector n)))
    (bytevector-copy! bvec start dst 0 n)
    dst))

(define (bytevector-fill-range! bvec start end val)
  (assert* 'bytevector-fill-range! (fx<=? 0 start end (bytevector-length bvec)))
  (do ((i start (fx1+ i)))
      ((fx>=? i end))
    (bytevector-u8-set! bvec i val)))


;; search bytevector range [start, end) and return index of first byte equal to b.
;; returned numerical index will be in the range [start, end).
;; return #f if no such byte is found in range.
(define bytevector-find/u8
  (case-lambda
    ((bvec start end b)
      (assert* 'bytevector-find/u8 (bytevector? bvec))
      (assert* 'bytevector-find/u8 (fx<=? 0 start end (bytevector-length bvec)))
      (assert* 'bytevector-find/u8 (fx<=? 0 b 255))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (fx=? b (bytevector-u8-ref bvec i)))
            (if (fx>=? i end) #f i))))
    ((bvec b)
      (bytevector-find/u8 bvec 0 (bytevector-length bvec) b))))


;; (bytevector-iterate l proc) iterates on all elements of given bytevector bvec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (bytevector-iterate bvec proc)
  (do ((i 0 (fx1+ i))
       (n (bytevector-length bvec)))
      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i))))
       (fx>=? i n))))

;; compare the two bytevectors bvec1 and bvec2.
;; return -1 if bvec1 is lexicographically lesser than bvec2,
;; return 0 if they are equal,
;; return 1 if bvec1 is lexicographically greater than bvec2
(define bytevector-compare
  (let ((c-bytevector-compare (foreign-procedure "c_bytevector_compare"
          (ptr ptr) integer-8)))
    (lambda (bvec1 bvec2)
      (assert* 'bytevector-compare (bytevector? bvec1))
      (assert* 'bytevector-compare (bytevector? bvec2))
      (or (eq? bvec1 bvec2)
          (c-bytevector-compare bvec1 bvec2)))))

(define (bytevector<=? bvec1 bvec2)
  (fx<=? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector<? bvec1 bvec2)
  (fx<? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector>=? bvec1 bvec2)
  (fx>=? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector>? bvec1 bvec2)
  (fx>? (bytevector-compare bvec1 bvec2) 0))
