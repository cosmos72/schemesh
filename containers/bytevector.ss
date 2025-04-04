;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh containers bytevector (0 8 2))
  (export
    in-bytevector list->bytevector subbytevector
    bytevector-compare bytevector-fill-range! bytevector-hash bytevector-index
    bytevector<=? bytevector<? bytevector>=? bytevector>? bytevector-iterate
    bytevector-uint-ref* bytevector-uint-set*! )
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         bytevector
                               bytevector-u24-ref  bytevector-u40-ref  bytevector-u48-ref  bytevector-u56-ref
                               bytevector-u24-set! bytevector-u40-set! bytevector-u48-set! bytevector-u56-set!
                               foreign-procedure fx1+ fx/ logbit? procedure-arity-mask)
    (only (schemesh bootstrap) assert* fx<=?*))


;; each element in list l must be a fixnum in the range [-128, 255]
(define (list->bytevector l)
  (apply bytevector l))


;; return a copy of bytevector bvec containing only elements
;; from start (inclusive) to end (exclusive)
(define (subbytevector bvec start end)
  (assert* 'subbytevector (fx<=?* 0 start end (bytevector-length bvec)))
  (let* ((n (fx- end start))
         (dst (make-bytevector n)))
    (bytevector-copy! bvec start dst 0 n)
    dst))

(define bytevector-fill-range!
  (let ((c-bytevector-fill-range (foreign-procedure "c_bytevector_fill_range" (ptr int int int) void)))
    (lambda (bvec start end val)
      (assert* 'bytevector-fill-range! (fx<=?* 0 start end (bytevector-length bvec)))
      (assert* 'bytevector-fill-range! (fx<=? -128 val 255))
      (let ((val (fxand val 255))
            (n   (fx- end start)))
        (if (fx<? n 3)
          (unless (fxzero? n)
            (bytevector-u8-set! bvec start val)
            (when (fx>? n 1)
              (bytevector-u8-set! bvec (fx1+ start) val)))
          (c-bytevector-fill-range bvec start end val))))))


;; search bytevector range [start, end) and return index of first byte equal to u8 or that satisfies pred.
;; returned numerical index will be in the range [start, end).
;; return #f if no such byte is found in range.
(define bytevector-index
  (let ((c-bytevector-index-u8 (foreign-procedure "c_bytevector_index_u8" (ptr int int int) ptr)))
    (case-lambda
      ((bvec start end byte-or-pred)
        (assert* 'bytevector-index (bytevector? bvec))
        (assert* 'bytevector-index (fx<=?* 0 start end (bytevector-length bvec)))
        (if (fixnum? byte-or-pred)
          (begin
            (assert* 'bytevector-index (fx<=? -128 byte-or-pred 255))
            (let ((u8 (fxand byte-or-pred 255)))
              (if (fx<? (fx- end start) 4)
                (do ((i start (fx1+ i)))
                    ((or (fx>=? i end) (fx=? u8 (bytevector-u8-ref bvec i)))
                      (if (fx<? i end) i #f)))
                (c-bytevector-index-u8 bvec start end u8))))
          (begin
            (assert* 'bytevector-index (logbit? 1 (procedure-arity-mask byte-or-pred)))
            (let ((pred byte-or-pred))
              (do ((i start (fx1+ i)))
                ((or (fx>=? i end) (pred (bytevector-u8-ref bvec i)))
                  (if (fx<? i end) i #f)))))))
      ((bvec byte-or-pred)
        (bytevector-index bvec 0 (bytevector-length bvec) byte-or-pred)))))



;; create and return a closure that iterates on elements of bytevector sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in bytevector sp and #t,
;; or (values #<unspecified> #f) if end of bytevector is reached.
(define in-bytevector
  (case-lambda
    ((sp start end step)
      (assert* 'in-bytevector (fx<=?* 0 start end (bytevector-length sp)))
      (assert* 'in-bytevector (fx>=? step 0))
      (let ((%in-bytevector ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (bytevector-u8-ref sp start)))
                    (set! start (fx+ start step))
                    (values elem #t)))
                  (values 0 #f))))
        %in-bytevector))
    ((sp start end)
      (in-bytevector sp start end 1))
    ((sp)
      (in-bytevector sp 0 (bytevector-length sp) 1))))



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

(define bytevector-hash
  (let ((c-bytevector-hash (foreign-procedure "c_bytevector_hash" (ptr) ptr)))
    (lambda (bvec)
      (assert* 'bytevector-hash (bytevector? bvec))
      (c-bytevector-hash bvec))))


(define (bytevector-uint-ref/little bv pos size)
  (if (fx>? size 8)
    (let* ((size1 (fx/ size 2))
           (size2 (fx- size size1))
           (lo    (bytevector-uint-ref/little bv pos             size1))
           (hi    (bytevector-uint-ref/little bv (fx+ pos size1) size2)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* size1 8))))
    (case size
      ((8) (bytevector-u64-ref bv pos (endianness little)))
      ((7) (bytevector-u56-ref bv pos (endianness little)))
      ((6) (bytevector-u48-ref bv pos (endianness little)))
      ((5) (bytevector-u40-ref bv pos (endianness little)))
      ((4) (bytevector-u32-ref bv pos (endianness little)))
      ((3) (bytevector-u24-ref bv pos (endianness little)))
      ((2) (bytevector-u16-ref bv pos (endianness little)))
      ((1) (bytevector-u8-ref  bv pos))
      (else 0)))) ; should not happen


(define (bytevector-uint-ref/big bv pos size)
  (if (fx>? size 8)
    (let* ((size1 (fx/ size 2))
           (size2 (fx- size size1))
           (hi    (bytevector-uint-ref/big bv pos             size1))
           (lo    (bytevector-uint-ref/big bv (fx+ pos size1) size2)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* size1 8))))
    (case size
      ((8) (bytevector-u64-ref bv pos (endianness big)))
      ((7) (bytevector-u56-ref bv pos (endianness big)))
      ((6) (bytevector-u48-ref bv pos (endianness big)))
      ((5) (bytevector-u40-ref bv pos (endianness big)))
      ((4) (bytevector-u32-ref bv pos (endianness big)))
      ((3) (bytevector-u24-ref bv pos (endianness big)))
      ((2) (bytevector-u16-ref bv pos (endianness big)))
      ((1) (bytevector-u8-ref  bv pos))
      (else 0)))) ; should not happen

;; optimized (bytevector-uint-ref)
(define (bytevector-uint-ref* bv pos eness size)
  (assert* 'bytevector-uint-ref* (fx>? size 0))
  (case eness
    ((little) (bytevector-uint-ref/little bv pos size))
    ((big)    (bytevector-uint-ref/big    bv pos size))
    (else     (syntax-violation 'bytevector-uint-ref* "invalid endianness" eness))))


(define (bytevector-uint-set/little! bv pos uint size)
  (if (fx>? size 8)
    (let* ((size1 (fx/ size 2))
           (size2 (fx- size size1))
           (lo    (bitwise-and uint (bitwise-not (bitwise-arithmetic-shift-left -1 (* size1 8)))))
           (hi    (bitwise-arithmetic-shift-right uint (* size1 8))))
      (bytevector-uint-set/little! bv pos lo size1)
      (bytevector-uint-set/little! bv (fx+ pos size1) hi size2))
    (case size
      ((8) (bytevector-u64-set! bv pos uint (endianness little)))
      ((7) (bytevector-u56-set! bv pos uint (endianness little)))
      ((6) (bytevector-u48-set! bv pos uint (endianness little)))
      ((5) (bytevector-u40-set! bv pos uint (endianness little)))
      ((4) (bytevector-u32-set! bv pos uint (endianness little)))
      ((3) (bytevector-u24-set! bv pos uint (endianness little)))
      ((2) (bytevector-u16-set! bv pos uint (endianness little)))
      ((1) (bytevector-u8-set!  bv pos uint))
      (else 0)))) ; should not happen


(define (bytevector-uint-set/big! bv pos uint size)
  (if (fx>? size 8)
    (let* ((size1 (fx/ size 2))
           (size2 (fx- size size1))
           (lo    (bitwise-and uint (bitwise-not (bitwise-arithmetic-shift-left -1 (* size1 8)))))
           (hi    (bitwise-arithmetic-shift-right uint (* size1 8))))
      (bytevector-uint-set/big! bv pos hi size1)
      (bytevector-uint-set/big! bv (fx+ pos size1) lo size2))
    (case size
      ((8) (bytevector-u64-set! bv pos uint (endianness little)))
      ((7) (bytevector-u56-set! bv pos uint (endianness little)))
      ((6) (bytevector-u48-set! bv pos uint (endianness little)))
      ((5) (bytevector-u40-set! bv pos uint (endianness little)))
      ((4) (bytevector-u32-set! bv pos uint (endianness little)))
      ((3) (bytevector-u24-set! bv pos uint (endianness little)))
      ((2) (bytevector-u16-set! bv pos uint (endianness little)))
      ((1) (bytevector-u8-set!  bv pos uint))
      (else 0)))) ; should not happen



;; optimized (bytevector-uint-ref)
(define (bytevector-uint-set*! bv pos uint eness size)
  (assert* 'bytevector-uint-set*! (integer? uint))
  (assert* 'bytevector-uint-set*! (exact? uint))
  (assert* 'bytevector-uint-set*! (>= uint 0))
  (assert* 'bytevector-uint-set*! (fx>? size 0))
  (case eness
    ((little) (bytevector-uint-set/little! bv pos uint size))
    ((big)    (bytevector-uint-set/big!    bv pos uint size))
    (else     (syntax-violation 'bytevector-uint-set*! "invalid endianness" eness))))


) ; close library
