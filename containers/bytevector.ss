;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh containers bytevector (0 9 0))
  (export
    in-bytevector list->bytevector subbytevector

    bytevector-compare subbytevector-fill! bytevector-hash bytevector-index
    bytevector<=? bytevector<? bytevector>=? bytevector>? bytevector-iterate

    bytevector-sint-ref* bytevector-sint-set*!
    bytevector-uint-ref* bytevector-uint-set*!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         1+ bytevector
                               bytevector-s24-ref  bytevector-s40-ref  bytevector-s48-ref  bytevector-s56-ref
                               bytevector-s24-set! bytevector-s40-set! bytevector-s48-set! bytevector-s56-set!
                               bytevector-u24-ref  bytevector-u40-ref  bytevector-u48-ref  bytevector-u56-ref
                               bytevector-u24-set! bytevector-u40-set! bytevector-u48-set! bytevector-u56-set!
                               foreign-procedure fx1+ fx1- fx/ logbit? procedure-arity-mask void)
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

(define c-subbytevector-fill! (foreign-procedure "c_subbytevector_fill" (ptr int int int) void))

(define (subbytevector-fill! bvec start end val)
  (assert* 'subbytevector-fill! (fx<=?* 0 start end (bytevector-length bvec)))
  (assert* 'subbytevector-fill! (fx<=? -128 val 255))
  (let ((val (fxand val 255))
        (n   (fx- end start)))
    (if (fx<? n 3)
      (unless (fxzero? n)
        (bytevector-u8-set! bvec start val)
        (when (fx>? n 1)
          (bytevector-u8-set! bvec (fx1+ start) val)))
      (c-subbytevector-fill! bvec start end val))))


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
    (let* ((sizehi (fx/ size 2))
           (sizelo (fx- size sizehi))
           (lo     (bytevector-uint-ref/little bv pos              sizelo))
           (hi     (bytevector-uint-ref/little bv (fx+ pos sizelo) sizehi)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* sizelo 8))))
    (case size
      ((8) (bytevector-u64-ref bv pos (endianness little)))
      ((7) (bytevector-u56-ref bv pos (endianness little)))
      ((6) (bytevector-u48-ref bv pos (endianness little)))
      ((5) (bytevector-u40-ref bv pos (endianness little)))
      ((4) (bytevector-u32-ref bv pos (endianness little)))
      ((3) (bytevector-u24-ref bv pos (endianness little)))
      ((2) (bytevector-u16-ref bv pos (endianness little)))
      ((1) (bytevector-u8-ref  bv pos))
      (else 0))))


(define (bytevector-uint-ref/big bv pos size)
  (if (fx>? size 8)
    (let* ((sizehi (fx/ size 2))
           (sizelo (fx- size sizehi))
           (hi     (bytevector-uint-ref/big bv pos              sizehi))
           (lo     (bytevector-uint-ref/big bv (fx+ pos sizehi) sizelo)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* sizelo 8))))
    (case size
      ((8) (bytevector-u64-ref bv pos (endianness big)))
      ((7) (bytevector-u56-ref bv pos (endianness big)))
      ((6) (bytevector-u48-ref bv pos (endianness big)))
      ((5) (bytevector-u40-ref bv pos (endianness big)))
      ((4) (bytevector-u32-ref bv pos (endianness big)))
      ((3) (bytevector-u24-ref bv pos (endianness big)))
      ((2) (bytevector-u16-ref bv pos (endianness big)))
      ((1) (bytevector-u8-ref  bv pos))
      (else 0))))


;; optimized (bytevector-uint-ref)
(define (bytevector-uint-ref* bv pos eness size)
  (assert* 'bytevector-uint-ref* (fx>? size 0))
  (assert* 'bytevector-uint-ref* (fx<=?* 0 pos (fx+ pos size) (bytevector-length bv)))
  (case eness
    ((little) (bytevector-uint-ref/little bv pos size))
    ((big)    (bytevector-uint-ref/big    bv pos size))
    (else     (syntax-violation 'bytevector-uint-ref* "invalid endianness" eness))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bytevector-uint-set/little! bv pos uint size)
  (if (fx>? size 8)
    (if (eqv? uint 0)
      (c-subbytevector-fill! bv pos (fx+ pos size) 0)
      (let* ((sizehi   (fx/ size 2))
             (sizelo   (fx- size sizehi))
             (sizelo*8 (* sizelo 8))
             (lo       (bitwise-and uint (bitwise-not (bitwise-arithmetic-shift-left -1 sizelo*8))))
             (hi       (bitwise-arithmetic-shift-right uint sizelo*8)))
        (bytevector-uint-set/little! bv pos              lo sizelo)
        (bytevector-uint-set/little! bv (fx+ pos sizelo) hi sizehi)))
    (case size
      ((8) (bytevector-u64-set! bv pos uint (endianness little)))
      ((7) (bytevector-u56-set! bv pos uint (endianness little)))
      ((6) (bytevector-u48-set! bv pos uint (endianness little)))
      ((5) (bytevector-u40-set! bv pos uint (endianness little)))
      ((4) (bytevector-u32-set! bv pos uint (endianness little)))
      ((3) (bytevector-u24-set! bv pos uint (endianness little)))
      ((2) (bytevector-u16-set! bv pos uint (endianness little)))
      ((1) (bytevector-u8-set!  bv pos uint))
      (else (void)))))


(define (bytevector-uint-set/big! bv pos uint size)
  (if (fx>? size 8)
    (if (eqv? uint 0)
      (c-subbytevector-fill! bv pos (fx+ pos size) 0)
      (let* ((sizehi (fx/ size 2))
             (sizelo (fx- size sizehi))
             (sizelo*8 (* sizelo 8))
             (lo    (bitwise-and uint (bitwise-not (bitwise-arithmetic-shift-left -1 sizelo*8))))
             (hi    (bitwise-arithmetic-shift-right uint sizelo*8)))
        (bytevector-uint-set/big! bv pos              hi sizehi)
        (bytevector-uint-set/big! bv (fx+ pos sizehi) lo sizelo)))
    (case size
      ((8) (bytevector-u64-set! bv pos uint (endianness big)))
      ((7) (bytevector-u56-set! bv pos uint (endianness big)))
      ((6) (bytevector-u48-set! bv pos uint (endianness big)))
      ((5) (bytevector-u40-set! bv pos uint (endianness big)))
      ((4) (bytevector-u32-set! bv pos uint (endianness big)))
      ((3) (bytevector-u24-set! bv pos uint (endianness big)))
      ((2) (bytevector-u16-set! bv pos uint (endianness big)))
      ((1) (bytevector-u8-set!  bv pos uint))
      (else (void)))))



;; optimized (bytevector-uint-set!)
(define (bytevector-uint-set*! bv pos uint eness size)
  (assert* 'bytevector-uint-set*! (integer? uint))
  (assert* 'bytevector-uint-set*! (exact? uint))
  (assert* 'bytevector-uint-set*! (>= uint 0))
  (assert* 'bytevector-uint-set*! (fx>? size 0))
  (assert* 'bytevector-uint-set*! (fx<=?* 0 pos (fx+ pos size) (bytevector-length bv)))
  (case eness
    ((little) (bytevector-uint-set/little! bv pos uint size))
    ((big)    (bytevector-uint-set/big!    bv pos uint size))
    (else     (syntax-violation 'bytevector-uint-set*! "invalid endianness" eness))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (bytevector-sint-ref/little bv pos size)
  (if (fx>? size 8)
    (let* ((sizehi (fx/ size 2))
           (sizelo (fx- size sizehi))
           (lo    (bytevector-uint-ref/little bv pos              sizelo))
           (hi    (bytevector-sint-ref/little bv (fx+ pos sizelo) sizehi)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* sizelo 8))))
    (case size
      ((8) (bytevector-s64-ref bv pos (endianness little)))
      ((7) (bytevector-s56-ref bv pos (endianness little)))
      ((6) (bytevector-s48-ref bv pos (endianness little)))
      ((5) (bytevector-s40-ref bv pos (endianness little)))
      ((4) (bytevector-s32-ref bv pos (endianness little)))
      ((3) (bytevector-s24-ref bv pos (endianness little)))
      ((2) (bytevector-s16-ref bv pos (endianness little)))
      ((1) (bytevector-s8-ref  bv pos))
      (else 0))))


(define (bytevector-sint-ref/big bv pos size)
  (if (fx>? size 8)
    (let* ((sizehi (fx/ size 2))
           (sizelo (fx- size sizehi))
           (hi     (bytevector-sint-ref/big bv pos              sizehi))
           (lo     (bytevector-uint-ref/big bv (fx+ pos sizehi) sizelo)))
      (bitwise-ior lo (bitwise-arithmetic-shift-left hi (* sizelo 8))))
    (case size
      ((8) (bytevector-s64-ref bv pos (endianness big)))
      ((7) (bytevector-s56-ref bv pos (endianness big)))
      ((6) (bytevector-s48-ref bv pos (endianness big)))
      ((5) (bytevector-s40-ref bv pos (endianness big)))
      ((4) (bytevector-s32-ref bv pos (endianness big)))
      ((3) (bytevector-s24-ref bv pos (endianness big)))
      ((2) (bytevector-s16-ref bv pos (endianness big)))
      ((1) (bytevector-s8-ref  bv pos))
      (else 0))))



;; return n > 0 if first n bytes are #xff in bytevector range [start, start+size)
;; return n < 0 if first n bytes are zero in bytevector range [start, start+size)
;; otherwise return 0
(define (%examine-left bv start size)
  (case (bytevector-u8-ref bv start)
    ((0)
      (let %count-zeroes ((n 1))
        (if (or (fx=? n size) (not (fxzero? (bytevector-u8-ref bv (fx+ start n)))))
          (fx- n)
          (%count-zeroes (fx1+ n)))))
    ((#xff)
      (let %count-ff ((n 1))
        (if (or (fx=? n size) (not (fx=? #xff (bytevector-u8-ref bv (fx+ start n)))))
          n
          (%count-ff (fx1+ n)))))
    (else
      0)))


;; return n > 0 if last n bytes are #xff in bytevector range [start, start+size)
;; return n < 0 if last n bytes are zero in bytevector range [start, start+size)
;; otherwise return 0
(define (%examine-right bv start size)
  (let ((last (fx1- (fx+ start size))))
    (case (bytevector-u8-ref bv last)
      ((0)
        (let %count-zeroes ((n 1))
          (if (or (fx=? n size) (not (fxzero? (bytevector-u8-ref bv (fx- last n)))))
            (fx- n)
            (%count-zeroes (fx1+ n)))))
      ((#xff)
        (let %count-ff ((n 1))
          (if (or (fx=? n size) (not (fx=? #xff (bytevector-u8-ref bv (fx- last n)))))
            n
            (%count-ff (fx1+ n)))))
      (else
        0))))



;; optimized (bytevector-sint-ref)
(define (bytevector-sint-ref* bv pos eness size)
  (assert* 'bytevector-sint-ref* (fx>? size 0))
  (assert* 'bytevector-sint-ref* (fx<=?* 0 pos (fx+ pos size) (bytevector-length bv)))
  (case eness
    ((little)
      (let ((skip-n (%examine-right bv pos size)))
        (cond
          ((fx=? skip-n size)
            -1)
          ((fx>? skip-n 0)
            (bytevector-sint-ref/little bv pos (fx1+ (fx- size skip-n))))
          ((fxzero? skip-n)
            (bytevector-sint-ref/little bv pos size))
          (else
            (bytevector-uint-ref/little bv pos (fx+ size skip-n))))))
    ((big)
      (let ((skip-n (%examine-left bv pos size)))
        (cond
          ((fx=? skip-n size)
            -1)
          ((fx>? skip-n 0)
            (let ((delta (fx1- skip-n)))
              (bytevector-sint-ref/big bv (fx+ pos delta) (fx- size delta))))
          ((fxzero? skip-n)
            (bytevector-sint-ref/big bv pos size))
          (else
            (bytevector-uint-ref/big bv (fx- pos skip-n) (fx+ size skip-n))))))
    (else
      (syntax-violation 'bytevector-sint-ref* "invalid endianness" eness))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bytevector-sint-set/little! bv pos sint size)
  (if (fx>? size 8)
    (case sint
      ((0)
        (c-subbytevector-fill! bv pos (fx+ pos size) 0))
      ((-1)
        (c-subbytevector-fill! bv pos (fx+ pos size) #xff))
      (else
        (let* ((sizehi   (fx/ size 2))
               (sizelo   (fx- size sizehi))
               (sizelo*8 (* sizelo 8))
               (lo       (bitwise-and sint (bitwise-not (bitwise-arithmetic-shift-left -1 sizelo*8))))
               (hi       (bitwise-arithmetic-shift-right sint sizelo*8)))
          (bytevector-uint-set/little! bv pos              lo sizelo)
          (bytevector-sint-set/little! bv (fx+ pos sizelo) hi sizehi))))
    (case size
      ((8) (bytevector-s64-set! bv pos sint (endianness little)))
      ((7) (bytevector-s56-set! bv pos sint (endianness little)))
      ((6) (bytevector-s48-set! bv pos sint (endianness little)))
      ((5) (bytevector-s40-set! bv pos sint (endianness little)))
      ((4) (bytevector-s32-set! bv pos sint (endianness little)))
      ((3) (bytevector-s24-set! bv pos sint (endianness little)))
      ((2) (bytevector-s16-set! bv pos sint (endianness little)))
      ((1) (bytevector-s8-set!  bv pos sint))
      (else (void)))))


(define (bytevector-sint-set/big! bv pos sint size)
  (if (fx>? size 8)
    (case sint
      ((0)
        (c-subbytevector-fill! bv pos (fx+ pos size) 0))
      ((-1)
        (c-subbytevector-fill! bv pos (fx+ pos size) #xff))
      (else
        (let* ((sizehi   (fx/ size 2))
               (sizelo   (fx- size sizehi))
               (sizelo*8 (* sizelo 8))
               (lo       (bitwise-and sint (bitwise-not (bitwise-arithmetic-shift-left -1 sizelo*8))))
               (hi       (bitwise-arithmetic-shift-right sint sizelo*8)))
          (bytevector-sint-set/big! bv pos              hi sizehi)
          (bytevector-uint-set/big! bv (fx+ pos sizehi) lo sizelo))))
    (case size
      ((8) (bytevector-s64-set! bv pos sint (endianness big)))
      ((7) (bytevector-s56-set! bv pos sint (endianness big)))
      ((6) (bytevector-s48-set! bv pos sint (endianness big)))
      ((5) (bytevector-s40-set! bv pos sint (endianness big)))
      ((4) (bytevector-s32-set! bv pos sint (endianness big)))
      ((3) (bytevector-s24-set! bv pos sint (endianness big)))
      ((2) (bytevector-s16-set! bv pos sint (endianness big)))
      ((1) (bytevector-s8-set!  bv pos sint))
      (else (void)))))


;; optimized (bytevector-sint-set!)
(define (bytevector-sint-set*! bv pos sint eness size)
  (assert* 'bytevector-sint-set*! (integer? sint))
  (assert* 'bytevector-sint-set*! (exact? sint))
  (assert* 'bytevector-sint-set*! (fx>? size 0))
  (assert* 'bytevector-sint-set*! (fx<=?* 0 pos (fx+ pos size) (bytevector-length bv)))
  (case eness
    ((little) (bytevector-sint-set/little! bv pos sint size))
    ((big)    (bytevector-sint-set/big!    bv pos sint size))
    (else     (syntax-violation 'bytevector-sint-set*! "invalid endianness" eness))))

) ; close library
