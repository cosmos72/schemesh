;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;      define Scheme type "cellvector", a vector of cells       ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers cellvector (0 8 3))
  (export
    cell cell? cell->char cell->palette-index palette-index?

    make-cellvector list->cellvector string->cellvector cellvector->string
    cellvector-length cellvector-empty? cellvector-ref
    cellvector-set! cellvector-fill! cellvector-copy!)

  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)                     fx1+ fx1- fx/ meta-cond)
    (only (schemesh bootstrap)             assert* assert-not* fx<=?*)
    (only (schemesh containers bytevector) subbytevector-fill!)
    (only (schemesh containers utf8b)      integer->char*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax cell-bytes (identifier-syntax 4))
(define-syntax cell-bits  (identifier-syntax 32))
(define-syntax cell-max   (identifier-syntax #xffffffff))

(define-syntax char-bits (identifier-syntax 21))
(define-syntax char-max  (identifier-syntax #x1fffff))

(define-syntax palette-bits (identifier-syntax 11))
(define-syntax palette-max  (identifier-syntax #x7ff))

(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))

(define-syntax <<   (identifier-syntax bitwise-arithmetic-shift-left))
(define-syntax >>   (identifier-syntax bitwise-arithmetic-shift-right))


(define (palette-index? palette-index)
  (meta-cond
    ((fixnum? palette-max) (fx<=? 0 palette-index palette-max))
    (else                  (<= 0 palette-index palette-max))))


(define (cell? cl)
  (meta-cond ((fixnum? cell-max)  (fx<=? 0 cl char-max))
             (else                  (<=  0 cl char-max))))

(define cell
  (case-lambda
    ((ch)
      (char->integer ch))
    ((ch palette-index)
      (assert* 'cell (palette-index? palette-index))
      (meta-cond
        ((fixnum? cell-max)  (fxior (fx<< palette-index char-bits) (char->integer ch)))
        (else            (bitwise-ior (<< palette-index char-bits) (char->integer ch)))))))

(define (cell->char cl)
  (integer->char*
     (meta-cond ((fixnum? cell-max) (fxand cl char-max))
                 (else        (bitwise-and cl char-max)))))

(define (cell->palette-index cl)
  (meta-cond ((fixnum? cell-max)  (fx>> cl char-bits))
             (else                  (>> cl char-bits))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax *cell
  (syntax-rules ()
    ((_ expr) (fx* expr cell-bytes))))

;; create a cellvector containing n cells.
;; If cell is specified, then cellvector is filled with it.
;; Otherwise it is filled with (cell #\nul).
(define make-cellvector
  (case-lambda
    ((n)
      (make-bytevector (*cell n) 0))
    ((n fill)
      (let ((ret (make-cellvector n)))
        (unless (zero? fill)
          (cellvector-fill! ret 0 n fill))
        ret))))


(define (cellvector-length clv)
  (fx/ (bytevector-length clv) cell-bytes))

(define (cellvector-empty? clv)
  (fxzero? (bytevector-length clv)))


(define cellvector-ref
  (case-lambda
    ((clv idx)
      (bytevector-u32-native-ref clv (*cell idx)))
    ((clv)
      (bytevector-u32-native-ref clv 0))))


(define (cellvector-set! clv idx cl)
  (bytevector-u32-native-set! clv (*cell idx) cl))


(define cellvector-fill!
  (case-lambda
    ((clv start end cell)
      (assert* 'cellvector-fill! (fx<=?* 0 start end (cellvector-length clv)))
      (assert* 'cellvector-fill! (cell? cell))
      (let* ((bstart (*cell start))
             (bend   (*cell end)))
        (cond
          ((zero? cell)
            (subbytevector-fill! clv bstart bend 0))
          ((= cell cell-max)
            (subbytevector-fill! clv bstart bend #xff))
          (else
            (do ((i bstart (fx+ i cell-bytes)))
                ((fx>=? i bend))
              (bytevector-u32-native-set! clv i cell))))))
    ((clv cell)
      (cellvector-fill! clv 0 (cellvector-length clv) cell))))


;; copy n cells from a cellvector to another one
(define (cellvector-copy! src src-start dst dst-start n)
  (assert* 'cellvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (cellvector-length src)))
  (assert* 'cellvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (cellvector-length dst)))
  (bytevector-copy! src (*cell src-start)
                    dst (*cell dst-start) (*cell n)))



;; convert cell list to cellvector
(define (list->cellvector l)
  (let* ((n   (length l))
         (dst (make-cellvector n)))
    (do ((i 0 (fx1+ i))
         (tail l (cdr tail)))
        ((null? tail) dst)
      (cellvector-set! dst i (car tail)))))


;; convert a portion of string to cellvector
(define string->cellvector
  (case-lambda
    ((str start end)
      (assert* 'string->cellvector (fx<=?* 0 start end (string-length str)))
      (let* ((n   (fx- end start))
             (dst (make-cellvector n)))
        (do ((si start (fx1+ si))
             (di   0   (fx1+ di)))
            ((fx>=? si n) dst)
          (cellvector-set! dst di (cell (string-ref str si))))))
    ((str)
      (string->cellvector str 0 (string-length str)))))


;; convert a portion of cellvector to string
(define cellvector->string
  (case-lambda
    ((clv start end)
      (assert* 'cellvector->string (fx<=?* 0 start end (cellvector-length clv)))
      (let* ((n   (fx- end start))
             (dst (make-string n)))
        (do ((si start (fx1+ si))
             (di   0   (fx1+ di)))
            ((fx>=? di n) dst)
          (string-set! dst di (cell->char (cellvector-ref clv si))))))
    ((clv)
      (cellvector->string clv 0 (cellvector-length clv)))))


) ; close library
