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
    cell cell? cell->char cell->tty-palette cell->tty-colors

    make-cellvector list->cellvector string->cellvector cellvector->string
    cellvector-length cellvector-empty? cellvector-ref
    cellvector-set! cellvector-fill! cellvector-copy!

    cellvector-display cellvector-write)

  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)                     fx1+ fx1- fx/ meta-cond)
    (only (schemesh bootstrap)             assert* assert-not* fx<=?*)
    (only (schemesh containers bytevector) subbytevector-fill!)
    (schemesh containers palette)
    (only (schemesh containers utf8b)      integer->char*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax cell-bytes-log2 (identifier-syntax 2))
(define-syntax cell-bytes      (identifier-syntax 4))
(define-syntax cell-min        (identifier-syntax #x-20000000))
(define-syntax cell-max        (identifier-syntax #x1fffffff))

(define-syntax char-bits       (identifier-syntax 21))
(define-syntax char-max        (identifier-syntax #x1fffff))

(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))

(define-syntax cell<<
  (syntax-rules ()
    ((_ expr) (fx<< expr cell-bytes-log2))))

(define-syntax cell>>
  (syntax-rules ()
    ((_ expr) (fx>> expr cell-bytes-log2))))


(define (cell? cl)
  (and (fixnum? cl)  (fx<=? cell-min cl cell-max)))

(define cell
  (case-lambda
    ((ch)
      (char->integer ch))
    ((ch palette-or-tty-colors)
      (let ((palette (if (tty-palette? palette-or-tty-colors)
                       palette-or-tty-colors
                       (tty-colors->palette palette-or-tty-colors))))
        (fxior (fx<< palette char-bits) (char->integer ch))))))


(define (cell->char cl)
  (integer->char* (fxand cl char-max)))

(define (cell->tty-palette cl)
  (fx>> cl char-bits))

(define (cell->tty-colors cl)
  (tty-palette->colors (cell->tty-palette cl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create a cellvector containing n cells.
;; If cell is specified, then cellvector is filled with it.
;; Otherwise it is filled with (cell #\nul).
(define make-cellvector
  (case-lambda
    ((n)
      (make-bytevector (cell<< n) 0))
    ((n fill)
      (assert* 'make-cellvector (cell? fill))
      (let ((ret (make-cellvector n)))
        (unless (fxzero? fill)
          (cellvector-fill! ret 0 n fill))
        ret))))


(define (cellvector-length clv)
  (cell>> (bytevector-length clv)))

(define (cellvector-empty? clv)
  (fxzero? (bytevector-length clv)))


(define cellvector-ref
  (case-lambda
    ((clv idx)
      (bytevector-s32-native-ref clv (cell<< idx)))
    ((clv)
      (bytevector-s32-native-ref clv 0))))


(define (cellvector-set! clv idx cl)
  (bytevector-s32-native-set! clv (cell<< idx) cl))


(define cellvector-fill!
  (case-lambda
    ((clv start end cell)
      (assert* 'cellvector-fill! (fx<=?* 0 start end (cellvector-length clv)))
      (assert* 'cellvector-fill! (cell? cell))
      (let* ((bstart (cell<< start))
             (bend   (cell<< end))
             (u8     (bitwise-and cell #xff)))
        (if (= cell (* #x1010101 u8))
          (subbytevector-fill! clv bstart bend u8)
          (do ((bi bstart (fx+ bi cell-bytes)))
              ((fx>=? bi bend))
            (bytevector-s32-native-set! clv bi cell)))))
    ((clv cell)
      (cellvector-fill! clv 0 (cellvector-length clv) cell))))


;; copy n cells from a cellvector to another one
(define (cellvector-copy! src src-start dst dst-start n)
  (assert* 'cellvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (cellvector-length src)))
  (assert* 'cellvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (cellvector-length dst)))
  (bytevector-copy! src (cell<< src-start)
                    dst (cell<< dst-start) (cell<< n)))



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


;; display cellvector, including colors, to textual output port
(define cellvector-display
  (case-lambda
    ((clv start end port)
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cell    (cellvector-ref clv i))
                 (ch      (cell->char cell))
                 (palette (cell->tty-palette cell)))
            (unless (fx=? palette old-palette)
              (tty-palette-display palette port)
              (set! old-palette palette))
            (put-char port ch)))))
    ((clv start end)
      (cellvector-display clv start end (current-output-port)))
    ((clv port)
      (cellvector-display clv 0 (cellvector-length clv) port))
    ((clv)
      (cellvector-display clv 0 (cellvector-length clv) (current-output-port)))))



;; write cellvector, including colors, to textual output port
(define cellvector-write
  (case-lambda
    ((clv start end port)
      (put-char port #\")
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cell    (cellvector-ref clv i))
                 (ch      (cell->char cell))
                 (palette (cell->tty-palette cell)))
            (unless (fx=? palette old-palette)
              (tty-palette-display palette port)
              (set! old-palette palette))
            (if (and (char<=? #\space ch #\~) (not (char=? ch #\")) (not (char=? ch #\\)))
              (put-char port ch)
              (let ((n (char->integer ch)))
                (put-string port "\\x")
                (display (number->string n 16) port)
                (put-char port #\;))))))
      (put-char port #\"))
    ((clv start end)
      (cellvector-write clv start end (current-output-port)))
    ((clv port)
      (cellvector-write clv 0 (cellvector-length clv) port))
    ((clv)
      (cellvector-write clv 0 (cellvector-length clv) (current-output-port)))))


) ; close library
