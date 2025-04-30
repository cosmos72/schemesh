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
    make-cellvector list->cellvector string->cellvector
    cellvector-length cellvector-empty? cellvector-ref
    cellvector-set! cellvector-update/char! cellvector-update/colors! cellvector-update/palette!
    cellvector-fill! cellvector-copy! cellvector-copy/string!

    cellvector-display cellvector-write)

  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)                     fx1+ fx1- fx/ meta-cond)
    (only (schemesh bootstrap)             assert* assert-not* fx<=?*)
    (only (schemesh containers bytevector) subbytevector-fill!)
    (schemesh containers cell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-syntax cell-bytes-log2 (identifier-syntax 2))
(define-syntax cell-bytes      (identifier-syntax 4))

(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))

(define-syntax cell<<
  (syntax-rules ()
    ((_ expr) (fx<< expr cell-bytes-log2))))

(define-syntax cell>>
  (syntax-rules ()
    ((_ expr) (fx>> expr cell-bytes-log2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create a cellvector containing n cells.
;; If cell is specified, then cellvector is filled with it.
;; Otherwise it is filled with (cell #\nul).
(define make-cellvector
  (case-lambda
    ((n)
      (make-bytevector (cell<< n) 0))
    ((n fill)
      (unless (cell? fill)
        (assert* 'make-cellvector (char? fill)))
      (let ((ret (make-cellvector n)))
        (unless (memv fill '(0 #\nul))
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


;; c must be a character or cell
(define (cellvector-set! clv idx c)
  (bytevector-s32-native-set! clv (cell<< idx) (if (char? c) (cell c) c)))

;; replace character at index idx, keeping the current colors
(define (cellvector-update/char! clv idx ch)
  (let ((palette (cell->palette (cellvector-ref clv idx))))
    (cellvector-set! clv idx (cell ch palette))))

;; replace the colors at index idx, keeping the current character
(define (cellvector-update/colors! clv idx cols)
  (let ((ch (cell->char (cellvector-ref clv idx))))
    (cellvector-set! clv idx (cell ch cols))))

;; replace the palette at index idx, keeping the current character
(define (cellvector-update/palette! clv idx palette)
  (let ((ch (cell->char (cellvector-ref clv idx))))
    (cellvector-set! clv idx (cell ch palette))))


;; c must be a character or cell
(define cellvector-fill!
  (case-lambda
    ((clv start end c)
      (assert* 'cellvector-fill! (fx<=?* 0 start end (cellvector-length clv)))
      (unless (char? c)
        (assert* 'cellvector-fill! (cell? c)))
      (let* ((bstart (cell<< start))
             (bend   (cell<< end))
             (cl     (if (char? c) (cell c) c))
             (u8     (bitwise-and cl #xff)))
        (if (= cl (* #x1010101 u8))
          (subbytevector-fill! clv bstart bend u8)
          (do ((bi bstart (fx+ bi cell-bytes)))
              ((fx>=? bi bend))
            (bytevector-s32-native-set! clv bi cl)))))
    ((clv c)
      (cellvector-fill! clv 0 (cellvector-length clv) c))))


;; copy n cells from a cellvector to another one
(define (cellvector-copy! src src-start dst dst-start n)
  (assert* 'cellvector-copy! (fx<=?* 0 src-start (fx+ src-start n) (cellvector-length src)))
  (assert* 'cellvector-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (cellvector-length dst)))
  (bytevector-copy! src (cell<< src-start)
                    dst (cell<< dst-start) (cell<< n)))


;; copy n characters from a string to a cellvector
(define (cellvector-copy/string! str-src src-start clv-dst dst-start n)
  (assert* 'cellvector-copy/string! (fx<=?* 0 src-start (fx+ src-start n) (string-length str-src)))
  (assert* 'cellvector-copy/string! (fx<=?* 0 dst-start (fx+ dst-start n) (cellvector-length clv-dst)))
  (do ((si src-start (fx1+ si))
       (di dst-start (fx1+ di))
       (dend (fx+ dst-start n)))
      ((fx>=? di dend))
    (cellvector-set! clv-dst di (cell (string-ref str-src si)))))



;; convert list of cells or characters to cellvector
(define (list->cellvector c-list)
  (let* ((n   (length c-list))
         (dst (make-cellvector n)))
    (do ((i 0 (fx1+ i))
         (tail c-list (cdr tail)))
        ((null? tail) dst)
      (let* ((c  (car tail))
             (cl (if (char? c) (cell c) c)))
        (cellvector-set! dst i cl)))))


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


;; display a range of cellvector, including colors, to textual output port
(define cellvector-display
  (case-lambda
    ((clv start end port)
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cell    (cellvector-ref clv i))
                 (ch      (cell->char cell))
                 (palette (cell->palette cell)))
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



;; write a range of cellvector, including colors, to textual output port
(define cellvector-write
  (case-lambda
    ((clv start end port)
      (put-char port #\")
      (let ((old-palette 0))
        (do ((i start (fx1+ i)))
            ((fx>=? i end))
          (let* ((cell    (cellvector-ref clv i))
                 (palette (cell->palette cell)))
            (cell-write cell old-palette port)
            (unless (fx=? palette old-palette)
              (set! old-palette palette)))))
      (put-char port #\"))
    ((clv start end)
      (cellvector-write clv start end (current-output-port)))
    ((clv port)
      (cellvector-write clv 0 (cellvector-length clv) port))
    ((clv)
      (cellvector-write clv 0 (cellvector-length clv) (current-output-port)))))


) ; close library
