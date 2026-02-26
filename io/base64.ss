;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

;;; high-level procedures for reading from and writing to ports.
;;;
;;; procedure names and effect are intentionally compatible with
;;; https://docs.racket-lang.org/reference/port-lib.html
;;;
(library (scheme2k io base64 (0 9 3))
  (export base64-string->bytevector bytevector->base64-string bytespan-insert-right/base64! put-base64)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)                 bytevector-truncate! bytevector-u24-ref bytevector-u24-set! endianness fx/ fx1+ fx1-)
    (only (scheme2k bootstrap)         assert* fx<=?*)
    (scheme2k containers bytespan))


(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))


;; convert 6 bits to a base64 byte
(define (u6->base64-byte n)
  (let ((u6 (fxand n #x3f)))
    (cond ((fx<? u6 26) (fx+ u6 65)) ; #\A
          ((fx<? u6 52) (fx+ u6 71)) ; #\a - 26
          ((fx<? u6 62) (fx- u6 4))  ; #\0 - 52
          ((fx=? u6 62) 43)          ; #\+
          (else         47))))       ; #\/


;; convert final 1 byte to base64 and append it to bytespan bsp
(define (base64-append/u8 bsp u8)
  (let ((u12 (fx<< u8 4)))
    (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u12 6)))
    (bytespan-insert-right/u8! bsp (u6->base64-byte u12))
    (bytespan-insert-right/u8! bsp 61)   ; #\=
    (bytespan-insert-right/u8! bsp 61))) ; #\=


;; convert final 2 bytes to base64 and append them to bytespan  bsp
(define (base64-append/u16 bsp u16)
  (let ((u18 (fx<< u16 2)))
    (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u18 12)))
    (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u18 6)))
    (bytespan-insert-right/u8! bsp (u6->base64-byte u18))
    (bytespan-insert-right/u8! bsp 61))) ; #\=


;; convert 3 bytes to base64 and append them to bytespan bsp
(define (base64-append/u24 bsp u24)
  (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u24 18)))
  (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u24 12)))
  (bytespan-insert-right/u8! bsp (u6->base64-byte (fx>> u24 6)))
  (bytespan-insert-right/u8! bsp (u6->base64-byte u24)))


(define (put-bytespan out bsp)
  (put-bytevector out (bytespan-peek-data bsp) (bytespan-peek-beg bsp) (bytespan-peek-end bsp)))


;; convert a bytevector to base64 and write it to binary output port
(define put-base64
  (case-lambda
    ((out bv start end wbuf)
      (bytespan-clear! wbuf)
      (let ((i (do ((i start (fx+ i 3))
                    (end-3 (fx- end 3)))
                   ((fx>? i end-3) i)
                 (base64-append/u24 wbuf (bytevector-u24-ref bv i (endianness big)))
                 (when (fx>=? (bytespan-length wbuf) 4096)
                   (put-bytespan out wbuf)
                   (bytespan-clear! wbuf)))))
        (case (fx- end i)
          ((2)
            (base64-append/u16 wbuf (bytevector-u16-ref bv i (endianness big))))
          ((1)
            (base64-append/u8 wbuf (bytevector-u8-ref bv i))))
        (put-bytespan out wbuf)
        (bytespan-clear! wbuf)))
    ((out bv start end)
      (put-base64 out bv start end (bytespan)))
    ((out bv)
      (put-base64 out bv 0 (bytevector-length bv) (bytespan)))))


;; convert a bytevector to base64 and append it to bytespan
(define bytespan-insert-right/base64!
  (case-lambda
    ((bsp bv start end)
      (assert* 'bytespan-insert-right/base64! (fx<=?* 0 start end (bytevector-length bv)))
      (let ((i (do ((i start (fx+ i 3))
                    (end-3 (fx- end 3)))
                   ((fx>? i end-3) i)
                 (base64-append/u24 bsp (bytevector-u24-ref bv i (endianness big))))))
        (case (fx- end i)
          ((2)
            (base64-append/u16 bsp (bytevector-u16-ref bv i (endianness big))))
          ((1)
            (base64-append/u8 bsp (bytevector-u8-ref bv i))))))
    ((bsp bv)
      (bytespan-insert-right/base64! bsp bv 0 (bytevector-length bv)))))


;; convert 1 byte to 4 base64 chars and write them to str
(define (string-set/base64-u8! str i u8)
  (let ((u12 (fx<< u8 4)))
    (string-set! str      i  (integer->char (u6->base64-byte (fx>> u12 6))))
    (string-set! str (fx1+ i) (integer->char (u6->base64-byte u12)))
    (string-set! str (fx+ 2 i) #\=)
    (string-set! str (fx+ 3 i) #\=)))


;; convert 2 bytes to 4 base64 chars and write them to str
(define (string-set/base64-u16! str i u16)
  (let ((u18 (fx<< u16 2)))
    (string-set! str      i  (integer->char (u6->base64-byte (fx>> u18 12))))
    (string-set! str (fx1+ i) (integer->char (u6->base64-byte (fx>> u18 6))))
    (string-set! str (fx+ 2 i) (integer->char (u6->base64-byte u18)))
    (string-set! str (fx+ 3 i) #\=)))


;; convert 3 bytes to 4 base64 chars and write them to str
(define (string-set/base64-u24! str i u24)
  (string-set! str      i  (integer->char (u6->base64-byte (fx>> u24 18))))
  (string-set! str (fx1+ i) (integer->char (u6->base64-byte (fx>> u24 12))))
  (string-set! str (fx+ 2 i) (integer->char (u6->base64-byte (fx>> u24 6))))
  (string-set! str (fx+ 3 i) (integer->char (u6->base64-byte u24))))


;; convert a bytevector to a string containing base64 encoded data
(define bytevector->base64-string
  (case-lambda
    ((bv start end)
      (assert* 'bytevector->base64-string (fx<=?* 0 start end (bytevector-length bv)))
      (let* ((bn (fx- end start))
             (sn (fx* (fx/ (fx+ bn 2) 3) 4))
             (str (make-string sn #\=)))
        (let %loop ((bi start) (si 0) (bend-2 (fx- end 2)))
          (cond
            ((fx<? bi bend-2)
              (string-set/base64-u24! str si (bytevector-u24-ref bv bi (endianness big)))
              (%loop (fx+ bi 3) (fx+ si 4) bend-2))
            ((fx=? bi bend-2)
              (string-set/base64-u16! str si (bytevector-u16-ref bv bi (endianness big))))
            ((fx<? bi end)
              (string-set/base64-u8!  str si (bytevector-u8-ref bv bi)))))
        str))
    ((bv)
      (bytevector->base64-string bv 0 (bytevector-length bv)))))


;; convert one base64 char to six bits
(define (base64-char->u6 ch)
  (and (char? ch)
       (cond
         ((char<=? #\A ch #\Z) (fx- (char->integer ch) 65)) ; #\A
         ((char<=? #\a ch #\z) (fx- (char->integer ch) 71)) ; #\a - 26
         ((char<=? #\0 ch #\9) (fx+ (char->integer ch) 4)) ; #\0 - 52
         ((char=? ch #\+)      62)
         ((char=? ch #\/)      63)
         (else                 #f))))


;; convert 1..4 base64 chars to 0..3 bytes and append them to bytevector bv.
;; return updated position
(define (base64-quartet->bytevector! str si send bv bi)
  (let ((n1 (base64-char->u6 (string-ref str si)))
        (n2 (base64-char->u6 (let ((pos (fx1+ si))) (and (fx<? pos send) (string-ref str pos)))))
        (n3 (base64-char->u6 (let ((pos (fx+ 2 si))) (and (fx<? pos send) (string-ref str pos)))))
        (n4 (base64-char->u6 (let ((pos (fx+ 3 si))) (and (fx<? pos send) (string-ref str pos))))))
    (cond
      ((not (and n1 n2))
        bi)
      ((and n3 n4)
        (let ((u24 (fxior (fx<< n1 18) (fx<< n2 12) (fx<< n3 6) n4)))
          (bytevector-u24-set! bv bi u24 (endianness big))
          (fx+ bi 3)))
      (n3
        (let ((u16 (fxior (fx<< n1 10) (fx<< n2 4) (fx>> n3 2))))
          (bytevector-u16-set! bv bi u16 (endianness big))
          (fx+ bi 2)))
      (else
        (let ((u8 (fxior (fx<< n1 2) (fx>> n2 4))))
          (bytevector-u8-set! bv bi u8)
          (fx1+ bi))))))


;; convert a string containing base64 encoded data to bytevector
(define base64-string->bytevector
  (case-lambda
    ((str start end)
      (assert* 'base64-string->bytevector (fx<=?* 0 start end (string-length str)))
      (let* ((sn (fx- end start))
             (bn (fx* (fx/ (fx+ 3 sn) 4) 3))
             (bv (make-bytevector bn))
             (bi (let %loop ((si start) (bi 0))
                   (if (fx>=? si end)
                     bi
                     (%loop (fx+ si 4) (base64-quartet->bytevector! str si end bv bi))))))
        (bytevector-truncate! bv bi)
        bv))
    ((str)
      (base64-string->bytevector str 0 (string-length str)))))

) ; close library
