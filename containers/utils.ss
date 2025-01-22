;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; define Scheme utility functions on containers:
;;; a. converting chars from/to UTF8 and reading/writing them into "bytevector" and "bytespan"
;;; b. converting integers to decimal and writing them into "bytevector" and "bytespan"


(library (schemesh containers utils (0 7 1))
  (export
    bytevector-ref/utf8b bytevector-set/utf8b! char->utf8b-length
    bytespan-ref/char bytespan-set/char! bytespan-insert-front/char! bytespan-insert-back/char!
    bytespan-insert-back/cspan! bytespan-insert-back/cbuffer!
    bytespan-display-back/fixnum! bytespan-insert-back/string!
    charspan->utf8b charspan->utf8b/0)
  (import
    (rename (rnrs)
      (fxarithmetic-shift-left  fxshl)
      (fxarithmetic-shift-right fxshr))
    (only (chezscheme) fx1+ fx1-)
    (only (schemesh bootstrap) assert* debugf)
    (schemesh containers misc)
    (schemesh containers utf8b)
    (schemesh containers bytespan)
    (schemesh containers charspan)
    (only (schemesh containers chargbuffer) chargbuffer-iterate chargbuffer-length))


;; encode a single raw byte in the range #x80 ... #xff that is NOT part of a valid UTF-8 sequence
;; as a char in the surrogate range range U+dc80 ... U+dcff according to UTF-8b specifications
(define (utf8b-singlet->char b0)
  (values (integer->char* (fxior #xdc80 b0)) 1))


;; interpret two bytes as UTF-8b sequence and return corresponding char.
;; b0 is assumed to be in the range #xc0 <= b0 < #xe0
(define (utf8b-pair->char b0 b1)
  (if (fx=? #x80 (fxand #xc0 b1)) ; is b1 valid continuation byte ?
    (let ((n (fxior
               (fxshl (fxand #x1f b0) 6)
               (fxshl (fxand #x3f b1) 0))))
      (if (fx<=? #x80 n #x7ff)
        (values (integer->char n) 2)
        ; overlong UTF-8 sequence, encode a single raw byte as UTF-8b
        (utf8b-singlet->char b0)))
    ; invalid continuation byte b1, encode a single raw byte as UTF-8b
    (utf8b-singlet->char b0)))


;; interpret three bytes as UTF-8b sequence and return corresponding char.
;; b0 is assumed to be in the range #xe0 <= b0 < #xf0
(define (utf8b-triplet->char b0 b1 b2)
  (if (fx=? #x80 (fxand #xc0 (fxior b1 b2)))  ; are b1, b2 valid continuation byte ?
    (let ((n (fxior
               (fxshl (fxand #x0f b0) 12)
               (fxshl (fxand #x3f b1)  6)
               (fxshl (fxand #x3f b2)  0))))
      (if (or (fx<=? #x800 n #xd7ff) (fx>=? n #xe000))
        (values (integer->char n) 3)
        (utf8b-singlet->char b0))) ; invalid surrogate half, or overlong UTF-8 sequence
    (utf8b-singlet->char b0)))     ; invalid continuation byte b0 or b1


;; interpret four bytes as UTF-8 sequence and return corresponding char.
;; b0 is assumed to be in the range #xf0 <= b0 < #xf5
(define (utf8b-quadruplet->char b0 b1 b2 b3)
  (if (fx=? #x80 (fxand #xc0 (fxior b1 b2 b3)))  ; are b1, b2, b3 valid continuation bytes ?
    (let ((n (fxior
               (fxshl (fxand #x07 b0) 18)
               (fxshl (fxand #x3f b1) 12)
               (fxshl (fxand #x3f b2)  6)
               (fxshl (fxand #x3f b3)  0))))
      (if (fx<=? #x10000 n #x10ffff)
        (values (integer->char n) 4)
        ; overlong UTF-8 sequence, or beyond #x10ffff
        (utf8b-singlet->char b0)))
    ; invalid continuation byte b1, b2 or b3
    (utf8b-singlet->char b0)))

;; read up to max-n bytes from bytevector at offset start, interpret
;; them as UTF-8b sequence and convert them to the corresponding char.
;;
;; Returns two values: converted char, and length in bytes of UTF-8b sequence.
;; If UTF-8 sequence is incomplete, return #t instead of converted char.
;; If UTF-8 sequence is invalid, convert a single raw byte according to UTF-8b and return converted char.
(define (bytevector-ref/utf8b vec start max-n)
  (assert* 'bytevector-ref/utf8b (fx<=? 0 start (bytevector-length vec)))
  (assert* 'bytevector-ref/utf8b (fx>=? max-n 0))
  (let* ((len (bytevector-length vec))
         (max-n (fxmin max-n (fx- len start)))
         (b0  (if (fx>? max-n 0) (bytevector-u8-ref vec start) -1)))
    (cond
      ((fx<? b0    0) (values #t 0)) ; 0 bytes available
      ((fx<? b0 #x80) (values (integer->char b0) 1))
      ; convert a single raw byte to unpaired surrogate half U+dc80 ... U+dcff according to UTF-8b
      ((fx<? b0 #xc0) (utf8b-singlet->char b0))
      ((fx<? b0 #xe0)
        (if (fx>? max-n 1)
          (let ((b1 (bytevector-u8-ref vec (fx1+ start))))
            (utf8b-pair->char b0 b1))
          (values #t 1))) ; < 2 bytes available
      ((fx<? b0 #xf0)
        (if (fx>? max-n 2)
          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))
                (b2 (bytevector-u8-ref vec (fx+ 2 start))))
            (utf8b-triplet->char b0 b1 b2))
          (values #t (fxmin 2 max-n)))) ; < 3 bytes available
      ((fx<? b0 #xf5)
        (if (fx>? max-n 3)
          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))
                (b2 (bytevector-u8-ref vec (fx+ 2 start)))
                (b3 (bytevector-u8-ref vec (fx+ 3 start))))
            (utf8b-quadruplet->char b0 b1 b2 b3))
          (values #t (fxmin 3 max-n)))) ; < 4 bytes available
      (#t (utf8b-singlet->char b0)))))

;; convert char to 2-byte UTF-8 sequence and return two values: the two converted bytes.
;; ch is assumed to be in the range #x80 <= ch < #x800
(define (char->utf8-pair ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xc0 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

;; convert char to 3-byte UTF-8 sequence and return three values: the three converted bytes.
;; ch is assumed to be in the range #x800 <= ch < #x10000
(define (char->utf8-triplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xe0 (fxand #x0f (fxshr n 12)))
      (fxior #x80 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

;; convert char to 4-byte UTF-8 sequence and return four values: the four converted bytes.
;; ch is assumed to be in the range #x10000 <= ch < #x110000
(define (char->utf8-quadruplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xf0 (fxand #x07 (fxshr n 18)))
      (fxior #x80 (fxand #x3f (fxshr n 12)))
      (fxior #x80 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

;; convert a char to UTF-8b sequence and write it into given bytevector
;; from offset = start.
;; Returns one value: the length in bytes of written UTF-8b sequence.
;; Raises condition if writing the UTF-8b sequence into bytevector starting
;; from offset = start exceeds bytevector's length.
(define (bytevector-set/utf8b! vec start ch)
  (assert* 'bytevector-set/utf8b! (fx<? -1 start (bytevector-length vec)))
  (let ((n (char->integer ch)))
    (cond
      ((fx<? n 0) 0) ; should not happen
      ((fx<? n #x80)
        (bytevector-u8-set! vec start n)
        1)
      ((fx<? n #x800)
        (let-values (((b0 b1) (char->utf8-pair ch)))
          (bytevector-u8-set! vec (fx1+ start) b1)
          (bytevector-u8-set! vec start b0))
        2)
      ((fx<=? #xdc80 n #xdcff)
        ; char is unpaired surrogate half, used by UTF-8b
        ; to represent raw bytes in the range #x80 .. #xff
        (bytevector-u8-set! vec start (fxand n #xff))
        1)
      ((fx<? n #x10000)
        (let-values (((b0 b1 b2) (char->utf8-triplet ch)))
          (bytevector-u8-set! vec (fx+ 2 start) b2)
          (bytevector-u8-set! vec (fx+ 1 start) b1)
          (bytevector-u8-set! vec start b0))
        3)
      ((fx<? n #x110000)
        (let-values (((b0 b1 b2 b3) (char->utf8-quadruplet ch)))
          (bytevector-u8-set! vec (fx+ 3 start) b3)
          (bytevector-u8-set! vec (fx+ 2 start) b2)
          (bytevector-u8-set! vec (fx+ 1 start) b1)
          (bytevector-u8-set! vec start b0))
        4)
      (#t 0)))) ; should not happen

;; convert a char to UTF-8b sequence and return the length in bytes of UTF-8b sequence.
(define (char->utf8b-length ch)
  (let ((n (char->integer ch)))
    (cond
      ((fx<? n   0) 0) ; should not happen
      ((fx<? n #x80) 1)
      ((fx<? n #x800) 2)
      ((fx<=?  #xdc80 n #xdcff) 1) ; unpaired surrogate half, used by UTF-8b to encode raw bytes into chars
      ((fx<? n #x10000) 3)
      ((fx<? n #x110000) 4)
      (#t 0)))) ; should not happen

;; read up to max-n bytes from bytespan at offset idx, interpret
;; them as UTF-8b sequence and convert them to the corresponding char.
;;
;; Returns two values: converted char, and length in bytes of UTF-8b sequence.
;; If UTF-8b sequence is incomplete, return #t instead of converted char.
;; If UTF-8b sequence is invalid, return #f instead of converted char.
(define (bytespan-ref/char sp idx max-n)
  (assert* 'bytespan-ref/char (fx<=? 0 idx (bytespan-length sp)))
  (bytevector-ref/utf8b (bytespan-peek-data sp)
    (fx+ idx (bytespan-peek-beg sp))
    (fxmin max-n (fx- (bytespan-length sp) idx))))

;; convert char to UTF-8b sequence and write it into bytespan starting at offset idx
(define (bytespan-set/char! sp idx ch)
  (assert* 'bytespan-set/char! (fx<=? 0 idx (fx+ (bytespan-length sp) (char->utf8b-length ch))))
  (bytevector-set/utf8b! (bytespan-peek-data sp) (fx+ idx (bytespan-peek-beg sp)) ch))

;; convert a character to UTF-8b sequence and prefix it to bytespan.
;; Return length in bytes of inserted UTF-8b sequence
(define (bytespan-insert-front/char! sp ch)
  (let ((new-len (fx+ (bytespan-length sp) (char->utf8b-length ch))))
    (bytespan-resize-front! sp new-len)
    (bytespan-set/char! sp 0 ch)))

;; convert a character to UTF-8b sequence and append it to bytespan.
;; Return length in bytes of inserted UTF-8b sequence
(define (bytespan-insert-back/char! sp ch)
  (let* ((old-len (bytespan-length sp))
         (new-len (fx+ old-len (char->utf8b-length ch))))
    (bytespan-resize-back! sp new-len)
    (bytespan-set/char! sp old-len ch)))

;; convert a string to UTF-8b sequences and append it to bytespan.
(define (bytespan-insert-back/string! sp str)
  (bytespan-reserve-back! sp (fx+ (bytespan-length sp) (string-length str)))
  (string-iterate str
    (lambda (i ch)
      (bytespan-insert-back/char! sp ch))))

;; convert a charspan to UTF-8b sequences and append it to bytespan.
(define (bytespan-insert-back/cspan! sp csp)
  (bytespan-reserve-back! sp (fx+ (bytespan-length sp) (charspan-length csp)))
  (charspan-iterate csp
    (lambda (i ch)
      (bytespan-insert-back/char! sp ch))))

;; convert a chargbuffer to UTF-8b sequences and append it to bytespan.
(define (bytespan-insert-back/cbuffer! sp cbuf)
  (bytespan-reserve-back! sp (fx+ (bytespan-length sp) (chargbuffer-length cbuf)))
  (chargbuffer-iterate cbuf
    (lambda (i ch)
      (bytespan-insert-back/char! sp ch))))

;; convert a charspan to UTF-8b bytespan.
(define (charspan->utf8b sp)
  (let ((ret (make-bytespan 0)))
    (bytespan-insert-back/cspan! ret sp)
    ret))

;; convert a charspan to UTF-8b bytespan, then append a final byte 0 if not already present.
(define (charspan->utf8b/0 sp)
  (let ((ret (charspan->utf8b sp)))
    (when (or (bytespan-empty? ret) (not (fxzero? (bytespan-back/u8 ret))))
      (bytespan-insert-back/u8! ret 0))
    ret))


;; convert a fixnum to decimal digits and append the digits to bytespan.
(define (bytespan-display-back/fixnum! sp n)
  (if (fx<? n 0)
    (bytespan-insert-back/u8! sp 45) ; append '-'
    (set! n (fx- n)))                ; always work with negative fixnum: wider range
  (if (fx>=? n -9)
    (bytespan-insert-back/u8! sp (fx- 48 n))                         ; |n| + '0'
    (let ((max-digit-n (fx1+ (fxdiv (fx* (bitwise-length n) 3) 10))) ; upper bound
          (len (bytespan-length sp)))
      (bytespan-reserve-back! sp (fx+ len max-digit-n))
      (let* ((beg (bytespan-peek-end sp)) ; we write after bytespan-peek-end
             (end (fx+ beg max-digit-n))
             (pos end)
             (bv  (bytespan-peek-data sp))) ; bytevector
        (do ()
            ((fxzero? n))
          (let-values (((n/10 n%10) (fxdiv-and-mod n 10)))
            (set! n%10 (if (fxzero? n%10) 0 (fx- 10 n%10)))
            (set! pos (fx1- pos))
            (assert* 'bytespan-display-back/fixnum! (fx>=? pos beg))
            (bytevector-u8-set! bv pos (fx+ 48 n%10))
            (set! n (if (fxzero? n%10) n/10 (fx1+ n/10)))))\n
        (let ((digit-n (fx- end pos)))
          (when (fx>? pos beg)
            (bytevector-copy! bv pos bv beg digit-n))
          (bytespan-resize-back! sp (fx+ len digit-n)))))))

) ; close library
