;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;;; define Scheme utility functions on containers:
;;; a. converting chars from/to UTF8 and reading/writing them into "bytevector" and "bytespan"
;;; b. converting integers to decimal and writing them into "bytevector" and "bytespan"


(library (schemesh containers utf8b utils (0 9 2))
  (export
    bytevector-char-ref bytevector-char-set! char->utf8b-length
    bytespan-ref/char bytespan-set/char! bytespan-insert-left/char! bytespan-insert-right/char!
    bytespan-insert-right/charspan!
    bytespan-display-right/fixnum! bytespan-display-right/integer! bytespan-insert-right/string!
    charspan->utf8b charspan->utf8b/0)
  (import
    (rename (rnrs) (fxarithmetic-shift-left  fx<<)
                   (fxarithmetic-shift-right fx>>))
    (only (chezscheme) fx1+ fx1-)
    (only (schemesh bootstrap)              assert* fx<=?*)
    (schemesh containers bytespan)
    (schemesh containers charspan)
    (only (schemesh containers string)      string-iterate)
    (schemesh containers utf8b))


;; encode a single raw byte in the range #x80 ... #xff that is NOT part of a valid UTF-8 sequence
;; as a char in the surrogate range range U+dc80 ... U+dcff according to UTF-8b specifications
(define (utf8b-singlet->char b0)
  (values (integer->char* (fxior #xdc80 b0)) 1))


;; interpret two bytes as UTF-8b sequence and return corresponding char.
;; b0 is assumed to be in the range #xc0 <= b0 < #xe0
(define (utf8b-pair->char b0 b1)
  (if (fx=? #x80 (fxand #xc0 b1)) ; is b1 valid continuation byte ?
    (let ((n (fxior
               (fx<< (fxand #x1f b0) 6)
               (fx<< (fxand #x3f b1) 0))))
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
               (fx<< (fxand #x0f b0) 12)
               (fx<< (fxand #x3f b1)  6)
               (fx<< (fxand #x3f b2)  0))))
      (if (or (fx<=? #x800 n #xd7ff) (fx>=? n #xe000))
        (values (integer->char n) 3)
        (utf8b-singlet->char b0))) ; invalid surrogate half, or overlong UTF-8 sequence
    (utf8b-singlet->char b0)))     ; invalid continuation byte b0 or b1


;; interpret four bytes as UTF-8 sequence and return corresponding char.
;; b0 is assumed to be in the range #xf0 <= b0 < #xf5
(define (utf8b-quadruplet->char b0 b1 b2 b3)
  (if (fx=? #x80 (fxand #xc0 (fxior b1 b2 b3)))  ; are b1, b2, b3 valid continuation bytes ?
    (let ((n (fxior
               (fx<< (fxand #x07 b0) 18)
               (fx<< (fxand #x3f b1) 12)
               (fx<< (fxand #x3f b2)  6)
               (fx<< (fxand #x3f b3)  0))))
      (if (fx<=? #x10000 n #x10ffff)
        (values (integer->char n) 4)
        ; overlong UTF-8 sequence, or beyond #x10ffff
        (utf8b-singlet->char b0)))
    ; invalid continuation byte b1, b2 or b3
    (utf8b-singlet->char b0)))

;; read byte range [start, end) from bytevector, interpret bytes
;; as UTF-8b sequence and convert them to the corresponding char.
;;
;; Returns two values: converted char, and length in bytes of UTF-8b sequence.
;; If UTF-8 sequence is incomplete, return #t instead of converted char:
;;   in such case, length is always (fx- end start) and should NOT be considered "consumed".
;; If UTF-8 sequence is invalid, convert a single raw byte according to UTF-8b:
;;   return converted char and length equal to 1.
(define bytevector-char-ref
  (case-lambda
    ((vec start end)
      (assert* 'bytevector-char-ref (fx<=?* 0 start end (bytevector-length vec)))
      (let* ((max-n (fx- end start))
             (b0    (if (fx>? max-n 0) (bytevector-u8-ref vec start) -1)))
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
          (else
            (utf8b-singlet->char b0)))))
    ((vec pos)
      (bytevector-char-ref vec pos (bytevector-length vec)))
    ((vec)
      (bytevector-char-ref vec 0 (bytevector-length vec)))))


;; convert char to 2-byte UTF-8 sequence and return two values: the two converted bytes.
;; ch is assumed to be in the range #x80 <= ch < #x800
(define (char->utf8-pair ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xc0 (fxand #x3f (fx>> n 6)))
      (fxior #x80 (fxand #x3f n)))))

;; convert char to 3-byte UTF-8 sequence and return three values: the three converted bytes.
;; ch is assumed to be in the range #x800 <= ch < #x10000
(define (char->utf8-triplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xe0 (fxand #x0f (fx>> n 12)))
      (fxior #x80 (fxand #x3f (fx>> n 6)))
      (fxior #x80 (fxand #x3f n)))))

;; convert char to 4-byte UTF-8 sequence and return four values: the four converted bytes.
;; ch is assumed to be in the range #x10000 <= ch < #x110000
(define (char->utf8-quadruplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xf0 (fxand #x07 (fx>> n 18)))
      (fxior #x80 (fxand #x3f (fx>> n 12)))
      (fxior #x80 (fxand #x3f (fx>> n 6)))
      (fxior #x80 (fxand #x3f n)))))


;; convert a char to UTF-8b sequence and write it into given bytevector
;; from offset = start.
;; Returns one value: the length in bytes of written UTF-8b sequence.
;; Raises condition if writing the UTF-8b sequence into bytevector starting
;; from offset = start exceeds bytevector's length.
(define (bytevector-char-set! vec start ch)
  (assert* 'bytevector-char-set! (fx<? -1 start (bytevector-length vec)))
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
        ;; char is unpaired surrogate half, used by UTF-8b
        ;; to represent raw bytes in the range #x80 .. #xff
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
      (else 0)))) ; should not happen

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
      (else 0)))) ; should not happen




;; read byte range [start, end) from bytespan at offset idx, interpret
;; bytes as UTF-8b sequence and convert them to the corresponding char.
;;
;; Returns two values: converted char, and length in bytes of UTF-8b sequence.
;; If UTF-8b sequence is incomplete, return #t instead of converted char.
;; If UTF-8b sequence is invalid, return #f instead of converted char.
(define bytespan-ref/char
  (case-lambda
    ((sp start end)
      (assert* 'bytespan-ref/char (fx<=?* 0 start end (bytespan-length sp)))
      (let ((offset (bytespan-peek-beg sp)))
        (bytevector-char-ref (bytespan-peek-data sp) (fx+ start offset) (fx+ end offset))))
    ((sp start)
      (bytespan-ref/char sp start (bytespan-length sp)))))


;; convert char to UTF-8b sequence and write it into bytespan starting at offset idx.
;; Return length in bytes of written UTF-8b sequence
(define (bytespan-set/char! sp idx ch)
  (assert* 'bytespan-set/char! (fx<=?* 0 idx (fx+ (bytespan-length sp) (char->utf8b-length ch))))
  (bytevector-char-set! (bytespan-peek-data sp) (fx+ idx (bytespan-peek-beg sp)) ch))

;; convert a character to UTF-8b sequence and prefix it to bytespan.
;; Return length in bytes of inserted UTF-8b sequence
(define (bytespan-insert-left/char! sp ch)
  (let ((new-len (fx+ (bytespan-length sp) (char->utf8b-length ch))))
    (bytespan-resize-left! sp new-len)
    (bytespan-set/char! sp 0 ch)))

;; convert a character to UTF-8b sequence and append it to bytespan.
;; Return length in bytes of inserted UTF-8b sequence
(define (bytespan-insert-right/char! sp ch)
  (let* ((old-len (bytespan-length sp))
         (new-len (fx+ old-len (char->utf8b-length ch))))
    (bytespan-resize-right! sp new-len)
    (bytespan-set/char! sp old-len ch)))

;; convert a string to UTF-8b sequences and append it to bytespan.
(define bytespan-insert-right/string!
  (case-lambda
    ((sp str start end)
      (assert* 'bytespan-insert-right/string! (fx<=?* 0 start end (string-length str)))
      (bytespan-reserve-right! sp (fx+ (bytespan-length sp) (fx- end start)))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (bytespan-insert-right/char! sp (string-ref str i))))
    ((sp str)
      (bytespan-insert-right/string! sp str 0 (string-length str)))))


;; convert a charspan to UTF-8b sequences and append it to bytespan.
(define bytespan-insert-right/charspan!
  (case-lambda
    ((sp csp start end)
      (assert* 'bytespan-insert-right/charspan! (fx<=?* 0 start end (charspan-length csp)))
      (bytespan-reserve-right! sp (fx+ (bytespan-length sp) (fx- end start)))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (bytespan-insert-right/char! sp (charspan-ref csp i))))
    ((sp csp)
      (bytespan-insert-right/charspan! sp csp 0 (charspan-length csp)))))


;; convert a charspan to UTF-8b bytespan.
(define charspan->utf8b
  (case-lambda
    ((sp start end)
      (let ((ret (make-bytespan 0)))
        (bytespan-insert-right/charspan! ret sp start end)
        ret))
    ((sp)
      (charspan->utf8b sp 0 (charspan-length sp)))))


;; convert a charspan to UTF-8b bytespan, then append a final byte 0 if not already present.
(define (charspan->utf8b/0 sp)
  (let ((ret (charspan->utf8b sp)))
    (when (or (bytespan-empty? ret) (not (fxzero? (bytespan-ref-right/u8 ret))))
      (bytespan-insert-right/u8! ret 0))
    ret))


;; convert a fixnum to decimal digits and append such digits to bytespan.
(define (bytespan-display-right/fixnum! sp n)
  (let ((n (if (fx<? n 0)
             (begin
               (bytespan-insert-right/u8! sp 45) ; append '-'
               n)
             (fx- n)))) ; always work with negative fixnum: wider range
    (if (fx>=? n -9)
      (bytespan-insert-right/u8! sp (fx- 48 n))  ; |n| + '0'
      (let ((max-digit-n (fx1+ (fxdiv (fx* (bitwise-length n) 3) 10))) ; upper bound
            (len         (bytespan-length sp)))
        (bytespan-reserve-right! sp (fx+ len max-digit-n))
        (%bytespan-display-right/nfixnum! sp len n max-digit-n)))))


;; convert a negative fixnum to decimal digits and append such digits to bytespan.
;; ignores the sign. does not support n >= 0.
(define (%bytespan-display-right/nfixnum! sp len n max-digit-n)
  (let* ((beg (bytespan-peek-end sp)) ; we write after bytespan-peek-end
         (end (fx+ beg max-digit-n))
         (bv  (bytespan-peek-data sp)) ; bytevector
         (wpos
           (let %display-fixnum-loop ((n n) (pos end))
             ;; (debugf "%display-fixnum-loop bv=~s pos=~s n=~s" bv pos n)
             (if (fxzero? n)
               pos
               (let-values (((n/10 n%10) (fxdiv-and-mod n 10)))
                 (let ((n%10 (if (fxzero? n%10) 0 (fx- 10 n%10)))
                       (pos (fx1- pos)))
                   (assert* 'bytespan-display-right/fixnum! (fx>=? pos beg))
                   (bytevector-u8-set! bv pos (fx+ 48 n%10))
                   (%display-fixnum-loop
                      (if (fxzero? n%10) n/10 (fx1+ n/10))
                      pos))))))
         (digit-n (fx- end wpos)))
    (when (fx>? wpos beg)
      (bytevector-copy! bv wpos bv beg digit-n))
    (bytespan-resize-right! sp (fx+ len digit-n))))


;; convert an exact integer to decimal digits and append the digits to bytespan.
(define (bytespan-display-right/integer! sp n)
  (assert* 'bytespan-display-right/integer! (exact? n))
  (assert* 'bytespan-display-right/integer! (integer? n))
  (cond
    ((fixnum? n)
      (bytespan-display-right/fixnum! sp n))
    (else
      (when (< n 0)
        (bytespan-insert-right/u8! sp 45) ; append '-'
        (set! n (- n)))                   ; always work with unsigned integers: easier
      (let ((max-digit-n (fx1+ (fxdiv (fx* (bitwise-length n) 3) 10))) ; upper bound
            (len (bytespan-length sp)))
        (bytespan-reserve-right! sp (fx+ len max-digit-n))
        (let* ((beg (bytespan-peek-end sp)) ; we write after bytespan-peek-end
               (end (fx+ beg max-digit-n))
               (bv  (bytespan-peek-data sp)) ; bytevector
               (wpos
                 (let %loop ((n n) (pos end))
                   ;; (debugf "%bytespan-display-right/integer! bv=~s n=~s" bv n)
                   (if (and (fixnum? n) (fxzero? n))
                     pos
                     (let-values (((n/10 n%10) (div-and-mod n 10)))
                       (let ((pos (fx1- pos)))
                         (assert* 'bytespan-display-right/integer! (fx>=? pos beg))
                         (bytevector-u8-set! bv pos (fx+ 48 n%10))
                         (%loop n/10 pos))))))
               (digit-n (fx- end wpos)))
          (when (fx>? wpos beg)
            (bytevector-copy! bv wpos bv beg digit-n))
          (bytespan-resize-right! sp (fx+ len digit-n)))))))


) ; close library
