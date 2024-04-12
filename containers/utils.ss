;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; define Scheme utility functions on containers:
;;; a. converting chars from/to UTF8 and reading/writing them into "bytevector" and "bytespan"
;;; b. converting integers to decimal and writing them into "bytevector" and "bytespan"


(library (schemesh containers utils (0 1))
  (export
    bytevector-ref/utf8 bytevector-set/utf8! char->utf8-length
    bytespan-ref/utf8 bytespan-set/utf8! bytespan-insert-front/utf8! bytespan-insert-back/utf8!
    bytespan-insert-back/cspan! bytespan-display-back/fixnum!
    charspan->utf8)
  (import
    (rename (rnrs)
      (fxarithmetic-shift-left  fxshl)
      (fxarithmetic-shift-right fxshr))
    (only (chezscheme) fx1+ fx1-)
    (only (schemesh bootstrap) assert*)
    (schemesh containers misc)
    (schemesh containers bytespan)
    (schemesh containers charspan))

; interpret two bytes as UTF-8 sequence and return corresponding char.
; b0 is assumed to be in the range #xc0 <= b0 < #xe0
(define (utf8-pair->char b0 b1)
  (if (fx=? #x80 (fxand #xc0 b1)) ; is b1 valid continuation byte ?
    (let ((n (fxior
               (fxshl (fxand #x1f b0) 6)
               (fxshl (fxand #x3f b1) 0))))
      (if (fx<=? #x80 n #x7ff)
        (integer->char n)
        #f)) ; overlong UTF-8 sequence
    #f)) ; invalid continuation byte b1

;
; interpret three bytes as UTF-8 sequence and return corresponding char.
; b0 is assumed to be in the range #xe0 <= b0 < #xf0
(define (utf8-triplet->char b0 b1 b2)
  (if (and (fx=? #x80 (fxand #xc0 b1))  ; is b1 valid continuation byte ?
           (fx=? #x80 (fxand #xc0 b2))) ; is b2 valid continuation byte ?
    (let ((n (fxior
               (fxshl (fxand #x0f b0) 12)
               (fxshl (fxand #x3f b1)  6)
               (fxshl (fxand #x3f b2)  0))))
      (if (or (fx<=? #x800 n #xd7ff) (fx<=? #xe000 n #xffff))
        (integer->char n)
        #f)) ; surrogate half, or overlong UTF-8 sequence
    #f)) ; invalid continuation byte b0 or b1

; interpret four bytes as UTF-8 sequence and return corresponding char.
; b0 is assumed to be in the range #xf0 <= b0 < #xf5
(define (utf8-quadruplet->char b0 b1 b2 b3)
  (if (and (fx=? #x80 (fxand #xc0 b1))  ; is b1 valid continuation byte ?
           (fx=? #x80 (fxand #xc0 b2))  ; is b2 valid continuation byte ?
           (fx=? #x80 (fxand #xc0 b3))) ; is b3 valid continuation byte ?
    (let ((n (fxior
               (fxshl (fxand #x07 b0) 18)
               (fxshl (fxand #x3f b1) 12)
               (fxshl (fxand #x3f b2)  6)
               (fxshl (fxand #x3f b3)  0))))
      (if (fx<=? #x10000 n #x10ffff)
        (integer->char n)
        #f)) ; overlong UTF-8 sequence, or beyond #x10ffff
    #f)) ; invalid continuation byte b0, b1 or b2

; read up to max-n bytes from bytevector at offset start, interpret
; them as UTF-8 sequence and convert them to the corresponding char.
;
; Returns two values: converted char, and length in bytes of UTF-8 sequence.
; If UTF-8 sequence is incomplete, return #t instead of converted char.
; If UTF-8 sequence is invalid, return #f instead of converted char.
(define (bytevector-ref/utf8 vec start max-n)
  (assert* 'bytevector-ref/utf8 (fx>=? start 0))
  (assert* 'bytevector-ref/utf8 (fx>=? max-n 0))
  (assert* 'bytevector-ref/utf8 (fx<=? start (bytevector-length vec)))
  (let* ((len (bytevector-length vec))
         (max-n (fxmin max-n (fx- len start)))
         (b0  (if (fx>? max-n 0) (bytevector-u8-ref vec start) -1)))
    (cond
      ((fx<? b0    0) (values #t 0)) ; 0 bytes available
      ((fx<? b0 #x80) (values (integer->char b0) 1))
      ((fx<? b0 #xc0) (values #f 1))
      ((fx<? b0 #xe0)
        (if (fx>? max-n 1)
          (let ((b1 (bytevector-u8-ref vec (fx1+ start))))
            (values (utf8-pair->char b0 b1) 2))
          (values #t 1))) ; < 2 bytes available
      ((fx<? b0 #xf0)
        (if (fx>? max-n 2)
          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))
                (b2 (bytevector-u8-ref vec (fx+ 2 start))))
            (values (utf8-triplet->char b0 b1 b2) 3))
          (values #t (fxmin 2 max-n)))) ; < 3 bytes available
      ((fx<? b0 #xf5)
        (if (fx>? max-n 3)
          (let ((b1 (bytevector-u8-ref vec (fx+ 1 start)))
                (b2 (bytevector-u8-ref vec (fx+ 2 start)))
                (b3 (bytevector-u8-ref vec (fx+ 3 start))))
            (values (utf8-quadruplet->char b0 b1 b2 b3) 4))
          (values #t (fxmin 3 max-n)))) ; < 4 bytes available
      (#t (values #f 1)))))

; convert char to 2-byte UTF-8 sequence and return two values: the two converted bytes.
; ch is assumed to be in the range #x80 <= ch < #x800
(define (char->utf8-pair ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xc0 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

; convert char to 3-byte UTF-8 sequence and return three values: the three converted bytes.
; ch is assumed to be in the range #x800 <= ch < #x1000
(define (char->utf8-triplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xe0 (fxand #x0f (fxshr n 12)))
      (fxior #x80 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

; convert char to 4-byte UTF-8 sequence and return four values: the four converted bytes.
; ch is assumed to be in the range #x1000 <= ch < #x110000
(define (char->utf8-quadruplet ch)
  (let ((n (char->integer ch)))
    (values
      (fxior #xf0 (fxand #x07 (fxshr n 18)))
      (fxior #x80 (fxand #x3f (fxshr n 12)))
      (fxior #x80 (fxand #x3f (fxshr n 6)))
      (fxior #x80 (fxand #x3f n)))))

; convert a char to UTF-8 sequence and write it into given bytevector
; from offset = start.
; Returns one value: the length in bytes of written UTF-8 sequence.
; Raises condition if writing the UTF-8 sequence into bytevector starting
; from offset = start exceeds bytevector's length.
(define (bytevector-set/utf8! vec start ch)
  (assert* 'bytevector-set/utf8! (fx<? -1 start (bytevector-length vec)))
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

; convert a char to UTF-8 sequence and return the length in bytes of UTF-8 sequence.
(define (char->utf8-length ch)
  (let ((n (char->integer ch)))
    (cond
      ((fx<? n   0) 0) ; should not happen
      ((fx<? n #x80) 1)
      ((fx<? n #x800) 2)
      ((fx<? n #x10000) 3)
      ((fx<? n #x110000) 4)
      (#t 0)))) ; should not happen

; read up to max-n bytes from bytespan at offset idx, interpret
; them as UTF-8 sequence and convert them to the corresponding char.
;
; Returns two values: converted char, and length in bytes of UTF-8 sequence.
; If UTF-8 sequence is incomplete, return #t instead of converted char.
; If UTF-8 sequence is invalid, return #f instead of converted char.
(define (bytespan-ref/utf8 sp idx max-n)
  (assert* 'bytespan-ref/utf8 (fx<=? 0 idx (bytespan-length sp)))
  (bytevector-ref/utf8 (bytespan-peek-data sp)
    (fx+ idx (bytespan-peek-beg sp))
    (fxmin max-n (fx- (bytespan-length sp) idx))))

; convert char to UTF-8 sequence and write it into bytespan starting at offset idx
(define (bytespan-set/utf8! sp idx ch)
  (assert* 'bytespan-set/utf8! (fx<=? 0 idx (fx+ (bytespan-length sp) (char->utf8-length ch))))
  (bytevector-set/utf8! (bytespan-peek-data sp) (fx+ idx (bytespan-peek-beg sp)) ch))

; convert a character to UTF-8 sequence and prefix it to bytespan.
; Return length in bytes of inserted UTF-8 sequence
(define (bytespan-insert-front/utf8! sp ch)
  (let ((new-len (fx+ (bytespan-length sp) (char->utf8-length ch))))
    (bytespan-resize-front! sp new-len)
    (bytespan-set/utf8! sp 0 ch)))

; convert a character to UTF-8 sequence and append it to bytespan.
; Return length in bytes of inserted UTF-8 sequence
(define (bytespan-insert-back/utf8! sp ch)
  (let* ((old-len (bytespan-length sp))
         (new-len (fx+ old-len (char->utf8-length ch))))
    (bytespan-resize-back! sp new-len)
    (bytespan-set/utf8! sp old-len ch)))

; convert a charspan to UTF-8 sequences and append it to bytespan.
(define (bytespan-insert-back/cspan! sp csp)
  (bytespan-reserve-back! sp (fx+ (bytespan-length sp) (charspan-length csp)))
  (charspan-iterate csp
    (lambda (i ch)
      (bytespan-insert-back/utf8! sp ch))))

; convert a charspan to UTF-8 bytespan.
(define (charspan->utf8 sp)
  (let ((ret (make-bytespan 0)))
    (bytespan-insert-back/cspan! ret sp)
    ret))

; convert a fixnum to decimal digits and append the digits to bytespan.
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
