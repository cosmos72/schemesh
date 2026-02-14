;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/wire/wire.ss

(define (len/container pos c n ref-proc)
  (let %len/container ((i 0) (pos (vlen+ n (tag+ pos)))) ; n is encoded as vlen
    (if (and pos (fx<? i n))
      (%len/container (fx1+ i) (len/any pos (ref-proc c i)))
      pos)))

(define (put/container bv pos tag c n ref-proc)
  (let* ((end0 (put/tag  bv pos tag))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (let %put/container ((i 0) (pos end1))
      (if (and pos (fx<? i n))
        (%put/container (fx1+ i) (put/any bv pos (ref-proc c i)))
        pos))))

(define (get/container bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (constructor n)))
        (let %get/container ((i 0) (pos pos))
          ;; (debugf "...get/container i=~s n=~s pos=~s end=~s" i n pos end)
          (cond
            ((or (not pos) (fx>? (fx- pos i) (fx- end n)))
              (values #f #f))
            ((fx>=? i n)
              (values ret pos))
            (else
              (let-values (((elem pos) (get/any bv pos end)))
                (set-proc! ret i elem)
                (%get/container (fx1+ i) pos))))))
      (values #f #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return the number of bytes needed to serialize
;; the highest-numbered character in char-container: either 1, 2 or 3
(define (bytes-per-char/char-container c n ref-proc)
  (let %again ((i 0) (max-ch #\x0))
    (if (and (fx<? i n) (char<=? max-ch #\xFFFF))
      (%again (fx1+ i) (char-max max-ch (ref-proc c i)))
      (char-len max-ch))))

(define (len/char-container pos c n ref-proc)
  (vlen+ n ;; n is encoded as vlen
    (tag+ pos
      (let ((bytes-per-char (bytes-per-char/char-container c n ref-proc)))
        (fx* n bytes-per-char))))) ;; each character is encoded as bytes-per-char bytes

(define (put/char-container bv pos tags c n ref-proc)
  (let* ((bytes-per-char (bytes-per-char/char-container c n ref-proc))
         (end0 (put/tag  bv pos (vector-ref tags (fx1- bytes-per-char))))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos bytes-per-char)))
        ((fx>=? i n)
           pos)
        (let ((ch-int (char->integer (ref-proc c i))))
          (case bytes-per-char
            ((1)  (put/u8  bv pos ch-int))
            ((2)  (put/u16 bv pos ch-int))
            (else (put/u24 bv pos ch-int)))))))

(define (get/char-container8 bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (constructor n)))
        (do ((i 0 (fx1+ i)) (pos pos (fx1+ pos)))
            ((fx>=? i n)
              (values ret pos))
          (set-proc! ret i (%get/char8 bv pos))))
      (values #f #f))))

(define (get/char-container16 bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (let ((bytes-per-char 2))
      (if (and pos (fx<=? (fx* n bytes-per-char) (fx- end pos)))
        (let %get/char-container16 ((i 0) (pos pos) (ret (constructor n)))
          (if (and pos (fx<? i n))
            (let ((ch (%get/char16 bv pos)))
              (if ch
                (begin
                  (set-proc! ret i ch)
                  (%get/char-container16 (fx1+ i) (fx+ pos bytes-per-char) ret))
                (values #f #f)))
            (values (if pos ret #f) pos)))
        (values #f #f)))))

(define (get/char-container24 bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (let ((bytes-per-char 3))
      (if (and pos (fx<=? (fx* n bytes-per-char) (fx- end pos)))
        (let %get/char-container24 ((i 0) (pos pos) (ret (constructor n)))
          (if (and pos (fx<? i n))
            (let ((ch (%get/char24 bv pos)))
              (if ch
                (begin
                  (set-proc! ret i ch)
                  (%get/char-container24 (fx1+ i) (fx+ pos bytes-per-char) ret))
                (values #f #f)))
            (values (if pos ret #f) pos)))
        (values #f #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "span" objects are serialized/deserialized

(define (len/span pos obj)
  (len/container pos obj (span-length obj) span-ref))

;; tag was already read and consumed. only read serialized n and elements
(define (get/span bv pos end)
  (get/container bv pos end make-span span-set!))

(define (put/span bv pos obj)
  (put/container bv pos tag-span obj (span-length obj) span-ref))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "gbuffer" objects are serialized/deserialized

(define (len/gbuffer pos obj)
  (len/container pos obj (gbuffer-length obj) gbuffer-ref))

;; tag was already read and consumed. only read serialized n and elements
(define (get/gbuffer bv pos end)
  (get/container bv pos end make-gbuffer gbuffer-set!))

(define (put/gbuffer bv pos obj)
  (put/container bv pos tag-gbuffer obj (gbuffer-length obj) gbuffer-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "charspan" objects are serialized/deserialized

(define (len/charspan pos obj)
  (len/char-container pos obj (charspan-length obj) charspan-ref))

;; tag-charspan8 was already read and consumed. only read serialized n and elements
(define (get/charspan8 bv pos end)
  (get/char-container8 bv pos end make-charspan charspan-set!))

;; tag-charspan24 was already read and consumed. only read serialized n and elements
(define (get/charspan16 bv pos end)
  (get/char-container16 bv pos end make-charspan charspan-set!))

;; tag-charspan24 was already read and consumed. only read serialized n and elements
(define (get/charspan24 bv pos end)
  (get/char-container24 bv pos end make-charspan charspan-set!))

(define tags-charspan (vector tag-charspan8 tag-charspan16 tag-charspan24))

(define (put/charspan bv pos obj)
  (put/char-container bv pos tags-charspan obj (charspan-length obj) charspan-ref))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "bytespan" objects are serialized/deserialized

(define (len/bytespan pos obj)
  (let ((n (bytespan-length obj)))
    (vlen+ n (tag+ pos) n)))

(define (put/bytespan bv pos obj)
  (let* ((n    (bytespan-length obj))
         (end0 (put/tag  bv pos tag-bytespan))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (if end1
      (begin
        (bytevector-copy! (bytespan-peek-data obj)
                          (bytespan-peek-beg obj) bv end1 n)
        (fx+ end1 n))
      #f)))

(define (get/bytespan bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-bytevector n)))
        (bytevector-copy! bv pos ret 0 n)
        (values (bytevector->bytespan* ret) (fx+ pos n)))
      (values #f #f))))
