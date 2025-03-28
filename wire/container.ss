;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file wire/wire.ss

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

;; return #t if all characters in char-container c are char<=? #\xFF
(define (char-container8? c n ref-proc)
  (do ((i 0 (fx1+ i)))
      ((or (fx>=? i n) (char>? (ref-proc c i) #\xFF))
        (fx>=? i n))))


(define (len/char-container pos c n ref-proc)
  (vlen+ n ;; n is encoded as vlen
    (tag+ pos
      (if (char-container8? c n ref-proc)
        n ;; each character is encoded as 1 byte
        (fx* n max-len-char))))) ;; each character is encoded as max-len-char bytes

(define (put/char-container bv pos tag tag8 c n ref-proc)
  (let* ((container8? (char-container8? c n ref-proc))
         (end0 (put/tag  bv pos (if container8? tag8 tag)))
         (end1 (put/vlen bv end0 n)) ; n is encoded as vlen
         (step (if container8? 1 max-len-char)))
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos step)))
        ((fx>=? i n)
           pos)
        (let ((ch-int (char->integer (ref-proc c i))))
          (if container8?
            (put/u8  bv pos ch-int)
            (put/u24 bv pos ch-int))))))

(define (get/char-container8 bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (constructor n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n))
          (set-proc! ret i (integer->char (get/u8 bv (fx+ pos i)))))
        (values ret (fx+ pos n)))
      (values #f #f))))

(define (get/char-container bv pos end constructor set-proc!)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx* max-len-char (fx- end pos))))
      (let %get/char-container ((ret (constructor n)) (i 0) (pos pos))
        (cond
          ((not pos)
            (values #f #f))
          ((fx<? i n)
            (let ((ch (get/char24* bv pos)))
              (if ch
                (begin
                  (set-proc! ret i ch)
                  (%get/char-container ret (fx1+ i) (fx+ pos max-len-char)))
                (values #f #f))))
          (else
            (values ret pos))))
      (values #f #f))))


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

;; tag-charspan was already read and consumed. only read serialized n and elements
(define (get/charspan bv pos end)
  (get/char-container bv pos end make-charspan charspan-set!))

(define (put/charspan bv pos obj)
  (put/char-container bv pos tag-charspan tag-charspan8 obj (charspan-length obj) charspan-ref))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "chargbuffer" objects are serialized/deserialized

(define (len/chargbuffer pos obj)
  (len/char-container pos obj (chargbuffer-length obj) chargbuffer-ref))

;; tag-chargbuffer8 was already read and consumed. only read serialized n and elements
(define (get/chargbuffer8 bv pos end)
  (get/char-container8 bv pos end make-chargbuffer chargbuffer-set!))

;; tag-chargbuffer was already read and consumed. only read serialized n and elements
(define (get/chargbuffer bv pos end)
  (get/char-container bv pos end make-chargbuffer chargbuffer-set!))

(define (put/chargbuffer bv pos obj)
  (put/char-container bv pos tag-chargbuffer tag-chargbuffer8 obj (chargbuffer-length obj) chargbuffer-ref))
