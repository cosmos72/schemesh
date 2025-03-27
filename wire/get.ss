;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file wire/wire.ss

(define (get/exact-s8 bv pos end)
  (values (get/s8 bv pos) (fx1+ pos)))

(define (get/exact-s16 bv pos end)
  (values (get/s16 bv pos) (fx+ pos 2)))

(define (get/exact-s24 bv pos end)
  (values (get/s24 bv pos) (fx+ pos 3)))

(define (get/exact-s32 bv pos end)
  (values (get/s32 bv pos) (fx+ pos 4)))

(define (get/exact-sint bv pos end)
  (let ((n (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (fx<=? n (fx- end pos))
      (values (bytevector-sint-ref bv pos endian n) (fx+ pos n))
      (values #f #f))))

;; reads tag, unlike most other (get/...) functions
;; returns two values: deserialized exact integer and updated pos,
;; or (values #f #f) on errors
(define (%get/exact-int bv pos end)
  (let* ((tag (get/tag bv pos))
         (obj (tag->obj tag))
         (pos (tag+ pos)))
    (cond
      ((fixnum? obj)
        (values obj pos))
      ((fx<=? tag tag-ratio)
        (obj bv pos end))
      (else
        (values #f #f)))))


;; reads tag, unlike most other (get/...) functions
;; returns two values: deserialized exact real and updated pos,
;; or (values #f #f) on errors
(define (%get/exact-real bv pos end)
  (let* ((tag (get/tag bv pos))
         (obj (tag->obj tag))
         (pos (tag+ pos)))
    (cond
      ((fixnum? obj)
        (values obj pos))
      ((fx<=? tag tag-ratio)
        (obj bv pos end))
      (else
        (values #f #f)))))


(define (get/ratio bv pos end)
  (let*-values (((num pos) (%get/exact-int bv pos end))
                ((den pos) (%get/exact-int bv pos end)))
    (if (and num den pos)
      (values (/ num den) pos)
      (values #f #f))))

(define (get/complex bv pos end)
  (let*-values (((real pos) (%get/exact-real bv pos end))
                ((imag pos) (%get/exact-real bv pos end)))
    (if (and real imag pos)
      (values (make-rectangular real imag) pos)
      (values #f #f))))


(define (get/flonum bv pos end)
  (values (bytevector-ieee-double-ref bv pos endian)
          (fx+ pos len-flonum)))

(define (get/cflonum bv pos end)
  (let ((real (bytevector-ieee-double-ref bv pos endian))
        (imag (bytevector-ieee-double-ref bv (fx+ pos len-flonum) endian)))
    (values (fl-make-rectangular real imag)
            (fx+ pos len-cflonum))))


(define (get/char8 bv pos end)
  (values (integer->char (get/u8 bv pos)) (fx1+ pos)))

(define (get/char16 bv pos end)
  (let ((x (get/u16 bv pos)))
    (cond
      ((or (fx<=? x #xD7FF) (fx>=? x #xE000))
        (values (integer->char x) (fx+ pos 2)))
      ((fx<=? #xDC80 x #xDCFF)
        (values (integer->char* x) (fx+ pos 2)))
      (else
        (values #f #f)))))

;; return one value: char deserialized from 3 bytes, or #f on error
(define (get/char24* bv pos)
  (let ((x (get/u24 bv pos)))
    (cond
      ((or (fx<=? x #xD7FF) (fx<=? #xE000 x #x10FFFF))
        (integer->char x))
      ((fx<=? #xDC80 x #xDCFF)
        (integer->char* x))
      (else
        #f))))

;; return two values:
;;   char deserialized from 3 bytes and updated pos,
;;   or (values #f #f) on error
(define (get/char24 bv pos end)
  (let ((ch(get/char24* bv pos)))
    (values ch (if ch (fx+ pos 3) #f))))

(define (get/box bv pos end)
  (let-values (((obj pos) (get/any bv pos end)))
    (if pos
      (values (box obj) pos)
      (values #f #f))))

(define (get/pair bv pos end)
  (let*-values (((head pos) (get/any bv pos end))
                ((tail pos) (get/any bv pos end)))
    (if pos
      (values (cons head tail) pos)
      (values #f #f))))

(define (get/list1 bv pos end)
  (let-values (((obj pos) (get/any bv pos end)))
    (if pos
      (values (list obj) pos)
      (values #f #f))))

(define (get/list* bv pos end)
  (values #f #f)) ; TODO implement

(define (get/list bv pos end)
  (let %get/list ((n   (get/dlen bv pos))
                  (pos (dlen+ pos))
                  (ret '()))
      (cond
        ((or (not pos) (fx>? n (fx- end pos)))
          (values #f #f))
        ((fxzero? n)
          (values (reverse! ret) pos))
        (else
          (let-values (((elem pos) (get/any bv pos end)))
            (%get/list (fx1- n) pos (cons elem ret)))))))


(define (get/container bv pos end constructor set-proc!)
  (let ((n (get/dlen bv pos))
        (pos (dlen+ pos)))
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


(define (get/vector bv pos end)
  (get/container bv pos end make-vector vector-set!))



(define (get/bvector bv pos end)
  (let ((n (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-bytevector n)))
        (bytevector-copy! bv pos ret 0 n)
        (values bv (fx+ pos n) end)))
    (values #f #f)))


(define (get/string8 bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-string n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n))
          (string-set! ret i (integer->char (get/u8 bv (fx+ pos i)))))
        (values ret (fx+ pos n)))
      (values #f #f))))

(define (get/string bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx* max-len-char (fx- end pos))))
      (let %get/string ((ret (make-string n)) (i 0) (pos pos))
        (cond
          ((not pos)
            (values #f #f))
          ((fx<? i n)
            (let ((ch (get/char24* bv pos)))
              (if ch
                (begin
                  (string-set! ret i ch)
                  (%get/string ret (fx1+ i) (fx+ pos max-len-char)))
                (values #f #f))))
          (else
            (values ret pos))))
      (values #f #f))))

(define (get/fxvector bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let %get/fxvector ((ret (make-fxvector n)) (i 0) (pos pos))
        (cond
          ((or (not pos) (fx>? (fx- pos i) (fx- end n)))
            (values #f #f))
          ((fx<? i n)
            (let-values (((elem pos) (%get/exact-int bv pos end)))
              ;; (debugf "...get/fxvector i=~s n=~s elem=~s pos=~s end=~s" i n elem pos end)
              (if (and (fixnum? elem) pos)
                (begin
                  (fxvector-set! ret i elem)
                  (%get/fxvector ret (fx1+ i) pos))
                (values #f #f))))
          (else
            (values ret pos))))
      (values #f #f))))

(define (get/flvector bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? (fx* n len-flonum) (fx- end pos)))
      (do ((ret (make-flvector n))
           (i 0 (fx1+ i))
           (pos pos (fx+ pos len-flonum)))
          ((fx>=? i n)
            (values ret pos))
        (flvector-set! ret i (bytevector-ieee-double-ref bv pos endian)))
      (values #f #f))))


(define (get/symbol8 bv pos end)
  (let-values (((str pos) (get/string8 bv pos end)))
    (values (if pos (string->symbol str) #f) pos)))

(define (get/symbol bv pos end)
  (let-values (((str pos) (get/string bv pos end)))
    (values (if pos (string->symbol str) #f) pos)))


(define known-cmp-proc  (hashtable-transpose known-cmp-sym (make-eq-hashtable)))
(define known-hash-proc (hashtable-transpose known-hash-sym (make-eq-hashtable)))

(define (cmp-sym->proc sym)  (hashtable-ref known-cmp-proc sym #f))
(define (hash-sym->proc sym) (hashtable-ref known-hash-proc sym #f))

(define (%fill/hashtable bv pos end n ret)
  (if (fxzero? n)
    (values ret pos)
    (let*-values (((key pos) (get/any bv pos end))
                  ((val pos) (get/any bv pos end)))
      (if pos
        (begin
          (hashtable-set! ret key val)
          (%fill/hashtable bv pos end (fx1- n) ret))
        (values #f #f)))))

(define (get/eq-hashtable bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx- end pos)))
      (%fill/hashtable bv pos end n (make-eq-hashtable n))
      (values #f #f))))

(define (get/eqv-hashtable bv pos end)
  (let ((n   (get/dlen bv pos))
        (pos (dlen+ pos)))
    (if (and pos (fx<=? n (fx- end pos)))
      (%fill/hashtable bv pos end n (make-eqv-hashtable n))
      (values #f #f))))

(define (get/hashtable bv pos end)
  (let*-values (((hash-sym pos) (get/any bv pos end))
                ((cmp-sym pos)  (get/any bv pos end)))
    (let ((n   (get/dlen bv pos))
          (pos (dlen+ pos)))
      ;; (debugf "...get/hashtable pos=~s n=~s hash-sym=~s cmp-sym=~s" pos n hash-sym cmp-sym)
      (if (and pos (fx<=? n (fx- end pos)) (symbol? hash-sym) (symbol? cmp-sym))
        (let ((hash-proc (hash-sym->proc hash-sym))
              (cmp-proc  (cmp-sym->proc cmp-sym)))
          ;; (debugf "...get/hashtable hash-proc=~s cmp-proc=~s" hash-proc cmp-proc)
          (if (and hash-proc cmp-proc)
            (%fill/hashtable bv pos end n (make-hashtable hash-proc cmp-proc n))
            (values #f #f)))
        (values #f #f)))))


(define known-tag
  (let ((plist
          (list tag-0 0 tag-1 1 tag-2 2 tag-3 3 tag-4 4 tag-5 5 tag-6 6 tag-7 7 tag-8 8
                tag-9 9 tag-10 10 tag--5 -5 tag--4 -4 tag--3 -3 tag--2 -2 tag--1 -1
                tag-s8 get/exact-s8 tag-s16 get/exact-s16 tag-s24 get/exact-s24 tag-s32 get/exact-s32
                tag-sint get/exact-sint tag-ratio get/ratio tag-complex get/complex
                tag-flonum get/flonum tag-cflonum get/cflonum
                tag-f #f tag-t #t tag-nil '() tag-void (void) tag-eof (eof-object) tag-bwp #!bwp
                tag-char8 get/char8 tag-char16 get/char16 tag-char24 get/char24 tag-box get/box
                tag-pair get/pair tag-list1 get/list1 tag-list* get/list* tag-list get/list
                tag-vector get/vector tag-bvector get/bvector tag-string8 get/string8 tag-string get/string
                tag-fxvector get/fxvector tag-flvector get/flvector tag-symbol8 get/symbol8 tag-symbol get/symbol
                tag-eq-hashtable get/eq-hashtable tag-eqv-hashtable get/eqv-hashtable tag-hashtable get/hashtable))
        (vec (make-vector 256 (void))))
    (for-plist ((tag obj plist))
      (vector-set! vec tag obj))
    (for-hash ((sym tag known-sym))
      (vector-set! vec tag sym))
    vec))

(define min-tag-to-allocate 87)
(define max-tag-to-allocate 253)
(define next-tag-to-allocate 247)

;; reserve a fixnum tag to use when serializing a custom record type
;; return the fixnum tag value, or #f if tags are exhausted.
(define (wire-reserve-tag)
  (let ((ret next-tag-to-allocate))
    (if (fx>=? ret min-tag-to-allocate)
      (begin
        (set! next-tag-to-allocate (fx1- ret))
        ret)
      #f)))

(define (tag->obj tag)
  (vector-ref known-tag tag))


;; read byte range [pos, end) from bytevector bv and deserialize an object from it.
;; Return two values:
;;   either object and updated start position in range [pos, end)
;;   or #f #f if serialized bytes are invalid
(define (get/any bv pos end)
  (if (fx<? pos end)
    (let ((obj (tag->obj (get/tag bv pos)))
          (pos (tag+ pos)))
      (if (procedure? obj)
        (obj bv pos end)
        (values obj pos)))
    (values #f #f)))



;; read byte range [start, end) from bytevector bv and deserialize an object from it.
;; Return two values:
;;   either object and updated start position in range [start, end)
;;   or #f #f if serialized bytes are invalid
;;   or #f -NNN if not enough bytes are available and at least NNN bytes should be added after end.
(define (wire-get/bvector bv start end)
  (let* ((pos       (fx+ start dlen))
         (available (fx- end pos)))
    (cond
      ((fx>=? available 0)
        (let ((len (get/dlen bv start)))
          (if (fx>=? available len)
            (if (fxzero? len)
              (values (void) pos) ; (void) can be encoded as dlen = 0
              (let ((end0 (fx+ pos len)))
                (let-values (((ret end1) (get/any bv pos end)))
                  (if (and end1 (fx=? end0 end1))
                    (values ret end1)
                    (values #f #f)))))
            (values #f (fx- available len)))))
      (else
        (values #f available)))))


;; read bytes from bytevector or bytespan stc and deserialize an object from them.
;; Return two values:
;;   either object and number of consumed bytes,
;;   or #f #f if serialized bytes are invalid
;;   or #f -NNN if not enough bytes are available and at least NNN bytes should be added to the bytespan end.
(define wire-get
  (case-lambda
    ((src)
      (if (bytevector? src)
        (wire-get/bvector src 0 (bytevector-length src))
        (wire-get/bvector (bytespan-peek-data src) (bytespan-peek-beg src) (bytespan-peek-end src))))
    ((src start end)
      (if (bytevector? src)
        (let ((len (bytevector-length src)))
          (if (fx<=?* 0 start end len)
            (wire-get/bvector src start end)
            (values #f #f)))
        (let ((offset (bytespan-peek-beg src)))
          (if (fx<=?* 0 start end (bytespan-length src))
            (wire-get/bvector (bytespan-peek-data src) (fx+ start offset) (fx+ end offset))
            (values #f #f)))))))
