;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


#!r6rs

;; this file should be included only by file wire/wire.ss

(include "wire/bwp.ss")

(define (get/exact-s8 bv pos end)
  ;; caller is (get/any) or similar, and guarantees that (fx<? pos end)
  (values (%get/s8 bv pos) (fx1+ pos)))

(define (get/exact-s16 bv pos end)
  (if (and (fixnum? pos) (fx>=? (fx- end pos) 2))
    (values (%get/s16 bv pos) (fx+ pos 2))
    (values #f #f)))

(define (get/exact-s24 bv pos end)
  (if (and (fixnum? pos) (fx>=? (fx- end pos) 3))
    (values (%get/s24 bv pos) (fx+ pos 3))
    (values #f #f)))

(define (get/exact-s32 bv pos end)
  (if (and (fixnum? pos) (fx>=? (fx- end pos) 4))
    (values (%get/s32 bv pos) (fx+ pos 4))
    (values #f #f)))

(define (get/exact-sint bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? 1 n (fx- end pos))) ; n must be > 0
      (values (bytevector-sint-ref* bv pos endian n) (fx+ pos n))
      (values #f #f))))


;; validate vlen before returning it
(define (vlen-values vlen pos end)
  (cond
    ((or (not (fixnum? vlen)) (fx>? vlen max-vlen))
      ;; message may have been serialized on a different system with larger (greatest-fixnum)
      ;; but this system does not support containers with such a large number of elements
      (values #f #f))
    ((fx<=? vlen (fx- end pos))
      (values vlen pos))
    (else
      ;; vlen exceeds remaining message length
      (values vlen #f))))


;; read unsigned fixnum vlen from bytevector starting at position pos.
;; return two values: vlen and updated position, or #f #f on errors.
(define (get/vlen bv pos end)
  (let ((lo (and pos (fx<? pos end) (%get/u8 bv pos))))
    (cond
      ((not lo)
        (values #f #f))
      ((fx<=? lo #x7f)
        (vlen-values lo (fx1+ pos) end))
      ((fx<=? pos (fx- end max-len-vlen))
        (let ((u32 (%get/u32 bv pos)))
          (meta-cond
            ((fixnum? #xffffffff)
              (let ((lo (fxand u32 #x7f))
                    (hi (fxsrl (fxand u32 #xffffff00) 1)))
                (vlen-values (fxior lo hi) (fx+ pos max-len-vlen) end)))
            (else
              (let ((lo (fxand lo #x7f))
                    (hi (bitwise-arithmetic-shift-right (bitwise-and u32 #xffffff00) 1)))
                (vlen-values (bitwise-ior lo hi) (fx+ pos max-len-vlen) end))))))
      (else
        (values #f #f)))))



;; also reads tag, unlike most other (get/...) functions
;; returns two values: deserialized exact integer and updated pos,
;; or (values #f #f) on errors
(define (%get/exact-int bv pos end)
  (if (and pos (fx<? pos end))
    (let* ((tag (%get/tag bv pos))
           (obj (tag->obj tag))
           (pos (tag+ pos)))
      (cond
        ((not pos)
          (values #f #f))
        ((fixnum? obj)
          (values obj pos))
        ((and (fx<? pos end) (fx<=? tag tag-ratio))
          (obj bv pos end))
        (else
          (values #f #f))))
    (values #f #f)))


;; reads tag, unlike most other (get/...) functions
;; returns two values: deserialized exact real and updated pos,
;; or (values #f #f) on errors
(define (%get/exact-real bv pos end)
  (if (and pos (fx<? pos end))
    (let* ((tag (%get/tag bv pos))
           (obj (tag->obj tag))
           (pos (tag+ pos)))
      (cond
        ((not pos)
          (values #f #f))
        ((fixnum? obj)
          (values obj pos))
        ((and (fx<? pos end) (fx<=? tag tag-ratio))
          (obj bv pos end))
        (else
          (values #f #f))))
    (values #f #f)))


(define (get/ratio bv pos end)
  (let*-values (((num pos) (%get/exact-int bv pos end))
                ((den pos) (%get/exact-int bv pos end)))
    (if (and num den pos (not (zero? den)))
      (values (/ num den) pos)
      (values #f #f))))

(define (get/complex bv pos end)
  (let*-values (((real pos) (%get/exact-real bv pos end))
                ((imag pos) (%get/exact-real bv pos end)))
    (if (and real imag pos)
      (values (make-rectangular real imag) pos)
      (values #f #f))))


(define (get/flonum bv pos end)
  (if (fx>=? (fx- end pos) len-flonum)
    (values (bytevector-ieee-double-ref bv pos endian)
            (fx+ pos len-flonum))
    (values #f #f)))

(define (get/cflonum bv pos end)
  (if (fx>=? (fx- end pos) len-cflonum)
    (let ((real (bytevector-ieee-double-ref bv pos endian))
          (imag (bytevector-ieee-double-ref bv (fx+ pos len-flonum) endian)))
      (values (fl-make-rectangular real imag)
              (fx+ pos len-cflonum)))
    (values #f #f)))


;; return one value: char deserialized from 1 byte.
(define (%get/char8 bv pos)
  (integer->char (%get/u8 bv pos)))

;; return one value: char deserialized from 2 bytes, or #f on error
(define (%get/char16 bv pos)
  (let ((x (%get/u16 bv pos)))
    (cond
      ((or (fx<=? x #xD7FF) (fx>=? x #xE000))
        (integer->char x))
      ((fx<=? #xDC80 x #xDCFF)
        (integer->char* x))
      (else
        #f))))

;; return one value: char deserialized from 3 bytes, or #f on error
(define (%get/char24 bv pos)
  (let ((x (%get/u24 bv pos)))
    (cond
      ((or (fx<=? x #xD7FF) (fx<=? #xE000 x #x10FFFF))
        (integer->char x))
      ((fx<=? #xDC80 x #xDCFF)
        (integer->char* x))
      (else
        #f))))


;; return two values:
;;   char deserialized from 1 byte and updated pos
(define (get/char8 bv pos end)
  ;; caller is (get/any) or similar, and guarantees that (fx<? pos end)
  (values (%get/char8 bv pos) (fx1+ pos)))

;; return two values:
;;   char deserialized from 2 bytes and updated pos,
;;   or (values #f #f) on error
(define (get/char16 bv pos end)
  (if (fx>=? (fx- end pos) 2)
    (let ((ch (%get/char16 bv pos)))
      (values ch (if ch (fx+ pos 2) #f)))
    (values #f #f)))

;; return two values:
;;   char deserialized from 3 bytes and updated pos,
;;   or (values #f #f) on error
(define (get/char24 bv pos end)
  (if (fx>=? (fx- end pos) 3)
    (let ((ch (%get/char24 bv pos)))
      (values ch (if ch (fx+ pos 3) #f)))
    (values #f #f)))

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


(define (get/list-impl bv pos end n ret)
  (cond
    ((or (not pos) (fx>? n (fx- end pos)))
      (values #f #f))
    ((fxzero? n)
      (values ret pos))
    (else
      (let-values (((elem pos) (get/any bv pos end)))
        (get/list-impl bv pos end (fx1- n) (cons elem ret))))))

(define (get/list* bv pos end)
  (let*-values (((n pos)   (get/u32-fixnum bv pos end))
                ((ret pos) (get/list-impl bv pos end n '())))
    (if (and ret pos)
      (values (list-reverse*! ret) pos)
      (values #f #f))))

(define (get/list bv pos end)
  (let*-values (((n pos)   (get/u32-fixnum bv pos end))
                ((ret pos) (get/list-impl bv pos end n '())))
    (if (and ret pos)
      (values (reverse! ret) pos)
      (values #f #f))))


(define (get/string8 bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (let ((ret (make-string n)))
        (do ((i 0 (fx1+ i)) (pos pos (fx1+ pos)))
            ((fx>=? i n)
              (values ret pos))
          (string-set! ret i (%get/char8 bv pos))))
      (values #f #f))))

(define (get/string16 bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (let ((bytes-per-char 2))
      (if (and pos (fx<=? (fx* n bytes-per-char) (fx- end pos)))
        (let %get-string16 ((i 0) (pos pos) (ret (make-string n)))
          (if (and pos (fx<? i n))
            (let ((ch (%get/char16 bv pos)))
              (if ch
                (begin
                  (string-set! ret i ch)
                  (%get-string16 (fx1+ i) (fx+ pos bytes-per-char) ret))
                (values #f #f)))
            (values (if pos ret #f) pos)))
        (values #f #f)))))

(define (get/string24 bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (let ((bytes-per-char 3))
      (if (and pos (fx<=? (fx* n bytes-per-char) (fx- end pos)))
        (let %get-string24 ((i 0) (pos pos) (ret (make-string n)))
          (if (and pos (fx<? i n))
            (let ((ch (%get/char24 bv pos)))
              (if ch
                (begin
                  (string-set! ret i ch)
                  (%get-string24 (fx1+ i) (fx+ pos bytes-per-char) ret))
                (values #f #f)))
            (values (if pos ret #f) pos)))
        (values #f #f)))))


(define (get/symbol8 bv pos end)
  (let-values (((str pos) (get/string8 bv pos end)))
    (values (if pos (string->symbol str) #f) pos)))

(define (get/symbol16 bv pos end)
  (let-values (((str pos) (get/string16 bv pos end)))
    (values (if pos (string->symbol str) #f) pos)))

(define (get/symbol24 bv pos end)
  (let-values (((str pos) (get/string24 bv pos end)))
    (values (if pos (string->symbol str) #f) pos)))


(define known-cmp-proc  (hashtable-transpose known-cmp-sym (make-eq-hashtable)))
(define known-hash-proc (hashtable-transpose known-hash-sym (make-eq-hashtable)))

(define (cmp-sym->proc sym)  (hashtable-ref known-cmp-proc sym #f))
(define (hash-sym->proc sym) (hashtable-ref known-hash-proc sym #f))

(define (%fill/hashtable bv pos end key-hash-validator key-cmp-validator n ret)
  (if (fxzero? n)
    (values ret pos)
    (let*-values (((key pos) (get/any bv pos end))
                  ((val pos) (get/any bv pos end)))
      (if (and pos (key-hash-validator key) (key-cmp-validator key))
        (begin
          (hashtable-set! ret key val)
          (%fill/hashtable bv pos end key-hash-validator key-cmp-validator (fx1- n) ret))
        (values #f #f)))))

(define (get/eq-hashtable bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (%fill/hashtable bv pos end always-true always-true n (make-eq-hashtable n))
      (values #f #f))))

(define (get/eqv-hashtable bv pos end)
  (let-values (((n pos) (get/vlen bv pos end)))
    (if (and pos (fx<=? n (fx- end pos)))
      (%fill/hashtable bv pos end always-true always-true n (make-eqv-hashtable n))
      (values #f #f))))

(define (get/hashtable bv pos end)
  (let*-values (((hash-sym pos) (get/any bv pos end))
                ((cmp-sym pos)  (get/any bv pos end))
                ((n pos)        (get/vlen bv pos end)))
    ;; (debugf "...get/hashtable pos=~s n=~s hash-sym=~s cmp-sym=~s" pos n hash-sym cmp-sym)
    (if (and pos (fx<=? n (fx- end pos)) (symbol? hash-sym) (symbol? cmp-sym))
      (let ((hash-proc (hash-sym->proc hash-sym))
            (cmp-proc  (cmp-sym->proc cmp-sym))
            (key-hash-validator (hashtable-ref known-hash-key-type-validator hash-sym #f))
            (key-cmp-validator  (hashtable-ref known-cmp-key-type-validator cmp-sym #f)))
        ;; (debugf "...get/hashtable hash-proc=~s cmp-proc=~s" hash-proc cmp-proc)
        (if (and hash-proc cmp-proc key-hash-validator key-cmp-validator)
          (%fill/hashtable bv pos end key-hash-validator key-cmp-validator n
                           (make-hashtable hash-proc cmp-proc n))
          (values #f #f)))
      (values #f #f))))


(define known-tag
  (let ((plist
          (list tag-0 0 tag-1 1 tag-2 2 tag-3 3 tag-4 4 tag-5 5 tag-6 6 tag-7 7 tag-8 8
                tag-9 9 tag-10 10 tag--5 -5 tag--4 -4 tag--3 -3 tag--2 -2 tag--1 -1
                tag-s8 get/exact-s8 tag-s16 get/exact-s16 tag-s24 get/exact-s24 tag-s32 get/exact-s32
                tag-sint get/exact-sint tag-ratio get/ratio tag-complex get/complex
                tag-flonum get/flonum tag-cflonum get/cflonum
                tag-f #f tag-t #t tag-nil '() tag-void (void) tag-eof (eof-object) tag-bwp (bwp-object)
                tag-char8 get/char8 tag-char16 get/char16 tag-char24 get/char24 tag-box get/box
                tag-pair get/pair tag-list1 get/list1 tag-list* get/list* tag-list get/list
                tag-vector   get/vector   tag-bytevector get/bytevector
                tag-string8  get/string8  tag-string16   get/string16   tag-string24 get/string24
                tag-fxvector get/fxvector tag-flvector   get/flvector
                tag-symbol8  get/symbol8  tag-symbol16   get/symbol16   tag-symbol24 get/symbol24
                tag-eq-hashtable get/eq-hashtable tag-eqv-hashtable get/eqv-hashtable tag-hashtable get/hashtable))
        (vec (make-vector 256 (void))))
    (for-plist ((tag obj plist))
      (vector-set! vec tag obj))
    (for-hash ((sym tag known-sym))
      (vector-set! vec tag sym))
    vec))

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
  (if (and (fixnum? pos) (fx<? pos end))
    (let ((obj (tag->obj (%get/tag bv pos)))
          (pos (tag+ pos)))
      (if (procedure? obj)
        (obj bv pos end)
        (values obj pos)))
    (values #f #f)))



;; read byte range [start, end) from bytevector bv and deserialize an object from it.
;; Return two values:
;;   either object and updated start position in range [start, end)
;;   or #f #f if serialized bytes are invalid and cannot be parsed;
;;   or #f -NNN if not enough bytes are available and at least NNN bytes should be added after end;
;;   or #t -NNN if serialized bytes are invalid and NNN bytes should be discarded.
(define (wire-get bv start end)
  (assert* 'wire-get (fx<=?* 0 start end (bytevector-length bv)))
  (let-values (((len pos) (get/vlen bv start end)))
    (cond
      ((and len pos)
        (let ((available (fx- end pos)))
          (if (fx>=? available len)
            (if (fxzero? len)
              (values (void) pos) ; (void) can be encoded as header = 0
              (let ((end0 (fx+ pos len)))
                (let-values (((ret end1) (get/any bv pos end)))
                  (if (and end1 (fx=? end0 end1))
                    (values ret end1)
                    ;; message deserialized, but it ends at unexpected position:
                    ;; discard it, and tell how many bytes should be discarded.
                    (values #t (fx- (fx+ len pos)))))))
            ;; not enough bytes to deserialize message: tell how many more bytes are needed
            (values #f (fx- available len)))))
      ((fx>=? start end)
        ;; zero bytes provided, need at least min-len-vlen
        (values #f (fx- min-len-vlen)))
      (len
        ;; not enough bytes to deserialize message: tell how many more bytes are needed
        (values #f (fx- (fx- end start) (vlen+ len len))))
      (else
        ;; could not deserialize vlen
        (values #f #f)))))


;; read bytes from bytevector or bytespan stc and deserialize an object from them.
;; Return two values:
;;   either object and number of consumed bytes,
;;   or #f #f if serialized bytes are invalid
;;   or #f -NNN if not enough bytes are available and at least NNN bytes should be added to the bytespan end.
;;
;; raises exception if src is not a bytevector or a bytespan,
;; or if start and end are out-of-range.
(define wire->datum
  (case-lambda
    ((src)
      (if (bytevector? src)
        (wire-get src 0 (bytevector-length src))
        (wire-get (bytespan-peek-data src) (bytespan-peek-beg src) (bytespan-peek-end src))))
    ((src start end)
      (if (bytevector? src)
        (let ((len (bytevector-length src)))
          (assert* 'wire->datum (fx<=?* 0 start end len))
          (wire-get src start end))
        (begin
          (assert* 'wire->datum (bytespan? src))
          (let ((len (bytevector-length src))
                (offset (bytespan-peek-beg src)))
            (assert* 'wire->datum (fx<=?* 0 start end len))
            (wire-get (bytespan-peek-data src) (fx+ start offset) (fx+ end offset))))))))
