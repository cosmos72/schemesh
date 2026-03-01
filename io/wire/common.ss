;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/wire/wire.ss

(define (symbol->tag sym)
  (hashtable-ref known-sym sym #f))


(define (valid-payload-len? n)
  (and (fixnum? n) (fx<=? 0 n max-len-payload)))

(define u32+
  (case-lambda
    ((pos)
      (and (fixnum? pos) (fx+ pos 4)))
    ((pos n)
      (and (fixnum? pos) (fixnum? n) (fx+ (fx+ pos n) 4)))))

(define tag+
  (case-lambda
    ((pos)
      (and (fixnum? pos) (fx+ pos len-tag)))
    ((pos n)
      (and (fixnum? pos) (fixnum? n) (fx+ (fx+ pos n) len-tag)))))

;; write one signed byte into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s8 bv pos s8)
  (bytevector-s8-set! bv pos s8)
  (fx1+ pos))

;; write one byte into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u8 bv pos u8)
  (bytevector-u8-set! bv pos u8)
  (fx1+ pos))

;; write exact signed integer as 2 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s16 bv pos s16)
  (bytevector-s16-set! bv pos s16 endian)
  (fx+ pos 2))

;; write exact unsigned integer as 2 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u16 bv pos u16)
  (bytevector-u16-set! bv pos u16 endian)
  (fx+ pos 2))

;; write exact unsigned integer as 3 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s24 bv pos s24)
  (bytevector-s24-set! bv pos s24 endian)
  (fx+ pos 3))

;; write exact unsigned integer as 3 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u24 bv pos u24)
  (bytevector-u24-set! bv pos u24 endian)
  (fx+ pos 3))

;; write exact signed integer as 4 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/s32 bv pos s32)
  (bytevector-s32-set! bv pos s32 endian)
  (fx+ pos 4))

;; write exact unsigned integer as 4 bytes into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u32 bv pos u32)
  (bytevector-u32-set! bv pos u32 endian)
  (fx+ pos 4))


;; read one signed byte from bytevector at position pos, and return it
(define %get/s8 bytevector-s8-ref)
;; read one byte from bytevector at position pos, and return it
(define %get/u8 bytevector-u8-ref)

;; read 2 bytes as exact signed integer from bytevector starting at position pos, and return it.
(define (%get/s16 bv pos) (bytevector-s16-ref bv pos endian))
;; read exact unsigned integer as 2 bytes from bytevector starting at position pos, and return it.
(define (%get/u16 bv pos) (bytevector-u16-ref bv pos endian))

;; read 3 bytes as exact signed integer from bytevector starting at position pos, and return it.
(define (%get/s24 bv pos) (bytevector-s24-ref bv pos endian))
;; read 3 bytes as exact unsigned integer from bytevector starting at position pos, and return it.
(define (%get/u24 bv pos) (bytevector-u24-ref bv pos endian))

;; read 4 bytes as exact signed integer from bytevector starting at position pos, and return it.
(define (%get/s32 bv pos) (bytevector-s32-ref bv pos endian))
;; read 4 bytes as exact unsigned integer from bytevector starting at position pos, and return it.
(define (%get/u32 bv pos) (bytevector-u32-ref bv pos endian))


(define (get/u8 bv pos end)
  (if (and (fixnum? pos) (fx<? pos end))
    (values (%get/u8 bv pos) (fx1+ pos))
    (values #f #f)))


;; read 1-byte tag from bytevector at position pos, and return it.
;; raise exception on errors.
(define %get/tag %get/u8)

;; write 1-byte tag into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define put/tag put/u8)


;; write unsigned fixnum, limited to 32 bits, into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/u32-fixnum bv pos n)
  (meta-cond
    ((fixnum? #xFFFFFFFF)
      (assert* 'put/u32-fixnum (fx<=? n #xFFFFFFFF))))
  (bytevector-u32-set! bv pos n endian)
  (fx+ pos 4))

;; read unsigned fixnum, limited to 32 bits, from bytevector starting at position pos.
;; return two values: unsigned fixnum, and updated pos
;; or #f #f on errors.
(define (get/u32-fixnum bv pos end)
  (let ((u32 (and (fixnum? pos) (fx<=? pos (fx- end 4)) (%get/u32 bv pos))))
    (if (fixnum? u32)
      (values u32 (fx+ pos 4))
      (values #f #f))))


(define (%vlen+ vlen pos)
  (cond
    ((fx<=? vlen #x7f)     (fx+ pos min-len-vlen))
    ((fx<=? vlen max-vlen) (fx+ pos max-len-vlen))
    (else                  #f)))


;; add space occupied by unsigned fixnum vlen, which is encoded as a variable number of bytes:
;;    0 ... #x7f       are encoded as u8
;; #x80 ... #x7fffffff are encoded as u32 where first byte has top bit set
;; larger values are NOT supported and cause serialization to fail
(define vlen+
  (case-lambda
    ((vlen pos)
      (and (fixnum? pos) (%vlen+ vlen pos)))
    ((vlen pos n)
      (and (fixnum? pos) (fixnum? n) (%vlen+ vlen (fx+ pos n))))))


;; subtract space occupied by unsigned fixnum vlen, which is encoded as a variable number of bytes:
;;    0 ... #x7f       are encoded as u8
;; #x80 ... #x7fffffff are encoded as u32 where first byte has top bit set
;; larger values are NOT supported and cause serialization to fail
(define (vlen- message-wire-len)
  (and
    (fixnum? message-wire-len)
    (fx<=? (fx- message-wire-len max-len-vlen) max-vlen)
    (begin
      (assert* 'wire-put (fx>=? message-wire-len min-len-vlen))
      (if (fx<=? message-wire-len #x80)
        (fx- message-wire-len min-len-vlen)     ;; small message, vlen is encoded as u8
        (fx- message-wire-len max-len-vlen))))) ;; large message, vlen is encoded as u32


;; write unsigned fixnum vlen bytevector starting at position pos.
;; return updated position, or #f on errors.
(define (put/vlen bv pos vlen)
  (cond
    ((or (not pos) (not (fixnum? vlen)) (fx<? vlen 0))
      #f)
    ((fx<=? vlen #x7f)
      (put/u8 bv pos vlen))
    ((fx<=? vlen max-vlen)
      (meta-cond
        ((fixnum? #xffffffff)
          (let ((lo  (fxand vlen #x7f))
                (hi  (fxand (fxsll vlen 1) #xffffff00)))
            (put/u32 bv pos (fxior hi #x80 lo))))
        (else
          (let ((lo  (fxand vlen #x7f))
                (hi  (bitwise-and (bitwise-arithmetic-shift-left vlen 1) #xffffff00)))
            (put/u32 bv pos (bitwise-ior hi (fxior #x80 lo)))))))
    (else
      #f)))


(define (len/exact-sint pos obj)
  (cond
    ((not pos)
      pos)
    ((and (fixnum? obj) (fx<=? #x-800000 obj #x7fffff))
      (cond
        ((fx<=? -5 obj 10)
          (tag+ pos)) ; only tag, datum is 0 bytes
        ((fx<=? #x-80 obj #x7f)
          (tag+ pos 1)) ; 1-byte datum
        ((fx<=? #x-8000 obj #x7fff)
          (tag+ pos 2)) ; 2-byte datum
        (else
          (tag+ pos 3)))) ; 3-byte datum
    ((<= #x-80000000 obj #x7fffffff)
      (tag+ pos 4)) ; 4-byte datum
    (else
      ;; datum is exact integer, sign-extended n bytes, two's complement little-endian
      (let ((n (fx1+ (fxsrl (integer-length obj) 3))))
        (vlen+ n (tag+ pos) n)))))


;; write tag and exact integer into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/exact-sint bv pos obj)
  (cond
    ((not pos)
      pos)
    ((and (fixnum? obj) (fx<=? #x-800000 obj #x7fffff))
      (cond
        ((fx<=? 0 obj 10)
          (put/tag bv pos (fx+ tag-0 obj))) ; only tag, datum is 0 bytes
        ((fx<=? -5 obj -1)
          (put/tag bv pos (fx+ tag--1 (fx1+ obj)))) ; only tag, datum is 0 bytes
        ((fx<=? #x-80 obj #x7f)
          (let ((pos (put/tag bv pos tag-s8)))
            (put/s8 bv pos obj)))   ; 1-byte datum
        ((fx<=? #x-8000 obj #x7fff)
          (let ((pos (put/tag bv pos tag-s16)))
            (put/s16 bv pos obj)))  ; 2-byte datum
        (else
          (let ((pos (put/tag bv pos tag-s24)))
            (put/s24 bv pos obj)))))  ; 3-byte datum
    ((<= #x-80000000 obj #x7fffffff)
      (let ((pos (put/tag bv pos tag-s32)))
        (put/s32 bv pos obj)))  ; 4-byte datum
    (else
      ;; datum is exact integer, sign-extended, two's complement little-endian
      (let* ((datum-byte-n (fx1+ (fxsrl (integer-length obj) 3)))
             (pos (put/tag bv pos tag-sint))
             (pos (put/vlen bv pos datum-byte-n)))
        (bytevector-sint-set*! bv pos obj endian datum-byte-n)
        (fx+ pos datum-byte-n)))))


(define (len/flonum pos obj)
  (tag+ pos len-flonum))

(define (put/flonum bv pos obj)
  (let ((pos (put/tag bv pos tag-flonum)))
    (bytevector-ieee-double-set! bv pos obj endian)
    (fx+ pos len-flonum)))


(define (len/cflonum pos obj)
  (tag+ pos len-cflonum))

(define (put/cflonum bv pos obj)
  (let ((pos (put/tag bv pos tag-cflonum)))
    (bytevector-ieee-double-set! bv pos (real-part obj) endian)
    (let ((pos (fx+ pos len-flonum)))
      (bytevector-ieee-double-set! bv pos (imag-part obj) endian)
      (fx+ pos len-flonum))))


(define (len/number pos obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (len/exact-sint pos obj)
        ;; exact ratio: encoded as tag, numerator, denominator
        (len/exact-sint
          (len/exact-sint (tag+ pos) (numerator obj))
          (denominator obj)))
      ;; exact complex: encoded as tag, real-part, imag-part
      (len/number
        (len/number (tag+ pos) (real-part obj))
        (imag-part obj)))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (len/flonum pos obj)
      (len/cflonum pos obj))))


(define (put/number bv pos obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (put/exact-sint bv pos obj)
        ;; exact ratio: encoded as tag, (numerator, denominator)
        (let* ((end0 (put/tag bv pos tag-ratio))
               (end1 (put/exact-sint bv end0 (numerator obj)))
               (end2 (put/exact-sint bv end1 (denominator obj))))
          end2))
      ;; exact complex: encoded as tag, (real-part, imag-part)
      (let* ((end0 (put/tag bv pos tag-complex))
             (end1 (put/number bv end0 (real-part obj)))
             (end2 (put/number bv end1 (imag-part obj))))
        end2))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (put/flonum bv pos obj)
      (put/cflonum bv pos obj))))


(define (len/char pos obj)
  (tag+ pos
    (cond ((char<=? obj #\xFF)   1)
          ((char<=? obj #\xFFFF) 2)
          (else                  3))))

;; write tag and one character into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/char bv pos obj)
  (cond
    ((char<=? obj #\xFF)
       (let ((pos (put/tag bv pos tag-char8)))
         (put/u8 bv pos (char->integer obj))))
    ((char<=? obj #\xFFFF)
       (let ((pos (put/tag bv pos tag-char16)))
         (put/u16 bv pos (char->integer obj))))
    (else
       (let ((pos (put/tag bv pos tag-char24)))
         (put/u24 bv pos (char->integer obj))))))


(define (len/box pos obj)
  (tag+
    (len/any pos (unbox obj))))

(define (put/box bv pos obj)
  (let* ((pos (put/tag bv pos tag-box))
         (pos (put/any bv pos (unbox obj))))
    pos))


;; 1-element proper list
(define (len/list1 pos obj)
  (tag+ (len/any pos (car obj))))

;; 1-element proper list
(define (put/list1 bv pos obj)
  (let* ((end0 (put/tag bv pos tag-list1))
         (end1 (put/any bv end0 (car obj))))
    end1))

;; proper or improper list
(define (len/list pos obj)
  (let %len/list ((pos (u32+ (tag+ pos))) (l obj)) ; n is encoded as u32
    (cond
      ((not pos)
        pos)
      ((null? l)
        pos)
      ((pair? l)
        (%len/list (len/any pos (car l)) (cdr l)))
      (else
        ;; last element of an improper list
        (len/any pos l)))))


(define (put/list bv pos obj)
  (let ((end0 (tag+ pos)))
    (let %put/list ((end (u32+ end0)) (n 0) (l obj)) ; n is encoded as u32
      ;; (debugf "%put-list l=~s n=~s end=~s" l n end)
      (cond
        ((not end)
          end)
        ((null? l)
          (put/tag bv pos tag-list)
          (put/u32-fixnum bv end0 n) ; n is encoded as u32
          end)
        ((pair? l)
          (let ((end (put/any bv end (car l))))
            (%put/list end (fx1+ n) (cdr l))))
        (else
          ;; last element of an improper list
          (let ((end (put/any bv end l)))
            (put/tag bv pos tag-list*)
            (put/u32-fixnum bv end0 (fx1+ n)) ; n is encoded as u32
            end))))))


(define (len/pair pos obj)
  (let ((tail (cdr obj)))
    (cond
      ((null? tail)
        (len/list1 pos obj))
      ((pair? tail)
        (len/list pos obj))
      (else
        (tag+
          (len/any (len/any pos (car obj))
                   tail))))))


(define (put/pair bv pos obj)
  (let ((tail (cdr obj)))
    (cond
      ((null? tail)
        (put/list1 bv pos obj))
      ((pair? tail)
        (put/list bv pos obj))
      (else
        (let* ((end0 (put/tag bv pos tag-pair))
               (end1 (put/any bv end0 (car obj)))
               (end2 (put/any bv end1 (cdr obj))))
          end2)))))

;; return the higher-numbered character between ch1 and ch2
(define (char-max ch1 ch2)
  (if (char<? ch1 ch2) ch2 ch1))

;; return the number of bytes needed to serialize given character: either 1, 2 or 3
(define (char-len ch)
  (cond ((char<=? ch #\xFF)   1)
        ((char<=? ch #\xFFFF) 2)
        (else                 3)))

;; return the number of bytes needed to serialize
;; the highest-numbered character in string: either 1, 2 or 3
(define (bytes-per-char/string s n)
  (let %again ((i 0) (max-ch #\x0))
    (if (and (fx<? i n) (char<=? max-ch #\xFFFF))
      (%again (fx1+ i) (char-max max-ch (string-ref s i)))
      (char-len max-ch))))

(define (len/string pos obj)
  (let* ((n (string-length obj))
         (bytes-per-char (bytes-per-char/string obj n)))
    (vlen+ n ;; n is encoded as vlen
      (tag+ pos (fx* n bytes-per-char))))) ;; each character is encoded as bytes-per-char bytes

(define (put/string bv pos obj)
  (let* ((n (string-length obj))
         (bytes-per-char (bytes-per-char/string obj n))
         (end0 (put/tag  bv pos (vector-ref tags-string (fx1- bytes-per-char))))
         (end1 (put/vlen bv end0 n))) ; n is encoded as vlen
    (do ((i 0 (fx1+ i))
         (pos end1 (fx+ pos bytes-per-char)))
        ((fx>=? i n)
           pos)
        (let ((ch-int (char->integer (string-ref obj i))))
          (case bytes-per-char
            ((1)  (put/u8  bv pos ch-int))
            ((2)  (put/u16 bv pos ch-int))
            (else (put/u24 bv pos ch-int)))))))


(define (len/symbol pos obj)
  (if (symbol->tag obj)
    (tag+ pos)
    (len/string pos (symbol->string obj))))


(define (put/symbol bv pos obj)
  (let ((tag (symbol->tag obj)))
    (if tag
      (put/tag bv pos tag)
      (let* ((end (put/string bv pos (symbol->string obj)))
             (old-tag (%get/tag bv pos))
             (new-tag (fx+ old-tag (fx- tag-symbol8 tag-string8))))
        (put/tag bv pos new-tag)
        end))))

(define (htable->cmp-sym obj)
  (hashtable-ref known-cmp-sym (hashtable-equivalence-function obj) #f))

(define (htable->hash-sym obj)
  (hashtable-ref known-hash-sym (hashtable-hash-function obj) #f))

(define (len/hashtable pos obj)
  (let ((cmp-sym  (htable->cmp-sym  obj))
        (hash-sym (htable->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (tag+ pos))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (len/symbol pos hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (len/symbol pos cmp-sym))))
             (pos (vlen+ (hashtable-size obj) pos))) ; n is encoded as vlen
        (when pos
          (for-hash ((k v obj))
            (set! pos (len/any (len/any pos k) v))))
        pos)
      #f)))

(define (put/hashtable bv pos obj)
  (let ((cmp-sym  (htable->cmp-sym  obj))
        (hash-sym (htable->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (put/tag bv pos (case cmp-sym ((eq?)  tag-eq-hashtable)
                                                ((eqv?) tag-eqv-hashtable)
                                                (else   tag-hashtable))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bv pos hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bv pos cmp-sym))))
             (pos (put/vlen bv pos (hashtable-size obj)))) ; n is encoded as vlen
        (when pos
          (for-hash ((k v obj))
            (set! pos (put/any bv (put/any bv pos k) v))))
        pos)
      #f)))


(define (ord-hash->cmp-sym obj)
  (hashtable-ref known-cmp-sym (ordered-hash-equivalence-function obj) #f))

(define (ord-hash->hash-sym obj)
  (hashtable-ref known-hash-sym (ordered-hash-hash-function obj) #f))

(define (len/ord-hash pos obj)
  (let ((cmp-sym  (ord-hash->cmp-sym  obj))
        (hash-sym (ord-hash->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (tag+ pos))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (len/symbol pos hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (len/symbol pos cmp-sym))))
             (pos (vlen+ (ordered-hash-size obj) pos))) ; n is encoded as vlen
        (when pos
          (for-ordered-hash ((k v obj))
            (set! pos (len/any (len/any pos k) v))))
        pos)
      #f)))

(define (put/ord-hash bv pos obj)
  (let ((cmp-sym  (ord-hash->cmp-sym  obj))
        (hash-sym (ord-hash->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (put/tag bv pos (case cmp-sym ((eq?)  tag-eq-ord-hash)
                                                ((eqv?) tag-eqv-ord-hash)
                                                (else   tag-ord-hash))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bv pos hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bv pos cmp-sym))))
             (pos (put/vlen bv pos (ordered-hash-size obj)))) ; n is encoded as vlen
        (when pos
          (for-ordered-hash ((k v obj))
            (set! pos (put/any bv (put/any bv pos k) v))))
        pos)
      #f)))


;; recursively traverse obj and return the number of bytes needed to serialize obj
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (len/any pos obj)
  (cond
    ((not pos)           #f)
    ((fixnum? obj)       (len/exact-sint  pos obj))
    ((char?   obj)       (len/char       pos obj))
    ((pair?   obj)       (len/pair       pos obj))
    ((symbol? obj)       (len/symbol     pos obj))
    ((flonum? obj)       (len/flonum     pos obj))
    ((null? obj)         (tag+ pos)) ; only tag, datum is 0 bytes
    ((eq? (void) obj)    (tag+ pos)) ; only tag, datum is 0 bytes
    ((boolean? obj)      (tag+ pos)) ; only tag, datum is 0 bytes
    ((eof-object? obj)   (tag+ pos)) ; only tag, datum is 0 bytes
    ((bwp-object? obj)   (tag+ pos)) ; only tag, datum is 0 bytes
    ((procedure? obj)  #f)
    ;; these are slower to check
    ((box?    obj)       (len/box        pos obj))
    ((number? obj)       (len/number     pos obj))
    ((vector? obj)       (len/vector     pos obj))
    ((bytevector? obj)   (len/bytevector pos obj))
    ((string? obj)       (len/string     pos obj))
    ((hashtable? obj)    (len/hashtable  pos obj))
    ((ordered-hash? obj) (len/ord-hash   pos obj))
    ((fxvector? obj)     (len/fxvector   pos obj))
    ((flvector? obj)     (len/flvector   pos obj))
    ((record? obj)       (len/record     pos obj))
    (else                #f)))


;; recursively traverse obj, serialize it and write it into bytevector bv starting at position pos.
;; return updated position, or #f on errors.
(define (put/any bv pos obj)
  (cond
    ((not pos)           #f)
    ((fixnum? obj)       (put/exact-sint  bv pos obj))
    ((char?   obj)       (put/char       bv pos obj))
    ((pair?   obj)       (put/pair       bv pos obj))
    ((symbol? obj)       (put/symbol     bv pos obj))
    ((flonum? obj)       (put/flonum     bv pos obj))
    ((null? obj)         (put/tag bv pos tag-nil))  ; only tag, datum is 0 bytes
    ((boolean? obj)      (put/tag bv pos (if obj tag-t tag-f))) ; only tag, datum is 0 bytes
    ((eq? (void) obj)    (put/tag bv pos tag-void)) ; only tag, datum is 0 bytes
    ((eof-object? obj)   (put/tag bv pos tag-eof))  ; only tag, datum is 0 bytes
    ((bwp-object? obj)   (put/tag bv pos tag-bwp))  ; only tag, datum is 0 bytes
    ((procedure? obj)    #f)
    ;; these are slower to check
    ((box?    obj)       (put/box        bv pos obj))
    ((number? obj)       (put/number     bv pos obj))
    ((vector? obj)       (put/vector     bv pos obj))
    ((bytevector? obj)   (put/bytevector bv pos obj))
    ((string? obj)       (put/string     bv pos obj))
    ((hashtable? obj)    (put/hashtable  bv pos obj))
    ((ordered-hash? obj) (put/ord-hash   bv pos obj))
    ((fxvector? obj)     (put/fxvector   bv pos obj))
    ((flvector? obj)     (put/flvector   bv pos obj))
    ((record? obj)       (put/record     bv pos obj))
    (else                #f)))


;; recursively traverse obj and return the number of bytes needed to serialize it.
;; Return #f if obj contains some datum that cannot be serialized: procedures, unregistered record-types, etc.
(define (datum->wire-length obj)
  (if (eq? (void) obj)
    (vlen+ 0 0)
    (let ((pos (len/any 0 obj)))
      (and (valid-payload-len? pos) (vlen+ pos pos)))))


;; append wire magic bytes to bytespan
(define (wire-put-magic-to-bytespan bsp)
  (bytespan-insert-right/bytevector! bsp wire-magic-bytes))


;; read magic bytes.
;; return #t -NNN if they match, instructing caller that NNN bytes should be ignored,
;; or raise condition if they don't match, because it means the other side uses an incompatible wire protocol.
(define (check-magic bv pos end)
  (let* ((start (fx1- pos))
         (n     (fx- end start)))
    (if (and (fx=? n (bytevector-length wire-magic-bytes))
             (subbytevector=? bv start wire-magic-bytes 0 n))
      (values #t (fx- n))
      (raise-errorf 'wire-get-from-bytevector "incompatible wire protocol ~s, expecting ~s"
        (bytevector->bytespan* bv start end) (bytevector->bytespan* wire-magic-bytes)))))


;; recursively traverse obj, serialize it and append it to bytespan bsp.
;; return number of written bytes, or #f on errors.
(define wire-put-to-bytespan
  (case-lambda
    ((bsp obj message-wire-len)
      (assert* 'wire-put (bytespan? bsp))
      (when message-wire-len
        (assert* 'wire-put (fixnum? message-wire-len)))
      (let ((payload-wire-len (vlen- message-wire-len))
            (len-before       (bytespan-length bsp)))
        (if (valid-payload-len? payload-wire-len)
          (let ((len-after (fx+ len-before message-wire-len)))
            (bytespan-reserve-right! bsp len-after)
            (let* ((bv   (bytespan-peek-data bsp))
                   (pos  (fx+ len-before (bytespan-peek-beg bsp)))
                   (end0 (put/vlen bv pos payload-wire-len))
                   (end  (if (eq? (void) obj)
                            end0
                            (put/any bv end0 obj))))
              (assert* 'wire-put (fx=? (fx- end pos) message-wire-len))
              (bytespan-resize-right! bsp len-after)
              message-wire-len))
          #f)))
    ((bsp obj)
      (wire-put-to-bytespan bsp obj (datum->wire-length obj)))))

;; recursively traverse obj, serialize it and return bytevector containing serialized bytes,
;; or #f on errors
(define (datum->wire obj)
  (let ((bsp (bytespan)))
    (if (wire-put-to-bytespan bsp obj)
      (bytespan->bytevector*! bsp)
      #f)))


