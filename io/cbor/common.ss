;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/cbor/cbor.ss


(define (grow bsp n)
  (let ((len (bytespan-length bsp)))
    (bytespan-resize-right! bsp (fx+ len n))
    (fx+ len (bytespan-peek-beg bsp))))

(define (bytes bsp)
  (bytespan-peek-data bsp))

(define (put/u8 bsp u8)
  (let ((pos (grow bsp 1)))
    (bytevector-u8-set! (bytes bsp) pos u8)))

(define (put/u16 bsp u16)
  (let ((pos (grow bsp 2)))
    (bytevector-u16-set! (bytes bsp) pos u16 endian)))

(define (put/u32 bsp u32)
  (let ((pos (grow bsp 4)))
    (bytevector-u32-set! (bytes bsp) pos u32 endian)))

(define (put/u64 bsp u64)
  (let ((pos (grow bsp 8)))
    (bytevector-u64-set! (bytes bsp) pos u64 endian)))

#|
(define (get/u8 bsp)
  (values (bytevector-u8-ref bv pos)
          (fx1+ pos)))

(define (get/u16 bv pos)
  (values (bytevector-u16-ref bv pos endian)
          (fx+ pos 2)))

(define (get/u32 bv pos)
  (values (bytevector-u32-ref bv pos endian)
          (fx+ pos 4)))

(define (get/u64 bv pos u64)
  (values (bytevector-u64-ref bv pos endian)
          (fx+ pos 8)))
|#


(define (put/small-fixnum bsp obj)
  (if (fx>=? obj 0)
    (cond
      ((fx<=? obj 23)
        (put/u8 bsp (fxior major-unsigned obj)))
      ((fx<=? obj 255)
        (put/u8 bsp (fxior major-unsigned minor-1byte))
        (put/u8 bsp obj))
      (else
        (put/u8 bsp (fxior major-unsigned minor-2byte))
        (put/u16 bsp obj)))
    (let ((obj (fxnot obj)))
      (cond
        ((fx<=? obj 23)
          (put/u8 bsp (fxior major-negative obj)))
        ((fx<=? obj 255)
          (put/u8 bsp (fxior major-negative minor-1byte))
          (put/u8 bsp obj))
        (else
          (put/u8 bsp (fxior major-negative minor-2byte))
          (put/u16 bsp obj))))))


(define (%put/bignum bsp obj tag-major-minor)
  (let ((byte-n (fxsra (fx+ (integer-length obj) 7) 3)))
    (put/u8 bsp tag-major-minor)
    (put/count bsp major-bytestring byte-n)
    (let ((pos (grow bsp byte-n)))
      (bytevector-uint-set*! (bytes bsp) pos obj endian byte-n))))


(define (put/large-unsigned bsp obj)
  (cond
    ((<= obj #xFFFFFFFF)
      (put/u8 bsp (fxior major-unsigned minor-4byte))
      (put/u32 bsp obj))
    ((<= obj #xFFFFFFFFFFFFFFFF)
      (put/u8 bsp (fxior major-unsigned minor-8byte))
      (put/u64 bsp obj))
    (else
      (%put/bignum bsp obj (fxior major-tag 2))))) ;; unsigned bignum


(define (put/large-negative bsp obj)
  (let ((obj (bitwise-not obj)))
    (cond
      ((<= obj #xFFFFFFFF)
        (put/u8 bsp (fxior major-negative minor-4byte))
        (put/u32 bsp obj))
      ((<= obj #xFFFFFFFFFFFFFFFF)
        (put/u8 bsp (fxior major-negative minor-8byte))
        (put/u64 bsp obj))
      (else
        (%put/bignum bsp obj (fxior major-tag 3)))))) ;; negative bignum


;; write an exact integer into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/integer bsp obj)
  (cond
    ((and (fixnum? obj) (fx<=? #x-10000 obj #xFFFF))
      (put/small-fixnum bsp obj))
    ((>= obj 0)
      (put/large-unsigned bsp obj))
    (else
      (put/large-negative bsp obj))))


(define (put/count bsp major n)
  (assert* 'cbor-writer (fixnum? n))
  (assert* 'cbor-writer (fx>=? n 0))
  (cond
    ((fx<=? n 23)
      (put/u8 bsp (fxior major n)))
    ((fx<=? n 255)
      (put/u8 bsp (fxior major minor-1byte))
      (put/u8 bsp n))
    ((fx<=? n #xFFFF)
      (put/u8 bsp (fxior major minor-2byte))
      (put/u16 bsp n))
    ((meta-cond ((fixnum? #xFFFFFFFF) (fx<=? n #xFFFFFFFF))
                 (else                #t))
      (put/u8 bsp (fxior major minor-4byte))
      (put/u32 bsp n))
    (else
      (put/u8 bsp (fxior major minor-8byte))
      (put/u64 bsp n))))


#|
(define (get/unsigned bsp end)
  (if (and (fixnum? pos) (fx<? pos end))
    (values 0 (fx1+ pos))
    (values #f #f)))

(define (get/negative bsp end)
  (if (and (fixnum? pos) (fx<? pos end))
    (values -1 (fx1+ pos))
    (values #f #f)))

;; read an exact integer from bytevector at position pos.
;; return two values: deserialized integer and updated position,
;; or (values #<unspecified> #f) on error
(define (get/integer bsp end)
  (if #t
    (get/unsigned bsp end)
    (get/negative bsp end)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; obj is an exact ratio
(define (put/ratio bsp obj)
  (put/count bsp major-tag tag-ratio)
  (put/count bsp major-array 2)
  (put/integer bsp (numerator obj))
  (put/integer bsp (denominator obj)))


;; obj is an exact or inexact complex
(define (put/complex bsp obj)
  (put/count bsp major-tag tag-complex)
  (put/count bsp major-array 2)
  (put/number bsp (real-part obj))
  (put/number bsp (imag-part obj)))


(define (put/flonum bsp obj)
  (put/u8 bsp (fxior major-misc minor-8byte))
  (let ((pos (grow bsp 8)))
    (bytevector-ieee-double-set! (bytes bsp) pos obj endian)))


#|
(define (get/flonum bsp obj)
  (values
    (bytevector-ieee-double-ref bsp endian)
    (fx+ pos len-flonum)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (put/number bsp obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        (put/integer bsp obj)
        (put/ratio bsp obj))
      (put/complex bsp obj))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (put/flonum bsp obj)
      (put/complex bsp obj))))


;; write tag and one character into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/char bsp obj)
  (put/count bsp major-tag tag-char)
  (put/integer bsp (char->integer obj)))


(define (put/box bsp obj)
  (put/count bsp major-tag tag-box)
  (put/any bsp (unbox obj)))

#|
;; 1-element proper list
(define (put/list1 bsp obj)
  (let* ((end0 (put/tag bsp tag-list1))
         (end1 (put/any bsp end0 (car obj))))
    end1))

(define (put/list bsp obj)
  (let ((end0 (tag+ pos)))
    (let %put/list ((end (u32+ end0)) (n 0) (l obj)) ; n is encoded as u32
      ;; (debugf "%put-list l=~s n=~s end=~s" l n end)
      (cond
        ((not end)
          end)
        ((null? l)
          (put/tag bsp tag-list)
          (put/u32-fixnum bsp end0 n) ; n is encoded as u32
          end)
        ((pair? l)
          (let ((end (put/any bsp end (car l))))
            (%put/list end (fx1+ n) (cdr l))))
        (else
          ;; last element of an improper list
          (let ((end (put/any bsp end l)))
            (put/tag bsp tag-list*)
            (put/u32-fixnum bsp end0 (fx1+ n)) ; n is encoded as u32
            end))))))
|#

(define (put/pair bsp obj)
  ;; TODO
  (void))
  #|
  (let ((tail (cdr obj)))
    (cond
      ((null? tail)
        (put/list1 bsp obj))
      ((pair? tail)
        (put/list bsp obj))
      (else
        (let* ((end0 (put/tag bsp tag-pair))
               (end1 (put/any bsp end0 (car obj)))
               (end2 (put/any bsp end1 (cdr obj))))
          end2))))
  |#


(define (put/symbol bsp obj)
  (put/count bsp major-tag tag-symbol)
  (let ((val (hashtable-ref known-sym obj #f)))
    (if val
      (put/small-fixnum bsp val)
      (put/string bsp (symbol->string obj)))))


#|
(define (htable->cmp-sym obj)
  (hashtable-ref known-cmp-sym (hashtable-equivalence-function obj) #f))

(define (htable->hash-sym obj)
  (hashtable-ref known-hash-sym (hashtable-hash-function obj) #f))

(define (put/hashtable bsp obj)
  (let ((cmp-sym  (htable->cmp-sym  obj))
        (hash-sym (htable->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (put/tag bsp (case cmp-sym ((eq?)  tag-eq-hashtable)
                                                ((eqv?) tag-eqv-hashtable)
                                                (else   tag-hashtable))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bsp hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bsp cmp-sym))))
             (pos (put/vlen bsp (hashtable-size obj)))) ; n is encoded as vlen
        (when pos
          (for-hash ((k v obj))
            (set! pos (put/any bsp (put/any bsp k) v))))
        pos)
      #f)))


(define (ord-hash->cmp-sym obj)
  (hashtable-ref known-cmp-sym (ordered-hash-equivalence-function obj) #f))

(define (ord-hash->hash-sym obj)
  (hashtable-ref known-hash-sym (ordered-hash-hash-function obj) #f))

(define (put/ord-hash bsp obj)
  (let ((cmp-sym  (ord-hash->cmp-sym  obj))
        (hash-sym (ord-hash->hash-sym obj)))
    (if (and cmp-sym (or hash-sym (memq cmp-sym '(eq? eqv?))))
      (let* ((pos (put/tag bsp (case cmp-sym ((eq?)  tag-eq-ord-hash)
                                                ((eqv?) tag-eqv-ord-hash)
                                                (else   tag-ord-hash))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bsp hash-sym))))
             (pos (case cmp-sym ((eq? eqv?) pos)
                                (else (put/symbol bsp cmp-sym))))
             (pos (put/vlen bsp (ordered-hash-size obj)))) ; n is encoded as vlen
        (when pos
          (for-ordered-hash ((k v obj))
            (set! pos (put/any bsp (put/any bsp k) v))))
        pos)
      #f)))
|#


;; recursively traverse obj, serialize it and write it into bytevector bsp starting at position pos.
;; return updated position, or #f on errors.
(define (put/any bsp obj)
  (cond
    ((fixnum? obj)       (put/integer bsp obj))
    ((char?   obj)       (put/char    bsp obj))
    ((pair?   obj)       (put/pair    bsp obj))
    ((symbol? obj)       (put/symbol  bsp obj))
    ((flonum? obj)       (put/flonum  bsp obj))
    ((null? obj)         (put/u8      bsp misc-nil))
    ((boolean? obj)      (put/u8      bsp (if obj misc-true misc-false)))
    ((eq? (void) obj)    (put/u8      bsp misc-void))
    ((eof-object? obj)   (put/u8      bsp misc-eof))
    ((bwp-object? obj)   (put/u8      bsp misc-bwp))
    ((procedure? obj)    #f)
    ;; these are slower to check
    ((box?    obj)       (put/box        bsp obj))
    ((number? obj)       (put/number     bsp obj))
    ((vector? obj)       (put/vector     bsp obj))
    ((bytevector? obj)   (put/bytevector bsp obj))
    ((string? obj)       (put/string     bsp obj))
    ;((hashtable? obj)    (put/hashtable  bsp obj))
    ;((ordered-hash? obj) (put/ord-hash   bsp obj))
    ((fxvector? obj)     (put/fxvector   bsp obj))
    ((flvector? obj)     (put/flvector   bsp obj))
    ;((record? obj)       (put/record     bsp obj))
    (else                #f)))



;; append cbor magic bytes to bytespan
(define (cbor-put-magic-to-bytespan bsp)
  (bytespan-insert-right/bytevector! bsp cbor-magic-bytes))

#|
;; read magic bytes.
;; return #t -NNN if they match, instructing caller that NNN bytes should be ignored,
;; or raise condition if they don't match, because it means the other side uses an incompatible cbor protocol.
(define (check-magic bsp end)
  (let* ((start (fx1- pos))
         (n     (fx- end start)))
    (if (and (fx=? n (bytevector-length cbor-magic-bytes))
             (subbytevector=? bsp start cbor-magic-bytes 0 n))
      (values #t (fx- n))
      (raise-errorf 'cbor-get-from-bytevector "incompatible cbor protocol ~s, expecting ~s"
        (bytevector->bytespan* bsp start end) (bytevector->bytespan* cbor-magic-bytes)))))


;; recursively traverse obj, serialize it and append it to bytespan bsp.
;; return number of written bytes, or #f on errors.
(define cbor-put-to-bytespan
  (case-lambda
    ((bsp obj message-cbor-len)
      (assert* 'cbor-put (bytespan? bsp))
      (when message-cbor-len
        (assert* 'cbor-put (fixnum? message-cbor-len)))
      (let ((payload-cbor-len (vlen- message-cbor-len))
            (len-before       (bytespan-length bsp)))
        (if (valid-payload-len? payload-cbor-len)
          (let ((len-after (fx+ len-before message-cbor-len)))
            (bytespan-reserve-right! bsp len-after)
            (let* ((bsp   (bytespan-peek-data bsp))
                   (pos  (fx+ len-before (bytespan-peek-beg bsp)))
                   (end0 (put/vlen bsp payload-cbor-len))
                   (end  (if (eq? (void) obj)
                            end0
                            (put/any bsp end0 obj))))
              (assert* 'cbor-put (fx=? (fx- end pos) message-cbor-len))
              (bytespan-resize-right! bsp len-after)
              message-cbor-len))
          #f)))
    ((bsp obj)
      (cbor-put-to-bytespan bsp obj (datum->cbor-length obj)))))

;; recursively traverse obj, serialize it and return bytevector containing serialized bytes,
;; or #f on errors
(define (datum->cbor obj)
  (let ((bsp (bytespan)))
    (if (cbor-put-to-bytespan bsp obj)
      (bytespan->bytevector*! bsp)
      #f)))


|#
