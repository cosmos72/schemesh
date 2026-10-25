;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/cbor/cbor.ss

#|
(define (symbol->tag sym)
  (hashtable-ref known-sym sym #f))
|#

(define (put/u8 bv pos u8)
  (bytevector-u8-set! bv pos u8)
  (fx1+ pos))

(define (put/u16 bv pos u16)
  (bytevector-u16-set! bv pos u16 endian)
  (fx+ pos 2))

(define (put/u32 bv pos u32)
  (bytevector-u32-set! bv pos u32 endian)
  (fx+ pos 4))

(define (put/u64 bv pos u64)
  (bytevector-u64-set! bv pos u64 endian)
  (fx+ pos 8))


(define (get/u8 bv pos)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (small-fixnum? obj)
  (and (fixnum? obj) (fx<=? #x-10000 obj #xFFFF)))


(define (len/small-fixnum pos obj)
  (let ((obj (if (fx<? obj 0) (fxnot obj) obj)))
    (cond
      ((fx<=? obj 23)   (fx1+ pos))
      ((fx<=? obj 255)  (fx+ pos 2))
      (else             (fx+ pos 3)))))


(define (len/unsigned pos obj)
  (cond
    ((<= obj #xFFFFFFFF)
      (fx+ pos 5))
    ((<= obj #xFFFFFFFF)
      (fx+ pos 9))
    (else
      pos))) ;; TODO: unsigned bignum


(define (len/negative pos obj)
  (len/unsigned pos (bitwise-not obj)))


(define (len/integer pos obj)
  (cond
    ((small-fixnum? obj)
      (len/small-fixnum pos obj))
    ((>= obj 0)
      (len/unsigned pos obj))
    (else
      (len/negative pos obj))))


(define (put/small-fixnum bv pos obj)
  (if (fx>=? obj 0)
    (cond
      ((fx<=? obj 23)
        (let ((pos (put/u8 bv pos (fxior major-unsigned obj))))
          pos))
      ((fx<=? obj 255)
        (let ((pos (put/u8 bv pos (fxior major-unsigned minor-1byte))))
          (put/u8 bv pos obj)))
      (else
        (let ((pos (put/u8 bv pos (fxior major-unsigned minor-2byte))))
          (put/u16 bv pos obj))))
    (let ((obj (fxnot obj)))
      (cond
        ((fx<=? obj 23)
          (let ((pos (put/u8 bv pos (fxior major-negative obj))))
            pos))
        ((fx<=? obj 255)
          (let ((pos (put/u8 bv pos (fxior major-negative minor-1byte))))
            (put/u8 bv pos obj)))
        (else
          (let ((pos (put/u8 bv pos (fxior major-negative minor-2byte))))
            (put/u16 bv pos obj)))))))


(define (put/unsigned bv pos obj)
  (cond
    ((<= obj #xFFFFFFFF)
      (let ((pos (put/u8 bv pos (fxior major-unsigned minor-4byte))))
        (put/u32 bv pos obj)))
    ((<= obj #xFFFFFFFFFFFFFFFF)
      (let ((pos (put/u8 bv pos (fxior major-unsigned minor-8byte))))
        (put/u64 bv pos obj)))
    (else
      pos))) ;; TODO: unsigned bignum


(define (put/negative bv pos obj)
  (let ((obj (bitwise-not obj)))
    (cond
      ((<= obj #xFFFFFFFF)
        (let ((pos (put/u8 bv pos (fxior major-negative minor-4byte))))
          (put/u32 bv pos obj)))
      ((<= obj #xFFFFFFFFFFFFFFFF)
        (let ((pos (put/u8 bv pos (fxior major-negative minor-8byte))))
          (put/u64 bv pos obj)))
      (else
        pos)))) ;; TODO: negative bignum


;; write an exact integer into bytevector starting at position pos.
;; return updated position, or raise exception on errors.
(define (put/integer bv pos obj)
  (cond
    ((small-fixnum? obj)
      (put/small-fixnum bv pos obj))
    ((>= obj 0)
      (put/unsigned bv pos obj))
    (else
      (put/negative bv pos obj))))


(define (get/unsigned bv pos end)
  (if (and (fixnum? pos) (fx<? pos end))
    (values 0 (fx1+ pos))
    (values #f #f)))

(define (get/negative bv pos end)
  (if (and (fixnum? pos) (fx<? pos end))
    (values -1 (fx1+ pos))
    (values #f #f)))

;; read an exact integer from bytevector at position pos.
;; return two values: deserialized integer and updated position,
;; or (values #<unspecified> #f) on error
(define (get/integer bv pos end)
  (if #t
    (get/unsigned bv pos end)
    (get/negative bv pos end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (len/flonum pos obj)
  pos)

(define (put/flonum bv pos obj)
  (bytevector-ieee-double-set! bv pos obj endian)
  (fx+ pos len-flonum))


(define (get/flonum bv pos obj)
  (values
    (bytevector-ieee-double-ref bv pos endian)
    (fx+ pos len-flonum)))

(define (len/cflonum pos obj)
  pos)

(define (put/cflonum bv pos obj)
  pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (len/number pos obj)
  (if (exact? obj)
    ;; exact number
    (if (real? obj)
      (if (integer? obj)
        ;; exact integer
        (len/integer pos obj)
        ;; exact ratio
        (len/integer
          (len/integer pos (numerator obj))
          (denominator obj)))
      ;; exact complex: encoded as tag, real-part, imag-part
      (len/number
        (len/number pos (real-part obj))
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
        (put/integer bv pos obj)
        ;; exact ratio
        (let* ((end0 (put/integer bv pos  (numerator obj)))
               (end1 (put/integer bv end0 (denominator obj))))
          end1))
      ;; exact complex: encoded as tag, (real-part, imag-part)
      (let* ((end0 (put/number bv pos  (real-part obj)))
             (end1 (put/number bv end0 (imag-part obj))))
        end1))
    ;; inexact number. assume flonum or cflonum
    (if (flonum? obj)
      (put/flonum bv pos obj)
      (put/cflonum bv pos obj))))


#|
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
    ((fixnum? obj)       (len/integer  pos obj))
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
    ((fixnum? obj)       (put/integer  bv pos obj))
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
(define (datum->cbor-length obj)
  (if (eq? (void) obj)
    (vlen+ 0 0)
    (let ((pos (len/any 0 obj)))
      (and (valid-payload-len? pos) (vlen+ pos pos)))))
|#

;; append cbor magic bytes to bytespan
(define (cbor-put-magic-to-bytespan bsp)
  (bytespan-insert-right/bytevector! bsp cbor-magic-bytes))

#|
;; read magic bytes.
;; return #t -NNN if they match, instructing caller that NNN bytes should be ignored,
;; or raise condition if they don't match, because it means the other side uses an incompatible cbor protocol.
(define (check-magic bv pos end)
  (let* ((start (fx1- pos))
         (n     (fx- end start)))
    (if (and (fx=? n (bytevector-length cbor-magic-bytes))
             (subbytevector=? bv start cbor-magic-bytes 0 n))
      (values #t (fx- n))
      (raise-errorf 'cbor-get-from-bytevector "incompatible cbor protocol ~s, expecting ~s"
        (bytevector->bytespan* bv start end) (bytevector->bytespan* cbor-magic-bytes)))))


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
            (let* ((bv   (bytespan-peek-data bsp))
                   (pos  (fx+ len-before (bytespan-peek-beg bsp)))
                   (end0 (put/vlen bv pos payload-cbor-len))
                   (end  (if (eq? (void) obj)
                            end0
                            (put/any bv end0 obj))))
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
