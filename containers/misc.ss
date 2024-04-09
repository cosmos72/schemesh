;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers misc (0 1))
  (export
    list-iterate reverse*!
    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable
    list->bytevector list-quoteq!
    subbytevector bytevector-fill-range! bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?
    string-fill-range! string-range=? string-iterate
    string->utf8b string->utf8b/0 integer->char*)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1-)
    (only (schemesh bootstrap) assert*))


; (list-iterate l proc) iterates on all elements of given list l,
; and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
(define (list-iterate l proc)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (proc (car tail)))))))

; For each item in items (which must be a list), when found in list l destructively
; replace it with (list 'quote item).
; Comparison between items is performed with eq?
;/
(define (list-quoteq! items l)
  (do ((tail l (cdr tail)))
      ((null? tail) l)
    (let ((item (car tail)))
      (when (memq item items)
        (set-car! tail (list 'quote item))))))

; (reverse*! l) destructively reverses list l,
; creating an improper list - unless (car l) is itself a list.
;
; Example: (reverse*! (list a b c)) returns '(c b . a)
(define (reverse*! l)
  (if (or (null? l) (null? (cdr l)))
    l
    (let* ((tail (if (pair? (cdr l)) (cddr l) '()))
           (head (let ((first  (car l))
                       (second (cadr l)))
                   (set-car! l second)
                   (set-cdr! l first)
                   l)))
      (let %step ((head head)
                  (tail tail))
        (if (null? tail)
          head
          (let ((new-head tail)
                (new-tail (cdr tail)))
            (set-cdr! new-head head)
            (%step new-head new-tail)))))))

; copy a portion of vector src into dst.
; works even if src are the same vector and the two ranges overlap.
(define (vector-copy! src src-start dst dst-start n)
  (if (and (eq? src dst) (fx<? src-start dst-start))
    ; copy backward
    (do ((i (fx1- n) (fx1- i)))
        ((fx<? i 0))
      (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))
    ; copy forward
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (vector-set! dst (fx+ i dst-start) (vector-ref src (fx+ i src-start))))))


; return a copy of vector vec containing only elements
; from start (inclusive) to end (exclusive)
(define (subvector vec start end)
  (assert* (fx<=? 0 start end))
  (let* ((n (fx- end start))
         (dst (make-vector n)))
    (vector-copy! vec start dst 0 n)
    dst))

; set n elements of vector from offset = start to specified value
(define (vector-fill-range! vec start n val)
  (do ((i 0 (fx1+ i)))
      ((fx>=? i n))
    (vector-set! vec (fx+ i start) val)))

; (vector-iterate l proc) iterates on all elements of given vector vec,
; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
(define (vector-iterate vec proc)
  (do ((i 0 (fx1+ i))
       (n (vector-length vec)))
      ((or (fx>=? i n) (not (proc i (vector-ref vec i)))))))

; (vector->hashtable vec htable) iterates on all elements of given vector vec,
; which must be cons cells, and inserts them into hashtable htable:
; (car cell) is used as key, and (cdr cell) is used ad value.
;
; Returns htable.
(define (vector->hashtable vec htable)
  (vector-iterate vec
    (lambda (i cell)
      (hashtable-set! htable (car cell) (cdr cell))))
  htable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;     some additional bytevector functions    ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list->bytevector l)
  (apply bytevector l))


; return a copy of bytevector vec containing only elements
; from start (inclusive) to end (exclusive)
(define (subbytevector vec start end)
  (assert* (fx<=? 0 start end))
  (let* ((n (fx- end start))
         (dst (make-bytevector n)))
    (bytevector-copy! vec start dst 0 n)
    dst))

(define (bytevector-fill-range! bvec start n val)
  (do ((i 0 (fx1+ i)))
      ((fx>=? i n))
    (bytevector-u8-set! bvec (fx+ i start) val)))

; (bytevector-iterate l proc) iterates on all elements of given bytevector vec,
; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
(define (bytevector-iterate bvec proc)
  (do ((i 0 (fx1+ i))
       (n (bytevector-length bvec)))
      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i)))))))

; compare the two bytevectors bvec1 and bvec2.
; return -1 if bvec1 is lexicographically lesser than bvec2,
; return 0 if they are equal,
; return 1 if bvec1 is lexicographically greater than bvec2
(define bytevector-compare
  (let ((c-bytevector-compare (foreign-procedure "c_bytevector_compare"
          (scheme-object scheme-object) integer-8)))
    (lambda (bvec1 bvec2)
      (assert* (bytevector? bvec1))
      (assert* (bytevector? bvec2))
      (or (eq? bvec1 bvec2)
          (c-bytevector-compare bvec1 bvec2)))))

(define (bytevector<=? bvec1 bvec2)
  (fx<=? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector<? bvec1 bvec2)
  (fx<? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector>=? bvec1 bvec2)
  (fx>=? (bytevector-compare bvec1 bvec2) 0))

(define (bytevector>? bvec1 bvec2)
  (fx>? (bytevector-compare bvec1 bvec2) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;     some additional string functions    ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set n elements of string from offset = start to specified value
(define (string-fill-range! str start n val)
  (do ((i 0 (fx1+ i)))
      ((fx>=? i n))
    (string-set! str (fx+ i start) val)))

;; return #t if range [left-start, left-start + n) of left string contains
;; the same characters as range [right-start, right-start + n) of right string.
(define (string-range=? left left-start right right-start n)
  (assert* (fx<=? 0 left-start (string-length left)))
  (assert* (fx<=? 0 right-start (string-length right)))
  (assert* (fx<=? 0 n (fx- (string-length left) left-start)))
  (assert* (fx<=? 0 n (fx- (string-length right) right-start)))
  (or
    (fxzero? n)
    (and (eq? left right) (fx=? left-start right-start))
    (do ((i 0 (fx1+ i)))
        ((or
           (fx>=? i n)
           (not (char=? (string-ref left (fx+ i left-start))
                        (string-ref right (fx+ i right-start)))))
         (fx>=? i n)))))


;; (string-iterate l proc) iterates on all elements of given string src,
;; and calls (proc index ch) on each character. stops iterating if (proc ...) returns #f
(define (string-iterate str proc)
  (do ((i 0 (fx1+ i))
       (n (string-length str)))
      ((or (fx>=? i n) (not (proc i (string-ref str i)))))))


;; convert a portion of a string to UTF-8b, and return bytevector containing the conversion result.
;;
;; For a definition of UTF-8b, see
;;   https://peps.python.org/pep-0383
;;   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
(define string-range->utf8b
  (let ((c-string-range->utf8b-append (foreign-procedure "c_string_range_to_utf8b_append"
                                          (scheme-object fixnum fixnum scheme-object fixnum) scheme-object))
        (c-string-range->utf8b-length (foreign-procedure "c_string_range_to_utf8b_length"
                                  (scheme-object fixnum fixnum) fixnum)))
    (lambda (str start n zeropad-byte-n)
      (assert* (fx<=? 0 start (string-length str)))
      (assert* (fx<=? 0 n (fx- (string-length str) start)))
      (assert* (fx>=? zeropad-byte-n 0))
      (let ((byte-n (c-string-range->utf8b-length str start n)))
        (assert* (fixnum? byte-n))
        (let* ((bvec (make-bytevector (fx+ byte-n zeropad-byte-n)))
               (written-n (c-string-range->utf8b-append str start n bvec 0)))
          (assert* (fx=? byte-n written-n))
          (when (fx>? zeropad-byte-n 0)
            (bytevector-fill-range! bvec byte-n zeropad-byte-n 0))
          bvec)))))

#| ;; slower
(define string-range->utf8b
  (let ((c-string-range->utf8b (foreign-procedure "c_string_range_to_utf8b"
                                          (scheme-object fixnum fixnum fixnum) scheme-object)))
    (lambda (str start n zeropad-byte-n)
      (assert* (fx<=? 0 start (string-length str)))
      (assert* (fx<=? 0 n (fx- (string-length str) start)))
      (assert* (fx>=? zeropad-byte-n 0))
      (let ((bvec (c-string-range->utf8b str start n zeropad-byte-n)))
        (assert* (bytevector? bvec))
        bvec))))
|#

;; convert a string to UTF-8b, and return bytevector containing the conversion result.
(define (string->utf8b str)
  (string-range->utf8b str 0 (string-length str) 0))

;; convert a string to UTF-8b, append an additional byte 0 to conversion,
;; and return bytevector containing the conversion result.
(define (string->utf8b/0 str)
  (string-range->utf8b str 0 (string-length str) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;     some additional char functions    ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; similar to (integer->char) but integer Unicode codepoint is not checked for validity:
;; it INTENTIONALLY allows invalid codepoints in the ranges #xD800..#xDFFF and #x10FFFF..#xFFFFFF
(define integer->char*
  (let ((c-integer->char (foreign-procedure "c_integer_to_char" (unsigned-32) scheme-object)))
    (lambda (codepoint)
      (if (fx<=? 0 codepoint #xFFFFFF)
        (c-integer->char codepoint)
        (integer->char codepoint))))) ; raises exception

) ; close library
