;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers misc (0 1))
  (export
    list-iterate list-nth list-quoteq! reverse*!
    string-list? assert-string-list? string-contains-only-decimal-digits?
    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable
    list->bytevector subbytevector
    bytevector-fill-range! bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?
    string-fill-range! string-range-count= string-range=? string-iterate)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1- void)
    (only (schemesh bootstrap) assert*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional list functions    ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (list-iterate l proc) iterates on all elements of given list l,
;; and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
(define (list-iterate l proc)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (proc (car tail)))))))


;; return n-th element of a list, where the car of the list is the "zeroth" element.
;; return (void) if list contains < n elements
(define (list-nth n l)
  (if (null? l)
    (void)
    (begin
      (assert* 'list-nth (pair? l))
      (do ((i n (fx1- i))
           (tail l (cdr tail)))
        ((or (fxzero? i) (null? tail))
           (if (and (fxzero? i) (not (null? tail)))
             (car tail)
             (void)))))))


;; For each item in items (which must be a list), when found in list l destructively
;; replace it with (list 'quote item).
;; Comparison between items is performed with eq?
(define (list-quoteq! items l)
  (do ((tail l (cdr tail)))
      ((null? tail) l)
    (let ((item (car tail)))
      (when (memq item items)
        (set-car! tail (list 'quote item))))))



;; (reverse*! l) destructively reverses list l,
;; creating an improper list - unless (car l) is itself a list.
;
;; Example: (reverse*! (list a b c)) returns '(c b . a)
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

;; return #t if l is a (possibly empty) list of strings
(define (string-list? l)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (string? (car tail)))) (null? tail))))


;; shortcut for (assert* caller (string-list? l)
(define (assert-string-list? caller l)
  (assert* caller (string-list? l)))


;; return #t if obj is a non-empty string and only contains decimal digits
(define (string-contains-only-decimal-digits? obj)
  (let ((n (if (string? obj) (string-length obj) 0)))
    (if (fxzero? n)
      #f
      (do ((i 0 (fx1+ i)))
          ((or (fx>=? i n) (not (decimal-digit? (string-ref obj i))))
             (fx>=? i n))))))

;; return #t if character is a decimal digit 0..9
(define (decimal-digit? ch)
  (char<=? #\0 ch #\9))


;; copy a portion of vector src into dst.
;; works even if src are the same vector and the two ranges overlap.
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


;; return a copy of vector vec containing only elements
;; from start (inclusive) to end (exclusive)
(define (subvector vec start end)
  (assert* 'subvector (fx<=? 0 start end))
  (let* ((n (fx- end start))
         (dst (make-vector n)))
    (vector-copy! vec start dst 0 n)
    dst))

;; set n elements of vector from offset = start to specified value
(define (vector-fill-range! vec start n val)
  (do ((i 0 (fx1+ i)))
      ((fx>=? i n))
    (vector-set! vec (fx+ i start) val)))

;; (vector-iterate l proc) iterates on all elements of given vector vec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
(define (vector-iterate vec proc)
  (do ((i 0 (fx1+ i))
       (n (vector-length vec)))
      ((or (fx>=? i n) (not (proc i (vector-ref vec i)))))))

;; (vector->hashtable vec htable) iterates on all elements of given vector vec,
;; which must be cons cells, and inserts them into hashtable htable:
;; (car cell) is used as key, and (cdr cell) is used ad value.
;
;; Returns htable.
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


;; return a copy of bytevector vec containing only elements
;; from start (inclusive) to end (exclusive)
(define (subbytevector vec start end)
  (assert* 'subbytevector (fx<=? 0 start end))
  (let* ((n (fx- end start))
         (dst (make-bytevector n)))
    (bytevector-copy! vec start dst 0 n)
    dst))

(define (bytevector-fill-range! bvec start n val)
  (do ((i 0 (fx1+ i)))
      ((fx>=? i n))
    (bytevector-u8-set! bvec (fx+ i start) val)))

;; (bytevector-iterate l proc) iterates on all elements of given bytevector vec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
(define (bytevector-iterate bvec proc)
  (do ((i 0 (fx1+ i))
       (n (bytevector-length bvec)))
      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i)))))))

;; compare the two bytevectors bvec1 and bvec2.
;; return -1 if bvec1 is lexicographically lesser than bvec2,
;; return 0 if they are equal,
;; return 1 if bvec1 is lexicographically greater than bvec2
(define bytevector-compare
  (let ((c-bytevector-compare (foreign-procedure "c_bytevector_compare"
          (ptr ptr) integer-8)))
    (lambda (bvec1 bvec2)
      (assert* 'bytevector-compare (bytevector? bvec1))
      (assert* 'bytevector-compare (bytevector? bvec2))
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

;; (string-iterate l proc) iterates on all elements of given string src,
;; and calls (proc index ch) on each character. stops iterating if (proc ...) returns #f
(define (string-iterate str proc)
  (do ((i 0 (fx1+ i))
       (n (string-length str)))
      ((or (fx>=? i n) (not (proc i (string-ref str i)))))))


;; compare the range [left-start, left-start + n) of left string
;; with the range [right-start, right-start + n) of right string.
;; return the leftmost position, starting from 0, containing different characters,
;; or n if the two ranges contain the same characters
(define (string-range-count= left left-start right right-start n)
  (assert* 'string-range-count= (fx<=? 0 left-start (string-length left)))
  (assert* 'string-range-count= (fx<=? 0 right-start (string-length right)))
  (assert* 'string-range-count= (fx<=? 0 n (fx- (string-length left) left-start)))
  (assert* 'string-range-count= (fx<=? 0 n (fx- (string-length right) right-start)))
  (cond
    ((fxzero? n)
      n)
    ((and (eq? left right) (fx=? left-start right-start))
      n)
    (#t
      (do ((i 0 (fx1+ i)))
          ((or
             (fx>=? i n)
             (not (char=? (string-ref left (fx+ i left-start))
                        (string-ref right (fx+ i right-start)))))
            i)))))


;; return #t if range [left-start, left-start + n) of left string contains
;; the same characters as range [right-start, right-start + n) of right string.
(define (string-range=? left left-start right right-start n)
  (fx=? n (string-range-count= left left-start right right-start n)))


) ; close library
