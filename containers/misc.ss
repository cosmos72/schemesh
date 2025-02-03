;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers misc (0 7 2))
  (export
    list-iterate list-quoteq! list-reverse*! list-remove-consecutive-duplicates!
    string-list? assert-string-list? string-list-split-after-nuls
    vector-copy! subvector vector-fill-range! vector-iterate vector->hashtable vector-range->list
    list->bytevector subbytevector
    bytevector-fill-range! bytevector-find/u8 bytevector-iterate bytevector-compare
    bytevector<=? bytevector<? bytevector>=? bytevector>?
    string-contains-only-decimal-digits?
    string-fill-range! string-range-count= string-range=? string-range<?
    string-find string-rfind string-find/char string-rfind/char
    string-split string-trim-split-at-blanks string-iterate string-replace/char!
    string-starts-with? string-ends-with? string-starts-with/char? string-ends-with/char?)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) bytevector foreign-procedure fx1+ fx1- reverse! substring-fill! void)
    (only (schemesh bootstrap) assert* while))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional list functions    ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (list-iterate l proc) iterates on all elements of given list l,
;; and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc elem) returned truish,
;; otherwise returns #f.
(define (list-iterate l proc)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (proc (car tail))))
       (null? tail))))


;; For each item in items (which must be a list), when found in list l destructively
;; replace it with (list 'quote item).
;; Comparison between items is performed with eq?
(define (list-quoteq! items l)
  (do ((tail l (cdr tail)))
      ((null? tail) l)
    (let ((item (car tail)))
      (when (memq item items)
        (set-car! tail (list 'quote item))))))



;; (list-reverse*! l) destructively reverses list l,
;; creating an improper list - unless (car l) is itself a list.
;;
;; Example: (list-reverse*! (list a b c)) returns '(c b . a)
(define (list-reverse*! l)
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

;; remove consecutive duplicates from a list, and return it.
;; elements are considered duplicates if (equal-pred elem1 elem2) returns truish.
(define (list-remove-consecutive-duplicates! l equal-pred)
  (let %recurse ((tail l))
    (cond
      ((or (null? tail) (null? (cdr tail)))
        (void))
      ((equal-pred (car tail) (cadr tail))
        (set-cdr! tail (cddr tail))
        (%recurse tail))
      (#t
        (%recurse (cdr tail)))))
  l)


;; return #t if l is a (possibly empty) list of strings
(define (string-list? l)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (string? (car tail)))) (null? tail))))


;; shortcut for (assert* caller (string-list? l)
(define (assert-string-list? caller l)
  (assert* caller (string-list? l)))


;; iterate on string-list l, and split each string after each #\nul
;; return a string-list containing each produced fragment.
(define (string-list-split-after-nuls l)
  (let ((ret '()))
    (list-iterate l
       (lambda (elem)
          (set! ret (%string-split-after-nuls elem ret))))
    ; (debugf "builtin-split-at-0 args=~s split=~s" prog-and-args (reverse ret)
    (reverse! ret)))


;; split a string at each #\nul, and cons each splitted fragment onto ret.
;; return updated ret.
(define (%string-split-after-nuls str ret)
  (let* ((start 0)
         (end (string-length str))
         (pos (string-find/char str start end #\nul)))
     (if pos
       (begin
         (while pos
           (set! ret (cons (substring str start pos) ret))
           (set! start (fx1+ pos))
           (set! pos (string-find/char str start end #\nul)))
         (when (fx<? start end)
           (set! ret (cons (substring str start end) ret)))
         ret)
       (cons
         (if (fxzero? start) str (substring str start end))
         ret))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;     some additional vector functions    ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; in range [start, end) i.e. from start (inclusive) to end (exclusive)
(define (subvector vec start end)
  (assert* 'subvector (fx<=? 0 start end (vector-length vec)))
  (let* ((n (fx- end start))
         (dst (make-vector n)))
    (vector-copy! vec start dst 0 n)
    dst))

;; set elements in range [start, end) of vector vec specified value
(define (vector-fill-range! vec start end val)
  (assert* 'vector-fill-range! (fx<=? 0 start end (vector-length vec)))
  (do ((i start (fx1+ i)))
      ((fx>=? i end))
    (vector-set! vec i val)))

;; read elements from vector range [start, end) and copy them into a list.
;; return such list.
(define (vector-range->list vec start end)
  (let %again ((pos (fx1- end))
               (ret '()))
    (if (fx>=? pos start)
      (%again (fx1- pos) (cons (vector-ref vec pos) ret))
      ret)))

;; (vector-iterate l proc) iterates on all elements of given vector vec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (vector-iterate vec proc)
  (do ((i 0 (fx1+ i))
       (n (vector-length vec)))
      ((or (fx>=? i n) (not (proc i (vector-ref vec i))))
       (fx>=? i n))))

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


;; return a copy of bytevector bvec containing only elements
;; from start (inclusive) to end (exclusive)
(define (subbytevector bvec start end)
  (assert* 'subbytevector (fx<=? 0 start end (bytevector-length bvec)))
  (let* ((n (fx- end start))
         (dst (make-bytevector n)))
    (bytevector-copy! bvec start dst 0 n)
    dst))

(define (bytevector-fill-range! bvec start end val)
  (assert* 'bytevector-fill-range! (fx<=? 0 start end (bytevector-length bvec)))
  (do ((i start (fx1+ i)))
      ((fx>=? i end))
    (bytevector-u8-set! bvec i val)))


;; search bytevector range [start, end) and return index of first byte equal to b.
;; returned numerical index will be in the range [start, end).
;; return #f if no such byte is found in range.
(define bytevector-find/u8
  (case-lambda
    ((bvec b)
      (bytevector-find/u8 bvec 0 (bytevector-length bvec) b))
    ((bvec start end b)
      (assert* 'bytevector-find/u8 (bytevector? bvec))
      (assert* 'bytevector-find/u8 (fx<=? 0 start end (bytevector-length bvec)))
      (assert* 'bytevector-find/u8 (fx<=? 0 b 255))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (fx=? b (bytevector-u8-ref bvec i)))
            (if (fx>=? i end) #f i))))))


;; (bytevector-iterate l proc) iterates on all elements of given bytevector bvec,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index elem) returned truish,
;; otherwise returns #f.
(define (bytevector-iterate bvec proc)
  (do ((i 0 (fx1+ i))
       (n (bytevector-length bvec)))
      ((or (fx>=? i n) (not (proc i (bytevector-u8-ref bvec i))))
       (fx>=? i n))))

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

;; set characters in range [start, end) of string str to character ch
(define (string-fill-range! str start end ch)
  (assert* 'string-fill-range! (fx<=? 0 start end (string-length str)))
  (when (fx<? start end)
    (substring-fill! str start end ch)))


;; (string-iterate l proc) iterates on all elements of given string src,
;; and calls (proc index ch) on each character. stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc index ch) returned truish,
;; otherwise returns #f.
(define (string-iterate str proc)
  (do ((i 0 (fx1+ i))
       (n (string-length str)))
      ((or (fx>=? i n) (not (proc i (string-ref str i))))
       (fx>=? i n))))


;; return #t if string str is non-empty and starts with character ch,
;; otherwise return #f.
(define (string-starts-with/char? str ch)
  (let ((len (string-length str)))
    (if (fxzero? len)
      #f
      (char=? #\/ (string-ref str 0)))))


;; return #t if string str is non-empty and ends with character ch,
;; otherwise return #f.
(define (string-ends-with/char? str ch)
  (let ((len (string-length str)))
    (if (fxzero? len)
      #f
      (char=? #\/ (string-ref str (fx1- len))))))


;; search string range [start, end) and return index of first character equal to ch.
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-find/char
  (case-lambda
    ((str ch)
      (string-find/char str 0 (string-length str) ch))
    ((str start end ch)
      (assert* 'string-find/char (string? str))
      (assert* 'string-find/char (fx<=? 0 start end (string-length str)))
      (assert* 'string-find/char (char? ch))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (char=? ch (string-ref str i)))
            (if (fx>=? i end) #f i))))))


;; search string range [start, end) and return index of last character equal to ch.
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-rfind/char
  (case-lambda
    ((str ch)
      (string-rfind/char str 0 (string-length str) ch))
    ((str start end ch)
      (assert* 'string-rfind/char (string? str))
      (assert* 'string-rfind/char (fx<=? 0 start end (string-length str)))
      (assert* 'string-rfind/char (char? ch))
      (do ((i (fx1- end) (fx1- i)))
          ((or (fx<? i start) (char=? ch (string-ref str i)))
            (if (fx<? i start) #f i))))))


;; search string range [start, end) and return index of first character
;; that causes (predicate ch) to return truish.
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-find
  (case-lambda
    ((str predicate)
      (string-find str 0 (string-length str) predicate))
    ((str start end predicate)
      (assert* 'string-find (string? str))
      (assert* 'string-find (fx<=? 0 start end (string-length str)))
      (assert* 'string-find (procedure? predicate))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (string-ref str i)))
            (if (fx>=? i end) #f i))))))


;; search string range [start, end) and return index of last character
;; that causes (predicate ch) to return truish.
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-rfind
  (case-lambda
    ((str predicate)
      (string-rfind str 0 (string-length str) predicate))
    ((str start end predicate)
      (assert* 'string-rfind (string? str))
      (assert* 'string-rfind (fx<=? 0 start end (string-length str)))
      (assert* 'string-rfind (procedure? predicate))
      (do ((i (fx1- end) (fx1- i)))
          ((or (fx<? i start) (predicate (string-ref str i)))
            (if (fx<? i start) #f i))))))


;; destructively replace each occurrence of old-char with new-char in string str.
;; return str, modified in-place.
(define (string-replace/char! str old-char new-char)
  (assert* 'string-replace/char (string? str))
  (assert* 'string-replace/char (char? old-char))
  (assert* 'string-replace/char (char? new-char))
  (let ((end (string-length str)))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i end) str)
      (when (char=? old-char (string-ref str i))
        (string-set! str i new-char)))))


;; split range [start, end) of string str into a list of substrings,
;; using specified character as delimiter.
;; Notes:
;; 1. delimiters are not included in returned list of substrings.
;; 2. multiple consecutive delimiters are *not* coalesced together;
;;    instead, each additional one adds an empty substring to the returned list.
;; 3. if the original string ends with a delimiter,
;;    an empty substring is appended to returned list.
;; 4. if the original string is empty, the returned list contains one empty string.
(define string-split
  (case-lambda
    ((str delim)
      (assert* 'string-split (string? str))
      (string-split str 0 (string-length str) delim))
    ((str start end delim)
      (assert* 'string-split (string? str))
      (assert* 'string-split (fx<=? 0 start end (string-length str)))
      (let ((l '()))
        (while start
          (let ((pos (string-find/char str start end delim)))
            (set! l (cons (substring str start (or pos end)) l))
            (set! start (if pos (fx1+ pos) #f))))
        (reverse! l)))))


(define (char-is-blank? ch)
  (char<=? ch #\space))

(define (char-is-not-blank? ch)
  (char>? ch #\space))

;; trim a string then split it into a list of substrings, using as delimiter any character <= #\space
;; Notes:
;; 1. delimiters are not included in returned list of substrings.
;; 2. multiple consecutive delimiters are coalesced together.
;; 3. if original string starts and/or ends with delimiters, such delimiters are ignored.
;; 4. if the original string is empty, the returned list will contain zero elements.
(define string-trim-split-at-blanks
  (case-lambda
    ((str)
      (assert* 'string-trim-split-at-blanks (string? str))
      (string-trim-split-at-blanks str 0 (string-length str)))
    ((str start end)
      (assert* 'string-trim-split-at-blanks (string? str))
      (assert* 'string-trim-split-at-blanks (fx<=? 0 start end (string-length str)))
      (let ((l '())
            (pos-not-blank (or (string-find str start end char-is-not-blank?) end)))
        (while (fx<? pos-not-blank end)
          (let ((pos-blank (string-find str (fx1+ pos-not-blank) end char-is-blank?)))
            (set! l (cons (substring str pos-not-blank (or pos-blank end)) l))
            (if pos-blank
              (set! pos-not-blank (or (string-find str (fx1+ pos-blank) end char-is-not-blank?) end))
              (set! pos-not-blank end))))
        (reverse! l)))))



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
;; otherwise return #f
(define (string-range=? left left-start right right-start n)
  (fx=? n (string-range-count= left left-start right right-start n)))



(define (string-range<? left  left-start  left-end
                        right right-start right-end)
   ; (debugf "> string-range<? left=~s, left-start=~s, left-end=~s, right=~s, right-start=~s, right-end=~s"
   ;         left left-start left-end right right-start right-end)
   (let ((done? #f)
         (ret   #f))
     (do ((i left-start  (fx1+ i))
          (j right-start (fx1+ j)))
         (done?)
       ; (debugf ". string-range<? i=~s, j=~s" i j)
       (cond
         ((fx>=? i left-end)
           (set! done? #t))
         ((fx>=? j right-end)
           (set! ret   #t)
           (set! done? #t))
         (#t
           (let ((ch1 (string-ref left i))
                 (ch2 (string-ref right j)))
              (cond
                ((char<? ch1 ch2)
                  (set! ret   #t)
                  (set! done? #t))
                ((char>? ch1 ch2)
                  (set! done? #t)))))))
     ; (debugf "< string-range<? ret=~s" ret)
     ret))


;; return #t if string str starts with specified string prefix,
;; otherwise return #f.
(define (string-starts-with? str prefix)
  (let ((str-len    (string-length str))
        (prefix-len (string-length prefix)))
    (and (fx>=? str-len prefix-len)
         (string-range=? str 0 prefix 0 prefix-len))))


;; return #t if string str ends with specified string suffix,
;; otherwise return #f.
(define (string-ends-with? str suffix)
  (let ((str-len    (string-length str))
        (suffix-len (string-length suffix)))
    (and (fx>=? str-len suffix-len)
         (string-range=? str (fx- str-len suffix-len) suffix 0 suffix-len))))


) ; close library
