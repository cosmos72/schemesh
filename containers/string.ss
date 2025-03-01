;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers string (0 7 7))
  (export
    assert-string-list? in-string
    string-any string-contains string-count string-every string-fill-range!
    string-index string-index-right
    string-is-unsigned-base10-integer? string-is-signed-base10-integer? string-iterate
    string-join string-list? string-list-split-after-nuls
    string-map string-prefix? string-prefix/char?
    string-range-count= string-range=? string-range<?
    string-replace-prefix string-replace-suffix string-replace/char! string-rtrim-newlines!
    string-split string-split-after-nuls string-suffix? string-suffix/char?
    string-trim-split-at-blanks)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+ fx1- reverse! string-copy! string-truncate! substring-fill! void)
    (only (schemesh bootstrap) assert* while)
    (only (schemesh containers list) list-iterate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional string functions    ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; shortcut for (assert* caller (string-list? l)
(define (assert-string-list? caller l)
  (assert* caller (string-list? l)))


;; apply proc element-wise to the i-th element of the strings
(define (%apply-proc proc str-list i)
  (apply proc (map (lambda (str) (string-ref str i)) str-list)))



;; apply proc element-wise to the elements of the strings, stop at the first truish value returned by (proc elem ...) and return it.
;; If all calls to (proc elem ...) return #f, then return #f.
;; If not all strings have the same length, iteration terminates when the end of shortest string is reached.
;; Proc must accept as many elements as there are strings, and must return a single value.
(define string-any
  (case-lambda
    ((proc)
      #f)
    ((proc str)
      (let ((n (string-length str)))
        (let %any ((i 0))
          (if (fx<? i n)
            (or (proc (string-ref str i))
                (%any (fx1+ i)))
            #f))))
    ((proc str1 str2)
      (let ((n (fxmin (string-length str1) (string-length str2))))
        (let %any ((i 0))
          (if (fx<? i n)
            (or (proc (string-ref str1 i) (string-ref str2 i))
                (%any (fx1+ i)))
            #f))))
    ((proc str1 str2 str3)
      (let ((n (fxmin (string-length str1) (string-length str2) (string-length str3))))
        (let %any ((i 0))
          (if (fx<? i n)
            (or (proc (string-ref str1 i) (string-ref str2 i) (string-ref str3 i))
                (%any (fx1+ i)))
            #f))))
    ((proc . str-list)
      (let ((n (apply fxmin (map string-length str-list))))
        (let %any ((i 0))
          (if (fx<? i n)
            (or (%apply-proc proc str-list i)
                (%any (fx1+ i)))
            #f))))))


;; apply proc element-wise to the elements of the strings, and count and return how many times (proc elem ...) evaluates to truish.
;; If not all strings have the same length, iteration terminates when the end of shortest string is reached.
;; Proc must accept as many elements as there are strings, and must return a single value.
(define string-count
  (case-lambda
    ((proc)
      0)
    ((proc str)
      (let ((n (string-length str)))
        (let %count ((i 0) (ret 0))
          (if (fx<? i n)
            (%count
              (if (proc (string-ref str i))
                (fx1+ ret)
                ret)
              (fx1+ i))
            ret))))
    ((proc str1 str2)
      (let ((n (fxmin (string-length str1) (string-length str2))))
        (let %count ((i 0) (ret 0))
          (if (fx<? i n)
            (%count (fx1+ i)
              (if (proc (string-ref str1 i) (string-ref str2 i))
                (fx1+ ret)
                ret))
            ret))))
    ((proc str1 str2 str3)
      (let ((n (fxmin (string-length str1) (string-length str2) (string-length str3))))
        (let %count ((i 0) (ret 0))
          (if (fx<? i n)
            (%count (fx1+ i)
              (if (proc (string-ref str1 i) (string-ref str2 i) (string-ref str3 i))
                (fx1+ ret)
                ret))
            ret))))
    ((proc . str-list)
      (let ((n (apply fxmin (map string-length str-list))))
        (let %count ((i 0) (ret 0))
          (if (fx<? i n)
            (%count (fx1+ i)
              (if (%apply-proc proc str-list i)
                (fx1+ ret)
                ret))
            ret))))))


;; apply proc element-wise to the elements of the strings, stop at the first #f returned by (proc elem ...) and return it.
;; If all calls to (proc elem ...) return truish, then return #t.
;; If not all strings have the same length, iteration terminates when the end of shortest string is reached.
;; Proc must accept as many elements as there are strings, and must return a single value.
(define string-every
  (case-lambda
    ((proc)
      #t)
    ((proc str)
      (string-iterate str proc))
    ((proc str1 str2)
      (string-any (lambda (ch1 ch2) (not (proc ch1 ch2))) str1 str2))
    ((proc str1 str2 str3)
      (string-any (lambda (ch1 ch2 ch3) (not (proc ch1 ch2 ch3))) str1 str2 str3))
    ((proc . str-list)
      (let ((n (apply fxmin (map string-length str-list))))
        (let %every ((i 0))
          (if (fx<? i n)
            (and (%apply-proc proc str-list i)
                 (%every (fx1+ i)))
            #t))))))


;; apply proc element-wise to the elements of the strings, and return a string of the results.
;; If not all strings have the same length, iteration terminates when the end of shortest string is reached.
;; Proc must accept as many elements as there are strings, and must return a character.
(define string-map
  (case-lambda
    ((proc)
      "")
    ((proc str)
      (let* ((n   (string-length str))
             (ret (make-string n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n)
              ret)
          (string-set! ret i (proc (string-ref str i))))))
    ((proc str1 str2)
      (let* ((n   (fxmin (string-length str1) (string-length str2)))
             (ret (make-string n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n)
              ret)
          (string-set! ret i (proc (string-ref str1 i) (string-ref str2 i))))))
    ((proc str1 str2 str3)
      (let* ((n   (fxmin (string-length str1) (string-length str2) (string-length str3)))
             (ret (make-string n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n)
              ret)
          (string-set! ret i (proc (string-ref str1 i) (string-ref str2 i) (string-ref str3 i))))))
    ((proc . str-list)
      (let* ((n   (apply fxmin (map string-length str-list)))
             (ret (make-string n)))
        (do ((i 0 (fx1+ i)))
            ((fx>=? i n)
              ret)
          (string-set! ret i (%apply-proc proc str-list i)))))))


;; return #t if l is a (possibly empty) list of strings
(define (string-list? l)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (string? (car tail))))
        (null? tail))))


;; iterate on string-list l, and split each string after each #\nul
;; return a string-list containing each produced fragment.
(define (string-list-split-after-nuls l)
  (let ((ret '()))
    (list-iterate l
       (lambda (elem)
          (set! ret (%string-split-after-nuls elem ret))))
    ; (debugf "builtin-split-at-0 args=~s split=~s" prog-and-args (reverse ret)
    (reverse! ret)))


;; split a string after each #\nul.
;; return as string-list containing each produced fragment.
(define (string-split-after-nuls str)
  (reverse! (%string-split-after-nuls str '())))


;; optimized version of (substring), avoids making a copy if extracting the whole string
(define (%substring str start end)
  (if (and (fxzero? start) (fx=? end (string-length str)))
    str
    (substring str start end)))


;; split a string at each #\nul, and cons each splitted fragment onto ret.
;; return updated ret.
(define (%string-split-after-nuls str ret)
  (let ((end (string-length str)))
    (let %loop ((start 0) (ret ret))
      (let ((pos (string-index/char str #\nul start end)))
        (if pos
          (%loop
            (fx1+ pos)
            (cons (substring str start pos) ret))
          (if (fx<? start end)
            (cons (%substring str start end) ret)
            ret))))))


;; destructively remove all consecutive trailing #\newline characters from string str.
;; return str.
(define (string-rtrim-newlines! str)
  (let %loop ((end (string-length str)))
    (if (and (fx>? end 0) (char=? #\newline (string-ref str (fx1- end))))
      (%loop (fx1- end))
      (begin
        (string-truncate! str end)
        str))))


;; return #t if character is a decimal digit 0..9
(define (char-is-decimal-digit? ch)
  (char<=? #\0 ch #\9))


;; return #t if obj is a non-empty string containing only decimal digits.
(define string-is-unsigned-base10-integer?
  (case-lambda
    ((obj start end)
      (if (and (string? obj) (fx<? start end))
        (do ((i start (fx1+ i)))
            ((or (fx>=? i end) (not (char-is-decimal-digit? (string-ref obj i))))
              (fx>=? i end)))
        #f))
    ((obj)
      (and (string? obj) (string-is-unsigned-base10-integer? obj 0 (string-length obj))))))


;; return #t if obj is a non-empty string containing only decimal digits, possibly prefixed by "-"
(define (string-is-signed-base10-integer? obj)
  (let ((n (string-length obj)))
    (cond
      ((fxzero? n)
        #f)
      ((char=? #\- (string-ref obj 0))
        (string-is-unsigned-base10-integer? obj 1 n))
      (else
        (string-is-unsigned-base10-integer? obj 0 n)))))


;; set characters in range [start, end) of string str to character ch
(define (string-fill-range! str start end ch)
  (assert* 'string-fill-range! (fx<=? 0 start end (string-length str)))
  (when (fx<? start end)
    (substring-fill! str start end ch)))


;; create and return a closure that iterates on elements of string str.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in string str and #t,
;; or (values #<unspecified> #f) if end of string is reached.
(define in-string
  (case-lambda
    ((str start end step)
      (assert* 'in-string (fx<=? 0 start end (string-length str)))
      (assert* 'in-string (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (string-ref str start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #\nul #f))))
    ((str start end)
      (in-string str start end 1))
    ((str)
      (in-string str 0 (string-length str) 1))))


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
(define (string-prefix/char? str ch)
  (let ((len (string-length str)))
    (if (fxzero? len)
      #f
      (char=? #\/ (string-ref str 0)))))


;; return #t if string str is non-empty and ends with character ch,
;; otherwise return #f.
(define (string-suffix/char? str ch)
  (let ((len (string-length str)))
    (if (fxzero? len)
      #f
      (char=? #\/ (string-ref str (fx1- len))))))


;; search string range [start, end) and return index of first character equal to ch.
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index/char
  (case-lambda
    ((str ch start end)
      (assert* 'string-index/char (string? str))
      (assert* 'string-index/char (fx<=? 0 start end (string-length str)))
      (assert* 'string-index/char (char? ch))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (char=? ch (string-ref str i)))
            (if (fx>=? i end) #f i))))
    ((str ch)
      (string-index/char str ch 0 (string-length str)))))


;; search string range [start, end) and return index of first character
;; that causes (predicate ch) to return truish.
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index/pred
  (case-lambda
    ((str predicate start end)
      (assert* 'string-index/pred (string? str))
      (assert* 'string-index/pred (procedure? predicate))
      (assert* 'string-index/pred (fx<=? 0 start end (string-length str)))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (string-ref str i)))
            (if (fx>=? i end) #f i))))
    ((str predicate)
      (string-index/pred str predicate 0 (string-length str)))))


;; search string range [start, end) and return index of first character
;; that matches char-or-predicate
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index
  (case-lambda
    ((str char-or-predicate start end)
      (if (char? char-or-predicate)
        (string-index/char str char-or-predicate start end)
        (string-index/pred str char-or-predicate start end)))
    ((str predicate)
      (string-index str predicate 0 (string-length str)))))



;; search string range [start, end) and return index of last character equal to ch.
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index-right/char
  (case-lambda
    ((str ch start end)
      (assert* 'string-index-right/char (string? str))
      (assert* 'string-index-right/char (fx<=? 0 start end (string-length str)))
      (assert* 'string-index-right/char (char? ch))
      (do ((i (fx1- end) (fx1- i)))
          ((or (fx<? i start) (char=? ch (string-ref str i)))
            (if (fx<? i start) #f i))))
    ((str ch)
      (string-index-right/char str ch 0 (string-length str)))))


;; search string range [start, end) and return index of last character
;; that causes (predicate ch) to return truish.
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index-right/pred
  (case-lambda
    ((str predicate start end)
      (assert* 'string-index-right/pred (string? str))
      (assert* 'string-index-right/pred (procedure? predicate))
      (assert* 'string-index-right/pred (fx<=? 0 start end (string-length str)))
      (do ((i (fx1- end) (fx1- i)))
          ((or (fx<? i start) (predicate (string-ref str i)))
            (if (fx<? i start) #f i))))
    ((str predicate)
      (string-index-right/pred str predicate 0 (string-length str)))))


;; search string range [start, end) and return index of last character
;; that matches char-or-predicate
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
(define string-index-right
  (case-lambda
    ((str char-or-predicate start end)
      (if (char? char-or-predicate)
        (string-index-right/char str char-or-predicate start end)
        (string-index-right/pred str char-or-predicate start end)))
    ((str predicate)
      (string-index-right str predicate 0 (string-length str)))))


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



;; concatenate the strings in str-list, which must be a list of strings,
;; inserting string sep between each pair of strings in str-list.
;; Notes:
;; 1. sep is not added before the first string
;; 2. sep is not added after the last string
;; 3. if the list of strings is empty or contains a single empty string,
;;      returns the empty string (consequence of 1. and 2.)
(define (string-join str-list sep)
  (cond
    ((null? str-list)
      "")
    ((null? (cdr str-list))
      ; always return a new string - NOT (car str-list)
      (string-copy (car str-list)))
    (else
      (let* ((sep-len   (string-length sep))
             (ret       (make-string (%sum-strings-length-plus-separator str-list sep-len 0)))
             (first     (car str-list))
             (first-len (string-length first)))
        (string-copy! first 0 ret 0 first-len)
        (let %copy-string-list! ((l (cdr str-list)) (pos first-len))
          (if (null? l)
            ret
            (let* ((elem     (car str-list))
                   (elem-len (string-length elem)))
              (string-copy! sep  0 ret pos sep-len)
              (string-copy! elem 0 ret (fx+ pos sep-len) elem-len)
              (%copy-string-list! (cdr l) (fx+ (fx+ pos sep-len) elem-len)))))))))


(define (%sum-strings-length-plus-separator l sep-len ret)
  (cond
    ((null? l)
      ret)
    ((null? (cdr l))
      (fx+ ret (string-length (car l))))
    (else
      (%sum-strings-length-plus-separator
        (cdr l)
        sep-len
        (fx+ (fx+ ret sep-len) (string-length (car l)))))))



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
    ((str delim start end)
      (assert* 'string-split (string? str))
      (assert* 'string-split (fx<=? 0 start end (string-length str)))
      (let ((l '()))
        (while start
          (let ((pos (string-index/char str delim start end)))
            (set! l (cons (substring str start (or pos end)) l))
            (set! start (if pos (fx1+ pos) #f))))
        (reverse! l)))
    ((str delim)
      (assert* 'string-split (string? str))
      (string-split str delim 0 (string-length str)))))


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
            (pos-not-blank (or (string-index/pred str char-is-not-blank? start end) end)))
        (while (fx<? pos-not-blank end)
          (let ((pos-blank (string-index/pred str char-is-blank? (fx1+ pos-not-blank) end)))
            (set! l (cons (substring str pos-not-blank (or pos-blank end)) l))
            (if pos-blank
              (set! pos-not-blank (or (string-index/pred str char-is-not-blank? (fx1+ pos-blank) end) end))
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
    (else
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
   ; (debugf "-> string-range<? left=~s, left-start=~s, left-end=~s, right=~s, right-start=~s, right-end=~s"
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
         (else
           (let ((ch1 (string-ref left i))
                 (ch2 (string-ref right j)))
              (cond
                ((char<? ch1 ch2)
                  (set! ret   #t)
                  (set! done? #t))
                ((char>? ch1 ch2)
                  (set! done? #t)))))))
     ; (debugf "<- string-range<? ret=~s" ret)
     ret))


;; if string str contains specified string key, return index of the first occurrence,
;; otherwise return #f.
;;
;; if optional arguments str-start str-end key-start key-end are specified,
;;   considers only matches that lie entirely in the range [str-start, str-end),
;;   and the returned index is either #f or a fixnum in such range.
(define string-contains
  (case-lambda
    ((str key str-start str-end key-start key-end)
      (assert* 'string-contains (fx<=? 0 str-start str-end (string-length str)))
      (assert* 'string-contains (fx<=? 0 key-start key-end (string-length key)))
      (let* ((key-len (fx- key-end key-start))
             (last    (fx- str-end key-len)))
        (do ((i str-start (fx1+ i)))
            ((or (fx>? i last) (string-range=? str i key key-start key-len))
              (if (fx<=? i last) i #f)))))
    ((str key)
      (string-contains str key 0 (string-length str) 0 (string-length key)))))


;; return #t if string str starts with specified string prefix,
;; otherwise return #f.
(define (string-prefix? str prefix)
  (let ((str-len    (string-length str))
        (prefix-len (string-length prefix)))
    (and (fx>=? str-len prefix-len)
         (string-range=? str 0 prefix 0 prefix-len))))


;; return #t if string str ends with specified string suffix.
;; otherwise return #f.
(define (string-suffix? str suffix)
  (let ((str-len    (string-length str))
        (suffix-len (string-length suffix)))
    (and (fx>=? str-len suffix-len)
         (string-range=? str (fx- str-len suffix-len) suffix 0 suffix-len))))


;; if string str begins with string old-prefix, create and return a copy of str
;; where the initial of old-prefix has been replaced by string new-prefix.
;;
;; otherwise return str
(define (string-replace-prefix str old-prefix new-prefix)
  (assert* 'string-replace-prefix (string? str))
  (assert* 'string-replace-prefix (string? old-prefix))
  (assert* 'string-replace-prefix (string? new-prefix))
  (if (string-prefix? str old-prefix)
    (let* ((len      (string-length str))
           (old-len  (string-length old-prefix))
           (new-len  (string-length new-prefix))
           (tail-len (fx- len old-len))
           (dst      (make-string (fx+ new-len tail-len))))
      (string-copy! new-prefix 0 dst 0 new-len)
      (string-copy! str old-len dst new-len tail-len)
      dst)
    str))


;; if str ends with old-suffix, create and return a copy of str
;; where the final old-suffix has been replaced by new-suffix.
;;
;; otherwise return str
(define (string-replace-suffix str old-suffix new-suffix)
  (assert* 'string-replace-suffix (string? str))
  (assert* 'string-replace-suffix (string? old-suffix))
  (assert* 'string-replace-suffix (string? new-suffix))
  (if (string-suffix? str old-suffix)
    (let* ((len      (string-length str))
           (old-len  (string-length old-suffix))
           (new-len  (string-length new-suffix))
           (head-len (fx- len old-len))
           (dst      (make-string (fx+ head-len new-len))))
      (string-copy! str 0 dst 0 head-len)
      (string-copy! new-suffix 0 dst head-len new-len)
      dst)
    str))

) ; close library
