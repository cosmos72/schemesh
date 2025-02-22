;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers string (0 7 6))
  (export
    string-list? assert-string-list? string-list-split-after-nuls
    string-is-unsigned-base10-integer? string-is-signed-base10-integer?
    string-fill-range! string-range-count= string-range=? string-range<?
    string-find string-rfind string-find/char string-rfind/char string-rtrim-newlines!
    string-split string-split-after-nuls string-trim-split-at-blanks string-replace/char!
    string-starts-with? string-ends-with? string-starts-with/char? string-ends-with/char?
    string-replace-start string-replace-end
    in-string string-iterate)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+ fx1- reverse! string-copy! string-truncate! substring-fill! void)
    (only (schemesh bootstrap) assert* while)
    (only (schemesh containers misc) list-iterate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional string functions    ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; return #t if l is a (possibly empty) list of strings
(define (string-list? l)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (string? (car tail))))
        (null? tail))))


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
      (let ((pos (string-find/char str start end #\nul)))
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


;; if str begins with old-str, create and return a copy of str
;; where the first occurrence of old-str has been replaced by new-str.
;;
;; otherwise return str
(define (string-replace-start str old-str new-str)
  (assert* 'string-replace (string? str))
  (assert* 'string-replace (string? old-str))
  (assert* 'string-replace (string? new-str))
  (if (string-starts-with? str old-str)
    (let* ((len      (string-length str))
           (old-len  (string-length old-str))
           (new-len  (string-length new-str))
           (tail-len (fx- len old-len))
           (dst      (make-string (fx+ new-len tail-len))))
      (string-copy! new-str 0 dst 0 new-len)
      (string-copy! str old-len dst new-len tail-len)
      dst)
    str))


;; if str ends with old-str, create and return a copy of str
;; where the last occurrence of old-str has been replaced by new-str.
;;
;; otherwise return str
(define (string-replace-end str old-str new-str)
  (assert* 'string-replace (string? str))
  (assert* 'string-replace (string? old-str))
  (assert* 'string-replace (string? new-str))
  (if (string-ends-with? str old-str)
    (let* ((len      (string-length str))
           (old-len  (string-length old-str))
           (new-len  (string-length new-str))
           (head-len (fx- len old-len))
           (dst      (make-string (fx+ head-len new-len))))
      (string-copy! str 0 dst 0 head-len)
      (string-copy! new-str 0 dst head-len new-len)
      dst)
    str))

) ; close library
