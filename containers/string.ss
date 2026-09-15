;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers string (1 0 1))
  (export
    assert-string-list? for-string in-string
    string-any string-contains string-count string-null? string-every string-iterate

    string-count= string-fold string-fold-right r7rs:string-for-each string-for-each-index
    string-index string-index-right string-is-unsigned-base10-integer? string-is-signed-base10-integer?
    string-join string-list? string-list-split-after-nuls string-map string-map! string-prefix?
    string-replace-prefix string-replace-suffix string-replace/char! string-rtrim-newlines!
    string-split string-split-after-nuls string-suffix? string-unfold string-unfold-right
    string-trim-split-at-blanks
    substring=? substring<? substring-move!

    display-procedure-name)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme)               foreign-procedure format fx1+ fx1- logbit? procedure-arity-mask
                                     reverse! string-copy! string-truncate! void)
    (only (scheme2k bootstrap)       assert* lambda0 for fx<=?* generate-pretty-temporaries lambda0 while)
    (only (scheme2k containers list) for-list list-copy*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional string functions    ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; shortcut for (assert* caller (string-list? l)
(define (assert-string-list? caller l)
  (assert* caller (string-list? l)))


;; apply proc element-wise to the i-th element of the strings
(define (%apply-proc proc str-list i)
  (apply proc (map (lambda (str) (string-ref str i)) str-list)))


(define (%to-pred caller char/char-set/pred)
  (cond
    ((char? char/char-set/pred)
      (lambda (ch) (char=? ch char/char-set/pred)))
    ((string? char/char-set/pred)
      (lambda (ch) (and (string-index char/char-set/pred ch) #t)))
    (else
      (assert* caller (procedure? char/char-set/pred))
      (assert* caller (logbit? 1 (procedure-arity-mask char/char-set/pred)))
      char/char-set/pred)))


;; for each element of string str:
;;   if char/char-set/pred is character, it is tested for equality with the element
;;   if char/char-set/pred is string, the element is tested for membership to it
;;   if char/char-set/pred is procedure, it is invoked with the element as only argument
;;
;; If the test for an element returns truish, further elements are not tested
;; and (string-any) returns the value produced by the last test.
;; Otherwise (string-any) returns #f.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-any
  (case-lambda
    ((char/char-set/pred str start end)
      (assert* 'string-any (fx<=?* 0 start end (string-length str)))
      (let %string-any ((ret #f) (str str) (i start) (end end)
                        (pred (%to-pred 'string-any char/char-set/pred)))
        (if (or ret (fx>=? i end))
          ret
          (%string-any (pred (string-ref str i)) str (fx1+ i) end pred))))
    ((char/char-set/pred str)
      (string-any char/char-set/pred str 0 (string-length str)))))


;; for each element of string str:
;;   if char/char-set/pred is character, it is tested for equality with the element
;;   if char/char-set/pred is string, the element is tested for membership to it
;;   if char/char-set/pred is procedure, it is invoked with the element as only argument
;;
;; Return the count of string element tests that produced a truish value.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-count
  (case-lambda
    ((char/char-set/pred str start end)
      (assert* 'string-count (fx<=?* 0 start end (string-length str)))
      (let %string-count ((ret 0) (str str) (i start) (end end)
                          (pred (%to-pred 'string-count char/char-set/pred)))
        (if (fx<? i end)
          (%string-count (if (pred (string-ref str i)) (fx1+ ret) ret)
                         str (fx1+ i) end pred)
          ret)))
    ((char/char-set/pred str)
      (string-count char/char-set/pred str 0 (string-length str)))))


;; return #t if string has zero length, otherwise return #f
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define (string-null? str)
  (fxzero? (string-length str)))


;; for each element of string str:
;;   if char/char-set/pred is character, it is tested for equality with the element
;;   if char/char-set/pred is string, the element is tested for membership to it
;;   if char/char-set/pred is procedure, it is invoked with the element as only argument
;;
;; if the test for an element returns #f, further elements are not tested and (string-every) returns #f
;; otherwise (string-every) returns the value produced by the last test, or #t if str is empty.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-every
  (case-lambda
    ((char/char-set/pred str start end)
      (assert* 'string-every (fx<=?* 0 start end (string-length str)))
      (let %string-every ((ret #t) (str str) (i start) (end end)
                          (pred (%to-pred 'string-every char/char-set/pred)))
        (if (and ret (fx<? i end))
          (%string-every (pred (string-ref str i)) str (fx1+ i) end pred)
          ret)))
    ((char/char-set/pred str)
      (string-every char/char-set/pred str 0 (string-length str)))))


;; left-fold map the kons procedure across the string from left to right
;; i.e. returns
;;
;; (kons str[end-1] (kons str[end-2] (... (kons str[start+1] (kons str[start] knil)))))
;;
;; obeying the tail recursion
;;
;; (string-fold kons knil str start end) =
;;   (string-fold kons (kons str[start] knil) start+1 end)
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-fold
  (case-lambda
    ((kons knil str start end)
      (assert* 'string-fold (fx<=?* 0 start end (string-length str)))
      (let %string-fold ((knil knil) (start start))
        (if (fx<? start end)
          (%string-fold (kons (string-ref str start) knil) (fx1+ start))
          knil)))
    ((kons knil str)
      (string-fold kons knil str 0 (string-length str)))))
        

;; right-fold map the kons procedure across the string from right to left
;; i.e. returns
;;
;; (kons str[start] (kons str[start+1] (... (kons str[end-2] (kons str[end-1] knil)))))
;;
;; obeying the tail recursion
;;
;; (string-fold-right kons knil str start end) =
;;   (string-fold-right kons (kons str[end-1] knil) start end-1)
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-fold-right
  (case-lambda
    ((kons knil str start end)
      (assert* 'string-fold-right (fx<=?* 0 start end (string-length str)))
      (let %string-fold-right ((knil knil) (end-1 (fx1- end)))
        (if (fx<=? start end-1)
          (%string-fold-right (kons (string-ref str end-1) knil) (fx1- end-1))
          knil)))
    ((kons knil str)
      (string-fold-right kons knil str 0 (string-length str)))))


(define (%string-for-each-assert* caller proc str start end)
  (assert* caller (procedure? proc))
  (assert* caller (logbit? 1 (procedure-arity-mask proc)))
  (assert* caller (string? str))
  (assert* caller (fx<=?* 0 start end (string-length str))))


;; apply proc to each character in str, in increasing order
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define r7rs:string-for-each
  (case-lambda
    ((proc str start end)
      (%string-for-each-assert* 'r7rs:string-for-each proc str start end)
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (proc (string-ref str i))))
    ((proc str)
      (r7rs:string-for-each proc str 0 (string-length str)))))


;; apply proc to each index of charactes in str, in increasing order
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-for-each-index
  (case-lambda
    ((proc str start end)
      (%string-for-each-assert* 'string-for-each-index proc str start end)
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (proc i)))
    ((proc str)
      (string-for-each-index proc str 0 (string-length str)))))


;; return #t if character is a decimal digit 0..9
(define (%char-is-decimal-digit? ch)
  (char<=? #\0 ch #\9))


;; return #t if obj is a non-empty string containing only decimal digits.
(define string-is-unsigned-base10-integer?
  (case-lambda
    ((obj start end)
      (and (string? obj) (fx<? start end)
           (do ((i start (fx1+ i)))
               ((or (fx>=? i end) (not (%char-is-decimal-digit? (string-ref obj i))))
                 (fx>=? i end)))))
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


;; split a string at each #\x0, and cons each splitted fragment onto ret.
;; return updated ret.
(define (%string-split-after-nuls str ret)
  (let %loop ((start 0) (end (string-length str)) (ret ret))
    (let ((pos (string-index str #\x0 start end)))
      (cond
        (pos              (%loop (fx1+ pos) end (cons (substring str start pos) ret)))
        ((fx<? start end) (cons (%substring/shared str start end) ret))
        (else             ret)))))


;; return #t if l is a (possibly empty) list of strings
(define (string-list? l)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (string? (car tail))))
        (null? tail))))


;; iterate on string-list l, and split each string after each #\x0
;; return a string-list containing each produced fragment.
(define (string-list-split-after-nuls l)
  (let %loop ((l l) (ret '()))
    (if (null? l)
      (reverse! ret)
      (%loop (cdr l) (%string-split-after-nuls (car l) ret)))))
    ;; (debugf "builtin-split-at-0 args=~s split=~s" prog-and-args (reverse ret)


;; apply proc element-wise to each element of string str, and return a string containing the transformed elements.
;; Proc must accept one character and return a character.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-map
  (case-lambda
    ((proc str start end)
      (assert* 'string-map (fx<=?* 0 start end (string-length str)))
      (let %string-map ((i start) (ret (make-string (fx- end start))))
        (if (fx<? i end)
          (begin
            (string-set! ret (fx- i start) (proc (string-ref str i)))
            (%string-map (fx1+ i) ret))
          ret)))
    ((proc str)
      (string-map proc str 0 (string-length str)))))


;; apply proc element-wise to each element of string str, and modify str in place storing the transformed elements.
;; Proc must accept one character and return a character.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-map!
  (case-lambda
    ((proc str start end)
      (assert* 'string-map (fx<=?* 0 start end (string-length str)))
      (let %string-map! ((i start))
        (when (fx<? i end)
          (string-set! str i (proc (string-ref str i)))
          (%string-map! (fx1+ i)))))
    ((proc str)
      (string-map! proc str 0 (string-length str)))))


;; destructively remove all consecutive trailing #\newline characters from string str.
;; return str.
(define (string-rtrim-newlines! str)
  (let %loop ((end (string-length str)))
    (if (and (fx>? end 0) (char=? #\newline (string-ref str (fx1- end))))
      (%loop (fx1- end))
      (string-truncate! str end))))


;; optimized version of (substring), avoids making a copy if extracting the whole string
(define (%substring/shared str start end)
  (if (and (fxzero? start) (fx=? end (string-length str)))
    str
    (substring str start end)))


;; split a string after each #\x0.
;; return as string-list containing each produced fragment.
(define (string-split-after-nuls str)
  (reverse! (%string-split-after-nuls str '())))


(define (%string-insert-left/char! str offset ch)
  (assert* 'string-unfold-right (char? ch))
  (let ((cap      (string-length str))
        (offset+1 (fx1+ offset)))
    (cond
      ((fx<? offset cap)
        (string-set! str (fx- cap offset+1) ch)
        str)
      (else
        (assert* 'string-unfold-right (fx=? offset cap))
        (let* ((new-cap (fxmax 8 (fx* 2 cap)))
               (new-str (make-string new-cap)))
          (string-copy! str 0 new-str (fx- new-cap cap) cap)
          (string-set! new-str (fx- new-cap offset+1) ch)
          new-str)))))


(define (%string-insert-right/char! str offset ch)
  (assert* 'string-unfold (char? ch))
  (let ((cap (string-length str)))
    (cond
      ((fx<? offset cap)
        (string-set! str offset ch)
        str)
      (else
        (assert* 'string-unfold (fx=? offset cap))
        (let* ((new-cap (fxmax 8 (fx* 2 cap)))
               (new-str (make-string new-cap)))
          (string-copy! str 0 new-str 0 cap)
          (string-set! new-str offset ch)
          new-str)))))


(define (%string-insert-left! str offset prefix)
  (assert* 'string-unfold-right (string? prefix))
  (let* ((cap  (string-length str))
         (hlen (string-length prefix))
         (len  (fx+ offset hlen)))
    (cond
      ((fx=? cap len)
        (string-copy! prefix 0 str 0 hlen)
        str)
      (else
        (let ((new-str (make-string len)))
          (string-copy! prefix 0 new-str 0 hlen)
          (string-copy! str (fx- cap offset) new-str hlen offset)
          new-str)))))


(define (%string-insert-right! str offset suffix)
  (assert* 'string-unfold (string? suffix))
  (let* ((cap  (string-length str))
         (tlen (string-length suffix))
         (len  (fx+ offset tlen)))
    (cond
      ((fx>=? cap len)
        (string-copy! suffix 0 str offset tlen)
        (string-truncate! str len)
        str)
      (else
        (let ((new-str (make-string len)))
          (string-copy! str 0 new-str offset offset)
          (string-copy! suffix 0 new-str offset tlen)
          new-str)))))


(define (%string-unfold-assert* caller stop? seed->char seed->next initial-string last-seed->final-string)
  (assert* caller (procedure? stop?))
  (assert* caller (procedure? seed->char))
  (assert* caller (procedure? seed->next))
  (assert* caller (string? initial-string))
  (assert* caller (procedure? last-seed->final-string))
  (assert* caller (logbit? 1 (procedure-arity-mask stop?)))
  (assert* caller (logbit? 1 (procedure-arity-mask seed->char)))
  (assert* caller (logbit? 1 (procedure-arity-mask seed->next)))
  (assert* caller (logbit? 1 (procedure-arity-mask last-seed->final-string))))


;; create and return a string from characters obtained by repeatedly calling seed->char
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-unfold
  (case-lambda
    ((stop? seed->char seed->next seed prefix last-seed->suffix)
      (%string-unfold-assert* 'string-unfold-right stop? seed->char seed->next prefix last-seed->suffix)
      (let %string-unfold ((seed seed) (offset (string-length prefix)) (str prefix))
        (if (stop? seed)
          (%string-insert-right! str offset (last-seed->suffix seed))
          (%string-unfold (seed->next seed) (fx1+ offset) (%string-insert-right/char! str offset (seed->char seed))))))
    ((stop? seed->char seed->next seed)
      (string-unfold stop? seed->char seed->next seed "" (lambda (seed) "")))))
    

;; create and return a string from characters obtained by repeatedly calling seed->char
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Added in 1.0.2
(define string-unfold-right
  (case-lambda
    ((stop? seed->char seed->next seed suffix last-seed->prefix)
      (%string-unfold-assert* 'string-unfold-right stop? seed->char seed->next suffix last-seed->prefix)
      (let %string-unfold-right ((seed seed) (offset (string-length suffix)) (str suffix))
        (if (stop? seed)
          (%string-insert-left! str offset (last-seed->prefix seed))
          (%string-unfold-right (seed->next seed) (fx1+ offset) (%string-insert-left/char! str offset (seed->char seed))))))
    ((stop? seed->char seed->next seed)
      (string-unfold-right stop? seed->char seed->next seed "" (lambda (seed) "")))))


;; create and return a closure that iterates on elements of string str.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in string str and #t,
;; or (values #<unspecified> #f) if end of string is reached.
(define in-string
  (case-lambda
    ((str start end step)
      (assert* 'in-string (fx<=?* 0 start end (string-length str)))
      (assert* 'in-string (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (string-ref str start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #\x0 #f))))
    ((str start end)
      (in-string str start end 1))
    ((str)
      (in-string str 0 (string-length str) 1))))



;; (string-iterate str proc) iterates on all elements of given string str,
;; and calls (proc index elem) on each element. stops iterating if (proc ...) returns #f
;;
;; (proc index elem) can call directly or indirectly functions
;; that inspect the string(s) elements, and can also call (string-set! str ...).
;;
;; It must NOT call any function that modifies the string's length, as for example
;; (string-truncate!)
;;
;; If no string is specified, the loop finishes when body ... evaluates to #f
;;
;; Returns value of last call to (proc index elem), or #t if (proc index elem) was never called.
(define string-iterate
  (case-lambda
    ((str start end proc)
      (assert* 'string-iterate (fx<=?* 0 start end (string-length str)))
      (assert* 'string-iterate (procedure? proc))
      (let %string-iterate ((str str) (proc proc) (ret #t) (i start) (n end))
        (if (fx<? i n)
          (let ((ret (proc i (string-ref str i))))
            (and ret (%string-iterate str proc ret (fx1+ i) n)))
          ret)))
    ((str proc)
      (string-iterate str 0 (string-length str) proc))))


;; Iterate in parallel on elements of given string(s) str ..., and evaluate body ... on each element.
;; Stop iterating when the shortest string is exhausted, or when body ... evaluates to #f
;; If no string is specified, the loop finishes when body ... evaluates to #f
;;
;; Returns value of last body ... evaluation, or #t if body .. was never evaluated.
;;
;; The implementation of body ... can call directly or indirectly functions
;; that inspect or modify the string(s) elements.
;; It must NOT call any function that modifies the string(s) length, as for example (string-truncate!)
;;
;; Added in 1.0.1
(define-syntax for-string
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(for () body ...))
      ((_ elem str body ...)
        (identifier? #'elem)
        #'(string-iterate str (lambda0 (_ elem) body ...)))
      ((_ ((elem str)) body ...)
        #'(string-iterate str (lambda0 (_ elem) body ...)))
      ((_ ((elem str) ...) body ...)
        (with-syntax (((tv ...) (generate-pretty-temporaries #'(str ...))))
          #'(let ((tv str) ...)
              (let %for-string ((i 0) (n (fxmin (string-length tv) ...)) (ret #t))
                (if (fx<? i n)
                  (let ((elem (string-ref tv i)) ...)
                    (let ((ret (begin0 body ...)))
                      (and ret (%for-string (fx1+ i) n ret))))))))))))


;; search string range [start, end) and return index of first character
;; that matches char/char-set/pred
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-index
  (let ((c-string-index-ch (foreign-procedure "c_string_index_ch" (ptr ptr fixnum fixnum) ptr)))
    (case-lambda
      ((str char/char-set/pred start end)
        (assert* 'string-index (string? str))
        (assert* 'string-index (fx<=?* 0 start end (string-length str)))
        (let ((key char/char-set/pred))
          (if (char? key)
            (if (fx<? (fx- end start) 4)
              (do ((i start (fx1+ i)))
                  ((or (fx>=? i end) (char=? key (string-ref str i)))
                    (and (fx<? i end) i)))
              (c-string-index-ch str key start end))
            (let ((pred (if (string? key)
                          (lambda (ch) (string-contains key ch))
                          key)))
              (assert* 'string-index (procedure? pred))
              (assert* 'string-index (logbit? 1 (procedure-arity-mask pred)))
              (do ((i start (fx1+ i)))
                ((or (fx>=? i end) (pred (string-ref str i)))
                  (and (fx<? i end) i)))))))
      ((str char/char-set/pred)
        (string-index str char/char-set/pred 0 (string-length str))))))


;; search string range [start, end) and return index of last character
;; that matches char/char-set/pred
;;
;; returned numerical index will be in the range [start, end).
;; return #f if no such character is found in range.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-index-right
  (let ((c-string-index-right-ch (foreign-procedure "c_string_index_right_ch" (ptr ptr fixnum fixnum) ptr)))
    (case-lambda
      ((str char/char-set/pred start end)
        ;; (debugf "string-index-right str ~s, char/char-set/pred ~s, start ~s, end ~s" str char/char-set/pred start end)
        (assert* 'string-index-right (string? str))
        (assert* 'string-index-right (fx<=?* 0 start end (string-length str)))
        (let ((key char/char-set/pred))
          (if (char? key)
            (if (fx<? (fx- end start) 4)
              (do ((i (fx1- end) (fx1- i)))
                  ((or (fx<? i start) (char=? key (string-ref str i)))
                    (and (fx>=? i start) i)))
              (c-string-index-right-ch str key start end))
            (let ((pred (if (string? key)
                          (lambda (ch) (string-contains key ch))
                          key)))
              (assert* 'string-index-right (procedure? pred))
              (assert* 'string-index-right (logbit? 1 (procedure-arity-mask pred)))
              (do ((i (fx1- end) (fx1- i)))
                  ((or (fx<? i start) (pred (string-ref str i)))
                    (and (fx>=? i start) i)))))))
      ((str char/char-set/pred)
        (string-index-right str char/char-set/pred 0 (string-length str))))))


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
            (let* ((elem     (car l))
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
      (assert* 'string-split (fx<=?* 0 start end (string-length str)))
      (let ((l '()))
        (while start
          (let ((pos (string-index str delim start end)))
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
;; 4. if the original string is empty or only contains delimiters, the returned list will contain zero elements.
(define string-trim-split-at-blanks
  (case-lambda
    ((str)
      (assert* 'string-trim-split-at-blanks (string? str))
      (string-trim-split-at-blanks str 0 (string-length str)))
    ((str start end)
      (assert* 'string-trim-split-at-blanks (string? str))
      (assert* 'string-trim-split-at-blanks (fx<=?* 0 start end (string-length str)))
      (let ((l '())
            (pos-not-blank (or (string-index str char-is-not-blank? start end) end)))
        (while (fx<? pos-not-blank end)
          (let ((pos-blank (string-index str char-is-blank? (fx1+ pos-not-blank) end)))
            (set! l (cons (substring str pos-not-blank (or pos-blank end)) l))
            (if pos-blank
              (set! pos-not-blank (or (string-index str char-is-not-blank? (fx1+ pos-blank) end) end))
              (set! pos-not-blank end))))
        (reverse! l)))))


;; compare the range [left-start, left-start + n) of left string
;; with the range [right-start, right-start + n) of right string.
;; return the leftmost position, starting from 0, containing different characters,
;; or n if the two ranges contain the same characters
(define string-count=
  (let ((c-string-count= (foreign-procedure "c_string_count_equal" (ptr fixnum ptr fixnum fixnum) fixnum)))
    (case-lambda
      ((left left-start right right-start n)
        (assert* 'string-count= (fx<=?* 0 left-start (string-length left)))
        (assert* 'string-count= (fx<=?* 0 right-start (string-length right)))
        (assert* 'string-count= (fx<=?* 0 n (fx- (string-length left) left-start)))
        (assert* 'string-count= (fx<=?* 0 n (fx- (string-length right) right-start)))
        (cond
          ((fxzero? n)
            n)
          ((and (eq? left right) (fx=? left-start right-start))
            n)
          ((fx<? n 4)
            (do ((i 0 (fx1+ i)))
                ((or
                   (fx>=? i n)
                   (not (char=? (string-ref left (fx+ i left-start))
                                (string-ref right (fx+ i right-start)))))
                  i)))
          (else
            (c-string-count= left left-start right right-start n))))

      ((left left-start right right-start)
        (string-count= left left-start right right-start
                       (fxmin (fx- (string-length left) left-start)
                              (fx- (string-length right) right-start))))
      ((left right)
        (string-count= left 0 right 0
                       (fxmin (string-length left) (string-length right)))))))


;; return #t if range [left-start, left-start + n) of left string contains
;; the same characters as range [right-start, right-start + n) of right string.
;; otherwise return #f
(define (substring=? left left-start right right-start n)
  (fx=? n (string-count= left left-start right right-start n)))


(define (substring<? left  left-start  left-end
                     right right-start right-end)
   (assert* 'substring<? (fx<=?* 0 left-start  left-end  (string-length left)))
   (assert* 'substring<? (fx<=?* 0 right-start right-end (string-length right)))
   (let %substring<? ((i left-start) (j right-start))
     (cond
       ((fx=? i left-end)
         (not (fx=? j right-end)))
       ((fx=? j right-end)
         #f)
       (else
         (let ((ch1 (string-ref left i))
               (ch2 (string-ref right j)))
            (cond
              ((char<? ch1 ch2)
                #t)
              ((char>? ch1 ch2)
                #f)
              (else
                (%substring<? (fx1+ i) (fx1+ j)))))))))


;; copy string range [src-start, src-end) to range [dst-start, ...)
;; the two ranges CAN overlap.
(define (substring-move! str src-start src-end dst-start)
  (let ((len (string-length str))
        (dst-end (fx+ dst-start (fx- src-end src-start))))
    (assert* 'substring-move! (fx<=?* 0 src-start src-end len))
    (assert* 'substring-move! (fx<=?* 0 dst-start dst-end len))
    (cond
      ((fx<? dst-start src-start)
        (do ((i src-start (fx1+ i))
             (j dst-start (fx1+ j)))
            ((fx>=? i src-end))
          (string-set! str j (string-ref str i))))
      ((fx>? dst-start src-start)
        (do ((i (fx1- src-end) (fx1- i))
             (j (fx1- dst-end) (fx1- j)))
            ((fx<? i src-start))
          (string-set! str j (string-ref str i)))))))


;; if string str contains specified string key, return index of the first occurrence,
;; otherwise return #f.
;;
;; if optional arguments str-start str-end key-start key-end are specified,
;;   considers only matches that lie entirely in the range [str-start, str-end),
;;   and the returned index is either #f or a fixnum in such range.
(define string-contains
  (let ((c-string-contains (foreign-procedure "c_string_contains" (ptr ptr fixnum fixnum fixnum fixnum) ptr)))
    (case-lambda
      ((str key str-start str-end key-start key-end)
        (assert* 'string-contains (fx<=?* 0 str-start str-end (string-length str)))
        (assert* 'string-contains (fx<=?* 0 key-start key-end (string-length key)))
        (cond
          ((fx=? key-start key-end)
            str-start)
          ((fx<? (fx- str-end str-start) 4)
            (let* ((key-len (fx- key-end key-start))
                   (last    (fx- str-end key-len)))
              (do ((i str-start (fx1+ i)))
                  ((or (fx>? i last) (substring=? str i key key-start key-len))
                    (and (fx<=? i last) i)))))
          (else
            (c-string-contains str key str-start str-end key-start key-end))))
      ((str key)
        (string-contains str key 0 (string-length str) 0 (string-length key))))))


;; return #t if string s1 starts with specified prefix,
;; otherwise return #f.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-prefix?
  (case-lambda
    ((s1 prefix-str-or-char)
      (let ((len1 (string-length s1))
            (s2   prefix-str-or-char))
        (if (char? s2)
          (and (not (fxzero? len1))
               (char=? s2 (string-ref s1 0)))
          (let ((len2 (string-length s2)))
            (and (fx>=? len1 len2)
                 (substring=? s1 0 s2 0 len2))))))
    ((s1 s2 start1 end1 start2 end2)
      (assert* 'string-prefix? (string? s1))
      (assert* 'string-prefix? (string? s2))
      (assert* 'string-prefix? (fx<=?* 0 start1 end1 (string-length s1)))
      (assert* 'string-prefix? (fx<=?* 0 start2 end2 (string-length s2)))
      (let ((len1 (fx- end1 start1))
            (len2 (fx- end2 start2)))
        (and (fx>=? len1 len2)
             (substring=? s1 start1 s2 start2 len2))))))


;; return #t if string s1 ends with specified suffix.
;; otherwise return #f.
;;
;; Conforms to R7RS SRFI 13 String Libraries
;; Modified in 1.0.2
(define string-suffix?
  (case-lambda
    ((s1 suffix-str-or-char)
      (let* ((len1 (string-length s1))
             (s2   suffix-str-or-char))
        (if (char? s2)
          (and (not (fxzero? len1))
               (char=? s2 (string-ref s1 (fx1- len1))))
          (let ((len2 (string-length s2)))
            (and (fx>=? len1 len2)
                 (substring=? s1 (fx- len1 len2) s2 0 len2))))))
    ((s1 s2 start1 end1 start2 end2)
      (assert* 'string-suffix? (string? s1))
      (assert* 'string-suffix? (string? s2))
      (assert* 'string-suffix? (fx<=?* 0 start1 end1 (string-length s1)))
      (assert* 'string-suffix? (fx<=?* 0 start2 end2 (string-length s2)))
      (let ((len1 (fx- end1 start1))
            (len2 (fx- end2 start2)))
        (and (fx>=? len1 len2)
             (substring=? s1 (fx- end1 len2) s2 start2 len2))))))


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


;; try to extract and display name of a procedure object
(define (display-procedure-name proc out)
  (if (procedure? proc)
    (let* ((str (format #f "~a" proc))
           (len (string-length str)))
      (if (and (fx>? len 13)
               (string-prefix? str "#<procedure ")
               (string-suffix? str ">"))
        (display (substring str 12 (fx1- len)) out)
        (display proc out)))
    (display proc out)))

) ; close library
