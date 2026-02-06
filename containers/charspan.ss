;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  define Scheme type "charspan", a resizeable string  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (scheme2k containers charspan (0 9 3))
  (export
    list->charspan string->charspan string->charspan* make-charspan
    charspan->string charspan->string*!
    charspan charspan? assert-charspan? charspan-length charspan-empty? charspan-clear!
    charspan-capacity charspan-capacity-left charspan-capacity-right
    charspan-ref charspan-ref-right
    charspan-set! charspan-fill! charspan-copy charspan-copy!
    charspan=? charspan<? charspan-count= charspan=? charspan/string=?
    charspan-reserve-left! charspan-reserve-right! charspan-resize-left! charspan-resize-right!
    charspan-insert-left!        charspan-insert-right!
    charspan-insert-left/charspan!  charspan-insert-right/charspan!
    charspan-insert-left/string! charspan-insert-right/string!
    charspan-delete-left!         charspan-delete-right! charspan-iterate in-charspan
    charspan-index charspan-index-right charspan-index/char charspan-index-right/char
    charspan-peek-data charspan-peek-beg charspan-peek-end

    string-replace-all) ; requires (charspan...)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+ fx1- record-writer string-copy! string-truncate! substring-fill! void)
    (only (scheme2k bootstrap)         assert* assert-not* fx<=?*)
    (only (scheme2k containers list)   for-list)
    (only (scheme2k containers string) string-index substring<? substring=? string-count=))


(define-record-type (%charspan %make-charspan charspan?)
  (fields
     (mutable beg charspan-beg charspan-beg-set!)
     (mutable end charspan-end charspan-end-set!)
     (mutable str charspan-str charspan-str-set!))
  (nongenerative %charspan-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define (assert-charspan? who line)
  (unless (charspan? line)
    (assertion-violation who "not a charspan" line)))

(define charspan-peek-beg charspan-beg)
(define charspan-peek-end charspan-end)
(define charspan-peek-data charspan-str)

(define (list->charspan l)
  (let ((str (list->string l)))
    (%make-charspan 0 (string-length str) str)))

;; create charspan copying contents of specified string
(define (string->charspan str)
  (%make-charspan 0 (string-length str) (string-copy str)))

;; view existing string as charspan
(define (string->charspan* str)
  (%make-charspan 0 (string-length str) str))

;; create a charspan containing n characters.
;; If char is specified, the charspan is filled with it.
;; Otherwise it is filled with #\x0 i.e. codepoint 0.
(define make-charspan
  (case-lambda
    ((n)      (%make-charspan 0 n (make-string n #\x0)))
    ((n char) (%make-charspan 0 n (make-string n char)))))


;; convert a portion of charspan to string
(define charspan->string
  (case-lambda
    ((sp start end)
      (assert* 'charspan->string (fx<=?* 0 start end (charspan-length sp)))
      (if (fx<? start end)
        (let ((offset (charspan-beg sp)))
          (substring (charspan-str sp) (fx+ offset start) (fx+ offset end)))
        ""))
    ((sp)
      (charspan->string sp 0 (charspan-length sp)))))


;; if possible, truncate charspan to its length and view it as a string.
;; otherwise convert it to string as (charspan->string) does.
(define (charspan->string*! sp)
  (if (or (charspan-empty? sp) (not (fxzero? (charspan-beg sp))))
    (charspan->string sp)
    (string-truncate! (charspan-str sp) (charspan-end sp))))

(define (charspan . charlist)
  (list->charspan charlist))

(define (charspan-length sp)
  (fx- (charspan-end sp) (charspan-beg sp)))

;; return length of internal string, i.e. maximum number of elements
;; that can be stored without reallocating
(define (charspan-capacity sp)
  (string-length (charspan-str sp)))

(define (charspan-empty? sp)
  (fx>=? (charspan-beg sp) (charspan-end sp)))

(define (charspan-clear! sp)
  (charspan-beg-set! sp 0)
  (charspan-end-set! sp 0))

(define charspan-ref
  (case-lambda
    ((sp idx)
      (assert* 'charspan-ref (fx<? -1 idx (charspan-length sp)))
      (string-ref (charspan-str sp) (fx+ idx (charspan-beg sp))))
    ((sp)
      (charspan-ref sp 0))))

(define charspan-ref-right
  (case-lambda
    ((sp idx)
      (assert* 'charspan-ref-right (fx<? -1 idx (charspan-length sp)))
      (let ((pos (fx- (charspan-length sp) (fx1+ idx))))
        (string-ref (charspan-str sp) (fx+ pos (charspan-beg sp)))))
    ((sp)
      (charspan-ref-right sp 0))))

(define (charspan-set! sp idx ch)
  (assert* 'charspan-set! (fx<? -1 idx (charspan-length sp)))
  (string-set! (charspan-str sp) (fx+ idx (charspan-beg sp)) ch))

(define charspan-fill!
  (case-lambda
    ((sp ch)
      (substring-fill! (charspan-str sp) (charspan-beg sp) (charspan-end sp) ch))
    ((sp start end ch)
      (assert* 'charspan-fill! (fx<=?* 0 start end (charspan-length sp)))
      (let ((offset (charspan-beg sp)))
        (substring-fill! (charspan-str sp) (fx+ start offset) (fx+ end offset) ch)))))

;; make a copy of charspan and return it
(define (charspan-copy src)
  (let* ((n (charspan-length src))
         (dst (make-charspan n)))
    (string-copy! (charspan-str src) (charspan-beg src)
                  (charspan-str dst) (charspan-beg dst) n)
    dst))

;; copy n characters from a charspan to another one
(define (charspan-copy! src src-start dst dst-start n)
  (assert* 'charspan-copy! (fx<=?* 0 src-start (fx+ src-start n) (charspan-length src)))
  (assert* 'charspan-copy! (fx<=?* 0 dst-start (fx+ dst-start n) (charspan-length dst)))
  (string-copy! (charspan-str src) (fx+ src-start (charspan-beg src))
                (charspan-str dst) (fx+ dst-start (charspan-beg dst)) n))


;; compare two charspans
(define (charspan<? left right)
  (substring<?
    (charspan-str left)  (charspan-beg left)  (charspan-end left)
    (charspan-str right) (charspan-beg right) (charspan-end right)))



;; compare two charspans, or a range of two charspans
(define charspan=?
  (case-lambda
    ((left left-start right right-start n)
      (assert* 'charspan=? (fx<=?* 0 left-start (fx+ left-start n) (charspan-length left)))
      (assert* 'charspan=? (fx<=?* 0 right-start (fx+ left-start n) (charspan-length right)))
      (substring=?
        (charspan-str left)  (fx+ left-start  (charspan-beg left))
        (charspan-str right) (fx+ right-start (charspan-beg right))
        n))
    ((left left-start right right-start)
      (let ((n1 (fx- (charspan-length left)  left-start))
            (n2 (fx- (charspan-length right) right-start)))
        (and (fx=? n1 n2)
             (charspan=? left left-start right right-start n1))))
    ((left right)
      (charspan=? left 0 right 0))))


;; compare a range of a charspan and a string
(define charspan/string=?
  (case-lambda
    ((left left-start right right-start n)
      (assert* 'charspan/string=? (fx<=?* 0 left-start  (fx+ left-start n)  (charspan-length left)))
      (assert* 'charspan/string=? (fx<=?* 0 right-start (fx+ right-start n) (string-length right)))
      (substring=?
        (charspan-str left)  (fx+ left-start  (charspan-beg left))
        right right-start
        n))
    ((left left-start right right-start)
      (let ((n1 (fx- (charspan-length left)  left-start))
            (n2 (fx- (string-length right)  right-start)))
        (and (fx=? n1 n2)
             (charspan/string=? left left-start right right-start n1))))
    ((left right)
      (charspan/string=? left 0 right 0))))


;; compare the range [left-start, left-start + n) of left charspan
;; with the range [right-start, right-start + n) of right charspan.
;; return the leftmost position, starting from 0, containing different characters,
;; or n if the two ranges contain the same characters
(define charspan-count=
  (case-lambda
    ((left left-start right right-start n)
      (assert* 'charspan-count= (fx<=?* 0 left-start  (fx+ left-start n)  (charspan-length left)))
      (assert* 'charspan-count= (fx<=?* 0 right-start (fx+ right-start n) (charspan-length right)))
      (string-count=
        (charspan-str left)  (fx+ left-start  (charspan-beg left))
        (charspan-str right) (fx+ right-start (charspan-beg right))
        n))
    ((left left-start right right-start)
      (charspan-count= left left-start right right-start
                       (fxmin (fx- (charspan-length left) left-start)
                              (fx- (charspan-length right) right-start))))
    ((left right)
      (charspan-count= left 0 right 0
                       (fxmin (charspan-length left)
                              (charspan-length right))))))


(define (charspan-reallocate-left! sp len cap)
  (assert* 'charspan-reallocate-left! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-str (charspan-str sp))
        (new-str (make-string cap))
        (new-beg (fx- cap len)))
    (string-copy! old-str (charspan-beg sp) new-str new-beg copy-len)
    (charspan-beg-set! sp new-beg)
    (charspan-end-set! sp cap)
    (charspan-str-set! sp new-str)))

(define (charspan-reallocate-right! sp len cap)
  (assert* 'charspan-reallocate-right! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-str (charspan-str sp))
        (new-str (make-string cap)))
    (string-copy! old-str (charspan-beg sp) new-str 0 copy-len)
    (charspan-beg-set! sp 0)
    (charspan-end-set! sp len)
    (charspan-str-set! sp new-str)))

;; return distance between begin of internal string and last element
(define (charspan-capacity-left sp)
  (charspan-end sp))

;; return distance between first element and end of internal string
(define (charspan-capacity-right sp)
  (fx- (string-length (charspan-str sp)) (charspan-beg sp)))

;; ensure distance between begin of internal string and last element is >= n.
;; does NOT change the length
(define (charspan-reserve-left! sp len)
  (assert* 'charspan-reserve-left! (fx>=? len 0))
  (let ((str (charspan-str sp))
        (cap-left (charspan-capacity-left sp)))
    (cond
      ((fx<=? len cap-left)
       ; nothing to do
       (void))
      ((fx<=? len (string-length str))
        ; string is large enough, move elements to the back
        (let* ((cap (charspan-capacity sp))
               (old-len (charspan-length sp))
               (new-beg (fx- cap old-len)))
          (string-copy! str (charspan-beg sp) str new-beg old-len)
          (charspan-beg-set! sp new-beg)
          (charspan-end-set! sp cap)))
      (else
       ; string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-left))))
         (charspan-reallocate-left! sp (charspan-length sp) new-cap))))))

;; ensure distance between first element and end of internal string is >= n.
;; does NOT change the length
(define (charspan-reserve-right! sp len)
  (assert* 'charspan-reserve-right! (fx>=? len 0))
  (let ((str (charspan-str sp))
        (cap-right (charspan-capacity-right sp)))
    (cond
      ((fx<=? len cap-right)
       ; nothing to do
       (void))
      ((fx<=? len (string-length str))
        ; string is large enough, move elements to the front
        (let ((len (charspan-length sp)))
          (string-copy! str (charspan-beg sp) str 0 len)
          (charspan-beg-set! sp 0)
          (charspan-end-set! sp len)))
      (else
       ; string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-right))))
         (charspan-reallocate-right! sp (charspan-length sp) new-cap))))))

;; grow or shrink charspan on the left (front), set length to n
(define (charspan-resize-left! sp len)
  (assert* 'charspan-resize-left! (fx>=? len 0))
  (charspan-reserve-left! sp len)
  (assert* 'charspan-resize-left! (fx>=? (charspan-capacity-left sp) len))
  (charspan-beg-set! sp (fx- (charspan-end sp) len)))

;; grow or shrink charspan on the right (back), set length to n
(define (charspan-resize-right! sp len)
  (assert* 'charspan-resize-right! (fx>=? len 0))
  (charspan-reserve-right! sp len)
  (assert* 'charspan-resize-right! (fx>=? (charspan-capacity-right sp) len))
  (charspan-end-set! sp (fx+ len (charspan-beg sp))))

(define charspan-insert-left!
  (case-lambda
    ((sp)
      (void))
    ((sp ch)
      (charspan-resize-left! sp (fx1+ (charspan-length sp)))
      (charspan-set! sp 0 ch))
    ((sp ch1 ch2)
      (charspan-resize-left! sp (fx+ 2 (charspan-length sp)))
      (charspan-set! sp 0 ch1)
      (charspan-set! sp 1 ch2))
    ((sp . charlist)
      (let ((pos 0)
            (new-len (fx+ (charspan-length sp) (length charlist))))
        (charspan-resize-left! sp new-len)
        (for-list ((ch charlist))
          (charspan-set! sp pos ch)
          (set! pos (fx1+ pos)))))))


(define charspan-insert-right!
  (case-lambda
    ((sp)
      (void))
    ((sp ch)
      (let ((pos (charspan-length sp)))
        (charspan-resize-right! sp (fx1+ pos))
        (charspan-set! sp pos ch)))
    ((sp ch1 ch2)
      (let ((pos (charspan-length sp)))
        (charspan-resize-right! sp (fx+ 2 pos))
        (charspan-set! sp pos ch1)
        (charspan-set! sp (fx1+ pos) ch2)))
    ((sp . charlist)
      (let* ((pos (charspan-length sp))
             (new-len (fx+ pos (length charlist))))
        (charspan-resize-right! sp new-len)
        (for-list ((ch charlist))
          (charspan-set! sp pos ch)
          (set! pos (fx1+ pos)))))))


;; insert range [start, end) of charspan sp-src at the beginning of charspan sp-dst
(define charspan-insert-left/charspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert* 'charspan-insert-left/charspan! (fx<=?* 0 src-start src-end (charspan-length sp-src)))
      (assert-not* 'charspan-insert-left/charspan! (eq? sp-dst sp-src))
      (when (fx<? src-start src-end)
        (let ((src-n (fx- src-end src-start)))
          (charspan-resize-left! sp-dst (fx+ src-n (charspan-length sp-dst)))
          (charspan-copy! sp-src src-start sp-dst 0 src-n))))
    ((sp-dst sp-src)
      (charspan-insert-left/charspan! sp-dst sp-src 0 (charspan-length sp-src)))))


;; append range [start, end) of charspan sp-src at the end of charspan sp-dst
(define charspan-insert-right/charspan!
  (case-lambda
    ((sp-dst sp-src src-start src-end)
      (assert* 'charspan-insert-right/charspan! (fx<=?* 0 src-start src-end (charspan-length sp-src)))
      (assert-not* 'charspan-insert-right/charspan! (eq? sp-dst sp-src))
      (when (fx<? src-start src-end)
        (let ((pos (charspan-length sp-dst))
              (src-n (fx- src-end src-start)))
          (charspan-resize-right! sp-dst (fx+ pos src-n))
          (charspan-copy! sp-src src-start sp-dst pos src-n))))
    ((sp-dst sp-src)
      (charspan-insert-right/charspan! sp-dst sp-src 0 (charspan-length sp-src)))))


;; insert range [start, end) of string str-src at the beginning of charspan sp-dst
(define charspan-insert-left/string!
  (case-lambda
    ((sp-dst str-src src-start src-end)
      (assert* 'charspan-insert-left/string! (fx<=?* 0 src-start src-end (string-length str-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty string is a common optimization of Scheme compilers
        (assert-not* 'charspan-insert-left/string! (eq? (charspan-str sp-dst) str-src))
        (let ((src-n (fx- src-end src-start)))
          (charspan-resize-left! sp-dst (fx+ src-n (charspan-length sp-dst)))
          (string-copy! str-src src-start
                        (charspan-str sp-dst) (charspan-beg sp-dst)
                        src-n))))
    ((sp-dst str-src)
      (charspan-insert-left/string! sp-dst str-src 0 (string-length str-src)))))


;; append range [start, end) of string str-src at the end of charspan sp-dst
(define charspan-insert-right/string!
  (case-lambda
    ((sp-dst str-src src-start src-end)
      (assert* 'charspan-insert-right/string! (fx<=?* 0 src-start src-end (string-length str-src)))
      (when (fx<? src-start src-end)
        ;; check for (not (eq? src dst)) only if dst is non-empty,
        ;; because reusing the empty string is a common optimization of Scheme compilers
        (assert-not* 'charspan-insert-right/string! (eq? (charspan-str sp-dst) str-src))
        (let ((pos (charspan-length sp-dst))
              (src-n (fx- src-end src-start)))
          (charspan-resize-right! sp-dst (fx+ pos src-n))
          (string-copy! str-src src-start
                        (charspan-str sp-dst) (fx- (charspan-end sp-dst) src-n)
                        src-n))))
    ((sp-dst str-src)
      (charspan-insert-right/string! sp-dst str-src 0 (string-length str-src)))))


;; erase n elements at the left (front) of charspan
(define (charspan-delete-left! sp n)
  (assert* 'charspan-delete-left! (fx<=? 0 n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-beg-set! sp (fx+ n (charspan-beg sp)))))

;; erase n elements at the right (back) of charspan
(define (charspan-delete-right! sp n)
  (assert* 'charspan-delete-right! (fx<=? 0 n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-end-set! sp (fx- (charspan-end sp) n))))


;; create and return a closure that iterates on elements of charspan sp.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in charspan sp and #t,
;; or (values #<unspecified> #f) if end of charspan is reached.
(define in-charspan
  (case-lambda
    ((sp start end step)
      (assert* 'in-charspan (fx<=?* 0 start end (charspan-length sp)))
      (assert* 'in-charspan (fx>=? step 0))
      (let ((%in-charspan ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (charspan-ref sp start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\x0 #f)))))
        %in-charspan))
    ((sp start end)
      (in-charspan sp start end 1))
    ((sp)
      (in-charspan sp 0 (charspan-length sp) 1))))


(define (charspan-iterate sp proc)
  (let ((start (charspan-beg sp))
        (end   (charspan-end sp))
        (s     (charspan-str sp)))
    (do ((i start (fx1+ i)))
      ((or (fx>=? i end) (not (proc (fx- i start) (string-ref s i))))
        (fx>=? i end)))))

;; iterate on charspan elements in range [start, end) and return the index
;; of first element that causes (predicate elem) to return truish.
;;
;; Return #f if no such element is found.
(define charspan-index
  (case-lambda
    ((sp start end predicate)
      (assert* 'charspan-index (fx<=?* 0 start end (charspan-length sp)))
      (do ((i start (fx1+ i)))
          ((or (fx>=? i end) (predicate (charspan-ref sp i)))
            (if (fx>=? i end) #f i))))
    ((sp predicate)
      (charspan-index sp 0 (charspan-length sp) predicate))))


;; iterate backward on charspan elements in the range [start, end)
;; and return the index of first (i.e. the highest index) charspan element that causes
;; (predicate elem) to return truish. Returns #f if no such element is found.
(define charspan-index-right
  (case-lambda
    ((sp start end predicate)
      (assert* 'charspan-index-right (fx<=?* 0 start end (charspan-length sp)))
      (do ((i (fx1- end) (fx1- i)))
          ((or (fx<? i start) (predicate (charspan-ref sp i)))
            (if (fx<? i start) #f i))))
    ((sp predicate)
      (charspan-index-right sp 0 (charspan-length sp) predicate))))


;; iterate on charspan elements in range [start, end) and return
;; the index of first charspan element equal to ch.
;;
;; Return #f if no such element is found.
(define charspan-index/char
  (case-lambda
    ((sp start end ch)
      (charspan-index sp start end (lambda (e) (char=? e ch))))
    ((sp ch)
      (charspan-index sp (lambda (e) (char=? e ch))))))


;; iterate backward on charspan elements in the range [start, end)
;; and return the index of first (i.e. the highest index) charspan element equal to ch.
;; Returns #f if no such element is found.
(define charspan-index-right/char
  (case-lambda
    ((sp start end ch)
      (charspan-index-right sp start end (lambda (e) (char=? e ch))))
    ((sp ch)
      (charspan-index-right sp (lambda (e) (char=? e ch))))))



;; create and return a copy of str, where all occurrences of old-str have been replaced by new-str.
(define string-replace-all
  (case-lambda
    ((str old-str new-str start end)
      (assert* 'string-replace-all (fx<=?* 0 start end (string-length str)))
      (assert* 'string-replace-all (string? old-str))
      (assert* 'string-replace-all (string? new-str))
      (assert-not* 'string-replace-all (fxzero? (string-length old-str)))
      (let ((dst (charspan))
            (ch0 (string-ref old-str 0))
            (old-len (string-length old-str)))
        (charspan-insert-right/string! dst str 0 start)
        (let %loop ((i start))
          (let ((pos (string-index str ch0 i end)))
            ;; (debugf "string-replace-all dst=~s str=~s i=~s pos=~s end=~s old=~s new=~s" (charspan->string dst) str i pos end old-str new-str)
            (cond
              ((and pos
                    (fx<=? pos (fx- end old-len))
                    (substring=? str pos old-str 0 old-len))
                ;; found a match, replace it
                (charspan-insert-right/string! dst str i pos)
                (charspan-insert-right/string! dst new-str)
                (%loop (fx+ pos old-len)))
              (pos
                ;; (string-ref str pos) matches ch0, but following characters don't match
                (charspan-insert-right/string! dst str i (fx1+ pos))
                (%loop (fx1+ pos)))
              (else
                ;; no match, copy all remaining characters from str to dst
                (charspan-insert-right/string! dst str i end)
                (charspan->string*! dst)))))))
    ((str old-str new-str)
      (string-replace-all str old-str new-str  0 (string-length str)))))



;; customize how charspan objects are printed
(record-writer (record-type-descriptor %charspan)
  (lambda (sp port writer)
    (display "(string->charspan* " port)
    (write (charspan->string sp) port)
    (display ")" port)))

) ; close library
