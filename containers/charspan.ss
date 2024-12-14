;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  define Scheme type "charspan", a resizeable string  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (schemesh containers charspan (0 1))
  (export
    list->charspan string->charspan string->charspan* make-charspan charspan->string charspan->string/range
    charspan charspan? assert-charspan? charspan-length charspan-empty? charspan-clear!
    charspan-capacity charspan-capacity-front charspan-capacity-back charspan-ref
    charspan-front charspan-back
    charspan-set! charspan-fill! charspan-fill-range! charspan-copy charspan-copy!
    charspan=? charspan-range=? charspan-range/string=?
    charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back!
    charspan-insert-front! charspan-insert-back!
    charspan-insert-front/cspan! charspan-insert-back/cspan!
    charspan-erase-front! charspan-erase-back! charspan-iterate
    charspan-find charspan-rfind charspan-find/ch charspan-rfind/ch
    charspan-peek-data charspan-peek-beg charspan-peek-end )
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+ fx1- record-writer string-copy! void)
    (only (schemesh bootstrap) assert*)
    (schemesh containers misc))

(define-record-type
  (%charspan %make-charspan charspan?)
  (fields
     (mutable beg charspan-beg charspan-beg-set!)
     (mutable end charspan-end charspan-end-set!)
     (mutable str charspan-str charspan-str-set!))
  (nongenerative #{%charspan b847ikzm9lftljwelbq0cknyh-0}))

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
;; Otherwise it is filled with #\nul i.e. codepoint 0.
(define make-charspan
  (case-lambda
    ((n)      (%make-charspan 0 n (make-string n #\nul)))
    ((n char) (%make-charspan 0 n (make-string n char)))))

;; convert a charspan to string
(define (charspan->string sp)
  (let ((beg (charspan-beg sp))
        (end (charspan-end sp)))
    (if (fx<? beg end)
      (substring (charspan-str sp) beg end)
      "")))

;; convert a portion of charspan to string
(define (charspan->string/range sp start len)
  (let ((n (charspan-length sp)))
    (if (fx<=? n 0)
      ""
      (let* ((beg (charspan-beg sp))
             (i (fxmin n (fxmax 0 start)))
             (j (fxmin n (fxmax i (fx+ start len)))))
        (if (fx>=? i j)
          ""
          (substring (charspan-str sp) (fx+ i beg) (fx+ j beg)))))))

(define (charspan . vals)
  (list->charspan vals))

(define (charspan-length sp)
  (fx- (charspan-end sp) (charspan-beg sp)))

; return length of internal string, i.e. maximum number of elements
 ; that can be stored without reallocating
(define (charspan-capacity sp)
  (string-length (charspan-str sp)))

(define (charspan-empty? sp)
  (fx>=? (charspan-beg sp) (charspan-end sp)))

(define (charspan-clear! sp)
  (charspan-beg-set! sp 0)
  (charspan-end-set! sp 0))

(define (charspan-ref sp idx)
  (assert* 'charspan-ref (fx<? -1 idx (charspan-length sp)))
  (string-ref (charspan-str sp) (fx+ idx (charspan-beg sp))))

(define (charspan-front sp)
  (assert* 'charspan-front (not (charspan-empty? sp)))
  (string-ref (charspan-str sp) (charspan-beg sp)))

(define (charspan-back sp)
  (assert* 'charspan-back (not (charspan-empty? sp)))
  (string-ref (charspan-str sp) (fx1- (charspan-end sp))))

(define (charspan-set! sp idx val)
  (assert* 'charspan-set! (fx<? -1 idx (charspan-length sp)))
  (string-set! (charspan-str sp) (fx+ idx (charspan-beg sp)) val))

(define (charspan-fill! sp val)
  (string-fill-range! (charspan-str sp) (charspan-beg sp) (charspan-length sp) val))

(define (charspan-fill-range! sp start n val)
  (assert* 'charspan-fill-range! (fx>=? start 0))
  (assert* 'charspan-fill-range! (fx>=? n 0))
  (assert* 'charspan-fill-range! (fx<=? (fx+ start n) (charspan-length sp)))
  (string-fill-range! (charspan-str sp) (fx+ start (charspan-beg sp)) n val))

; make a copy of charspan and return it
(define (charspan-copy src)
  (let* ((n (charspan-length src))
         (dst (make-charspan n)))
    (string-copy! (charspan-str src) (charspan-beg src)
                  (charspan-str dst) (charspan-beg dst) n)
    dst))

(define (charspan-copy! src src-start dst dst-start n)
  (assert* 'charspan-copy! (fx<=? 0 src-start (fx+ src-start n) (charspan-length src)))
  (assert* 'charspan-copy! (fx<=? 0 dst-start (fx+ dst-start n) (charspan-length dst)))
  (string-copy! (charspan-str src) (fx+ src-start (charspan-beg src))
                (charspan-str dst) (fx+ dst-start (charspan-beg dst)) n))

(define (charspan=? left right)
  (let ((n1 (charspan-length left))
        (n2 (charspan-length right)))
    (and (fx=? n1 n2)
         (string-range=?
           (charspan-str left) (charspan-beg left)
           (charspan-str right) (charspan-beg right)
           n1))))

;; compare a range of two charspans
(define (charspan-range=? left left-start right right-start n)
  (assert* 'charspan-range=? (fx<=? 0 left-start  (fx+ left-start n)  (charspan-length left)))
  (assert* 'charspan-range=? (fx<=? 0 right-start (fx+ right-start n) (charspan-length right)))
  (string-range=?
    (charspan-str left)  (fx+ left-start  (charspan-beg left))
    (charspan-str right) (fx+ right-start (charspan-beg right))
    n))

;; compare a range of a charspan and a string
(define (charspan-range/string=? left left-start right right-start n)
  (assert* 'charspan-range=? (fx<=? 0 left-start  (fx+ left-start n)  (charspan-length left)))
  (assert* 'charspan-range=? (fx<=? 0 right-start (fx+ right-start n) (string-length right)))
  (string-range=?
    (charspan-str left)  (fx+ left-start  (charspan-beg left))
    right right-start
    n))

(define (charspan-reallocate-front! sp len cap)
  (assert* 'charspan-reallocate-front! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-str (charspan-str sp))
        (new-str (make-string cap))
        (new-beg (fx- cap len)))
    (string-copy! old-str (charspan-beg sp) new-str new-beg copy-len)
    (charspan-beg-set! sp new-beg)
    (charspan-end-set! sp cap)
    (charspan-str-set! sp new-str)))

(define (charspan-reallocate-back! sp len cap)
  (assert* 'charspan-reallocate-back! (fx<=? 0 len cap))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-str (charspan-str sp))
        (new-str (make-string cap)))
    (string-copy! old-str (charspan-beg sp) new-str 0 copy-len)
    (charspan-beg-set! sp 0)
    (charspan-end-set! sp len)
    (charspan-str-set! sp new-str)))

;; return distance between begin of internal string and last element
(define (charspan-capacity-front sp)
  (charspan-end sp))

;; return distance between first element and end of internal string
(define (charspan-capacity-back sp)
  (fx- (string-length (charspan-str sp)) (charspan-beg sp)))

;; ensure distance between begin of internal string and last element is >= n.
;; does NOT change the length
(define (charspan-reserve-front! sp len)
  (assert* 'charspan-reserve-front! (fx>=? len 0))
  (let ((str (charspan-str sp))
        (cap-front (charspan-capacity-front sp)))
    (cond
      ((fx<=? len cap-front)
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
      (#t
       ; string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))
         (charspan-reallocate-front! sp (charspan-length sp) new-cap))))))

;; ensure distance between first element and end of internal string is >= n.
;; does NOT change the length
(define (charspan-reserve-back! sp len)
  (assert* 'charspan-reserve-back! (fx>=? len 0))
  (let ((str (charspan-str sp))
        (cap-back (charspan-capacity-back sp)))
    (cond
      ((fx<=? len cap-back)
       ; nothing to do
       (void))
      ((fx<=? len (string-length str))
        ; string is large enough, move elements to the front
        (let ((len (charspan-length sp)))
          (string-copy! str (charspan-beg sp) str 0 len)
          (charspan-beg-set! sp 0)
          (charspan-end-set! sp len)))
      (#t
       ; string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))
         (charspan-reallocate-back! sp (charspan-length sp) new-cap))))))

; grow or shrink charspan on the left (front), set length to n
(define (charspan-resize-front! sp len)
  (assert* 'charspan-resize-front! (fx>=? len 0))
  (charspan-reserve-front! sp len)
  (assert* 'charspan-resize-front! (fx>=? (charspan-capacity-front sp) len))
  (charspan-beg-set! sp (fx- (charspan-end sp) len)))

; grow or shrink charspan on the right (back), set length to n
(define (charspan-resize-back! sp len)
  (assert* 'charspan-resize-back! (fx>=? len 0))
  (charspan-reserve-back! sp len)
  (assert* 'charspan-resize-back! (fx>=? (charspan-capacity-back sp) len))
  (charspan-end-set! sp (fx+ len (charspan-beg sp))))

(define (charspan-insert-front! sp . vals)
  (unless (null? vals)
    (let ((pos 0)
          (new-len (fx+ (charspan-length sp) (length vals))))
      (charspan-resize-front! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (charspan-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

(define (charspan-insert-back! sp . vals)
  (unless (null? vals)
    (let* ((pos (charspan-length sp))
           (new-len (fx+ pos (length vals))))
      (charspan-resize-back! sp new-len)
      (list-iterate vals
        (lambda (elem)
          (charspan-set! sp pos elem)
          (set! pos (fx1+ pos)))))))

; prefix a portion of another charspan to this charspan
(define (charspan-insert-front/cspan! sp-dst sp-src src-start src-n)
  (assert* 'charspan-insert-front/cspan! (not (eq? sp-dst sp-src)))
  (unless (fxzero? src-n)
    (let ((len (charspan-length sp-dst)))
      (charspan-resize-front! sp-dst (fx+ len src-n))
      (charspan-copy! sp-src src-start sp-dst 0 src-n))))

; append a portion of another charspan to this charspan
(define (charspan-insert-back/cspan! sp-dst sp-src src-start src-n)
  (assert* 'charspan-insert-back/cspan! (not (eq? sp-dst sp-src)))
  (unless (fxzero? src-n)
    (let ((pos (charspan-length sp-dst)))
      (charspan-resize-back! sp-dst (fx+ pos src-n))
      (charspan-copy! sp-src src-start sp-dst pos src-n))))

; erase n elements at the left (front) of charspan
(define (charspan-erase-front! sp n)
  (assert* 'charspan-erase-front! (fx<=? 0 n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-beg-set! sp (fx+ n (charspan-beg sp)))))

; erase n elements at the right (back) of charspan
(define (charspan-erase-back! sp n)
  (assert* 'charspan-erase-back! (fx<=? 0 n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-end-set! sp (fx- (charspan-end sp) n))))

(define (charspan-iterate sp proc)
  (do ((i (charspan-beg sp) (fx1+ i))
       (n (charspan-end sp))
       (v (charspan-str sp)))
    ((or (fx>=? i n) (not (proc i (string-ref v i)))))))

;; iterate on charspan elements from start to (fx+ start n)
;; and return the index of first charspan element that causes
;; (predicate elem) to return non-#f. Returns #f if no such element is found.
(define (charspan-find sp start n predicate)
  (let* ((clen  (charspan-length sp))
         (start (fxmin clen (fxmax 0 start)))
         (end   (fxmin clen (fx+ start n)))
         (ret #f))
    (do ((i start (fx1+ i)))
        ((or ret (fx>=? i end)) ret)
      (when (predicate (charspan-ref sp i))
        (set! ret i)))))

;; iterate backward on charspan elements from (fx1- (fx+ start n)) to start
;; and return the index of first (i.e. the highest index) charspan element that causes
;; (predicate elem) to return non-#f. Returns #f if no such element is found.
(define (charspan-rfind sp start n predicate)
  (let* ((clen  (charspan-length sp))
         (start (fxmin clen (fxmax 0 start)))
         (end   (fxmin clen (fx+ start n)))
         (ret #f))
    (do ((i  (fx1- end) (fx1- i)))
        ((or ret (fx<? i start)) ret)
      (when (predicate (charspan-ref sp i))
        (set! ret i)))))

;; iterate on charspan elements from start to (fx+ start n)
;; and return the index of first charspan element equal to ch.
;; Returns #f if no such element is found.
(define (charspan-find/ch sp start n ch)
  (charspan-find sp start n (lambda (e) (char=? e ch))))

;; iterate backward on charspan elements from (fx1- (fx+ start n)) to start
;; and return the index of first (i.e. the highest index) charspan element equal to ch.
;; Returns #f if no such element is found.
(define (charspan-rfind/ch sp start n ch)
  (charspan-rfind sp start n (lambda (e) (char=? e ch))))

; customize how charspan objects are printed
(record-writer (record-type-descriptor %charspan)
  (lambda (sp port writer)
    (display "(string->charspan* " port)
    (write (charspan->string sp) port)
    (display ")" port)))

) ; close library
