;;; Copyright (C) 2023 by Massimiliano Ghilardi
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
    list->charspan string->charspan string->charspan* make-charspan charspan->string
    charspan charspan? charspan-length charspan-empty? charspan-clear! charspan-capacity
    charspan-capacity-front charspan-capacity-back charspan-ref charspan-back charspan-set!
    charspan-fill! charspan-fill-range! charspan-copy charspan-copy! charspan=? charspan-range=?
    charspan-reserve-front! charspan-reserve-back! charspan-resize-front! charspan-resize-back!
    charspan-insert-front! charspan-insert-back!
    charspan-csp-insert-front! charspan-csp-insert-back!
    charspan-erase-front! charspan-erase-back! charspan-iterate charspan-find
    charspan-peek-data charspan-peek-beg charspan-peek-end )
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) fx1+ fx1- record-writer string-copy! void)
    (schemesh containers misc))

(define-record-type
  (%charspan %make-charspan charspan?)
  (fields
     (mutable beg charspan-beg charspan-beg-set!)
     (mutable end charspan-end charspan-end-set!)
     (mutable vec charspan-vec charspan-vec-set!))
  (nongenerative #{%charspan b847ikzm9lftljwelbq0cknyh-0}))

(define charspan-peek-beg charspan-beg)
(define charspan-peek-end charspan-end)
(define charspan-peek-data charspan-vec)

(define (list->charspan l)
  (let ((vec (list->string l)))
    (%make-charspan 0 (string-length vec) vec)))

; create charspan copying contents of specified string
(define (string->charspan vec)
  (%make-charspan 0 (string-length vec) (string-copy vec)))

; view existing string as charspan
(define (string->charspan* vec)
  (%make-charspan 0 (string-length vec) vec))

(define (make-charspan n . val)
  (%make-charspan 0 n (apply make-string n val)))

(define (charspan->string sp)
  (let ((beg (charspan-beg sp))
        (end (charspan-end sp)))
    (if (fx>=? beg end)
      ""
      (substring (charspan-vec sp) beg end))))

(define (charspan . vals)
  (list->charspan vals))

(define (charspan-length sp)
  (fx- (charspan-end sp) (charspan-beg sp)))

; return length of internal string, i.e. maximum number of elements
 ; that can be stored without reallocating
(define (charspan-capacity sp)
  (string-length (charspan-vec sp)))

(define (charspan-empty? sp)
  (fx>=? (charspan-beg sp) (charspan-end sp)))

(define (charspan-clear! sp)
  (charspan-beg-set! sp 0)
  (charspan-end-set! sp 0))

(define (charspan-ref sp idx)
  (assert (fx>=? idx 0))
  (assert (fx<? idx (charspan-length sp)))
  (string-ref (charspan-vec sp) (fx+ idx (charspan-beg sp))))

(define (charspan-back sp)
  (assert (not (charspan-empty? sp)))
  (string-ref (charspan-vec sp) (fx1- (charspan-end sp))))

(define (charspan-set! sp idx val)
  (assert (fx>=? idx 0))
  (assert (fx<? idx (charspan-length sp)))
  (string-set! (charspan-vec sp) (fx+ idx (charspan-beg sp)) val))

(define (charspan-fill! sp val)
  (string-fill-range! (charspan-vec sp) (charspan-beg sp) (charspan-length sp) val))

(define (charspan-fill-range! sp start n val)
  (assert (fx>=? start 0))
  (assert (fx>=? n 0))
  (assert (fx<=? (fx+ start n) (charspan-length sp)))
  (string-fill-range! (charspan-vec sp) (fx+ start (charspan-beg sp)) n val))

; make a copy of charspan and return it
(define (charspan-copy src)
  (let* ((n (charspan-length src))
         (dst (make-charspan n)))
    (string-copy! (charspan-vec src) (charspan-beg src)
                  (charspan-vec dst) (charspan-beg dst) n)
    dst))

(define (charspan-copy! src src-start dst dst-start n)
  (assert (fx<=? 0 src-start (fx+ src-start n) (charspan-length src)))
  (assert (fx<=? 0 dst-start (fx+ dst-start n) (charspan-length dst)))
  (string-copy! (charspan-vec src) (fx+ src-start (charspan-beg src))
                (charspan-vec dst) (fx+ dst-start (charspan-beg dst)) n))

(define (charspan=? left right)
  (let ((n1 (charspan-length left))
        (n2 (charspan-length right)))
    (and (fx=? n1 n2)
         (string-range=?
           (charspan-vec left) (charspan-beg left)
           (charspan-vec right) (charspan-beg right)
           n1))))

(define (charspan-range=? left left-start right right-start n)
  (assert (fx<=? 0 left-start  (fx+ left-start n)  (charspan-length left)))
  (assert (fx<=? 0 right-start (fx+ right-start n) (charspan-length right)))
  (string-range=?
    (charspan-vec left)  (fx+ left-start  (charspan-beg left))
    (charspan-vec right) (fx+ right-start (charspan-beg right))
    n))

(define (charspan-reallocate-front! sp len cap)
  (assert (fx>=? len 0))
  (assert (fx>=? cap len))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-vec (charspan-vec sp))
        (new-vec (make-string cap))
        (new-beg (fx- cap len)))
    (string-copy! old-vec (charspan-beg sp) new-vec new-beg copy-len)
    (charspan-beg-set! sp new-beg)
    (charspan-end-set! sp cap)
    (charspan-vec-set! sp new-vec)))

(define (charspan-reallocate-back! sp len cap)
  (assert (fx>=? len 0))
  (assert (fx>=? cap len))
  (let ((copy-len (fxmin len (charspan-length sp)))
        (old-vec (charspan-vec sp))
        (new-vec (make-string cap)))
    (string-copy! old-vec (charspan-beg sp) new-vec 0 copy-len)
    (charspan-beg-set! sp 0)
    (charspan-end-set! sp len)
    (charspan-vec-set! sp new-vec)))

; return distance between begin of internal string and last element
(define (charspan-capacity-front sp)
  (charspan-end sp))

; return distance between first element and end of internal string
(define (charspan-capacity-back sp)
  (fx- (string-length (charspan-vec sp)) (charspan-beg sp)))

; ensure distance between begin of internal string and last element is >= n.
 ; does NOT change the length
(define (charspan-reserve-front! sp len)
  (assert (fx>=? len 0))
  (let ((vec (charspan-vec sp))
        (cap-front (charspan-capacity-front sp)))
    (cond
      ((fx<=? len cap-front)
;      nothing to do
       (void))
      ((fx<=? len (string-length vec))
;       string is large enough, move elements to the back
        (let* ((cap (charspan-capacity sp))
               (old-len (charspan-length sp))
               (new-beg (fx- cap old-len)))
          (string-copy! vec (charspan-beg sp) vec new-beg old-len)
          (charspan-beg-set! sp new-beg)
          (charspan-end-set! sp cap)))
      (#t
;       string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-front))))
         (charspan-reallocate-front! sp (charspan-length sp) new-cap))))))

; ensure distance between first element and end of internal string is >= n.
 ; does NOT change the length
(define (charspan-reserve-back! sp len)
  (assert (fx>=? len 0))
  (let ((vec (charspan-vec sp))
        (cap-back (charspan-capacity-back sp)))
    (cond
      ((fx<=? len cap-back)
;      nothing to do
       (void))
      ((fx<=? len (string-length vec))
;       string is large enough, move elements to the front
        (let ((len (charspan-length sp)))
          (string-copy! vec (charspan-beg sp) vec 0 len)
          (charspan-beg-set! sp 0)
          (charspan-end-set! sp len)))
      (#t
;       string is too small, reallocate it
       (let ((new-cap (fxmax 8 len (fx* 2 cap-back))))
         (charspan-reallocate-back! sp (charspan-length sp) new-cap))))))

; grow or shrink charspan on the left (front), set length to n
(define (charspan-resize-front! sp len)
  (assert (fx>=? len 0))
  (charspan-reserve-front! sp len)
  (assert (fx>=? (charspan-capacity-front sp) len))
  (charspan-beg-set! sp (fx- (charspan-end sp) len)))

; grow or shrink charspan on the right (back), set length to n
(define (charspan-resize-back! sp len)
  (assert (fx>=? len 0))
  (charspan-reserve-back! sp len)
  (assert (fx>=? (charspan-capacity-back sp) len))
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
(define (charspan-csp-insert-front! sp-dst sp-src src-start src-n)
  (assert (not (eq? sp-dst sp-src)))
  (unless (fxzero? src-n)
    (let ((len (charspan-length sp-dst)))
      (charspan-resize-front! sp-dst (fx+ len src-n))
      (charspan-copy! sp-src src-start sp-dst 0 src-n))))

; append a portion of another charspan to this charspan
(define (charspan-csp-insert-back! sp-dst sp-src src-start src-n)
  (assert (not (eq? sp-dst sp-src)))
  (unless (fxzero? src-n)
    (let ((pos (charspan-length sp-dst)))
      (charspan-resize-back! sp-dst (fx+ pos src-n))
      (charspan-copy! sp-src src-start sp-dst pos src-n))))

; erase n elements at the left (front) of charspan
(define (charspan-erase-front! sp n)
  (assert (fx>=? n 0))
  (assert (fx<=? n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-beg-set! sp (fx+ n (charspan-beg sp)))))

; erase n elements at the right (back) of charspan
(define (charspan-erase-back! sp n)
  (assert (fx>=? n 0))
  (assert (fx<=? n (charspan-length sp)))
  (unless (fxzero? n)
    (charspan-end-set! sp (fx- (charspan-end sp) n))))

(define (charspan-iterate sp proc)
  (do ((i (charspan-beg sp) (fx1+ i))
       (n (charspan-end sp))
       (v (charspan-vec sp)))
    ((or (fx>=? i n) (not (proc i (string-ref v i)))))))

;*
 ; (charspan-find) iterates on charspan elements from start to (fxmin (fx+ start n)
 ; (charspan-length sp)), and returns the index of first charspan element that causes
 ; (predicate elem) to return non-#f. Returns #f if no such element is found.

(define (charspan-find sp start n predicate)
  (let ((ret #f))
    (do ((i   start (fx1+ i))
         (end (fxmin (fx+ start n) (charspan-length sp))))
        ((or ret (fx>=? i end)) ret)
      (when (predicate (charspan-ref sp i))
        (set! ret i)))))

; customize how charspan objects are printed
(record-writer (record-type-descriptor %charspan)
  (lambda (sp port writer)
    (display "(string->charspan* " port)
    (write (charspan->string sp) port)
    (display #\) port)))

) ; close library
