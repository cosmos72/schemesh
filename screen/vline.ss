;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh screen vline (0 9 0))
  (export
    vline vline? assert-vline? vline->string
    vline-nl? vline-copy-on-write vline-empty?
    vline-length vline-ref vline-ref/char vline-at vline-at/char
    vline-equal/chars? vline-set! vline-clear!
    vline-delete! vline-insert-at! vline-insert-at/vcellspan! vline-insert-at/vbuffer!
    vline-index vline-index-right vline-index/char vline-count vline-count-right
    vline-dirty-start-x vline-dirty-end-x vline-dirty-x-add! vline-dirty-x-unset!
    in-vline vline-iterate vline-display/bytespan vline-write)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme)           fx1+ fx1- record-writer string-copy!)
    (only (schemesh bootstrap)   assert* fx<=?*)
    (schemesh screen vcell)
    (schemesh screen vcellspan)
    (schemesh screen vbuffer))

;; copy-pasted from containers/vbuffer.ss
(define-record-type (%vbuffer %make-vbuffer %vbuffer?)
  (fields
     (mutable left  cl< vbuffer-left-set!)
     (mutable right cl> vbuffer-right-set!))
  (nongenerative %vbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))

;; type vline is a cell gap-buffer with additional fields:
;;
;; - vline-share a cons. its car will be > 0 if the vline is shared copy-on-write
;;   between two or more vlines: its content will be automatically cloned at the first
;;   attempt to modify it.
;; - vline-dirty-start-x and vline-dirty-end-x fixnums indicating the range of cells
;;   that were recently modified and not yet redrawn

(define-record-type (%vline %make-vline vline?)
  (parent %vbuffer)
  (fields
    (mutable share) ; a cons (share-count . #f)
    (mutable dirty-start-x vline-dirty-start-x vline-dirty-start-x-set!)
    (mutable dirty-end-x   vline-dirty-end-x   vline-dirty-end-x-set!))
  (nongenerative %vline-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define (assert-vline? who line)
  (unless (vline? line)
    (assertion-violation who "not a vline" line)))

(define (make-vline left-span right-span)
  (assert* 'make-vline (vcellspan? left-span))
  (assert* 'make-vline (vcellspan? right-span))
  (%make-vline left-span right-span (cons 0 #f) (greatest-fixnum) 0))

;; increment vline share count by 1.
;; return pair containing share count
(define (vline-share-inc! line)
  (let ((pair (%vline-share line)))
    (set-car! pair (fx1+ (car pair)))
    pair))

;; decrement vline share count by 1.
;; return #t if vline was shared, otherwise return #f
(define (vline-share-dec! line)
  (let* ((pair (%vline-share line))
         (count (car pair))
         (shared? (fx>? count 0)))
    (when shared?
      (set-car! pair (fx1- count)))
    shared?))

(define vline
  (case-lambda
    (()
      (make-vline (vcellspan) (vcellspan)))
    ;; convert string to vline
    ((str)
      (make-vline (vcellspan) (string->vcellspan str)))))


;; Return a copy-on-write clone of specified vline.
(define (vline-copy-on-write line)
  (assert* 'vline-copy-on-write (vline? line))
  (%make-vline (cl< line) (cl> line) (vline-share-inc! line)
                  (vline-dirty-start-x line) (vline-dirty-end-x line)))

;; if vline was a copy-on-write clone, actually clone it.
(define (vline-unshare! line)
  (assert* 'vline-unshare! (vline? line))
  (when (vline-share-dec! line)
    (vbuffer-left-set!  line (vcellspan-copy (cl< line)))
    (vbuffer-right-set! line (vcellspan-copy (cl> line)))
    (%vline-share-set!   line (cons 0 #f))))

(define vline-empty?     vbuffer-empty?)
(define vline-length     vbuffer-length)
(define vline-ref        vbuffer-ref)

;; return char at position x
(define (vline-ref/char line x)
  (vcell->char (vbuffer-ref line x)))


;; return cell at position x, or #f if x is out of range
(define (vline-at line x)
  (if (fx<? -1 x (vline-length line))
    (vline-ref line x)
    #f))

;; return char at position x, or #f if x is out of range
(define (vline-at/char line x)
  (if (fx<? -1 x (vline-length line))
    (vline-ref/char line x)
    #f))


;; return #t if vline ends with #\newline, otherwise return #f
(define (vline-nl? line)
  (let ((last (fx1- (vline-length line))))
    (and (fx>=? last 0) (char=? #\newline (vline-ref/char line last)))))


;; return #t if line1 and line2 contain the same chars
(define (vline-equal/chars? line1 line2)
  (assert* 'vline-equal/chars? (vline? line1))
  (assert* 'vline-equal/chars? (vline? line2))
  (or (eq? line1 line2)
      (and (eq? (cl< line1) (cl< line2))
           (eq? (cl> line1) (cl> line2)))
      (let ((n1 (vline-length line1)))
        (and (fx=? n1 (vline-length line2))
          (do ((i 0 (fx1+ i)))
              ((or (fx>=? i n1)
                   (not (char=? (vline-ref/char line1 i) (vline-ref/char line2 i))))
               (fx>=? i n1)))))))


(define (vline-dirty-x-add! line start end)
  (vline-dirty-start-x-set! line (fxmin start (vline-dirty-start-x line)))
  (vline-dirty-end-x-set!   line (fxmax end   (vline-dirty-end-x   line))))

;; mark the whole vline as not dirty
(define (vline-dirty-x-unset! line)
  (vline-dirty-start-x-set! line (greatest-fixnum))
  (vline-dirty-end-x-set!   line 0))

;; c must be a character or cell
(define (vline-set! line x c)
  (vline-unshare! line)
  (vbuffer-set! line x c)
  (vline-dirty-x-add! line x (fx1+ x)))

;; insert one character or cell into line at position x
(define (vline-insert-at! line x c)
  (vline-unshare! line)
  (vbuffer-insert-at! line x c)
  (vline-dirty-x-add! line x (vline-length line)))


;; read elements in range [src-start, src-end) from vcellspan csp-src,
;; and insert them into vline line at position x
(define vline-insert-at/vcellspan!
  (case-lambda
    ((line x csp-src src-start src-end)
      (when (fx<? src-start src-end)
        (vline-unshare! line)
        (vbuffer-insert-at/vcellspan! line x csp-src src-start src-end)
        (vline-dirty-x-add! line x (vline-length line))))
    ((line x csp-src)
      (vline-insert-at/vcellspan! line x csp-src 0 (vcellspan-length csp-src)))))


;; read elements in range [src-start, src-end) from vbuffer or vline csp-src,
;; and insert them into vline at position x
(define vline-insert-at/vbuffer!
  (case-lambda
    ((line x cbuf-src src-start src-end)
      (when (fx<? src-start src-end)
        (vline-unshare! line)
        (vbuffer-insert-at/vbuffer! line x cbuf-src src-start src-end)
        (vline-dirty-x-add! line x (vline-length line))))
    ((line x cbuf-src)
      (vline-insert-at/vbuffer! line x cbuf-src 0 (vbuffer-length cbuf-src)))))


;; erase the chars in range [start, end) from vline
(define (vline-delete! line start end)
  (when (fx<? start end)
    (vline-unshare! line)
    (let ((len (vline-length line)))
      (vbuffer-delete! line start end)
      ;; mark as dirty until original end of line
      (vline-dirty-x-add! line start len))))

;; remove all chars from vline
(define (vline-clear! line)
  (let ((len (vline-length line)))
    (unless (fxzero? len)
      (vline-unshare! line)
      (vbuffer-clear! line)
      (vline-dirty-x-add! line 0 len))))


;; search leftward starting from x - 1,
;; find first cell that satisfies (pred ch)
;; and return position of such cell.
;; return #f if no cell satisfies (pred ch)
;;
;; note: if x > (vline-length line), it is truncated to (vline-length line)
(define (vline-index line x pred)
  (do ((i (fx1- (fxmin x (vline-length line))) (fx1- i)))
      ((or (fx<? i 0) (pred (vline-ref line i)))
        (if (fx<? i 0) #f i))))


;; search rightward starting from specified x,
;; find first cell that satisfies (pred ch)
;; and return position of such cell.
;; return #f if no cell satisfies (pred ch).
;;
;; note: if x < 0, it is truncated to 0
(define (vline-index-right line x pred)
  (let ((len (vline-length line)))
    (do ((i (fxmax x 0) (fx1+ i)))
        ((or (fx>=? i len) (pred (vline-ref line i)))
          (if (fx>=? i len) #f i)))))


;; search rightward in the range [start, end)
;; find first cell equal to character ch,
;; and return position of such cell.
;; return #f if no such cell is found in the range.
;;
;; note: if start < 0, it is truncated to 0
;; note: if end > (vline-length line), it is truncated to (vline-length line)
(define (vline-index/char line start end ch)
  (let ((end (fxmin end (vline-length line))))
    (do ((i (fxmax start 0) (fx1+ i)))
        ((or (fx>=? i end) (char=? ch (vline-ref/char line i)))
          (if (fx>=? i end) #f i)))))


;; search leftward starting from x - 1,
;; count number of consecutive cells that satisfy (pred ch)
;; and return such number
;;
;; note: if x > (vline-length line), it is truncated to (vline-length line)
(define (vline-count line x pred)
  (let ((end (fxmin x (vline-length line))))
    (do ((i (fx1- end) (fx1- i)))
        ((or (fx<? i 0) (not (pred (vline-ref line i))))
          (fx- end (fx1+ i))))))


;; search rightward starting from specified x,
;; count number of consecutive cells that satisfy (pred ch)
;; and return such number
;;
;; note: if x < 0, it is truncated to 0
(define (vline-count-right line x pred)
  (let ((start (fxmax x 0))
        (end   (vline-length line)))
    (do ((i start (fx1+ i)))
        ((or (fx>=? i end) (not (pred (vline-ref line i))))
          (fx- i start)))))


;; create and return a closure that iterates on elements of vline line.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vline line and #t,
;; or (values #<unspecified> #f) if end of vline is reached.
(define in-vline
  (case-lambda
    ((line start end step)
      (assert* 'in-vline (fx<=?* 0 start end (vline-length line)))
      (assert* 'in-vline (fx>=? step 0))
      (let ((%in-vline ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vline-ref line start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #\nul #f)))))
        %in-vline))
    ((line start end)
      (in-vline line start end 1))
    ((line)
      (in-vline line 0 (vline-length line) 1))))

(define vline-iterate vbuffer-iterate)


;; convert vline to string, removing all palette colors
(define (vline->string line)
  (let ((str (make-string (vline-length line))))
    (vline-iterate line
      (lambda (i c)
        (string-set! str i (vcell->char c))))
    str))


;; write colored vline to bytespan, NOT escaping special characters
(define vline-display/bytespan vbuffer-display/bytespan)



;; write a textual representation of vline to output port
(define vline-write vbuffer-write)

;; customize how "vline" objects are printed
(record-writer (record-type-descriptor %vline)
  (lambda (line port writer)
    (display "(vline " port)
    (vline-write line port)
    (display ")" port)))

) ; close library
