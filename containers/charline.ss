;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charline (0 1))
  (export
    charline charline? string->charline string->charline* charline->string
    assert-charline? charline-nl? charline-copy-on-write charline-empty?
    charline-length charline-ref charline-at charline-set! charline-clear!
    charline-erase-at! charline-insert-at! charline-insert-at/cspan! charline-insert-at/cbuf!
    charline-find-left charline-find-right
    charline-dirty-x-start charline-dirty-end-x charline-dirty-x-add! charline-dirty-x-unset!)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (schemesh containers charspan)
    (schemesh containers chargbuffer))

;; copy-pasted from containers/cbuffer.ss
(define-record-type
  (%chargbuffer %make-chargbuffer %chargbuffer?)
  (fields
     (mutable left  chargbuffer-left  chargbuffer-left-set!)
     (mutable right chargbuffer-right chargbuffer-right-set!))
  (nongenerative #{%chargbuffer itah4n3k0nl66ucaakkpqk55m-16}))

;; type charline is a char gap-buffer with additional fields:
;;
;; - charline-share a cons. its car will be > 0 if the charline is shared copy-on-write
;;   between two or more charlines: its content will be automatically cloned at the first
;;   attempt to modify it.
;; - charline-dirty-x-start and charline-dirty-end-x fixnums indicating the range of characters
;;   that were recently modified and not yet redrawn

(define-record-type
  (%charline %make-charline charline?)
  (parent %chargbuffer)
  (fields
    (mutable share) ; a cons (share-count . #f)
    (mutable dirty-x-start charline-dirty-x-start charline-dirty-x-start-set!)
    (mutable dirty-end-x   charline-dirty-end-x   charline-dirty-end-x-set!))
  (nongenerative #{%charline bptainzyb6dz0fmgkz7a0ic6v-439}))

(define (assert-charline? who line)
  (unless (charline? line)
    (assertion-violation who "not a charline" line)))

(define (make-charline left-span right-span)
  (assert (charspan? left-span))
  (assert (charspan? right-span))
  (%make-charline left-span right-span (cons 0 #f) (greatest-fixnum) 0))

;; increment charline share count by 1.
;; return pair containing share count
(define (charline-share-inc! line)
  (let ((pair (%charline-share line)))
    (set-car! pair (fx1+ (car pair)))
    pair))

;; decrement charline share count by 1.
;; return #t if charline was shared, otherwise return #f
(define (charline-share-dec! line)
  (let* ((pair (%charline-share line))
         (count (car pair))
         (shared? (fx>? count 0)))
    (when shared?
      (set-car! pair (fx1- count)))
    shared?))

(define (charline)
  (make-charline (charspan) (charspan)))

;; Return a copy-on-write clone of specified charline.
(define (charline-copy-on-write line)
  (assert (charline? line))
  (%make-charline (chargbuffer-left line) (chargbuffer-right line) (charline-share-inc! line)
                  (charline-dirty-x-start line) (charline-dirty-end-x line)))

;; if charline was a copy-on-write clone, actually clone it.
(define (charline-unshare! line)
  (assert (charline? line))
  (when (charline-share-dec! line)
    (chargbuffer-left-set!  line (charspan-copy (chargbuffer-left line)))
    (chargbuffer-right-set! line (charspan-copy (chargbuffer-right line)))
    (%charline-share-set!   line (cons 0 #f))))

(define charline-empty?     chargbuffer-empty?)
(define charline-length     chargbuffer-length)
(define charline-ref        chargbuffer-ref)

;; return character at position x, or #f if x is out of range
(define (charline-at line x)
  (if (fx<? -1 x (charline-length line))
    (charline-ref line x)
    #f))

;; return #t if charline ends with #\newline, otherwise return #f
(define (charline-nl? line)
  (let ((last (fx1- (charline-length line))))
    (and (fx>=? last 0) (char=? #\newline (chargbuffer-ref line last)))))

(define (charline-dirty-x-add! line start end)
  (charline-dirty-x-start-set! line (fxmin start (charline-dirty-x-start line)))
  (charline-dirty-end-x-set!   line (fxmax end   (charline-dirty-end-x   line))))

;; mark the whole charline as not dirty
(define (charline-dirty-x-unset! line)
  (charline-dirty-x-start-set! line (greatest-fixnum))
  (charline-dirty-end-x-set!   line 0))

(define (charline-set! line x ch)
  (charline-unshare! line)
  (chargbuffer-set! line x ch)
  (charline-dirty-x-add! line x (fx1+ x)))

;; insert one char into line at position x
(define (charline-insert-at! line x ch)
  (charline-unshare! line)
  (chargbuffer-insert-at! line x ch)
  (charline-dirty-x-add! line x (fx1+ (charline-length line))))


; read src-n elements from charspan csp-src,
; starting from src-start, and insert them into charline at position x
(define (charline-insert-at/cspan! line x csp-src src-start src-n)
  (when (fx>? src-n 0)
    (charline-unshare! line)
    (chargbuffer-insert-at/cspan! line x csp-src src-start src-n)
    (charline-dirty-x-add! line x (charline-length line))))

; read src-n elements from charbuffer or charline cbuf-src,
; starting from src-start, and insert them into charline at position x
(define (charline-insert-at/cbuf! line x cbuf-src src-start src-n)
  (when (fx>? src-n 0)
    (charline-unshare! line)
    (chargbuffer-insert-at/cbuf! line x cbuf-src src-start src-n)
    (charline-dirty-x-add! line x (charline-length line))))


; erase n chars from charline starting at position x
(define (charline-erase-at! line x n)
  (when (fx>? n 0)
    (charline-unshare! line)
    (chargbuffer-erase-at! line x n)
    (charline-dirty-x-add! line x (fx+ n (charline-length line)))))

(define (charline-clear! line)
  (charline-unshare! line)
  (let ((n (charline-length line)))
    (chargbuffer-clear! line)
    (charline-dirty-x-add! line 0 n)))

;; search leftward starting from x - 1,
;; find first character that satisfies (pred ch)
;; and return position of such character.
;; return #f if no character satisfies (pred ch)
;;
;; note: if x > (charline-length line), it is truncated to (charline-length line)
(define (charline-find-left line x pred)
  (do ((i (fx1- (fxmin x (charline-length line))) (fx1- i)))
      ((or (fx<? i 0) (pred (charline-ref line i)))
        (if (fx<? i 0) #f i))))

;; search rightward starting from specified x,
;; find first character that satisfies (pred ch)
;; and return position of such character.
;; return #f if no character satisfies (pred ch).
;;
;; note: if x < 0, it is truncated to 0
(define (charline-find-right line x pred)
  (let ((len (charline-length line)))
    (do ((i (fxmax x 0) (fx1+ i)))
        ((or (fx>=? i len) (pred (charline-ref line i)))
          (if (fx>=? i len) #f i)))))

;; make a copy of string str and store it into a newly created charline
;; return the created charline
(define (string->charline str)
  (make-charline (charspan) (string->charspan str)))

;; store a reference to string str into a newly created charline
;; return the created charline
(define (string->charline* str)
  (make-charline (charspan) (string->charspan* str)))

(define (charline->string line)
  (chargbuffer->string line))

;; customize how "charline" objects are printed
(record-writer (record-type-descriptor %charline)
  (lambda (line port writer)
    (display "(string->charline* " port)
    (write (charline->string line) port)
    (display ")" port)))

) ; close library
