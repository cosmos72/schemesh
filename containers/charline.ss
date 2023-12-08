;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charline (0 1))
  (export
    charline charline? string->charline string->charline* charline->string
    assert-charline? charline-nl? charline-nl?-set! charline-copy-on-write
    charline-empty? charline-length charline-ref charline-set! charline-clear!
    charline-erase-at! charline-insert-at! charline-insert-at/cbuf!
    charline-dirty-x-start charline-dirty-x-end charline-dirty-x-add! charline-dirty-x-unset!)

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
;; - charline-newline? true if the gap buffer logically ends with a #\newline
;; - charline-share a cons. its car will be > 0 if the charline is shared copy-on-write
;;   between two or more charlines: its content will be automatically cloned at the first
;;   attempt to modify it.
;; - charline-dirty-x-start and charline-dirty-x-end fixnums indicating the range of characters that were recently modified and not yet redrawn

(define-record-type
  (%charline %make-charline charline?)
  (parent %chargbuffer)
  (fields
    (mutable newline? charline-nl? charline-nl?-set!)
    (mutable share) ; a cons (share-count . #f)
    (mutable dirty-x-start charline-dirty-x-start charline-dirty-x-start-set!)
    (mutable dirty-x-end   charline-dirty-x-end   charline-dirty-x-end-set!))
  (nongenerative #{%charline bptainzyb6dz0fmgkz7a0ic6v-439}))

(define (assert-charline? who line)
  (unless (charline? line)
    (assertion-violation who "not a charline" line)))

(define (make-charline left-span right-span nl?)
  (assert (charspan? left-span))
  (assert (charspan? right-span))
  (assert (boolean? nl?))
  (%make-charline left-span right-span nl? (cons 0 #f) (greatest-fixnum) 0))

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
  (make-charline (charspan) (charspan) #f))

;; Return a copy-on-write clone of specified charline.
(define (charline-copy-on-write line)
  (assert (charline? line))
  (%make-charline (chargbuffer-left line) (chargbuffer-right line)
                  (charline-nl? line) (charline-share-inc! line)
                  (charline-dirty-x-start line) (charline-dirty-x-end line)))

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

(define (charline-dirty-x-add! line start end)
  (charline-dirty-x-start-set! line (fxmin start (charline-dirty-x-start line)))
  (charline-dirty-x-end-set!   line (fxmax end   (charline-dirty-x-end   line))))

;; mark the whole charline as not dirty
(define (charline-dirty-x-unset! line)
  (charline-dirty-x-start-set! line (greatest-fixnum))
  (charline-dirty-x-end-set!   line 0))

(define (charline-set! line x ch)
  (charline-unshare! line)
  (chargbuffer-set! line x ch)
  (charline-dirty-x-add! line x (fx1+ x)))

;; insert one char into line at position x
(define (charline-insert-at! line x ch)
  (charline-unshare! line)
  (charline-dirty-x-add! line x (fx1+ (charline-length line)))
  (chargbuffer-insert-at! line x ch))

; read src-n elements from charbuffer or charline csp-src,
; starting from src-start, and insert them into charline at position x
(define (charline-insert-at/cbuf! line x csp-src src-start src-n)
  (charline-unshare! line)
  (chargbuffer-insert-at/cbuf! line x csp-src src-start src-n)
  (charline-dirty-x-add! line x (charline-length line)))

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

;; make a copy of string str and store it into a newly created charline
;; return the created charline
(define (string->charline str)
  (let ((line (make-charline (charspan) (string->charspan str) #f))
        (last (fx1- (string-length str))))
    (when (and (fx>=? last 0) (char=? #\newline (string-ref str last)))
      (charline-erase-at! line last 1)
      (charline-nl?-set! line #t))
    line))

;; store a reference to string str into a newly created charline
;; return the created charline
(define (string->charline* str)
  (let ((line (make-charline (charspan) (string->charspan* str) #f))
        (last (fx1- (string-length str))))
    (when (and (fx>=? last 0) (char=? #\newline (string-ref str last)))
      (charline-erase-at! line last 1)
      (charline-nl?-set! line #t))
    line))

(define (charline->string line)
  (if (not (charline-nl? line))
    (chargbuffer->string line)
    (let* ((left    (chargbuffer-left  line))
           (right   (chargbuffer-right line))
           (left-n  (charspan-length left))
           (right-n (charspan-length right))
           (n       (fx+ left-n right-n))
           (dst     (make-string (fx1+ n))))
      (string-copy! (charspan-peek-data left)  (charspan-peek-beg left)
                    dst 0 left-n)
      (string-copy! (charspan-peek-data right) (charspan-peek-beg right)
                    dst left-n right-n)
      (string-set! dst n #\newline)
      dst)))


;; customize how "charline" objects are printed
(record-writer (record-type-descriptor %charline)
  (lambda (line port writer)
    (display "(string->charline* " port)
    (write (charline->string line) port)
    (display ")" port)))

) ; close library
