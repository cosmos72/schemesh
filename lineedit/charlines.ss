;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit charlines (0 1))
  (export
    charline charline? string->charline string->charline* charline->string
    charline-nl? charline-nl?-set! charline-copy-on-write
    charline-empty? charline-length charline-ref charline-set!
    charline-clear! charline-erase-at! charline-insert-at! assert-charline?

    charlines charlines? assert-charlines?
    charlines-iterate charlines-empty? charlines-length charlines-ref
    charlines-clear! charlines-copy-on-write charlines-erase-at! charlines-insert-at!)
  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (schemesh containers))

;; copy-pasted from containers/chargbuffer.ss
(define-record-type
  (%chargbuffer %make-chargbuffer %chargbuffer?)
  (fields
     (mutable left  chargbuffer-left  chargbuffer-left-set!)
     (mutable right chargbuffer-right chargbuffer-right-set!))
  (nongenerative #{%chargbuffer itah4n3k0nl66ucaakkpqk55m-16}))

;; type charline is a char gap-buffer with two additional fields:
;; - charline-newline? true if the gap buffer logically ends with a #\newline
; - charline-share a cons. its car will be > 0 if the gab buffer is shared copy-on-write
;   between two or more charlines: its content will be automatically cloned at the first
;   attempt to modify it.

(define-record-type
  (%charline %make-charline charline?)
  (parent %chargbuffer)
  (fields
    (mutable newline? charline-nl? charline-nl?-set!)
    (mutable share))
  (nongenerative #{%charline i81qf0lqcmlgj68ai4ihn68w3-28}))

(define (assert-charline? who line)
  (unless (charline? line)
    (assertion-violation who "not a charline" line)))

(define (make-charline left-span right-span nl?)
  (assert (charspan? left-span))
  (assert (charspan? right-span))
  (assert (boolean? nl?))
  (%make-charline left-span right-span nl? (cons 0 #f)))

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
                  (charline-nl? line) (charline-share-inc! line)))

;; if charline was a copy-on-write clone, actually clone it.
(define (charline-unshare! line)
  (when (charline-share-dec! line)
    (chargbuffer-left-set!  line (charspan-copy (chargbuffer-left line)))
    (chargbuffer-right-set! line (charspan-copy (chargbuffer-right line)))
    (%charline-share-set! line (cons 0 #f))))

(define charline-empty?     chargbuffer-empty?)
(define charline-length     chargbuffer-length)
(define charline-ref        chargbuffer-ref)

(define (charline-set! line idx ch)
  (charline-unshare! line)
  (chargbuffer-set! line idx ch))

(define (charline-insert-at! line idx ch)
  (charline-unshare! line)
  (chargbuffer-insert-at! line idx ch))

(define (charline-erase-at! line start n)
  (charline-unshare! line)
  (chargbuffer-erase-at! line start n))

(define (charline-clear! line)
  (charline-unshare! line)
  (chargbuffer-clear! line))

(define (string->charline str)
  (let ((line (make-charline (charspan) (string->charspan str) #f))
        (last (fx1- (string-length str))))
    (when (and (fx>=? last 0) (char=? #\newline (string-ref str last)))
      (charline-erase-at! line last 1)
      (charline-nl?-set! line #t))
    line))

(define (string->charline* str)
  (let ((line (make-charline (charspan) (string->charspan* str) #f))
        (last (fx1- (string-length str))))
    (when (and (fx>=? last 0) (char=? #\newline (string-ref str last)))
      (charline-erase-at! line last 1)
      (charline-nl?-set! line #t))
    line))

(define (charline->string line)
  (if (charline-nl? line)
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
      dst)
    (chargbuffer->string line)))

;; copy-pasted from containers/buffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left  gbuffer-left  gbuffer-left-set!)
     (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))

;; type charlines is a gap-buffer, containing charline elements

(define-record-type
  (%charlines %make-charlines charlines?)
  (parent %gbuffer)
  (nongenerative #{%charlines g2x4legjl16y9nnoua5c9y1u9-28}))

(define (assert-charlines? who lines)
  (unless (charlines? lines)
    (assertion-violation who "not a charlines" lines)))

(define (charlines . vals)
  (list-iterate vals (lambda (val) (assert-charline? 'charlines val)))
  (%make-charlines (span) (list->span vals)))

(define charlines-iterate    gbuffer-iterate)
(define charlines-empty?     gbuffer-empty?)
(define charlines-length     gbuffer-length)
(define charlines-clear!     gbuffer-clear!)
(define charlines-erase-at!  gbuffer-erase-at!)
(define charlines-insert-at! gbuffer-insert-at!)
(define charlines-ref        gbuffer-ref)
(define (charlines-set! lines idx line)
  (assert-charlines? 'charlines-set! lines)
  (assert-charline?  'charlines-set! line)
  (gbuffer-set! lines idx line))

;; Return a copy-on-write clone of charlines.
;; Also calls (charline-copy-on-write) on each line.

(define (charlines-copy-on-write lines)
  (let ((dst (make-span (charlines-length lines))))
    (charlines-iterate lines
      (lambda (i line)
        (span-set! dst i (charline-copy-on-write line))))
    (%make-charlines (span) dst)))

;; customize how "charline" objects are printed
(record-writer (record-type-descriptor %charline)
  (lambda (line port writer)
    (display "(string->charline* " port)
    (write (charline->string line) port)
    (display #\) port)))

;; customize how "charlines" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (sp port writer)
    (display "(charlines" port)
    (charlines-iterate sp
      (lambda (i elem)
        (display #\space port)
        (writer elem port)))
    (display #\) port)))

) ; close library
