;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charline (0 7 6))
  (export
    charline charline? string->charline string->charline* charline->string
    assert-charline? charline-nl? charline-copy-on-write charline-empty?
    charline-length charline-ref charline-at charline-equal? charline-set! charline-clear!
    charline-erase-range! charline-insert-at! charline-insert-at/cspan! charline-insert-at/cbuf!
    charline-find/left charline-find/right charline-find/char charline-count/left charline-count/right
    charline-dirty-start-x charline-dirty-end-x charline-dirty-x-add! charline-dirty-x-unset!
    in-charline)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (only (schemesh bootstrap) assert*)
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
;; - charline-dirty-start-x and charline-dirty-end-x fixnums indicating the range of characters
;;   that were recently modified and not yet redrawn

(define-record-type
  (%charline %make-charline charline?)
  (parent %chargbuffer)
  (fields
    (mutable share) ; a cons (share-count . #f)
    (mutable dirty-x-start charline-dirty-start-x charline-dirty-start-x-set!)
    (mutable dirty-end-x   charline-dirty-end-x   charline-dirty-end-x-set!))
  (nongenerative #{%charline bptainzyb6dz0fmgkz7a0ic6v-439}))

(define (assert-charline? who line)
  (unless (charline? line)
    (assertion-violation who "not a charline" line)))

(define (make-charline left-span right-span)
  (assert* 'make-charline (charspan? left-span))
  (assert* 'make-charline (charspan? right-span))
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
  (assert* 'charline-copy-on-write (charline? line))
  (%make-charline (chargbuffer-left line) (chargbuffer-right line) (charline-share-inc! line)
                  (charline-dirty-start-x line) (charline-dirty-end-x line)))

;; if charline was a copy-on-write clone, actually clone it.
(define (charline-unshare! line)
  (assert* 'charline-unshare! (charline? line))
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
    (and (fx>=? last 0) (char=? #\newline (charline-ref line last)))))


;; return #t if line1 and line2 contain the same chars
(define (charline-equal? line1 line2)
  (assert* 'charline-equal? (charline? line1))
  (assert* 'charline-equal? (charline? line2))
  (or (eq? line1 line2)
      (and (eq? (chargbuffer-left line1)  (chargbuffer-left line2))
           (eq? (chargbuffer-right line1) (chargbuffer-right line2)))
      (let ((n1 (charline-length line1)))
        (and (fx=? n1 (charline-length line2))
          (do ((i 0 (fx1+ i)))
              ((or (fx>=? i n1)
                   (not (char=? (charline-ref line1 i) (charline-ref line2 i))))
               (fx>=? i n1)))))))


(define (charline-dirty-x-add! line start end)
  (charline-dirty-start-x-set! line (fxmin start (charline-dirty-start-x line)))
  (charline-dirty-end-x-set!   line (fxmax end   (charline-dirty-end-x   line))))

;; mark the whole charline as not dirty
(define (charline-dirty-x-unset! line)
  (charline-dirty-start-x-set! line (greatest-fixnum))
  (charline-dirty-end-x-set!   line 0))

(define (charline-set! line x ch)
  (charline-unshare! line)
  (chargbuffer-set! line x ch)
  (charline-dirty-x-add! line x (fx1+ x)))

;; insert one char into line at position x
(define (charline-insert-at! line x ch)
  (charline-unshare! line)
  (chargbuffer-insert-at! line x ch)
  (charline-dirty-x-add! line x (charline-length line)))


; read elements in range [src-start, src-end) from charspan csp-src,
; and insert them into charline line at position x
(define charline-insert-at/cspan!
  (case-lambda
    ((line x csp-src src-start src-end)
      (when (fx<? src-start src-end)
        (charline-unshare! line)
        (chargbuffer-insert-at/cspan! line x csp-src src-start src-end)
        (charline-dirty-x-add! line x (charline-length line))))
    ((line x csp-src)
      (charline-insert-at/cspan! line x csp-src 0 (charspan-length csp-src)))))


; read elements in range [src-start, src-end) from charbuffer or charline csp-src,
; and insert them into charline at position x
(define charline-insert-at/cbuf!
  (case-lambda
    ((line x cbuf-src src-start src-end)
      (when (fx<? src-start src-end)
        (charline-unshare! line)
        (chargbuffer-insert-at/cbuf! line x cbuf-src src-start src-end)
        (charline-dirty-x-add! line x (charline-length line))))
    ((line x cbuf-src)
      (charline-insert-at/cbuf! line x cbuf-src 0 (chargbuffer-length cbuf-src)))))


; erase the chars in range [start, end) from charline
(define (charline-erase-range! line start end)
  (when (fx<? start end)
    (charline-unshare! line)
    (let ((len (charline-length line)))
      (chargbuffer-erase-range! line start end)
      ;; mark as dirty until original end of line
      (charline-dirty-x-add! line start len))))

;; remove all chars from charline
(define (charline-clear! line)
  (let ((len (charline-length line)))
    (unless (fxzero? len)
      (charline-unshare! line)
      (chargbuffer-clear! line)
      (charline-dirty-x-add! line 0 len))))


;; search leftward starting from x - 1,
;; find first character that satisfies (pred ch)
;; and return position of such character.
;; return #f if no character satisfies (pred ch)
;;
;; note: if x > (charline-length line), it is truncated to (charline-length line)
(define (charline-find/left line x pred)
  (do ((i (fx1- (fxmin x (charline-length line))) (fx1- i)))
      ((or (fx<? i 0) (pred (charline-ref line i)))
        (if (fx<? i 0) #f i))))


;; search rightward starting from specified x,
;; find first character that satisfies (pred ch)
;; and return position of such character.
;; return #f if no character satisfies (pred ch).
;;
;; note: if x < 0, it is truncated to 0
(define (charline-find/right line x pred)
  (let ((len (charline-length line)))
    (do ((i (fxmax x 0) (fx1+ i)))
        ((or (fx>=? i len) (pred (charline-ref line i)))
          (if (fx>=? i len) #f i)))))


;; search rightward in the range [start, end)
;; find first character equal to ch,
;; and return position of such character.
;; return #f if no such character is found in the range.
;;
;; note: if start < 0, it is truncated to 0
;; note: if end > (charline-length line), it is truncated to (charline-length line)
(define (charline-find/char line start end ch)
  (let ((end (fxmin end (charline-length line))))
    (do ((i (fxmax start 0) (fx1+ i)))
        ((or (fx>=? i end) (char=? ch (charline-ref line i)))
          (if (fx>=? i end) #f i)))))


;; search leftward starting from x - 1,
;; count number of consecutive characters that satisfy (pred ch)
;; and return such number
;;
;; note: if x > (charline-length line), it is truncated to (charline-length line)
(define (charline-count/left line x pred)
  (let ((end (fxmin x (charline-length line))))
    (do ((i (fx1- end) (fx1- i)))
        ((or (fx<? i 0) (not (pred (charline-ref line i))))
          (fx- end (fx1+ i))))))


;; search rightward starting from specified x,
;; count number of consecutive characters that satisfy (pred ch)
;; and return such number
;;
;; note: if x < 0, it is truncated to 0
(define (charline-count/right line x pred)
  (let ((start (fxmax x 0))
        (end   (charline-length line)))
    (do ((i start (fx1+ i)))
        ((or (fx>=? i end) (not (pred (charline-ref line i))))
          (fx- i start)))))


;; create and return a closure that iterates on elements of charline line.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in charline line and #t,
;; or (values #<unspecified> #f) if end of charline is reached.
(define in-charline
  (case-lambda
    ((line start end step)
      (assert* 'in-charline (fx<=? 0 start end (charline-length line)))
      (assert* 'in-charline (fx>=? step 0))
      (lambda ()
        (if (fx<? start end)
          (let ((elem (charline-ref line start)))
            (set! start (fx+ start step))
            (values elem #t))
          (values #\nul #f))))
    ((line start end)
      (in-charline line start end 1))
    ((line)
      (in-charline line 0 (charline-length line) 1))))


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
