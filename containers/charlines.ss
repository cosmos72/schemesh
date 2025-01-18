;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers charlines (0 1))
  (export
    charlines charlines? strings->charlines strings->charlines*
    assert-charlines? charlines-shallow-copy charlines-copy-on-write charlines-iterate
    charlines-empty? charlines-length charlines-equal? charlines-ref charlines-set/cline!
    charlines-clear! charlines-find/left charlines-find/right charlines-count/left charlines-count/right
    charlines-dirty-start-y charlines-dirty-end-y charlines-dirty-y-add! charlines-dirty-xy-unset!
    charlines-erase-at/cline! charlines-insert-at/cline! charlines-starts-with?
    charlines-next-xy charlines-prev-xy charlines-char-at-xy charlines-char-before-xy charlines-char-after-xy
    write-charlines)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (only (schemesh bootstrap) assert* debugf while)
    (only (schemesh containers misc) list-iterate)
    (schemesh containers span)
    (schemesh containers charline)
    (schemesh containers gbuffer))


;; copy-pasted from containers/gbuffer.ss
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
  (fields
    (mutable dirty-start-y charlines-dirty-start-y charlines-dirty-start-y-set!)
    (mutable dirty-end-y   charlines-dirty-end-y   charlines-dirty-end-y-set!))
  (nongenerative #{%charlines lf2lr8d65f8atnffcpi1ja7l0-439}))


(define (assert-charlines? who lines)
  (unless (charlines? lines)
    (assertion-violation who "not a charlines" lines)))

;; create a charlines from zero or more charline
(define (charlines . cline)
  (list-iterate cline (lambda (val) (assert-charline? 'charlines val)))
  (%make-charlines (span) (list->span cline) (greatest-fixnum) 0))


;; make a copy of specified strings and store them into a newly created charlines.
;; return the created charlines.
(define (strings->charlines . str)
  (apply charlines (map string->charline str)))


;; store references to specified strings into a newly created charlines.
;; return the created charlines.
(define (strings->charlines* . str)
  (apply charlines (map string->charline* str)))


(define charlines-iterate    gbuffer-iterate)
(define charlines-empty?     gbuffer-empty?)
(define charlines-length     gbuffer-length)


;; return #t if lines1 and lines2 contain the same number of charline
;; and each charline in lines1 is equal to the corresponding charline in lines2
(define (charlines-equal? lines1 lines2)
  (assert* 'charlines-equal? (charlines? lines1))
  (assert* 'charlines-equal? (charlines? lines2))
  (or (eq? lines1 lines2)
      (and (eq? (gbuffer-left lines1)  (gbuffer-left lines2))
           (eq? (gbuffer-right lines1) (gbuffer-right lines2)))
      (let ((n1 (charlines-length lines1)))
        (and (fx=? n1 (charlines-length lines2))
             (do ((i 0 (fx1+ i)))
                 ((or (fx>=? i n1)
                      (not (charline-equal? (charlines-ref lines1 i) (charlines-ref lines2 i))))
                  (fx>=? i n1)))))))


(define (charlines-dirty-y-add! lines start end)
  (charlines-dirty-start-y-set! lines (fxmin start (charlines-dirty-start-y lines)))
  (charlines-dirty-end-y-set!   lines (fxmax end   (charlines-dirty-end-y   lines))))


(define (charlines-dirty-xy-unset! lines)
  (charlines-dirty-start-y-set! lines (greatest-fixnum))
  (charlines-dirty-end-y-set!   lines 0)
  (charlines-iterate lines
    (lambda (i line)
      (charline-dirty-x-unset! line))))


;; Return a copy-on-write clone of charlines.
;; Also calls (charline-copy-on-write) on each line.
(define (charlines-copy-on-write lines)
  (let ((dst (make-span (charlines-length lines))))
    (charlines-iterate lines
      (lambda (i line)
        (span-set! dst i (charline-copy-on-write line))))
    (%make-charlines (span) dst
      (charlines-dirty-start-y lines)
      (charlines-dirty-end-y   lines))))


;; Return a shallow clone of charlines, i.e. a new charline referencing
;; the same left and right internal spans.
;; Reuses each existing line in charlines, does not call (charline-copy-on-write) on them.
(define (charlines-shallow-copy lines)
  (%make-charlines (gbuffer-left lines) (gbuffer-right lines)
      (charlines-dirty-start-y lines)
      (charlines-dirty-end-y   lines)))


;; get n-th line
(define charlines-ref   gbuffer-ref)

;; replace a charline in lines at y
(define (charlines-set/cline! lines y line)
  (assert-charline? 'charlines-set/cline! line)
  (charlines-dirty-y-add! lines y (fx1+ y))
  (gbuffer-set! lines y line))


(define (charlines-clear! lines)
  (charlines-dirty-y-add! lines 0 (charlines-length lines))
  (gbuffer-clear! lines))


;; erase a charline from lines at y
(define (charlines-erase-at/cline! lines y)
  (let ((yn (charlines-length lines)))
    (when (fx<? -1 y yn)
      (charlines-dirty-y-add! lines y yn)
      (gbuffer-erase-at! lines y 1))))


;; insert a charline into lines at y
(define (charlines-insert-at/cline! lines y line)
  (assert-charline? 'charlines-insert-at/cline! line)
  (gbuffer-insert-at! lines y line)
  (charlines-dirty-y-add! lines y (charlines-length lines)))


;; search leftward starting from one character left of specified x and y,
;; find first character that satisfies (pred ch)
;; and return number of characters to skip until such character.
;; return #f if no character satisfies (pred ch)
;;
;; notes: if y < 0 returns #f
;;        if y >= (charlines-length lines)
;;                it is truncated to charlines-length - 1
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-find/left lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmin y (fx1- ny)))
    (while (fx>=? y 0)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (charline-find/left line xx pred)))
        (if pos
          (begin ;; match found
            (set! ret (fx+ ret (fx- xx pos)))
            (set! y -1)) ; finishes (while) loop
          (begin ;; match not found yet
            (set! x (greatest-fixnum))
            (set! y (fx1- y))
            (if (fx>=? y 0)
              (set! ret (fx+ ret xx))
              (set! ret #f)))))) ; (while) loop finished, no char found
      ret))


;; search rightward starting from specified x and y,
;; find first character that satisfies (pred ch)
;; and return number of characters to skip until such character.
;; return #f if no character satisfies (pred ch)
;;
;; notes: if y < 0 it is truncated to 0
;;        if y >= (charlines-length lines) returns #f
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-find/right lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmax 0 y))
    (while (fx<? y ny)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (charline-find/right line xx pred)))
        (if pos
          (begin ;; match found
            (set! ret (fx+ ret (fx- pos xx)))
            (set! y ny)) ; finishes (while) loop
          (begin ;; match not found yet
            (set! x 0)
            (set! y (fx1+ y))
            (if (fx<? y ny)
              (set! ret (fx+ ret (fx- len xx)))
              (set! ret #f)))))) ; (while) loop finished, no char found
      ret))


;; starting from one character left of specified x and y and moving left,
;; count number of consecutive characters that satisfy (pred ch)
;; and return such number.
;;
;; notes: if y < 0 returns #f
;;        if y >= (charlines-length lines)
;;                it is truncated to charlines-length - 1
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-count/left lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmin y (fx1- ny)))
    (while (fx>=? y 0)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (n    (charline-count/left line xx pred)))
        (set! ret (fx+ ret n))
        (if (fx=? n xx)
          (begin ;; all characters satisfy pred, continue on previous line
            (set! x (greatest-fixnum))
            (set! y (fx1- y)))
          (set! y -1)))) ;; some character does not satify pred, stop
    ret))

;; starting from specified x and y and moving right,
;; count number of consecutive character that satisfies (pred ch)
;; and return such number.
;;
;; notes: if y < 0 it is truncated to 0
;;        if y >= (charlines-length lines) returns #f
;;        if x < 0 it is truncated to 0
;;        if x > (charline-length (charlines-ref lines y))
;;               it is truncated to charline-length
(define (charlines-count/right lines x y pred)
  (let ((ny (charlines-length lines))
        (ret 0))
    (set! y (fxmax 0 y))
    (while (fx<? y ny)
      (let* ((line (charlines-ref lines y))
             (len  (charline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (n    (charline-count/right line xx pred)))
        (set! ret (fx+ ret n))
        (if (fx=? n (fx- len xx))
          (begin ;; all characters satisfy pred, continue on previous line
            (set! x 0)
            (set! y (fx1+ y)))
          (set! y ny)))) ;; some character does not satify pred, stop
    ret))


;; return #t if lines start with range [0 0, endx endy) of prefix - which must be a charlines too -
;; otherwise return #f
(define (charlines-starts-with? lines prefix endx endy)
  (let %again ((x1 0) (y1 0) (x2 0) (y2 0))
    (let ((ch1 (and x1 y1 (charlines-char-at-xy lines x1 y1)))
          (ch2 (and x2 y2 (charlines-char-at-xy prefix x2 y2))))
    ; (debugf "... charlines-starts-with? xy+ch1=(~s ~s ~s) xy+ch2=(~s ~s ~s)" x1 y1 ch1 x2 y2 ch2)
    (cond
      ((not ch2) ; reached end of prefix
        #t)
      ((or (fx>? y2 endy) (and (fx=? y2 endy) (fx>=? x2 endx))) ; reached end of prefix range
        #t)
      ((not ch1) ; reached end of lines
        #f)
      ((char=? ch1 ch2)
        (let-values (((x1 y1) (charlines-next-xy lines x1 y1)))
          (let-values (((x2 y2) (charlines-next-xy prefix x2 y2)))
            (%again x1 y1 x2 y2))))
      (#t                    ; found different characters
        #f)))))


;; return position one character to the left of x y.
;; returned position may be on the previous line.
;; return #f #f if x y is out of range or is the first valid position.
(define (charlines-prev-xy lines x y)
  (let ((ymax  (fx1- (charlines-length lines))))
    (if (and (fixnum? x) (fixnum? y) (fx<=? 0 y ymax))
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (charline-length (charlines-ref lines y))
                       (if (fx=? y ymax) 0 1))))
        ; (debugf "charlines-length-prev-xy xy = (~s ~s), xmax = ~s" x y xmax)
        (if (fx<=? 1 x xmax)
          (values (fx1- x) y) ;; (x-1 y) is a valid position, return it
          (if (fx>? y 0)
            ;; return last position in previous line
            (values (fx1- (charline-length (charlines-ref lines (fx1- y)))) (fx1- y))
            (values #f #f))))
      (values #f #f))))


;; return position one character to the right of x y.
;; returned position may be on the next line.
;; return #f #f if x y is out of range or is the last valid position.
(define (charlines-next-xy lines x y)
  (let ((ymax  (fx1- (charlines-length lines))))
    (if (and (fixnum? x) (fixnum? y) (fx<=? 0 y ymax))
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (charline-length (charlines-ref lines y))
                       (if (fx=? y ymax) 0 1))))
        ; (debugf "charlines-length-next-xy xy = (~s ~s), xmax = ~s" x y xmax)
        (if (fx<? -1 x xmax)
          (values (fx1+ x ) y) ;; (x+1 y) is a valid position, return it
          (if (fx<? y ymax)
            (values 0 (fx1+ y)) ;; return beginning of next line
            (values #f #f))))
      (values #f #f))))


;; return charlines char at specified x y, or #f if x y are out of range
(define (charlines-char-at-xy lines x y)
  (if (and (fixnum? x) (fixnum? y) (fx<? -1 y (charlines-length lines)))
    (let* ((line (charlines-ref lines y))
           (len  (charline-length line)))
      (if (fx<? -1 x len)
        (charline-ref line x)
        #f))
    #f))

;; return position immediately before x y, and char at such position.
;; return #f #f #f if x y are out of range or 0 0.
(define (charlines-char-before-xy lines x y)
  (let-values (((x y) (charlines-prev-xy lines x y)))
    (values x y (and x y (charlines-char-at-xy lines x y)))))


;; return position immediately after x y, and char at such position.
;; return #f #f #f if x y are out of range.
;; return x+1 y #f if x y correspond to the last character in the last line
(define (charlines-char-after-xy lines x y)
  (let-values (((x y) (charlines-next-xy lines x y)))
    (values x y (and x y (charlines-char-at-xy lines x y)))))


;; write a textual representation of charlines to output port
(define (write-charlines lines port)
  (assert-charlines? "write-charlines" lines)
  (display "(strings->charlines*" port)
  (charlines-iterate lines
    (lambda (i line)
      (display #\space port)
      (write (charline->string line) port)))
  (display ")" port))

;; customize how "charlines" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (lines port writer)
    (write-charlines lines port)))

) ; close library
