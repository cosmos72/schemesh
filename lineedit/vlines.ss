;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit vlines (0 8 3))
  (export
    vlines vlines? assert-vlines? vlines->string
    vlines-shallow-copy vlines-copy-on-write vlines-iterate
    vlines-empty? vlines-length vlines-equal/chars?
    vlines-cell-length vlines-ref vlines-set! vlines-clear!
    vlines-index vlines-index-right vlines-count vlines-count-right
    vlines-dirty-start-y vlines-dirty-end-y vlines-dirty-y-add! vlines-dirty-xy-unset!
    vlines-delete-at! vlines-insert-at! vlines-starts-with?
    vlines-next-xy vlines-prev-xy vlines-cell-at-xy vlines-cell-before-xy vlines-cell-after-xy
    in-vlines)

  (import
    (rnrs)
    (only (rnrs mutable-pairs)   set-car!)
    (only (rnrs mutable-strings) string-set!)
    (only (chezscheme) fx1+ fx1- record-writer string-copy!)
    (only (schemesh bootstrap)   assert* fx<=?* while)
    (only (schemesh containers list) for-list)
    (only (schemesh containers charspan) make-charspan charspan-insert-right! charspan->string*!)
    (only (schemesh containers cell)  cell->char)
    (schemesh containers span)
    (schemesh containers cellspan)
    (schemesh containers cellgbuffer)
    (schemesh containers gbuffer)
    (schemesh lineedit vline))


;; copy-pasted from containers/gbuffer.ss
(define-record-type (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left  gbuffer-left  gbuffer-left-set!)
     (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative %gbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))

;; type vlines is a gap-buffer, containing vline elements
(define-record-type (%vlines %make-vlines vlines?)
  (parent %gbuffer)
  (fields
    (mutable dirty-start-y vlines-dirty-start-y vlines-dirty-start-y-set!)
    (mutable dirty-end-y   vlines-dirty-end-y   vlines-dirty-end-y-set!))
  (nongenerative %vlines-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (assert-vlines? who lines)
  (unless (vlines? lines)
    (assertion-violation who "not a vlines" lines)))

;; create a vlines from zero or more vline or string elements
(define (vlines . l)
  (let ((sp (list->span l)))
    (span-iterate sp
      (lambda (i line)
        (if (string? line)
         (span-set! sp i (vline line))
         (assert-vline? 'vlines line))))
    (%make-vlines (span) sp (greatest-fixnum) 0)))


(define vlines-iterate    gbuffer-iterate)
(define vlines-empty?     gbuffer-empty?)
(define vlines-length     gbuffer-length)


;; convert vlines to string, removing all palette colors
(define (vlines->string lines)
  (let ((str (make-string (vlines-cell-length lines)))
        (i 0))
    (vlines-iterate lines
      (lambda (y line)
        (vline-iterate line
          (lambda (x c)
            (string-set! str i (cell->char c))
            (set! i (fx1+ i))))))
    str))


;; return #t if lines1 and lines2 contain the same number of vline
;; and each vline in lines1 contains the same chars as the corresponding vline in lines2
(define (vlines-equal/chars? lines1 lines2)
  (assert* 'vlines-equal/chars? (vlines? lines1))
  (assert* 'vlines-equal/chars? (vlines? lines2))
  (or (eq? lines1 lines2)
      (and (eq? (gbuffer-left lines1)  (gbuffer-left lines2))
           (eq? (gbuffer-right lines1) (gbuffer-right lines2)))
      (let ((n1 (vlines-length lines1)))
        (and (fx=? n1 (vlines-length lines2))
             (do ((i 0 (fx1+ i)))
                 ((or (fx>=? i n1)
                      (not (vline-equal/chars? (vlines-ref lines1 i) (vlines-ref lines2 i))))
                  (fx>=? i n1)))))))


(define (vlines-dirty-y-add! lines start end)
  (vlines-dirty-start-y-set! lines (fxmin start (vlines-dirty-start-y lines)))
  (vlines-dirty-end-y-set!   lines (fxmax end   (vlines-dirty-end-y   lines))))


(define (vlines-dirty-xy-unset! lines)
  (vlines-dirty-start-y-set! lines (greatest-fixnum))
  (vlines-dirty-end-y-set!   lines 0)
  (vlines-iterate lines
    (lambda (i line)
      (vline-dirty-x-unset! line))))


;; Return a copy-on-write clone of vlines.
;; Also calls (vline-copy-on-write) on each line.
(define (vlines-copy-on-write lines)
  (let ((dst (make-span (vlines-length lines))))
    (vlines-iterate lines
      (lambda (i line)
        (span-set! dst i (vline-copy-on-write line))))
    (%make-vlines (span) dst
      (vlines-dirty-start-y lines)
      (vlines-dirty-end-y   lines))))


;; Return a shallow clone of vlines, i.e. a new vline referencing
;; the same left and right internal spans.
;; Reuses each existing line in vlines, does not call (vline-copy-on-write) on them.
(define (vlines-shallow-copy lines)
  (%make-vlines (gbuffer-left lines) (gbuffer-right lines)
      (vlines-dirty-start-y lines)
      (vlines-dirty-end-y   lines)))


;; return the number of cells in a vlines
(define (vlines-cell-length lines)
  (let %vlines-cell-length ((ret 0) (lines lines)
                            (y 0)   (yn (vlines-length lines)))
    (if (fx<? y yn)
      (%vlines-cell-length
        (fx+ ret (vline-length (vlines-ref lines y)))
        lines (fx1+ y) yn)
      ret)))


;; get n-th line
(define vlines-ref   gbuffer-ref)

;; replace a vline in lines at y
(define (vlines-set! lines y line)
  (assert-vline? 'vlines-set! line)
  (vlines-dirty-y-add! lines y (fx1+ y))
  (gbuffer-set! lines y line))


(define (vlines-clear! lines)
  (vlines-dirty-y-add! lines 0 (vlines-length lines))
  (gbuffer-clear! lines))


;; erase a vline from lines at y
(define (vlines-delete-at! lines y)
  (let ((yn (vlines-length lines)))
    (when (fx<? -1 y yn)
      (vlines-dirty-y-add! lines y yn)
      (gbuffer-delete! lines y (fx1+ y)))))


;; insert a vline into lines at y
(define (vlines-insert-at! lines y line)
  (assert-vline? 'vlines-insert-at! line)
  (gbuffer-insert-at! lines y line)
  (vlines-dirty-y-add! lines y (vlines-length lines)))


;; search leftward starting from one cell left of specified x and y,
;; find first cell that satisfies (pred c)
;; and return number of cells to skip until such cell.
;; return #f if no cell satisfies (pred c)
;;
;; notes: if y < 0 returns #f
;;        if y >= (vlines-length lines)
;;                it is truncated to vlines-length - 1
;;        if x < 0 it is truncated to 0
;;        if x > (vline-length (vlines-ref lines y))
;;               it is truncated to vline-length
(define (vlines-index lines x y pred)
  (let ((ny (vlines-length lines))
        (ret 0))
    (set! y (fxmin y (fx1- ny)))
    (while (fx>=? y 0)
      (let* ((line (vlines-ref lines y))
             (len  (vline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (vline-index line xx pred)))
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
;; find first cell that satisfies (pred ch)
;; and return number of cells to skip until such cell.
;; return #f if no cell satisfies (pred ch)
;;
;; notes: if y < 0 it is truncated to 0
;;        if y >= (vlines-length lines) returns #f
;;        if x < 0 it is truncated to 0
;;        if x > (vline-length (vlines-ref lines y))
;;               it is truncated to vline-length
(define (vlines-index-right lines x y pred)
  (let ((ny (vlines-length lines))
        (ret 0))
    (set! y (fxmax 0 y))
    (while (fx<? y ny)
      (let* ((line (vlines-ref lines y))
             (len  (vline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (pos  (vline-index-right line xx pred)))
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


;; starting from one cell left of specified x and y and moving left,
;; count number of consecutive cells that satisfy (pred ch)
;; and return such number.
;;
;; notes: if y < 0 returns #f
;;        if y >= (vlines-length lines)
;;                it is truncated to vlines-length - 1
;;        if x < 0 it is truncated to 0
;;        if x > (vline-length (vlines-ref lines y))
;;               it is truncated to vline-length
(define (vlines-count lines x y pred)
  (let ((ny (vlines-length lines))
        (ret 0))
    (set! y (fxmin y (fx1- ny)))
    (while (fx>=? y 0)
      (let* ((line (vlines-ref lines y))
             (len  (vline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (n    (vline-count line xx pred)))
        (set! ret (fx+ ret n))
        (if (fx=? n xx)
          (begin ;; all cells satisfy pred, continue on previous line
            (set! x (greatest-fixnum))
            (set! y (fx1- y)))
          (set! y -1)))) ;; some cell does not satify pred, stop
    ret))

;; starting from specified x and y and moving right,
;; count number of consecutive cell that satisfies (pred ch)
;; and return such number.
;;
;; notes: if y < 0 it is truncated to 0
;;        if y >= (vlines-length lines) returns #f
;;        if x < 0 it is truncated to 0
;;        if x > (vline-length (vlines-ref lines y))
;;               it is truncated to vline-length
(define (vlines-count-right lines x y pred)
  (let ((ny (vlines-length lines))
        (ret 0))
    (set! y (fxmax 0 y))
    (while (fx<? y ny)
      (let* ((line (vlines-ref lines y))
             (len  (vline-length line))
             (xx   (fxmax 0 (fxmin x len)))
             (n    (vline-count-right line xx pred)))
        (set! ret (fx+ ret n))
        (if (fx=? n (fx- len xx))
          (begin ;; all cells satisfy pred, continue on previous line
            (set! x 0)
            (set! y (fx1+ y)))
          (set! y ny)))) ;; some cell does not satify pred, stop
    ret))


;; return #t if lines start with range [0 0, endx endy) of prefix - which must be a vlines too -
;; otherwise return #f
(define (vlines-starts-with? lines prefix endx endy)
  (let %again ((x1 0) (y1 0) (x2 0) (y2 0))
    (let ((ch1 (and x1 y1 (vlines-cell-at-xy lines x1 y1)))
          (ch2 (and x2 y2 (vlines-cell-at-xy prefix x2 y2))))
    ; (debugf "... vlines-starts-with? xy+ch1=(~s ~s ~s) xy+ch2=(~s ~s ~s)" x1 y1 ch1 x2 y2 ch2)
    (cond
      ((not ch2) ; reached end of prefix
        #t)
      ((or (fx>? y2 endy) (and (fx=? y2 endy) (fx>=? x2 endx))) ; reached end of prefix range
        #t)
      ((not ch1) ; reached end of lines
        #f)
      ((char=? ch1 ch2)
        (let-values (((x1 y1) (vlines-next-xy lines x1 y1)))
          (let-values (((x2 y2) (vlines-next-xy prefix x2 y2)))
            (%again x1 y1 x2 y2))))
      (else                    ; found different cells
        #f)))))


;; return position one cell to the left of x y.
;; returned position may be on the previous line.
;; return #f #f if x y is out of range or is the first valid position.
(define (vlines-prev-xy lines x y)
  (let ((ymax  (fx1- (vlines-length lines))))
    (if (and (fixnum? x) (fixnum? y) (fx<=? 0 y ymax))
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (vline-length (vlines-ref lines y))
                       (if (fx=? y ymax) 0 1))))
        ; (debugf "vlines-length-prev-xy xy = (~s ~s), xmax = ~s" x y xmax)
        (if (fx<=? 1 x xmax)
          (values (fx1- x) y) ;; (x-1 y) is a valid position, return it
          (if (fx>? y 0)
            ;; return last position in previous line
            (values (fx1- (vline-length (vlines-ref lines (fx1- y)))) (fx1- y))
            (values #f #f))))
      (values #f #f))))


;; return position one cell to the right of x y.
;; returned position may be on the next line.
;; return #f #f if x y is out of range or is the last valid position.
(define (vlines-next-xy lines x y)
  (let ((ymax  (fx1- (vlines-length lines))))
    (if (and (fixnum? x) (fixnum? y) (fx<=? 0 y ymax))
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (vline-length (vlines-ref lines y))
                       (if (fx=? y ymax) 0 1))))
        ; (debugf "vlines-length-next-xy xy = (~s ~s), xmax = ~s" x y xmax)
        (if (fx<? -1 x xmax)
          (values (fx1+ x ) y) ;; (x+1 y) is a valid position, return it
          (if (fx<? y ymax)
            (values 0 (fx1+ y)) ;; return beginning of next line
            (values #f #f))))
      (values #f #f))))


;; return vlines char at specified x y, or #f if x y are out of range
(define (vlines-cell-at-xy lines x y)
  (if (and (fixnum? x) (fixnum? y) (fx<? -1 y (vlines-length lines)))
    (let* ((line (vlines-ref lines y))
           (len  (vline-length line)))
      (if (fx<? -1 x len)
        (vline-ref line x)
        #f))
    #f))

;; return position immediately before x y, and char at such position.
;; return #f #f #f if x y are out of range or 0 0.
(define (vlines-cell-before-xy lines x y)
  (let-values (((x y) (vlines-prev-xy lines x y)))
    (values x y (and x y (vlines-cell-at-xy lines x y)))))


;; return position immediately after x y, and char at such position.
;; return #f #f #f if x y are out of range.
;; return x+1 y #f if x y correspond to the last cell in the last line
(define (vlines-cell-after-xy lines x y)
  (let-values (((x y) (vlines-next-xy lines x y)))
    (values x y (and x y (vlines-cell-at-xy lines x y)))))


;; create and return a closure that iterates on elements of vlines lines.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in vlines lines and #t,
;; or (values #<unspecified> #f) if end of vlines is reached.
(define in-vlines
  (case-lambda
    ((lines start end step)
      (assert* 'in-vlines (fx<=?* 0 start end (vlines-length lines)))
      (assert* 'in-vlines (fx>=? step 0))
      (let ((%in-vlines ; name shown when displaying the closure
              (lambda ()
                (if (fx<? start end)
                  (let ((elem (vlines-ref lines start)))
                    (set! start (fx+ start step))
                    (values elem #t))
                  (values #f #f)))))
        %in-vlines))
    ((lines start end)
      (in-vlines lines start end 1))
    ((lines)
      (in-vlines lines 0 (vlines-length lines) 1))))


;; customize how "vlines" objects are printed
(record-writer (record-type-descriptor %vlines)
  (lambda (lines port writer)
    (display "(vlines " port)
    (vlines-iterate lines
      (lambda (i line)
        (display #\space port)
        (write line port)))
    (display ")" port)))

) ; close library
