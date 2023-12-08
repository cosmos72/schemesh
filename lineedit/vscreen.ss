;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit vscreen (0 1))
  (export
    vscreen  vscreen*  vscreen?
    vscreen-cursor-x   vscreen-cursor-x-set!   vscreen-cursor-y   vscreen-cursor-y-set!
    vscreen-width      vscreen-width-set!      vscreen-height     vscreen-height-set!
    vscreen-prompt-len vscreen-prompt-len-set!
    vscreen-cursor-move/left! vscreen-cursor-move/right! vscreen-cursor-move/up! vscreen-cursor-move/down!
    vscreen-erase/left!       vscreen-erase/right!       vscreen-erase-at/xy!
    write-vscreen)

  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer)
    (schemesh bootstrap)       ;; while
    (schemesh containers span)
    (schemesh containers charline)
    (schemesh containers charlines))


;; copy-pasted from containers/gbuffer.ss
(define-record-type
  (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left)
     (mutable right))
  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))


;; copy-pasted from containers/charlines.ss
(define-record-type
  (%charlines %make-charlines %charlines?)
  (parent %gbuffer)
  (fields
    (mutable dirty-y-start)
    (mutable dirty-y-end))
  (nongenerative #{%charlines lf2lr8d65f8atnffcpi1ja7l0-439}))


;; vscreen is an in-memory representation of user-typed input
;; and how it is split in multiple lines, each limited by screen width
(define-record-type
  (%vscreen %make-vscreen vscreen?)
  (parent %charlines)
  (fields
    (mutable cursor-x   vscreen-cursor-x   vscreen-cursor-x-set!  )  ;; cursor x position
    (mutable cursor-y   vscreen-cursor-y   vscreen-cursor-y-set!  )  ;; cursor y position
    (mutable width      vscreen-width      vscreen-width-set!     )  ;; screen width
    (mutable height     vscreen-height     vscreen-height-set!    )  ;; screen height
    (mutable prompt-len vscreen-prompt-len vscreen-prompt-len-set!)) ;; prompt length
  (nongenerative #{%vscreen jrk9oih6lhpsih9dh3qu06xvo-525}))


(define (assert-vscreen? who screen)
  (unless (vscreen? screen)
    (assertion-violation who "not a vscreen" screen)))


;; create a vscreen
(define (vscreen)
  (%make-vscreen (span) (span (charline)) (greatest-fixnum) 0 0 0 80 24 0))


;; create a vscreen referencing specified strings, one per charline
(define (vscreen* width height . strings)
  (let ((sp (list->span strings)))
    (span-iterate sp
      (lambda (i elem)
        (span-set! sp i (string->charline* elem))))
    (%make-vscreen (span) sp (greatest-fixnum) 0 0 0 width height 0)))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (charlines-length screen))
    (charlines-ref screen y)
    #f))

;; return vscreen line length at specified y, or 0 if y is out of range.
;; implicit newline is counted as a character i.e. increases returned value by one.
(define (vscreen-length-at-y screen y)
  (let ((line (vscreen-line-at-y screen y)))
    (if line
      (fx+ (charline-length line) (if (charline-nl? line) 1 0))
      0)))


;; return vscreen width i.e. maximum charline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length must be subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-len screen) 0)))


;; move vscreen cursor n characters to the left.
;; Each time the beginning of a line is reached, moving further left
;;   means moving to the end of previous line.
;; Every implicit newline counts as a character.
;; Return number of characters skipped/moved, including implicit newlines.
(define (vscreen-cursor-move/left! screen n)
  (let ((x (vscreen-cursor-x screen))
        (y (vscreen-cursor-y screen))
        (saved-n n))
    (while (and (fx>? n 0) (or (fx>? y 0) (fx>? x 0)))
      (let ((delta (fxmax 0 (fxmin x n))))
        (set! n (fx- n delta))
        (set! x (fx- x delta)))
      (when (and (fx>? n 0) (fxzero? x) (fx>? y 0))
          ;; move to end of previous line
        (set! n (fx1- n))
        (set! y (fx1- y))
        (set! x (fx1- (vscreen-length-at-y screen y)))))
    (vscreen-cursor-x-set! screen x)
    (vscreen-cursor-y-set! screen y)
    (fx- saved-n n)))


;; move vscreen cursor n characters to the right.
;; Each time the end of a line is reached, moving further right
;;   means moving to the beginning of next line.
;; Every implicit newline counts as a character.
;; Return number of characters skipped/moved, including implicit newlines.
(define (vscreen-cursor-move/right! screen n)
  (let* ((x (vscreen-cursor-x screen))
         (y (vscreen-cursor-y screen))
         (ymax (fx1- (charlines-length screen)))
         (xmax (fx1- (vscreen-length-at-y screen ymax)))
         (saved-n n))
    (assert (fx>=? ymax 0))
    (assert (fx>=? xmax 0))
    (while (and (fx>? n 0) (fx<=? y ymax) (or (fx<? y ymax) (fx<? x xmax)))
      (let* ((linemax (fx1- (vscreen-length-at-y screen y)))
             (delta   (fxmax 0 (fxmin n (fx- linemax x)))))
        (set! n (fx- n delta))
        (set! x (fx+ x delta))
        (when (and (fx>? n 0) (fx=? x linemax) (fx<? y ymax))
          ;; move to beginning of next line
          (set! n (fx1- n))
          (set! y (fx1+ y))
          (set! x 0))))
    (when (and (fx>? n 0) (fx=? y ymax) (fx=? x xmax)
               (not (charline-nl? (charlines-ref screen y))))
      ;; allow moving cursor immediately after last character of last line
      (set! n (fx1- n))
      (set! x (fx1+ x)))
    (vscreen-cursor-x-set! screen x)
    (vscreen-cursor-y-set! screen y)
    (fx- saved-n n)))


;; move vscreen cursor n characters up.
(define (vscreen-cursor-move/up! screen n)
  (when (fx>? n 0)
    (let* ((y          (vscreen-cursor-y screen))
           (ynew       (fxmax 0 (fx- y n)))
           (xmax       (fx1- (vscreen-length-at-y screen ynew)))
           (prompt-len (vscreen-prompt-len screen))
           (x          (fx+ (vscreen-cursor-x screen) (if (fxzero? y) prompt-len 0)))
           (xnew       (fxmin x xmax)))
      (assert (fx>=? xmax 0))
      (when (fxzero? ynew)
        (set! x (fxmax 0 (fx- x prompt-len))))
      (vscreen-cursor-x-set! screen xnew)
      (vscreen-cursor-y-set! screen ynew))))


;; move vscreen cursor n characters down.
(define (vscreen-cursor-move/down! screen n)
  (when (fx>? n 0)
    (let* ((y          (vscreen-cursor-y screen))
           (ymax       (fx1- (charlines-length screen)))
           (ynew       (fxmin ymax (fx+ y n)))
           (prompt-len (vscreen-prompt-len screen))
           (x          (fx+ (vscreen-cursor-x screen) (if (fxzero? y) prompt-len 0)))
           (line       (charlines-ref screen ynew))
           (len        (charline-length line))
           (xmax       (fx- len (if (or (fx=? ymax ynew) (charline-nl? line)) 0 1)))
           (xnew       (fxmin x xmax)))
      (assert (fx>=? ymax 0))
      (when (fxzero? ynew)
        (set! x (fxmax 0 (fx- x prompt-len))))
      (vscreen-cursor-x-set! screen xnew)
      (vscreen-cursor-y-set! screen ynew))))


;; append characters to vscreen line at y removing them from the beginning of line at y+1
;; and repeat with lines *without* an implicit newline at y+2... until line is refilled
;; up to vscreen width
(define (vscreen-underflow-at-y screen y)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when (and line1 (not (charline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen y) (charline-length line1))))
        (while (and (fx>? n 0) (fx<? (fx1+ y) (charlines-length screen)))
          (let* ((line2 (vscreen-line-at-y screen (fx1+ y)))
                 (i     (fxmin n (charline-length line2))))
            (assert-charline? "vscreen-erase/..." line2)
            ;; insert chars into line1
            (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 i)
            ;; remove chars from line2
            (charline-erase-at! line2 0 i)
            (set! n (fx- n i))
            (when (charline-empty? line2)
              (when (charline-nl? line2)
                (charline-nl?-set! line1 #t)
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it
              (charlines-erase-at/cline! screen (fx1+ y)))))))))


;; erase n characters of vscreen starting from specified x and y and moving rightward.
;; if the implicit newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase-at/xy! screen x y n)
  (when (fx>? n 0)
    (let ((saved-y y))
      (while (and (fx>? n 0) (fx<? -1 y (charlines-length screen)))
        (let* ((line (charlines-ref screen y))
               (len  (charline-length line))
               (i    (fxmin n (fx- len x))))
          (when (fx>? i 0)
            ;; erase i characters
            (charline-erase-at! line x i)
            (set! n (fx- n i))
            (set! len (fx- len i)))
          (when (and (fx>? n 0) (charline-nl? line))
            ;; also erase the implicit newline
            (charline-nl?-set! line #f)
            (set! n (fx1- n)))
          (when (fx>=? x len)
            ;; erased until the end of line, continue with next line
            (if (and (fxzero? len) (not (charline-nl? line)))
              ;; line is now empty, remove it
              (charlines-erase-at/cline! screen y)
              ;; line is not empty, move to next line
              (begin
                (set! x 0)
                (set! y (fx1+ y)))))))
      (vscreen-underflow-at-y screen saved-y))))


;; erase n characters from vscreen, starting at cursor and moving rightward.
;; if the implicit newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase/right! screen n)
  (vscreen-erase-at/xy! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen) n))


;; erase n characters from vscreen, starting 1 char left of cursor and moving leftward.
;; if the implicit newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase/left! screen n)
  (let ((n (vscreen-cursor-move/left! screen n)))
    (vscreen-erase-at/xy! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen) n)))


;; write a textual representation of vscreen to output port
(define (write-vscreen screen port)
  (assert-vscreen? "write-vscreen" screen)
  (display "(vscreen* " port)
  (display (vscreen-width screen) port)
  (display #\space port)
  (display (vscreen-height screen) port)
  (charlines-iterate screen
    (lambda (i line)
      (display #\space port)
      (write (charline->string line) port)))
  (display ")" port))


;; customize how "vscreen" objects are printed
(record-writer (record-type-descriptor %charlines)
  (lambda (screen port writer)
    (write-vscreen screen port)))

) ; close library
