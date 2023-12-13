;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit vscreen (0 1))
  (export
    vscreen  vscreen*  vscreen?  assert-vscreen?
    vscreen-width-height  vscreen-width-height-set!
    vscreen-vcursor-xy    vscreen-vcursor-xy-set!
    vscreen-prompt-len    vscreen-prompt-len-set!
    vscreen-cursor-move/left!  vscreen-cursor-move/right!  vscreen-cursor-move/up! vscreen-cursor-move/down!
    vscreen-erase-left/n!    vscreen-erase-right/n!     vscreen-erase-at-xy!
    vscreen-erase-left/line! vscreen-erase-right/line!
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


;; vscreen is an in-memory representation of user-typed input and how it is split
;; in multiple lines, each limited either by a newline or by screen width
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

;; return two values: vscreen width and height
(define (vscreen-width-height screen)
  (values (vscreen-width screen)
          (vscreen-height screen)))

;; set vscreen width and height
(define (vscreen-width-height-set! screen width height)
  (vscreen-width-set! screen (fxmax 1 width))
  (vscreen-height-set! screen (fxmax 1 height)))


;; return vscreen width i.e. maximum charline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length must be subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-len screen) 0)))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (charlines-length screen))
    (charlines-ref screen y)
    #f))

;; return charline length at specified y, or 0 if y is out of range.
(define (vscreen-length-at-y screen y)
  (if (fx<? -1 y (charlines-length screen))
    (charline-length (charlines-ref screen y))
    0))

;; return visual cursor x position. it is equal to vscreen-cursor-x,
;; except for first line where vscreen-prompt-len must be added.
(define (vscreen-vcursor-x screen)
  (fx+ (vscreen-cursor-x screen)
       (if (fxzero? (vscreen-cursor-y screen))
         (vscreen-prompt-len screen)
         0)))

;; return visual cursor y position. it is equal to vscreen-cursor-y.
(define vscreen-vcursor-y vscreen-cursor-y)

;; return two values: visual cursor x, y position
(define (vscreen-vcursor-xy screen)
  (values (vscreen-cursor-x screen)
          (vscreen-cursor-y screen)))

;; set visual cursor x and y position.
;; equivalent to calling separately vscreen-cursor-x-set! and vscreen-cursor-y-set!,
;; with the following differences:
;; * x and y values will be clamped to existing charlines and their lengths
;; * if clamped y is 0, vscreen-prompt-len will be subtracted from x
;; * x can also be set to immediately after the end of *last* charline
(define (vscreen-vcursor-xy-set! screen x y)
  (let* ((ymax  (fx1- (charlines-length screen)))
         (y     (fxmax 0 (fxmin y ymax)))
         ;; allow positioning cursor after end of line only for *last* charline
         (xmax  (fx- (vscreen-length-at-y screen y)
                     (if (fx=? y ymax) 0 1)))
         ;; subtract prompt length only for *first* charline
         (xreal (fx- x (if (fxzero? y) (vscreen-prompt-len screen) 0)))
         (x     (fxmax 0 (fxmin xreal xmax))))
    (vscreen-cursor-y-set! screen y)
    (vscreen-cursor-x-set! screen x)))


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
    (when (and (fx>? n 0) (fx=? y ymax) (fx=? x xmax))
      ;; allow moving cursor immediately after last character of last line
      (set! n (fx1- n))
      (set! x (fx1+ x)))
    (vscreen-cursor-x-set! screen x)
    (vscreen-cursor-y-set! screen y)
    (fx- saved-n n)))


;; move vscreen cursor n characters up.
(define (vscreen-cursor-move/up! screen n)
  (vscreen-vcursor-xy-set! screen
    (vscreen-vcursor-x screen)
    (fx- (vscreen-vcursor-y screen) n)))

;; move vscreen cursor n characters down.
(define (vscreen-cursor-move/down! screen n)
  (vscreen-vcursor-xy-set! screen
    (vscreen-vcursor-x screen)
    (fx+ (vscreen-vcursor-y screen) n)))


;; append characters to vscreen line at y removing them from the beginning of line at y+1
;; and repeat with lines *without* an implicit newline at y+2... until line is refilled
;; up to vscreen width
(define (vscreen-underflow-at-y screen y)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when (and line1 (not (charline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen y) (charline-length line1))))
        (while (and (fx>? n 0) (fx<? (fx1+ y) (charlines-length screen)))
          (let* ((line2 (vscreen-line-at-y screen (fx1+ y)))
                 (line2-nl? (charline-nl? line2))
                 (i     (fxmin n (charline-length line2))))
            (assert-charline? "vscreen-erase/..." line2)
            ;; insert chars into line1
            (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 i)
            ;; remove chars from line2
            (charline-erase-at! line2 0 i)
            (set! n (fx- n i))
            (when (charline-empty? line2)
              (when line2-nl?
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it
              (charlines-erase-at/cline! screen (fx1+ y)))))))))


;; erase n characters of vscreen starting from specified x and y and moving rightward.
;; if the implicit newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase-at-xy! screen x y n)
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
          (when (fx>=? x len)
            ;; erased until the end of line, continue with next line
            (if (fxzero? len)
              ;; line is now empty, remove it
              (charlines-erase-at/cline! screen y)
              ;; line is not empty, move to next line
              (begin
                (set! x 0)
                (set! y (fx1+ y)))))))
      (vscreen-underflow-at-y screen saved-y))))


;; erase n characters from vscreen, starting 1 char left of cursor and moving leftward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase-left/n! screen n)
  (let ((n (vscreen-cursor-move/left! screen n)))
    (vscreen-erase-at-xy! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen) n)))


;; erase n characters from vscreen, starting at cursor and moving rightward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width
(define (vscreen-erase-right/n! screen n)
  (vscreen-erase-at-xy! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen) n))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-erase-left/until-nl! screen)
  (let* ((y    (vscreen-cursor-y screen))
         (line (vscreen-line-at-y screen y)))
    (when line
      (charline-erase-at! line 0 (vscreen-cursor-x screen))
      (vscreen-cursor-x-set! screen 0)
      (set! y    (fx1- y))
      (set! line (vscreen-line-at-y screen y))
      (while (and line (not (charline-nl? line)))
        (charlines-erase-at/cline! screen y)
        (set! y    (fx1- y))
        (set! line (vscreen-line-at-y screen y)))
      (vscreen-cursor-x-set! screen (fx1+ y)))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-erase-right/until-nl! screen)
  (let* ((y    (vscreen-cursor-y screen))
         (line (vscreen-line-at-y screen y))
         (nl?  #f))
    (when line
      (set! nl? (charline-nl? line))
      (let* ((x (vscreen-cursor-x screen))
             (avail-n (fx- (charline-length line) x)))
        ;; don't erase the newline, if present
        (charline-erase-at! line x (fxmax 0 (fx- avail-n (if nl? 1 0)))))
      (while (and line (not nl?))
        (set! y    (fx1+ y))
        (set! line (vscreen-line-at-y screen y))
        (when line
          (set! nl? (charline-nl? line))
          ;; also erase the newline, if present
          (charlines-erase-at/cline! screen y)))
      (set! y (vscreen-cursor-y screen))
      (set! line (vscreen-line-at-y screen y))
      (when (and nl? line (not (charline-nl? line)))
        ;; move the erased newline to current charline
        (charline-insert-at! line (charline-length line) #\newline)))))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define (vscreen-erase-left/line! screen)
  (let ((x    (vscreen-cursor-x screen))
        (line (vscreen-line-at-y screen (fx1- (vscreen-cursor-y screen)))))
    (if (and (fxzero? x) line (charline-nl? line))
      (vscreen-erase-left/n! screen 1)
      (vscreen-erase-left/until-nl! screen))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define (vscreen-erase-right/line! screen)
  (let ((x    (vscreen-cursor-x screen))
        (line (vscreen-line-at-y screen (vscreen-cursor-y screen))))
    (if (and line (eqv? #\newline (charline-at line x)))
      (vscreen-erase-right/n! screen 1)
      (vscreen-erase-right/until-nl! screen))))


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
