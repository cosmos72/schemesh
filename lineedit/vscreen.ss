;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit vscreen (0 1))
  (export
    vscreen  vscreen*  vscreen?  assert-vscreen?
    vscreen-width        vscreen-height    vscreen-resize!
    vscreen-cursor-x     vscreen-cursor-y  vscreen-cursor-xy   vscreen-cursor-xy-set!
    vscreen-vcursor-xy   vscreen-vcursor-xy-set!
    vscreen-prompt-end-x vscreen-prompt-end-x-set! vscreen-end-y
    vscreen-clear!       vscreen-empty?
    vscreen-cursor-move/left! vscreen-cursor-move/right!  vscreen-cursor-move/up! vscreen-cursor-move/down!
    vscreen-erase-left/n!    vscreen-erase-right/n!       vscreen-erase-at-xy!
    vscreen-erase-left/line! vscreen-erase-right/line!
    vscreen-find-at-xy/left  vscreen-find-at-xy/right
    vscreen-insert-at-xy/ch! vscreen-insert-at-xy/newline! vscreen-insert-at-xy/cspan!
    vscreen-insert/ch!       vscreen-insert/cspan!
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
    (mutable prompt-end-x vscreen-prompt-end-x vscreen-prompt-end-x-set!)) ;; prompt length
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

;; return number of charlines in vscreen
(define vscreen-end-y charlines-length)


;; return vscreen width i.e. maximum charline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length must be subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-end-x screen) 0)))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (vscreen-end-y screen))
    (charlines-ref screen y)
    #f))

;; return charline length at specified y, or 0 if y is out of range.
(define (vscreen-length-at-y screen y)
  (if (fx<? -1 y (vscreen-end-y screen))
    (charline-length (charlines-ref screen y))
    0))

;; return tow values: cursor x and y position
(define (vscreen-cursor-xy screen)
  (values (vscreen-cursor-x screen) (vscreen-cursor-y screen)))


;; set cursor x and y position. notes:
;; * x and y values will be clamped to existing charlines and their lengths
;; * x can also be set to immediately after the end of *last* charline
(define (vscreen-cursor-xy-set! screen x y)
  (let* ((ymax  (fx1- (vscreen-end-y screen)))
         (y     (fxmax 0 (fxmin y ymax)))
         ;; allow positioning cursor after end of line only for *last* charline
         (xmax  (fx- (vscreen-length-at-y screen y)
                     (if (fx=? y ymax) 0 1)))
         (x     (fxmax 0 (fxmin x xmax))))
    (vscreen-cursor-y-set! screen y)
    (vscreen-cursor-x-set! screen x)))

;; return visual cursor x position. it is equal to vscreen-cursor-x,
;; except for first line where vscreen-prompt-end-x must be added.
(define (vscreen-vcursor-x screen)
  (fx+ (vscreen-cursor-x screen)
       (if (fxzero? (vscreen-cursor-y screen))
         (vscreen-prompt-end-x screen)
         0)))

;; return visual cursor y position. it is equal to vscreen-cursor-y.
(define vscreen-vcursor-y vscreen-cursor-y)

;; return two values: visual cursor x, y position
(define (vscreen-vcursor-xy screen)
  (values (vscreen-vcursor-x screen)
          (vscreen-vcursor-y screen)))

;; set visual cursor x and y position. equivalent to calling vscreen-cursor-xy-set!,
;; with the following difference:
;; * if clamped y is 0, vscreen-prompt-end-x will be subtracted from x
(define (vscreen-vcursor-xy-set! screen x y)
  (let* ((ymax  (fx1- (vscreen-end-y screen)))
         (y     (fxmax 0 (fxmin y ymax)))
         ;; allow positioning cursor after end of line only for *last* charline
         (xmax  (fx- (vscreen-length-at-y screen y)
                     (if (fx=? y ymax) 0 1)))
         ;; subtract prompt length only for *first* charline
         (xreal (fx- x (if (fxzero? y) (vscreen-prompt-end-x screen) 0)))
         (x     (fxmax 0 (fxmin xreal xmax))))
    (vscreen-cursor-y-set! screen y)
    (vscreen-cursor-x-set! screen x)))



;; return total number of characters in vscreen before cursor,
;; including characters in previous lines if cursor y > 0.
(define (vscreen-cursor-count/left screen)
  (let ((n     (vscreen-cursor-x screen))
        (y     (vscreen-cursor-y screen))
        (end-y (vscreen-end-y screen)))
    (while (fx<? 0 y end-y)
      (set! y (fx1- y))
      (set! n (fx+ n (charline-length (charlines-ref screen y)))))
    n))

;; change vscreen width and height. Also reflows screen.
;; moves cursor to preserve total # of characters before cursor.
(define (vscreen-resize! screen width height)
  (let ((width (fxmax 1 width))
        (height (fxmax 1 height))
        (old-width (vscreen-width screen))
        (old-height (vscreen-height screen)))
    (unless (and (fx=? width old-width) (fx=? height old-height))
      (let ((reflow-func (if (fx<? width old-width)
                            vscreen-overflow-at-y
                            vscreen-underflow-at-y))
            (n (vscreen-cursor-count/left screen))
            (y 0))
        (vscreen-width-set! screen width)
        (vscreen-height-set! screen height)
        (while (fx<? y (fx1- (vscreen-end-y screen)))
          (set! y (fx1+ (reflow-func screen y))))
        (vscreen-cursor-xy-set! screen 0 0)
        (vscreen-cursor-move/right! screen n)))))



;; remove all lines from screen, and set cursor to 0 0
(define (vscreen-clear! screen)
  (charlines-clear! screen)
  (charlines-insert-at/cline! screen 0 (charline))
  (vscreen-cursor-x-set! screen 0)
  (vscreen-cursor-y-set! screen 0))


;; return #t if vscreen is empty i.e. it contains at most a single charline having zero length
(define (vscreen-empty? screen)
  (case (vscreen-end-y screen)
    ((0) #t)
    ((1) (fxzero? (charline-length (charlines-ref screen 0))))
    (else #f)))


;; move vscreen cursor n characters to the left.
;; Each time the beginning of a line is reached, moving further left
;;   means moving to the end of previous line.
;; Every newline counts as a character.
;; Return number of characters skipped/moved, including newlines.
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
;; Every newline counts as a character.
;; Return number of characters skipped/moved, including newlines.
(define (vscreen-cursor-move/right! screen n)
  (let* ((x (vscreen-cursor-x screen))
         (y (vscreen-cursor-y screen))
         (ymax (fx1- (vscreen-end-y screen)))
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
;; and repeat with subsequent lines *without* a newline at y+2...
;; until line is refilled up to vscreen width.
;; stops when a modified line has a newline or has length = vscreen width.
;; return y of last modified line.
(define (vscreen-underflow-at-y screen y)
  (let ((line1 (vscreen-line-at-y screen y)))
    (if (and line1 (not (charline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen y) (charline-length line1)))
            (y+1 (fx1+ y)))
        (while (and (fx>? n 0) (fx<? y+1 (vscreen-end-y screen)))
          (let* ((line2 (charlines-ref screen y+1))
                 (line2-nl? (charline-nl? line2))
                 (i     (fxmin n (charline-length line2))))
            ;; insert chars into line1
            (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 i)
            ;; remove chars from line2
            (charline-erase-at! line2 0 i)
            (set! n (fx- n i))
            (when (charline-empty? line2)
              (when line2-nl?
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it
              (charlines-erase-at/cline! screen y+1))))
        y+1)
      y)))


;; erase n characters of vscreen starting from specified x and y and moving rightward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-erase-at-xy! screen x y n)
  (let ((saved-n n))
    (when (fx>? n 0)
      (let ((saved-y y))
        (while (and (fx>? n 0) (fx<? -1 y (vscreen-end-y screen)))
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
        (unless (fx=? saved-n n)
          (vscreen-underflow-at-y screen saved-y))))
    (fx- saved-n n)))


;; erase n characters from vscreen, starting 1 char left of cursor and moving leftward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-erase-left/n! screen n)
  (let ((n (vscreen-cursor-move/left! screen n)))
    (vscreen-erase-at-xy! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen) n)))


;; erase n characters from vscreen, starting at cursor and moving rightward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
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


;; return three values:
;; position x y of rightmost character that is at position < x0 y0 and satisfies (pred ch),
;; and number of characters between x y and x0 y0.
;; return #f #f #f if no character before position x0 y0 satisfies (pred ch).
;; Note: never returns x0 y0 0, because position x0 y0 is *not* examined.
(define (vscreen-find-at-xy/left screen x0 y0 pred)
  (let ((found? #f)
        (xret (fx1- x0))
        (yret y0)
        (count 1))
    (do ((y (fxmin y0 (fx1- (vscreen-end-y screen))) (fx1- y)))
        ((or found? (fx<? y 0))
          (if found? (values xret yret count) (values #f #f #f)))
      (let ((line (charlines-ref screen y)))
        (do ((x (fx1- (fxmin x0 (charline-length line))) (fx1- x)))
            ((or found? (fx<? x 0)))
          (if (pred (charline-ref line x))
            (begin
              (set! found? #t)
              (set! xret x)
              (set! yret y))
            (set! count (fx1+ count))))
        (set! x0 (greatest-fixnum))))))


;; return three values:
;; position x y of leftmost character that is at position >= x0 y0 and satisfies (pred ch),
;; and number of characters between x y and x0 y0.
;; return #f #f #f if no character at position x0 y0 or right of it satisfies (pred ch).
;; Note: may also return x0 y0 0, because position x0 y0 is examined.
(define (vscreen-find-at-xy/right screen x0 y0 pred)
  (let ((found? #f)
        (xret x0)
        (yret y0)
        (yn (vscreen-end-y screen))
        (count 0))
    (do ((y (fxmax y0 0) (fx1+ y)))
        ((or found? (fx>=? y yn))
           (if found? (values xret yret count) (values #f #f #f)))
      (let* ((line (charlines-ref screen y))
             (xn   (charline-length line)))
        (do ((x (fxmax x0 0) (fx1+ x)))
            ((or found? (fx>=? x xn)))
          (if (pred (charline-ref line x))
            (begin
              (set! found? #t)
              (set! xret x)
              (set! yret y))
            (set! count (fx1+ count))))
        (set! x0 0)))))


;; move characters from end of vscreen line at y to the beginning of line at y+1
;; and repeat on subsequent lines *without* a newline at y+2...
;; until lengths of modified lines are all <= vscreen width.
;; stops when a modified line has length <= vscreen width.
;; return y of last modified line.
(define (vscreen-overflow-at-y screen y)
  ; (format #t "before overflow at ~s: ~s~%" y screen)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when line1
      (while (and (fx<? y (vscreen-end-y screen))
                  (fx>? (charline-length line1) (vscreen-width-at-y screen y)))
        (let* ((line1-nl?  (charline-nl? line1))
               (line1-len  (charline-length line1))
               (n          (fx- line1-len (vscreen-width-at-y screen y)))
               (line1-pos  (fx- line1-len n))
               (y+1        (fx1+ y))
               (line2-new? (or line1-nl? (fx=? y+1 (vscreen-end-y screen))))
               (line2      (if line2-new?
                             (charline)
                             (charlines-ref screen y+1)))
               (line2-nl? (charline-nl? line2)))
          (when line2-new?
            (charlines-insert-at/cline! screen y+1 line2))
          ;; insert chars into line2
          (charline-insert-at/cbuf! line2 0 line1 line1-pos n)
          ;; remove chars from line1
          (charline-erase-at! line1 line1-pos n)
          ; (format #t "during overflow at ~s: ~s~%" y screen)
          ;; iterate on line2, as it may now have length > vscreen-width-at-y
          (set! y y+1)
          (set! line1 line2)))))
  ; (format #t "after  overflow at ~s: ~s~%" y screen)
  y)


;; prepare vscreen for insertion at specified x and y
;; return three values: x and y clamped to valid range, and charline at clamped y.
;; may insert additional lines if needed.
(define (vscreen-insert-at-xy/before! screen x y)
  (when (charlines-empty? screen) ;; should not happen
    (charlines-insert-at/cline! screen 0 (charline)))
  (let* ((ylen (vscreen-end-y screen))
         (y    (fxmax 0 (fxmin y (fx1- ylen))))
         (line (charlines-ref screen y))
         (xlen (charline-length line))
         (x    (fxmax 0 (fxmin x xlen))))
    (if (and (fx=? x xlen) (charline-nl? line))
      ;; cannot insert after a newline, return beginning of following line instead
      (let ((y+1 (fx1+ y)))
        (when (fx=? y+1 ylen)
          (charlines-insert-at/cline! screen y+1 (charline)))
        (values 0 y+1 (charlines-ref screen y+1)))
      ;; return position inside line, or at end of it
      (values x y line))))


;; insert a single character into vscreen at specified x and y.
;; Character must be printable, i.e. (char>=? ch #\space).
;; If the line at y becomes longer than vscreen-width-at-y,
;; move extra characters into the following line(s)
;; i.e. reflow them according to vscreen width.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/ch! screen x y ch)
  (assert (char>=? ch #\space))
  (let-values (((x y line) (vscreen-insert-at-xy/before! screen x y)))
    (charline-insert-at! line x ch)
    (vscreen-overflow-at-y screen y)))


;; insert a newline, which may split the current line in two,
;; into vscreen at specified x and y.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/newline! screen x y)
  (let-values (((x y line1) (vscreen-insert-at-xy/before! screen x y)))
    (let* ((x+1   (fx1+ x))
           (y+1   (fx1+ y))
           (n     (fx- (charline-length line1) x)) ;; # of chars to move to line2
           (create-line2? (or (charline-nl? line1)
                              (fx=? y+1 (vscreen-end-y screen))))
           (line2 (if create-line2? (charline) (charlines-ref screen y+1))))
      (charline-insert-at! line1 x #\newline)
      (when create-line2?
        (charlines-insert-at/cline! screen y+1 line2))
      (if (fx>? n 0)
        (begin
          ;; insert into line2 the chars from line1 after inserted newline
          (charline-insert-at/cbuf! line2 0 line1 x+1 n)
          ;; remove from line1 the chars after inserted newline
          (charline-erase-at! line1 x+1 n)
          (unless (charline-nl? line2)
            (if create-line2?
              (vscreen-underflow-at-y screen y+1) ;; created line2 may be too short
              (vscreen-overflow-at-y screen y+1)))) ;; we appended to existing line2 => may overflow
        ;; we appended newline to line1 as additional last char => may overflow
        (vscreen-overflow-at-y screen y)))))


;; insert part of a charspan into vscreen at specified x and y.
;; if the line at y becomes longer than vscreen-width-at-y,
;; move extra characters into the following line(s)
;; i.e. reflow them according to vscreen width.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/cspan! screen x y csp csp-start csp-n)
  (when (fx>? csp-n 0)
    (let-values (((x y line) (vscreen-insert-at-xy/before! screen x y)))
      (charline-insert-at/cspan! line x csp csp-start csp-n)
      (vscreen-overflow-at-y screen y))))


;; insert a char, which can be a #\newline, into vscreen at cursor position
;; then move cursor right by one.
(define (vscreen-insert/ch! screen ch)
  (let ((x (vscreen-cursor-x screen))
        (y (vscreen-cursor-y screen)))
    (if (char=? ch #\newline)
      (vscreen-insert-at-xy/newline! screen x y)
      (vscreen-insert-at-xy/ch! screen x y ch))
    (vscreen-cursor-move/right! screen 1)))


;; insert part of a charspan into vscreen at cursor position,
;; then move cursor right by csp-n.
(define (vscreen-insert/cspan! screen csp csp-start csp-n)
  (when (fx>? csp-n 0)
    (vscreen-insert-at-xy/cspan! screen (vscreen-cursor-x screen) (vscreen-cursor-y screen)
                                 csp csp-start csp-n)
    (vscreen-cursor-move/right! screen csp-n)))


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
