;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit vscreen (0 1))
  (export
    vscreen  vscreen*  vscreen?  assert-vscreen?
    vscreen-width        vscreen-height     vscreen-width-at-y  vscreen-resize!
    vscreen-dirty?       vscreen-dirty-set!
    vscreen-cursor-ix    vscreen-cursor-iy  vscreen-cursor-ixy  vscreen-cursor-ixy-set!
    vscreen-cursor-vx    vscreen-cursor-vy  vscreen-cursor-vxy  vscreen-cursor-vxy-set!
    vscreen-prompt-end-x vscreen-prompt-end-y vscreen-prompt-length  vscreen-prompt-length-set!
    vscreen-length-at-y  vscreen-length
    vscreen-char-at-xy   vscreen-char-before-xy  vscreen-char-after-xy
    vscreen-next-xy      vscreen-prev-xy   vscreen-next-xy/or-self  vscreen-prev-xy/or-self
    vscreen-count-at-xy/left  vscreen-count-at-xy/right
    vscreen-clear!       vscreen-empty?
    vscreen-cursor-move/left! vscreen-cursor-move/right!  vscreen-cursor-move/up!  vscreen-cursor-move/down!
    vscreen-erase-left/n!     vscreen-erase-right/n!      vscreen-erase-at-xy!
    vscreen-erase-left/line!  vscreen-erase-right/line!
    vscreen-insert-at-xy/ch!  vscreen-insert-at-xy/newline! vscreen-insert-at-xy/cspan!
    vscreen-insert/ch!        vscreen-insert/cspan!         vscreen-assign*!
    write-vscreen)

  (import
    (rnrs)
    (only (chezscheme) format fx1+ fx1- record-writer)
    (only (schemesh bootstrap) assert* while)
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
    ;; lines between y >= dirty-start-y and y < dirty-end-y
    ;; are completely dirty i.e. must be fully redrawn on screen
    (mutable dirty-start-y) ;; fixnum
    (mutable dirty-end-y))  ;; fixnum
  (nongenerative #{%charlines lf2lr8d65f8atnffcpi1ja7l0-439}))


;; vscreen is an in-memory representation of user-typed input and how it is split
;; in multiple lines, each limited either by a newline or by screen width.
;;
;; vscreen must always contain at least one (possibly empty) charline.
(define-record-type
  (%vscreen %make-vscreen vscreen?)
  (parent %charlines)
  (fields
    (mutable dirty?     vscreen-dirty?     %vscreen-dirty-set!  )  ;; boolean, #t if some line is dirty
    (mutable width      vscreen-width      vscreen-width-set!   )  ;; fixnum, screen width
    (mutable height     vscreen-height     vscreen-height-set!  )  ;; fixnum, screen height
    (mutable prompt-end-x vscreen-prompt-end-x vscreen-prompt-end-x-set!) ;; fixnum, x column where prompt ends
    (mutable prompt-end-y vscreen-prompt-end-y vscreen-prompt-end-y-set!) ;; fixnum, y row where prompt ends
    (mutable cursor-x   vscreen-cursor-ix   vscreen-cursor-ix-set!)  ;; fixnum, cursor x position
    (mutable cursor-y   vscreen-cursor-iy   vscreen-cursor-iy-set!)) ;; fixnum, cursor y position
  (nongenerative #{%vscreen jrk9oih6lhpsih9dh3qu06xvo-525}))


(define (assert-vscreen? who screen)
  (unless (vscreen? screen)
    (assertion-violation who "not a vscreen" screen)))


;; create a vscreen
(define (vscreen)
  (%make-vscreen (span) (span (charline)) (greatest-fixnum) 0 #f 80 24 0 0 0 0))


;; create a vscreen referencing specified strings, one per charline.
;; note: created screen should be reflowed, i.e. caller should invoke (vscreen-reflow screen)
(define (vscreen* width height . strings)
  (let ((sp (list->span strings)))
    (span-iterate sp
      (lambda (i elem)
        (span-set! sp i (string->charline* elem))))
    (%make-vscreen (span) sp (greatest-fixnum) 0 #f width height 0 0 0 0)))


;; return number of charlines in vscreen
(define vscreen-length charlines-length)

;; return prompt length in characters
(define (vscreen-prompt-length screen)
  (fx+ (vscreen-prompt-end-x screen)
       (fx* (vscreen-prompt-end-y screen)
            (vscreen-width screen))))

;; set prompt length in characters.
;; also updates vscreen-prompt-end-x and vscreen-prompt-end-y
(define (vscreen-prompt-length-set! screen prompt-length)
  (let ((width (vscreen-width screen)))
    (assert* 'vscreen-prompt-length-set! (fx>=? prompt-length 0))
    (assert* 'vscreen-prompt-length-set! (fx>? width 0))
    (let-values (((y x) (div-and-mod prompt-length width)))
      (assert* 'vscreen-prompt-length-set! (fx<? -1 x width))
      (vscreen-prompt-end-x-set! screen x)
      (vscreen-prompt-end-y-set! screen (fxmin y (fx1- (vscreen-height screen)))))))


(define (vscreen-dirty-set! screen flag?)
  (unless flag?
    (charlines-dirty-xy-unset! screen))
  (%vscreen-dirty-set! screen flag?))

;; return vscreen width i.e. maximum charline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length is subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-end-x screen) 0)))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (vscreen-length screen))
    (charlines-ref screen y)
    #f))

;; return charline length at specified y, or 0 if y is out of range.
(define (vscreen-length-at-y screen y)
  (if (fx<? -1 y (vscreen-length screen))
    (charline-length (charlines-ref screen y))
    0))

;; return vscreen char at specified x y, or #f if x y are out of range
(define (vscreen-char-at-xy screen x y)
  (if (fx<? -1 y (vscreen-length screen))
    (let* ((line (charlines-ref screen y))
           (len  (charline-length line)))
      (if (fx<? -1 x len)
        (charline-ref line x)
        #f))
    #f))


;; return tow values: cursor x and y position in input charlines
(define (vscreen-cursor-ixy screen)
  (values (vscreen-cursor-ix screen) (vscreen-cursor-iy screen)))


;; set cursor x and y position in input charlines. notes:
;; * x and y values will be clamped to existing charlines and their lengths
;; * x can also be set to immediately after the end of *last* charline
(define (vscreen-cursor-ixy-set! screen x y)
  (let* ((ymax  (fx1- (vscreen-length screen)))
         (y     (fxmax 0 (fxmin y ymax)))
         ;; allow positioning cursor after end of line only for *last* charline
         (xmax  (fx- (vscreen-length-at-y screen y)
                     (if (fx=? y ymax) 0 1)))
         (x     (fxmax 0 (fxmin x xmax))))
    (vscreen-cursor-iy-set! screen y)
    (vscreen-cursor-ix-set! screen x)))

;; return visual cursor x position. it is equal to vscreen-cursor-ix,
;; except for first input line where vscreen-prompt-end-x must be added.
(define (vscreen-cursor-vx screen)
  (fx+ (vscreen-cursor-ix screen)
       (if (fxzero? (vscreen-cursor-iy screen))
         (vscreen-prompt-end-x screen)
         0)))

;; return visual cursor y position. it is equal to vscreen-cursor-iy + vscreen-prompt-end-y
(define (vscreen-cursor-vy screen)
  (fx+ (vscreen-cursor-iy screen)
       (vscreen-prompt-end-y screen)))

;; return two values: visual cursor x, y position
(define (vscreen-cursor-vxy screen)
  (values (vscreen-cursor-vx screen)
          (vscreen-cursor-vy screen)))

;; set visual cursor x and y position. equivalent to calling vscreen-cursor-ixy-set!,
;; with the following differences:
;; * (vscreen-prompt-end-y screen) is subtracted from y
;; * if clamped y is 0, vscreen-prompt-end-x will be subtracted from x
(define (vscreen-cursor-vxy-set! screen x y)
  (let* ((iy (fx- y (vscreen-prompt-end-y screen)))
         (ix (fx- x (if (or (fx<=? iy 0) (fx<=? (vscreen-length screen) 1))
                     (vscreen-prompt-end-x screen)
                     0))))
    (vscreen-cursor-ixy-set! screen ix iy)))


;; return total number of characters in vscreen before position x y,
;; including characters in previous lines if y > 0.
(define (vscreen-count-to-start screen x y)
  (let ((n     x)
        (end-y (vscreen-length screen)))
    (while (fx<? 0 y end-y)
      (set! y (fx1- y))
      (set! n (fx+ n (charline-length (charlines-ref screen y)))))
    n))


;; return total number of characters in vscreen at position x y or after it,
;; including characters in next lines if y < maximum y.
(define (vscreen-count-to-end screen x y)
  (let ((n     0)
        (end-y (vscreen-length screen)))
    (while (fx<? 0 y end-y)
      (let* ((len (charline-length (charlines-ref screen y)))
             (xx  (fxmax 0 (fxmin x len))))
        (set! n (fx+ n (fx- len xx)))
        (set! x 0)
        (set! y (fx1- y))))
    n))

;; change vscreen width and height. Also reflows screen.
;; moves cursor to preserve total # of characters before cursor.
(define (vscreen-resize! screen width height)
  (let ((width (fxmax 1 width))
        (height (fxmax 1 height))
        (old-width (vscreen-width screen))
        (old-height (vscreen-height screen)))
    (unless (and (fx=? width old-width) (fx=? height old-height))
      (let ((n (vscreen-count-to-start screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen))))
        (vscreen-dirty-set! screen #t)
        (vscreen-width-set! screen width)
        (vscreen-height-set! screen height)
        (vscreen-reflow-prompt screen old-width)
        (vscreen-reflow screen)
        (vscreen-cursor-ixy-set! screen 0 0)
        (vscreen-cursor-move/right! screen n)))))


;; update prompt-end-x and prompt-end-y after screen resize
(define (vscreen-reflow-prompt screen old-width)
  (assert* 'vscreen-reflow-prompt (fx>? old-width 0))
  (let ((len (fx+ (vscreen-prompt-end-x screen) (fx* old-width (vscreen-prompt-end-y screen)))))
    (vscreen-prompt-length-set! screen len)))



;; remove all existing lines from screen, insert a new empty line, and set cursor to 0 0.
(define (vscreen-clear! screen)
  (vscreen-dirty-set! screen #t)
  (charlines-dirty-y-add! screen 0 (vscreen-length screen))
  ;; Implementation note: linectx-to-history* saves a shallow copy of vscreen to history,
  ;; which references the %gbuffer-left and %gbuffer-right internal spans of vscreen,
  ;; so we cannot continue using them: create new ones
  (%gbuffer-left-set! screen (span))
  (%gbuffer-right-set! screen (span (charline)))
  (vscreen-cursor-ix-set! screen 0)
  (vscreen-cursor-iy-set! screen 0))


;; return #t if vscreen is empty i.e. it contains at most a single charline having zero length
(define (vscreen-empty? screen)
  (case (vscreen-length screen)
    ((0) #t)
    ((1) (fxzero? (charline-length (charlines-ref screen 0))))
    (else #f)))


;; remove a line from screen. Last line cannot be removed, it will be cleared instead.
(define (vscreen-erase-at/cline! screen y)
  (vscreen-dirty-set! screen #t)
  (if (and (fxzero? y) (fx=? 1 (vscreen-length screen)))
    (charline-clear! (charlines-ref screen 0))
    (charlines-erase-at/cline! screen y)))



;; move vscreen cursor n characters to the left.
;; Each time the beginning of a line is reached, moving further left
;;   means moving to the end of previous line.
;; Every newline counts as a character.
;; Return number of characters skipped/moved, including newlines.
(define (vscreen-cursor-move/left! screen n)
  (let ((x (vscreen-cursor-ix screen))
        (y (vscreen-cursor-iy screen))
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
    (vscreen-cursor-ix-set! screen x)
    (vscreen-cursor-iy-set! screen y)
    (fx- saved-n n)))


;; move vscreen cursor n characters to the right.
;; Each time the end of a line is reached, moving further right
;;   means moving to the beginning of next line.
;; Every newline counts as a character.
;; Return number of characters skipped/moved, including newlines.
(define (vscreen-cursor-move/right! screen n)
  (let* ((x (vscreen-cursor-ix screen))
         (y (vscreen-cursor-iy screen))
         (ymax (fx1- (vscreen-length screen)))
         ;; x position immediately after last char is allowed
         (xmax (vscreen-length-at-y screen ymax))
         (saved-n n))
    ; (debugf "~%; vscreen-cursor-move/right! n ~s, x ~s, xmax ~s, y ~s, ymax ~s~%" n x xmax y ymax)
    (when (and (fx>=? ymax 0) (fx>=? xmax 0))
      (while (and (fx>? n 0) (fx<=? y ymax) (or (fx<? y ymax) (fx<? x xmax)))
        ;; x position immediately after last char is allowed
        (let* ((linemax (fx- (vscreen-length-at-y screen y) (if (fx=? y ymax) 0 1)))
               (delta   (fxmax 0 (fxmin n (fx- linemax x)))))
          (set! n (fx- n delta))
          (set! x (fx+ x delta))
          (when (and (fx>? n 0) (fx=? x linemax) (fx<? y ymax))
            ;; move to beginning of next line
            (set! n (fx1- n))
            (set! y (fx1+ y))
            (set! x 0))))
      (vscreen-cursor-ix-set! screen x)
      (vscreen-cursor-iy-set! screen y)
      (fx- saved-n n))))


;; move vscreen cursor n characters up.
(define (vscreen-cursor-move/up! screen n)
  (vscreen-cursor-vxy-set! screen
    (vscreen-cursor-vx screen)
    (fx- (vscreen-cursor-vy screen) n)))

;; move vscreen cursor n characters down.
(define (vscreen-cursor-move/down! screen n)
  (vscreen-cursor-vxy-set! screen
    (vscreen-cursor-vx screen)
    (fx+ (vscreen-cursor-vy screen) n)))


;; append characters to vscreen line at y by removing them from the beginning
;; of subsequent lines at y+1...
;; Stop when line at y is refilled up to vscreen width,
;; or when a newline is found and appended - whatever happens first.
;;
;; Warning: does not underflow subsequent lines!
;; Callers should use (vscreen-underflow) instead
(define (vscreen-underflow-at-y screen y)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when (and line1 (not (charline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen y) (charline-length line1)))
            (y+1 (fx1+ y)))
        (while (and (fx>? n 0) (fx<? y+1 (vscreen-length screen)))
          (let* ((line2 (charlines-ref screen y+1))
                 (line2-nl? (charline-nl? line2))
                 (i     (fxmin n (charline-length line2))))
            (when (fx>? i 0)
              (vscreen-dirty-set! screen #t)
              ;; insert chars into line1
              (charline-insert-at/cbuf! line1 (charline-length line1) line2 0 i)
              ;; remove chars from line2
              (charline-erase-at! line2 0 i)
              (set! n (fx- n i)))
            (when (charline-empty? line2)
              (when line2-nl?
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it unless line1 is full and line2 is last line
              (unless (and (or (charline-nl? line1)
                               (fx>=? (charline-length line1) (vscreen-width-at-y screen y)))
                           (fx>=? y+1 (fx1- (vscreen-length screen))))
                (vscreen-erase-at/cline! screen y+1)))))))))


;; for each vscreen line at y or later *without* a newline and shorter than vscreen width,
;; append characters to it removing them from the beginning of next line.
(define vscreen-underflow
  (let ((%vscreen-underflow
          (lambda (screen y)
            (do ((i y (fx1+ i)))
                ((fx>=? i (vscreen-length screen)))
              (vscreen-underflow-at-y screen i)))))
    (case-lambda
      ((screen)   (%vscreen-underflow screen 0))
      ((screen y) (%vscreen-underflow screen y)))))

;; erase n characters of vscreen starting from specified x and y and moving rightward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-erase-at-xy! screen x y n)
  (let ((saved-n n))
    (when (fx>? n 0)
      (vscreen-dirty-set! screen #t)
      (let ((saved-y y))
        (while (and (fx>? n 0) (fx<? -1 y (vscreen-length screen)))
          ;; (debugf "vscreen-erase-at-xy! ~s chars to delete at y = ~s~%" n y)
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
              (set! x 0)
              (if (and (fxzero? len) (not (vscreen-empty? screen)))
                ;; line is empty, remove it
                (vscreen-erase-at/cline! screen y)
                ;; line is not empty, move to next line
                (set! y (fx1+ y))))))
        (unless (fx=? saved-n n)
          (vscreen-underflow screen saved-y))))
    (fx- saved-n n)))


;; erase n characters from vscreen, starting 1 char left of cursor and moving leftward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-erase-left/n! screen n)
  (let ((n (vscreen-cursor-move/left! screen n)))
    (vscreen-erase-at-xy! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) n)))


;; erase n characters from vscreen, starting at cursor and moving rightward.
;; if the newline of a charline is erased, the following line(s)
;; are merged into the current charline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-erase-right/n! screen n)
  (vscreen-erase-at-xy! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) n))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-erase-left/until-nl! screen)
  (let* ((y    (vscreen-cursor-iy screen))
         (line (vscreen-line-at-y screen y)))
    (when line
      (vscreen-dirty-set! screen #t)
      (charline-erase-at! line 0 (vscreen-cursor-ix screen))
      (vscreen-cursor-ix-set! screen 0)
      (set! y    (fx1- y))
      (set! line (vscreen-line-at-y screen y))
      (while (and line (not (charline-nl? line)))
        (vscreen-erase-at/cline! screen y)
        (set! y    (fx1- y))
        (set! line (vscreen-line-at-y screen y)))
      (vscreen-cursor-iy-set! screen (fx1+ y)))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-erase-right/until-nl! screen)
  (let* ((x    (vscreen-cursor-ix screen))
         (y    (vscreen-cursor-iy screen))
         (line (vscreen-line-at-y screen y))
         (n    0))
    (while line
      (cond
        ((charline-nl? line)
          (set! n (fx+ n (fxmax 0 (fx1- (fx- (charline-length line) x)))))
          (set! line #f))
        (else
          (set! n (fx+ n (fxmax 0 (fx- (charline-length line) x))))
          (set! x 0)
          (set! y (fx1+ y))
          (set! line (vscreen-line-at-y screen y)))))
    (vscreen-erase-right/n! screen n)))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define (vscreen-erase-left/line! screen)
  (let ((x    (vscreen-cursor-ix screen))
        (line (vscreen-line-at-y screen (fx1- (vscreen-cursor-iy screen)))))
    (if (and (fxzero? x) line (charline-nl? line))
      (vscreen-erase-left/n! screen 1)
      (vscreen-erase-left/until-nl! screen))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define (vscreen-erase-right/line! screen)
  (let ((x    (vscreen-cursor-ix screen))
        (line (vscreen-line-at-y screen (vscreen-cursor-iy screen))))
    (if (and line (eqv? #\newline (charline-at line x)))
      (vscreen-erase-right/n! screen 1)
      (vscreen-erase-right/until-nl! screen))))


;; move characters from end of vscreen line at y to the beginning of line at y+1
;; and repeat on subsequent lines *without* a newline at y+2...
;; until lengths of modified lines are all <= vscreen width.
;; stops when a modified line has length <= vscreen width.
;; return y of last modified line.
(define (vscreen-overflow-at-y screen y)
  ; (debugf "~%; before vscreen-overflow-at-y ~s ~s~%" y screen)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when line1
      (while (and (fx<? y (vscreen-length screen))
                  (fx>? (charline-length line1) (vscreen-width-at-y screen y)))
        ; (debugf "while0 vscreen-overflow-at-y ~s ~s ~s~%" y line1 screen)
        (vscreen-dirty-set! screen #t)
        (let* ((line1-nl?  (charline-nl? line1))
               (line1-len  (charline-length line1))
               (line1-pos  (vscreen-width-at-y screen y))
               (n          (fx- line1-len line1-pos))
               (y+1        (fx1+ y))
               (line2-new? (or line1-nl? (fx=? y+1 (vscreen-length screen))))
               (line2      (if line2-new?
                             (charline)
                             (charlines-ref screen y+1)))
               (line2-nl? (charline-nl? line2)))
          (when line2-new?
            (charlines-insert-at/cline! screen y+1 line2))
          (unless (fxzero? n)
            ;; insert chars into line2
            (charline-insert-at/cbuf! line2 0 line1 line1-pos n)
            ;; remove chars from line1
            (charline-erase-at! line1 line1-pos n))
          ; (debugf "while1 vscreen-overflow-at-y ~s ~s ~s~%" y line1 screen)
          ;; iterate on line2, as it may now have length >= vscreen-width-at-y
          (set! y y+1)
          (set! line1 line2)))))
  (vscreen-append-empty-line-if-needed screen)
  ; (debugf "after  vscreen-overflow-at-y at ~s: ~s~%" y screen)
  y)


;; for each vscreen line at y or later that exceeds vscreen width,
;; move characters from end of it to the beginning of following line(s),
;; until lengths of each line is <= vscreen width.
(define vscreen-overflow
  (let ((%vscreen-overflow
          (lambda (screen y)
            (do ((i y (fx1+ (vscreen-overflow-at-y screen i))))
                ((fx>=? i (vscreen-length screen)))))))
    (case-lambda
      ((screen)   (%vscreen-overflow screen 0))
      ((screen y) (%vscreen-overflow screen y)))))


;; add a final empty line if needed.
(define (vscreen-append-empty-line-if-needed screen)
  (let* ((yn (vscreen-length screen))
         (y  (fx1- yn))
         (line (vscreen-line-at-y screen y)))
    (when (or (not line)
              (charline-nl? line)
              (fx=? (charline-length line) (vscreen-width-at-y screen y)))
      (charlines-insert-at/cline! screen yn (charline)))))


;; overflow and underflow all lines. add a final empty line if needed.
(define (vscreen-reflow screen)
  (vscreen-overflow screen)
  (vscreen-underflow screen))


;; prepare vscreen for insertion at specified x and y
;; return three values: x and y clamped to valid range, and charline at clamped y.
;; may insert additional lines if needed.
(define (vscreen-insert-at-xy/prepare! screen x y)
  (when (charlines-empty? screen) ;; should not happen
    (charlines-insert-at/cline! screen 0 (charline)))
  (let* ((ylen (vscreen-length screen))
         (y    (fxmax 0 (fxmin y (fx1- ylen))))
         (line (charlines-ref screen y))
         (xlen (charline-length line))
         (x    (fxmax 0 (fxmin x xlen))))
    (if (or (and (fx=? x xlen) (charline-nl? line))
            (fx=? x (vscreen-width-at-y screen y)))
      ;; cannot insert after a newline or at screen width,
      ;; return beginning of following line instead
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
  (assert* 'vscreen-insert-at-xy/ch! (char>=? ch #\space))
  (let-values (((x y line) (vscreen-insert-at-xy/prepare! screen x y)))
    (vscreen-dirty-set! screen #t)
    (charline-insert-at! line x ch)
    (vscreen-overflow-at-y screen y)))


;; insert a newline, which may split the current line in two,
;; into vscreen at specified x and y.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/newline! screen x y)
  (let-values (((x y line1) (vscreen-insert-at-xy/prepare! screen x y)))
    (let* ((x+1   (fx1+ x))
           (y+1   (fx1+ y))
           (n     (fx- (charline-length line1) x)) ;; # of chars to move to line2
           (create-line2? (or (charline-nl? line1)
                              (fx=? y+1 (vscreen-length screen))))
           (line2 (if create-line2? (charline) (charlines-ref screen y+1))))
      (vscreen-dirty-set! screen #t)
      (charline-insert-at! line1 x #\newline)
      (when create-line2?
        (charlines-insert-at/cline! screen y+1 line2))
      (when (fx>? n 0)
        ;; insert into line2 the chars from line1 after inserted newline
        (charline-insert-at/cbuf! line2 0 line1 x+1 n)
        ;; remove from line1 the chars after inserted newline
        (charline-erase-at! line1 x+1 n))
      (vscreen-reflow screen))))


;; insert part of a charspan into vscreen at specified x and y.
;; if the line at y becomes longer than vscreen-width-at-y,
;; move extra characters into the following line(s)
;; i.e. reflow them according to vscreen width.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/cspan! screen x y csp csp-start csp-n)
  (when (fx>? csp-n 0)
    (let-values (((x y line) (vscreen-insert-at-xy/prepare! screen x y)))
      (vscreen-dirty-set! screen #t)
      (charline-insert-at/cspan! line x csp csp-start csp-n)
      (vscreen-overflow-at-y screen y))))


;; insert a char, which can be a #\newline, into vscreen at cursor position
;; then move cursor right by one.
(define (vscreen-insert/ch! screen ch)
  (let ((x (vscreen-cursor-ix screen))
        (y (vscreen-cursor-iy screen)))
    (if (char=? ch #\newline)
      (vscreen-insert-at-xy/newline! screen x y)
      (vscreen-insert-at-xy/ch! screen x y ch))
    (vscreen-cursor-move/right! screen 1)))


;; insert part of a charspan into vscreen at cursor position,
;; then move cursor right by csp-n.
(define (vscreen-insert/cspan! screen csp csp-start csp-n)
  (when (fx>? csp-n 0)
    (vscreen-insert-at-xy/cspan! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen)
                                 csp csp-start csp-n)
    (vscreen-cursor-move/right! screen csp-n)))


;; return position one character to the left of x y.
;; returned position may be on the previous line.
;; return #f #f if x y is out of range or is the first valid position.
(define (vscreen-prev-xy screen x y)
  (let ((ymax  (fx1- (vscreen-length screen))))
    (if (fx<=? 0 y ymax)
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (charline-length (charlines-ref screen y))
                       (if (fx=? y ymax) 0 1))))
        ;; (debugf "vscreen-prev-xy xy = (~s ~s), xmax = ~s~%" x y xmax)
        (if (fx<=? 1 x xmax)
          (values (fx1- x) y) ;; (x-1 y) is a valid position, return it
          (if (fx>? y 0)
            ;; return last position in previous line
            (values (fx1- (charline-length (charlines-ref screen (fx1- y)))) (fx1- y))
            (values #f #f))))
      (values #f #f))))


;; return position one character to the right of x y.
;; returned position may be on the next line.
;; return #f #f if x y is out of range or is the last valid position.
(define (vscreen-next-xy screen x y)
  (let ((ymax  (fx1- (vscreen-length screen))))
    (if (fx<=? 0 y ymax)
      ;; allow positioning cursor after end of line only if it's the last line
      (let ((xmax (fx- (charline-length (charlines-ref screen y))
                       (if (fx=? y ymax) 0 1))))
        ;; (debugf "vscreen-next-xy xy = (~s ~s), xmax = ~s~%" x y xmax)
        (if (fx<? -1 x xmax)
          (values (fx1+ x ) y) ;; (x+1 y) is a valid position, return it
          (if (fx<? y ymax)
            (values 0 (fx1+ y)) ;; return beginning of next line
            (values #f #f))))
      (values #f #f))))


;; return position one character to the left of x y, and n+1.
;; returned position may be on the previous line.
;; if position x y is are out of range or is the first valid position, return unmodified x y n.
(define (vscreen-prev-xy/or-self screen x y n)
  (let-values (((x1 y1) (vscreen-prev-xy screen x y)))
    (if (and x1 y1)
      (values x1 y1 (fx1+ n))
      (values x y n))))

;; return position one character to the right of x y and n+1.
;; returned position may be on the next line.
;; if position x y is are out of range or is the last valid position, return unmodified x y n.
(define (vscreen-next-xy/or-self screen x y n)
  (let-values (((x1 y1) (vscreen-next-xy screen x y)))
    (if (and x1 y1)
      (values x1 y1 (fx1+ n))
      (values x y n))))


;; return position immediately before x y, and char at such position.
;; return #f #f #f if x y are out of range or 0 0.
(define (vscreen-char-before-xy screen x y)
  (let-values (((x y) (vscreen-prev-xy screen x y)))
    (if (and x y)
      (values x y (vscreen-char-at-xy screen x y))
      (values #f #f #f))))


;; return position immediately after x y, and char at such position.
;; return #f #f #f if x y are out of range.
;; return x+1 y #f if x y correspond to the last character in the last line
(define (vscreen-char-after-xy screen x y)
  (let-values (((x y) (vscreen-next-xy screen x y)))
    (if (and x y)
      (values x y (vscreen-char-at-xy screen x y))
      (values #f #f #f))))


;; return count of consecutive characters before x y that satisfy (pred ch), and their position.
;; return x y 0 if either x y are out of range, or character before x y does not satisfy (pred ch)
(define (vscreen-count-at-xy/left screen x y pred)
  (let ((n 0)
        (continue? #t))
    (while continue?
      (let-values (((x1 y1 ch) (vscreen-char-before-xy screen x y)))
        (set! continue? (and x1 y1 ch (pred ch)))
        (when continue?
          (set! x x1)
          (set! y y1)
          (set! n (fx1+ n)))))
    (values x y n)))


;; return count of consecutive characters at x y or afterwards that satisfy (pred ch), and their position.
;; return x y 0 if either x y are out of range, or character at x y does not satisfy (pred ch)
(define (vscreen-count-at-xy/right screen x y pred)
  (let ((n 0)
        (continue? (let ((ch (vscreen-char-at-xy screen x y)))
                     (and ch (pred ch)))))
    (while continue?
      (let-values (((x1 y1 ch) (vscreen-char-after-xy screen x y)))
        ;; (debugf "vscreen-count-at-xy/right xy = (~s ~s), ch = ~s~%" x1 y1 ch)
        (set! continue? (and x1 y1 ch (pred ch)))
        (when continue?
          (set! x x1)
          (set! y y1)
          (set! n (fx1+ n)))))
    (values x y n)))



;; completely replace vscreen contents, setting it to specified charlines.
;; note: charlines will be retained and modified!
;; Sets vscreen cursor to 0 0.
(define (vscreen-assign*! screen lines)
  (vscreen-dirty-set! screen #t)
  (charlines-dirty-y-add! screen 0 (vscreen-length screen))
  ;; Implementation note: linectx-to-history* saves a shallow copy of vscreen to history,
  ;; which references the %gbuffer-left and %gbuffer-right internal spans of vscreen,
  ;; so we cannot continue using them: create new ones
  (%gbuffer-left-set! screen (%gbuffer-left lines))
  (%gbuffer-right-set! screen (%gbuffer-right lines))
  (vscreen-reflow screen)
  (vscreen-cursor-ix-set! screen 0)
  (vscreen-cursor-iy-set! screen 0)
  (charlines-dirty-y-add! screen 0 (vscreen-length screen)))


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
(record-writer (record-type-descriptor %vscreen)
  (lambda (screen port writer)
    (write-vscreen screen port)))

) ; close library
