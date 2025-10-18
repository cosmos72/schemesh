;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh screen vscreen (0 9 2))
  (export
    make-vscreen  (rename (%vscreen vscreen))  vscreen?  assert-vscreen?
    vscreen-width        vscreen-height     vscreen-width-at-y  vscreen-resize!
    vscreen-dirty?       vscreen-dirty-set!
    vscreen-cursor-ix    vscreen-cursor-iy  vscreen-cursor-ixy  vscreen-cursor-ixy-set!
    vscreen-cursor-vx    vscreen-cursor-vy  vscreen-cursor-vxy  vscreen-cursor-vxy-set!
    vscreen-prompt-end-x vscreen-prompt-end-y vscreen-prompt-length  vscreen-prompt-length-set!
    vscreen-length-at-y  vscreen-length     vscreen-cell-at-xy
    vscreen-char-at-xy   vscreen-char-before-xy  vscreen-char-after-xy
    vscreen-next-xy      vscreen-prev-xy   vscreen-next-xy/or-self  vscreen-prev-xy/or-self
    vscreen-count-before-xy/left  vscreen-count-at-xy/right
    vscreen-clear!       vscreen-empty?
    vscreen-cursor-move/left! vscreen-cursor-move/right!  vscreen-cursor-move/up!  vscreen-cursor-move/down!
    vscreen-delete-left/n!     vscreen-delete-right/n!      vscreen-delete-at-xy!
    vscreen-delete-left/vline!  vscreen-delete-right/vline!
    vscreen-insert/c!            vscreen-insert-at-xy/c!
    vscreen-insert-at-xy/newline! vscreen-insert-at-xy/vcellspan!
    vscreen-assign*!  vscreen-reflow  vscreen-write)

  (import
    (rnrs)
    (only (chezscheme)         format fx1+ fx1- record-writer)
    (only (schemesh bootstrap) assert* fx<=?* while)
    (only (schemesh containers charspan) charspan-insert-left! charspan-insert-right!)
    (schemesh containers span)
    (only (schemesh screen vcell) vcell->char)
    (schemesh screen vcellspan)
    (schemesh screen vline)
    (schemesh screen vlines))


;; copy-pasted from containers/gbuffer.ss
(define-record-type (%gbuffer %make-gbuffer %gbuffer?)
  (fields
     (mutable left  gbuffer-left  gbuffer-left-set!)
     (mutable right gbuffer-right gbuffer-right-set!))
  (nongenerative %gbuffer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; copy-pasted from containers/vlines.ss
(define-record-type (%vlines %make-vlines %vlines?)
  (parent %gbuffer)
  (fields
    ;; lines between y >= dirty-start-y and y < dirty-end-y
    ;; are completely dirty i.e. must be fully redrawn on screen
    (mutable dirty-start-y) ;; fixnum
    (mutable dirty-end-y))  ;; fixnum
  (nongenerative %vlines-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; vscreen is an in-memory representation of user-typed input and how it is split
;; in multiple lines, each limited either by a newline or by screen width.
;;
;; vscreen must always contain at least one (possibly empty) vline.
(define-record-type (vscreen %make-vscreen vscreen?)
  (parent %vlines)
  (fields
    (mutable dirty? vscreen-dirty? %vscreen-dirty-set!) ;; boolean, #t if some line is dirty
    (mutable width)        ;; fixnum, screen width
    (mutable height)       ;; fixnum, screen height
    (mutable prompt-end-x) ;; fixnum, x column where prompt ends
    (mutable prompt-end-y) ;; fixnum, y row where prompt ends
    (mutable cursor-ix)    ;; fixnum, cursor x position
    (mutable cursor-iy))   ;; fixnum, cursor y position
  (nongenerative vscreen-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (assert-vscreen? who screen)
  (unless (vscreen? screen)
    (assertion-violation who "not a vscreen" screen)))


;; create a vscreen
(define (make-vscreen)
  (%make-vscreen (span) (span (vline)) (greatest-fixnum) 0 #f 80 24 0 0 0 0))


;; create a vscreen referencing specified vlines or strings
;; note: created screen should be reflowed, i.e. caller should invoke (vscreen-reflow screen)
(define (%vscreen width height . l)
  (let ((sp (list->span l)))
    (span-iterate sp
      (lambda (i line)
        (if (string? line)
         (span-set! sp i (vline line))
         (assert-vline? 'vlines line))))
    (%make-vscreen (span) sp (greatest-fixnum) 0 #f width height 0 0 0 0)))


;; return number of vlines in vscreen
(define vscreen-length vlines-length)

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
    (vlines-dirty-xy-unset! screen))
  (%vscreen-dirty-set! screen flag?))

;; return vscreen width i.e. maximum vline length at specified y
;; it is equal (vscreen-width), except when y = 0 where prompt length is subtracted
(define (vscreen-width-at-y screen y)
  (fx- (vscreen-width screen)
       (if (fxzero? y) (vscreen-prompt-end-x screen) 0)))


;; return vscreen line at specified y, or #f if y is out of range
(define (vscreen-line-at-y screen y)
  (if (fx<? -1 y (vscreen-length screen))
    (vlines-ref screen y)
    #f))

;; return vline length at specified y, or 0 if y is out of range.
(define (vscreen-length-at-y screen y)
  (if (fx<? -1 y (vscreen-length screen))
    (vline-length (vlines-ref screen y))
    0))


;; return vscreen cell at specified x y, or #f if x y are out of range
(define (vscreen-cell-at-xy screen x y)
  (vlines-cell-at-xy screen x y))


;; return vscreen char at specified x y, or #f if x y are out of range
(define (vscreen-char-at-xy screen x y)
  (let ((cl (vlines-cell-at-xy screen x y)))
    (and cl (vcell->char cl))))

;; return tow values: cursor x and y position in input vlines
(define (vscreen-cursor-ixy screen)
  (values (vscreen-cursor-ix screen) (vscreen-cursor-iy screen)))


;; set cursor x and y position in input vlines. notes:
;; * x and y values will be clamped to existing vlines and their lengths
;; * x can also be set to immediately after the end of *last* vline
(define (vscreen-cursor-ixy-set! screen x y)
  (let* ((ymax  (fx1- (vscreen-length screen)))
         (y     (fxmax 0 (fxmin y ymax)))
         ;; allow positioning cursor after end of line only for *last* vline
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


;; return total number of cells in vscreen before position x y,
;; including characters in previous lines if y > 0.
(define (vscreen-count-to-start screen x y)
  (let ((n     x)
        (end-y (vscreen-length screen)))
    (while (fx<? 0 y end-y)
      (set! y (fx1- y))
      (set! n (fx+ n (vline-length (vlines-ref screen y)))))
    n))


;; return total number of cells in vscreen at position x y or after it,
;; including cells in next lines if y < maximum y.
(define (vscreen-count-to-end screen x y)
  (let ((n     0)
        (end-y (vscreen-length screen)))
    (while (fx<? 0 y end-y)
      (let* ((len (vline-length (vlines-ref screen y)))
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
  (vlines-dirty-y-add! screen 0 (vscreen-length screen))
  ;; Implementation note: linectx-to-history* saves a shallow copy of vscreen to history,
  ;; which references the gbuffer-left and gbuffer-right internal spans of vscreen,
  ;; so we cannot continue using them: create new ones
  (gbuffer-left-set! screen (span))
  (gbuffer-right-set! screen (span (vline)))
  (vscreen-cursor-ix-set! screen 0)
  (vscreen-cursor-iy-set! screen 0))


;; return #t if vscreen is empty i.e. it contains at most a single vline having zero length
(define (vscreen-empty? screen)
  (case (vscreen-length screen)
    ((0) #t)
    ((1) (fxzero? (vline-length (vlines-ref screen 0))))
    (else #f)))


;; remove a line from screen. Last line cannot be removed, it will be cleared instead.
(define (vscreen-delete-at/vline! screen y)
  (vscreen-dirty-set! screen #t)
  (if (and (fxzero? y) (fx=? 1 (vscreen-length screen)))
    (vline-clear! (vlines-ref screen 0))
    (vlines-delete-at! screen y)))



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
    (when (and (fx>=? ymax 0) (fx>=? xmax 0))
      (while (and (fx>? n 0) (fx<=? y ymax) (or (fx<? y ymax) (fx<? x xmax)))
        ;; x position immediately after last char is allowed
        (let* ((linemax (fx- (vscreen-length-at-y screen y) (if (fx=? y ymax) 0 1)))
               (delta   (fxmax 0 (fxmin n (fx- linemax x)))))
          ; (debugf "~%; vscreen-cursor-move/right! n ~s, x ~s, xmax ~s, y ~s, ymax ~s, delta ~s" n x xmax y ymax delta)
          (set! n (fx- n delta))
          (set! x (fx+ x delta))
          (when (and (fx>? n 0) (fx>=? x linemax) (fx<? y ymax))
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
    (when (and line1 (not (vline-nl? line1)))
      (let ((n (fx- (vscreen-width-at-y screen y) (vline-length line1)))
            (y+1 (fx1+ y)))
        (while (and (fx>? n 0) (fx<? y+1 (vscreen-length screen)))
          (let* ((line2 (vlines-ref screen y+1))
                 (line2-nl? (vline-nl? line2))
                 (i     (fxmin n (vline-length line2))))
            (when (fx>? i 0)
              (vscreen-dirty-set! screen #t)
              ;; insert chars into line1
              (vline-insert-at/vbuffer! line1 (vline-length line1) line2 0 i)
              ;; remove chars from line2
              (vline-delete! line2 0 i)
              (set! n (fx- n i)))
            (when (vline-empty? line2)
              (when line2-nl?
                (set! n 0)) ;; newline found, stop refilling line1
              ;; we consumed all chars from line2, erase it unless line1 is full and line2 is last line
              (unless (and (or (vline-nl? line1)
                               (fx>=? (vline-length line1) (vscreen-width-at-y screen y)))
                           (fx>=? y+1 (fx1- (vscreen-length screen))))
                (vscreen-delete-at/vline! screen y+1)))))))))


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
;; if the newline of a vline is erased, the following line(s)
;; are merged into the current vline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define (vscreen-delete-at-xy! screen x y n)
  (let ((saved-n n))
    (when (fx>? n 0)
      (vscreen-dirty-set! screen #t)
      (let ((saved-y y))
        (while (and (fx>? n 0) (fx<? -1 y (vscreen-length screen)))
          ; (debugf "vscreen-delete-at-xy! ~s chars to delete at y = ~s" n y)
          (let* ((line (vlines-ref screen y))
                 (len  (vline-length line))
                 (i    (fxmin n (fx- len x))))
            (when (fx>? i 0)
              ;; erase i characters
              (vline-delete! line x (fx+ x i))
              (set! n (fx- n i))
              (set! len (fx- len i)))
            (when (fx>=? x len)
              ;; erased until the end of line, continue with next line
              (set! x 0)
              (if (and (fxzero? len) (not (vscreen-empty? screen)))
                ;; line is empty, remove it
                (vscreen-delete-at/vline! screen y)
                ;; line is not empty, move to next line
                (set! y (fx1+ y))))))
        (unless (fx=? saved-n n)
          (vscreen-underflow screen saved-y))))
    (fx- saved-n n)))


;; erase n characters from vscreen, starting 1 char left of cursor and moving leftward.
;; if the newline of a vline is erased, the following line(s)
;; are merged into the current vline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define vscreen-delete-left/n!
  (case-lambda
    ((screen n)
      (vscreen-delete-left/n! screen n #f))
    ((screen n clipboard)
      (clipboard-insert-vscreen/left! clipboard screen n)
      (let ((n (vscreen-cursor-move/left! screen n)))
        (vscreen-delete-at-xy! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) n)))))


;; erase n characters from vscreen, starting at cursor and moving rightward.
;; if the newline of a vline is erased, the following line(s)
;; are merged into the current vline and reflowed according to vscreen width.
;; return number of characters actually erased.
(define vscreen-delete-right/n!
  (case-lambda
    ((screen n clipboard)
      (clipboard-insert-vscreen/right! clipboard screen n)
      (vscreen-delete-at-xy! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) n))
    ((screen n)
      (vscreen-delete-right/n! screen n #f))))



(define (clipboard-insert-vscreen/left! clipboard screen n)
  ; (debugf "->   linectx-clipboard-insert/left! n=~s" char-count-leftward-before-cursor)
  (when (and clipboard (fx>? n 0))
    (let-values (((x y) (vscreen-cursor-ixy screen)))
      (let %loop ((x x) (y y) (n n))
        (when (and x y (fx>? n 0))
          (let-values (((x1 y1 ch) (vscreen-char-before-xy screen x y)))
            (when ch
              (vcellspan-insert-left! clipboard ch))
            (%loop x1 y1 (fx1- n))))))))


(define (clipboard-insert-vscreen/right! clipboard screen n)
  (when (and clipboard (fx>? n 0))
    (let-values (((x y) (vscreen-cursor-ixy screen)))
      (let %loop ((x x) (y y) (n n) (ch (vscreen-char-at-xy screen x y)))
        (when (and x y ch (fx>? n 0))
          (vcellspan-insert-right! clipboard ch)
          (let-values (((x1 y1 ch1) (vscreen-char-after-xy screen x y)))
            (%loop x1 y1 (fx1- n) ch1)))))))


(define (clipboard-insert-vline/left! clipboard line start end)
  (when clipboard
    (let %loop ((end end))
      (when (fx<? start end)
        (let ((pos (fx1- end)))
          (vcellspan-insert-left! clipboard (vline-ref/char line pos))
          (%loop pos))))))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-delete-left/until-nl! screen clipboard)
  (let* ((x    (vscreen-cursor-ix screen))
         (y    (vscreen-cursor-iy screen))
         (line (vscreen-line-at-y screen y)))
    (when line
      (clipboard-insert-vline/left! clipboard line 0 x)
      (vscreen-dirty-set! screen #t)
      (vline-delete! line 0 x)
      (vscreen-cursor-ix-set! screen 0)
      (let* ((y    (fx1- y))
             (line (vscreen-line-at-y screen y)))
        (while (and line (not (vline-nl? line)))
          (clipboard-insert-vline/left! clipboard line 0 (vline-length line))
          (vscreen-delete-at/vline! screen y)
          (set! y    (fx1- y))
          (set! line (vscreen-line-at-y screen y)))
        (vscreen-cursor-iy-set! screen (fx1+ y))))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is not erased.
(define (vscreen-delete-right/until-nl! screen clipboard)
  (let* ((x    (vscreen-cursor-ix screen))
         (y    (vscreen-cursor-iy screen))
         (line (vscreen-line-at-y screen y))
         (n    0))
    (while line
      (cond
        ((vline-nl? line)
          (set! n (fx+ n (fxmax 0 (fx1- (fx- (vline-length line) x)))))
          (set! line #f))
        (else
          (set! n (fx+ n (fxmax 0 (fx- (vline-length line) x))))
          (set! x 0)
          (set! y (fx1+ y))
          (set! line (vscreen-line-at-y screen y)))))
    (vscreen-delete-right/n! screen n clipboard)))


;; erase leftward, starting 1 char left of vscreen cursor and continuing
;; (possibly to previous lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define vscreen-delete-left/vline!
  (case-lambda
    ((screen)
      (vscreen-delete-left/vline! screen #f))
    ((screen clipboard)
      (let ((x    (vscreen-cursor-ix screen))
            (line (vscreen-line-at-y screen (fx1- (vscreen-cursor-iy screen)))))
        (if (and (fxzero? x) line (vline-nl? line))
          (vscreen-delete-left/n! screen 1 clipboard)
          (vscreen-delete-left/until-nl! screen clipboard))))))


;; erase rightward, starting at vscreen cursor and continuing
;; (possibly to next lines) until a newline is found.
;; The newline is only erased if it's the first character found.
(define vscreen-delete-right/vline!
  (case-lambda
    ((screen clipboard)
      (let ((x    (vscreen-cursor-ix screen))
            (line (vscreen-line-at-y screen (vscreen-cursor-iy screen))))
        (if (and line (eqv? #\newline (vline-at/char line x)))
          (vscreen-delete-right/n! screen 1 clipboard)
          (vscreen-delete-right/until-nl! screen clipboard))))
    ((screen)
      (vscreen-delete-right/vline! screen #f))))


;; move characters from end of vscreen line at y to the beginning of line at y+1
;; and repeat on subsequent lines *without* a newline at y+2...
;; until lengths of modified lines are all <= vscreen width.
;; stops when a modified line has length <= vscreen width.
;; return y of last modified line.
(define (vscreen-overflow-at-y screen y)
  ; (debugf "~%; before vscreen-overflow-at-y ~s ~s" y screen)
  (let ((line1 (vscreen-line-at-y screen y)))
    (when line1
      (while (and (fx<? y (vscreen-length screen))
                  (fx>? (vline-length line1) (vscreen-width-at-y screen y)))
        ; (debugf "while0 vscreen-overflow-at-y ~s ~s ~s" y line1 screen)
        (vscreen-dirty-set! screen #t)
        (let* ((line1-nl?  (vline-nl? line1))
               (line1-end  (vline-length line1))
               (line1-pos  (vscreen-width-at-y screen y))
               (y+1        (fx1+ y))
               (line2-new? (or line1-nl? (fx=? y+1 (vscreen-length screen))))
               (line2      (if line2-new?
                             (vline)
                             (vlines-ref screen y+1)))
               (line2-nl? (vline-nl? line2)))
          (when line2-new?
            (vlines-insert-at! screen y+1 line2))
          (when (fx<? line1-pos line1-end)
            ;; insert chars into line2
            (vline-insert-at/vbuffer! line2 0 line1 line1-pos line1-end)
            ;; remove chars from line1
            (vline-delete! line1 line1-pos line1-end))
          ; (debugf "while1 vscreen-overflow-at-y ~s ~s ~s" y line1 screen)
          ;; iterate on line2, as it may now have length >= vscreen-width-at-y
          (set! y y+1)
          (set! line1 line2)))))
  (vscreen-append-empty-line-if-needed screen)
  ; (debugf "after  vscreen-overflow-at-y at ~s: ~s" y screen)
  y)

;; add a final empty line if needed.
(define (vscreen-append-empty-line-if-needed screen)
  (let* ((yn (vscreen-length screen))
         (y  (fx1- yn))
         (line (vscreen-line-at-y screen y)))
    (when (or (not line)
              (vline-nl? line)
              (fx=? (vline-length line) (vscreen-width-at-y screen y)))
      (vlines-insert-at! screen yn (vline)))))


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


;; if vscreen line at y contains a #\newline followed by some other characters,
;; split it into two lines, ensuring that the first #\newline is at the end of its line,
;; thus not followed anymore by other characters.
(define (vcreen-split-line-after-first-newline screen y)
  (let* ((line (vscreen-line-at-y screen y))
         (end  (and line (vline-length line)))
         (pos  (and line (vline-index/char line 0 end #\newline))))
    (when (and pos (fx<? (fx1+ pos) end))
      (let ((line2 (vline))
            (pos+1 (fx1+ pos)))
        (vline-insert-at/vbuffer! line2 0 line pos+1 end)
        (vline-delete!    line pos+1 end)
        (vlines-insert-at! screen (fx1+ y) line2)))))


;; for each vscreen line that contains a #\newline followed by some other characters,
;; split it into multiple lines, ensuring that each #\newline is always at the end of its line,
;; thus not followed anymore by other characters.
(define (vscreen-split-after-newlines screen)
  (do ((y 0 (fx1+ y)))
      ((fx>? y (vscreen-length screen)))
    (vcreen-split-line-after-first-newline screen y)))


;; overflow and underflow all lines. add a final empty line if needed.
(define (vscreen-reflow screen)
  (vscreen-split-after-newlines screen)
  (vscreen-overflow screen)
  (vscreen-underflow screen))


;; prepare vscreen for insertion at specified x and y
;; return three values: x and y clamped to valid range, and vline at clamped y.
;; may insert additional lines if needed.
(define (vscreen-insert-at-xy/prepare! screen x y)
  (when (vlines-empty? screen) ;; should not happen
    (vlines-insert-at! screen 0 (vline)))
  (let* ((ylen (vscreen-length screen))
         (y    (fxmax 0 (fxmin y (fx1- ylen))))
         (line (vlines-ref screen y))
         (xlen (vline-length line))
         (x    (fxmax 0 (fxmin x xlen))))
    (if (or (and (fx=? x xlen) (vline-nl? line))
            (fx=? x (vscreen-width-at-y screen y)))
      ;; cannot insert after a newline or at screen width,
      ;; return beginning of following line instead
      (let ((y+1 (fx1+ y)))
        (when (fx=? y+1 ylen)
          (vlines-insert-at! screen y+1 (vline)))
        (values 0 y+1 (vlines-ref screen y+1)))
      ;; return position inside line, or at end of it
      (values x y line))))


;; insert a single character or cell into vscreen at specified x and y.
;; Character must be printable, i.e. (char>=? ch #\space).
;; If the line at y becomes longer than vscreen-width-at-y,
;; move extra characters into the following line(s)
;; i.e. reflow them according to vscreen width.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/c! screen x y c)
  (let ((ch (if (char? c) c (vcell->char c))))
    (assert* 'vscreen-insert-at-xy/c! (char>=? ch #\space)))
  (let-values (((x y line) (vscreen-insert-at-xy/prepare! screen x y)))
    (vscreen-dirty-set! screen #t)
    (vline-insert-at! line x c)
    (vscreen-overflow-at-y screen y)))


;; insert a newline, which may split the current line in two,
;; into vscreen at specified x and y.
;; Both x and y are clamped to valid range.
(define (vscreen-insert-at-xy/newline! screen x y)
  (let-values (((x y line1) (vscreen-insert-at-xy/prepare! screen x y)))
    (let* ((x+1   (fx1+ x))
           (y+1   (fx1+ y))
           ;; line1 length after we insert the newline
           (line1-end+1   (fx1+ (vline-length line1)))
           (create-line2? (or (vline-nl? line1)
                              (fx=? y+1 (vscreen-length screen))))
           (line2 (if create-line2? (vline) (vlines-ref screen y+1))))
      (vscreen-dirty-set! screen #t)
      (vline-insert-at! line1 x #\newline)
      (when create-line2?
        (vlines-insert-at! screen y+1 line2))
      (when (fx<? x+1 line1-end+1)
        ;; (debugf "vscreen-insert-at-xy/newline! before moving range [~s,~s): line1=~s, line2=~s" x+1 line1-end+1 line1 line2)
        ;; insert into line2 the chars from line1 after inserted newline
        (vline-insert-at/vbuffer! line2 0 line1 x+1 line1-end+1)
        ;; remove from line1 the chars after inserted newline
        (vline-delete! line1 x+1 line1-end+1))
        ;; (debugf "vscreen-insert-at-xy/newline! after  moving range [~s,~s): line1=~s, line2=~s" x+1 line1-end+1 line1 line2)
      (vscreen-reflow screen))))



;; insert a character or cell, which can be a #\newline, into vscreen at cursor position
;; then move cursor right by one.
(define (vscreen-insert/c! screen c)
  (let ((x (vscreen-cursor-ix screen))
        (y (vscreen-cursor-iy screen)))
    (if (char=? #\newline (if (char? c) c (vcell->char c)))
      (vscreen-insert-at-xy/newline! screen x y)
      (vscreen-insert-at-xy/c! screen x y c))
    (vscreen-cursor-move/right! screen 1)))


;; insert cells in range [start, end) of vcellspan csp
;; into vscreen at specified x and y.
;; Both x and y are clamped to valid range.
;;
;; If the line at y becomes longer than vscreen-width-at-y,
;; moves extra characters into the following line(s)
;; i.e. reflows them according to vscreen width.
;;
;; If one or more #\newline are inserted, performs a full (vscreen-reflow)
(define vscreen-insert-at-xy/vcellspan!
  (case-lambda
    ((screen x y csp csp-start csp-end)
      (assert* 'vscreen-insert-at-xy/vcellspan! (fx<=?* 0 csp-start csp-end (vcellspan-length csp)))
      (when (fx<? csp-start csp-end)
        (let-values (((x y line) (vscreen-insert-at-xy/prepare! screen x y)))
          (vscreen-dirty-set! screen #t)
          (vline-insert-at/vcellspan! line x csp csp-start csp-end)
          (if (vcellspan-index/char csp csp-start csp-end #\newline)
            (vscreen-reflow screen)
            (vscreen-overflow-at-y screen y)))))
    ((screen x y csp)
      (vscreen-insert-at-xy/vcellspan! screen x y csp 0 (vcellspan-length csp)))))


;; return position one character to the left of x y.
;; returned position may be on the previous line.
;; return #f #f if x y is out of range or is the first valid position.
(define (vscreen-prev-xy screen x y)
  (vlines-prev-xy x y))


;; return position one character to the right of x y.
;; returned position may be on the next line.
;; return #f #f if x y is out of range or is the last valid position.
(define (vscreen-next-xy screen x y)
  (vlines-next-xy x y))


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
(define vscreen-char-before-xy vlines-char-before-xy)


;; return position immediately after x y, and char at such position.
;; return #f #f #f if x y are out of range.
;; return x+1 y #f if x y correspond to the last character in the last line
(define vscreen-char-after-xy vlines-char-after-xy)


;; return count of consecutive characters starting immediately before x y and moving backward
;; that satisfy (pred ch), and the x y position of the first such character.
;; return x y 0 if either x y are out of range, or character immediately before x y does not satisfy (pred ch)
(define (vscreen-count-before-xy/left screen x y pred)
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


;; return count of consecutive characters starting at x y and moving forward
;; that satisfy (pred ch), and x y position immediately after the last such character.
;; return x y 0 if either x y are out of range, or character at x y does not satisfy (pred ch)
(define (vscreen-count-at-xy/right screen x y pred)
  (let ((n 0)
        (continue? (let ((ch (vscreen-char-at-xy screen x y)))
                     (and ch (pred ch)))))
    (while continue?
      (let-values (((x1 y1 ch) (vscreen-char-after-xy screen x y)))
        ; (debugf "vscreen-count-at-xy/right xy = (~s ~s), ch = ~s" x1 y1 ch)
        (when (and x1 y1)
          (set! continue? (and ch (pred ch)))
          (set! x x1)
          (set! y y1)
          (set! n (fx1+ n)))))
    (values x y n)))


;; completely replace vscreen contents, setting it to specified vlines.
;; note: vlines will be retained and modified!
;; Sets vscreen cursor to 0 0.
(define (vscreen-assign*! screen lines)
  (vscreen-dirty-set! screen #t)
  (vlines-dirty-y-add! screen 0 (vscreen-length screen))
  ;; Implementation note: linectx-to-history* saves a shallow copy of vscreen to history,
  ;; which references the gbuffer-left and gbuffer-right internal spans of vscreen,
  ;; so we cannot continue using them: create new ones
  (gbuffer-left-set! screen (gbuffer-left lines))
  (gbuffer-right-set! screen (gbuffer-right lines))
  (vscreen-reflow screen)
  (vscreen-cursor-ix-set! screen 0)
  (vscreen-cursor-iy-set! screen 0)
  (vlines-dirty-y-add! screen 0 (vscreen-length screen)))


;; write a textual representation of vscreen to output port
(define (vscreen-write screen port)
  (display (vscreen-width screen) port)
  (display #\space port)
  (display (vscreen-height screen) port)
  (vlines-iterate screen
    (lambda (i line)
      (display #\space port)
      (vline-write line port))))


;; customize how "vscreen" objects are printed
(record-writer (record-type-descriptor vscreen)
  (lambda (screen port writer)
    (display "(vscreen " port)
    (vscreen-write screen port)
    (display ")" port)))

) ; close library
