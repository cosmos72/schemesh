
;; if tty size changed, resize and reflow vscreen
(define (linectx-resize lctx width height)
  (vscreen-resize! (linectx-vscreen lctx) width height)
  (linectx-redraw-set! lctx #t))

;; react to SIGWINCH
(define (linectx-consume-sigwinch lctx)
  (when (signal-consume-sigwinch)
    (let ((sz (tty-size)))
      (when (pair? sz)
        (linectx-resize lctx (car sz) (cdr sz))))))

;; unconditionally draw prompt. does not update term-x, term-y
(define (linectx-draw-prompt lctx)
  ; (debugf "linectx-draw-prompt: prompt = ~s" (linectx-prompt lctx))
  (lineterm-write/bspan lctx (linectx-prompt lctx)))

;; unconditionally draw all lines. does not update term-x, term-y
(define (linectx-draw-lines lctx)
  (let* ((screen (linectx-vscreen lctx))
         (width  (vscreen-width screen))
         (ymax   (fxmax 0 (fx1- (vscreen-length screen))))
         (nl?    #f))
    ; (debugf "linectx-draw-lines ~s" screen)
    (charlines-iterate screen
      (lambda (y line)
        (lineterm-write/cbuffer lctx line 0 (charline-length line))
        (unless (or (fx=? y ymax) (charline-nl? line))
          (lineterm-write/u8 lctx 10))))
    (vscreen-dirty-set! screen #f)
    (lineterm-clear-to-eos lctx)))

;; sett term-x, term-y cursor to end of charlines
(define (linectx-term-xy-set/end-lines! lctx)
  (let* ((screen (linectx-vscreen lctx))
         (iy (fxmax 0 (fx1- (vscreen-length screen))))
         (ix (vscreen-length-at-y screen iy))
         (vy (fx+ iy (vscreen-prompt-end-y screen)))
         (vx (fx+ ix (if (fxzero? iy) (vscreen-prompt-end-x screen) 0))))
    ; (debugf "linectx-move-from-end-lines vx = ~s, vy = ~s" vx vy)
    (linectx-term-xy-set! lctx vx vy)))


(define bv-prompt-error (string->utf8b "error expanding prompt $ "))

;; update prompt
(define (linectx-update-prompt lctx)
  (let ((prompt (linectx-prompt lctx)))
    (assert* 'linectx-update-prompt (bytespan? prompt))
    (try ((linectx-prompt-func lctx) lctx)
      (catch (ex)
        (bytespan-clear! prompt)
        (let ((err-len (bytevector-length bv-prompt-error)))
          (bytespan-insert-back/bvector! prompt bv-prompt-error 0 err-len)
          (linectx-prompt-length-set! lctx err-len))))))


;; if needed, redraw prompt, lines, cursor and matching parentheses.
(define (linectx-redraw-as-needed lctx)
  (cond
    ((linectx-redraw? lctx)
      (linectx-redraw-all lctx))
    ((vscreen-dirty? (linectx-vscreen lctx))
      (linectx-redraw-dirty lctx 'highlight))
    (else
      (linectx-redraw-cursor+paren lctx))))


;; erase everything, then set flag "redraw prompt and lines"
(define (linectx-undraw lctx)
  (lineterm-move-dy lctx (fx- (linectx-term-y lctx)))
  (lineterm-move-to-bol lctx)
  (lineterm-clear-to-eos lctx)
  (linectx-term-xy-set! lctx 0 0)
  (linectx-redraw-set! lctx #t))


;; redraw everything
(define (linectx-redraw-all lctx)
  (lineterm-move-dy lctx (fx- (linectx-term-y lctx)))
  (lineterm-move-to-bol lctx)
  (linectx-update-prompt lctx)
  (linectx-draw-prompt lctx)
  (linectx-draw-lines lctx)
  ;; set term-x and term-y to end of charlines
  (linectx-term-xy-set/end-lines! lctx)
  (parenmatcher-clear! (linectx-parenmatcher lctx))
  (linectx-paren-update! lctx)
  (linectx-draw-bad-parens lctx 'highlight)
  (linectx-draw-paren lctx (linectx-paren lctx) 'highlight)
  (linectx-redraw-set! lctx #f)
   ;; move the cursor to final position, and update term-x and term-y accordingly
  (let ((vx (linectx-vx lctx))
        (vy (linectx-vy lctx)))
    (lineterm-move-to lctx vx vy)
    (linectx-term-xy-set! lctx vx vy)))


;; redraw only dirty parts of vscreen.
;; paren-option should be one of:
;;   'plain     to de-highlight bad and matching parentheses
;;   'highlight to re-highlight bad and matching parentheses
(define (linectx-redraw-dirty lctx paren-option)
  (linectx-draw-bad-parens lctx 'plain)
  (linectx-draw-paren lctx (linectx-paren lctx) 'plain)
  (let* ((screen (linectx-vscreen lctx))
         (ymin   (charlines-dirty-start-y screen))
         (ymax   (fx1- (charlines-dirty-end-y screen)))
         (vx     (linectx-term-x lctx))
         (vy     (linectx-term-y lctx))
         (prompt-x (vscreen-prompt-end-x screen))
         (prompt-y (vscreen-prompt-end-y screen))
         (width  (vscreen-width screen)))
    ;; lines with (fx<=? ymin i ymax) are fully dirty
    (charlines-iterate screen
      (lambda (i line)
        (let* ((fully-dirty? (fx<=? ymin i ymax))
               (len          (charline-length line))
               (width-at-i   (vscreen-width-at-y screen i))
               (xdirty0      (if fully-dirty? 0 (fxmin width-at-i (charline-dirty-start-x line))))
               (xdirty1      (if fully-dirty? width-at-i (charline-dirty-end-x line))))
          (when (fx<? xdirty0 xdirty1)
            (let* ((vxoffset (if (fxzero? i) prompt-x 0))
                   (vi       (fx+ i prompt-y))
                   ;; xdraw0 and xdraw1 are xdirty0 and xdirty1 clamped to line length
                   (xdraw0   (fxmax 0 (fxmin xdirty0 len)))
                   (xdraw1   (fxmax 0 (fxmin xdirty1 len)))
                   (nl       (if (and (charline-nl? line) (fx=? xdraw1 len)) 1 0))) ;; 1 if newline, 0 otherwise
              ; (debugf "linectx-redraw-dirty i = ~s, len = ~s, width-at-i = ~s, xdirty0 = ~s -> ~s, xdirty1 = ~s -> ~s, nl = ~s"
              ;         i len width-at-i xdirty0 xdraw0 xdirty1 xdraw1 nl)
              (lineterm-move lctx vx vy (fx+ xdraw0 vxoffset) vi)
              (lineterm-write/cbuffer lctx line xdraw0 (fx- xdraw1 nl)) ;; do not print the newline yet
              ;; clear to end-of-line only when
              ;; * xdirty1 extends beyond end of charline
              ;; * and charline is shorter than screen width
              ;; note: cursor cannot be at right of rightmost vscreen char,
              ;; and printing end-of-line when cursor is *at* rightmost char erases it
              (when (and (fx>? xdirty1 len) (fx<? len width-at-i))
                (lineterm-clear-to-eol lctx))
              (if (or (fx=? nl 1) ;; newline must be printed as part of charline
                      (fx<? (fx1+ i) (charlines-length screen))) ; more lines will follow
                (begin
                  ;; cursor move down does not scroll, so print a newline.
                  (lineterm-write/u8 lctx 10)
                  (set! vx 0)
                  (set! vy (fx1+ vi)))
                (begin
                  (set! vx (fxmin (fx+ xdraw1 vxoffset) (fx1- width))) ;; cursor cannot be at vscreen width
                  (set! vy vi))))))))

    ;; if there is a dirty area below the last line, clear it
    (let ((yn (charlines-length screen)))
      (when (fx>=? ymax yn)
        (let ((vyn (fx+ prompt-y yn)))
          ; (debugf "linectx-redraw-dirty move (~s . ~s) -> (~s . ~s) then clear-to-eos" vx vy 0 vyn)
          (lineterm-move lctx vx vy 0 vyn)
          (set! vx 0)
          (set! vy vyn)
          (lineterm-clear-to-eos lctx))))

    ;; mark whole screen as not dirty
    (vscreen-dirty-set! screen #f)
    ;; set term-x and term-y to the current position
    (linectx-term-xy-set! lctx vx vy))

  ;; highlight matching parentheses
  (parenmatcher-clear! (linectx-parenmatcher lctx))
  (when (eq? 'highlight paren-option)
    (linectx-paren-update! lctx)
    (linectx-draw-bad-parens lctx 'highlight)
    (linectx-draw-paren lctx (linectx-paren lctx) 'highlight))

  ;; move the cursor to final position, and update term-x and term-y accordingly
  (let ((vx (linectx-vx lctx))
        (vy (linectx-vy lctx)))
    (lineterm-move-to lctx vx vy)
    (linectx-term-xy-set! lctx vx vy)))



;; redraw only cursor and parentheses
(define (linectx-redraw-cursor+paren lctx)
  (let ((old-paren (linectx-paren lctx))
        (new-paren (lineedit-paren-find/before-cursor lctx)))
    (unless (paren-equal-xy? old-paren new-paren)
      (linectx-draw-paren lctx old-paren 'plain)
      (linectx-draw-paren lctx new-paren 'highlight)
      (linectx-paren-set! lctx new-paren)))

  ;; move the cursor to final position, and update term-x and term-y accordingly
  (let ((vx (linectx-vx lctx))
        (vy (linectx-vy lctx)))
    (lineterm-move-to lctx vx vy)
    (linectx-term-xy-set! lctx vx vy)))


;; draw a single parentheses using specified style.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-paren lctx paren style)
  ; (debugf " linectx-draw-paren paren=~s style=~s" paren style)
  (when (paren? paren)
    (let ((style (if (eq? style 'highlight)
                   (if (paren-valid? paren) 'good 'bad)
                   'plain)))
      ; each token can be a char, or #f which means missing, or #t which means BOF/EOF
      (when (char? (paren-start-token paren))
        (linectx-draw-char-at-xy lctx (paren-start-x paren) (paren-start-y paren) style))
      (when (char? (paren-end-token paren))
        (linectx-draw-char-at-xy lctx (paren-end-x paren)   (paren-end-y paren)   style)))))



;; draw all invalid parentheses using specified style.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-bad-parens lctx style)
  (when #f ;; currently disabled, is broken by optimization in (lineedit-paren-find/before-cursor)
    (let ((paren (parenmatcher-paren (linectx-parenmatcher lctx))))
      (linectx-draw-bad-paren-recurse/start lctx paren style)
      (linectx-draw-bad-paren-recurse/end lctx paren style))))

;; draw the start of specified paren and the start of all contained parens using specified style.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-bad-paren-recurse/start lctx paren style)
  (when paren
    (linectx-draw-bad-paren/start lctx paren style)
    (let ((inner-span (paren-inner paren)))
      (when inner-span
        (span-iterate inner-span
          (lambda (i inner-paren)
            (linectx-draw-bad-paren-recurse/start lctx inner-paren style)))))))


;; draw the end of specified paren and the start of all contained parens using specified style.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-bad-paren-recurse/end lctx paren style)
  (when paren
    (let ((inner-span (paren-inner paren)))
      (when inner-span
        (span-iterate inner-span
          (lambda (i inner-paren)
            (linectx-draw-bad-paren-recurse/end lctx inner-paren style)))))
    (linectx-draw-bad-paren/end lctx paren style)))


;; draw the start of a single invalid parentheses using specified style.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-bad-paren/start lctx paren style)
  (void))

;; draw the end of a single invalid parentheses using specified style.
;; assumes linectx-term-x linectx-term term-x are up to date and updates them.
(define (linectx-draw-bad-paren/end lctx paren style)
  (void))

;; if position x y is inside current charlines, redraw char at x y with specified style.
;; used to highlight/unhighlight parentheses, brackes, braces and quotes.
;; assumes linectx-term-x and linectx-term-x are up to date and updates them.
(define (linectx-draw-char-at-xy lctx x y style)
  (let ((ch    (vscreen-char-at-xy (linectx-vscreen lctx) x y))
        (wbuf  (linectx-wbuf  lctx))
        (vx    (if (fxzero? y) (fx+ x (linectx-prompt-end-x lctx)) x)) ;; also count prompt length!
        (vy    (fx+ y (linectx-prompt-end-y lctx))))                   ;; also count prompt length!
    ; (debugf "linectx-draw-char-at-xy at (~s ~s) char ~s" x y ch)
    (when (and ch (char>=? ch #\space))
      (lineterm-move-to lctx vx vy)
      (case style
        ((good)
          (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 54 109)))  ; ESC[1;36m
        ((bad)
          (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 49 109)))) ; ESC[1;31m
      (bytespan-insert-back/char! wbuf ch)
      (when (or (eq? 'good style) (eq? 'bad style))
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 109))) ; ESC[m
      (linectx-term-xy-set! lctx (fx1+ vx) vy))))
