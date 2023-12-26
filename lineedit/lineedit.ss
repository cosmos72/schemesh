;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit (0 1))
  (export
    lineedit-clear!
    lineedit-lines-set! linectx-insert/rbuf!
    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down
    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol
    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char
    lineedit-key-del-left lineedit-key-del-right
    lineedit-key-del-word-left lineedit-key-del-word-right
    lineedit-key-del-line lineedit-key-del-line-left lineedit-key-del-line-right
    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right
    lineedit-key-history-next lineedit-key-history-prev
    lineedit-key-redraw lineedit-key-tab lineedit-key-toggle-insert
    lineedit-key-inspect
    lineedit-read
    lineedit-flush lineedit-finish)
  (import
    (rnrs)
    (only (chezscheme) display-condition fx1+ fx1- inspect record-writer void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (schemesh lineedit vscreen)
    (schemesh lineedit charhistory)
    (schemesh lineedit parens)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit linectx)
    (schemesh lineedit lineterm)
    (only (schemesh lineedit parser) make-parsectx*)
    (only (schemesh lineedit io) open-charlines-input-port)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))




;; find one key sequence in linectx-keytable matching rbuf and execute it
(define (linectx-keytable-call ctx)
  (assert (linectx? ctx))
  (let-values (((proc n) (linectx-keytable-find
                           (linectx-ktable ctx) (linectx-rbuf ctx))))
    ;; (format #t "; linectx-keytable-call consume ~s chars, call ~s~%" n proc)
    (cond
      ((procedure? proc) (proc ctx))
      ((hashtable? proc) (set! n 0)) ; incomplete sequence, wait for more keystrokes
      (#t  ; insert received bytes into current line
        (set! n (linectx-insert/rbuf! ctx n))))
    (let ((rbuf (linectx-rbuf ctx)))
      (bytespan-erase-front! rbuf n)
      (when (bytespan-empty? rbuf)
        (bytespan-clear! rbuf))) ; set begin, end to 0
    n))


;; return three values: position x y of start of word under cursor,
;; and number of characters between cursor and word start.
(define (linectx-find-left/word-begin ctx)
  (let ((screen (linectx-vscreen ctx)))
    (let-values (((x y) (vscreen-cursor-xy screen)))
      (let-values (((x y nsp) (vscreen-count-at-xy/left screen x y (lambda (ch) (char<=? ch #\space)))))
        (let-values (((x y nw) (vscreen-count-at-xy/left screen x y (lambda (ch) (char>? ch #\space)))))
          (values x y (fx+ nsp nw)))))))


;; return three values: position x y of end of word under cursor,
;; and number of characters between cursor and word end.
(define (linectx-find-right/word-end ctx)
  (let ((screen (linectx-vscreen ctx)))
    (let-values (((x y) (vscreen-cursor-xy screen)))
      (let-values (((x y nsp) (vscreen-count-at-xy/right screen x y (lambda (ch) (char<=? ch #\space)))))
        (let-values (((x y nsp) (vscreen-next-xy/or-self screen x y nsp)))
          (let-values (((x y nw) (vscreen-count-at-xy/right screen x y (lambda (ch) (char>? ch #\space)))))
            (let-values (((x y nw) (vscreen-next-xy/or-self screen x y nw)))
              (values x y (fx+ nsp nw)))))))))


(define (lineedit-flush ctx)
  (let* ((wbuf (linectx-wbuf ctx))
         (beg  (bytespan-peek-beg wbuf))
         (end  (bytespan-peek-end wbuf)))
    (when (fx<? beg end)
      (let ((bv (bytespan-peek-data wbuf))
            (stdout (linectx-stdout ctx))
            (n (fx- end beg)))
        (if (fixnum? stdout)
          ; TODO: loop on short writes
          (set! n (fd-write stdout bv beg end))
          (put-bytevector stdout bv beg n))
        (bytespan-clear! wbuf)))))

(define (lineedit-clear! ctx)
  (linectx-clear! ctx))


;; write #\newline before returning from (repl)
(define (lineedit-finish ctx)
  (lineterm-write/u8 ctx 10)
  (lineedit-flush ctx))


;; save current linectx-vscreen to history, then replace them
;; with a copy-on-write of specified charlines.
;; Does not update cursor x and y position.
(define (lineedit-lines-set! ctx lines)
  (assert-charlines? 'lineedit-lines-set! lines)
  (linectx-to-history ctx)
  (lineedit-clear! ctx) ; leaves a single, empty line in vscreen
  (unless (charlines-empty? lines)
    (let ((screen (linectx-vscreen ctx)))
      (charlines-clear! screen) ; removes the single, empty line from vscreen
      (charlines-iterate lines
        (lambda (y line)
          (charlines-insert-at/cline! screen y (charline-copy-on-write line)))))))


;; insert a single character into vscreen at cursor.
;; Also moves vscreen cursor one character to the right, and reflows vscreen as needed.
(define (linectx-insert/ch! ctx ch)
  (vscreen-insert/ch! (linectx-vscreen ctx) ch))


;; read n chars from charspan csp, starting at position = start
;; and insert them into vscreen at cursor.
;; Also moves cursor n characters to the right, and reflows vscreen as needed.
(define (linectx-insert/cspan! ctx csp start n)
  (assert (fx<=? 0 start (fx+ start n) (charspan-length csp)))
  (when (fx>? n 0)
    (vscreen-insert/cspan! (linectx-vscreen ctx) csp start n)))


;; read up to n bytes from bytespan bsp, starting at offset = start,
;; assume they are utf-8, convert them to characters and insert them into vscreen at cursor.
;; stops at any byte < 32, unless it's the first byte (which is skipped).
;; Also stops at incomplete utf-8 sequences.
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
;; return number of bytes actually read from bytespan.
(define (linectx-insert/bspan! ctx bsp start n)
  (assert (fx<=? 0 start (fx+ start n) (bytespan-length bsp)))
  (let ((beg   start)
        (pos   start)
        (end   (fx+ start n))
        (incomplete-utf8? #f))
    (do ()
        ((or incomplete-utf8?
             (fx>=? pos end)
             ; stop at any byte < 32, unless it's the first byte (which we skip)
             (and (fx>? pos beg) (fx<? (bytespan-ref/u8 bsp pos) 32))))
      (let-values (((ch len) (bytespan-ref/utf8 bsp pos (fx- end pos))))
        (set! pos (fxmin end (fx+ pos len)))
        (cond
          ((eq? #t ch)
            (set! incomplete-utf8? #t))
          ((and (char? ch) (char>=? ch #\space))
            (linectx-insert/ch! ctx ch)))))
    (fx- pos beg))) ; return number of bytes actually consumed

;; consume up to n bytes from rbuf and insert them into current line.
;; return number of bytes actually consumed
(define (linectx-insert/rbuf! ctx n)
  (linectx-insert/bspan! ctx (linectx-rbuf ctx) 0 n))


(define (lineedit-key-nop ctx)
  (void))

;; move cursor left by 1, moving to previous line if cursor x is 0
(define (lineedit-key-left ctx)
  (vscreen-cursor-move/left! (linectx-vscreen ctx) 1))


;; move cursor right by 1, moving to next line if cursor x is at end of current line
(define (lineedit-key-right ctx)
  (vscreen-cursor-move/right! (linectx-vscreen ctx) 1))


;; move cursor up by 1, moving to previous history entry if cursor y is 0
(define (lineedit-key-up ctx)
  (if (fx>? (linectx-iy ctx) 0)
    (vscreen-cursor-move/up! (linectx-vscreen ctx) 1)
    (lineedit-navigate-history ctx -1)))

;; move cursor up by 1, moving to next history entry if cursor y is at end of vscreen
(define (lineedit-key-down ctx)
  (if (fx<? (linectx-iy ctx) (linectx-end-y ctx))
    (vscreen-cursor-move/down! (linectx-vscreen ctx) 1)
    (lineedit-navigate-history ctx +1)))

;; move to start of word under cursor
(define (lineedit-key-word-left ctx)
  (let-values (((x y n) (linectx-find-left/word-begin ctx)))
    (when (and x y n (fx>? n 0))
      (linectx-ixy-set! ctx x y))))

;; move to end of word under cursor
(define (lineedit-key-word-right ctx)
  (let-values (((x y n) (linectx-find-right/word-end ctx)))
    (when (and x y n (fx>? n 0))
      (linectx-ixy-set! ctx x y))))

;; move to start of line
(define (lineedit-key-bol ctx)
  (linectx-ixy-set! ctx 0 (linectx-iy ctx)))

;; move to end of line
(define (lineedit-key-eol ctx)
  (linectx-ixy-set! ctx (greatest-fixnum) (linectx-iy ctx)))

(define (lineedit-key-break ctx)
  (linectx-clear! ctx))

;; delete one character to the right.
;; acts as end-of-file if vscreen is empty.
(define (lineedit-key-ctrl-d ctx)
  (if (vscreen-empty? (linectx-vscreen ctx))
    (linectx-eof-set! ctx #t)
    (lineedit-key-del-right ctx)))

(define (lineedit-key-transpose-char ctx)
  (void)) ;; TODO: implement

;; delete one character to the left of cursor.
;; moves cursor one character to the left.
(define (lineedit-key-del-left ctx)
  (vscreen-erase-left/n! (linectx-vscreen ctx) 1)
  (void))

;; delete one character under cursor.
;; does not move cursor.
(define (lineedit-key-del-right ctx)
  (vscreen-erase-right/n! (linectx-vscreen ctx) 1)
  (void))

;; delete from cursor to start of word under cursor.
;; moves cursor n characters to the left, where n is the number of deleted characters.
(define (lineedit-key-del-word-left ctx)
  (let-values (((x y n) (linectx-find-left/word-begin ctx)))
    (when (and x y n (fx>? n 0))
      (vscreen-erase-left/n! (linectx-vscreen ctx) n)
      (void))))

;; delete from cursor to end of word under cursor.
;; does not move cursor.
(define (lineedit-key-del-word-right ctx)
  (let-values (((x y n) (linectx-find-right/word-end ctx)))
    (when (and x y n (fx>? n 0))
      (vscreen-erase-right/n! (linectx-vscreen ctx) n)
      (void))))

(define (lineedit-key-del-line ctx)
  (void)) ;; TODO: implement

(define (lineedit-key-del-line-left ctx)
  (vscreen-erase-left/line! (linectx-vscreen ctx)))

(define (lineedit-key-del-line-right ctx)
  (vscreen-erase-right/line! (linectx-vscreen ctx)))

(define (lineedit-key-newline-left ctx)
  (void)) ;; TODO: implement

(define (lineedit-key-newline-right ctx)
  (void)) ;; TODO: implement

(define (lineedit-key-enter ctx)
  (linectx-return-set! ctx #t))

(define (lineedit-key-history-next ctx)
  (lineedit-navigate-history ctx +1))

(define (lineedit-key-history-prev ctx)
  (lineedit-navigate-history ctx -1))

(define (lineedit-key-redraw ctx)
  (let ((screen (linectx-vscreen ctx)))
    (charlines-dirty-y-add! screen 0 (charlines-length screen))))

(define (lineedit-key-tab ctx)
  (let ((completions (linectx-completions ctx))
        (func (linectx-completion-func ctx)))
    (when func
      ;; protect against exceptions in linectx-completion-func
      (try (func ctx)
        (catch (cond)
          (span-clear! completions)))
      (when (fx=? 1 (span-length completions))
        (let* ((completion (span-ref completions 0))
               (stem-len (charspan-length (linectx-completion-stem ctx)))
               (len (fx- (charspan-length completion) stem-len)))
          (when (fx>? len 0)
            (linectx-insert/cspan! ctx completion stem-len len)))))))

(define (lineedit-key-toggle-insert ctx)
  ; (error 'toggle-insert "test error"))
  (lineedit-key-inspect ctx))

(define (lineedit-key-inspect ctx)
  (dynamic-wind
    tty-restore!       ; run before body
    (lambda () (inspect ctx)) ; body
    tty-setraw!))      ; run after body

(define (lineedit-navigate-history ctx delta-y)
  (let ((y    (fx+ delta-y (linectx-history-index ctx)))
        (hist (linectx-history ctx)))
    ; TODO: when delta-y < 0, move cursor to end of first line
    (when (fx<? -1 y (charhistory-length hist))
      ; also saves a copy of current linectx-vscreen to history
      (lineedit-lines-set! ctx (charhistory-cow-ref hist y))
      (linectx-history-index-set! ctx y))))



;; append a copy-on-write clone of linectx-vscreen to history, and return such clone
;; which must NOT be modified - not even temporarily - because history references it
(define (linectx-return-lines ctx)
  (linectx-return-set! ctx #f) ; clear flag "user pressed ENTER"
  (linectx-redraw-set! ctx #t) ; set flag "redraw prompt and lines"
  (linectx-draw-parens ctx (linectx-parens ctx) 'plain) ; unhighlight parentheses
  (lineterm-move-dy ctx (fx- (charlines-length (linectx-vscreen ctx))
                         (linectx-iy ctx))) ; move to last input line
  (lineterm-write/u8 ctx 10) ; advance to next line.
  (let* ((y (linectx-history-index ctx))
         (hist (linectx-history ctx))
         (hist-len (charhistory-length hist)))
    ; always overwrite last history slot
    (linectx-history-index-set! ctx (fxmax 0 y (fx1- hist-len)))
    (let ((lines (linectx-to-history ctx)))
      (linectx-history-index-set! ctx (charhistory-length hist))
      (linectx-clear! ctx) ;; clear vscreen
      lines)))

;; repeatedly call (linectx-keytable-call) until ENTER is found and processed,
;; or until no more keytable matches are found.
;; if user pressed ENTER, return a reference to internal charlines (linectx-vscreen)
;; if waiting for more keypresses, return #t
;; if got end-of-file, return #f
(define (linectx-keytable-iterate ctx)
  (do ()
      ((or (linectx-return? ctx)
           (linectx-eof? ctx)
           (bytespan-empty? (linectx-rbuf ctx))
           (fxzero? (linectx-keytable-call ctx)))))
  (cond
    ((linectx-return? ctx) (linectx-return-lines ctx))
    ((linectx-eof?    ctx) #f)
    (#t                    #t)))

;; if tty size changed, resize and reflow vscreen
(define (linectx-resize ctx width height)
  (let ((screen (linectx-vscreen ctx)))
    (vscreen-resize! screen width height)))

;; react to SIGWINCH
(define (linectx-consume-sigwinch ctx)
  (when (signal-consume-sigwinch)
    (let ((sz (tty-size)))
      (when (pair? sz)
        (linectx-resize ctx (car sz) (cdr sz))))))

;; unconditionally draw prompt
(define (linectx-draw-prompt ctx)
  (let ((prompt (linectx-prompt ctx)))
    ;; (format #t "linectx-draw-prompt: prompt = ~s~%" prompt)
    (lineterm-write/bspan ctx prompt 0 (bytespan-length prompt))))

;; unconditionally draw all lines, then move tty cursor to its expected tty position,
;; finally draw matching parentheses.
(define (linectx-draw-lines+move+parens ctx)
  (let* ((screen (linectx-vscreen ctx))
         (width  (vscreen-width screen))
         (ymax   (fxmax 0 (fx1- (vscreen-end-y screen))))
         (nl?    #f))
    (charlines-iterate screen
      (lambda (y line)
        (lineterm-write/cbuffer ctx line 0 (charline-length line))
        (unless (or (fx=? y ymax) (charline-nl? line))
          (lineterm-write/u8 ctx 10))))
    (lineterm-clear-to-eos ctx)
    (let ((xmax (fx+ (vscreen-length-at-y screen ymax)
                     (if (fxzero? ymax) (vscreen-prompt-end-x screen) 0))))
      ;; (format #t "; linectx-draw-lines+move+parens xmax = ~s, ymax = ~s~%" xmax ymax)
      (lineterm-move-from ctx xmax ymax)))
  (linectx-draw-parens ctx (linectx-parens ctx) 'highlight))


(define bv-prompt-error (string->utf8 "error expanding prompt $ "))

;; update prompt
(define (linectx-update-prompt ctx)
  (let ((prompt (linectx-prompt ctx)))
    (assert (bytespan? prompt))
    (try ((linectx-prompt-func ctx) ctx)
      (catch (cond)
        (bytespan-clear! prompt)
        (let ((err-len (bytevector-length bv-prompt-error)))
          (bytespan-insert-back/bvector! prompt bv-prompt-error 0 err-len)
          (linectx-prompt-length-set! ctx err-len))))
    (let ((prompt-length (linectx-prompt-length ctx)))
      (assert (fx<=? 0 prompt-length (bytespan-length prompt)))
      (let-values (((y x) (fxdiv-and-mod prompt-length (linectx-width ctx))))
        (when (and (fxzero? x) (not (fxzero? y)))
          ; prompt actually ends at rightmost column
          (set! x (linectx-width ctx))
          (set! y (fx1- y)))
        (linectx-prompt-end-x-set! ctx x)
        (linectx-prompt-end-y-set! ctx y)))))


;; if needed, draw new prompt and lines
(define (linectx-draw-if-needed ctx)
  (when #t ; (linectx-redraw? ctx)
    (parenmatcher-clear! (linectx-parenmatcher ctx)))
    (lineterm-move-dy ctx (fx- (linectx-prompt-end-y ctx)))
    (lineterm-move-to-bol ctx)
    (linectx-update-prompt ctx)
    (linectx-draw-prompt ctx)
    ;; set term-x and term-y to the desired position
    (linectx-term-xy-set! ctx (linectx-vx ctx) (linectx-vy ctx))
    (linectx-draw-lines+move+parens ctx)
    (linectx-redraw-set! ctx #f)
    (vscreen-dirty-set! (linectx-vscreen ctx) #f))


;; if position x y is inside linectx-vscreen, redraw char at x y with specified style.
;; used to highlight/unhighlight parentheses, brackes, braces and quotes.
;; assumes tty cursor is at term-x term-y, and moves it back there after drawing char.
(define (linectx-draw-char-at-xy ctx x y style)
  (let ((ch    (vscreen-char-at-xy (linectx-vscreen ctx) x y))
        (wbuf  (linectx-wbuf  ctx))
        (vx    (if (fxzero? y) (fx+ x (linectx-prompt-end-x ctx)) x)) ;; also count prompt length!
        (vy    y))
    ;; (format #t "; linectx-draw-char-at-xy at (~s ~s) char ~s~%" x y ch)
    (when (and ch (char>=? ch #\space))
      (lineterm-move-to ctx vx vy)
      (when (eq? 'highlight style)
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 54 109) 0 7)) ; ESC[1;36m
      (bytespan-insert-back/utf8! wbuf ch)
      (when (eq? 'highlight style)
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 109) 0 3)) ; ESC[m
      (lineterm-move-from ctx (fx1+ vx) vy))))


(define (linectx-draw-parens ctx parens style)
  ;; draw parens only if both start and end positions are valid
  (when (parens-valid? parens)
    (linectx-draw-char-at-xy ctx (parens-start-x parens) (parens-start-y parens) style)
    (linectx-draw-char-at-xy ctx (parens-end-x parens)   (parens-end-y parens)   style)))


;; return x y position immediately to the left of cursor.
;; Returned position will be in the previous line if cursor x = 0 and y > 0.
;; Returns 0 -1 if cursor x = 0 and y = 0.
(define (linectx-ixy-before-cursor ctx)
  (let* ((screen (linectx-vscreen ctx))
         (x      (vscreen-cursor-x screen))
         (y      (vscreen-cursor-y screen))
         (ymax   (fx1- (vscreen-end-y screen))))
    (cond
      ((fx>? x 0) (values (fx1- x) y))
      ((fx>? y 0) (values (fx1- (vscreen-length-at-y screen (fx1- y))) (fx1- y)))
      (else       (values 0 -1))))) ;; x y is at start of input, there's no previous position


;; return #f or a parens object containing matching parentheses immediately to the left of cursor.
(define (linectx-parens-find ctx)
  (let ((ret     #f)
        (parsers (linectx-parsers ctx))
        (parenmatcher (linectx-parenmatcher ctx)))
    (when (and parsers parenmatcher)
      (let-values (((x y) (linectx-ixy-before-cursor ctx)))
        (let* ((screen  (linectx-vscreen ctx))
               (ch (vscreen-char-at-xy screen x y)))
          ;; (format #t "(linectx-parens-find) x = ~s, y = ~s, ch = ~s~%" x y ch)
          (when (and ch (is-parens-char? ch))
            ;; protect against exceptions in linectx-completion-func
            (try
              (set! ret
                (parenmatcher-find-match
                  parenmatcher
                  (lambda () (make-parsectx* (open-charlines-input-port screen) parsers 0 0))
                  (linectx-parser-name ctx)
                  x y))
              (catch (cond)
                (let ((port (current-output-port)))
                  (put-string port "\nexception in parenmatcher: ")
                  (display-condition cond port)
                  (put-char port #\newline))))))))
    ret))

;; return #t if both old-parens and new-parens are #f
;; or if both are parens and contain the same start-x start-y end-x and-y
(define (parens-equal-xy? old-parens new-parens)
  (or
    (and (not old-parens) (not new-parens))
    (and (parens? old-parens) (parens? new-parens)
      (fx=? (parens-start-x old-parens)
            (parens-start-x new-parens))
      (fx=? (parens-start-y old-parens)
            (parens-start-y new-parens))
      (fx=? (parens-end-x old-parens)
            (parens-end-x new-parens))
      (fx=? (parens-end-y old-parens)
            (parens-end-y new-parens)))))


(define (linectx-parens-maybe-update-and-redraw ctx)
  (let* ((old-parens (linectx-parens ctx))
         (new-parens (linectx-parens-find ctx)))
    (linectx-parens-set! ctx new-parens)
    (unless (parens-equal-xy? old-parens new-parens)
      (linectx-draw-parens ctx old-parens 'plain)
      (linectx-draw-parens ctx new-parens 'highlight))))


;; read some bytes, blocking at most for read-timeout-milliseconds
;;   (0 = non-blocking, -1 = unlimited timeout)
;; from (linectx-stdin ctx) and append them to (linectx-rbuf ctx).
;; return number of read bytes.
;; return 0 on timeout
;; return -1 on eof
(define (linectx-read ctx read-timeout-milliseconds)
  (lineedit-flush ctx)
  (let* ((rbuf (linectx-rbuf ctx))
         (rlen (bytespan-length rbuf))
         (max-n 1024)
         (stdin (linectx-stdin ctx))
         (got 0)
         (eof? #f))
    ; ensure bytespan-capacity-back is large enough
    (bytespan-reserve-back! rbuf (fx+ rlen max-n))
    (if (fixnum? stdin)
      ; stdin is a file descriptor -> call (fd-select) then (fd-read)
      (when (eq? 'read (fd-select stdin 'read read-timeout-milliseconds))
        (set! got (fd-read stdin (bytespan-peek-data rbuf)
                     (bytespan-peek-end rbuf) max-n))
        ; (fxzero? got) means end of file
        (set! eof? (fxzero? got)))
      ; stdin is a binary input port -> call (get-bytevector-n!)
      (let ((n (get-bytevector-n! stdin (bytespan-peek-data rbuf)
                                  (bytespan-peek-end rbuf) max-n)))
        (when (fixnum? n)
          (set! got n)
          ; (fxzero? n) means end of file
          (set! eof? (fxzero? n)))))
    (assert (fx>=? got 0))
    (bytespan-resize-back! rbuf (fx+ rlen got))
    (assert (fixnum? got))
    (assert (fx<=? 0 got max-n))
    (if eof? -1 got)))


;; invoked when some function called by lineedit-read raises a condition
(define (%lineedit-error ctx cond)
  ; remove offending input that triggered the condition
  (bytespan-clear! (linectx-rbuf ctx))
  ; display the condition
  (let ((port (current-output-port)))
    (put-string port "\nexception in lineedit-read: ")
    (display-condition cond port)
    (put-char port #\newline))
  (dynamic-wind
    tty-restore!
    (lambda () (inspect cond))
    tty-setraw!))


;; implementation of (lineedit-read)
(define (%lineedit-read ctx timeout-milliseconds)
  (linectx-consume-sigwinch ctx)
  (linectx-draw-if-needed ctx)
  (let ((ret (if (bytespan-empty? (linectx-rbuf ctx))
               #t ; need more input
               ; some bytes already in rbuf, try to consume them
               (linectx-keytable-iterate ctx))))
    (if (eq? #t ret)
      ; need more input
      (let ((n (linectx-read ctx timeout-milliseconds)))
        (cond
          ((fx>? n 0)
            ; got some bytes, call again (linectx-keytable-iterate) and return its value
             (linectx-keytable-iterate ctx))
          ((fxzero? n)
             ; read timed out, return #t
             (linectx-consume-sigwinch ctx)
             #t)
          (else
             ; end-of-file, return #f
             #f)))
      ; propagate return value of first (linectx-keytable-iterate)
      ret)))

;; Main entry point of lineedit library.
;; Reads user input from linectx-stdin and processes it.
;;
;; if user pressed ENTER, return a reference to internal linectx-vscreen.
;; if waiting for more keypresses, return #t
;; if got end-of-file, return #f
(define (lineedit-read ctx timeout-milliseconds)
  (try
    (begin
      ; write current-output-port buffered output before entering read loop
      (flush-output-port (current-output-port))
      (let ((ret (%lineedit-read ctx timeout-milliseconds)))
        ; write linectx buffered output before returning
        (linectx-parens-maybe-update-and-redraw ctx)
        (lineedit-flush ctx)
        ret))
    (catch (cond)
      (%lineedit-error ctx cond)
      #t))) ; return "waiting for more keypresses"

(let ((t linectx-default-keytable)
      (%add linectx-keytable-set!))
  (%add t lineedit-key-bol 1)      ; CTRL+A
  (%add t lineedit-key-left 2)     ; CTRL+B
  (%add t lineedit-key-break 3)    ; CTRL+C
  (%add t lineedit-key-ctrl-d 4)   ; CTRL+D
  (%add t lineedit-key-eol 5)      ; CTRL+E
  (%add t lineedit-key-right 6)    ; CTRL+F
  (%add t lineedit-key-del-left 8 127)    ; CTRL+H or BACKSPACE
  (%add t lineedit-key-tab 9)      ; CTRL+I or TAB
  (%add t lineedit-key-enter 10 13); CTRL+J or ENTER, CTRL+M
  (%add t lineedit-key-del-line-right 11) ; CTRL+K
  (%add t lineedit-key-redraw 12)  ; CTRL+L
  (%add t lineedit-key-history-next 14)   ; CTRL+N
  (%add t lineedit-key-newline-right 15)  ; CTRL+O
  (%add t lineedit-key-history-prev 16)   ; CTRL+P
  (%add t lineedit-key-transpose-char 20) ; CTRL+T
  (%add t lineedit-key-del-line-left 21)  ; CTRL+U
  ; CTRL+W, CTRL+BACKSPACE, ALT+BACKSPACE
  (%add t lineedit-key-del-word-left 23 31 '(27 127))
  ; sequences starting with ESC
  (%add t lineedit-key-word-left '(27 66) '(27 98)); ALT+B, ALT+b
  (%add t lineedit-key-del-word-right '(27 68) '(27 100)) ; ALT+D, ALT+d
  (%add t lineedit-key-word-right '(27 70) '(27 102))     ; ALT+F, ALT+f
  ; sequences starting with ESC O
  (%add t lineedit-key-up    '(27 79 65))     ; UP    \eOA
  (%add t lineedit-key-down  '(27 79 66))     ; DOWN  \eOB
  (%add t lineedit-key-right '(27 79 67))     ; RIGHT \eOC
  (%add t lineedit-key-left  '(27 79 68))     ; LEFT  \eOD
  (%add t lineedit-key-eol   '(27 79 70))     ; END   \eOF
  (%add t lineedit-key-bol   '(27 79 72))     ; HOME  \eOH
  (%add t lineedit-key-newline-left '(27 79 77))     ; KPRET \eOM
  (%add t lineedit-key-nop   '(27 79 80)      ; NUM-LOCK
     '(27 79 81) '(27 79 82) '(27 79 83))     ; KP/ KP* KP-
  ; sequences starting with ESC [             ;
  (%add t lineedit-key-up    '(27 91 65))     ; UP    \e[A
  (%add t lineedit-key-down  '(27 91 66))     ; DOWN  \e[B
  (%add t lineedit-key-right '(27 91 67))     ; RIGHT \e[C
  (%add t lineedit-key-left  '(27 91 68))     ; LEFT  \e[D
  (%add t lineedit-key-eol   '(27 91 70))     ; END   \e[F
  (%add t lineedit-key-bol   '(27 91 72)      ; HOME  \e[H
                         '(27 91 49 126))     ; HOME  \e[1~
  (%add t lineedit-key-history-prev '(27 91 53 126)) ; PGUP  \e[5~
  (%add t lineedit-key-history-next '(27 91 54 126)) ; PGDWN \e[6~

  (%add t lineedit-key-nop   '(27 91 91 65) '(27 91 91 66)    ; F1..F2
    '(27 91 91 67) '(27 91 91 68) '(27 91 91 69)              ; F3..F4
    '(27 91 49 53 126) '(27 91 49 55 126) '(27 91 49 56 126)  ; F4..F7
    '(27 91 49 57 126) '(27 91 50 48 126) '(27 91 50 49 126)  ; F8..F10
    '(27 91 50 50 126) '(27 91 50 51 126) '(27 91 50 52 126)) ; F?..F12

  (%add t lineedit-key-toggle-insert '(27 91 50 126)) ; INSERT \e[2~

  (%add t lineedit-key-del-right '(27 91 51 126)) ; DELETE \e[3~
  (%add t lineedit-key-eol   '(27 91 52 126))     ; END    \e[4~
) ; close let

) ; close library
