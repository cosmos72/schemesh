;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit lineedit (0 7 0))
  (export
    lineedit-clear!
    lineedit-lines-set! lineedit-insert/rbuf!
    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down
    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol
    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char
    lineedit-key-del-left lineedit-key-del-right
    lineedit-key-del-word-left lineedit-key-del-word-right
    lineedit-key-del-line lineedit-key-del-line-left lineedit-key-del-line-right
    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right
    lineedit-key-history-next lineedit-key-history-prev
    lineedit-key-redraw lineedit-key-tab lineedit-key-toggle-insert
    lineedit-paren-find/before-cursor lineedit-paren-find/surrounds-cursor
    lineedit-read lineedit-read-confirm-y-or-n? lineedit-flush lineedit-finish)
  (import
    (rnrs)
    (only (chezscheme) display-condition format fx1+ fx1- fx/ inspect record-writer top-level-value void)
    (schemesh bootstrap)
    (schemesh containers)
    (only (schemesh conversions) display-condition*)
    (schemesh posix fd)
    (schemesh lineedit vscreen)
    (schemesh lineedit charhistory)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit linectx)
    (schemesh lineedit lineterm)
    (only (schemesh lineedit parser) make-parsectx*)
    (only (schemesh lineedit charlines io) open-charlines-input-port)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))


;; find one key sequence in linectx-keytable matching rbuf and execute it
(define (linectx-keytable-call lctx)
  (assert* 'linectx-keytable-call (linectx? lctx))
  (let ((rbuf (linectx-rbuf lctx)))
    (let-values (((proc n) (linectx-keytable-find (linectx-ktable lctx) rbuf)))
      ; (debugf "linectx-keytable-call consume ~s bytes, call ~s" n proc)
      (cond
        ((procedure? proc) (void))     ; proc called below, we update rbuf first
        ((hashtable? proc) (set! n 0)) ; incomplete sequence, wait for more keystrokes
        (#t  ; insert received bytes into current line
          (set! n (lineedit-insert/rbuf! lctx n))))
      (unless (fxzero? n)
        (bytespan-erase-front! rbuf n)
        (when (bytespan-empty? rbuf)
          (bytespan-clear! rbuf))) ; set begin, end to 0
      (when (procedure? proc)
        ; call lineedit-key-... procedure after updating rbuf:
        ; it may need to read more keystrokes
        (proc lctx))
      n)))


;; return three values: position x y of start of word under cursor,
;; and number of characters between cursor and word start.
(define (linectx-find-left/word-begin lctx)
  (let ((screen (linectx-vscreen lctx)))
    (let-values (((x y) (vscreen-cursor-ixy screen)))
      (let-values (((x y nsp) (vscreen-count-before-xy/left screen x y %char-is-not-alphanumeric)))
        (let-values (((x y nw) (vscreen-count-before-xy/left screen x y %char-is-alphanumeric)))
          (values x y (fx+ nsp nw)))))))


;; return three values: position x y of end of word under cursor,
;; and number of characters between cursor and word end.
(define (linectx-find-right/word-end lctx)
  (let ((screen (linectx-vscreen lctx)))
    (let-values (((x y) (vscreen-cursor-ixy screen)))
      (let-values (((x y nsp) (vscreen-count-at-xy/right screen x y %char-is-not-alphanumeric)))
        (let-values (((x y nw) (vscreen-count-at-xy/right screen x y %char-is-alphanumeric)))
          (values x y (fx+ nsp nw)))))))


;; return #t if ch is an alphanumeric i.e. one of [0-9A-Za-z] or a Unicode codepoint >= 128
;; otherwise return #f
(define (%char-is-alphanumeric ch)
  (or (char<=? #\0 ch #\9)
      (char<=? #\A ch #\Z)
      (char<=? #\a ch #\z)
      (fx>=? (char->integer ch) 128)))

;; return #t if ch is not alphanumeric i.e. not one of [0-9A-Za-z] and not a Unicode codepoint >= 128
;; otherwise return #f
(define (%char-is-not-alphanumeric ch)
  (not (%char-is-alphanumeric ch)))


(define (lineedit-flush lctx)
  (let* ((wbuf (linectx-wbuf lctx))
         (beg  (bytespan-peek-beg wbuf))
         (end  (bytespan-peek-end wbuf)))
    (when (fx<? beg end)
      (let ((bv (bytespan-peek-data wbuf))
            (stdout (linectx-stdout lctx))
            (n (fx- end beg)))
        (if (fixnum? stdout)
          ; TODO: loop on short writes
          (set! n (fd-write stdout bv beg end))
          (put-bytevector stdout bv beg n))
        (bytespan-clear! wbuf)))))

(define (lineedit-clear! lctx)
  (linectx-clear! lctx))


;; write #\newline before returning from (repl)
(define (lineedit-finish lctx)
  (lineterm-write/u8 lctx 10)
  (lineedit-flush lctx))


;; save current linectx-vscreen to history,
;; then replace them with specified charlines.
;; Sets vscreen cursor to 0 0.
(define (lineedit-lines-set! lctx lines)
  (assert-charlines? 'lineedit-lines-set! lines)
  (linectx-to-history* lctx)
  (vscreen-assign*! (linectx-vscreen lctx) lines))

;; insert a single character into vscreen at cursor.
;; Also moves vscreen cursor one character to the right, and reflows vscreen as needed.
(define (linectx-insert/ch! lctx ch)
  (vscreen-insert/ch! (linectx-vscreen lctx) ch))


;; read n chars from charspan csp, starting at position = start
;; and insert them into vscreen at cursor.
;; Also moves cursor n characters to the right, and reflows vscreen as needed.
(define (linectx-insert/cspan! lctx csp start n)
  (assert* 'linectx-insert/cspan! (fx<=? 0 start (fx+ start n) (charspan-length csp)))
  (when (fx>? n 0)
    (vscreen-insert/cspan! (linectx-vscreen lctx) csp start n)))


;; read up to n bytes from bytespan bsp, starting at offset = start,
;; assume they are utf-8, convert them to characters and insert them into vscreen at cursor.
;; stops at any byte < 32, unless it's the first byte (which is skipped).
;; Also stops at incomplete utf-8 sequences.
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
;; return number of bytes actually read from bytespan and inserted.
(define (linectx-insert/bspan! lctx bsp start n)
  (assert* 'linectx-insert/bspan! (fx<=? 0 start (fx+ start n) (bytespan-length bsp)))
  (let ((beg   start)
        (pos   start)
        (end   (fx+ start n))
        (incomplete-utf8? #f))
    (do ()
        ((or incomplete-utf8?
             (fx>=? pos end)
             ; stop at any byte < 32, unless it's the first byte (which we skip)
             (and (fx>? pos beg) (fx<? (bytespan-ref/u8 bsp pos) 32))))
      (let-values (((ch len) (bytespan-ref/char bsp pos (fx- end pos))))
        (set! pos (fxmin end (fx+ pos len)))
        (cond
          ((eq? #t ch)
            (set! incomplete-utf8? #t))
          ((and (char? ch) (char>=? ch #\space))
            (linectx-insert/ch! lctx ch)))))
    (fx- pos beg))) ; return number of bytes actually inserted

;; read up to n bytes from rbuf and insert them into current line.
;; return number of bytes actually read from rbuf and inserted
(define (lineedit-insert/rbuf! lctx n)
  (linectx-insert/bspan! lctx (linectx-rbuf lctx) 0 n))

;; n is the number of bytes at the end of (linectx-rbuf)
;; that caused the call to this function.
;; If needed, such bytes can be read to choose which action will be performed.
(define (lineedit-key-nop lctx)
  (void))

;; move cursor left by 1, moving to previous line if cursor x is 0
(define (lineedit-key-left lctx)
  (vscreen-cursor-move/left! (linectx-vscreen lctx) 1))


;; move cursor right by 1, moving to next line if cursor x is at end of current line
(define (lineedit-key-right lctx)
  (vscreen-cursor-move/right! (linectx-vscreen lctx) 1))


;; move cursor up by 1, moving to previous history entry if cursor y is 0
(define (lineedit-key-up lctx)
  (if (fx>? (linectx-iy lctx) 0)
    (vscreen-cursor-move/up! (linectx-vscreen lctx) 1)
    (lineedit-navigate-history lctx -1)))

;; move cursor up by 1, moving to next history entry if cursor y is at end of vscreen
(define (lineedit-key-down lctx)
  (if (fx<? (fx1+ (linectx-iy lctx)) (linectx-end-y lctx))
    (vscreen-cursor-move/down! (linectx-vscreen lctx) 1)
    (lineedit-navigate-history lctx +1)))

;; move to start of word under cursor
(define (lineedit-key-word-left lctx)
  (let-values (((x y n) (linectx-find-left/word-begin lctx)))
    (when (and x y n (fx>? n 0))
      (linectx-ixy-set! lctx x y))))

;; move to end of word under cursor
(define (lineedit-key-word-right lctx)
  (let-values (((x y n) (linectx-find-right/word-end lctx)))
    (when (and x y n (fx>? n 0))
      (linectx-ixy-set! lctx x y))))

;; move to start of line
(define (lineedit-key-bol lctx)
  (linectx-ixy-set! lctx 0 (linectx-iy lctx)))

;; move to end of line
(define (lineedit-key-eol lctx)
  (linectx-ixy-set! lctx (greatest-fixnum) (linectx-iy lctx)))

(define (lineedit-key-break lctx)
  (linectx-clear! lctx)
  (linectx-return-set! lctx #t))

;; delete one character to the right.
;; acts as end-of-file if vscreen is empty.
(define (lineedit-key-ctrl-d lctx)
  (if (vscreen-empty? (linectx-vscreen lctx))
    (linectx-eof-set! lctx #t)
    (lineedit-key-del-right lctx)))

(define (lineedit-key-transpose-char lctx)
  (void)) ;; TODO: implement

;; delete one character to the left of cursor.
;; moves cursor one character to the left.
(define (lineedit-key-del-left lctx)
  (vscreen-erase-left/n! (linectx-vscreen lctx) 1)
  (void))

;; delete one character under cursor.
;; does not move cursor.
(define (lineedit-key-del-right lctx)
  (vscreen-erase-right/n! (linectx-vscreen lctx) 1)
  (void))

;; delete from cursor to start of word under cursor.
;; moves cursor n characters to the left, where n is the number of deleted characters.
(define (lineedit-key-del-word-left lctx)
  (let-values (((x y n) (linectx-find-left/word-begin lctx)))
    (when (and x y n (fx>? n 0))
      (vscreen-erase-left/n! (linectx-vscreen lctx) n)
      (void))))

;; delete from cursor to end of word under cursor.
;; does not move cursor.
(define (lineedit-key-del-word-right lctx)
  (let-values (((x y n) (linectx-find-right/word-end lctx)))
    (when (and x y n (fx>? n 0))
      (vscreen-erase-right/n! (linectx-vscreen lctx) n)
      (void))))

(define (lineedit-key-del-line lctx)
  (void)) ;; TODO: implement

(define (lineedit-key-del-line-left lctx)
  (vscreen-erase-left/line! (linectx-vscreen lctx)))

(define (lineedit-key-del-line-right lctx)
  (vscreen-erase-right/line! (linectx-vscreen lctx)))

(define (lineedit-key-newline-left lctx)
  (let ((screen (linectx-vscreen lctx)))
    (vscreen-insert-at-xy/newline! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen))
    (vscreen-cursor-move/right! screen 1)))

(define (lineedit-key-newline-right lctx)
  (let ((screen (linectx-vscreen lctx)))
    (vscreen-insert-at-xy/newline! screen (vscreen-cursor-ix screen) (vscreen-cursor-iy screen))))

(define (lineedit-key-enter lctx)
  (linectx-paren-update/force! lctx)
  (if (linectx-paren-recursive-ok? lctx)
    (linectx-return-set! lctx #t)
    (lineedit-key-newline-left lctx)))

(define (lineedit-key-history-next lctx)
  (let-values (((x y) (linectx-ixy lctx)))
    (let* ((hist (linectx-history lctx))
           (idx  (linectx-history-index lctx))
           (next-idx (charhistory-find/starts-with
                       hist
                       (fx1+ idx)
                       (charhistory-length hist)
                       (linectx-vscreen lctx)
                       x y)))
    (when next-idx
      (lineedit-navigate-history lctx (fx- next-idx idx))
      (linectx-ixy-set! lctx x y)))))

(define (lineedit-key-history-prev lctx)
  (let-values (((x y) (linectx-ixy lctx)))
    (let* ((hist (linectx-history lctx))
           (idx  (linectx-history-index lctx))
           (prev-idx (charhistory-rfind/starts-with hist 0 idx (linectx-vscreen lctx) x y)))
      (when prev-idx
        (lineedit-navigate-history lctx (fx- prev-idx idx))
        (linectx-ixy-set! lctx x y)))))


(define (lineedit-key-redraw lctx)
  (linectx-redraw-set! lctx #t))

(define (lineedit-key-tab lctx)
  (let ((func (linectx-completion-func lctx)))
    (when func
      (func lctx)
      (%lineedit-update-with-completions lctx))))


(define %linectx-confirm-display-completions?
  (let ((header (string->utf8 "; display all "))
        (footer (string->utf8 " possibilities? (y or n) ")))
    (lambda (lctx completions-n)
      (let ((wbuf (linectx-wbuf lctx)))
        (bytespan-insert-back/bvector! wbuf header 0 (bytevector-length header))
        (bytespan-display-back/fixnum! wbuf completions-n)
        (bytespan-insert-back/bvector! wbuf footer 0 (bytevector-length footer))
        (lineedit-flush lctx)
        (let ((ok? (lineedit-read-confirm-y-or-n? lctx)))
          (bytespan-insert-back/u8! wbuf (if ok? 121 110) 10)
          ok?)))))


(define (%lineedit-update-with-completions lctx)
  (let* ((stem          (linectx-completion-stem lctx))
         (completions   (linectx-completions lctx))
         (completions-n (span-length completions)))
    ; (debugf "%lineedit-update-with-completions stem=~s, completions=~s ..." stem completions)
    (unless (fxzero? completions-n)
      (let-values (((common-len max-len) (%lineedit-analyze-completions completions)))
        ; (debugf "%lineedit-update-with-completions stem=~s, common-len=~s, completions=~s" stem common-len completions)
        (cond
          ((not (fxzero? common-len))
            ; insert common prefix of all completions
            (let ((completion-0 (span-ref completions 0)))
              (linectx-insert/cspan! lctx completion-0 0 common-len)
              (when (and (fx=? 1 completions-n)
                         (not (char=? #\/ (charspan-ref completion-0 (fx1- common-len)))))
                (linectx-insert/ch! lctx #\space))))
          ((fx>? completions-n 1)
            ; erase prompt and lines (also sets flag "redraw prompt and lines"),
            ; then list all possible completions
            (linectx-undraw lctx)
            (let ((column-width (fx+ 2 (fx+ max-len (charspan-length stem)))))
              (let-values (((column-n row-n) (%lineedit-completions-column-n-row-n lctx completions-n column-width)))
                (when (or (%lineedit-completions-fit-vscreen? lctx column-n row-n)
                          (%linectx-confirm-display-completions? lctx completions-n))
                  (%lineedit-print-completion-table lctx stem completions column-width column-n row-n))))))))))


;; analyze completions, and return two values:
;;  the length of longest common prefix among completions,
;;  and the length of longest completion
(define (%lineedit-analyze-completions completions)
  (let* ((completion-0 (span-ref completions 0))
         (completion   (if (charspan? completion-0) completion-0 (charspan)))
         (common-len   (charspan-length completion))
         (max-len      common-len))
    (do ((n (span-length completions))
         (i 1 (fx1+ i)))
        ((fx>=? i n)
         (values common-len max-len))
      (let ((completion-i (span-ref completions i)))
        (when (charspan? completion-i) ; sanity
          ; (debugf "... %lineedit-analyze-completions stem-len = ~s, completion-i = ~s ... " stem-len completion-i)
          (let* ((completion-i-len (charspan-length completion-i))
                 (common-i-len
                   (if (fxzero? common-len)
                     0
                     (charspan-range-count= completion 0 completion-i 0 (fxmin common-len completion-i-len)))))
            ; (debugf "... %lineedit-analyze-completions common-i-len = ~s" common-i-len)
            (when (fx>? common-len common-i-len)
              (set! common-len common-i-len))
            (when (fx<? max-len completion-i-len)
              (set! max-len completion-i-len))))))))


;; return two values: number of screen columns and number screen of rows
;; needed to list completions
(define (%lineedit-completions-column-n-row-n lctx completions-n column-width)
  (let* ((screen-width  (linectx-width lctx))
         (screen-height (linectx-height lctx))
         (column-n      (fxmax 1 (fx/ screen-width column-width)))
         (row-n         (fx//round-up completions-n column-n)))
    (values column-n row-n)))


(define (%lineedit-completions-fit-vscreen? lctx column-n row-n)
  (fx<? row-n (linectx-height lctx)))


(define (%lineedit-print-completion-table lctx stem completions column-width column-n row-n)
  ; (debugf "%lineedit-print-completion-table stem=~s, completions=~s" stem completions)
  (repeat (linectx-width lctx)
    (lineterm-write/u8 lctx 45)) ; write a whole line full of #\-
  (lineterm-write/u8 lctx 10) ; write a newline
  (do ((completions-n (span-length completions))
       (row-i 0 (fx1+ row-i)))
      ((fx>=? row-i row-n))
    (%lineedit-print-completion-row lctx stem completions column-width column-n row-n row-i)))


(define (%lineedit-print-completion-row lctx stem completions column-width column-n row-n row-i)
  ; (debugf "%lineedit-print-completion-row column-n=~s, row-i=~s" column-n row-i)
  (do ((completions-n (span-length completions))
       (completions-i row-i (fx+ row-n completions-i))
       (column-i      0     (fx1+ column-i)))
      ((or (fx>=? column-i column-n) (fx>=? completions-i completions-n)))
    (%lineedit-print-completion-cell lctx stem (span-ref completions completions-i) column-width))
  (lineterm-write/u8 lctx 10)) ;; append newline after each row


(define (%lineedit-print-completion-cell lctx stem completion column-width)
  (lineterm-write/cspan lctx stem)
  (lineterm-write/cspan lctx completion)
  (repeat (fx- column-width (fx+ (charspan-length stem) (charspan-length completion)))
    (lineterm-write/u8 lctx 32))) ; pad with spaces


(define (fx//round-up x1 x2)
  (let ((q (fx/ x1 x2)))
    (if (fx=? x1 (fx* q x2))
      q
      (fx1+ q))))

(define (lineedit-key-toggle-insert lctx)
  (tty-inspect lctx))

(define (lineedit-key-cmd-cd-parent lctx)
  ((top-level-value 'sh-cd) "..")
  (linectx-redraw-all lctx))

(define (lineedit-key-cmd-ls lctx)
  (lineterm-move-to lctx (linectx-prompt-end-x lctx) (linectx-prompt-end-y lctx))
  (lineterm-write/bvector lctx #vu8(108 115 27 91 74 10) 0 6) ; l s ESC [ J \n
  (lineedit-flush lctx)
  ((top-level-value 'sh-run) ((top-level-value 'sh-cmd) "ls"))
  ; make enough space after command output for prompt and current line(s)
  (repeat (linectx-vy lctx)
    (lineterm-write/u8 lctx 10))
  (linectx-redraw-all lctx))

(define (lineedit-navigate-history lctx delta-y)
  (let ((y      (fx+ delta-y (linectx-history-index lctx)))
        (hist   (linectx-history lctx)))
    (when (fx<? -1 y (charhistory-length hist))
      ; also saves a copy of current linectx-vscreen to history
      (lineedit-lines-set! lctx (charhistory-ref/cow hist y))
      (linectx-history-index-set! lctx y)
      ;; if moving up, set vscreen cursor to end of first line
      ;; if moving down, set vscreen cursor to end of last line
      (linectx-ixy-set! lctx (greatest-fixnum)
                            (if (fx<? delta-y 0) 0 (greatest-fixnum))))))


;;
;; if linectx-vscreen is empty, return a shallow copy of it.
;; otherwise, append a shallow copy of it to history, and return such copy.
;;
;; in either case, returned charlines must NOT be modified - not even temporarily -
;; because in one case they are the vscreen, and in the other case history references it.
(define (linectx-return-lines* lctx)
  (linectx-return-set! lctx #f) ; clear flag "user pressed ENTER"
  (linectx-redraw-set! lctx #t) ; set flag "redraw prompt and lines"
  (linectx-draw-bad-parens lctx 'plain)                ; unhighlight bad parentheses
  (linectx-draw-paren lctx (linectx-paren lctx) 'plain) ; unhighlight current parentheses
  (lineterm-move-dy lctx (fx- (fx1- (linectx-end-y lctx))
                             (linectx-iy lctx))) ; move to last input line
  (lineterm-write/u8 lctx 10) ; advance to next line.
  (linectx-term-xy-set! lctx 0 0) ; set tty cursor to 0 0
  (let* ((y (linectx-history-index lctx))
         (hist (linectx-history lctx)))
    ; always overwrite last history slot
    (linectx-history-index-set! lctx (fxmax 0 y (fx1- (charhistory-length hist))))
    (let* ((screen (linectx-vscreen lctx))
           (lines (linectx-to-history* lctx)))
      (charhistory-erase-consecutive-empty-charlines-before! hist (charhistory-length hist))
      (linectx-history-index-set! lctx (charhistory-length hist))
      (linectx-clear! lctx) ;; clear vscreen
      lines)))

;; repeatedly call (linectx-keytable-call) until ENTER is found and processed,
;; or until no more keytable matches are found.
;; if user pressed ENTER, return a reference to internal charlines (linectx-vscreen)
;; if waiting for more keypresses, return #t
;; if got end-of-file, return #f
(define (linectx-keytable-iterate lctx)
  (do ()
      ((or (linectx-return? lctx)
           (linectx-eof? lctx)
           (bytespan-empty? (linectx-rbuf lctx))
           (fxzero? (linectx-keytable-call lctx)))))
  (cond
    ((linectx-return? lctx)
      (linectx-return-lines* lctx))
    ((linectx-eof?    lctx)
      ;; forget eof in case lctx is used by an outer repl
      (linectx-eof-set! lctx #f)
      ;; set flag "redraw prompt and lines" in case lctx is used by an outer repl
      (linectx-redraw-set! lctx #t)
      ;; return
      #f)
    (#t #t)))

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
  (let ((prompt (linectx-prompt lctx)))
    ; (debugf "linectx-draw-prompt: prompt = ~s" prompt)
    (lineterm-write/bspan lctx prompt 0 (bytespan-length prompt))))

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
    ((linectx-redraw? lctx)                  (linectx-redraw-all lctx))
    ((vscreen-dirty? (linectx-vscreen lctx)) (linectx-redraw-dirty lctx))
    (else                                   (linectx-redraw-cursor+paren lctx))))


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


;; redraw only dirty parts of vscreen
(define (linectx-redraw-dirty lctx)
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
  (linectx-paren-update! lctx)
  (linectx-draw-bad-parens lctx 'highlight)
  (linectx-draw-paren lctx (linectx-paren lctx) 'highlight)

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
          (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 54 109) 0 7))  ; ESC[1;36m
        ((bad)
          (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 49 109) 0 7))) ; ESC[1;31m
      (bytespan-insert-back/char! wbuf ch)
      (when (or (eq? 'good style) (eq? 'bad style))
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 109) 0 3)) ; ESC[m
      (linectx-term-xy-set! lctx (fx1+ vx) vy))))


;; return x y position immediately to the left of cursor.
;; Returned position will be in the previous line if cursor x = 0 and y > 0.
;; Returns 0 -1 if cursor x = 0 and y = 0.
(define (linectx-ixy-before-cursor lctx)
  (let* ((screen (linectx-vscreen lctx))
         (x      (vscreen-cursor-ix screen))
         (y      (vscreen-cursor-iy screen))
         (ymax   (fx1- (vscreen-length screen))))
    (cond
      ((fx>? x 0) (values (fx1- x) y))
      ((fx>? y 0) (values (fx1- (vscreen-length-at-y screen (fx1- y))) (fx1- y)))
      (else       (values 0 -1))))) ;; x y is at start of input, there's no previous position




;; return #f or a paren object containing matching parentheses immediately to the left of cursor.
(define (lineedit-paren-find/before-cursor lctx)
  (let ((ret     #f)
        (parsers (linectx-parsers lctx))
        (parenmatcher (linectx-parenmatcher lctx)))
    (when (and parsers parenmatcher)
      (let-values (((x y) (linectx-ixy-before-cursor lctx)))
        ;; parse parens ONLY if cursor is immediately after a is-paren? char,
        ;; because we want to only highlight current parentheses and its matching one, if any.
        ;;
        ;; this is fast, but breaks (linectx-draw-bad-parens)
        ;;
        (let* ((screen  (linectx-vscreen lctx))
               (ch (vscreen-char-at-xy screen x y)))
          (when (and ch (is-paren-char? ch))
            ;; protect against exceptions in parenmatcher-find/at
            (try
              (set! ret
                (parenmatcher-find/at
                  parenmatcher
                  (lambda () (make-parsectx* (open-charlines-input-port screen)
                                             parsers
                                             (vscreen-width screen)
                                             (vscreen-prompt-end-x screen)
                                             0
                                             0))
                  (linectx-parser-name lctx)
                  x y))
              (catch (ex)
                (let ((port (current-output-port)))
                  (put-string port "\n; Exception in parenmatcher-find/at: ")
                  (display-condition ex port)
                  (newline port))))))))
    ret))

;; return #f or innermost paren object surrounding the cursor.
(define (lineedit-paren-find/surrounds-cursor lctx)
  (let ((ret     #f)
        (parsers (linectx-parsers lctx))
        (parenmatcher (linectx-parenmatcher lctx)))
    (when (and parsers parenmatcher)
      (let* ((screen  (linectx-vscreen lctx))
             (x       (vscreen-cursor-ix screen))
             (y       (vscreen-cursor-iy screen)))
        ;; parse parens even if cursor is NOT immediately after a is-paren? char,
        ;; because we want the innermost paren than surrounds cursor.
        ;; protect against exceptions in parenmatcher-find/surrounds
        (try
          (set! ret
            (parenmatcher-find/surrounds
              parenmatcher
              (lambda () (make-parsectx* (open-charlines-input-port screen)
                                         parsers
                                         (vscreen-width screen)
                                         (vscreen-prompt-end-x screen)
                                         0
                                         0))
              (linectx-parser-name lctx)
              x y))
          (catch (ex)
            (let ((port (current-output-port)))
              (put-string port "\n; Exception in parenmatcher-find/surrounds: ")
              (display-condition ex port)
              (newline port))))))
    ret))


;; call (lineedit-paren-find/before-cursor) and save result into (linectx-paren). Return such result.
(define (linectx-paren-update! lctx)
  (let ((new-paren (lineedit-paren-find/before-cursor lctx)))
    (linectx-paren-set! lctx new-paren)
    new-paren))

;; clear cached parentheses and recompute them
(define (linectx-paren-update/force! lctx)
  (let ((parenmatcher (linectx-parenmatcher lctx))
        (parsers      (linectx-parsers lctx))
        (screen       (linectx-vscreen lctx)))
    (when parenmatcher
      (parenmatcher-clear! parenmatcher)
      (when parsers
        (parenmatcher-maybe-update!
          parenmatcher
          (lambda () (make-parsectx* (open-charlines-input-port screen)
                                     parsers
                                     (vscreen-width screen)
                                     (vscreen-prompt-end-x screen)
                                     0
                                     0))
          (linectx-parser-name lctx))))))

;; return #t if (paren-recursive-ok? paren) is truish for outermost paren inside parenmatcher,
;; and vscreen ends with an even number of #\\ - for example zero.
;;
;; reason: an unescaped #\\ must be followed by some other char
;; both in scheme and in shell syntax,
;; thus if present it means the input is incomplete and should not be evaluated.
(define (linectx-paren-recursive-ok? lctx)
  (and
    (fxeven?
      (charlines-count/left (linectx-vscreen lctx) (greatest-fixnum) (greatest-fixnum)
        (lambda (ch) (char=? ch #\\))))
    (let* ((parenmatcher (linectx-parenmatcher lctx))
           (paren (and parenmatcher (parenmatcher-paren parenmatcher))))
      (or (not paren)
          (paren-recursive-ok? paren)))))



;; return #t if both old-paren and new-paren are #f
;; or if both are paren and contain the same start-x start-y end-x and-y
(define (paren-equal-xy? old-paren new-paren)
  (or (eq? old-paren new-paren)
      (and
        (paren? old-paren) (paren? new-paren)
        (fx=? (paren-start-x old-paren)
              (paren-start-x new-paren))
        (fx=? (paren-start-y old-paren)
              (paren-start-y new-paren))
        (fx=? (paren-end-x old-paren)
              (paren-end-x new-paren))
        (fx=? (paren-end-y old-paren)
              (paren-end-y new-paren)))))



;; read some bytes, blocking at most for read-timeout-milliseconds
;;   (0 = non-blocking, -1 = unlimited timeout)
;; from (linectx-stdin lctx) and append them to (linectx-rbuf lctx).
;; return number of read bytes, or 0 on timeout, or -1 on eof
(define (linectx-read lctx read-timeout-milliseconds)
  (lineedit-flush lctx)
  (%linectx-read/some lctx 1024 read-timeout-milliseconds))


;; read some bytes, blocking at most for read-timeout-milliseconds
;;   (0 = non-blocking, -1 = unlimited timeout)
;; from (linectx-stdin lctx) and append them to (linectx-rbuf lctx).
;; return number of read bytes, or 0 on timeout, or -1 on eof or I/O error.
(define (%linectx-read/some lctx max-n read-timeout-milliseconds)
  (let* ((fd   (linectx-stdin lctx))
         (rbuf (linectx-rbuf lctx))
         (rlen (bytespan-length rbuf))
         (got  0)
         (eof? #f))
    (bytespan-reserve-back! rbuf (fx+ rlen max-n))
    (if (fixnum? fd)
      ; fd is a file descriptor -> call (fd-select) then (fd-read)
      (when (eq? 'read (fd-select fd 'read read-timeout-milliseconds))
        (let ((end (bytespan-peek-end rbuf)))
          (set! got (fd-read fd (bytespan-peek-data rbuf) end (fx+ end max-n))))
        ; (fxzero? got) means end of file
        (set! eof? (fxzero? got)))
      ; fd is a binary input port -> call (get-bytevector-n!)
      (let ((n (get-bytevector-n! fd (bytespan-peek-data rbuf)
                                     (bytespan-peek-end rbuf) max-n)))
        (when (fixnum? n)
          (set! got n)
          ; (fxzero? n) means end of file
          (set! eof? (fxzero? n)))))
    (assert* 'linectx-read (fixnum? got))
    (assert* 'linectx-read (fx<=? 0 got max-n))
    (bytespan-resize-back! rbuf (fx+ rlen got))
    (if eof? -1 got)))


;; consume a single byte from (linectx-rbuf lctx) and return it.
;; returns #f on eof or I/O error.
;;
;; if (linectx-rbuf lctx) is empty, refills it first.
;;
(define (%linectx-read-consume/u8 lctx)
  ; (debugf "> %linectx-read-consume/u8")
  (let ((rbuf (linectx-rbuf lctx)))
    (when (bytespan-empty? rbuf)
      (%linectx-read/some lctx 1 -1))
    ; (debugf "< %linectx-read-consume/u8 got=~s" (bytespan-length rbuf))
    (if (bytespan-empty? rbuf)
      #f
      (let ((u8 (bytespan-ref/u8 rbuf 0)))
        (bytespan-erase-front! rbuf 1)
        u8))))


;; consume bytes from (linectx-rbuf lctx) and refill it as needed,
;; until one of #\y #\n or eof is received.
;; return #t if #\y is received, otherwise return #f
(define (lineedit-read-confirm-y-or-n? lctx)
  (do ((byte-or-eof (%linectx-read-consume/u8 lctx) (%linectx-read-consume/u8 lctx)))
    ((memv byte-or-eof '(110 121 #f)) ; #\n #\y or eof
     (eqv? byte-or-eof 121))))


;; invoked when some function called by lineedit-read raises a condition
(define (%lineedit-error lctx ex)
  ; remove offending input that triggered the exception
  (bytespan-clear! (linectx-rbuf lctx))
  ; display the condition
  (let ((port (current-output-port)))
    (put-string port "\n; Exception in lineedit-read: ")
    (display-condition ex port)
    (newline port)
    (flush-output-port port))
  (tty-inspect ex))


;; implementation of (lineedit-read)
(define (%%lineedit-read lctx timeout-milliseconds)
  (let ((ret (if (bytespan-empty? (linectx-rbuf lctx))
               #t ; need more input
               ; some bytes already in rbuf, try to consume them
               (linectx-keytable-iterate lctx))))
    (if (eq? #t ret)
      ; need more input
      (let ((n (linectx-read lctx timeout-milliseconds)))
        (cond
          ((fx>? n 0)
            ; got some bytes, call again (linectx-keytable-iterate) and return its value
             (linectx-keytable-iterate lctx))
          ((fxzero? n)
             ; read timed out, return #t
             (linectx-consume-sigwinch lctx)
             #t)
          (else
             ; end-of-file, return #f
             #f)))
      ; propagate return value of first (linectx-keytable-iterate)
      ret)))

;; wrapper around (%%lineedit-read)
(define (%lineedit-read lctx timeout-milliseconds)
  (dynamic-wind
    (lambda ()
      (flush-output-port (current-output-port))
      (linectx-consume-sigwinch lctx)
      (linectx-redraw-as-needed lctx)
      (lineedit-flush lctx))
    (lambda () (%%lineedit-read lctx timeout-milliseconds))
    (lambda () (lineedit-flush lctx))))

;; Main entry point of lineedit library.
;; Reads user input from linectx-stdin and processes it.
;;
;; if user pressed ENTER, return a reference to internal linectx-vscreen.
;; if waiting for more keypresses, return #t
;; if got end-of-file, return #f
(define (lineedit-read lctx timeout-milliseconds)
  (try
    (%lineedit-read lctx timeout-milliseconds)
    (catch (ex)
      (%lineedit-error lctx ex)
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
  (%add t lineedit-key-word-left '(27 66) '(27 98))       ; ALT+B, ALT+b
  (%add t lineedit-key-del-word-right '(27 68) '(27 100)) ; ALT+D, ALT+d
  (%add t lineedit-key-word-right '(27 70) '(27 102))     ; ALT+F, ALT+f
  ; sequences starting with ESC O
  (%add t lineedit-key-up    '(27 79 65))        ; UP    \eOA
  (%add t lineedit-key-down  '(27 79 66))        ; DOWN  \eOB
  (%add t lineedit-key-right '(27 79 67))        ; RIGHT \eOC
  (%add t lineedit-key-left  '(27 79 68))        ; LEFT  \eOD
  (%add t lineedit-key-eol   '(27 79 70))        ; END   \eOF
  (%add t lineedit-key-bol   '(27 79 72))        ; HOME  \eOH
  (%add t lineedit-key-newline-left '(27 79 77)) ; KPRET \eOM
  (%add t lineedit-key-nop   '(27 79 80)         ; NUM-LOCK
    '(27 79 81) '(27 79 82))                     ; KP/ KP*
  (%add t lineedit-key-cmd-cd-parent '(27 79 83))  ; KP-
  (%add t lineedit-key-cmd-ls        '(27 79 108)) ; KP+
  ; sequences starting with ESC [                ;
  (%add t lineedit-key-up    '(27 91 65))        ; UP    \e[A
  (%add t lineedit-key-down  '(27 91 66))        ; DOWN  \e[B
  (%add t lineedit-key-right '(27 91 67))        ; RIGHT \e[C
  (%add t lineedit-key-left  '(27 91 68))        ; LEFT  \e[D
  (%add t lineedit-key-eol   '(27 91 70))        ; END   \e[F
  (%add t lineedit-key-bol   '(27 91 72)         ; HOME  \e[H
                         '(27 91 49 126))        ; HOME  \e[1~

  (%add t lineedit-key-toggle-insert '(27 91 50 126)) ; INSERT \e[2~
  (%add t lineedit-key-del-right     '(27 91 51 126)) ; DELETE \e[3~
  (%add t lineedit-key-eol           '(27 91 52 126)) ; END    \e[4~
  (%add t lineedit-key-history-prev  '(27 91 53 126)) ; PGUP   \e[5~
  (%add t lineedit-key-history-next  '(27 91 54 126)) ; PGDWN  \e[6~

  (%add t lineedit-key-nop   '(27 91 91 65) '(27 91 91 66)    ; F1..F2
    '(27 91 91 67) '(27 91 91 68) '(27 91 91 69)              ; F3..F4
    '(27 91 49 53 126) '(27 91 49 55 126) '(27 91 49 56 126)  ; F4..F7
    '(27 91 49 57 126) '(27 91 50 48 126) '(27 91 50 49 126)  ; F8..F10
    '(27 91 50 50 126) '(27 91 50 51 126) '(27 91 50 52 126)) ; F?..F12

) ; close let

) ; close library
