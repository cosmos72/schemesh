;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit (0 1))
  (export
    make-linectx make-linectx* linectx? linectx-line linectx-lines linectx-x linectx-y
    linectx-completions linectx-completion-stem
    linectx-parser-name linectx-parser-name-set!
    linectx-prompt linectx-prompt-length linectx-prompt-length-set!
    lineedit-default-keytable lineedit-clear!
    lineedit-lines-set! linectx-stdin-set! linectx-stdout-set! linectx-rbuf-insert!
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
    lineedit-keytable-set! lineedit-keytable-find lineedit-read
    lineedit-flush lineedit-finish)
  (import
    (rnrs)
    (only (chezscheme) display-condition fx1+ fx1- inspect record-writer void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (schemesh lineedit base)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))


; linectx is the top-level object used by most lineedit functions
(define-record-type
  (linectx %make-linectx linectx?)
  (fields
    (mutable rbuf)    ; bytespan, buffer for (fd-read)
    (mutable wbuf)    ; bytespan, buffer for (fd-write)
    (mutable line)    ; charline, input's current line being edited
    (mutable lines)   ; charlines, input being edited
    (mutable x); fixnum, cursor x position in line
    (mutable y); fixnum, cursor y position in lines
    (mutable match-x) ; fixnum, x position of matching parenthesis
    (mutable match-y) ; fixnum, y position of matching parenthesis
    (mutable rows)    ; fixnum, max number of rows being edited
    (mutable width)   ; fixnum, terminal width
    (mutable height)  ; fixnum, terminal height
    (mutable stdin)   ; input file descriptor, or binary input port
    (mutable stdout)  ; output file descriptor, or binary output port
    (mutable read-timeout-milliseconds) ; -1 means unlimited timeout
    ; bitwise or of: flag-eof? flag-return? flag-sigwinch? flag-redisplay?
    (mutable flags)
    (mutable parser-name)   ; symbol, name of current parser
    prompt           ; bytespan, prompt
    (mutable prompt-end-x)  ; fixnum, tty x column where prompt ends
    (mutable prompt-end-y)  ; fixnum, tty y row where prompt ends
    (mutable prompt-length) ; fixnum, prompt display length
    ; procedure, receives linectx as argument and should update prompt and prompt-length
    (mutable prompt-func)
    completions     ;         span of charspans, possible completions
    completion-stem ;         charspan, chars from lines used as stem
    ; procedure, receives linectx as argument and should update completions and stem
    (mutable completion-func)
    (mutable keytable)      ; hashtable, contains keybindings
    (mutable history-index) ; index of last used item in history
    history))        ; charhistory, history of entered commands

(define flag-eof? 1)
(define flag-return? 2)
(define flag-sigwinch? 4)
(define flag-redisplay? 8)

(define (linectx-flag? ctx bit)
  (not (fxzero? (fxand bit (linectx-flags ctx)))))

(define (linectx-flag-set! ctx bit flag?)
  (assert (boolean? flag?))
  (let ((flags (linectx-flags ctx)))
    (linectx-flags-set! ctx
      (if flag?
        (fxior flags bit)
        (fxand flags (fxnot bit))))))
;   (format #t "linectx-flag-set! ~s -> ~s~%" flags (linectx-flags ctx))))

(define (linectx-eof? ctx)
  (linectx-flag? ctx flag-eof?))
(define (linectx-return? ctx)
  (linectx-flag? ctx flag-return?))
(define (linectx-sigwinch? ctx)
  (linectx-flag? ctx flag-sigwinch?))
(define (linectx-redisplay? ctx)
  (linectx-flag? ctx flag-redisplay?))

(define (linectx-eof-set! ctx flag?)
  (linectx-flag-set! ctx flag-eof? flag?))
(define (linectx-return-set! ctx flag?)
  (linectx-flag-set! ctx flag-return? flag?))
(define (linectx-sigwinch-set! ctx flag?)
  (linectx-flag-set! ctx flag-sigwinch? flag?))
(define (linectx-redisplay-set! ctx flag?)
  (linectx-flag-set! ctx flag-redisplay? flag?))

(define lineedit-default-keytable (eq-hashtable))

; argument prompt-func must be a procedure accepting linectx
; and updating linectx-prompt and linectx-prompt-length.
;
; argument prompt-func must be #f or a procedure accepting linectx
; and updating linectx-completions
(define (make-linectx* prompt-func completion-func)
  (assert (procedure? prompt-func))
  (when completion-func
    (assert (procedure? completion-func)))
  (let* ((sz    (tty-size))
         (rbuf  (bytespan))
         (wbuf  (bytespan))
         (line  (charline))
         (lines (charlines line)))
    (bytespan-reserve-back! rbuf 1024)
    (bytespan-reserve-back! wbuf 1024)
    (%make-linectx
      rbuf wbuf line lines
      0 0 -1 -1 1                ; x y match-x match-y rows
      (if (pair? sz) (car sz) 80); width
      (if (pair? sz) (cdr sz) 24); height
      0 1 -1 flag-redisplay?     ; stdin stdout read-timeout flags
      'shell                     ; parser-name
      (bytespan) 0 0             ; prompt prompt-end-x prompt-end-y
      0 prompt-func              ; prompt-length prompt-func
      (span) (charspan) completion-func ; completions stem completion-func
      lineedit-default-keytable  ; keytable
      0 (charhistory))))         ; history

(define (default-prompt-func ctx)
  (let* ((str    (symbol->string (linectx-parser-name ctx)))
         (bv     (string->utf8 str))
         (prompt (linectx-prompt ctx)))
    (bytespan-clear! prompt)
    (bytespan-bv-insert-back! prompt bv 0 (bytevector-length bv))
    ; append colon and space after parser name
    (bytespan-u8-insert-back! prompt 58 32)
    (linectx-prompt-length-set! ctx (fx+ 2 (string-length str)))))

(define make-linectx
  (case-lambda
    (()
       (make-linectx* default-prompt-func #f))
    ((prompt-func)
       (make-linectx* prompt-func #f))
    ((prompt-func completion-func)
       (make-linectx* prompt-func completion-func))))

; Clear and recreate line and lines: they may have been saved to history, which retains
; them.
; Does NOT write anything to the tty
(define (linectx-clear! ctx)
  (linectx-x-set! ctx 0)
  (linectx-y-set! ctx 0)
  (linectx-match-x-set! ctx -1)
  (linectx-match-y-set! ctx -1)
  (linectx-return-set!  ctx #f)
  (let ((line (charline)))
    (linectx-line-set!  ctx line)
    (linectx-lines-set! ctx (charlines line))))

; write a byte to wbuf
(define (linectx-u8-write ctx u8)
  (bytespan-u8-insert-back! (linectx-wbuf ctx) u8))

; write a portion of given bytevector to wbuf
(define (linectx-bv-write ctx bv start n)
  (bytespan-bv-insert-back! (linectx-wbuf ctx) bv start n))

; write a portion of given bytespan to wbuf
(define (linectx-bsp-write ctx bsp start n)
  (bytespan-bsp-insert-back! (linectx-wbuf ctx) bsp start n))

; write a portion of given chargbuffer to wbuf
(define (linectx-cgb-write ctx cgb start end)
  (do ((wbuf (linectx-wbuf ctx))
       (pos start (fx1+ pos)))
      ((fx>=? pos end))
    (bytespan-utf8-insert-back! wbuf (chargbuffer-ref cgb pos))))

; return a copy-on-write clone of current lines being edited
(define (linectx-lines-copy ctx)
; "  (format #t "linectx-lines-copy~%")"
; "  (dynamic-wind tty-restore! break tty-setraw!)
  (charlines-copy-on-write (linectx-lines ctx)))

; save current linectx-lines to history, and return them
(define (linectx-to-history ctx)
;  TODO: do not insert duplicates in history
  (let ((lines (linectx-lines ctx)))
    (charhistory-set! (linectx-history ctx) (linectx-history-index ctx) lines)
    lines))

; Move tty cursor vertically.
; If dy > 0, send escape sequence "move cursor down by dy".
; If dy < 0, send escape sequence "move cursor up by -dy".
; Does not check or update linectx.

(define (term-move-dy ctx dy)
  (cond
    ((fxzero? dy) ; do nothing
      (void))
    ((fx=? dy 1)  ; move down by 1
      (linectx-bv-write ctx #vu8(27 91 66) 0 3))  ; ESC [ B
    ((fx=? dy -1) ; move up by 1
      (linectx-bv-write ctx #vu8(27 91 65) 0 3))  ; ESC [ A
    ((fx>? dy 1) ; move down by dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-u8-insert-back! wbuf 27 91)     ; ESC [
        (bytespan-fixnum-display-back! wbuf dy)   ; n
        (bytespan-u8-insert-back! wbuf 66)))      ; B
    ((fx<? dy -1) ; move up by -dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-u8-insert-back! wbuf 27 91)     ; ESC [
        (bytespan-fixnum-display-back! wbuf (fx- dy)) ; n
        (bytespan-u8-insert-back! wbuf 65)))))    ; A

; Move tty cursor horizontally.
; If dx > 0, send escape sequence "move cursor right by dx".
; If dx < 0, send escape sequence "move cursor left by -dx".
; Does not check or update linectx.
(define (term-move-dx ctx dx)
  (cond
    ((fxzero? dx) ; do nothing             ;
      (void))                                  ;
    ((fx=? dx 1) ; move right by 1         ;
      (linectx-bv-write ctx #vu8(27 91 67) 0 3))      ; ESC [ C
    ((fx=? dx -1) ; move left by 1         ;
      (linectx-bv-write ctx #vu8(27 91 68) 0 3))      ; ESC [ D
    ((fx>? dx 1) ; move right by n         ;
      (let ((wbuf (linectx-wbuf ctx)))         ;
        (bytespan-u8-insert-back! wbuf 27 91)  ; ESC [
        (bytespan-fixnum-display-back! wbuf dx); n
        (bytespan-u8-insert-back! wbuf 67)))   ; C
    ((fx<? dx -1) ; move left by -dx       ;
      (let ((wbuf (linectx-wbuf ctx)))         ;
        (bytespan-u8-insert-back! wbuf 27 91)  ; ESC [
        (bytespan-fixnum-display-back! wbuf (fx- dx)) ; n
        (bytespan-u8-insert-back! wbuf 68))))) ; D

; move tty cursor back to linectx-x, linectx-y from its current position at from-x, from-y
(define (linectx-move-from ctx from-x from-y)
  (let ((x (linectx-x ctx))
        (y (linectx-y ctx))
        (prompt-x (linectx-prompt-end-x ctx)))
    (term-move-dy ctx (fx- y from-y))
    ; we may move from/to first line, which is prefixed by the prompt: adjust x and from-x
    (when (fxzero? y)
      (set! x (fx+ x prompt-x)))
    (when (fxzero? from-y)
      (set! from-x (fx+ from-x prompt-x)))
    (term-move-dx ctx (fx- x from-x))))

; send escape sequence "delete n chars at right", without checking or updating linectx
(define (term-del-right-n ctx n)
  (cond
    ((fx<=? n 0) (void)) ; nop
    ((fx=? n 1)
      (linectx-bv-write ctx #vu8(27 91 80) 0 3)) ; VT102 sequence: ESC [ P
    (#t
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-u8-insert-back! wbuf 27 91)   ; ESC [
        (bytespan-fixnum-display-back! wbuf n)  ; n
        (bytespan-u8-insert-back! wbuf 80)))))  ; P

; send escape sequence "move to begin-of-line". Moves at beginning of prompt!
(define (term-move-to-bol ctx)
  (linectx-u8-write ctx 13)) ; CTRL+M i.e. '\r'

; send escape sequence "clear from cursor to end-of-line"
(define (term-clear-to-eol ctx)
  (linectx-bv-write ctx #vu8(27 91 75) 0 3)) ; ESC [ K

; send escape sequence "clear from cursor to end-of-screen"
(define (term-clear-to-eos ctx)
  (linectx-bv-write ctx #vu8(27 91 74) 0 3)) ; ESC [ J

; clear-line-right must be either 'clear-line-right or 'dont-clear-line-right*/
(define (term-redraw-to-eol ctx clear-line-right)
  (let* ((line (linectx-line ctx))
         (beg  (linectx-x ctx))
         (end  (charline-length line)))
    (when (fx<? beg end)
      (linectx-cgb-write ctx line beg end))
    (when (eq? clear-line-right 'clear-line-right)
      (term-clear-to-eol ctx))
    (term-move-dx ctx (fx- beg end))))

; return index of first character before position = end in line that satisfies (pred ch).
; return -1 if no character before position = end in line satisfies (pred ch)
(define (char-find-left line end pred)
  (assert (fx<=? end (charline-length line)))
  (do ((x (fx1- end) (fx1- x)))
      ((or (fx<? x 0) (pred (charline-ref line x)))
        x)))

; return index of first character at position = start or later in line that satisfies
; (pred ch). return (charline-length line) if no character at position = start or later in
; line satisfies (pred ch)
(define (char-find-right line start pred)
  (assert (fx>=? start 0))
  (do ((x start (fx1+ x))
       (len (charline-length line)))
      ((or (fx>=? x len) (pred (charline-ref line x)))
        x)))

; return index of beginning of word at position < end in line
(define (word-find-begin-left line end)
  (let* ((pos1 (fx1+ (char-find-left line end  (lambda (ch) (char>? ch #\space)))))
         (pos2 (fx1+ (char-find-left line pos1 (lambda (ch) (char<=? ch #\space))))))
    pos2))

; return index of end of word at position >= start in line
(define (word-find-end-right line start)
  (let* ((pos1 (char-find-right line start  (lambda (ch) (char>? ch #\space))))
         (pos2 (char-find-right line pos1 (lambda (ch) (char<=? ch #\space)))))
    pos2))

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
  (let ((x  (linectx-x ctx))
        (y  (linectx-y ctx)))
    (linectx-clear! ctx)
    (linectx-move-from ctx x y)
    (term-clear-to-eos ctx))) ; erase all lines, preserving prompt

; write #\newline before returning from (repl)
(define (lineedit-finish ctx)
  (linectx-u8-write ctx 10)
  (lineedit-flush ctx))
; save current linectx-lines to history, then replace current lines with specified
; charlines - which are retained, do NOT modify them after calling this function

(define (lineedit-lines-set! ctx lines)
  (assert-charlines? 'lineedit-lines-set! lines)
  (linectx-to-history ctx)
  (lineedit-clear! ctx) ; leaves a single, empty line in lines
  (when (charlines-empty? lines)
    (set! lines (linectx-lines ctx)))
  (charlines-iterate lines
    (lambda (i line)
      (linectx-cgb-write ctx line 0 (charline-length line))))
  (let* ((lines-n-1 (fx1- (charlines-length lines)))
         (line      (charlines-ref lines lines-n-1)))
    (linectx-line-set!  ctx line)
    (linectx-lines-set! ctx lines)
    (linectx-x-set! ctx (charline-length line))
    (linectx-y-set! ctx lines-n-1)))

; consume n chars from charspan csp, starting at offset = start
; and insert them into current line.
(define (linectx-csp-insert! ctx csp start n)
  (assert (fx<=? 0 start (fx+ start n) (charspan-length csp)))
  (let* ((beg   (fx+ start (charspan-peek-beg csp)))
         (pos   beg)
         (end   (fx+ pos n))
         (line  (linectx-line ctx))
         (x     (linectx-x ctx))
         (wbuf  (linectx-wbuf ctx)))
    ; TODO: handle lines longer than tty width
    (do ()
        ((fx>=? pos end))
      (let ((ch (charspan-ref csp pos)))
        (set! pos (fx1+ pos))
        (charline-insert-at! line x ch)
        (bytespan-utf8-insert-back! wbuf ch)
        (set! x (fx1+ x))))
    (linectx-x-set! ctx x)
    (term-redraw-to-eol ctx 'dont-clear-line-right)))

; consume up to n bytes from bytespan bsp, starting at offset = start
; and insert them into current line.
; return number of bytes actually consumed
(define (linectx-bsp-insert! ctx bsp start n)
  (assert (fx<=? 0 start (fx+ start n) (bytespan-length bsp)))
  (let ((beg   start)
        (pos   start)
        (end   (fx+ start n))
        (line  (linectx-line ctx))
        (x     (linectx-x ctx))
        (incomplete #f)
        (wbuf  (linectx-wbuf ctx)))
;  TODO: handle lines longer than tty width
    (do ((iter 0 (fx1+ iter)))
        ((or incomplete
             (fx>=? pos end)
;            stop at any byte < 32, unless it's the first byte (which we skip)
             (and (fx>? pos beg) (fx<? (bytespan-u8-ref bsp pos) 32))))
      (let-values (((ch len) (bytespan-utf8-ref bsp pos (fx- end pos))))
        (when (eq? #t ch)
          (set! incomplete #t))
        (set! pos (fxmin end (fx+ pos len)))
        (when (and (char? ch) (char>=? ch #\space))
          (charline-insert-at! line x ch)
          (bytespan-utf8-insert-back! wbuf ch)
          (set! x (fx1+ x)))))
    (linectx-x-set! ctx x)
    (term-redraw-to-eol ctx 'dont-clear-line-right)
    (fx- pos beg))) ; return number of bytes actually consumed

; consume up to n bytes from rbuf and insert them into current line.
; return number of bytes actually consumed
(define (linectx-rbuf-insert! ctx n)
  (linectx-bsp-insert! ctx (linectx-rbuf ctx) 0 n))

(define (lineedit-key-nop ctx)
  (void))

; move cursor left by 1
(define (lineedit-key-left ctx)
  (let ((x (linectx-x ctx)))
    (when (fx>? x 0)
      (linectx-x-set! ctx (fx1- x))
      (term-move-dx ctx -1))))

(define (lineedit-key-right ctx)
  (let ((x (linectx-x ctx)))
    (when (fx<? x (charline-length (linectx-line ctx)))
      (linectx-x-set! ctx (fx1+ x))
      (term-move-dx ctx 1))))

(define (lineedit-key-up ctx)
  ; TODO: multiline editing
  (lineedit-navigate-history ctx -1))

(define (lineedit-key-down ctx)
  ; TODO: multiline editing
  (lineedit-navigate-history ctx +1))

(define (lineedit-key-word-left ctx)
  (let* ((x   (linectx-x ctx))
         (pos (word-find-begin-left (linectx-line ctx) x))
         (dx  (fx- pos x)))
    (when (fx<? dx 0)
      (linectx-x-set! ctx pos)
      (term-move-dx ctx dx))))

(define (lineedit-key-word-right ctx)
  (let* ((x    (linectx-x ctx))
         (pos   (word-find-end-right (linectx-line ctx) x))
         (move-n (fx- pos x)))
    (when (fx>? move-n 0)
      (linectx-x-set! ctx pos)
      (term-move-dx ctx move-n))))

(define (lineedit-key-bol ctx)
  (let ((x (linectx-x ctx)))
    (when (fx>? x 0)
      (linectx-x-set! ctx 0)
      ; do not use (term-move-to-bol), there may be prompt at bol
      (term-move-dx ctx (fx- x)))))

(define (lineedit-key-eol ctx)
  (let ((x    (linectx-x ctx))
        (len  (charline-length (linectx-line ctx))))
    (when (fx<? x len)
      (linectx-x-set! ctx len)
      (term-move-dx ctx (fx- len x)))))

(define (lineedit-key-break ctx)
  (lineedit-clear! ctx))

(define (lineedit-key-ctrl-d ctx)
  (if (and (fx=? 0 (charline-length (linectx-line ctx)))
           (fx=? 1 (charlines-length (linectx-lines ctx))))
    (linectx-eof-set! ctx #t)
    (lineedit-key-del-right ctx)))

(define (lineedit-key-transpose-char ctx)
  (let* ((x    (linectx-x ctx))
         (line (linectx-line ctx))
         (len  (charline-length line)))
    (when (and (fx>? x 0) (fx>? len 1))
      (let ((eol (fx=? x len)))
        (term-move-dx ctx (if eol -2 -1))
        (when eol
          (set! x (fx1- x))))
      (let ((ch1  (charline-ref line (fx1- x)))
            (ch2  (charline-ref line x))
            (wbuf (linectx-wbuf ctx)))
        (bytespan-utf8-insert-back! wbuf ch2)
        (bytespan-utf8-insert-back! wbuf ch1)
        (charline-set! line (fx1- x) ch2)
        (charline-set! line x ch1)
        (linectx-x-set! ctx (fx1+ x))))))

(define (lineedit-key-del-left ctx)
  (when (fx>? (linectx-x ctx) 0)
    (lineedit-key-left ctx)
    (lineedit-key-del-right ctx)))

(define (lineedit-key-del-right ctx)
  (let ((x    (linectx-x ctx))
        (line (linectx-line ctx)))
    (when (fx<? x (charline-length line))
      (charline-erase-at! line x 1)
      (term-del-right-n ctx 1))))

(define (lineedit-key-del-word-left ctx)
  (let* ((x     (linectx-x ctx))
         (line  (linectx-line ctx))
         (pos   (word-find-begin-left line x))
         (del-n (fx- x pos)))
    (when (fx>? del-n 0)
      (charline-erase-at! line pos del-n)
      (linectx-x-set! ctx pos)
      (term-move-dx ctx (fx- del-n))
      (term-del-right-n ctx del-n))))

(define (lineedit-key-del-word-right ctx)
  (let* ((x     (linectx-x ctx))
         (line  (linectx-line ctx))
         (pos   (word-find-end-right line x))
         (del-n (fx- pos x)))
    (when (fx>? del-n 0)
      (charline-erase-at! line x del-n)
      (term-del-right-n ctx del-n))))

(define (lineedit-key-del-line ctx)
  (void))

(define (lineedit-key-del-line-left ctx)
  (let* ((x    (linectx-x ctx))
         (line (linectx-line ctx))
         (len  (charline-length line)))
    (when (and (fx>? x 0) (fx>? len 0))
      (charline-erase-at! line 0 x)
      (linectx-x-set! ctx 0)
      (term-move-dx ctx (fx- x))
      (term-redraw-to-eol ctx 'clear-line-right))))

(define (lineedit-key-del-line-right ctx)
  (let* ((x    (linectx-x ctx))
         (line (linectx-line ctx))
         (len  (charline-length line)))
    (when (fx<? x len)
      (charline-erase-at! line x (fx- len x))
      (term-clear-to-eol ctx))))

(define (lineedit-key-newline-left ctx)
  (void))

(define (lineedit-key-newline-right ctx)
  (void))

(define (lineedit-key-enter ctx)
  (linectx-return-set! ctx #t)
  (linectx-u8-write ctx 10))

(define (lineedit-key-history-next ctx)
  (lineedit-navigate-history ctx +1))

(define (lineedit-key-history-prev ctx)
  (lineedit-navigate-history ctx -1))

(define (lineedit-key-redraw ctx)
  (term-move-to-bol ctx)
  (term-move-dy ctx (fx- (fx+ (linectx-y ctx) (linectx-prompt-end-y ctx))))
  (linectx-display-if-needed ctx 'force))

(define (lineedit-key-tab ctx)
  (let ((completions (linectx-completions ctx))
        (func (linectx-completion-func ctx)))
    (when func
      ; protect against exceptions in linectx-completion-func
      (try (func ctx)
        (catch (cond)
          (span-clear! completions)))
      (when (fx=? 1 (span-length completions))
        (let* ((completion (span-ref completions 0))
               (stem-len (charspan-length (linectx-completion-stem ctx)))
               (len (fx- (charspan-length completion) stem-len)))
          (when (fx>? len 0)
            (linectx-csp-insert! ctx completion stem-len len)))))))

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
      ; also saves a copy of current linectx-lines to history
      (lineedit-lines-set! ctx (charhistory-cow-ref hist y))
      (linectx-history-index-set! ctx y))))


(define (lineedit-keytable-set! keytable proc . keysequences)
  (letrec
    ((%add-bytelist (lambda (htable bytelist)
      (let ((byte (car bytelist)))
        (if (null? (cdr bytelist))
          (hashtable-set! htable byte proc)
          (let ((inner-htable (hashtable-ref htable byte #f)))
            (unless (hashtable? inner-htable)
              (set! inner-htable (eq-hashtable))
              (hashtable-set! htable byte inner-htable))
            (%add-bytelist inner-htable (cdr bytelist)))))))
     (%any->bytelist (lambda (keyseq)
       (cond
         ((fixnum?     keyseq) (list keyseq))
         ((pair?       keyseq) keyseq)
         ((bytevector? keyseq) (bytevector->u8-list keyseq))
         ((string?     keyseq) (bytevector->u8-list (string->utf8 keyseq)))
         (#t (assert
               (or (fixnum? keyseq) (pair? keyseq)
                   (bytevector? keyseq) (string? keyseq))))))))
    (do ((l keysequences (cdr l)))
        ((null? l))
      (%add-bytelist keytable (%any->bytelist (car l))))))

(define (lineedit-keytable-find keytable rbuf)
  (assert (bytespan? rbuf))
  (let %find ((htable keytable)
              (rpos 0))
    (if (fx>=? rpos (bytespan-length rbuf))
      (values htable rpos)
      (let* ((ch (bytespan-u8-ref rbuf rpos))
             (entry (hashtable-ref htable ch #f))
             (rpos+1 (fx1+ rpos)))
        (cond
          ((procedure? entry) (values entry rpos+1))
          ((hashtable? entry) (%find  entry rpos+1))
          (#t                 (values #f    (bytespan-length rbuf))))))))

; find one key sequence in lineedit-keytable matching rbuf and execute it
(define (linectx-keytable-call ctx)
  (assert (linectx? ctx))
  (let-values (((proc n) (lineedit-keytable-find
                           (linectx-keytable ctx) (linectx-rbuf ctx))))
    (cond
      ((procedure? proc) (proc ctx))
      ((hashtable? proc) (set! n 0)) ; incomplete sequence, wait for more keystrokes
      (#t                (set! n (linectx-rbuf-insert! ctx n))))
    (let ((rbuf (linectx-rbuf ctx)))
      (bytespan-erase-front! rbuf n)
      (when (bytespan-empty? rbuf)
        (bytespan-clear! rbuf))) ; set begin, end to 0
    n))

; append linectx-lines to history, and return them.
; the returned charlines MUST NOT be modified, not even temporarily,
; because linectx-history still references it.
(define (linectx-return-lines ctx)
  (linectx-return-set! ctx #f)    ; clear flag "user pressed ENTER"
  (linectx-redisplay-set! ctx #t) ; set flag "redisplay prompt and lines"
  (let* ((y (linectx-history-index ctx))
         (hist (linectx-history ctx))
         (hist-len (charhistory-length hist)))
    ; always overwrite last history slot
    (linectx-history-index-set! ctx (fxmax 0 y (fx1- hist-len)))
    (let* ((lines (linectx-to-history ctx))
           (empty-line (charline)))
      (linectx-history-index-set! ctx (charhistory-length hist))
      ; lines are referenced by history - allocate new ones
      (linectx-line-set! ctx empty-line)
      (linectx-lines-set! ctx (charlines empty-line))
      (linectx-x-set! ctx 0)
      (linectx-y-set! ctx 0)
      lines)))

; repeatedly call (linectx-keytable-call) until ENTER is found and processed,
; or until no more keytable matches are found.
; if user pressed ENTER, return a reference to internal charlines (linectx-lines)
; if waiting for more keypresses, return #t
; if got end-of-file, return #f
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

; if tty size changed, redraw
(define (linectx-reflow ctx width height)
  ; TODO: reflow lines, update x and y
  (unless (and (fx=? width (linectx-width ctx))
               (fx=? height (linectx-height ctx)))
    (linectx-width-set! ctx width)
    (linectx-height-set! ctx height)
    (lineedit-key-redraw ctx)))

; react to SIGWINCH
(define (linectx-consume-sigwinch ctx)
  (when (signal-consume-sigwinch)
    (let ((sz (tty-size)))
      (when (pair? sz)
        (linectx-reflow ctx (car sz) (cdr sz))))))

; unconditionally display prompt
(define (linectx-display-prompt ctx)
  (let ((prompt (linectx-prompt ctx)))
    ; (format #t "linectx-display-prompt: prompt = ~s~%" prompt)
    (linectx-bsp-write ctx prompt 0 (bytespan-length prompt))))

; unconditionally display lines
(define (linectx-display-lines ctx)
  (let* ((lines (linectx-lines ctx))
         (lines-n-1 (fx1- (charlines-length lines)))
         (nl? #f))
    (charlines-iterate lines
      (lambda (i line)
        (when nl?
          (term-clear-to-eol ctx)
          (linectx-u8-write ctx 10))
        (linectx-cgb-write ctx line 0 (charline-length line))
        (set! nl? #t)))
    (term-clear-to-eos ctx)
    (linectx-move-from ctx
      (charline-length (charlines-ref lines lines-n-1))
      lines-n-1)))

; unconditionally display prompt and lines
(define (linectx-display ctx)
  (linectx-display-prompt ctx)
  (linectx-display-lines  ctx))

(define bv-prompt-error (string->utf8 "error expanding prompt $ "))

; if needed, display new prompt and lines
(define (linectx-display-if-needed ctx force?)
  (when (or force? (linectx-redisplay? ctx))
    (let ((prompt (linectx-prompt ctx)))
      (assert (bytespan? prompt))
      (try ((linectx-prompt-func ctx) ctx)
        (catch (cond)
          (bytespan-clear! prompt)
          (let ((err-len (bytevector-length bv-prompt-error)))
            (bytespan-bv-insert-back! prompt bv-prompt-error 0 err-len)
            (linectx-prompt-length-set! ctx err-len))))
      (let ((prompt-length (linectx-prompt-length ctx)))
        (assert (fx<=? 0 prompt-length (bytespan-length prompt)))
        (let-values (((y x) (fxdiv-and-mod prompt-length (linectx-width ctx))))
          (when (and (fxzero? x) (not (fxzero? y)))
            ; prompt actually ends at rightmost column
            (set! x (linectx-width ctx))
            (set! y (fx1- y)))
          (linectx-prompt-end-x-set! ctx x)
          (linectx-prompt-end-y-set! ctx y))))
    (linectx-display ctx)
    (linectx-redisplay-set! ctx #f)))

; read some bytes, blocking at most for read-timeout-milliseconds
;   (0 = non-blocking, -1 = unlimited timeout)
; from (linectx-stdin ctx) and append them to (linectx-rbuf ctx).
; return number of read bytes.
; return 0 on timeout
; return -1 on eof
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

; invoked when some function called by lineedit-read raises a condition
(define (%lineedit-error ctx cond)
  ; remove offending input that triggered the condition
  (bytespan-clear! (linectx-rbuf ctx))
  ; display the condition
  (let ((port (current-output-port)))
    (display #\newline port)
    (display "Exception in lineedit-read: " port)
    (display-condition cond port)
    (display #\newline port)))

; implementation of (lineedit-read )
(define (%lineedit-read ctx timeout-milliseconds)
  (linectx-consume-sigwinch ctx)
  (linectx-display-if-needed ctx #f)
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

; Main entry point of lineedit library.
; Reads user input from linectx-stdin and processes it.
;
; if user pressed ENTER, return a reference to internal linectx-lines.
; if waiting for more keypresses, return #t
; if got end-of-file, return #f
(define (lineedit-read ctx timeout-milliseconds)
  (try
    (begin
      ; write current-output-port buffered output before entering read loop
      (flush-output-port (current-output-port))
      (let ((ret (%lineedit-read ctx timeout-milliseconds)))
        ; write linectx buffered output before returning
        (lineedit-flush ctx)
        ret))
    (catch (cond)
      (%lineedit-error ctx cond)
      #t))) ; return "waiting for more keypresses"

(let ((t lineedit-default-keytable)
      (%add lineedit-keytable-set!))
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

; customize how "linectx" objects are printed
(record-writer (record-type-descriptor linectx)
  (lambda (ctx port writer)
    (display "#<linectx " port)
    (display (linectx-parser-name ctx) port)
    (display ">" port)))

) ; close library