;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit (0 1))
  (export
    make-linectx make-linectx* linectx? linectx-line linectx-lines
    linectx-x linectx-y
    linectx-parenmatcher
    linectx-completions linectx-completion-stem
    linectx-parser-name linectx-parser-name-set!
    linectx-parsers     linectx-parsers-set!
    linectx-prompt linectx-prompt-length linectx-prompt-length-set!

    lineedit-default-keytable lineedit-clear!
    lineedit-lines-set! linectx-stdin-set! linectx-stdout-set! linectx-insert/rbuf!
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
    (schemesh lineedit charlines)
    (schemesh lineedit charhistory)
    (schemesh lineedit parens)
    (schemesh lineedit parenmatcher)
    (only (schemesh lineedit parser) make-parsectx*)
    (only (schemesh lineedit io) open-charlines-input-port)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))


;; linectx is the top-level object used by most lineedit functions
(define-record-type
  (linectx %make-linectx linectx?)
  (fields
    (mutable rbuf)    ; bytespan, buffer for (fd-read)
    (mutable wbuf)    ; bytespan, buffer for (fd-write)
    (mutable line)    ; charline, input's current line being edited
    (mutable lines)   ; charlines, input being edited
    (mutable x)       ; fixnum, cursor x position in line
    (mutable y)       ; fixnum, cursor y position in lines
    (mutable rows)    ; fixnum, max number of rows being edited
    (mutable width)   ; fixnum, terminal width
    (mutable height)  ; fixnum, terminal height
    (mutable stdin)   ; input file descriptor, or binary input port
    (mutable stdout)  ; output file descriptor, or binary output port
    (mutable read-timeout-milliseconds) ; -1 means unlimited timeout
    ; bitwise or of: flag-eof? flag-return? flag-sigwinch? flag-redraw?
    (mutable flags)
    (mutable parser-name)   ; symbol, name of current parser
    (mutable parsers)       ; #f or hashtable symbol -> parser, table of enabled parsers
    prompt                  ; bytespan, prompt
    (mutable prompt-end-x)  ; fixnum, tty x column where prompt ends
    (mutable prompt-end-y)  ; fixnum, tty y row where prompt ends
    (mutable prompt-length) ; fixnum, prompt draw length
    ; procedure, receives linectx as argument and should update prompt and prompt-length
    (mutable prompt-func)
    parenmatcher
    (mutable parens) ;        #f or parens containing matching parentheses
    completions      ;        span of charspans, possible completions
    completion-stem  ;        charspan, chars from lines used as stem
    ; procedure, receives linectx as argument and should update completions and stem
    (mutable completion-func)
    (mutable keytable)      ; hashtable, contains keybindings
    (mutable history-index) ; index of last used item in history
    history))        ; charhistory, history of entered commands

(define flag-eof? 1)
(define flag-return? 2)
(define flag-sigwinch? 4)
(define flag-redraw? 8)

(define (linectx-flag? ctx bit)
  (not (fxzero? (fxand bit (linectx-flags ctx)))))

(define (linectx-flag-set! ctx bit flag?)
  (assert (boolean? flag?))
  (let ((flags (linectx-flags ctx)))
    (linectx-flags-set! ctx
      (if flag?
        (fxior flags bit)
        (fxand flags (fxnot bit))))))
;;   (format #t "linectx-flag-set! ~s -> ~s~%" flags (linectx-flags ctx))))

(define (linectx-eof? ctx)
  (linectx-flag? ctx flag-eof?))
(define (linectx-return? ctx)
  (linectx-flag? ctx flag-return?))
(define (linectx-sigwinch? ctx)
  (linectx-flag? ctx flag-sigwinch?))
(define (linectx-redraw? ctx)
  (linectx-flag? ctx flag-redraw?))

(define (linectx-eof-set! ctx flag?)
  (linectx-flag-set! ctx flag-eof? flag?))
(define (linectx-return-set! ctx flag?)
  (linectx-flag-set! ctx flag-return? flag?))
(define (linectx-sigwinch-set! ctx flag?)
  (linectx-flag-set! ctx flag-sigwinch? flag?))
(define (linectx-redraw-set! ctx flag?)
  (linectx-flag-set! ctx flag-redraw? flag?))


(define lineedit-default-keytable (eq-hashtable))

;; Variant of (make-linectx) where all arguments are mandatory
;;
;; argument prompt-func must be a procedure accepting linectx,
;; and updating linectx-prompt and linectx-prompt-length.
;;
;; argument parenmatcher must be #f or a parenmatcher
;;
;; argument prompt-func must be #f or a procedure accepting linectx,
;; and updating linectx-completions
(define (make-linectx* prompt-func parenmatcher completion-func enabled-parsers)
  (assert (procedure? prompt-func))
  (when parenmatcher
    (assert (parenmatcher? parenmatcher)))
  (when completion-func
    (assert (procedure? completion-func)))
  (when enabled-parsers
    (assert (hashtable? enabled-parsers)))
  (let* ((sz    (tty-size))
         (rbuf  (bytespan))
         (wbuf  (bytespan))
         (line  (charline))
         (lines (charlines line)))
    (bytespan-reserve-back! rbuf 1024)
    (bytespan-reserve-back! wbuf 1024)
    (%make-linectx
      rbuf wbuf line lines
      0 0 1                      ; x y rows
      (if (pair? sz) (car sz) 80); width
      (if (pair? sz) (cdr sz) 24); height
      0 1 -1 flag-redraw?        ; stdin stdout read-timeout flags
      'shell enabled-parsers     ; parser-name parsers
      (bytespan) 0 0             ; prompt prompt-end-x prompt-end-y
      0 prompt-func              ; prompt-length prompt-func
      parenmatcher #f            ; parenmatcher parens
      (span) (charspan) completion-func ; completions stem completion-func
      lineedit-default-keytable  ; keytable
      0 (charhistory))))         ; history

(define (default-prompt-func ctx)
  (let* ((str    (symbol->string (linectx-parser-name ctx)))
         (bv     (string->utf8 str))
         (prompt (linectx-prompt ctx)))
    (bytespan-clear! prompt)
    (bytespan-insert-back/bvector! prompt bv 0 (bytevector-length bv))
    ; append colon and space after parser name
    (bytespan-insert-back/u8! prompt 58 32)
    (linectx-prompt-length-set! ctx (fx+ 2 (string-length str)))))


;; Create and return a linectx
(define make-linectx
  (case-lambda
    (()
       (make-linectx* default-prompt-func #f #f #f))
    ((prompt-func)
       (make-linectx* prompt-func #f #f #f))
    ((prompt-func parenmatcher)
       (make-linectx* prompt-func parenmatcher #f #f))
    ((prompt-func parenmatcher completion-func)
       (make-linectx* prompt-func parenmatcher completion-func #f))
    ((prompt-func parenmatcher completion-func enabled-parsers)
       (make-linectx* prompt-func parenmatcher completion-func enabled-parsers))))


;; Clear and recreate line and lines: they may have been saved to history,
;; which retains them.
;; Does NOT write anything to the tty
(define (linectx-clear! ctx)
  (linectx-x-set! ctx 0)
  (linectx-y-set! ctx 0)
  (linectx-parens-set! ctx #f)
  (linectx-return-set! ctx #f)
  (let ((line (charline)))
    (linectx-line-set!  ctx line)
    (linectx-lines-set! ctx (charlines line))))


(define (linectx-lines-changed ctx)
  (parenmatcher-clear! (linectx-parenmatcher ctx)))


;; write a byte to wbuf
(define (term-write/u8 ctx u8)
  (bytespan-insert-back/u8! (linectx-wbuf ctx) u8))

;; write a portion of given bytevector to wbuf
(define (term-write/bvector ctx bv start n)
  (bytespan-insert-back/bvector! (linectx-wbuf ctx) bv start n))

;; write a portion of given bytespan to wbuf
(define (term-write/bspan ctx bsp start n)
  (bytespan-insert-back/bspan! (linectx-wbuf ctx) bsp start n))

;; write a portion of given chargbuffer to wbuf
(define (term-write/cbuffer ctx cgb start end)
  (do ((wbuf (linectx-wbuf ctx))
       (pos start (fx1+ pos)))
      ((fx>=? pos end))
    (bytespan-utf8-insert-back! wbuf (chargbuffer-ref cgb pos))))

;; return a copy-on-write clone of current lines being edited
(define (linectx-lines-copy ctx)
  ;; (format #t "linectx-lines-copy~%")
  ;; (dynamic-wind tty-restore! break tty-setraw!)
  (charlines-copy-on-write (linectx-lines ctx)))

;; save current linectx-lines to history, and return them
(define (linectx-to-history ctx)
  ;; TODO: do not insert duplicates in history
  (let ((lines (linectx-lines ctx)))
    (charhistory-set! (linectx-history ctx) (linectx-history-index ctx) lines)
    lines))

;; Move tty cursor vertically.
;; If dy > 0, send escape sequence "move cursor down by dy".
;; If dy < 0, send escape sequence "move cursor up by -dy".
;; Does not check or update linectx.

(define (term-move-dy ctx dy)
  (cond
    ((fxzero? dy) ; do nothing
      (void))
    ((fx=? dy 1)  ; move down by 1
      (term-write/bvector ctx #vu8(27 91 66) 0 3))  ; ESC [ B
    ((fx=? dy -1) ; move up by 1
      (term-write/bvector ctx #vu8(27 91 65) 0 3))  ; ESC [ A
    ((fx>? dy 1) ; move down by dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-insert-back/u8! wbuf 27 91)     ; ESC [
        (bytespan-fixnum-display-back! wbuf dy)   ; n
        (bytespan-insert-back/u8! wbuf 66)))      ; B
    ((fx<? dy -1) ; move up by -dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-insert-back/u8! wbuf 27 91)     ; ESC [
        (bytespan-fixnum-display-back! wbuf (fx- dy)) ; n
        (bytespan-insert-back/u8! wbuf 65)))))    ; A

;; Move tty cursor horizontally.
;; If dx > 0, send escape sequence "move cursor right by dx".
;; If dx < 0, send escape sequence "move cursor left by -dx".
;; Does not check or update linectx.
(define (term-move-dx ctx dx)
  (cond
    ((fxzero? dx) ; do nothing             ;
      (void))                                  ;
    ((fx=? dx 1) ; move right by 1         ;
      (term-write/bvector ctx #vu8(27 91 67) 0 3))      ; ESC [ C
    ((fx=? dx -1) ; move left by 1         ;
      (term-write/bvector ctx #vu8(27 91 68) 0 3))      ; ESC [ D
    ((fx>? dx 1) ; move right by n         ;
      (let ((wbuf (linectx-wbuf ctx)))         ;
        (bytespan-insert-back/u8! wbuf 27 91)  ; ESC [
        (bytespan-fixnum-display-back! wbuf dx); n
        (bytespan-insert-back/u8! wbuf 67)))   ; C
    ((fx<? dx -1) ; move left by -dx       ;
      (let ((wbuf (linectx-wbuf ctx)))         ;
        (bytespan-insert-back/u8! wbuf 27 91)  ; ESC [
        (bytespan-fixnum-display-back! wbuf (fx- dx)) ; n
        (bytespan-insert-back/u8! wbuf 68))))) ; D

;; move tty cursor from its current position at from-x, from-y
;; back to linectx-x, linectx-y
(define (linectx-move-from ctx from-x from-y)
  (linectx-move ctx from-x from-y (linectx-x ctx) (linectx-y ctx)))

;; move tty cursor from its current position at linectx-x linectx-y
;; to specified position to-x to-y
(define (linectx-move-to ctx to-x to-y)
  (linectx-move ctx (linectx-x ctx) (linectx-y ctx) to-x to-y))

;; move tty cursor from position from-x from-y to position to-x to-y
(define (linectx-move ctx from-x from-y to-x to-y)
  (let ((prompt-x (linectx-prompt-end-x ctx)))
    ; we may move from/to first line, which is prefixed by the prompt:
    ; adjust from-x and to-x
    (when (fxzero? from-y)
      (set! from-x (fx+ from-x prompt-x)))
    (when (fxzero? to-y)
      (set! to-x (fx+ to-x prompt-x)))
    (term-move-dy ctx (fx- to-y from-y))
    (term-move-dx ctx (fx- to-x from-x))))

;; if current x position is at tty right border or exceeds it, move to next line
(define (linectx-xy-wraparound ctx)
  (let* ((x (linectx-x ctx))
         (y (linectx-y ctx))
         (prompt-len (if (fxzero? y) (linectx-prompt-length ctx) 0)))
    ;; TODO: implement
    #f))


;; return absolute x y position corresponding to relative dx dy position,
;; clamping returned values to current charlines
(define (linectx-dxdy->xy ctx dx dy)
  (let* ((x (fx+ dx (linectx-x ctx)))
     (y (fx+ dy (linectx-y ctx)))
     (lines (linectx-lines ctx))
     (last-y (fx1- (charlines-length lines))))
    (cond
     ((fx<? x 0)
      ;; move to previous line
      (set! x (greatest-fixnum))
      (set! y (fx1- y)))
     ((and (fx<? -1 y last-y)
       (fx>? x (charline-length (charlines-ref lines y))))
      ;; move to next line
      (set! x 0)
      (set! y (fx1+ y))))
    (set! y (fxmax 0 (fxmin y last-y)))
    (set! x (fxmax 0 (fxmin x (charline-length (charlines-ref lines y)))))
    (values x y)))


;; send escape sequence "delete n chars at right", without checking or updating linectx
(define (term-del-right-n ctx n)
  (cond
    ((fx<=? n 0) (void)) ; nop
    ((fx=? n 1)
      (term-write/bvector ctx #vu8(27 91 80) 0 3)) ; VT102 sequence: ESC [ P
    (#t
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-insert-back/u8! wbuf 27 91)   ; ESC [
        (bytespan-fixnum-display-back! wbuf n)  ; n
        (bytespan-insert-back/u8! wbuf 80)))))  ; P

;; send escape sequence "move to begin-of-line". Moves at beginning of prompt!
(define (term-move-to-bol ctx)
  (term-write/u8 ctx 13)) ; CTRL+M i.e. '\r'

;; send escape sequence "clear from cursor to end-of-line"
(define (term-clear-to-eol ctx)
  (term-write/bvector ctx #vu8(27 91 75) 0 3)) ; ESC [ K

;; send escape sequence "clear from cursor to end-of-screen"
(define (term-clear-to-eos ctx)
  (term-write/bvector ctx #vu8(27 91 74) 0 3)) ; ESC [ J


;; redraw from cursor to end of line.
;; clear-line-right must be either 'clear-line-right or 'dont-clear-line-right*/
(define (term-redraw-to-eol ctx clear-line-right)
  (let* ((line (linectx-line ctx))
         (beg  (linectx-x ctx))
         (end  (charline-length line)))
    (when (fx<? beg end)
      (term-write/cbuffer ctx line beg end))
    (when (eq? clear-line-right 'clear-line-right)
      (term-clear-to-eol ctx))
    (term-move-dx ctx (fx- beg end))))


;; redraw from cursor to end of screen.
(define (term-redraw-to-eos ctx)
  (term-redraw-to-eol ctx 'dont-clear-line-right)
  (let* ((lines (linectx-lines ctx))
         (lines-n-1 (fx1- (charlines-length lines))))
    (do ((y (fx1+ (linectx-y ctx)) (fx1+ y)))
        ((fx>? y lines-n-1))
      (let ((line (charlines-ref lines y)))
        (term-clear-to-eol ctx)
        (term-write/u8 ctx 10)
        (term-write/cbuffer ctx line 0 (charline-length line))))
    (term-clear-to-eos ctx)
    (linectx-move-from ctx
      (charline-length (charlines-ref lines lines-n-1))
      lines-n-1)))


;; return index of first character before position = end in line that satisfies (pred ch).
;; return -1 if no character before position = end in line satisfies (pred ch)
(define (char-find-left line end pred)
  (assert (fx<=? end (charline-length line)))
  (do ((x (fx1- end) (fx1- x)))
      ((or (fx<? x 0) (pred (charline-ref line x)))
        x)))

;; return index of first character at position = start or later in line that satisfies
;; (pred ch). return (charline-length line) if no character at position = start or later in
;; line satisfies (pred ch)
(define (char-find-right line start pred)
  (assert (fx>=? start 0))
  (do ((x start (fx1+ x))
       (len (charline-length line)))
      ((or (fx>=? x len) (pred (charline-ref line x)))
        x)))

;; return index of beginning of word at position < end in line
(define (word-find-begin-left line end)
  (let* ((pos1 (fx1+ (char-find-left line end  (lambda (ch) (char>? ch #\space)))))
         (pos2 (fx1+ (char-find-left line pos1 (lambda (ch) (char<=? ch #\space))))))
    pos2))

;; return index of end of word at position >= start in line
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


;; write #\newline before returning from (repl)
(define (lineedit-finish ctx)
  (term-write/u8 ctx 10)
  (lineedit-flush ctx))


;; save current linectx-lines to history, then replace current lines with specified
;; charlines - which are retained, do NOT modify them after calling this function
(define (lineedit-lines-set! ctx lines)
  (assert-charlines? 'lineedit-lines-set! lines)
  (linectx-to-history ctx)
  (lineedit-clear! ctx) ; leaves a single, empty line in lines
  (linectx-lines-changed ctx)
  (when (charlines-empty? lines)
    (set! lines (linectx-lines ctx)))
  (charlines-iterate lines
    (lambda (i line)
      (term-write/cbuffer ctx line 0 (charline-length line))))
  (let* ((lines-n-1 (fx1- (charlines-length lines)))
         (line      (charlines-ref lines lines-n-1)))
    (linectx-line-set!  ctx line)
    (linectx-lines-set! ctx lines)
    (linectx-x-set! ctx (charline-length line))
    (linectx-y-set! ctx lines-n-1)))


;; return number of excess characters of cursor position that overflow tty right border
;; return <= 0 if no excess characters
(define (linectx-cursor-excess ctx)
  (let* ((x (linectx-x ctx))
         (y (linectx-y ctx))
         (prompt-len (if (fxzero? y) (linectx-prompt-length ctx) 0)))
    (fx- (fx+ prompt-len x) (linectx-width ctx))))


;; return number of excess characters of y-th line that overflow tty right border
;; return <= 0 if no excess characters
(define (linectx-line-excess ctx y)
  (let* ((prompt-len (if (fxzero? y) (linectx-prompt-length ctx) 0))
         (line-len (charline-length (linectx-line ctx))))
    (fx- (fx+ prompt-len line-len) (linectx-width ctx))))


;; move the last n characters of y-th line to next line,
;; or to a new line if y-th line has nl? flag set.
;;
;; also move the cursor down if needed.
(define (linectx-line-overflow! ctx y n)
  (let* ((lines   (linectx-lines ctx))
         (lines-n (charlines-length lines))
         (y+1     (fx1+ y))
         (src     (charlines-ref lines y))
         (dst     (if (and (not (charline-nl? src)) (fx<? y+1 lines-n))
                    (charlines-ref lines y+1)
                    #f)))
    (assert (fx<=? 0 n (charline-length src)))
    ; create a new line if needed
    (unless dst
      (set! dst (charline))
      (when (charline-nl? src)
        (charline-nl?-set! src #f)
        (charline-nl?-set! dst #t))
      (charlines-insert-at! lines y+1 dst))
    (do ((i 0 (fx1+ i))
         (src-pos (fx1- (charline-length src)) (fx1- src-pos)))
        ((or (fx>=? i n) (fx<? src-pos 0))
         (set! n i))
      (let ((ch (charline-ref src src-pos)))
        (charline-erase-at! src src-pos 1)
        (charline-insert-at! dst 0 ch)))
    (when (fx=? y (linectx-y ctx))
      (let ((excess (linectx-cursor-excess ctx)))
        (when (fx>=? excess 0)
          (linectx-y-set! ctx y+1)
          (linectx-x-set! ctx excess)
          (linectx-line-set! ctx dst) ; advance lines to next line.
          (term-write/u8 ctx 10) ; advance cursor to next line.
          (term-move-dx ctx n))))))


;; move the last n characters of y-th input line to next line.
;; if next line becomes full, move the overflowing characters to the following line,
;; and so on until the last line
(define (linectx-lines-overflow! ctx y n)
  (linectx-line-overflow! ctx y n)
  (let* ((y+1 (fx1+ y))
         (excess (linectx-line-excess ctx y+1)))
    (when (fx>? excess 0)
      (linectx-lines-overflow! ctx y+1 excess))))


;; insert a single character into current line.
;;
;; return #t if insertion caused some character to move to the next line(s)
;; (in such case, caller must also invoke (term-redraw-to-eos ctx))
;;
;; otherwise return #f
;; (in such case, caller must also invoke (term-redraw-to-eol ctx 'dont-clear-line-right))
;;
;; in all cases, caller must also invoke (linectx-lines-changed ctx)
(define (%linectx-insert/char! ctx ch)
  (let ((x (linectx-x ctx))
        (y (linectx-y ctx)))
    (charline-insert-at! (linectx-line ctx) x ch)
    (bytespan-utf8-insert-back! (linectx-wbuf ctx) ch)
    (linectx-x-set! ctx (fx1+ x))
    (let* ((excess (linectx-line-excess ctx y))
           (overflow? (fx>=? excess 0)))
      (when overflow?
        (linectx-lines-overflow! ctx y excess))
      overflow?)))


;; consume n chars from charspan csp, starting at offset = start
;; and insert them into current line.
(define (linectx-insert/charspan! ctx csp start n)
  (assert (fx<=? 0 start (fx+ start n) (charspan-length csp)))
  (when (fx>? n 0)
    (let* ((beg   (fx+ start (charspan-peek-beg csp)))
           (end   (fx+ beg n))
           (overflow? #f))
      (do ((pos beg (fx1+ pos)))
          ((fx>=? pos end))
        (when (%linectx-insert/char! ctx (charspan-ref csp pos))
          (set! overflow? #t)))
      (linectx-lines-changed ctx)
      (if overflow?
        (term-redraw-to-eos ctx)
        (term-redraw-to-eol ctx 'dont-clear-line-right)))))


;; consume up to n bytes from bytespan bsp, starting at offset = start
;; and insert them into current line.
;; return number of bytes actually consumed
(define (linectx-insert/bspan! ctx bsp start n)
  (assert (fx<=? 0 start (fx+ start n) (bytespan-length bsp)))
  (let ((beg   start)
        (pos   start)
        (end   (fx+ start n))
        (inserted-some? #f)
        (incomplete-utf8? #f)
        (overflow? #f)) ; #t if inserting caused some chars to move to next line
    (do ()
        ((or incomplete-utf8?
             (fx>=? pos end)
             ; stop at any byte < 32, unless it's the first byte (which we skip)
             (and (fx>? pos beg) (fx<? (bytespan-ref/u8 bsp pos) 32))))
      (let-values (((ch len) (bytespan-utf8-ref bsp pos (fx- end pos))))
        (set! pos (fxmin end (fx+ pos len)))
        (cond
          ((eq? #t ch)
            (set! incomplete-utf8? #t))
          ((and (char? ch) (char>=? ch #\space))
            (when (%linectx-insert/char! ctx ch)
              (set! overflow? #t))
            (set! inserted-some? #t)))))
    (when inserted-some?
      (if overflow?
        (term-redraw-to-eos ctx)
        (term-redraw-to-eol ctx 'dont-clear-line-right))
      (linectx-lines-changed ctx))
    (fx- pos beg))) ; return number of bytes actually consumed

;; consume up to n bytes from rbuf and insert them into current line.
;; return number of bytes actually consumed
(define (linectx-insert/rbuf! ctx n)
  (linectx-insert/bspan! ctx (linectx-rbuf ctx) 0 n))


(define (lineedit-key-nop ctx)
  (void))

;; move cursor left by 1
(define (lineedit-key-left ctx)
  (let ((x (linectx-x ctx))
        (y (linectx-y ctx)))
    (cond
      ((fx>? x 0)
        (linectx-x-set! ctx (fx1- x))
        (term-move-dx ctx -1))
      ((fx>? y 0)
        (let* ((y-1 (fx1- y))
               (line (charlines-ref (linectx-lines ctx) y-1))
               (line-len (charline-length line))
               (x    (if (fx>? line-len 0) (fx1- line-len) 0)))
          (linectx-line-set! ctx line)
          (linectx-x-set!    ctx x)
          (linectx-y-set!    ctx y-1)
          (term-move-dy ctx -1)
          (term-move-dx ctx (if (fxzero? y-1)
                              (fx+ x (linectx-prompt-length ctx))
                              x)))))))


;; move cursor right by 1
(define (lineedit-key-right ctx)
  (let ((x (linectx-x ctx))
        (y (linectx-y ctx))
        (line-len (charline-length (linectx-line ctx))))
    (cond
      ((fx<? x line-len)
        (linectx-x-set! ctx (fx1+ x))
        (term-move-dx ctx 1))
      ((fx<? y (fx1- (charlines-length (linectx-lines ctx))))
        (let* ((y+1 (fx1+ y))
               (line (charlines-ref (linectx-lines ctx) y+1)))
          (linectx-line-set! ctx line)
          (linectx-x-set! ctx 0)
          (linectx-y-set! ctx y+1)
          (term-write/u8 ctx 10))))))


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
        (linectx-lines-changed ctx)
        (linectx-x-set! ctx (fx1+ x))))))

(define (lineedit-key-del-left ctx)
  (when (fx>? (linectx-x ctx) 0)
    (lineedit-key-left ctx)
    (lineedit-key-del-right ctx)
    (linectx-lines-changed ctx)))

(define (lineedit-key-del-right ctx)
  (let ((x    (linectx-x ctx))
        (line (linectx-line ctx)))
    (when (fx<? x (charline-length line))
      (charline-erase-at! line x 1)
      (linectx-lines-changed ctx)
      (term-del-right-n ctx 1))))

(define (lineedit-key-del-word-left ctx)
  (let* ((x     (linectx-x ctx))
         (line  (linectx-line ctx))
         (pos   (word-find-begin-left line x))
         (del-n (fx- x pos)))
    (when (fx>? del-n 0)
      (charline-erase-at! line pos del-n)
      (linectx-lines-changed ctx)
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
      (linectx-lines-changed ctx)
      (term-del-right-n ctx del-n))))

(define (lineedit-key-del-line ctx)
  (void))

(define (lineedit-key-del-line-left ctx)
  (let* ((x    (linectx-x ctx))
         (line (linectx-line ctx))
         (len  (charline-length line)))
    (when (and (fx>? x 0) (fx>? len 0))
      (charline-erase-at! line 0 x)
      (linectx-lines-changed ctx)
      (linectx-x-set! ctx 0)
      (term-move-dx ctx (fx- x))
      (term-redraw-to-eol ctx 'clear-line-right))))

(define (lineedit-key-del-line-right ctx)
  (let* ((x    (linectx-x ctx))
         (line (linectx-line ctx))
         (len  (charline-length line)))
    (when (fx<? x len)
      (charline-erase-at! line x (fx- len x))
      (linectx-lines-changed ctx)
      (term-clear-to-eol ctx))))

(define (lineedit-key-newline-left ctx)
  (void))

(define (lineedit-key-newline-right ctx)
  (void))

(define (lineedit-key-enter ctx)
  (linectx-return-set! ctx #t))

(define (lineedit-key-history-next ctx)
  (lineedit-navigate-history ctx +1)
  (linectx-lines-changed ctx))

(define (lineedit-key-history-prev ctx)
  (lineedit-navigate-history ctx -1)
  (linectx-lines-changed ctx))

(define (lineedit-key-redraw ctx)
  (term-move-to-bol ctx)
  (term-move-dy ctx (fx- (fx+ (linectx-y ctx) (linectx-prompt-end-y ctx))))
  (linectx-lines-changed ctx)
  (linectx-draw-if-needed ctx 'force))

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
            (linectx-insert/charspan! ctx completion stem-len len)
            (linectx-lines-changed ctx)))))))

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
      (let* ((ch (bytespan-ref/u8 rbuf rpos))
             (entry (hashtable-ref htable ch #f))
             (rpos+1 (fx1+ rpos)))
        (cond
          ((procedure? entry) (values entry rpos+1))
          ((hashtable? entry) (%find  entry rpos+1))
          (#t                 (values #f    (bytespan-length rbuf))))))))

;; find one key sequence in lineedit-keytable matching rbuf and execute it
(define (linectx-keytable-call ctx)
  (assert (linectx? ctx))
  (let-values (((proc n) (lineedit-keytable-find
                           (linectx-keytable ctx) (linectx-rbuf ctx))))
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

;; append linectx-lines to history, and return them.
;; the returned charlines MUST NOT be modified, not even temporarily,
;; because linectx-history still references it.
(define (linectx-return-lines ctx)
  (linectx-return-set! ctx #f) ; clear flag "user pressed ENTER"
  (linectx-redraw-set! ctx #t) ; set flag "redraw prompt and lines"
  (linectx-draw-parens ctx (linectx-parens ctx) 'plain) ; unhighlight parentheses
  (term-move-dy ctx (fx- (charlines-length (linectx-lines ctx))
                         (linectx-y ctx))) ; move to last input line
  (term-write/u8 ctx 10) ; advance to next line.
  (let* ((y (linectx-history-index ctx))
         (hist (linectx-history ctx))
         (hist-len (charhistory-length hist)))
    ; always overwrite last history slot
    (linectx-history-index-set! ctx (fxmax 0 y (fx1- hist-len)))
    (let* ((lines (linectx-to-history ctx))
           (empty-line (charline)))
      (linectx-history-index-set! ctx (charhistory-length hist))
      ; lines are referenced by history - allocate new ones and store them into linectx-lines
      (linectx-line-set! ctx empty-line)
      (linectx-lines-set! ctx (charlines empty-line))
      (linectx-lines-changed ctx)
      (linectx-x-set! ctx 0)
      (linectx-y-set! ctx 0)
      lines)))

;; repeatedly call (linectx-keytable-call) until ENTER is found and processed,
;; or until no more keytable matches are found.
;; if user pressed ENTER, return a reference to internal charlines (linectx-lines)
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

;; if tty size changed, redraw
(define (linectx-reflow ctx width height)
  ; TODO: reflow lines, update x and y
  (unless (and (fx=? width (linectx-width ctx))
               (fx=? height (linectx-height ctx)))
    (linectx-width-set! ctx width)
    (linectx-height-set! ctx height)
    (lineedit-key-redraw ctx)))

;; react to SIGWINCH
(define (linectx-consume-sigwinch ctx)
  (when (signal-consume-sigwinch)
    (let ((sz (tty-size)))
      (when (pair? sz)
        (linectx-reflow ctx (car sz) (cdr sz))))))

;; unconditionally draw prompt
(define (linectx-draw-prompt ctx)
  (let ((prompt (linectx-prompt ctx)))
    ;; (format #t "linectx-draw-prompt: prompt = ~s~%" prompt)
    (term-write/bspan ctx prompt 0 (bytespan-length prompt))))

;; unconditionally draw lines
(define (linectx-draw-lines ctx)
  (let* ((lines (linectx-lines ctx))
         (lines-n-1 (fx1- (charlines-length lines)))
         (nl? #f))
    (charlines-iterate lines
      (lambda (i line)
        (when nl?
          (term-clear-to-eol ctx)
          (term-write/u8 ctx 10))
        (term-write/cbuffer ctx line 0 (charline-length line))
        (set! nl? #t)))
    (term-clear-to-eos ctx)
    (linectx-move-from ctx
      (charline-length (charlines-ref lines lines-n-1))
      lines-n-1))
  (linectx-draw-parens ctx (linectx-parens ctx) 'highlight))

;; unconditionally draw prompt and lines
(define (linectx-draw ctx)
  (linectx-draw-prompt ctx)
  (linectx-draw-lines  ctx))

(define bv-prompt-error (string->utf8 "error expanding prompt $ "))

;; if needed, draw new prompt and lines
(define (linectx-draw-if-needed ctx force?)
  (when (or force? (linectx-redraw? ctx))
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
          (linectx-prompt-end-y-set! ctx y))))
    (linectx-draw ctx)
    (linectx-redraw-set! ctx #f)))


;; if position x y is inside linectx-lines, redraw char at x y with specified style.
;; used to (un)highlight parentheses
(define (linectx-draw-char-at ctx x y style)
  (let* ((lines (linectx-lines ctx))
         (line  (if (fx<? -1 y (charlines-length lines))
                  (charlines-ref lines y)
                  #f))
         (ch    (if (and line (fx<? -1 x (charline-length line)))
                  (charline-ref line x)
                  #f))
         (wbuf  (linectx-wbuf  ctx)))
    (when ch
      (linectx-move-to ctx x y)
      (when (eq? 'highlight style)
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 49 59 51 54 109) 0 7)) ; ESC[1;36m
      (bytespan-utf8-insert-back! wbuf ch)
      (when (eq? 'highlight style)
        (bytespan-insert-back/bvector! wbuf '#vu8(27 91 109) 0 3)) ; ESC[m
      (linectx-move-from ctx (fx1+ x) y))))


(define (linectx-draw-parens ctx parens style)
  ;; draw parens only if both start and end positions are valid
  (when (parens-valid? parens)
    (linectx-draw-char-at ctx (parens-start-x parens) (parens-start-y parens) style)
    (linectx-draw-char-at ctx (parens-end-x parens)   (parens-end-y parens)   style)))


(define (linectx-parens-find ctx)
  (let ((ret     #f)
        (parsers (linectx-parsers ctx))
        (parenmatcher (linectx-parenmatcher ctx)))
    (when (and parsers parenmatcher)
      (let-values (((x y) (linectx-dxdy->xy ctx -1 0)))
        (let* ((lines (linectx-lines ctx))
               (line  (charlines-ref lines y)))
          ;; (format #t "(linectx-parens-find) x = ~s, y = ~s~%" x y)
          (when (and (fx<? -1 x (charline-length line))
                     (is-parens-char? (charline-ref line x)))
            ;; (format #t "(linectx-parens-find) ch = ~s~%" (charline-ref line x))
            ;; protect against exceptions in linectx-completion-func
            (try
              (set! ret
                (parenmatcher-find-match
                  parenmatcher
                  (lambda () (make-parsectx* (open-charlines-input-port lines) parsers 0 0))
                  (linectx-parser-name ctx)
                  x y))
              (catch (cond)
                (let ((port (current-output-port)))
                  (display #\newline port)
                  (display "Exception in parenmatcher: " port)
                  (display-condition cond port)
                  (display #\newline port))))))))
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
    (display #\newline port)
    (display "Exception in lineedit-read: " port)
    (display-condition cond port)
    (display #\newline port))
  (dynamic-wind
    tty-restore!
    (lambda () (inspect cond))
    tty-setraw!))


;; implementation of (lineedit-read)
(define (%lineedit-read ctx timeout-milliseconds)
  (linectx-consume-sigwinch ctx)
  (linectx-draw-if-needed ctx #f)
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
;; if user pressed ENTER, return a reference to internal linectx-lines.
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

;; customize how "linectx" objects are printed
(record-writer (record-type-descriptor linectx)
  (lambda (ctx port writer)
    (display "#<linectx " port)
    (display (linectx-parser-name ctx) port)
    (display ">" port)))

) ; close library
