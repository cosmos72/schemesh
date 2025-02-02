;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit lineedit (0 7 1))
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
    lineedit-key-history-next lineedit-key-history-prev lineedit-key-insert-clipboard
    lineedit-key-redraw lineedit-key-tab lineedit-key-toggle-insert
    lineedit-paren-find/before-cursor lineedit-paren-find/surrounds-cursor
    lineedit-read lineedit-read-confirm-y-or-n? lineedit-flush lineedit-finish)
  (import
    (rnrs)
    (only (chezscheme)    console-output-port console-error-port
                          display-condition format fx1+ fx1- fx/ include inspect
                          make-time record-writer sleep top-level-value void)
    (schemesh bootstrap)
    (schemesh containers)
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
    (let-values (((proc n) (linectx-keytable-find (linectx-keytable lctx) rbuf)))
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
      (cond
        ((procedure? proc)
          ; call lineedit-key-... procedure after updating rbuf:
          ; it may need to read more keystrokes
          (proc lctx)
          (linectx-last-key-set! lctx proc))
        ((not (fxzero? n))
          (linectx-last-key-set! lctx #f)))
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
(define (linectx-insert/char! lctx ch)
  (vscreen-insert/char! (linectx-vscreen lctx) ch))


;; read chars in the range [start, end) from charspan csp,
;; and insert them into vscreen at cursor.
;; Also moves cursor (fx- end start) characters to the right, and reflows vscreen as needed.
(define (linectx-insert/cspan! lctx csp start end)
  (assert* 'linectx-insert/cspan! (fx<=? 0 start end (charspan-length csp)))
  (when (fx<? start end)
    (vscreen-insert/cspan! (linectx-vscreen lctx) csp start end)))


;; read up to n bytes from bytespan bsp, starting at offset = start,
;; assume they are utf-8, convert them to characters and insert them into vscreen at cursor.
;; stops at any byte < 32, unless it's the first byte (which is skipped).
;; Also stops at incomplete utf-8 sequences.
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
;; return number of bytes actually read from bytespan and inserted.
(define (linectx-insert/bspan! lctx bsp start end)
  (assert* 'linectx-insert/bspan! (fx<=? 0 start end (bytespan-length bsp)))
  (let ((pos start)
        (incomplete-utf8? #f))
    (do ()
        ((or incomplete-utf8?
             (fx>=? pos end)
             ; stop at any byte < 32, unless it's the first byte (which we skip)
             (and (fx>? pos start) (fx<? (bytespan-ref/u8 bsp pos) 32))))
      (let-values (((ch len) (bytespan-ref/char bsp pos (fx- end pos))))
        (set! pos (fxmin end (fx+ pos len)))
        (cond
          ((eq? #t ch)
            (set! incomplete-utf8? #t))
          ((and (char? ch) (char>=? ch #\space))
            (linectx-insert/char! lctx ch)))))
    (fx- pos start))) ; return number of bytes actually inserted

;; read up to n bytes from rbuf and insert them into current line.
;; return number of bytes actually read from rbuf and inserted
(define (lineedit-insert/rbuf! lctx n)
  (linectx-insert/bspan! lctx (linectx-rbuf lctx) 0 n))


(include "lineedit/linekeys.ss")
(include "lineedit/linedraw.ss")


;; if linectx-vscreen is empty, return a shallow copy of it.
;; otherwise, append a shallow copy of it to history, and return such copy.
;;
;; in either case, returned charlines must NOT be modified - not even temporarily -
;; because in one case they are the vscreen, and in the other case history references it.
(define (linectx-return-lines* lctx)
  ; also un-highlights bad parentheses and current parentheses
  (linectx-redraw-dirty lctx 'plain)
  (lineterm-move-dy lctx (fx- (fx1- (linectx-end-y lctx))
                             (linectx-iy lctx))) ; move to last input line
  (lineterm-write/u8 lctx 10) ; advance to next line.
  (linectx-term-xy-set! lctx 0 0) ; set tty cursor to 0 0
  (linectx-return-set! lctx #f) ; clear flag "user pressed ENTER"
  (linectx-redraw-set! lctx #t) ; set flag "redraw prompt and lines"
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
                (let ((port (console-output-port)))
                  (put-string port "\n; Exception in parenmatcher-find/at: ")
                  (display-condition ex port)
                  (newline port)
                  (flush-output-port port))))))))
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
            (let ((port (console-error-port)))
              (put-string port "\n; Exception in parenmatcher-find/surrounds: ")
              (display-condition ex port)
              (newline port)
              (flush-output-port port))))))
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
  (linectx-read-some lctx 1024 read-timeout-milliseconds))


;; read some bytes, blocking at most for read-timeout-milliseconds
;;   (0 = non-blocking, -1 = unlimited timeout)
;; from (linectx-stdin lctx) and append them to (linectx-rbuf lctx).
;; return number of read bytes, or 0 on timeout, or -1 on eof or I/O error.
(define (linectx-read-some lctx max-n read-timeout-milliseconds)
  (let* ((fd   (linectx-stdin lctx))
         (rbuf (linectx-rbuf lctx))
         (rlen (bytespan-length rbuf))
         (got  0)
         (eof? #f))
    (bytespan-reserve-back! rbuf (fx+ rlen max-n))
    (try
      (if (fixnum? fd)
        ; fd is a file descriptor -> call (fd-select) then (fd-read)
        (when (eq? 'read (fd-select fd 'read read-timeout-milliseconds))
          (let ((end (bytespan-peek-end rbuf)))
            ; fd-read raises exception on I/O errors
            (set! got (fd-read fd (bytespan-peek-data rbuf) end (fx+ end max-n))))
          (set! eof? (fxzero? got))) ; (fxzero? got) means end of file
        ; fd is a binary input port -> call (get-bytevector-n!)
        (let ((n (get-bytevector-n! fd (bytespan-peek-data rbuf)
                                       (bytespan-peek-end rbuf) max-n)))
          (when (fixnum? n)
            (set! got n)
            (set! eof? (fxzero? n))))) ; (fxzero? n) means end of file
      (catch (ex)
        (lineedit-show-error lctx "Fatal error, schemesh exiting" ex)
        (when (fxzero? got)
          (set! eof? #t))))
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
      (linectx-read-some lctx 1 -1))
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


;; invoked when some function called by lineedit-read raises a condition:
;;
;; display the condition on (console-error-port)
(define (lineedit-show-error lctx message ex)
  ; remove offending input that triggered the exception
  (bytespan-clear! (linectx-rbuf lctx))
  ; display the condition
  (let ((port (console-error-port)))
    (put-string port "\n; ")
    (put-string port message)
    (put-string port ": ")
    (display-condition ex port)
    (newline port)
    (flush-output-port port)))


;; actual implementation of (lineedit-read)
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
    (lambda () ; before body
      (flush-output-port (console-output-port))
      (linectx-consume-sigwinch lctx)
      (linectx-redraw-as-needed lctx)
      (lineedit-flush lctx))
    (lambda () ; body
      (%%lineedit-read lctx timeout-milliseconds))
    (lambda () ; after body
      (lineedit-flush lctx))))

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
      (lineedit-show-error lctx "Exception in lineedit-read" ex)
      ;; sleep 0.2 seconds, to rate-limit error messages
      (sleep (make-time 'time-duration 200000000 1))
      ;; assume error is recoverable, return "waiting for more keypresses"
      #t)))


(let ((t linectx-default-keytable)
      (%add linectx-keytable-insert!))
  (%add t lineedit-key-bol          1) ; CTRL+A
  (%add t lineedit-key-left         2) ; CTRL+B
  (%add t lineedit-key-break        3) ; CTRL+C
  (%add t lineedit-key-ctrl-d       4) ; CTRL+D
  (%add t lineedit-key-eol          5) ; CTRL+E
  (%add t lineedit-key-right        6) ; CTRL+F
  (%add t lineedit-key-del-left 8 127) ; CTRL+H, BACKSPACE
  (%add t lineedit-key-tab          9) ; CTRL+I, TAB
  (%add t lineedit-key-enter    10 13) ; CTRL+J or ENTER, CTRL+M
  (%add t lineedit-key-del-line-right   11) ; CTRL+K
  (%add t lineedit-key-redraw           12) ; CTRL+L
  (%add t lineedit-key-history-next     14) ; CTRL+N
  (%add t lineedit-key-newline-right    15) ; CTRL+O
  (%add t lineedit-key-history-prev     16) ; CTRL+P
  (%add t lineedit-key-transpose-char   20) ; CTRL+T
  (%add t lineedit-key-del-line-left    21) ; CTRL+U
  (%add t lineedit-key-insert-clipboard 25) ; CTRL+Y
  (%add t lineedit-key-inspect-linectx  28) ; CTRL+4, CTRL+BACKSPACE
  ; CTRL+W, CTRL+BACKSPACE, ALT+BACKSPACE
  (%add t lineedit-key-del-word-left 23 31 '(27 127))
  ; sequences starting with ESC
  (%add t lineedit-key-word-left      '(27 66) '(27 98))  ; ALT+B, ALT+b
  (%add t lineedit-key-del-word-right '(27 68) '(27 100)) ; ALT+D, ALT+d
  (%add t lineedit-key-word-right     '(27 70) '(27 102)) ; ALT+F, ALT+f
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
