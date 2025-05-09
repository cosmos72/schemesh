;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit lineedit (0 9 1))
  (export
    ;; linedraw.ss
    lineedit-undraw linectx-redraw-all

    ;; lineedit.ss
    linectx-read

    lineedit-clear!     lineedit-display-table
    lineedit-lines-set! lineedit-insert/rbuf!
    lineedit-key-autocomplete lineedit-key-nop
    lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down
    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol
    lineedit-key-break lineedit-key-clear lineedit-key-ctrl-d lineedit-key-transpose-char
    lineedit-key-del-left lineedit-key-del-right
    lineedit-key-del-word-left lineedit-key-del-word-right
    lineedit-key-del-line lineedit-key-del-line-left lineedit-key-del-line-right
    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right
    lineedit-key-history-next lineedit-key-history-prev lineedit-key-insert-clipboard
    lineedit-key-redraw lineedit-key-toggle-insert
    lineedit-paren-find/before-cursor lineedit-paren-find/surrounds-cursor
    lineedit-read lineedit-read-confirm-y-or-n? lineedit-flush)
  (import
    (rnrs)
    (only (chezscheme)    break-handler console-output-port console-error-port
                          debug-condition display-condition format fx1+ fx1- fx/ include
                          inspect parameterize record-writer sleep top-level-value void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (only (schemesh posix signal) countdown signal-consume-sigwinch)
    (schemesh posix tty)
    (schemesh screen vcell)
    (schemesh screen vcellspan)
    (schemesh screen vline)
    (schemesh screen vlines)
    (schemesh screen vlines io)
    (schemesh screen vscreen)
    (schemesh screen vhistory)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit linectx)
    (schemesh lineedit lineterm)
    (only (schemesh lineedit parser) make-parsectx*))



;; find one key sequence in linectx-keytable matching rbuf and execute it
(define (linectx-keytable-call lctx)
  (assert* 'linectx-keytable-call (linectx? lctx))
  (let ((rbuf (linectx-rbuf lctx)))
    (let-values (((proc n) (linectx-keytable-find (linectx-keytable lctx) rbuf)))
      ; (debugf "linectx-keytable-call consume ~s bytes, call ~s" n proc)
      (cond
        ((procedure? proc) (void))     ; proc called below, we update rbuf first
        ((hashtable? proc) (set! n 0)) ; incomplete sequence, wait for more keystrokes
        (else  ; insert received bytes into current line
          (set! n (lineedit-insert/rbuf! lctx n))))
      (unless (fxzero? n)
        (bytespan-delete-left! rbuf n)
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
  (linectx-flush lctx))

(define (lineedit-clear! lctx)
  (linectx-clear! lctx))




;; save current linectx-vscreen to history,
;; then replace them with specified vlines.
;; Sets vscreen cursor to 0 0.
(define (lineedit-lines-set! lctx lines)
  (assert-vlines? 'lineedit-lines-set! lines)
  (linectx-to-history* lctx)
  (vscreen-assign*! (linectx-vscreen lctx) lines))

;; insert a single character or cell into vscreen at cursor.
;; Also moves vscreen cursor one character to the right, and reflows vscreen as needed.
(define (linectx-insert/c! lctx c)
  (vscreen-insert/c! (linectx-vscreen lctx) c))


;; read up to n chars from charspan bsp, starting at offset = start
;; and insert them into vscreen at cursor.
;;
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
(define linectx-insert/charspan!
  (case-lambda
    ((lctx csp start end)
      (assert* 'linectx-insert/charspan! (fx<=?* 0 start end (charspan-length csp)))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (linectx-insert/c! lctx (charspan-ref csp i))))
    ((lctx bsp)
      (linectx-insert/bytespan! lctx bsp 0 (bytespan-length bsp)))))


;; read up to n bytes from bytespan bsp, starting at offset = start,
;; assume they are utf-8, convert them to characters and insert them into vscreen at cursor.
;; stops at any byte < 32, unless it's the first byte (which is skipped).
;; Also stops at incomplete utf-8 sequences.
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
;; return number of bytes actually read from bytespan and inserted.
(define linectx-insert/bytespan!
  (case-lambda
    ((lctx bsp start end)
      (assert* 'linectx-insert/bytespan! (fx<=?* 0 start end (bytespan-length bsp)))
      (let ((pos start)
            (incomplete-utf8? #f))
        (do ()
            ((or incomplete-utf8?
                 (fx>=? pos end)
                 ; stop at any byte < 32, unless it's the first byte (which we skip)
                 (and (fx>? pos start) (fx<? (bytespan-ref/u8 bsp pos) 32))))
          (let-values (((ch len) (bytespan-ref/char bsp pos end)))
            (set! pos (fxmin end (fx+ pos len)))
            (cond
              ((eq? #t ch)
                (set! incomplete-utf8? #t))
              ((and (char? ch) (char>=? ch #\space))
                (linectx-insert/c! lctx ch)))))
        (fx- pos start))) ; return number of bytes actually inserted
    ((lctx bsp)
      (linectx-insert/bytespan! lctx bsp 0 (bytespan-length bsp)))))


;; read up to n bytes from rbuf and insert them into current line.
;; return number of bytes actually read from rbuf and inserted
(define (lineedit-insert/rbuf! lctx n)
  (linectx-insert/bytespan! lctx (linectx-rbuf lctx) 0 n))


(include "lineedit/linekeys.ss")
(include "lineedit/linedraw.ss")


;; if linectx-vscreen is empty, return a shallow copy of it.
;; otherwise, append a shallow copy of it to history, and return such copy.
;;
;; in either case, returned vlines must NOT be modified - not even temporarily -
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
  (linectx-mark-not-bol-set! lctx #t) ; set flag "highlight if prompt is not at bol"
  (let* ((y (linectx-history-index lctx))
         (hist (linectx-history lctx)))
    ; always overwrite last history slot
    (linectx-history-index-set! lctx (fxmax 0 y (fx1- (vhistory-length hist))))
    (let* ((screen (linectx-vscreen lctx))
           (lines (linectx-to-history* lctx)))
      (vhistory-delete-empty-lines! hist (vhistory-length hist))
      (linectx-history-index-set! lctx (vhistory-length hist))
      (linectx-clear! lctx) ;; clear vscreen
      lines)))


;; repeatedly call (linectx-keytable-call) until ENTER is found and processed,
;; or until no more keytable matches are found.
;; if user pressed ENTER, return a reference to internal vlines (linectx-vscreen)
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
      ;; return end-of-file
      #f)
    (else
      ;; need more keypresses. caller will sleep, draw before sleeping
      (linectx-redraw-as-needed lctx)
      #t)))


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
                  (lambda () (make-parsectx* (open-vlines-input-port screen)
                                             parsers
                                             (vscreen-width screen)
                                             (vscreen-prompt-end-x screen)
                                             0
                                             0))
                  (linectx-parser-name lctx)
                  x y))
              (catch (ex)
                (let ((out (console-error-port)))
                  (put-string out "\n; Exception in parenmatcher-find/at: ")
                  (display-condition ex out)
                  (newline out)
                  (flush-output-port out))))))))
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
              (lambda () (make-parsectx* (open-vlines-input-port screen)
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
          (lambda () (make-parsectx* (open-vlines-input-port screen)
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
      (vlines-count (linectx-vscreen lctx) (greatest-fixnum) (greatest-fixnum)
        (lambda (cl) (char=? #\\ (vcell->char cl)))))
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


;; consume a single byte from (linectx-rbuf lctx) and return it.
;; returns #f on eof or I/O error.
;;
;; if (linectx-rbuf lctx) is empty, refills it first.
;;
(define (%linectx-read-consume/u8 lctx)
  ; (debugf "-> %linectx-read-consume/u8")
  (let ((rbuf (linectx-rbuf lctx)))
    (when (bytespan-empty? rbuf)
      (linectx-read-some lctx 1 -1))
    ; (debugf "<- %linectx-read-consume/u8 got=~s" (bytespan-length rbuf))
    (if (bytespan-empty? rbuf)
      #f
      (let ((u8 (bytespan-ref/u8 rbuf 0)))
        (bytespan-delete-left! rbuf 1)
        u8))))


;; consume bytes from (linectx-rbuf lctx) and refill it as needed,
;; until one of #\y #\n or eof is received.
;; return #t if #\y is received, otherwise return #f
(define (lineedit-read-confirm-y-or-n? lctx)
  (do ((byte-or-eof (%linectx-read-consume/u8 lctx) (%linectx-read-consume/u8 lctx)))
    ((memv byte-or-eof '(110 121 #f)) ; #\n #\y or eof
     (eqv? byte-or-eof 121))))


;; actual implementation of (lineedit-read)
(define (%lineedit-read lctx timeout-milliseconds)
  (linectx-consume-sigwinch lctx)

  (let ((ret (linectx-keytable-iterate lctx)))
    (cond
      ((eq? #t ret)
        ;; need more input.
        (let ((n (linectx-read lctx timeout-milliseconds)))
          (cond
            ((fx>? n 0)
              ; got some bytes, call again (linectx-keytable-iterate)
              (linectx-keytable-iterate lctx))
            ((fxzero? n)
               ; read interrupted or timed out, return #t
               (linectx-consume-sigwinch lctx)
               #t)
            (else
               ; end-of-file, return #f
               #f))))
      (else
        ;; propagate return value of first (linectx-keytable-iterate)
        ;;a (debugf "...skipped read lineedit-read rbuf=~s wbuf=~s flags=~s vscreen=~s ret=~s" (linectx-rbuf lctx) (linectx-wbuf lctx) (linectx-flags lctx) (linectx-vscreen lctx) ret)
        ret))))


;; Main entry point of lineedit library.
;; Reads user input from linectx-stdin and processes it.
;;
;; if user pressed ENTER, return a reference to internal linectx-vscreen.
;; if waiting for more keypresses, return #t
;; if got end-of-file, return #f
(define (lineedit-read lctx timeout-milliseconds)
  (try
    (parameterize ((break-handler nop))
      (let ((ret (%lineedit-read lctx timeout-milliseconds)))
        (linectx-flush lctx)
        ret))
    (catch (ex)
      (debug-condition ex)
      (linectx-show-error lctx "Exception in lineedit-read" ex)
      ;; sleep 0.2 seconds, to rate-limit error messages
      (countdown '(0 . 200000000))
      ;; assume error is recoverable, return "waiting for more keypresses"
      #t)))


(let ((t linectx-default-keytable)
      (%add linectx-keytable-insert!))
  (%add t lineedit-key-bof          1) ; CTRL+A
  (%add t lineedit-key-left         2) ; CTRL+B
  (%add t lineedit-key-break        3) ; CTRL+C
  (%add t lineedit-key-ctrl-d       4) ; CTRL+D
  (%add t lineedit-key-eof          5) ; CTRL+E
  (%add t lineedit-key-right        6) ; CTRL+F
  (%add t lineedit-key-del-left 8 127) ; CTRL+H, BACKSPACE
  (%add t lineedit-key-autocomplete 9) ; CTRL+I, TAB
  (%add t lineedit-key-enter    10 13) ; CTRL+J or ENTER, CTRL+M
  (%add t lineedit-key-del-line-right   11) ; CTRL+K
  (%add t lineedit-key-redraw           12) ; CTRL+L
  (%add t lineedit-key-history-next     14) ; CTRL+N
  (%add t lineedit-key-newline-right    15) ; CTRL+O
  (%add t lineedit-key-history-prev     16) ; CTRL+P
  (%add t lineedit-key-transpose-char   20) ; CTRL+T
  (%add t lineedit-key-del-line-left    21) ; CTRL+U
  (%add t lineedit-key-insert-clipboard 25) ; CTRL+Y
  (%add t lineedit-key-inspect-linectx  28) ; CTRL+4, CTRL+BACKSLASH
  ; CTRL+W, CTRL+BACKSPACE, ALT+BACKSPACE
  (%add t lineedit-key-del-word-left 23 31 '(27 127))
  ; sequences starting with ESC
  (%add t lineedit-key-word-left      '(27 66) '(27 98))  ; ALT+B, ALT+b
  (%add t lineedit-key-del-word-right '(27 68) '(27 100)) ; ALT+D, ALT+d
  (%add t lineedit-key-word-right     '(27 70) '(27 102)) ; ALT+F, ALT+f
  ; sequences starting with ESC O
  (%add t lineedit-key-up    '(27 79 65))          ; UP    \eOA
  (%add t lineedit-key-down  '(27 79 66))          ; DOWN  \eOB
  (%add t lineedit-key-right '(27 79 67))          ; RIGHT \eOC
  (%add t lineedit-key-left  '(27 79 68))          ; LEFT  \eOD
  (%add t lineedit-key-eol   '(27 79 70))          ; END   \eOF
  (%add t lineedit-key-bol   '(27 79 72))          ; HOME  \eOH
  (%add t lineedit-key-newline-left   '(27 79 77)) ; KPRET \eOM
  (%add t lineedit-key-nop            '(27 79 80)) ; NUMLOCK \eOP ; F1 on MacOSX
  (%add t lineedit-key-cmd-cd-parent  '(27 79 81)) ; KP/   \eOQ   ; F2 on MacOSX
  (%add t lineedit-key-cmd-ls         '(27 79 82)) ; KP*   \eOR   ; F3 on MacOSX
  (%add t lineedit-key-cmd-cd-old-dir '(27 79 83)) ; KP-   \eOS   ; F4 on MacOSx
  (%add t lineedit-key-nop            '(27 79 108)); KP+   \eOl
  (%add t lineedit-key-cmd-cd-old-dir '(27 79 109)); KP-   \eOm

  (%add t lineedit-key-nop   '(27 79 110) '(27 79 112) ; KP.  KP0
    '(27 79 113) '(27 79 114) '(27 79 115)             ; KP1  KP2  KP3
    '(27 79 116) '(27 79 117) '(27 79 118)             ; KP4  KP5  KP6
    '(27 79 119) '(27 79 120) '(27 79 121))            ; KP7  KP8  KP9

  ; sequences starting with ESC [                  ;
  (%add t lineedit-key-up    '(27 91 65))          ; UP    \e[A
  (%add t lineedit-key-down  '(27 91 66))          ; DOWN  \e[B
  (%add t lineedit-key-right '(27 91 67))          ; RIGHT \e[C
  (%add t lineedit-key-left  '(27 91 68))          ; LEFT  \e[D
  (%add t lineedit-key-eol   '(27 91 70))          ; END   \e[F
  (%add t lineedit-key-bol   '(27 91 72)           ; HOME  \e[H
                                     '(27 91 49 126)) ; HOME   \e[1~
  (%add t lineedit-key-toggle-insert '(27 91 50 126)) ; INSERT \e[2~
  (%add t lineedit-key-del-right     '(27 91 51 126)) ; DELETE \e[3~
  (%add t lineedit-key-eol           '(27 91 52 126)) ; END    \e[4~
  (%add t lineedit-key-history-prev  '(27 91 53 126)) ; PGUP   \e[5~
  (%add t lineedit-key-history-next  '(27 91 54 126)) ; PGDWN  \e[6~

  (%add t lineedit-key-nop '(27 91 91 65) '(27 91 91 66)      ; F1..F2
    '(27 91 91 67)     '(27 91 91 68)     '(27 91 91 69)      ; F3..F4
    '(27 91 49 53 126) '(27 91 49 55 126) '(27 91 49 56 126)  ; F4..F7
    '(27 91 49 57 126) '(27 91 50 48 126) '(27 91 50 49 126)  ; F8..F10
    '(27 91 50 50 126) '(27 91 50 51 126) '(27 91 50 52 126)) ; F?..F12

) ; close let

) ; close library
