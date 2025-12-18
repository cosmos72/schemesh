;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file lineedit/lineedit.ss


;; linectx is the top-level object used by most lineedit functions
(define-record-type (linectx %make-linectx linectx?)
  (fields
    rbuf              ; bytespan, buffer for stdin
    wbuf              ; bytespan, buffer for stdout
    (mutable vscreen) ; vscreen, input being edited
    (mutable term-x)  ; fixnum, cursor x position in tty
    (mutable term-y)  ; fixnum, cursor y position in tty
    (mutable stdin)   ; input file descriptor, or binary input port
    (mutable stdout)  ; output file descriptor, or binary output port
    (mutable read-timeout-milliseconds) ; -1 means unlimited timeout
    ; bitwise or of: flag-eof? flag-return? flag-sigwinch? flag-redraw? flag-mark-not-bol?
    (mutable flags)
    (mutable parser-name)   ; symbol, name of current parser
    (mutable parsers)       ; #f or hashtable symbol -> parser, table of enabled parsers
    (mutable prompt)        ; bytespan, prompt
    parenmatcher
    (mutable paren)         ; #f or paren containing current parenthes to be highlighted
    clipboard               ; vcellspan
    completions             ; span of cellspans, possible completions
    completion-stem         ; charspan, chars from vscreen used as stem
    (mutable keytable)      ; hashtable, contains keybindings. Usually eq? linectx-default-keytable
    (mutable last-key)      ; #f or procedure, last executed lineedit-key... procedure
    (mutable history-index) ; index of last used item in history
    history)                ; vhistory, history of entered commands
  (nongenerative linectx-7c46d04b-34f4-4046-b5c7-b63753c1be40))


(define flag-eof? 1)
(define flag-return? 2)
(define flag-sigwinch? 4)
(define flag-redraw?     8)
(define flag-mark-not-bol? 16)

(define (linectx-flag? lctx bit)
  (not (fxzero? (fxand bit (linectx-flags lctx)))))

(define (linectx-flag-set! lctx bit flag?)
  (assert* 'linectx-flag-set! (boolean? flag?))
  (let ((flags (linectx-flags lctx)))
    (linectx-flags-set! lctx
      (if flag?
        (fxior flags bit)
        (fxand flags (fxnot bit))))))

(define (linectx-eof? lctx)
  (linectx-flag? lctx flag-eof?))
(define (linectx-return? lctx)
  (linectx-flag? lctx flag-return?))
(define (linectx-sigwinch? lctx)
  (linectx-flag? lctx flag-sigwinch?))
(define (linectx-redraw? lctx)
  (linectx-flag? lctx flag-redraw?))
(define (linectx-mark-not-bol? lctx)
  (linectx-flag? lctx flag-mark-not-bol?))

(define (linectx-eof-set! lctx flag?)
  (linectx-flag-set! lctx flag-eof? flag?))
(define (linectx-return-set! lctx flag?)
  (linectx-flag-set! lctx flag-return? flag?))
(define (linectx-sigwinch-set! lctx flag?)
  (linectx-flag-set! lctx flag-sigwinch? flag?))
(define (linectx-redraw-set! lctx flag?)
  (linectx-flag-set! lctx flag-redraw? flag?))
(define (linectx-mark-not-bol-set! lctx flag?)
  (linectx-flag-set! lctx flag-mark-not-bol? flag?))

;; return number of vlines
(define (linectx-end-y lctx)
  (vscreen-length (linectx-vscreen lctx)))

;; return vscreen cursor x position
(define (linectx-ix lctx)
  (vscreen-cursor-ix (linectx-vscreen lctx)))

;; return vscreen cursor y position
(define (linectx-iy lctx)
  (vscreen-cursor-iy (linectx-vscreen lctx)))

;; return two values: vscreen cursor x and y position
(define (linectx-ixy lctx)
  (vscreen-cursor-ixy (linectx-vscreen lctx)))

;; set vscreen cursor x and y position
(define (linectx-ixy-set! lctx x y)
  (vscreen-cursor-ixy-set! (linectx-vscreen lctx) x y))

;; return vscreen cursor x visual position. It is equal to linectx-ix,
;; unless cursor y = 0, where prompt-end-x is added.
(define (linectx-vx lctx)
  (vscreen-cursor-vx (linectx-vscreen lctx)))

;; return vscreen cursor y visual position. It is equal to linectx-iy + linectx-prompt-end-y.
(define (linectx-vy lctx)
  (vscreen-cursor-vy (linectx-vscreen lctx)))

;; set tty cursor x and y position.
;; Only updates linectx-term-x and linectx-term-y, does *not* write anything to the tty.
(define (linectx-term-xy-set! lctx x y)
  (linectx-term-x-set! lctx x)
  (linectx-term-y-set! lctx y))

;; return screen width
(define (linectx-width lctx)
  (vscreen-width (linectx-vscreen lctx)))

;; return screen height
(define (linectx-height lctx)
  (vscreen-height (linectx-vscreen lctx)))

(define (linectx-prompt-end-x lctx)
  (vscreen-prompt-end-x (linectx-vscreen lctx)))
(define (linectx-prompt-end-y lctx)
  (vscreen-prompt-end-y (linectx-vscreen lctx)))
(define (linectx-prompt-length lctx)
  (vscreen-prompt-length (linectx-vscreen lctx)))
(define (linectx-prompt-length-set! lctx prompt-len)
  (vscreen-prompt-length-set! (linectx-vscreen lctx) prompt-len))




(define linectx-default-keytable (make-eq-hashtable))

;; Variant of (make-linectx) where all arguments are mandatory
;;
;; argument parenmatcher must be #f or a parenmatcher
(define (make-linectx* parenmatcher enabled-parsers history-path)
  (when parenmatcher
    (assert* 'make-linectx* (parenmatcher? parenmatcher)))
  (when enabled-parsers
    (assert* 'make-linectx* (hashtable? enabled-parsers)))
  (when history-path
    (assert* 'make-linectx* (string? history-path)))
  (let* ((sz    (tty-size))
         (rbuf  (bytespan))
         (wbuf  (bytespan))
         (history (vhistory)))
    (bytespan-reserve-right! rbuf 1024)
    (bytespan-reserve-right! wbuf 1024)
    (vhistory-path-set! history history-path)
    (%make-linectx
      rbuf wbuf
      (vscreen (if (pair? sz) (car sz) 80) (if (pair? sz) (cdr sz) 24) "")
      0 0                         ; term-x term-y
      0 2 -1 flag-redraw?         ; stdin stdout read-timeout flags
      'shell enabled-parsers      ; parser-name parsers
      (bytespan)                  ; prompt
      parenmatcher #f             ; parenmatcher paren
      (vcellspan)                 ; clipboard
      (span) (charspan)           ; completions stem
      linectx-default-keytable #f ; keytable last-key
      0 history)))                ; history



;; Create and return a linectx
(define make-linectx
  (case-lambda
    (()
       (make-linectx* #f #f #f))
    ((prompt-func parenmatcher)
       (make-linectx* parenmatcher #f #f))
    ((prompt-func parenmatcher enabled-parsers)
       (make-linectx* parenmatcher enabled-parsers #f))
    ((prompt-func parenmatcher enabled-parsers history-path)
       (make-linectx* parenmatcher enabled-parsers history-path))))


;; Clear and recreate empty vscreen: it may have been saved to history,
;; which retains it.
;; Does NOT write anything to the tty. Does not modify the clipboard.
(define (linectx-clear! lctx)
  (vscreen-clear! (linectx-vscreen lctx))
  (linectx-paren-set! lctx #f)
  (linectx-return-set! lctx #f))



;; Write all pending output to tty.
(define (linectx-flush lctx)
  (let* ((wbuf (linectx-wbuf lctx))
         (beg  (bytespan-peek-beg wbuf))
         (end  (bytespan-peek-end wbuf)))
    (when (fx<? beg end)
      (let ((bv (bytespan-peek-data wbuf))
            (stdout (linectx-stdout lctx)))
        (if (fixnum? stdout)
          (fd-write-all stdout bv beg end)
          (begin
            (put-bytevector stdout bv beg (fx- end beg))
            (flush-output-port stdout))))
      (bytespan-clear! wbuf))))


;; read some bytes, blocking at most for read-timeout-milliseconds
;;   (0 = non-blocking, -1 = unlimited timeout)
;; from (linectx-stdin lctx) and append them to (linectx-rbuf lctx).
;; return number of read bytes, or 0 on timeout, or -1 on eof
(define (linectx-read lctx read-timeout-milliseconds)
  (linectx-flush lctx)
  ;;a (debugf "...before read  lineedit-read rbuf=~s wbuf=~s flags=~s vscreen=~s" (linectx-rbuf lctx) (linectx-wbuf lctx) (linectx-flags lctx) (linectx-vscreen lctx))
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
    (bytespan-reserve-right! rbuf (fx+ rlen max-n))
    (try
      (if (fixnum? fd)
        ;; fd is a file descriptor -> call (fd-select) then (fd-read)
        ;; fd-select raises exception on I/O errors,
        (when (eq? 'read (fd-select fd 'read read-timeout-milliseconds))
          (let ((end (bytespan-peek-end rbuf)))
            ;; fd-read-noretry raises exception on I/O errors,
            ;; and returns #t if interrupted.
            (set! got (fd-read-noretry fd (bytespan-peek-data rbuf) end (fx+ end max-n))))
          (set! eof? (eqv? 0 got)) ; means end of file
          (unless (and (integer? got) (> got 0))
            (set! got 0))) ; #t means interrupted
        ; fd is a binary input port -> call (get-bytevector-n!)
        (let ((n (get-bytevector-n! fd (bytespan-peek-data rbuf)
                                       (bytespan-peek-end rbuf) max-n)))
          (when (fixnum? n)
            (set! got n)
            (set! eof? (fxzero? n))))) ; (fxzero? n) means end of file
      (catch (ex)
        (linectx-show-error lctx "Fatal error, schemesh exiting" ex)
        (set! got 0)
        (set! eof? #t)))
    (assert* 'linectx-read (fixnum? got))
    (assert* 'linectx-read (fx<=? 0 got max-n))
    (bytespan-resize-right! rbuf (fx+ rlen got))
    (if eof? -1 got)))


;; invoked when some function called by linectx-read or lineedit-read raises a condition:
;;
;; display the condition on (console-error-port)
(define (linectx-show-error lctx message ex)
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




(define (linectx-clipboard-clear! lctx)
  (vcellspan-clear! (linectx-clipboard lctx)))


;; append to history a shallow copy of current lines, and return such copy.
;; Also clears current lines, and removes empty lines from history.
(define (linectx-to-history lctx)
  (let* ((y    (linectx-history-index lctx))
         (hist (linectx-history lctx)))
    ;; always overwrite last history slot
    (linectx-history-index-set! lctx (fxmax 0 y (fx1- (vhistory-length hist))))
    (let ((lines (linectx-to-history* lctx)))
      (vhistory-delete-empty-lines! hist (vhistory-length hist))
      (linectx-history-index-set! lctx (vhistory-length hist))
      (linectx-clear! lctx) ;; clear vscreen
      lines)))


;; save to history at current index a shallow copy of current lines, and return such copy.
;; Does NOT modify current lines.
(define (linectx-to-history* lctx)
  (let-values (((ret idx) (vhistory-set*! (linectx-history lctx) (linectx-history-index lctx) (linectx-vscreen lctx))))
    (linectx-history-index-set! lctx idx)
    ret))


;; load history from file. return #t if successful, otherwise return #f
(define (linectx-load-history! lctx)
  (let ((hist (linectx-history lctx)))
     (vhistory-load! hist)
     (linectx-history-index-set! lctx (vhistory-length hist))))


;; save history to file. return #t if successful, otherwise return #f
(define (linectx-save-history lctx)
  (vhistory-save (linectx-history lctx)))

(define (linectx-keytable-insert! keytable proc . keysequences)
  (letrec
    ((%add-bytelist (lambda (htable bytelist)
      (let ((byte (car bytelist)))
        (if (null? (cdr bytelist))
          (hashtable-set! htable byte proc)
          (let ((inner-htable (hashtable-ref htable byte #f)))
            (unless (hashtable? inner-htable)
              (set! inner-htable (make-eq-hashtable))
              (hashtable-set! htable byte inner-htable))
            (%add-bytelist inner-htable (cdr bytelist)))))))
     (%any->bytelist (lambda (keyseq)
       (cond
         ((fixnum?     keyseq) (list keyseq))
         ((pair?       keyseq) keyseq)
         ((bytevector? keyseq) (bytevector->u8-list keyseq))
         ((string?     keyseq) (bytevector->u8-list (string->utf8b keyseq)))
         (else (assert
                 (or (fixnum? keyseq) (pair? keyseq)
                     (bytevector? keyseq) (string? keyseq))))))))
    (do ((l keysequences (cdr l)))
        ((null? l))
      (%add-bytelist keytable (%any->bytelist (car l))))))


(define (linectx-keytable-find keytable rbuf)
  (assert* 'linectx-keytable-find (bytespan? rbuf))
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
          (else               (values #f    (bytespan-length rbuf))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parameter containing the current procedure that updates
;; linectx-prompt and linectx-prompt-length with new prompt
;;
;; initially set to #f, will be set to sh-expand-ps1 by shell/utils.ss
(define linectx-prompt-proc
  (sh-make-parameter
    #f
    (lambda (proc)
      (when proc
        (unless (procedure? proc)
          (raise-errorf 'linectx-prompt-proc "~s is not a procedure" proc))
        (unless (logbit? 1 (procedure-arity-mask proc))
          (raise-errorf 'linectx-prompt-proc "~s is not a is not a procedure accepting 1 argument" proc)))
      proc)))


;; parameter containing the current function that updates
;; linectx-completion-stem and linectx-completions with possible completions.
;;
;; initially set to #f.
(define linectx-completion-proc
  (sh-make-parameter
    #f
    (lambda (proc)
      (when proc
        (unless (procedure? proc)
          (raise-errorf 'sh-current-autocomplete-proc "~s is not a procedure" proc))
        (unless (logbit? 1 (procedure-arity-mask proc))
          (raise-errorf 'sh-current-autocomplete-proc "~s is not a is not a procedure accepting 1 argument" proc)))
      proc)))



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


;; insert a single character or cell into vscreen at cursor.
;; Also moves vscreen cursor one character to the right, and reflows vscreen as needed.
(define (linectx-insert/char! lctx c)
  (vscreen-insert/c! (linectx-vscreen lctx) c))


;; read (- end start) chars from string str, starting at offset = start
;; and insert them into vscreen at cursor.
;;
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
(define linectx-insert/string!
  (case-lambda
    ((lctx str start end)
      (assert* 'linectx-insert/string! (fx<=?* 0 start end (string-length str)))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (linectx-insert/char! lctx (string-ref str i))))
    ((lctx str)
      (linectx-insert/string! lctx str 0 (string-length str)))))


;; read (- end start) chars from charspan csp, starting at offset = start
;; and insert them into vscreen at cursor.
;;
;; Moves cursor appropriately to the right, and reflows vscreen as needed.
(define linectx-insert/charspan!
  (case-lambda
    ((lctx csp start end)
      (assert* 'linectx-insert/charspan! (fx<=?* 0 start end (charspan-length csp)))
      (do ((i start (fx1+ i)))
          ((fx>=? i end))
        (linectx-insert/char! lctx (charspan-ref csp i))))
    ((lctx csp)
      (linectx-insert/charspan! lctx csp 0 (charspan-length csp)))))


;; read up to (- end start) bytes from bytespan bsp, starting at offset = start,
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
                (linectx-insert/char! lctx ch)))))
        (fx- pos start))) ; return number of bytes actually inserted
    ((lctx bsp)
      (linectx-insert/bytespan! lctx bsp 0 (bytespan-length bsp)))))


;; view linectx prompt as mutable ansi-text
(define (linectx-prompt-ansi-text lctx)
  (make-ansi-text (linectx-prompt lctx) ; reuse bytespan containing prompt
                  (vscreen-prompt-length (linectx-vscreen lctx))))


;; store ansi-text as linectx-prompt
(define (linectx-prompt-ansi-text-set! lctx a)
  (linectx-prompt-set! lctx (ansi-text-bytes a))
  (vscreen-prompt-length-set! (linectx-vscreen lctx) (ansi-text-visible-length a)))
