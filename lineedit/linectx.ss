;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit linectx (0 8 3))
  (export
    linectx-prompt-proc linectx-completion-proc

    make-linectx make-linectx* linectx? linectx-rbuf linectx-wbuf
    linectx-vscreen linectx-width linectx-height linectx-end-y
    linectx-ix     linectx-iy     linectx-ixy  linectx-ixy-set!
    linectx-vx     linectx-vy
    linectx-term-x linectx-term-y linectx-term-xy-set!
    linectx-stdin  linectx-stdin-set!   linectx-stdout        linectx-stdout-set!
    linectx-prompt linectx-prompt-set!  linectx-prompt-end-x  linectx-prompt-end-y
    linectx-prompt-length linectx-prompt-length-set!
    linectx-prompt-ansi-text            linectx-prompt-ansi-text-set!
    linectx-parenmatcher linectx-paren linectx-paren-set!
    linectx-clipboard linectx-clipboard-clear!
    linectx-completions linectx-completion-stem
    linectx-flags linectx-parser-name linectx-parser-name-set!
    linectx-parsers linectx-parsers-set!
    linectx-history linectx-history-index linectx-history-index-set! linectx-to-history*
    linectx-clear!  linectx-flush linectx-read linectx-read-some linectx-show-error
    linectx-load-history! linectx-save-history
    linectx-eof? linectx-eof-set! linectx-redraw? linectx-redraw-set!
    linectx-return? linectx-return-set! linectx-mark-not-bol? linectx-mark-not-bol-set!
    linectx-default-keytable linectx-keytable linectx-keytable-find linectx-keytable-insert!
    linectx-last-key linectx-last-key-set!)

  (import
    (rnrs)
    (only (chezscheme) console-error-port display-condition fx1+ fx1- logbit? procedure-arity-mask record-writer void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (schemesh screen vcellspan)
    (schemesh screen vscreen)
    (schemesh screen vhistory)
    (schemesh screen vhistory io)
    (schemesh screen vlines io)
    (schemesh lineedit ansi)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (only (schemesh lineedit parser) make-parsectx*)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))



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
      0 1 -1 flag-redraw?         ; stdin stdout read-timeout flags
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


;; save to history a shallow clone of vlines in linectx-vscreen,
;; remove empty vlines from history, and return such clone
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


;; view linectx prompt as mutable ansi-text
(define (linectx-prompt-ansi-text lctx)
  (make-ansi-text (linectx-prompt lctx) ; reuse bytespan containing prompt
                  (vscreen-prompt-length (linectx-vscreen lctx))))


;; store ansi-text as linectx-prompt
(define (linectx-prompt-ansi-text-set! lctx a)
  (linectx-prompt-set! lctx (ansi-text-bytes a))
  (vscreen-prompt-length-set! (linectx-vscreen lctx) (ansi-text-visible-length a)))


;; customize how "linectx" objects are printed
(record-writer (record-type-descriptor linectx)
  (lambda (lctx port writer)
    (display "#<linectx " port)
    (display (linectx-parser-name lctx) port)
    (display ">" port)))

) ; close library
