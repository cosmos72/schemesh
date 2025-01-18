;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit linectx (0 1))
  (export
    make-linectx make-linectx* linectx? linectx-rbuf linectx-wbuf
    linectx-vscreen linectx-width linectx-height linectx-end-y
    linectx-ix     linectx-iy     linectx-ixy  linectx-ixy-set!
    linectx-vx     linectx-vy
    linectx-term-x linectx-term-y linectx-term-xy-set!
    linectx-stdin  linectx-stdin-set! linectx-stdout linectx-stdout-set!
    linectx-prompt      linectx-prompt-end-x  linectx-prompt-end-y
    linectx-prompt-func linectx-prompt-length linectx-prompt-length-set!
    linectx-parenmatcher linectx-ktable
    linectx-paren linectx-paren-set!
    linectx-completions linectx-completion-stem linectx-completion-func
    linectx-parser-name linectx-parser-name-set!
    linectx-parsers linectx-parsers-set!
    linectx-history linectx-history-index linectx-history-index-set! linectx-to-history*
    linectx-load-history! linectx-save-history
    linectx-clear!  linectx-eof? linectx-eof-set! linectx-redraw? linectx-redraw-set!
    linectx-return? linectx-return-set!
    linectx-default-keytable linectx-keytable-set! linectx-keytable-find)

  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- record-writer void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (schemesh lineedit vscreen)
    (schemesh lineedit charhistory)
    (schemesh lineedit charhistory io)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (only (schemesh lineedit parser) make-parsectx*)
    (only (schemesh lineedit io) open-charlines-input-port)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))


;; linectx is the top-level object used by most lineedit functions
(define-record-type
  (linectx %make-linectx linectx?)
  (fields
    rbuf              ; bytespan, buffer for stdin
    wbuf              ; bytespan, buffer for stdout
    (mutable vscreen) ; vscreen, input being edited
    (mutable term-x)  ; fixnum, cursor x position in tty
    (mutable term-y)  ; fixnum, cursor y position in tty
    (mutable stdin)   ; input file descriptor, or binary input port
    (mutable stdout)  ; output file descriptor, or binary output port
    (mutable read-timeout-milliseconds) ; -1 means unlimited timeout
    ; bitwise or of: flag-eof? flag-return? flag-sigwinch? flag-redraw?
    (mutable flags)
    (mutable parser-name)   ; symbol, name of current parser
    (mutable parsers)       ; #f or hashtable symbol -> parser, table of enabled parsers
    prompt                  ; bytespan, prompt
    ; procedure, receives linectx as argument and should update prompt and prompt-length
    (mutable prompt-func)
    parenmatcher
    (mutable paren)         ; #f or paren containing current parenthes to be highlighted
    completions             ; span of charspans, possible completions
    completion-stem         ; charspan, chars from vscreen used as stem
    ; procedure, receives linectx as argument and should update completions and stem
    (mutable completion-func)
    (mutable ktable)        ; hashtable, contains keybindings
    (mutable history-index) ; index of last used item in history
    history))               ; charhistory, history of entered commands

(define flag-eof? 1)
(define flag-return? 2)
(define flag-sigwinch? 4)
(define flag-redraw? 8)

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

(define (linectx-eof-set! lctx flag?)
  (linectx-flag-set! lctx flag-eof? flag?))
(define (linectx-return-set! lctx flag?)
  (linectx-flag-set! lctx flag-return? flag?))
(define (linectx-sigwinch-set! lctx flag?)
  (linectx-flag-set! lctx flag-sigwinch? flag?))
(define (linectx-redraw-set! lctx flag?)
  (linectx-flag-set! lctx flag-redraw? flag?))

;; return number of charlines
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




(define linectx-default-keytable (eq-hashtable))

;; Variant of (make-linectx) where all arguments are mandatory
;;
;; argument prompt-func must be a procedure accepting linectx,
;; and updating linectx-prompt and linectx-prompt-length.
;;
;; argument parenmatcher must be #f or a parenmatcher
;;
;; argument prompt-func must be #f or a procedure accepting linectx,
;; and updating linectx-completions
(define (make-linectx* prompt-func parenmatcher completion-func enabled-parsers history-path)
  (assert* 'make-linectx* (procedure? prompt-func))
  (when parenmatcher
    (assert* 'make-linectx* (parenmatcher? parenmatcher)))
  (when completion-func
    (assert* 'make-linectx* (procedure? completion-func)))
  (when enabled-parsers
    (assert* 'make-linectx* (hashtable? enabled-parsers)))
  (when history-path
    (assert* 'make-linectx* (string? history-path)))
  (let* ((sz    (tty-size))
         (rbuf  (bytespan))
         (wbuf  (bytespan))
         (history (charhistory)))
    (bytespan-reserve-back! rbuf 1024)
    (bytespan-reserve-back! wbuf 1024)
    (charhistory-path-set! history history-path)
    (%make-linectx
      rbuf wbuf
      (vscreen* (if (pair? sz) (car sz) 80) (if (pair? sz) (cdr sz) 24) "")
      0 0                        ; term-x term-y
      0 1 -1 flag-redraw?        ; stdin stdout read-timeout flags
      'shell enabled-parsers     ; parser-name parsers
      (bytespan)                 ; prompt
      prompt-func                ; prompt-func
      parenmatcher #f            ; parenmatcher paren
      (span) (charspan) completion-func ; completions stem completion-func
      linectx-default-keytable   ; keytable
      0 history)))              ; history

(define (default-prompt-func lctx)
  (let* ((str    (symbol->string (linectx-parser-name lctx)))
         (bv     (string->utf8b str))
         (prompt (linectx-prompt lctx)))
    (bytespan-clear! prompt)
    (bytespan-insert-back/bvector! prompt bv 0 (bytevector-length bv))
    ; append colon and space after parser name
    (bytespan-insert-back/u8! prompt 58 32)
    (linectx-prompt-length-set! lctx (fx+ 2 (string-length str)))))


;; Create and return a linectx
(define make-linectx
  (case-lambda
    (()
       (make-linectx* default-prompt-func #f #f #f #f))
    ((prompt-func)
       (make-linectx* prompt-func #f #f #f #f))
    ((prompt-func parenmatcher)
       (make-linectx* prompt-func parenmatcher #f #f #f))
    ((prompt-func parenmatcher completion-func)
       (make-linectx* prompt-func parenmatcher completion-func #f #f))
    ((prompt-func parenmatcher completion-func enabled-parsers)
       (make-linectx* prompt-func parenmatcher completion-func enabled-parsers #f))
    ((prompt-func parenmatcher completion-func enabled-parsers history-path)
       (make-linectx* prompt-func parenmatcher completion-func enabled-parsers history-path))))


;; Clear and recreate empty vscreen: it may have been saved to history,
;; which retains it.
;; Does NOT write anything to the tty
(define (linectx-clear! lctx)
  (vscreen-clear! (linectx-vscreen lctx))
  (linectx-paren-set! lctx #f)
  (linectx-return-set! lctx #f))



;; save to history a shallow clone of charlines in linectx-vscreen,
;; remove empty charlines from history, and return such clone
(define (linectx-to-history* lctx)
  (let-values (((ret idx) (charhistory-set*! (linectx-history lctx) (linectx-history-index lctx) (linectx-vscreen lctx))))
    (linectx-history-index-set! lctx idx)
    ret))

;; load history from file. return #t if successful, otherwise return #f
(define (linectx-load-history! lctx)
  (let ((hist (linectx-history lctx)))
     (charhistory-load! hist)
     (linectx-history-index-set! lctx (charhistory-length hist))))


;; save history to file. return #t if successful, otherwise return #f
(define (linectx-save-history lctx)
  (charhistory-save (linectx-history lctx)))


(define (linectx-keytable-set! keytable proc . keysequences)
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
         ((string?     keyseq) (bytevector->u8-list (string->utf8b keyseq)))
         (#t (assert
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
          (#t                 (values #f    (bytespan-length rbuf))))))))


;; customize how "linectx" objects are printed
(record-writer (record-type-descriptor linectx)
  (lambda (lctx port writer)
    (display "#<linectx " port)
    (display (linectx-parser-name lctx) port)
    (display ">" port)))

) ; close library
