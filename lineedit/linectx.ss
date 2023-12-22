;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh lineedit linectx (0 1))
  (export
    make-linectx make-linectx* linectx? linectx-rbuf linectx-wbuf
    linectx-vscreen   linectx-x     linectx-y   linectx-xy   linectx-xy-set!
    linectx-width     linectx-end-y
    linectx-stdin linectx-stdin-set! linectx-stdout linectx-stdout-set!
    linectx-prompt-end-x linectx-prompt-end-x-set! linectx-prompt-end-y linectx-prompt-end-y-set!
    linectx-prompt linectx-prompt-length linectx-prompt-length-set! linectx-prompt-func
    linectx-parenmatcher linectx-ktable
    linectx-parens linectx-parens-set!
    linectx-completions linectx-completion-stem linectx-completion-func
    linectx-parser-name linectx-parser-name-set!
    linectx-parsers   linectx-parsers-set!
    linectx-history linectx-history-index linectx-history-index-set!
    linectx-to-history linectx-lines-copy-on-write
    linectx-clear! linectx-vscreen-changed
    linectx-eof? linectx-eof-set! linectx-redraw? linectx-redraw-set! linectx-return? linectx-return-set!
    linectx-default-keytable linectx-keytable-set! linectx-keytable-find)

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
    (only (schemesh lineedit parser) make-parsectx*)
    (only (schemesh lineedit io) open-charlines-input-port)
    (schemesh posix tty)
    (only (schemesh posix signal) signal-consume-sigwinch))


;; linectx is the top-level object used by most lineedit functions
(define-record-type
  (linectx %make-linectx linectx?)
  (fields
    (mutable rbuf)    ; bytespan, buffer for stdin
    (mutable wbuf)    ; bytespan, buffer for stdout
    (mutable vscreen) ; vscreen, input being edited
    (mutable stdin)   ; input file descriptor, or binary input port
    (mutable stdout)  ; output file descriptor, or binary output port
    (mutable read-timeout-milliseconds) ; -1 means unlimited timeout
    ; bitwise or of: flag-eof? flag-return? flag-sigwinch? flag-redraw?
    (mutable flags)
    (mutable parser-name)   ; symbol, name of current parser
    (mutable parsers)       ; #f or hashtable symbol -> parser, table of enabled parsers
    prompt                  ; bytespan, prompt
    (mutable prompt-end-y)  ; fixnum, tty y row where prompt ends
    (mutable prompt-length) ; fixnum, prompt draw length
    ; procedure, receives linectx as argument and should update prompt and prompt-length
    (mutable prompt-func)
    parenmatcher
    (mutable parens) ;        #f or parens containing matching parentheses
    completions      ;        span of charspans, possible completions
    completion-stem  ;        charspan, chars from vscreen used as stem
    ; procedure, receives linectx as argument and should update completions and stem
    (mutable completion-func)
    (mutable ktable)        ; hashtable, contains keybindings
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

;; return number of charlines
(define (linectx-end-y ctx)
  (vscreen-end-y (linectx-vscreen ctx)))

;; return cursor x position
(define (linectx-x ctx)
  (vscreen-cursor-x (linectx-vscreen ctx)))

;; return cursor y position
(define (linectx-y ctx)
  (vscreen-cursor-y (linectx-vscreen ctx)))

;; return two values: cursor x and y position
(define (linectx-xy ctx)
  (vscreen-cursor-xy (linectx-vscreen ctx)))

;; set cursor x and y position
(define (linectx-xy-set! ctx x y)
  (vscreen-cursor-xy-set! (linectx-vscreen ctx) x y))

;; return screen width
(define (linectx-width ctx)
  (vscreen-width (linectx-vscreen ctx)))


(define (linectx-prompt-end-x ctx)
  (vscreen-prompt-end-x (linectx-vscreen ctx)))
(define (linectx-prompt-end-x-set! ctx end-x)
  (vscreen-prompt-end-x-set! (linectx-vscreen ctx) end-x))




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
         (wbuf  (bytespan)))
    (bytespan-reserve-back! rbuf 1024)
    (bytespan-reserve-back! wbuf 1024)
    (%make-linectx
      rbuf wbuf
      (vscreen* (if (pair? sz) (car sz) 80) (if (pair? sz) (cdr sz) 24) "")
      0 1 -1 flag-redraw?        ; stdin stdout read-timeout flags
      'shell enabled-parsers     ; parser-name parsers
      (bytespan) 0               ; prompt prompt-end-y
      0 prompt-func              ; prompt-length prompt-func
      parenmatcher #f            ; parenmatcher parens
      (span) (charspan) completion-func ; completions stem completion-func
      linectx-default-keytable   ; keytable
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


;; Clear and recreate empty vscreen: it may have been saved to history,
;; which retains it.
;; Does NOT write anything to the tty
(define (linectx-clear! ctx)
  (vscreen-clear! (linectx-vscreen ctx))
  (linectx-vscreen-changed ctx)
  (linectx-parens-set! ctx #f)
  (linectx-return-set! ctx #f))


(define (linectx-vscreen-changed ctx)
  (parenmatcher-clear! (linectx-parenmatcher ctx)))

;; return a copy-on-write clone of current vscreen being edited
(define (linectx-vscreen-copy ctx)
  ;; (format #t "linectx-vscreen-copy~%")
  ;; (dynamic-wind tty-restore! break tty-setraw!)
  (charlines-copy-on-write (linectx-vscreen ctx)))

;; save current linectx-vscreen to history, and return it
(define (linectx-to-history ctx)
  ;; TODO: do not insert duplicates in history
  (let ((screen (linectx-vscreen ctx)))
    (charhistory-set! (linectx-history ctx) (linectx-history-index ctx) screen)
    screen))

;; return a copy-on-write clone of current charlines being edited
(define (linectx-lines-copy-on-write ctx)
  ;; (format #t "linectx-lines-copy-on-write~%")
  ;; (dynamic-wind tty-restore! break tty-setraw!)
  (charlines-copy-on-write (linectx-vscreen ctx)))


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
         ((string?     keyseq) (bytevector->u8-list (string->utf8 keyseq)))
         (#t (assert
               (or (fixnum? keyseq) (pair? keyseq)
                   (bytevector? keyseq) (string? keyseq))))))))
    (do ((l keysequences (cdr l)))
        ((null? l))
      (%add-bytelist keytable (%any->bytelist (car l))))))

(define (linectx-keytable-find keytable rbuf)
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


;; customize how "linectx" objects are printed
(record-writer (record-type-descriptor linectx)
  (lambda (ctx port writer)
    (display "#<linectx " port)
    (display (linectx-parser-name ctx) port)
    (display ">" port)))

) ; close library
