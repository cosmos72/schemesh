;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file lineedit/lineedit.ss


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

;; move to start of input
(define (lineedit-key-bof lctx)
  (linectx-ixy-set! lctx 0 0))

;; move to start of line
(define (lineedit-key-bol lctx)
  (linectx-ixy-set! lctx 0 (linectx-iy lctx)))

;; move to end of line
(define (lineedit-key-eol lctx)
  (linectx-ixy-set! lctx (greatest-fixnum) (linectx-iy lctx)))

;; move to end of input
(define (lineedit-key-eof lctx)
  (linectx-ixy-set! lctx (greatest-fixnum) (greatest-fixnum)))

(define (lineedit-key-break lctx)
  (linectx-clear! lctx)
  (linectx-return-set! lctx #t))

(define lineedit-key-clear linectx-clear!)

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
  (vscreen-delete-left/n! (linectx-vscreen lctx) 1)
  (void))

;; delete one character under cursor.
;; does not move cursor.
(define (lineedit-key-del-right lctx)
  (vscreen-delete-right/n! (linectx-vscreen lctx) 1)
  (void))

;; delete from cursor to start of word under cursor.
;; moves cursor n characters to the left, where n is the number of deleted characters.
(define (lineedit-key-del-word-left lctx)
  (let-values (((x y n) (linectx-find-left/word-begin lctx)))
    (when (and x y n (fx>? n 0))
      (lineedit-clipboard-maybe-clear! lctx)
      (vscreen-delete-left/n! (linectx-vscreen lctx) n (linectx-clipboard lctx))
      (void))))

;; delete from cursor to end of word under cursor.
;; does not move cursor.
(define (lineedit-key-del-word-right lctx)
  (let-values (((x y n) (linectx-find-right/word-end lctx)))
    (when (and x y n (fx>? n 0))
      (lineedit-clipboard-maybe-clear! lctx)
      (vscreen-delete-right/n! (linectx-vscreen lctx) n (linectx-clipboard lctx))
      (void))))


(define (lineedit-clipboard-maybe-clear! lctx)
  (unless (key-inserted-into-clipboard? (linectx-last-key lctx))
    (linectx-clipboard-clear! lctx)))


(define (lineedit-key-del-line lctx)
  (void)) ;; TODO: implement

(define (lineedit-key-del-line-left lctx)
  (lineedit-clipboard-maybe-clear! lctx)
  (vscreen-delete-left/vline! (linectx-vscreen lctx) (linectx-clipboard lctx)))

(define (lineedit-key-del-line-right lctx)
  (lineedit-clipboard-maybe-clear! lctx)
  (vscreen-delete-right/vline! (linectx-vscreen lctx) (linectx-clipboard lctx)))

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
           (next-idx (vhistory-index/starts-with
                       hist
                       (fx1+ idx)
                       (vhistory-length hist)
                       (linectx-vscreen lctx)
                       x y)))
    (when next-idx
      (lineedit-navigate-history lctx (fx- next-idx idx))
      (linectx-ixy-set! lctx x y)))))

(define (lineedit-key-history-prev lctx)
  (let-values (((x y) (linectx-ixy lctx)))
    (let* ((hist (linectx-history lctx))
           (idx  (linectx-history-index lctx))
           (prev-idx (vhistory-index-right/starts-with hist 0 idx (linectx-vscreen lctx) x y)))
      (when prev-idx
        (lineedit-navigate-history lctx (fx- prev-idx idx))
        (linectx-ixy-set! lctx x y)))))

(define (lineedit-key-insert-clipboard lctx)
  (let* ((screen    (linectx-vscreen lctx))
         (clipboard (linectx-clipboard lctx))
         (n         (charspan-length clipboard)))
    (unless (fxzero? n)
      (let-values (((x y) (vscreen-cursor-ixy screen)))
        (vscreen-insert-at-xy/vcellspan! screen x y clipboard))
      (when (vcellspan-index/char clipboard #\newline)
        (vscreen-reflow screen))
      (vscreen-cursor-move/right! screen n))))

(define (lineedit-key-redraw lctx)
  (linectx-redraw-set! lctx #t))

(define (lineedit-key-autocomplete lctx)
  (let ((proc (linectx-completion-proc)))
    (when proc
      (proc lctx)
      (%lineedit-update-with-completions lctx))))


(define %linectx-confirm-display-table?
  (let ((header (string->utf8 "; display all "))
        (footer (string->utf8 " possibilities? (y or n) ")))
    (lambda (lctx table-n)
      (let ((wbuf (linectx-wbuf lctx)))
        (bytespan-insert-right/bytevector! wbuf header)
        (bytespan-display-right/fixnum! wbuf table-n)
        (bytespan-insert-right/bytevector! wbuf footer)
        (lineedit-flush lctx)
        (let ((good? (lineedit-read-confirm-y-or-n? lctx)))
          (bytespan-insert-right/u8! wbuf (if good? 121 110) 10)
          good?)))))


(define (%lineedit-update-with-completions lctx)
  (let* ((stem    (linectx-completion-stem lctx))
         (table   (linectx-completions lctx))
         (table-n (span-length table)))
    ;; (debugf "%lineedit-update-with-completions stem=~s, table=~s ..." stem table)
    (unless (fxzero? table-n)
      (let-values (((common-len max-len) (%table-analyze table)))
        ;; (debugf "%lineedit-update-with-completions stem=~s, common-len=~s, table=~s" stem common-len table)
        (cond
          ((not (fxzero? common-len))
            ;; insert common prefix of all completions
            (let ((elem-0 (span-ref table 0)))
              (linectx-insert/charspan! lctx elem-0 0 common-len)
              (when (and (fx=? 1 table-n)
                         (not (char=? #\/ (charspan-ref elem-0 (fx1- common-len)))))
                (linectx-insert/c! lctx #\space))))
          ((fx>? table-n 1)
            ;; erase prompt and lines (also sets flag "redraw prompt and lines"),
            ;; then list all possible table
            (lineedit-undraw lctx)
            (lineedit-display-table lctx stem table max-len '(ask-if-large display-dashes))))))))


(define lineedit-display-table
  (case-lambda
    ((lctx stem table max-len options)
      (let ((column-width (fx+ 2 (fx+ max-len (charspan-length stem))))
            (table-n      (span-length table)))
        (let-values (((column-n row-n) (%lineedit-table-column-n-row-n lctx table-n column-width)))
          (when (or (not (memq 'ask-if-large options))
                    (%lineedit-table-fit-vscreen? lctx column-n row-n)
                    (%linectx-confirm-display-table? lctx table-n))
            (%lineedit-display-table lctx stem table column-width column-n row-n options)))))
    ((lctx table)
      (lineedit-display-table lctx (charspan) table (%table-max-len table) '()))))


;; analyze table, and return the length of longest element
(define (%table-max-len table)
  (let %loop ((i   0)
              (n   (span-length table))
              (len 0))
    (if (fx<? i n)
      (let ((elem (span-ref table i)))
        (%loop (fx1+ i) n
          (cond
            ((string? elem)   (fxmax len (string-length elem)))
            ((charspan? elem) (fxmax len (charspan-length elem)))
            (else             len))))
      len)))


;; analyze table, and return two values:
;;  the length of longest common prefix among table,
;;  and the length of longest elem
(define (%table-analyze table)
  (let* ((elem-0 (span-ref table 0))
         (elem   (if (charspan? elem-0) elem-0 (charspan)))
         (common-len   (charspan-length elem))
         (max-len      common-len))
    (do ((n (span-length table))
         (i 1 (fx1+ i)))
        ((fx>=? i n)
         (values common-len max-len))
      (let ((elem-i (span-ref table i)))
        (when (charspan? elem-i) ; sanity
          ; (debugf "... %table-analyze stem-len = ~s, elem-i = ~s ... " stem-len elem-i)
          (let* ((elem-i-len (charspan-length elem-i))
                 (common-i-len
                   (if (fxzero? common-len)
                     0
                     (charspan-count= elem 0 elem-i 0 (fxmin common-len elem-i-len)))))
            ; (debugf "... %table-analyze common-i-len = ~s" common-i-len)
            (when (fx>? common-len common-i-len)
              (set! common-len common-i-len))
            (when (fx<? max-len elem-i-len)
              (set! max-len elem-i-len))))))))


;; return two values: number of screen columns and number screen of rows
;; needed to list table
(define (%lineedit-table-column-n-row-n lctx table-n column-width)
  (let* ((screen-width  (linectx-width lctx))
         (screen-height (linectx-height lctx))
         (column-n      (fxmax 1 (fx/ screen-width column-width)))
         (row-n         (fx//round-up table-n column-n)))
    (values column-n row-n)))


(define (%lineedit-table-fit-vscreen? lctx column-n row-n)
  (fx<? row-n (linectx-height lctx)))


(define (%lineedit-display-table lctx stem table column-width column-n row-n options)
  ; (debugf "%lineedit-display-table stem=~s, table=~s" stem table)
  (when (memq 'display-dashes options)
    (repeat (linectx-width lctx)
      (lineterm-write/u8 lctx 45)) ; write a whole line full of #\-
    (lineterm-write/u8 lctx 10)) ; write a newline
  (do ((table-n (span-length table))
       (row-i 0 (fx1+ row-i)))
      ((fx>=? row-i row-n))
    (%lineedit-display-table-row lctx stem table column-width column-n row-n row-i)))


(define (%lineedit-display-table-row lctx stem table column-width column-n row-n row-i)
  ; (debugf "%lineedit-display-table-row column-n=~s, row-i=~s" column-n row-i)
  (do ((table-n  (span-length table))
       (table-i  row-i (fx+ row-n table-i))
       (column-i 0     (fx1+ column-i)))
      ((or (fx>=? column-i column-n) (fx>=? table-i table-n)))
    (%lineedit-display-table-cell lctx stem (span-ref table table-i) column-width))
  (lineterm-write/u8 lctx 10)) ;; append newline after each row


(define (%lineedit-display-table-cell lctx stem elem column-width)
  (lineterm-write/charspan lctx stem)
  (let ((elem-len
          (cond
            ((string? elem)
              (lineterm-write/string lctx elem)
              (string-length elem))
            ((charspan? elem)
              (lineterm-write/charspan lctx elem)
              (charspan-length elem))
            (else
              0))))
    (repeat (fx- column-width (fx+ (charspan-length stem) elem-len))
      (lineterm-write/u8 lctx 32)))) ; pad with spaces


(define (fx//round-up x1 x2)
  (let ((q (fx/ x1 x2)))
    (if (fx=? x1 (fx* q x2))
      q
      (fx1+ q))))

(define (lineedit-key-inspect-linectx lctx)
  (tty-inspect lctx))

(define (lineedit-key-toggle-insert lctx)
  (void))

(define (lineedit-key-cmd-cd-parent lctx)
  ((top-level-value 'sh-cd) "..")
  (linectx-redraw-all lctx))

(define (lineedit-key-cmd-cd-old-dir lctx)
  (when
    (try
      ((top-level-value 'sh-cd-))
      #t
      (catch (ex)
        #f))
    (linectx-redraw-all lctx)))

(define (lineedit-key-cmd-ls lctx)
  (lineterm-move-to lctx (linectx-prompt-end-x lctx) (linectx-prompt-end-y lctx))
  (lineterm-write/bytevector lctx #vu8(108 115 27 91 74 10)) ; l s ESC [ J \n
  (lineedit-flush lctx)
  ((top-level-value 'sh-run) ((top-level-value 'sh-cmd) "ls"))
  ; make enough space after command output for prompt and current line(s)
  (repeat (linectx-vy lctx)
    (lineterm-write/u8 lctx 10))
  (linectx-redraw-all lctx))

(define (lineedit-navigate-history lctx delta-y)
  (let ((y      (fx+ delta-y (linectx-history-index lctx)))
        (hist   (linectx-history lctx)))
    (when (fx<? -1 y (vhistory-length hist))
      ; also saves a copy of current linectx-vscreen to history
      (lineedit-lines-set! lctx (vhistory-ref/cow hist y))
      (linectx-history-index-set! lctx y)
      ;; if moving up, set vscreen cursor to end of first line
      ;; if moving down, set vscreen cursor to end of last line
      (linectx-ixy-set! lctx (greatest-fixnum)
                            (if (fx<? delta-y 0) 0 (greatest-fixnum))))))

(define key-inserted-into-clipboard?
  (let ((keys (list lineedit-key-del-line-left lineedit-key-del-line-right
                    lineedit-key-del-word-left lineedit-key-del-word-right
                    lineedit-key-redraw)))
    (lambda (key)
      (and key (memq key keys)))))
