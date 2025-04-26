;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit lineterm (0 8 3))
  (export
    lineterm-write/u8
    lineterm-write/bytevector lineterm-write/bytespan lineterm-write/charspan lineterm-write/cbuffer lineterm-write/string
    lineterm-move-dx lineterm-move-dy lineterm-move-to-bol lineterm-clear-to-eol lineterm-clear-to-eos
    lineterm-move lineterm-move-from lineterm-move-to lineterm-write-not-bol-marker)

  (import
    (rnrs)
    (only (chezscheme) fx1+ fx1- void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh posix fd)
    (only (schemesh lineedit vscreen) vscreen-height vscreen-width)
    (schemesh lineedit linectx)
    (schemesh posix tty))


;; write a byte to wbuf
(define (lineterm-write/u8 ctx u8)
  (bytespan-insert-right/u8! (linectx-wbuf ctx) u8))

;; write a portion of given bytevector to wbuf
(define lineterm-write/bytevector
  (case-lambda
    ((ctx bv)
      (bytespan-insert-right/bytevector! (linectx-wbuf ctx) bv))
    ((ctx bv start end)
      (bytespan-insert-right/bytevector! (linectx-wbuf ctx) bv start end))))

;; write a portion of given bytespan to wbuf
(define lineterm-write/bytespan
  (case-lambda
    ((ctx bsp)
      (bytespan-insert-right/bytespan! (linectx-wbuf ctx) bsp))
    ((ctx bsp start end)
      (bytespan-insert-right/bytespan! (linectx-wbuf ctx) bsp start end))))

;; write given charspan to wbuf
(define (lineterm-write/charspan ctx csp)
  (bytespan-insert-right/charspan! (linectx-wbuf ctx) csp))

;; write a portion of given chargbuffer to wbuf
(define (lineterm-write/cbuffer ctx cgb start end)
  (do ((wbuf (linectx-wbuf ctx))
       (pos start (fx1+ pos)))
      ((fx>=? pos end))
    (bytespan-insert-right/char! wbuf (chargbuffer-ref cgb pos))))

;; write given string to wbuf
(define (lineterm-write/string ctx str)
  (bytespan-insert-right/string! (linectx-wbuf ctx) str))


;; Move tty cursor horizontally.
;; If dx > 0, send escape sequence "move cursor right by dx".
;; If dx < 0, send escape sequence "move cursor left by -dx".
;; Does not check or update linectx.
(define (lineterm-move-dx ctx dx)
  (cond
    ((fxzero? dx) ; do nothing
      (void))
    ((fx=? dx 1) ; move right by 1
      (lineterm-write/bytevector ctx #vu8(27 91 67)))        ; ESC [ C
    ((fx>? dx 1) ; move right by dx                        ;
      (let ((wbuf (linectx-wbuf ctx)))                     ;
        (bytespan-insert-right/u8! wbuf 27 91)              ; ESC [
        (bytespan-display-right/fixnum! wbuf dx)            ; n
        (bytespan-insert-right/u8! wbuf 67)))               ; C
    ((fx>=? dx -3) ; move left by 1, 2 or 3                ;
      (lineterm-write/bytevector ctx #vu8(8 8 8) 0 (fx- dx))) ; ^H ^H ^H
    ((fx<? dx -3)  ; move left by -dx                      ;
      (let ((wbuf (linectx-wbuf ctx)))                     ;
        (bytespan-insert-right/u8! wbuf 27 91)              ; ESC [
        (bytespan-display-right/fixnum! wbuf (fx- dx))      ; n
        (bytespan-insert-right/u8! wbuf 68)))))             ; D


;; Move tty cursor vertically.
;; If dy > 0, send escape sequence "move cursor down by dy".
;; If dy < 0, send escape sequence "move cursor up by -dy".
;; Does not check or update linectx.
(define (lineterm-move-dy ctx dy)
  (cond
    ((fxzero? dy) ; do nothing
      (void))
    ((fx=? dy 1)  ; move down by 1
      (lineterm-write/bytevector ctx #vu8(27 91 66)))  ; ESC [ B
    ((fx=? dy -1) ; move up by 1
      (lineterm-write/bytevector ctx #vu8(27 91 65)))  ; ESC [ A
    ((fx>? dy 1) ; move down by dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-insert-right/u8! wbuf 27 91)     ; ESC [
        (bytespan-display-right/fixnum! wbuf dy)   ; n
        (bytespan-insert-right/u8! wbuf 66)))      ; B
    ((fx<? dy -1) ; move up by -dy
      (let ((wbuf (linectx-wbuf ctx)))
        (bytespan-insert-right/u8! wbuf 27 91)     ; ESC [
        (bytespan-display-right/fixnum! wbuf (fx- dy)) ; n
        (bytespan-insert-right/u8! wbuf 65)))))    ; A

;; send escape sequence "move to begin-of-line". Moves at beginning of prompt!
(define (lineterm-move-to-bol ctx)
  (lineterm-write/u8 ctx 13)) ; CTRL+M i.e. '\r'

;; send escape sequence "clear from cursor to end-of-line"
(define (lineterm-clear-to-eol ctx)
  (lineterm-write/bytevector ctx #vu8(27 91 75))) ; ESC [ K

;; send escape sequence "clear from cursor to end-of-screen"
(define (lineterm-clear-to-eos ctx)
  (lineterm-write/bytevector ctx #vu8(27 91 74))) ; ESC [ J

;; move tty cursor from tty position from-x from-y to tty position to-x to-y
;; does not check or update linectx
(define (lineterm-move ctx from-x from-y to-x to-y)
  (let* ((screen (linectx-vscreen ctx))
         (xmax   (fx1- (vscreen-width screen)))
         (ymax   (fx1- (vscreen-height screen)))
         ;; clamp x to 0 ... width-1, and clamp y to 0 ... height-1
         (from-x (fxmax 0 (fxmin from-x xmax)))
         (from-y (fxmax 0 (fxmin from-y ymax)))
         (to-x   (fxmax 0 (fxmin to-x   xmax)))
         (to-y   (fxmax 0 (fxmin to-y   ymax))))
    (lineterm-move-dy ctx (fx- to-y from-y))
    (if (and (fxzero? to-x) (not (fxzero? from-x)))
      (lineterm-move-to-bol ctx)
      (lineterm-move-dx ctx (fx- to-x from-x)))))

;; move tty cursor from its current tty position at from-x, from-y
;; back to linectx-term-x linectx-term-y
(define (lineterm-move-from ctx from-x from-y)
  ; (debugf "lineterm-move-from (~s ~s)" from-x from-y)
  (lineterm-move ctx from-x from-y (linectx-term-x ctx) (linectx-term-y ctx)))

;; move tty cursor from its current tty position at linectx-term-x linectx-term-y
;; to specified tty position to-x to-y
(define (lineterm-move-to ctx to-x to-y)
  (lineterm-move ctx (linectx-term-x ctx) (linectx-term-y ctx) to-x to-y))

;; if cursor is not at beginning of line, write a highlighted space to show that last command
;; did not write a newline, then move to next line.
;;
;; if cursor is at beginning of line, writes some useless spaces that will be erased by prompt and input.
(define lineterm-write-not-bol-marker
  (let* ((space-n 256)
         (spaces (make-bytevector space-n 32)))
    (lambda (ctx)
      (let ((wbuf  (linectx-wbuf ctx))
            (width (linectx-width ctx)))
        (bytespan-insert-right/bytevector! wbuf #vu8(27 91 55 109 32 27 91 109)) ; ESC [ 7 m SPACE ESC [ m
        (do ((write-n (fx1- width) (fx- write-n space-n)))
            ((fx<=? write-n 0))
          (bytespan-insert-right/bytevector! wbuf spaces 0 (fxmin write-n space-n)))))))


) ; close library
