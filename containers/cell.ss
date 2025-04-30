;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(library (schemesh containers cell (0 8 3))
  (export
    cell cell? cell->char cell->palette cell->colors cell-write cell-display/bytespan

    tty-color? tty-rgb24 tty-rgb8 tty-rgb4 tty-gray5 symbol->tty-rgb4
    tty-colors tty-colors? tty-colors->fg tty-colors->bg tty-colors->palette
    tty-palette? tty-palette->colors

    tty-fg-display tty-bg-display tty-colors-display tty-palette-display
    tty-fg-display/bytespan tty-bg-display/bytespan  tty-colors-display/bytespan tty-palette-display/bytespan)
  (import
    (rnrs)
    (only (chezscheme)                      fx1- meta-cond record-writer)
    (only (schemesh bootstrap)              assert*)
    (schemesh containers bytespan)
    (only (schemesh containers list)        list-index)
    (only (schemesh containers hashtable)   eqv-hashtable)
    (schemesh containers span)
    (only (schemesh containers utf8b)       integer->char*)
    (only (schemesh containers utf8b utils) bytespan-display-right/fixnum! bytespan-insert-right/char!))

(define-syntax cell-bytes-log2 (identifier-syntax 2))
(define-syntax cell-bytes      (identifier-syntax 4))
(define-syntax cell-min        (identifier-syntax #x-20000000))
(define-syntax cell-max        (identifier-syntax #x1fffffff))

(define-syntax char-bits       (identifier-syntax 21))
(define-syntax char-max        (identifier-syntax #x1fffff))


(define-syntax palette-bits (meta-cond ((fixnum? #x7fffffff) (identifier-syntax 11))
                                       (else                 (identifier-syntax 9))))
(define-syntax palette-min  (meta-cond ((fixnum? #x7fffffff) (identifier-syntax #x-400))
                                       (else                 (identifier-syntax #x-100))))
(define-syntax palette-max  (meta-cond ((fixnum? #x7fffffff) (identifier-syntax #x3ff))
                                       (else                 (identifier-syntax #xff))))
(define-syntax palette-n    (meta-cond ((fixnum? #x7fffffff) (identifier-syntax #x800))
                                       (else                 (identifier-syntax #x200))))

(define-syntax color-min  (identifier-syntax #x-100))
(define-syntax color-max  (identifier-syntax #xffffff))

(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tty-color is a fixnum or #f:
;;  range [0, #xffffff] indicates 24-bit rgb
;;  range [-256, -1] indicates 8-bit color
;;  #f indicates the terminal's default foreground or background color
(define (tty-color? col)
  (or (not col)
      (and (fixnum? col) (fx<=? color-min col color-max))))

;; create and return tty-color indicating 24-bit rgb: r g and b must be in [0, 255]
(define (tty-rgb24 r g b)
  (assert* 'tty-rgb24 (fx<=? 0 r 255))
  (assert* 'tty-rgb24 (fx<=? 0 g 255))
  (assert* 'tty-rgb24 (fx<=? 0 b 255))
  (fxior r (fx<< g 8) (fx<< b 16)))

;; create and return tty-color indicating 6x6x6 rgb color cube: r g and b must be in [0, 5]
(define (tty-rgb8 r g b)
  (assert* 'tty-rgb8 (fx<=? 0 r 5))
  (assert* 'tty-rgb8 (fx<=? 0 g 5))
  (assert* 'tty-rgb8 (fx<=? 0 b 5))
  (fxnot (fx+ (fx+ (fx* 36 r) (fx* 6 g))
              (fx+ b 16))))

;; create and return tty-color indicating 24-step greyscale: w must be in [0, 23]
(define (tty-gray5 w)
  (assert* 'tty-gray5 (fx<=? 0 w 23))
  (fxnot (fx+ 232 w)))

;; create and return tty-color indicating ANSI/VGA color: r g b and high must be in [0, 1]
(define (tty-rgb4 r g b high)
  (assert* 'tty-rgb4 (fx<=? 0 r 1))
  (assert* 'tty-rgb4 (fx<=? 0 g 1))
  (assert* 'tty-rgb4 (fx<=? 0 b 1))
  (assert* 'tty-rgb4 (fx<=? 0 high 1))
  (fxnot (fxior r (fx<< g 1) (fx<< b 2) (fx<< high 3))))

;; create and return tty-color indicating ANSI/VGA color.
;; name must be one of the symbols:
;;  'black 'red 'green 'yellow 'blue 'magenta 'cyan 'white
;; if high is truish, create and return high intensity color
(define symbol->tty-rgb4
  (let ((names '(black red green yellow blue magenta cyan white)))
    (case-lambda
      ((name high?)
        (let ((idx (list-index (lambda (e) (eq? e name)) names)))
          (unless idx
            (assert* 'symbol->tty-rgb4 (memq name names)))
          (fxnot (fxior idx (if high? 8 0)))))
      ((name)
        (symbol->tty-rgb4 name #f)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; tty-colors is a pair of tty-color: foreground, background
;; plus an associated palette index.
;; #f is NOT a valid tty-colors.
(define-record-type (%colors %make-colors tty-colors?)
  (fields
    (immutable fg      tty-colors->fg)        ; tty-color
    (immutable bg      tty-colors->bg)        ; tty-color
    (immutable palette tty-colors->palette))  ; tty-palette
  (nongenerative %colors-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; global conversion table tty-palette -> tty-colors.
(define palette-span (span (%make-colors #f #f 0)))

;; global conversion table fg -> bg -> tty-colors.
(define colors-map (eqv-hashtable #f (eqv-hashtable #f (span-ref palette-span 0))))


;; compose foreground and background tty-color in a single tty-colors and return it.
;; return #f if palette table is full
(define (tty-colors fg bg)
  (assert* 'tty-colors (tty-color? fg))
  (assert* 'tty-colors (tty-color? bg))
  (let* ((sp     palette-span)
         (index  (span-length sp))
         (bg-map (hashtable-ref colors-map fg #f)))
    (or
      (and bg-map (hashtable-ref bg-map bg #f))
      (if (fx<? index palette-n)
        (let* ((palette (if (fx<=? index palette-max) index (fx- index palette-n)))
               (ret     (%make-colors fg bg palette)))
          (span-insert-right! sp ret)
          (if bg-map
            (hashtable-set! bg-map bg ret)
            (hashtable-set! colors-map fg (eqv-hashtable bg ret)))
          ret)
        #f)))) ; palette table is full


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; palette is a small fixnum used as shortcut for a previously created tty-colors
(define (tty-palette? palette)
  (and (fixnum? palette) (fx<=? palette-min palette palette-max)))


;; return the tty-colors associated to specified palette, or #f if not present
(define (tty-palette->colors palette)
  (assert* 'tty-palette->colors (tty-palette? palette))
  (let ((sp    palette-span)
        (index (if (fx>=? palette 0) palette (fx+ palette palette-n))))
    (and (fx<? -1 index (span-length sp))
         (span-ref sp index))))


;; send to textual port the ANSI escape sequences for setting terminal foreground color.
(define (tty-fg-display fg port)
  (cond
    ((not fg) ; keep default foreground color
      #f)
    ((fx>=? fg 0)   ; 24-bit RGB fg color
      (put-string port ";38;2;")
      (display (fxand #xff fg) port)
      (put-char port #\;)
      (display (fxand #xff (fx>> fg 8)) port)
      (put-char port #\;)
      (display (fxand #xff (fx>> fg 16)) port))
    ((fx>=? fg -8)  ; low-intensity VGA/ANSI fg color
      (put-string port ";3")
      (display (fxnot fg) port))
    ((fx>=? fg -16) ; high-intensity VGA/ANSI fg color
      (put-string port ";1;3")
      (display (fxand 7 (fxnot fg)) port))
    (else           ; 8-bit RGB fg color
      (put-string port ";38;5;")
      (display (fxnot fg) port))))


;; send to textual port the ANSI escape sequence for setting terminal background color.
(define (tty-bg-display bg port)
  (cond
    ((not bg) ; keep default foreground color
      #f)
    ((fx>=? bg 0)   ; 24-bit RGB bg color
      (put-string port ";48;2;")
      (display (fxand #xff bg) port)
      (put-char port #\;)
      (display (fxand #xff (fx>> bg 8)) port)
      (put-char port #\;)
      (display (fxand #xff (fx>> bg 16)) port))
    ((fx>=? bg -8)  ; low-intensity VGA/ANSI bg color
      (put-string port ";4")
      (display (fxnot bg) port))
    ;; high-intensity VGA/ANSI bg color are not standard
    ;; => use instead 8-bit RGB bg color
    (else           ; 8-bit RGB bg color
      (put-string port ";48;5;")
      (display (fxnot bg) port))))


;; append to bytespan the ANSI escape sequences for setting terminal foreground color.
(define (tty-fg-display/bytespan fg wbuf)
  (cond
    ((not fg) ; keep default foreground color
      #f)
    ((fx>=? fg 0)   ; 24-bit RGB
      (bytespan-insert-right/bytevector! wbuf #vu8(59 51 56 59 50 59))
      (bytespan-display-right/fixnum!    wbuf (fxand #xff fg) wbuf)
      (bytespan-insert-right/u8!         wbuf 59)
      (bytespan-display-right/fixnum!    wbuf (fxand #xff (fx>> fg 8)))
      (bytespan-insert-right/u8!         wbuf 59)
      (bytespan-display-right/fixnum!    wbuf (fxand #xff (fx>> fg 16))))
    ((fx>=? fg -8)  ; low-intensity VGA/ANSI color
      (bytespan-insert-right/bytevector! wbuf #vu8(59 51))
      (bytespan-display-right/fixnum!    wbuf (fxnot fg)))
    ((fx>=? fg -16) ; high-intensity VGA/ANSI color
      (bytespan-insert-right/bytevector! wbuf #vu8(59 49 59 51))
      (bytespan-display-right/fixnum!    wbuf (fxand 7 (fxnot fg))))
    (else           ; 8-bit RGB
      (bytespan-insert-right/bytevector! wbuf #vu8(59 51 56 59 53 59))
      (bytespan-display-right/fixnum!    wbuf (fxnot fg)))))



;; append to bytespan the ANSI escape sequences for setting terminal foreground color.
(define (tty-bg-display/bytespan fg wbuf)
  (cond
    ((not fg) ; keep default background color
      #f)
    ((fx>=? fg 0)   ; 24-bit RGB
      (bytespan-insert-right/bytevector! wbuf #vu8(59 52 56 59 50 59))
      (bytespan-display-right/fixnum!    wbuf (fxand #xff fg) wbuf)
      (bytespan-insert-right/u8!         wbuf 59)
      (bytespan-display-right/fixnum!    wbuf (fxand #xff (fx>> fg 8)))
      (bytespan-insert-right/u8!         wbuf 59)
      (bytespan-display-right/fixnum!    wbuf (fxand #xff (fx>> fg 16))))
    ((fx>=? fg -8)  ; low-intensity VGA/ANSI color
      (bytespan-insert-right/bytevector! wbuf #vu8(59 52))
      (bytespan-display-right/fixnum!    wbuf (fxnot fg)))
    ;; high-intensity VGA/ANSI bg color are not standard
    ;; => use instead 8-bit RGB bg color
    (else           ; 8-bit RGB
      (bytespan-insert-right/bytevector! wbuf #vu8(59 52 56 59 53 59))
      (bytespan-display-right/fixnum!    wbuf (fxnot fg)))))


;; send to textual port the ANSI escape sequence for setting
;; terminal foreground and background colors to specified tty-colors.
(define (tty-colors-display cols port)
  (put-string port "\x1b;[") ; reset fg and bg to default
  (tty-fg-display (and cols (tty-colors->fg cols)) port)
  (tty-bg-display (and cols (tty-colors->bg cols)) port)
  (put-char port #\m))


;; append to bytespan the ANSI escape sequence for setting
;; terminal foreground and background colors to specified tty-colors.
(define (tty-colors-display/bytespan cols wbuf)
  (bytespan-insert-right/u8! wbuf 27 91) ; reset fg and bg to default
  (tty-fg-display/bytespan (and cols (tty-colors->fg cols)) wbuf)
  (tty-bg-display/bytespan (and cols (tty-colors->bg cols)) wbuf)
  (bytespan-insert-right/u8! wbuf 109))



;; send to textual port the ANSI escape sequences for setting
;; terminal foreground and background colors to specified palette.
(define (tty-palette-display palette port)
  (tty-colors-display (tty-palette->colors palette) port))


;; append to bytespan the ANSI escape sequences for setting
;; terminal foreground and background colors to specified palette.
(define (tty-palette-display/bytespan palette wbuf)
  (tty-colors-display/bytespan (tty-palette->colors palette) wbuf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (tty-color-write col port)
  (if col
    (begin
      (display "#x" port)
      (display (number->string col 16) port))
    (display "#f" port)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; cell is a colored char, contains a palette to indicate the fg ad bg colors
(define (cell? cl)
  (and (fixnum? cl)  (fx<=? cell-min cl cell-max)))

(define cell
  (case-lambda
    ((ch)
      (char->integer ch))
    ((ch palette-or-tty-colors)
      (let ((palette (if (tty-palette? palette-or-tty-colors)
                       palette-or-tty-colors
                       (tty-colors->palette palette-or-tty-colors))))
        (fxior (fx<< palette char-bits) (char->integer ch))))))


(define (cell->char cl)
  (integer->char* (fxand cl char-max)))

(define (cell->palette cl)
  (fx>> cl char-bits))

(define (cell->colors cl)
  (tty-palette->colors (cell->palette cl)))

;; write colored char to textual output port, escaping special characters
(define (cell-write cl old-palette port)
  (let ((ch      (cell->char    cl))
        (palette (cell->palette cl)))
    (unless (fx=? palette old-palette)
      (tty-palette-display palette port))
    (cond
      ((or (char=? #\" ch) (char=? #\\ ch))
        (put-char port #\\)
        (put-char port ch))
      ((char<=? #\space ch #\~)
        (put-char port ch))
      ((char=? #\newline ch)
        (put-char port #\\)
        (put-char port #\n))
      (else
        (let ((n (char->integer ch)))
          (put-string port "\\x")
          (display (number->string n 16) port)
          (put-char port #\;))))))


;; write colored char to bytespan, NOT escaping special characters
(define (cell-display/bytespan cl old-palette wbuf)
  (let ((ch      (cell->char    cl))
        (palette (cell->palette cl)))
    (unless (fx=? palette old-palette)
      (tty-palette-display/bytespan palette wbuf))
    (bytespan-insert-right/char! wbuf ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; customize how "tty-colors" objects are printed
(record-writer (record-type-descriptor %colors)
  (lambda (cols port writer)
    (display "(tty-colors " port)
    (tty-color-write (tty-colors->fg cols) port)
    (display " " port)
    (tty-color-write (tty-colors->bg cols) port)
    (display ")" port)))


) ; close library
