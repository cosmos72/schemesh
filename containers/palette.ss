;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(library (schemesh containers palette (0 8 3))
  (export
    make-palette palette? palette-insert! palette->index palette->rgb2

    rgb rgb? rgb->red rgb->green rgb->blue
    rgb2 rgb2? rgb2->fg rgb2->bg)
  (import
    (rnrs)
    (only (chezscheme)         fx1- meta-cond record-writer)
    (only (schemesh bootstrap) assert*)
    (only (schemesh containers cellvector) palette-index?)
    (schemesh containers span))


(define-syntax rgb-bits (identifier-syntax 24))
(define-syntax rgb-max  (identifier-syntax #xffffff))

(define-syntax rgb2-bits (identifier-syntax 48))
(define-syntax rgb2-max  (identifier-syntax #xffffffffffff))

(define-syntax fx<< (identifier-syntax fxarithmetic-shift-left))
(define-syntax fx>> (identifier-syntax fxarithmetic-shift-right))

(define-syntax >>   (identifier-syntax bitwise-arithmetic-shift-right))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (rgb r g b)
  (fxior (fxand r #xff) (fx<< (fxand g #xff) 8) (fx<< (fxand b #xff) 16)))

(define (rgb? col)
  (and (fixnum? col) (fx<=? 0 col rgb-max)))

(define (rgb->red col)
  (fxand col #xff))

(define (rgb->green col)
  (fxand (fx>> col 8) #xff))

(define (rgb->blue col)
  (fxand (fx>> col 16) #xff))


;; compose foreground and background colors in a single "rgb2" datum
(define (rgb2 fg bg)
  (assert* 'rgb2 (rgb? fg))
  (assert* 'rgb2 (rgb? bg))
  (meta-cond
    ((fixnum? rgb2-max)  (fxior fg (fx<< bg rgb-bits)))
    (else          (bitwise-ior fg (fx<< bg rgb-bits)))))


(define (rgb2? col2)
  (meta-cond
    ((fixnum? rgb2-max) (fx<=? 0 col2 rgb2-max))
    (else  (and (integer? col2) (exact? col2) (<= 0 col2 rgb2-max)))))


;; extract and return foreground rgb color from rgb2
(define (rgb2->fg col2)
  (assert* 'rgb2->fg (rgb2? col2))
  (meta-cond
    ((fixnum? rgb2-max) (fxand col2 rgb-max))
    (else         (bitwise-and col2 rgb-max))))


;; extract and return background rgb color from rgb2
(define (rgb2->bg col2)
  (assert* 'rgb2->bg (rgb2? col2))
  (meta-cond
    ((fixnum? rgb2-max) (fx>> col2 rgb-bits))
    (else                 (>> col2 rgb-bits))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define-record-type (palette %make-palette palette?)
  (fields
     by-color              ;; eqv hashtable indexed by 48-bit rgb2 color pair
     (mutable first-index) ;; fixnum, lowest palette index
     by-index)             ;; span indexed by palette index
  (nongenerative palette-7c46d04b-34f4-4046-b5c7-b63753c1be39))



(define (make-palette)
  (%make-palette (make-eqv-hashtable) 1 (make-span 0)))


;; return index assigned to specified rgb foreground and background colors, or #f if not present
(define (palette->index pal fg bg)
  (assert* 'palette-insert! (rgb? fg))
  (assert* 'palette-insert! (rgb? bg))
  (hashtable-ref (palette-by-color pal) (rgb2 fg bg) #f))


;; return index assigned to specified rgb foreground and background colors, inserting them if needed.
;; return #f if palette is full
(define (palette-insert! pal fg bg)
  (or (palette->index pal fg bg)
      (let* ((col2  (rgb2 fg bg))
             (sp    (palette-by-index pal))
             (index (fx+ (palette-first-index pal) (span-length sp))))
        (cond
          ((palette-index? index)
            (span-insert-right! sp col2)
            (hashtable-set! (palette-by-color pal) col2 index)
            index)
          (else
            #f)))))



;; return rgb2 color associated to specified index, or #f if not present
(define (palette->rgb2 pal index)
  (assert* 'palette->rgb2 (fixnum? index))
  (let* ((sp (palette-by-index pal))
         (lo (palette-first-index pal))
         (hi (fx+ (fx1- lo) (span-length sp))))
    (and (fx<=? lo index hi)
         (span-ref sp (fx- index lo)))))


;; customize how "palette" objects are printed
(record-writer (record-type-descriptor palette)
  (lambda (pal port writer)
    (display "#<palette>" port)))

) ; close library
