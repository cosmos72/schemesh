;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; Define the functions (sh-fd-allocate) (sh-fd-release) to manage reserved fds


(library (schemesh shell fds (0 1))
  (export
    sh-fd-allocate sh-fd-release)
  (import
    (rnrs)
    (only (chezscheme) fx1-)
    (only (schemesh bootstrap) assert* raise-errorf)
    (schemesh containers bitmap)
    (only (schemesh posix fd) fd-open-max))

;; 1+ highest reserved fd
(define fd-max (fd-open-max))

;; lowest reserved fd
(define fd-min (fx* 3 (fxarithmetic-shift-right fd-max 2)))

;; bitmap of reserved fds
(define fd-bitmap (make-bitmap (fx- fd-max fd-min)))

;; reserve a fd, return its number
(define (sh-fd-allocate)
  (let ((index (bitmap-last-zero fd-bitmap)))
    (when (fx<? index 0)
      (raise-errorf 'sh-fd-allocate "too many reserved fds: " index))
    (bitmap-set! fd-bitmap index 1)
    (fx+ fd-min index)))

;; release a previously reserved fd
(define (sh-fd-release fd)
  (assert* 'sh-fd-release (fx<=? fd-min fd (fx1- fd-max)))
  (let ((index (fx- fd fd-min)))
    (assert* 'sh-fd-release (fx=? 1 (bitmap-ref fd-bitmap index)))
    (bitmap-set! fd-bitmap index 0)))


) ; close library
