;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix tty (0 9 1))
  (export tty-setraw! tty-restore! tty-inspect tty-size with-cooked-tty with-raw-tty)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure inspect))


(define tty-restore! (foreign-procedure "c_tty_restore" () int))

(define tty-setraw! (foreign-procedure "c_tty_setraw" () int))

;; (tty-size) calls C functions c_tty_size(),
;; which returns controlling tty size as pair (width . height), or c_errno() < 0 on error
(define tty-size   (foreign-procedure "c_tty_size" () ptr))

(define-syntax with-cooked-tty
  (syntax-rules ()
    ((_ body1 body2 ...)
      (dynamic-wind
        tty-restore!       ; run before body
        (lambda () body1 body2 ...)
        tty-setraw!))))      ; run after body

(define-syntax with-raw-tty
  (syntax-rules ()
    ((_ body1 body2 ...)
      (dynamic-wind
        tty-setraw!       ; run before body
        (lambda () body1 body2 ...)
        tty-restore!))))      ; run after body

(define (tty-inspect obj)
  (with-cooked-tty (inspect obj)))



) ; close library
