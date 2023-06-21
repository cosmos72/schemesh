;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix tty (0 1))
  (export tty-setraw! tty-restore! tty-size)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure))

(define tty-setraw! (foreign-procedure "c_tty_setraw" () int))

(define tty-restore! (foreign-procedure "c_tty_restore" () int))

; (tty-size) calls C functions c_tty_size(),
; which returns controlling tty size as pair (width . height), or c_errno() on error
(define tty-size   (foreign-procedure "c_tty_size" () scheme-object))

) ; close library
