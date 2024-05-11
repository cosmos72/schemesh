;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-false sh-true sh-pwd)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure fx1+ fx1- void)
    (only (schemesh bootstrap) while)
    (schemesh containers charspan)
    (only (schemesh conversions) text->bytevector0)
    (only (schemesh posix fd) raise-c-errno)
    (schemesh shell paths)
    (schemesh shell jobs))


(define (sh-false . ignored-args)
  '(exited . 1))


(define (sh-true . ignored-args)
  (void))






(define (sh-pwd)
  (let ((out (current-output-port)))
    (put-string out (charspan->string (sh-cwd)))
    (put-string out "\n")))

) ; close library
