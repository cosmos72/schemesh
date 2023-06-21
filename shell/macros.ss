;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell macros (0 1))
  (export shell shell-list shell-backquote)
  (import
    (rnrs)
    (schemesh bootstrap)
    (schemesh shell jobs)
    (schemesh shell parse))

(define-macro (shell . args)
  (sh-parse args))

(define-syntax shell-list
  (syntax-rules ()
    ((_)           '(sh-true))
    ((_ arg)          arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))

(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ((_ arg)           (sh-run-capture-output arg))
    ((_ arg0 arg1 ...) (sh-run-capture-output (sh-list arg0 arg1 ...)))))

) ; close library
