;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell macros (0 1))
  (export shell shell-backquote shell-concat shell-env shell-list shell-subshell)
  (import
    (rnrs)
    (schemesh bootstrap)
    (schemesh shell jobs)
    (schemesh shell parse))

;; wraps shell DSL
(define-macro (shell . args)
  (sh-parse args))

(define-syntax shell-list
  (syntax-rules ()
    ((_)           '(sh-true))
    ((_ arg)          arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))

(define-syntax shell-subshell
  (syntax-rules ()
    ((_)         '(sh-true))
    ((_ arg0 ...) (sh-subshell arg0 ...))))

(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ;; FIXME: (sh-run/string) cannot be stopped - but neither can `...` in POSIX shells
    ((_ arg ...)       (lambda (job) (sh-run/string (shell arg ...))))))

(define-syntax shell-concat
  (syntax-rules ()
    ((_)          "")
    ((_ arg)      arg)
    ((_ arg0 arg1 ...) (lambda (job) (sh-concat job arg0 arg1 ...)))))

(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)      (lambda (job) (sh-env job arg)))))


) ; close library
