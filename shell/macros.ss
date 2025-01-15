;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell macros (0 1))
  (export shell shell-backquote shell-env shell-list shell-subshell shell-wildcard)
  (import
    (rnrs)
    (only (chezscheme) meta reverse!)
    (schemesh bootstrap)
    (only (schemesh posix pattern) sh-wildcard?)
    (schemesh shell jobs))

;; wraps shell DSL
(define-macro (shell . args)
  (sh-parse (cons 'shell args)))

(define-macro (shell-subshell . args)
  (sh-parse (cons 'shell-subshell args)))


(define-syntax shell-list
  (syntax-rules ()
    ((_)               (sh-cmd))
    ((_ arg)           arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))

(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ;; NOTE: (sh-run/string-rtrim-newlines) cannot be stopped and resumed. But neither can `...` in POSIX shells
    ((_ arg ...)       (lambda (job) (sh-run/string-rtrim-newlines (shell arg ...))))))

(meta begin
  (define (%sh-wildcard-flatten ret args)
    (do ((args args (cdr args)))
        ((null? args) ret)
      ; (debugf "... %sh-wildcard-flatten ret=~s args=~s" (reverse ret) args)
      (let ((arg (car args)))
        (cond
          ((and (pair? arg) (eq? 'shell-wildcard (car arg)))
            (set! ret (%sh-wildcard-flatten ret (cdr arg))))
          ((sh-wildcard? arg)
            (set! ret (cons (list 'quote arg) ret)))
          (#t
            (set! ret (cons arg ret)))))))

  ;; flatten nested macros (shell-wildcard ... (shell-wildcard ...) ...)
  (define-macro (%shell-wildcard-flatten . args)
    (reverse! (%sh-wildcard-flatten '() args)))

  (define (%is-wildcard? arg)
    ; (debugf "%is-wildcard? arg=~s" arg)
    (memq arg '(* ? ~)))

) ; close meta

(define-syntax shell-wildcard
  (lambda (stx)
    (syntax-case stx ()
      ((_)
         "")
      ((_ arg)
        ; single-argument (shell-syntax arg) can be simplified to arg
        ; unless arg it's a wildcard i.e. one of * ? ~
        (not (%is-wildcard? (syntax->datum (syntax arg))))
        #`arg)
      ((_ arg0 ...) #`(lambda (job) (%shell-wildcard-flatten sh-wildcard job arg0 ...))))))

(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)      (lambda (job) (sh-env job arg)))))


) ; close library
