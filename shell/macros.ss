;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
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
  (sh-parse-datum (cons 'shell args)))

(define-macro (shell-subshell . args)
  (sh-parse-datum (cons 'shell-subshell args)))


(define-syntax shell-list
  (syntax-rules ()
    ((_)               (sh-cmd))
    ((_ arg)           arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))

(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ;; NOTE: (sh-run/string-rtrim-newlines) cannot be stopped and resumed. But neither can `...` in POSIX shells
    ((_ arg ...)       (lambda () (sh-run/string-rtrim-newlines (shell arg ...))))))

(meta begin
  (define (%sh-wildcard-simplify wildcards? ret args)
    (do ((args args (cdr args)))
        ((null? args) (values wildcards? ret))
      ; (debugf "... %sh-wildcard-simplify ret=~s args=~s" (reverse ret) args)
      (let ((arg (car args)))
        (cond
          ((and (pair? arg) (eq? 'shell-wildcard (car arg)))
            (let-values (((inner-wildcards? inner-ret) (%sh-wildcard-simplify wildcards? ret (cdr arg))))
              (set! wildcards? inner-wildcards?)
              (set! ret        inner-ret)))
          ((sh-wildcard? arg)
            (set! wildcards? #t)
            (set! ret (cons (list 'quote arg) ret)))
          (#t
            (unless (string? arg)
              (set! wildcards? #t))
            (set! ret (cons arg ret)))))))

  ;; flatten nested macros (shell-wildcard ... (shell-wildcard ...) ...)
  ;; replace (shell-wildcard ...) containing only strings with the concatenation of those strings
  (define-macro (%shell-wildcard-simplify proc job . args)
    (let-values (((wildcards? rev-args) (%sh-wildcard-simplify #f '() args)))
      (let ((args (reverse! rev-args)))
        (if wildcards?
          `(lambda (,job) (,proc ,job ,@args))
          (apply string-append args)))))

) ; close meta

(define-syntax shell-wildcard
  (lambda (stx)
    (syntax-case stx ()
      ((_)
         "")
      ((_ arg)
        ; single-argument (shell-wildcard arg) can be simplified to arg
        ; if arg is a string
        (string? (syntax->datum (syntax arg)))
        #`arg)
      ((_ arg0 ...) #`(%shell-wildcard-simplify sh-wildcard job arg0 ...)))))

(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)      (lambda (job) (sh-env job arg)))))


) ; close library
