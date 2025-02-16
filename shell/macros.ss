;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell macros (0 7 5))
  (export
    include/lang include/lang*
    shell shell-backquote shell-env shell-list shell-subshell shell-expr shell-wildcard)
  (import
    (rnrs)
    (only (chezscheme) datum meta reverse!)
    (schemesh bootstrap)
    (only (schemesh posix pattern) sh-wildcard?)
    (schemesh shell builtins)
    (schemesh shell job)
    (only (schemesh shell eval) sh-read-file))

;; wraps shell DSL
(define-macro (shell . args)
  (sh-parse-datum (cons 'shell args)))


(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ;; NOTE: (sh-run/string-rtrim-newlines) cannot be stopped and resumed. But neither can $(...) or `...` in POSIX shells
    ((_ arg ...)       (lambda (job) (sh-run/string-rtrim-newlines (shell arg ...) (cons 'same-parent-as-job job))))))


(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)      (lambda (job) (sh-env-ref job arg)))))


;; macro: read specified file path, parse it with (sh-read-file)
;; and execute its contents at macroexpansion time.
;;
;; optional arguments:
;;   initial-parser - one of the symbols: scheme shell r6rs
;;   enables-parsers - a list containing one or more symbols among: scheme shell r6rs
(define-syntax include/lang
  (lambda (x)
    (syntax-case x ()
      ((k path)
         (datum->syntax #'k (sh-read-file (datum path))))

      ((k path initial-parser)
         (datum->syntax #'k (sh-read-file (datum path) (datum initial-parser))))

      ((k path initial-parser enabled-parsers)
         (datum->syntax #'k (sh-read-file (datum path) (datum initial-parser) (datum enabled-parsers)))))))


;; macro: read specified file path, parse it with (sh-read-file)
;; and execute its contents at macroexpansion time.
;;
;; same as (include/lang*), with the difference that all arguments are mandatory
(define-syntax include/lang*
  (lambda (x)
    (syntax-case x ()
      ((k path initial-parser enabled-parsers)
        (datum->syntax #'k (sh-read-file (datum path) (datum initial-parser) (datum enabled-parsers)))))))


(define-syntax shell-list
  (syntax-rules ()
    ((_)               (sh-cmd))
    ((_ arg)           arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))


(define-macro (shell-subshell . args)
  (sh-parse-datum (cons 'shell-subshell args)))


;; macro: create a (sh-cmd) that evaluates specified Scheme expressions when executed,
;; and returns success i.e. (void) if last expression is truish,
;; or failure i.e '(exited . 1) if expression is false.
(define-syntax shell-expr
  (syntax-rules ()
    ((_ expr exprs ...) (sh-cmd* "builtin" "expr" (lambda () (sh-bool (begin expr exprs ...)))))))


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



) ; close library
