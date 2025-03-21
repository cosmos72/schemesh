;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh shell macros (0 8 1))
  (export
    in-shell-glob sh-include sh-include*
    shell shell-backquote shell-env shell-expr shell-glob shell-list shell-subshell shell-wildcard)
  (import
    (rnrs)
    (only (chezscheme) datum format fx1- meta parameterize reverse!)
    (schemesh bootstrap)
    (only (schemesh containers list) in-list)
    (only (schemesh posix pattern) sh-wildcard?)
    (schemesh shell job)
    (only (schemesh shell eval) sh-read-file))

;; wraps shell DSL
(define-macro (shell . args)
  (sh-parse-datum (cons 'shell args)))


(define-syntax shell-backquote
  (syntax-rules ()
    ((_)               "")
    ;; NOTE: (sh-run/string-rtrim-newlines) cannot be stopped and resumed. But neither can $(...) or `...` in POSIX shells
    ((_ arg ...)       (lambda () (sh-run/string-rtrim-newlines (shell arg ...))))))


(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)      (lambda (job) (sh-env-ref job arg)))))


;; macro: read specified file path, parse it with (sh-read-file)
;; and execute its contents at macroexpansion time.
;;
;; optional arguments:
;;   initial-parser - one of the symbols: scheme shell r6rs
;;   enables-parsers - a list containing one or more symbols among: scheme shell r6rs
(define-syntax sh-include
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
;; same as (sh-include*), with the difference that all arguments are mandatory
(define-syntax sh-include*
  (lambda (x)
    (syntax-case x ()
      ((k path initial-parser enabled-parsers)
        (datum->syntax #'k (sh-read-file (datum path) (datum initial-parser) (datum enabled-parsers)))))))


;; macro: create a (sh-expr) that evaluates specified Scheme expressions
;; when executed, and returns the value of last expression.
(define-syntax shell-expr
  (lambda (stx)
    (syntax-case stx ()
      ((_ expr)
        (let ((label (format #f "~s" (datum expr))))
          (if (fx<=? (string-length label) 80)
            #`(sh-expr (lambda () expr) #,label)
            #`(sh-expr (lambda () expr)))))
      ((_ expr exprs ...)
        #`(shell-expr (begin expr exprs ...))))))


(define-syntax shell-list
  (syntax-rules ()
    ((_)               (sh-cmd))
    ((_ arg)           arg)
    ((_ arg0 arg1 ...) (sh-list arg0 arg1 ...))))


(define-macro (shell-subshell . args)
  (sh-parse-datum (cons 'shell-subshell args)))


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
          (else
            (unless (string? arg)
              (set! wildcards? #t))
            (set! ret (cons arg ret)))))))

  ;; flatten nested macros (shell-wildcard ... (shell-wildcard ...) ...)
  ;; replace (shell-wildcard ...) containing only strings with the concatenation of those strings
  ;;
  ;; wrap non-constant (shell-wildcard ...) in a (lambda (,job) (,proc ,job ...))
  (define-macro (%shell-wildcard proc job . args)
    (let-values (((wildcards? rev-args) (%sh-wildcard-simplify #f '() args)))
      (let ((args (reverse! rev-args)))
        (if wildcards?
          `(lambda (,job) (,proc ,job ,@args))
          (apply string-append args)))))

  ;; flatten nested macros (shell-wildcard ... (shell-wildcard ...) ...)
  ;; replace (shell-wildcard ...) containing only strings with the concatenation of those strings
  ;;
  ;; wrap non-constant (shell-wildcard ...) in a (,proc ,job-or-id ...)
  (define-macro (%shell-glob proc job-or-id . args)
    (let-values (((wildcards? rev-args) (%sh-wildcard-simplify #f '() args)))
      (let ((args (reverse! rev-args)))
        (if wildcards?
          `(,proc ,job-or-id ,@args)
          (apply string-append args)))))


) ; close meta

;; simplify a tree of (shell-wildcard) calls
;; and wrap them inside a (lambda (job) ..) if they contain wildcards
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
      ((_ arg ...)
        #`(%shell-wildcard sh-wildcard job arg ...)))))


;; extract the arguments inside a (shell (shell-wildcard ...)) macro,
;; simplify them, and wrap them in a (sh-wildcard) for executing them.
(define-syntax shell-glob
  (lambda (stx)
    (syntax-case stx ()
      ((_ (macro-name arg))
        #'(shell-glob #f (macro-name arg)))
      ((_ job-or-id (macro-name (submacro-name arg ...)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-wildcard))
        #`(%shell-glob sh-wildcard job-or-id arg ...)))))


;; (in-shell-glob ...) is a shortcut for (in-list (shell-glob ...))
(define-syntax in-shell-glob
  (syntax-rules ()
    ((_ . args)
      (in-list (shell-glob . args)))))

) ; close library
