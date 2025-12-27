;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh shell macros (0 9 3))
  (export
    for-glob in-glob sh-include sh-include*
    shell shell-backquote shell-env shell-expr shell-glob shell-list shell-string shell-subshell shell-wildcard
    with-fd with-port with-resource with-parameterized-resource)
  (import
    (rnrs)
    (only (chezscheme)                 datum format fx1- meta parameterize reverse! void)
    (scheme2k bootstrap)
    (only (scheme2k containers list)   for-list in-list)
    (only (scheme2k containers macros) begin^ lambda^ let^ let-pairs)
    (only (scheme2k posix fd)          fd-close)
    (only (scheme2k posix pattern)     wildcard?)
    (schemesh shell job)
    (only (schemesh shell eval)        sh-read-file sh-dynamic-wind))

;; wraps shell DSL
(define-macro (shell . args)
  (sh-parse-datum (cons 'shell args)))


(define-syntax shell-backquote
  (syntax-rules ()
    ((_)           "")
    ;; NOTE: (sh-run/string-rtrim-newlines) cannot be stopped and resumed. But neither can $(...) or `...` in POSIX shells
    ((_ . args)    (lambda () (sh-run/string-rtrim-newlines (shell . args))))))


(define-syntax shell-env
  (syntax-rules ()
    ((_ arg)       (lambda (job) (sh-env-ref job arg)))))


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
  (define (%wildcard-simplify wildcards? ret args)
    (do ((args args (cdr args)))
        ((null? args) (values wildcards? ret))
      ; (debugf "... %wildcard-simplify ret=~s args=~s" (reverse ret) args)
      (let ((arg (car args)))
        (cond
          ((and (pair? arg) (eq? 'shell-wildcard (car arg)))
            (let-values (((inner-wildcards? inner-ret) (%wildcard-simplify wildcards? ret (cdr arg))))
              (set! wildcards? inner-wildcards?)
              (set! ret        inner-ret)))
          ((wildcard? arg)
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
    (let-values (((wildcards? rev-args) (%wildcard-simplify #f '() args)))
      (let ((args (reverse! rev-args)))
        (if wildcards?
          `(lambda (,job) (,proc ,job ,@args))
          (apply string-append args)))))

  ;; flatten nested macros (shell-wildcard ... (shell-wildcard ...) ...)
  ;; replace (shell-wildcard ...) containing only strings with the concatenation of those strings
  ;;
  ;; wrap non-constant (shell-wildcard ...) in a (,proc ,job-or-id ...)
  (define-macro (%shell-glob proc job-or-id . args)
    (let-values (((wildcards? rev-args) (%wildcard-simplify #f '() args)))
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
      ((_ . args)
        #`(%shell-wildcard wildcard1+ job . args)))))


;; extract the arguments inside a (shell-glob {...}) macro,
;; simplify them, and return a form that executes them from Scheme syntax and returns a list of strings.
;;
;; WARNING: will also execute commands found inside shell job substitution syntax `...` or $[...]
;;
;; Example: (shell-glob {~/*.txt}) expands to an (wildcard ...) form that, when executed,
;; returns a list of strings containing all the filesystem paths matching the shell glob pattern ~/*.txt
;;
;; If no filesystem path matches the shell glob pattern, when the form is executed
;; it will return an empty list.
;;;
;; If the argument of shell-glob is a NOT a shell glob pattern, then it must be a sequence of literal strings
;; and shell environment variable names, possibly starting with ~ or ~user that means a user's home directory.
;;
;; Example: (shell-glob {~bob}) expands to an (wildcard ...) form that, when executed,
;; return a list containing a single string: the home directory of user "bob"
(define-syntax shell-glob
  (lambda (stx)
    (syntax-case stx ()
      ((_ (macro-name arg))
        #'(shell-glob #f (macro-name arg)))
      ((_ job-or-id (macro-name (submacro-name arg)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-env))
        #`(%shell-glob wildcard job-or-id (sh-env-ref job-or-id arg)))
      ((_ job-or-id (macro-name (submacro-name . args)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-backquote))
        #`(%shell-glob wildcard job-or-id (shell-backquote . args)))
      ((_ job-or-id (macro-name (submacro-name . args)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-wildcard))
        #`(%shell-glob wildcard job-or-id . args)))))


;; extract the arguments inside a (shell-string {...}) macro,
;; simplify them, and return a form that will execute them from Scheme syntax and return a *single* string,
;; or raises an exception if the form would return multiple strings.
;;
;; WARNING: will also execute commands found inside shell job substitution syntax `...` or $[...]
;;
;; If the argument inside {...} must be either a shell glob pattern or a sequence of literal strings
;; and shell environment variable names, possibly starting with ~ or ~user that means a user's home directory.

;; Example: (shell-string {$FOO-$BAR}) expands to an (wildcard ...) form that, when executed,
;; returns a string containing the concatenation of:
;; 1. value of environment variable "FOO"
;; 2. string "-"
;; 3. value of environment variable "BAR"
;;
;; If the arguments inside {...} are a shell glob pattern, and they match a single filesystem entry,
;; such path is returned as string.
;;
;; Example: (shell-string {~/my/file.txt}) expands to a form that, when executed,
;; returns a string containing user's home directory followed by "/my/file.txt"
;;;
;; If the shell glob pattern matches multiple filesystem entries, the form will raise an exception when executed.
(define-syntax shell-string
  (lambda (stx)
    (syntax-case stx ()
      ((_ (macro-name arg))
        #'(shell-string #f (macro-name arg)))
      ((_ job-or-id (macro-name (submacro-name arg)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-env))
        #`(sh-env-ref job-or-id arg))
      ((_ job-or-id (macro-name (submacro-name . args)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-backquote))
        #`(%shell-glob wildcard1 job-or-id (shell-backquote . args)))
      ((_ job-or-id (macro-name (submacro-name . args)))
        (and (free-identifier=? #'macro-name #'shell )
             (free-identifier=? #'submacro-name #'shell-wildcard))
        #`(%shell-glob wildcard1 job-or-id . args)))))


;; (for-glob var glob body ...) iterates on shell paths produced by glob,
;; executing body repeatedly with var bound to each one.
;;
;; (for-globs ((var glob) ...) body ...) iterates in parallel on shell paths produced by zero or more glob ...,
;; executing body repeatedly with var ... bound to each element in each list.
;; Stops iterating when the shortest list produced by glob ... is exhausted.
;;
;; If no glob is specified, behave as (forever body ...)
;;
;; Added in schemesh 0.9.3
(define-syntax for-glob
  (syntax-rules ()
    ((_ ((var glob) ...) body ...)
      (for-list ((var (shell-glob glob)) ...)
         body ...))
    ((_ var glob body ...)
      (for-list ((var (shell-glob glob)))
         body ...))))


;; (in-glob ...) is a shortcut for (in-list (shell-glob ...))
;;
;; Added in schemesh 0.9.3
(define-syntax in-glob
  (syntax-rules ()
    ((_ . args)
      (in-list (shell-glob . args)))))


;; evaluate body ... with variables var ... bound to expr ..., then always call (close expr-value) ...
;; even if body ... raises a condition or calls a continuation
(define-syntax with-resource
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #`(begin^ body ...))
      ((_ ((var expr close-proc) ...) body ...)
        (with-syntax (((obj   ...) (generate-pretty-temporaries #'(var ...)))
                      ((close ...) (generate-pretty-temporaries #'(close-proc ...))))
          #`(let-pairs ((obj expr close close-proc) ...)
              (sh-dynamic-wind
                void
                (lambda () (let^ ((var obj) ...) body ...))
                void
                (lambda () (close obj) ...))))))))



;; evaluate body ... with parameters param ... set to expr ...,
;; then always restore param ... and call (close expr-value) ...,
;; even if body ... raises a condition or calls a continuation
(define-syntax with-parameterized-resource
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #`(begin^ body ...))
      ((_ ((param expr close) ...) body ...)
        (with-syntax (((tparam ...) (generate-pretty-temporaries #'(param ...)))
                      ((tobj   ...) (generate-pretty-temporaries #'(expr  ...)))
                      ((tclose ...) (generate-pretty-temporaries #'(close ...)))
                      ((tsave  ...) (generate-pretty-temporaries #'(expr  ...)))
                      ((tcurr  ...) (generate-pretty-temporaries #'(param ...))))
          #`(let* ((tparam param) ...
                   (tobj   expr)  ...
                   (tclose close) ...
                   (tsave  tobj)  ...
                   (swap
                     (lambda ()
                       (let ((tcurr (tparam)) ...)
                         (tparam tsave) ...
                         (set! tsave tcurr) ...))))
              (sh-dynamic-wind
                swap
                (lambda^ () body ...)
                swap
                (lambda () (tclose tobj) ...))))))))


;; evaluate body ... with variables var ... bound to expr ..., then always call (fd-close var) ...,
;; even if body ... raises a condition or calls a continuation
(define-syntax with-fd
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ ((var expr) ...) body ...)
      (with-resource ((var expr fd-close) ...) body ...))))


;; evaluate body ... with variables var ... bound to expr ..., then always call (close-port var) ...,
;; even if body ... raises a condition or calls a continuation
(define-syntax with-port
  (syntax-rules ()
    ((_ () body ...)
      (begin^ body ...))
    ((_ ((var expr) ...) body ...)
      (with-resource ((var expr close-port) ...) body ...))))


) ; close library
