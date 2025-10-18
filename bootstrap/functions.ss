;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; if this file is loaded multiple times, only the first one has any effect.
;; implementation note:
;; this is done by setting the top-level symbols sh-current-environment and sh-current-eval
;; only if they are not bound yet.

#!r6rs

(library (schemesh bootstrap functions (0 9 2))
  (export
      check-interrupts eval-form fx<=?* nop parameter-swapper
      generate-pretty-temporaries generate-pretty-temporary gensym-pretty

      raise-assert0 raise-assert1 raise-assert2 raise-assert3
      raise-assert4 raise-assert5 raise-assertf raise-assertl raise-errorf

      warn-check-failed0 warn-check-failed1 warn-check-failed2 warn-check-failed3
      warn-check-failed4 warn-check-failed5 warnf warn-check-failedl

      sh-make-parameter sh-make-thread-parameter sh-make-volatile-parameter sh-version sh-version-number)
  (import
    (rnrs)
    (only (chezscheme) $primitive console-error-port eval format gensym import make-continuation-condition
                       make-format-condition meta-cond interaction-environment library-exports
                       string->immutable-string top-level-bound? top-level-value void))


;; immediately check if an event occurred:
;; * an interrupt from the keyboard
;; * a POSIX signal with a register-signal-handler for it
;; * the expiration of an internal timer set by (set-timer)
;; * a breakpoint caused by a call to (break)
;; * a request from the storage manager to initiate a garbage collection
;;
;; depending on the event and on user's commands, may or may not return.
(define check-interrupts ($primitive 3 $event))


(define (can-eval-whole? form env)
  (if (not (and (pair? form) (symbol? (car form))))
    #t
    (let ((sym (car form)))
      (if (eq? 'begin sym)
        ;; recursively check forms inside (begin ...)
        (do ((tail (cdr form) (cdr tail)))
            ((or (null? tail)
                 (not (can-eval-whole? (car tail) env)))
              (null? tail)))
        ;; macros are (top-level-syntax?) but not (top-level-bound?)
        (top-level-bound? (car form) env)))))


(define (eval-one-by-one form env)
  (if (and (pair? form) (eq? 'begin (car form)))
    (do ((tail (cdr form) (cdr tail)))
        ((null? (cdr tail))
          (eval-one-by-one (car tail) env)) ; return value(s) of last eval
      (eval-one-by-one (car tail) env))
    (eval form env)))



;; enhanced variant of Chez Scheme eval:
;; if form recursively contains a (begin ...) with one or more macros,
;; evaluate the sub-forms one-by-one in order.
;;
;; Reason: we want to allow things like
;; (begin
;;   (library-directories my-dir)
;;   (import (srfi :197))
;;   (chain 42 (+ 1 _)))
;;
;; where (import (srfi ...)) works only if macroexpanded *after*
;; calling the procedure (library-directories ...)
;;
;; See also https://github.com/cosmos72/schemesh/issues/28
(define (eval-form form env)
  (if (can-eval-whole? form env)
    (eval form env)
    (eval-one-by-one form env)))


;; version of fx<=? that does not allocate, and allows up to 6 arguments
(define fx<=?*
  (case-lambda
    ((a b)         (fx<=? a b))
    ((a b c)       (fx<=? a b c))
    ((a b c d)     (and (fx<=? a b c) (fx<=? c d)))
    ((a b c d e)   (and (fx<=? a b c) (fx<=? c d e)))
    ((a b c d e f) (and (fx<=? a b c) (fx<=? c d e) (fx<=? e f)))))


;; ignore all arguments, do nothing and return (void)
(define nop
  (case-lambda
    (() (void))
    ((a) (void))
    ((a b) (void))
    ((a b c) (void))
    ((a b c d) (void))
    ((a b c d e) (void))
    ((a b c d e f) (void))
    ((a b c d e f g . args) (void))))



(define (gensym-pretty x)
  (cond
    ((string? x) (gensym x))
    ((symbol? x) (gensym (symbol->string x)))
    ((number? x) (gensym (number->string x)))
    ((pair? x)
      (gensym-pretty
        (let ((a (car x))
              (b (cdr x)))
          (cond
            ((null? b)      a)
            ((eq? 'quote a) b)
            (else           a)))))
    (else
      (gensym))))


(define (generate-pretty-temporary stx)
  (datum->syntax #'stx (gensym-pretty (syntax->datum stx))))


(define (generate-pretty-temporaries l)
  (map generate-pretty-temporary l))


;; Raise a condition describing an assertion violation.
;; Condition format message and its arguments must be provided by caller.
(define (raise-assertf who format-string . format-args)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-assertion-violation)
          (make-continuation-condition k)
          (make-non-continuable-violation)
          (make-who-condition (if (symbol? who) who (format #f "~s" who)))
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))


;; Raise a condition describing an assertion violation evaluating a form.
;; Condition format message is hardcoded, caller needs to provide:
;; * form - a string containing source code of the failed assertion
;; * form-values - values of each subform in the failed assertion
(define (raise-assert0 who form)
  (raise-assertf who "failed assertion ~a" form))
(define (raise-assert1 who form arg1)
  (raise-assertf who "failed assertion ~a with argument ~s" form arg1))
(define (raise-assert2 who form arg1 arg2)
  (raise-assertf who "failed assertion ~a with arguments ~s ~s" form arg1 arg2))
(define (raise-assert3 who form arg1 arg2 arg3)
  (raise-assertf who "failed assertion ~a with arguments ~s ~s ~s" form arg1 arg2 arg3))
(define (raise-assert4 who form arg1 arg2 arg3 arg4)
  (raise-assertf who "failed assertion ~a with arguments ~s ~s ~s ~s" form arg1 arg2 arg3 arg4))
(define (raise-assert5 who form arg1 arg2 arg3 arg4 arg5)
  (raise-assertf who "failed assertion ~a with arguments ~s ~s ~s ~s ~s" form arg1 arg2 arg3 arg4 arg5))
(define (raise-assertl who form args)
  (raise-assertf who "failed assertion ~a with arguments ~s" form args))


;; Raise a condition describing an error.
;; Condition format message and its arguments must be provided by caller.
(define (raise-errorf who format-string . format-args)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-error)
          (make-continuation-condition k)
          (make-non-continuable-violation)
          (make-who-condition (if (symbol? who) who (format #f "~s" who)))
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))


(define (warnf-port)
  (console-error-port))

;; Display a warning message to (warnf-port).
(define (warnf format-string . format-args)
  (let ((out (warnf-port)))
    (apply format out format-string format-args)
    (flush-output-port out)))


;; Display a warning message describing a failed check evaluating a form.
;; Message format is hardcoded, caller needs to provide:
;; * who  - a string or symbol identifying the caller. helps finding the location of the failed check.
;; * form - a string containing source code of the failed check
;; * arg... - values of each subform in the failed check
(define (warn-check-failed0 who form)
  (warnf "; warning in ~a: failed check ~a\n" who form))
(define (warn-check-failed1 who form arg1)
  (warnf "; warning in ~a: failed check ~a with argument ~s\n" who form arg1))
(define (warn-check-failed2 who form arg1 arg2)
  (warnf "; warning in ~a: failed check ~a with arguments ~s ~s\n" who form arg1 arg2))
(define (warn-check-failed3 who form arg1 arg2 arg3)
  (warnf "; warning in ~a: failed check ~a with arguments ~s ~s ~s\n" who form arg1 arg2 arg3))
(define (warn-check-failed4 who form arg1 arg2 arg3 arg4)
  (warnf "; warning in ~a: failed check ~a with arguments ~s ~s ~s ~s\n" who form arg1 arg2 arg3 arg4))
(define (warn-check-failed5 who form arg1 arg2 arg3 arg4 arg5)
  (warnf "; warning in ~a: failed check ~a with arguments ~s ~s ~s ~s ~s\n" who form arg1 arg2 arg3 arg4 arg5))
(define (warn-check-failedl who form args)
  (warnf "; warning in ~a: failed check ~a with arguments ~s\n" who form args))


;; low-level alternative to (parameterize):
;; create and return a closure that swaps value of parameter (param)
;; with value-to-set each time it is invoked.
;;
;; For example, the two fragments below are equivalent:
;;
;; (parameterize ((current-output-port my-port))
;;   body ...)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (let ((swap (parameter-swapper current-output-port my-port)))
;;   (dynamic-wind
;;     swap
;;     (lambda () body ...)
;;     swap))
;;
(define (parameter-swapper param value-to-set)
  (lambda ()
    (let ((current (param)))
      (param value-to-set)
      (set! value-to-set current))))


;; portable reimplementation of Chez Scheme (make-parameter)
(define sh-make-parameter
  (case-lambda
    ((initial-value updater-proc)
      (let ((current-value (updater-proc initial-value)))
        (case-lambda
          (() current-value)
          ((new-value) (set! current-value (updater-proc new-value))))))
    ((initial-value)
      (sh-make-parameter initial-value (lambda (x) x)))))


;; approximate reimplementation of Chez Scheme make-thread-parameter:
;; calls (make-thread-parameter) if available,
;; otherwise calls (sh-make-parameter) above.
(define sh-make-thread-parameter
  (meta-cond
    ((memq 'make-thread-parameter (library-exports '(chezscheme)))
      (let ()
         (import (prefix (only (chezscheme) make-thread-parameter)
                         chez:))
         chez:make-thread-parameter))
    (else
      sh-make-parameter)))


;; alternate version of (make-parameter), where parameter value is expected to change
;; due to external reasons and must be retrieved each time by calling a procedure.
(define (sh-make-volatile-parameter getter-proc updater-proc)
  (case-lambda
    (()          (getter-proc))
    ((new-value) (updater-proc new-value))))


;; return Schemesh version string
(define sh-version
  (let ((ret (string->immutable-string "Schemesh Version 0.9.2")))
    (lambda ()
      ret)))


;; return three values: schemesh version MAJOR, MINOR and PATCH fixnums
(define (sh-version-number)
  (values 0 9 2))

) ; close library




;; (library-reexport) is a macro that expands to (library ...)
;; and defines a library that automatically exports all imported bindings.
;;
;; defining it in a library is not very useful, because one would need to (import) it
;; before calling (library-reexport) at top level
;;
(define-syntax library-reexport
  (let ((library-exports-with-excepts
          (lambda (library-name)
            (if (and (pair? library-name) (eq? 'except (car library-name)))
              (let ((except-syms (cddr library-name)))
                (filter (lambda (sym)
                          (not (memq sym except-syms)))
                  (library-exports (cadr library-name))))
              (library-exports library-name)))))
    (lambda (stx)
      (syntax-case stx (import)
        ((k name (import . imported-library-names))
          (let ((export-list (apply append (map library-exports-with-excepts (datum imported-library-names)))))
            #`(library name
              (export . #,(datum->syntax (syntax k) export-list))
              (import . imported-library-names))))))))
