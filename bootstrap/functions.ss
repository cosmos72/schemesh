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

(library (schemesh bootstrap functions (0 8 0))
  (export
      generate-pretty-temporaries generate-pretty-temporary gensym-pretty

      raise-assert0 raise-assert1 raise-assert2 raise-assert3
      raise-assert4 raise-assert5 raise-assertf raise-assertl raise-errorf

      warn-check-failed0 warn-check-failed1 warn-check-failed2 warn-check-failed3
      warn-check-failed4 warn-check-failed5 warnf warn-check-failedl

      sh-make-parameter sh-make-thread-parameter sh-version)
  (import
    (rnrs)
    (only (chezscheme) console-error-port format gensym make-continuation-condition make-format-condition
                       interaction-environment top-level-bound? top-level-value))


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
  (apply format (warnf-port) format-string format-args))


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
  (if (top-level-bound? 'make-thread-parameter)
    (top-level-value 'make-thread-parameter)
    sh-make-parameter))


;; return schemesh version
(define (sh-version)
  (list 0 8 0))

) ; close library




;; (library-reexport) is a macro that expands to (library ...)
;; and defines a library that automatically exports all imported bindings.
;;
;; defining it in a library is not very useful, because one would need to (import) it
;; before calling (library-reexports) at top level
;;
(define-syntax library-reexport
  (lambda (stx)
    (syntax-case stx (import)
      ((k name (import . imported-library-names))
        (let ((export-list (apply append (map library-exports (datum imported-library-names)))))
          #`(library name
            (export . #,(datum->syntax (syntax k) export-list))
            (import . imported-library-names)))))))
