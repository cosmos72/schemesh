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

(library (schemesh bootstrap first)
  (export
     sh-make-parameter sh-make-thread-parameter raise-assertf raise-assertv raise-errorf)
  (import
    (rnrs)
    (only (chezscheme) define-top-level-value  environment? environment-mutable?
                       make-continuation-condition make-format-condition interaction-environment
                       top-level-bound? top-level-value))


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
;; otherwise calls (sh-make-parameter) above
(define sh-make-thread-parameter
  (if (top-level-bound? 'make-thread-parameter)
        (top-level-value 'make-thread-parameter)
        sh-make-parameter))


;; Raise a condition describing an assertion violation.
;; Condition format message and its arguments must be provided by caller.
(define (raise-assertf who format-string . format-args)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-assertion-violation)
          (make-continuation-condition k)
          (make-who-condition who)
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))


;; Raise a condition describing an assertion violation evaluating a form.
;; Condition format message is hardcoded, caller needs to provide:
;; * form - a string containing source code of the failed assertion
;; * form-values - values of each subform in form
(define (raise-assertv who form . form-values)
  (raise-assertf who "failed assertion ~a with arguments ~s" form form-values))


;; Raise a condition describing an error.
;; Condition format message and its arguments must be provided by caller.
(define (raise-errorf who format-string . format-args)
  (call/cc
    (lambda (k)
      (raise
        (condition
          (make-error)
          (make-continuation-condition k)
          (make-who-condition who)
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))



;; Thread parameter containing the scheme enviroment where to eval forms,
;; usually with (sh-eval) that calls ((sh-current-eval) form (sh-current-environment))
;;
;; Initially set to Chez Scheme's (interaction-environment), because it's mutable
;; and contains all r6rs and chezscheme bindings.
(unless (top-level-bound? 'sh-current-environment (interaction-environment))
  (define-top-level-value 'sh-current-environment
    (sh-make-thread-parameter
      (interaction-environment)
      (lambda (env)
        (unless (environment? env)
          (raise-errorf 'sh-current-environment "~s is not an environment" env))
        (unless (environment-mutable? env)
          (raise-errorf 'sh-current-environment "~s is not a mutable environment" env))
        env))
    (interaction-environment)))

) ; close library
