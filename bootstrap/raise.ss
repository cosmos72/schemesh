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

(library (schemesh bootstrap raise (0 7 5))
  (export
      raise-assert0 raise-assert1 raise-assert2 raise-assert3 raise-assert4 raise-assert5
      raise-assertf raise-assertl raise-errorf)
  (import
    (rnrs)
    (only (chezscheme) make-continuation-condition make-format-condition))


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
          (make-who-condition who)
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))


;; Raise a condition describing an assertion violation evaluating a form.
;; Condition format message is hardcoded, caller needs to provide:
;; * form - a string containing source code of the failed assertion
;; * form-values - values of each subform in form
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
          (make-who-condition who)
          (make-format-condition)
          (make-message-condition format-string)
          (make-irritants-condition format-args))))))




) ; close library
