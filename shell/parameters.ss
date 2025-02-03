;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; if this file is loaded multiple times, only the first one has any effect.
;; implementation note:
;; this is done by setting the top-level symbol sh-persistent-parameters
;; only if it's not bound yet, and by retrieving its value if it's bound.

(library (schemesh shell parameters)
  (export
      ;; parameter1.ss
      sh-persistent-parameters

      ;; parameters.ss
      sh-current-environment sh-current-eval sh-globals sh-pid-table
      sh-schemesh-reload-count sh-repl-restart sh-repl-restart?
      sh-eval sh-eval-string sh-eval->bytevector)
  (import
    (rnrs)
    (only (schemesh bootstrap) sh-make-parameter sh-make-thread-parameter raise-errorf)
    (only (schemesh conversions) any->bytevector)
    (only (schemesh shell parameter1) sh-persistent-parameters))


;; retrieve parameter sh-current-environment set by parameters/parameter1.ss
(define sh-current-environment (vector-ref (sh-persistent-parameters) 0))

;; retrieve parameter sh-current-eval set by parameters/parameter1.ss
(define sh-current-eval (vector-ref (sh-persistent-parameters) 1))

;; retrieve parameter sh-globals set by parameters/parameter1.ss
(define sh-globals (vector-ref (sh-persistent-parameters) 2))

;; retrieve parameter sh-pid-table set by parameters/parameter1.ss
(define sh-pid-table (vector-ref (sh-persistent-parameters) 3))

;; retrieve integer sh-schemesh-reload-count set by parameters/parameter1.ss
(define (sh-schemesh-reload-count) (vector-ref (sh-persistent-parameters) 4))

;; retrieve boolean flag sh-repl-restart? set by parameters/parameter1.ss
(define (sh-repl-restart?) (vector-ref (sh-persistent-parameters) 5))

;; set to #t or #f the boolean flag sh-repl-restart?
(define sh-repl-restart
  (case-lambda
    (() (sh-repl-restart #t))
    ((flag) (vector-set! (sh-persistent-parameters) 5 (not (not flag))))))


;; evaluate a form with (sh-current-eval) in specified environment,
;; which is (sh-current-environment) by default
(define sh-eval
  (case-lambda
    ((form)     ((sh-current-eval) form (sh-current-environment)))
    ((form env) ((sh-current-eval) form env))))


;; parse and evaluate a string with (sh-eval).
(define (sh-eval-string str)
  (sh-eval (read (open-string-input-port str))))


;; parse and evaluate a string with (sh-eval),
;; then convert result to bytevector.
(define (sh-eval->bytevector str)
  (any->bytevector (sh-eval-string str)))

) ; close library
