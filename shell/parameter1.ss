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

(library (schemesh shell parameter1 (0 8 1))
  (export
      sh-persistent-parameters)
  (import
    (rnrs)
    (only (chezscheme)  define-top-level-value environment? environment-mutable? eval fx1+
                        interaction-environment logbit? procedure-arity-mask top-level-bound? top-level-value)
    (only (schemesh bootstrap) sh-make-parameter sh-make-thread-parameter raise-errorf))


;; Create and return thread parameter containing the scheme environment where to eval forms,
;; usually with (sh-eval) that calls ((sh-current-eval) form (sh-current-environment))
;;
;; Initially set to Chez Scheme's (interaction-environment), because it's mutable
;; and contains all r6rs and chezscheme bindings.
(define (make-parameter-environment)
  (sh-make-thread-parameter
    (interaction-environment)
    (lambda (env)
      (unless (environment? env)
        (raise-errorf 'sh-current-environment "~s is not an environment" env))
      (unless (environment-mutable? env)
        (raise-errorf 'sh-current-environment "~s is not a mutable environment" env))
      env)))


;; Create and return thread parameter containing the eval function to use.
;; Will be called as ((sh-current-eval) obj environment).
;;
;; Initially set to Chez Scheme's eval, because it can also create definitions.
(define (make-parameter-eval)
  (sh-make-thread-parameter
    eval
    (lambda (proc)
      (unless (procedure? proc)
        (raise-errorf 'sh-current-eval "~s is not a procedure" proc))
      (unless (logbit? 2 (procedure-arity-mask proc))
        (raise-errorf 'sh-current-eval "~s is not a procedure accepting 2 arguments" proc))
      proc)))

;; Create and return thread parameter containing the global job corresponding to this process.
;; Jobs started with (sh-start) will be children of (sh-globals).
;;
;; May be parameterized to a different value in subshells.
(define (make-parameter-globals)
  (sh-make-thread-parameter #f))


;; Create and return parameter containing the global hashtable pid -> job.
;;
;; May be parameterized to a different value in subshells.
(define (make-parameter-pid-table)
  (sh-make-parameter
    (make-eqv-hashtable)
    (lambda (htable)
      (unless (hashtable? htable)
        (raise-errorf 'sh-pid-table "~s is not a hashtable" htable))
      (unless (hashtable-mutable? htable)
        (raise-errorf 'sh-pid-table "~s is not a mutable hashtable" htable))
      (unless (eq? (hashtable-equivalence-function htable) eqv?)
        (raise-errorf 'sh-pid-table "~s is not an eqv hashtable" htable))
      htable)))

;; Return vector #(sh-schemesh-reload-count sh-current-environment sh-current-eval sh-globals sh-pid-table)
;;
;; Carefully access symbol sh-persistent-parameters* in (interaction-environment)
;; to preserve vector across libschemesh library reloads.
(define sh-persistent-parameters
  (let* ((params
           (if (top-level-bound? 'sh-persistent-parameters* (interaction-environment))
             ;; retrieve existing parameters vector
             ((top-level-value 'sh-persistent-parameters* (interaction-environment)))
             ;; create a new parameters vector
             (vector (make-parameter-environment)
                     (make-parameter-eval)
                     (make-parameter-globals)
                     (make-parameter-pid-table)
                     0     ; sh-schemesh-reload-count
                     #f))) ; repl-restart?
          ;; create closure around parameters vector
         (persistent-parameters (lambda () params)) ; displayed as #<procedure persistent-parameters>
         (proc persistent-parameters))              ; short alias

    ;; always overwrite symbol sh-persistent-parameters* in (interaction-environment),
    ;; it should help garbage collector to discard previously loaded libschemesh library.
    (define-top-level-value 'sh-persistent-parameters* proc (interaction-environment))
    ;; always increase by 1 sh-schemesh-reload-count
    (vector-set! (proc) 4 (fx1+ (vector-ref (proc) 4)))
    ;; define sh-persistent-parameters as the closure created around parameters vector
    proc))


) ; close library
