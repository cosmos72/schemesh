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

(eval-when (compile load eval)

(let ()

;; imports are local to scope (let () ...) above
(import
  (rnrs)
  (only (chezscheme) environment? environment-mutable? eval
                     interaction-environment logbit? make-parameter make-thread-parameter))

;; (%raise-errorf) is local to scope (let () ...) above
(define (%raise-errorf who format-string . format-args)
  (raise
    (condition
      (make-error)
      (make-who-condition who)
      (make-format-condition)
      (make-message-condition format-string)
      (make-irritants-condition format-args))))


;; Thread parameter containing the scheme enviroment where to eval forms,
;; usually with (sh-eval) that calls ((sh-current-eval) form (sh-current-environment))
;;
;; Initially set to Chez Scheme's (interaction-environment), because it's mutable
;; and contains all r6rs and chezscheme bindings.
(unless (top-level-bound? 'sh-current-environment)
  (set! sh-current-environment
    (make-thread-parameter
      (interaction-environment)
      (lambda (env)
        (unless (environment? env)
          (%raise-errorf 'sh-current-environment "~s is not an environment" env))
        (unless (environment-mutable? env)
          (%raise-errorf 'sh-current-environment "~s is not a mutable environment" env))
        env))))


;; Thread parameter containing the eval function to use.
;; Will be called as ((sh-current-eval) obj environment).
;;
;; Initially set to Chez Scheme's eval, because it can also create definitions.
(unless (top-level-bound? 'sh-current-eval)
  (set! sh-current-eval
    (make-thread-parameter
      eval
      (lambda (proc)
        (unless (procedure? proc)
          (%raise-errorf 'sh-current-eval "~s is not a procedure" proc))
        (unless (logbit? 2 (procedure-arity-mask proc))
          (%raise-errorf 'sh-current-eval "~s is not a procedure accepting 2 arguments" proc))
        proc))))


;; Parameter containing the global job corresponding to this process.
;; Jobs started with (sh-start) will be children of (sh-globals).
;;
;; May be parameterized to a different value in subshells.
(unless (top-level-bound? 'sh-globals)
  ; (set! sh-globals (make-thread-parameter #f))) ; fails with "attempt to assign immutable variable sh-globals"
  (eval '(set! sh-globals (make-parameter #f))
        (sh-current-environment)))


;; Parameter containing the global hashtable pid -> job.
;;
;; May be parameterized to a different value in subshells.
(unless (top-level-bound? 'sh-pid-table)
  (set! sh-pid-table
    (make-parameter
      (make-eqv-hashtable)
      (lambda (htable)
        (unless (hashtable? htable)
          (%raise-errorf 'sh-pid-table "~s is not a hashtable" htable))
        (unless (hashtable-mutable? htable)
          (%raise-errorf 'sh-pid-table "~s is not a mutable hashtable" htable))
        (unless (eq? (hashtable-equivalence-function htable) eqv?)
          (%raise-errorf 'sh-pid-table "~s is not an eqv hashtable" htable))
        htable))))


) ; close let
) ; close eval-when
