;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; if this file is loaded multiple times, it reuses pre-existing values of bindings
;;   sh-current-environment sh-current-eval sh-globals sh-pid-table
;; found in (interaction-environment).
;;
;; implementation note:
;;   this is done by setting those top-level bindings only if they are not bound yet.

(library (schemesh bootstrap parameters)
  (export sh-current-environment)
  (import
    (rnrs)
    (only (chezscheme) define-top-level-value environment? environment-mutable? eval eval-when
                       interaction-environment logbit? procedure-arity-mask top-level-bound? top-level-value)
    (schemesh bootstrap first))


;; retrieve value of sh-current-environment set by bootstrap/first.ss
(define sh-current-environment (top-level-value 'sh-current-environment (interaction-environment)))


;; Thread parameter containing the eval function to use.
;; Will be called as ((sh-current-eval) obj environment).
;;
;; Initially set to Chez Scheme's eval, because it can also create definitions.
(unless (top-level-bound? 'sh-current-eval (sh-current-environment))
  (define-top-level-value 'sh-current-eval
    (sh-make-thread-parameter
      eval
      (lambda (proc)
        (unless (procedure? proc)
          (raise-errorf 'sh-current-eval "~s is not a procedure" proc))
        (unless (logbit? 2 (procedure-arity-mask proc))
          (raise-errorf 'sh-current-eval "~s is not a procedure accepting 2 arguments" proc))
        proc))
    (sh-current-environment)))


;; Parameter containing the global job corresponding to this process.
;; Jobs started with (sh-start) will be children of (sh-globals).
;;
;; May be parameterized to a different value in subshells.
(unless (top-level-bound? 'sh-globals (sh-current-environment))
  (define-top-level-value 'sh-globals (sh-make-thread-parameter #f) (sh-current-environment)))


;; Parameter containing the global hashtable pid -> job.
;;
;; May be parameterized to a different value in subshells.
(unless (top-level-bound? 'sh-pid-table (sh-current-environment))
  (define-top-level-value 'sh-pid-table
    (sh-make-parameter
      (make-eqv-hashtable)
      (lambda (htable)
        (unless (hashtable? htable)
          (raise-errorf 'sh-pid-table "~s is not a hashtable" htable))
        (unless (hashtable-mutable? htable)
          (raise-errorf 'sh-pid-table "~s is not a mutable hashtable" htable))
        (unless (eq? (hashtable-equivalence-function htable) eqv?)
          (raise-errorf 'sh-pid-table "~s is not an eqv hashtable" htable))
        htable))
    (sh-current-environment)))

) ; close library
