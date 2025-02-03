;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; TODO: is there a better solution not requiring (sh-eval) ?
(define-syntax import-schemesh/minimal
  (lambda (stx)
    (import (only (chezscheme) eval void))
    (syntax-case stx ()
      ((_)
        ; import libraries at macroexpansion time, not at runtime
        (eval '(import (schemesh shell) (schemesh repl)))
        #'(void)))))


(define-syntax import-schemesh/all
  (lambda (stx)
    (import (only (chezscheme) eval void))
    (syntax-case stx ()
      ((_)
        ; import libraries at macroexpansion time, not at runtime
        (eval
          '(import
             (schemesh bootstrap)
             (schemesh containers)
             (schemesh conversions)
             (schemesh lineedit)
             (schemesh parser)
             (schemesh posix)
             (schemesh repl)
             (schemesh shell)))
        #'(void)))))


;; when reloading libschemesh.ss, reimport (schemesh shell) and (schemesh repl)
;; fixes error "compiled program requires a different compilation instance of (schemesh ...)""
(eval-when (eval)
  (let ()
    (import (rnrs) (only (chezscheme) top-level-bound? eval))

    (when (top-level-bound? 'sh-persistent-parameters)
      (eval '(import (schemesh shell))))
    (when (top-level-bound? 'sh-repl)
      (eval '(import (schemesh repl))))))
