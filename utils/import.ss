;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;; TODO: is there a better solution not requiring (sh-eval) ?
(define-syntax import-schemesh/minimal
  (lambda (stx)
    (import (only (chezscheme) void)
            (only (schemesh bootstrap) sh-eval))
    (syntax-case stx ()
      ((_)
        ; import libraries when (import-schemesh/minimal) is macroexpanded, not at runtime
        (sh-eval '(import (schemesh repl) (schemesh shell)))
        #'(void)))))


(define-syntax import-schemesh/all
  (lambda (stx)
    (import (only (chezscheme) void)
            (only (schemesh bootstrap) sh-eval))
    (syntax-case stx ()
      ((_)
        ; import libraries when (import-schemesh/all) is macroexpanded, not at runtime
        (sh-eval
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
