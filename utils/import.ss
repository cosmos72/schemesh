;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; define (schemesh minimal) as a library that exports all its imported bindings
(library-reexport (schemesh minimal (0 7 6))
  (import (schemesh shell)
          (schemesh repl)))


;; define (schemesh) as a library that exports all its imported bindings
(library-reexport (schemesh (0 7 6))
  (import
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh conversions)
    (schemesh lineedit)
    (schemesh parser)
    (schemesh posix)
    (schemesh posix replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh shell)
    (schemesh repl)))


;; when reloading libschemesh.ss, reimport (schemesh shell) and (schemesh repl)
;; fixes error "compiled program requires a different compilation instance of (schemesh ...)""
(eval-when (eval)
  (let ()
    (import (rnrs) (only (chezscheme) top-level-bound? eval))

    (when (top-level-bound? 'sh-persistent-parameters)
      (eval '(import (schemesh shell))))
    (when (top-level-bound? 'repl)
      (eval '(import (schemesh repl))))))
