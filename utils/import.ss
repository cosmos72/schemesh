;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; TODO: is there a better solution not requiring (eval) ?
(eval
  `(library (schemesh minimal (0 7 3))
      (export ,@(library-exports '(schemesh repl))
              ,@(library-exports '(schemesh shell)))
      (import (schemesh shell)
              (schemesh repl))))


(eval
  `(library (schemesh all (0 7 3))
      (export ,@(library-exports '(schemesh bootstrap))
              ,@(library-exports '(schemesh containers))
              ,@(library-exports '(schemesh conversions))
              ,@(library-exports '(schemesh lineedit))
              ,@(library-exports '(schemesh parser))
              ,@(library-exports '(schemesh posix))
              ,@(library-exports '(schemesh shell))
              ,@(library-exports '(schemesh repl)))
      (import
        (schemesh bootstrap)
        (schemesh containers)
        (schemesh conversions)
        (schemesh lineedit)
        (schemesh parser)
        (schemesh posix)
        (schemesh shell)
        (schemesh repl))))


;; when reloading libschemesh.ss, reimport (schemesh shell) and (schemesh repl)
;; fixes error "compiled program requires a different compilation instance of (schemesh ...)""
(eval-when (eval)
  (let ()
    (import (rnrs) (only (chezscheme) top-level-bound? eval))

    (when (top-level-bound? 'sh-persistent-parameters)
      (eval '(import (schemesh shell))))
    (when (top-level-bound? 'sh-repl)
      (eval '(import (schemesh repl))))))
