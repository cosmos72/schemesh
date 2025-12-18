;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh minimal) as a library that exports all its imported bindings
(library-reexport (schemesh minimal (0 9 2))
  (import (schemesh shell)
          (schemesh repl)))


;; library (schemesh rnrs) exports the same bindings as (rnrs),
;; except for few bindings that are replaced with improved alternatives:
;;
;;   bytevector-sint-ref bytevector-sint-set!
;;   bytevector-uint-ref bytevector-uint-set!
;;   file-exists? delete-file
;;   get-bytevector-all get-bytevector-n get-bytevector-some
;;   get-char get-datum get-line get-string-all get-string-n get-u8
;;   put-bytevector put-char put-datum put-string put-u8
;;
(library-reexport (schemesh rnrs (0 9 2))
  (import
    (except (rnrs) bytevector-sint-ref bytevector-sint-set!
                   bytevector-uint-ref bytevector-uint-set!
                   file-exists? delete-file
                   get-bytevector-all get-bytevector-n get-bytevector-some
                   get-char get-datum get-line get-string-all get-string-n get-u8
                   put-bytevector put-char put-datum put-string put-u8)
    (schemesh containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh posix replacements)      ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh shell replacements)))    ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.


;; library (schemesh) collects and exports *all* bindings defined by all libschemesh sub-libraries,
;; including few bindings that intentionally conflict with some R6RS and Chez Scheme functions
;; because they are intended as replacements
(library-reexport (schemesh (0 9 2))
  (import
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh conversions)
    (schemesh wire)
    (schemesh vscreen)
    (schemesh lineedit)
    (schemesh parser)
    (schemesh port)
    (schemesh posix)
    (schemesh posix replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh ipc)
    (schemesh shell)
    (schemesh shell replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh repl)))


;; when reloading libschemesh.ss, reimport (schemesh shell) and (schemesh repl)
;; fixes error "compiled program requires a different compilation instance of (schemesh ...)""
(eval-when (eval)
  (let ()
    (import (rnrs)
            (only (chezscheme) top-level-bound? eval))

    (when (top-level-bound? 'sh-version)
      (eval '(import (schemesh bootstrap))))
    (when (top-level-bound? 'subbytevector)
      (eval '(import (schemesh containers)))
      (eval '(import (schemesh containers replacements))))
    (when (top-level-bound? 'text->bytevector)
      (eval '(import (schemesh conversions))))
    (when (top-level-bound? 'vcell)
      (eval '(import (schemesh vscreen))))
    (when (top-level-bound? 'lineedit-read)
      (eval '(import (schemesh lineedit))))
    (when (top-level-bound? 'parsers)
      (eval '(import (schemesh parser))))
    (when (top-level-bound? 'pid-wait)
      (eval '(import (schemesh posix)))
      (eval '(import (schemesh posix replacements))))
    (when (top-level-bound? 'sh-persistent-parameters)
      (eval '(import (schemesh shell)))
      (eval '(import (schemesh shell replacements))))
    (when (top-level-bound? 'repl)
      (eval '(import (schemesh repl))))))
