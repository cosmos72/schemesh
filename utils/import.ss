;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh minimal) as a library that exports all its imported bindings
(library-reexport (schemesh minimal (0 9 3))
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
(library-reexport (schemesh rnrs (0 9 3))
  (import
    (except (rnrs) bytevector-sint-ref bytevector-sint-set!
                   bytevector-uint-ref bytevector-uint-set!
                   file-exists? delete-file
                   get-bytevector-all get-bytevector-n get-bytevector-some
                   get-char get-datum get-line get-string-all get-string-n get-u8
                   put-bytevector put-char put-datum put-string put-u8
                   with-input-from-file with-output-to-file)
    (scheme2k containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (scheme2k posix replacements)      ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh shell replacements)))    ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.


;; library (schemesh) collects and exports *all* bindings defined by all libschemesh sub-libraries,
;; including few bindings that intentionally conflict with some R6RS and Chez Scheme functions
;; because they are intended as replacements
(library-reexport (schemesh (0 9 3))
  (import
    (scheme2k bootstrap)
    (scheme2k bootstrap arrow)
    (scheme2k containers)
    (scheme2k containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (scheme2k conversions)
    (scheme2k ipc)
    (scheme2k lineedit)
    (scheme2k io)
    (scheme2k os)
    (scheme2k posix)
    (scheme2k reflect)
    (scheme2k vscreen)
    (scheme2k posix replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
    (schemesh parser)
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
      (eval '(import (scheme2k bootstrap))))
    (when (top-level-bound? 'subbytevector)
      (eval '(import (scheme2k containers)))
      (eval '(import (scheme2k containers replacements))))
    (when (top-level-bound? 'text->bytevector)
      (eval '(import (scheme2k conversions))))
    (when (top-level-bound? 'vcell)
      (eval '(import (scheme2k vscreen))))
    (when (top-level-bound? 'lineedit-read)
      (eval '(import (scheme2k lineedit))))
    (when (top-level-bound? 'parsers)
      (eval '(import (schemesh parser))))
    (when (top-level-bound? 'obj-reader?)
      (eval '(import (schemesh obj-reader))))
    (when (top-level-bound? 'pid-wait)
      (eval '(import (scheme2k posix)))
      (eval '(import (scheme2k posix replacements))))
    (when (top-level-bound? 'process-reader?)
      (eval '(import (schemesh os))))
    (when (top-level-bound? 'reflect-field)
      (eval '(import (scheme2k reflect))))
    (when (top-level-bound? 'sh-persistent-parameters)
      (eval '(import (schemesh shell)))
      (eval '(import (schemesh shell replacements))))
    (when (top-level-bound? 'repl)
      (eval '(import (schemesh repl))))))
