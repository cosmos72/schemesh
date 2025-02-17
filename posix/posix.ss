;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; define (schemesh posix) as a library that exports all its imported bindings
(library-reexport (schemesh posix (0 7 5))
  (import
    (schemesh posix fd)
    (schemesh posix dir)
    (schemesh posix pattern)

    ;; by default, do not re-export bindings from (schemesh posix replacements)
    ;; because they intentionally conflict with R6RS functions (file-exists?) and (delete-file)
    ;;
    ;; (schemesh posix replacements)

    (schemesh posix signal)
    (schemesh posix tty)
    (schemesh posix pid)))
