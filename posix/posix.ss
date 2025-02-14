;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; define (schemesh posix) as a library that exports all its imported bindings
(library-reexport (schemesh posix (0 7 4))
  (import
    (schemesh posix fd)
    (schemesh posix dir)
    (schemesh posix pattern)
    (schemesh posix signal)
    (schemesh posix tty)
    (schemesh posix pid)))
