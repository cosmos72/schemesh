;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh shell) as a library that exports all its imported bindings
(library-reexport (schemesh shell (0 9 3))
  (import
    (schemesh shell autocomplete)
    (schemesh shell eval)
    (schemesh shell fds)
    (schemesh shell job)
    (schemesh shell macros)
    (schemesh shell parameters)
    (schemesh shell paths)
    (schemesh shell utils)))
