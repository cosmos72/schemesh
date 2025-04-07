;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; define (schemesh ipc) as a library that exports all its imported bindings
(library-reexport (schemesh ipc (0 8 3))
  (import
    (schemesh ipc channel)
    (schemesh ipc fifo)))
