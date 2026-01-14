;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

;; define (scheme2k ipc) as a library that exports all its imported bindings
(library-reexport (scheme2k ipc (0 9 3))
  (import
    (scheme2k ipc channel)
    (scheme2k ipc fifo)))
