;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh containers) as a library that exports all its imported bindings
(library-reexport (schemesh containers (0 8 3))
  (import (schemesh containers bitmap)
          (schemesh containers bytespan)
          (schemesh containers bytevector)
          (schemesh containers cellgbuffer)
          (schemesh containers cellvector)
          (schemesh containers cellspan)
          (schemesh containers charspan)
          (schemesh containers flvector)
          (schemesh containers in)
          (schemesh containers gbuffer)
          (schemesh containers list)
          (schemesh containers hashtable)
          (schemesh containers macros)
          (schemesh containers cell)
          (schemesh containers sort)
          (schemesh containers span)
          (schemesh containers string)
          (schemesh containers utf8b)
          (schemesh containers utf8b utils)
          (schemesh containers vector)))
