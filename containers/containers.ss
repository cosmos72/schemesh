;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; define (schemesh containers) as a library that exports all its imported bindings
(library-reexport (schemesh containers (0 8 1))
  (import (schemesh containers bitmap)
          (schemesh containers bytevector)
          (schemesh containers in)
          (schemesh containers list)
          (schemesh containers hashtable)   ; requires (schemesh containers list)
          (schemesh containers macros)
          (schemesh containers sort)
          (schemesh containers string)
          (schemesh containers vector)

          (schemesh containers bytespan)    ; requires (schemesh containers bytevector)
          (schemesh containers charspan)    ; requires (schemesh containers string)
          (schemesh containers span)        ; requires (schemesh containers vector)

          (schemesh containers utf8b)       ; requires (schemesh containers bytespan)
          (schemesh containers utf8b utils)

          (schemesh containers chargbuffer) ; requires (schemesh containers charspan)
          (schemesh containers gbuffer)     ; requires (schemesh containers span)

          (schemesh containers charline)    ; requires (schemesh containers gbuffer)
          (schemesh containers charlines))) ; requires (schemesh containers charlines)
