;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; define (scheme2k containers) as a library that exports all its imported bindings
(library-reexport (scheme2k containers (0 9 3))
  (import (scheme2k containers bitmap)
          (scheme2k containers bytespan)
          (scheme2k containers bytevector)
          (scheme2k containers cf32span)
          (scheme2k containers charspan)
          (scheme2k containers f32span)
          (scheme2k containers flvector)
          (scheme2k containers fxspan)
          (scheme2k containers fxvector)
          (scheme2k containers in)
          (scheme2k containers gbuffer)
          (scheme2k containers list)
          (scheme2k containers hashtable)
          (scheme2k containers macros)
          (scheme2k containers sort)
          (scheme2k containers span)
          (scheme2k containers string)
          (scheme2k containers utf8b)
          (scheme2k containers vector)))
