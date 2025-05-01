;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh screen) as a library that exports all its imported bindings
(library-reexport (schemesh screen (0 9 0))
  (import (schemesh screen vbuffer)
          (schemesh screen vcell)
          (schemesh screen vcellspan)
          (schemesh screen vcellvector)
          (schemesh screen vhistory)
          (schemesh screen vhistory io)
          (schemesh screen vline)
          (schemesh screen vlines)
          (schemesh screen vlines io)
          (schemesh screen vscreen)))
