;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh containers replacements (0 8 2))
  ;; the following functions *intentionally* conflict with R6RS and Chez Scheme
  ;; functions with the same names,
  ;;
  ;; because they are intended as replacements
  (export ;(rename (bytevector-sint-ref*  bytevector-sint-ref)
          ;        ;; (bytevector-sint-set*! bytevector-sint-set!) ; currently bugged
          ;        (bytevector-uint-ref*  bytevector-uint-ref)
          ;        (bytevector-uint-set*! bytevector-uint-set!))
  )
  (import (only (schemesh containers bytevector)
                  bytevector-sint-ref* ; bytevector-sint-set*!
                  bytevector-uint-ref* bytevector-uint-set*!)))
