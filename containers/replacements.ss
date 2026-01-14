;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers replacements (0 9 3))
  ;; the following functions *intentionally* conflict with R6RS and Chez Scheme
  ;; functions with the same names,
  ;;
  ;; because they are intended as replacements
  (export (rename (bytevector-sint-ref*  bytevector-sint-ref)
                  (bytevector-sint-set*! bytevector-sint-set!)
                  (bytevector-uint-ref*  bytevector-uint-ref)
                  (bytevector-uint-set*! bytevector-uint-set!)))

  (import (only (scheme2k containers bytevector)
                  bytevector-sint-ref* bytevector-sint-set*!
                  bytevector-uint-ref* bytevector-uint-set*!)))
