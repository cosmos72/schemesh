;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

;; pure Scheme implementation of test/test.c that only compiles libchez_batteries_0.9.3.so
;;
;; It has the defect of requiring to find the correct Chez Scheme executable.


;; (top-level-program

  (import (chezscheme))

  (parameterize ((optimize-level 2))
    (compile-file "libchez_batteries.ss" "libchez_batteries_temp.so")
    (strip-fasl-file "libchez_batteries_temp.so" "libchez_batteries_0.9.3.so"
      (fasl-strip-options inspector-source source-annotations profile-source)))

;;) ; close top-level-program
