;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(let ((compile-and-load (lambda (path)
        (compile-file path)
        (load path))))
  (compile-and-load "all_libraries.ss"))
