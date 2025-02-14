;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; (library-reexport) is a macro that expands to (library ...)
;; and defines a library that automatically exports all imported bindings.
;;
;; defining it in a library is not very useful, because one would need to (import) it
;; before calling (library-reexports) at top level
;;
(define-syntax library-reexport
  (lambda (stx)
    (syntax-case stx (import)
      ((k name (import . imported-library-names))
        (let ((export-list (apply append (map library-exports (datum imported-library-names)))))
          #`(library name
            (export . #,(datum->syntax (syntax k) export-list))
            (import . imported-library-names)))))))
