;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define (schemesh lineedit) as a library that exports all imported bindings
(library-reexport (schemesh lineedit (0 9 2))
  (import
    (schemesh lineedit ansi)
    (schemesh lineedit linectx)
    (schemesh lineedit lineterm)
    (schemesh lineedit lineedit)
    (schemesh lineedit paren)
    (schemesh lineedit parenmatcher)
    (schemesh lineedit parser)))
