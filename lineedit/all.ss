;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; define (scheme2k lineedit) as a library that exports all imported bindings
(library-reexport (scheme2k lineedit (0 9 3))
  (import
    (scheme2k lineedit ansi)
    (scheme2k lineedit lineedit)
    (scheme2k lineedit paren)
    (scheme2k lineedit parenmatcher)
    (scheme2k lineedit parser)))
