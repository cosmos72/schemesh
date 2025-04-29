;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit vlines io (0 8 3))
  (export open-vlines-input-port)
  (import
    (rnrs)
    (only (schemesh lineedit vlines) vlines->string))


;; create an input port reading from a vlines
(define (open-vlines-input-port lines)
  ;; a string-input-port supports unlimited (unget-char)
  ;; useful for parsers
  (open-string-input-port (vlines->string lines)))

) ; close library
