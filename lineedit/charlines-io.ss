;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh lineedit charlines io (0 8 1))
  (export open-charline-input-port open-charlines-input-port)
  (import
    (rnrs)
    (only (chezscheme)         fx1+)
    (only (schemesh bootstrap) assert*)
    (schemesh containers chargbuffer)
    (schemesh containers charlines))


;; create an input port reading from a charline
(define (open-charline-input-port cline)
  ;; a string-input-port supports unlimited (unget-char)
  ;; useful for parsers
  (open-string-input-port (chargbuffer->string cline)))

;; create an input port reading from a charlines
(define (open-charlines-input-port clines)
  ;; a string-input-port supports unlimited (unget-char)
  ;; useful for parsers
  (open-string-input-port (charlines->string clines)))

) ; close library
