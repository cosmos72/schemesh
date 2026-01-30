;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

;;; high-level procedures for reading from and writing to ports.
;;;
;;; procedure names and effect are intentionally compatible with
;;; https://docs.racket-lang.org/reference/port-lib.html
;;;
(library-reexport (scheme2k io (0 9 3))
  (import
    (scheme2k io json)
    (scheme2k io http)
    (scheme2k io port)
    (scheme2k io redir)
    (scheme2k io stdio)
    (scheme2k io wire)))
