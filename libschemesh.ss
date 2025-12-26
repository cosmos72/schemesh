;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(begin
  (include "parser/lisp.ss")
  (include "parser/r6rs.ss")
  (include "parser/scheme.ss")
  (include "parser/shell.ss")
  (include "parser/parser.ss")

  (include "shell/parameter1.ss")
  (include "shell/parameters.ss")
  (include "shell/fds.ss")
  (include "shell/paths.ss")
  (include "shell/job.ss")
  (include "shell/replacements.ss")
  (include "shell/eval.ss")
  (include "shell/macros.ss")
  (include "shell/autocomplete.ss")
  (include "shell/utils.ss")
  (include "shell/shell.ss")

  (include "repl/repl.ss")

  (include "utils/import.ss")

) ; close begin
