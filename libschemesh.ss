;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(begin
  (include "bootstrap/bootstrap.ss")
  (include "containers/misc.ss")
  (include "containers/hashtable.ss")
  (include "containers/span.ss")
  (include "containers/bytespan.ss")
  (include "containers/charspan.ss")
  (include "containers/gbuffer.ss")
  (include "containers/chargbuffer.ss")
  (include "containers/utils.ss")
  (include "containers/containers.ss")
  (include "conversions/conversions.ss")
  (include "io/io.ss")
  (include "parser/base.ss")
  (include "parser/lisp.ss")
  (include "parser/r6rs.ss")
  (include "parser/scheme.ss")
  (include "parser/shell.ss")
  (include "parser/parser.ss")
  (include "posix/fd.ss")
  (include "posix/signal.ss")
  (include "posix/tty.ss")
  (include "posix/misc.ss")
  (include "posix/pid.ss")
  (include "posix/posix.ss")
  (include "lineedit/base.ss")
  (include "lineedit/lineedit.ss")
  (include "shell/jobs.ss")
  (include "shell/builtins.ss")
  (include "shell/parse.ss")
  (include "shell/macros.ss")
  (include "shell/utils.ss")
  (include "shell/shell.ss")
  (include "repl/repl.ss")

) ; close begin