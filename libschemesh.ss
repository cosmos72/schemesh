;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(begin
  (include "bootstrap/functions.ss")
  (include "bootstrap/bootstrap.ss")

  (include "containers/bitmap.ss")
  (include "containers/bytevector.ss")
  (include "containers/flvector.ss")
  (include "containers/in.ss")
  (include "containers/list.ss")
  (include "containers/string.ss")
  (include "containers/vector.ss")
  (include "containers/hashtable.ss")   ; requires containers/list.ss
  (include "containers/bytespan.ss")    ; requires containers/bytevector.ss containers/list.ss
  (include "containers/charspan.ss")    ; requires containers/string.ss
  (include "containers/span.ss")        ; requires containers/vector.ss
  (include "containers/sort.ss")        ; requires containers/span.ss
  (include "containers/utf8b.ss")       ; requires containers/bytespan.ss
  (include "containers/chargbuffer.ss") ; requires containers/charspan.ss
  (include "containers/gbuffer.ss")     ; requires containers/span.ss
  (include "containers/charline.ss")    ; requires containers/gbuffer.ss
  (include "containers/charlines.ss")   ; requires containers/charlines.ss
  (include "containers/utf8b-utils.ss") ; requires containers/utf8b.ss containers/chargbuffer.ss
  (include "containers/macros.ss")
  (include "containers/containers.ss")
  (include "containers/replacements.ss")

  (include "conversions/unicode.ss")
  (include "conversions/conversions.ss")

  (include "wire/wire.ss")

  (include "posix/fd.ss")
  (include "posix/dir.ss")
  (include "posix/io.ss")
  (include "posix/pattern.ss")
  (include "posix/replacements.ss")
  (include "posix/signal.ss")
  (include "posix/status.ss")
  (include "posix/tty.ss")
  (include "posix/pid.ss")
  (include "posix/posix.ss")

  (include "lineedit/vscreen.ss")
  (include "lineedit/charhistory.ss")
  (include "lineedit/charhistory-io.ss")
  (include "lineedit/charlines-io.ss")
  (include "lineedit/paren.ss")
  (include "lineedit/parenmatcher.ss")
  (include "lineedit/parser.ss")
  (include "lineedit/linectx.ss")
  (include "lineedit/lineterm.ss")
  (include "lineedit/lineedit.ss")
  (include "lineedit/all.ss")

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
