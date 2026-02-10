;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(begin
  (include "bootstrap/arrow.ss")
  (include "bootstrap/functions.ss")
  (include "bootstrap/bootstrap.ss")

  (include "io/obj/obj.ss")

  (include "containers/bitmap.ss")
  (include "containers/bytevector.ss")
  (include "containers/flvector.ss")
  (include "containers/fxvector.ss")
  (include "containers/in.ss")
  (include "containers/list.ss")
  (include "containers/string.ss")
  (include "containers/vector.ss")
  (include "containers/hashtable.ss")   ; requires containers/list.ss  containers/string.ss  containers/vector.ss
  (include "containers/ordered-hash.ss")
  (include "containers/bytespan.ss")    ; requires containers/bytevector.ss containers/list.ss
  (include "containers/cf32span.ss")    ; requires containers/bytevector.ss containers/list.ss
  (include "containers/f32span.ss")     ; requires containers/bytevector.ss containers/list.ss
  (include "containers/fxspan.ss")      ; requires containers/fxvector.ss   containers/list.ss
  (include "containers/charspan.ss")    ; requires containers/string.ss
  (include "containers/span.ss")        ; requires containers/vector.ss
  (include "containers/sort.ss")        ; requires containers/span.ss
  (include "containers/gbuffer.ss")     ; requires containers/span.ss
  (include "containers/utf8b.ss")       ; requires containers/bytespan.ss
  (include "containers/macros.ss")
  (include "containers/time.ss")        ; requires containers/utf8b.ss
  (include "containers/date.ss")        ; requires containers/time.ss
  (include "containers/containers.ss")
  (include "containers/replacements.ss")

  (include "conversions/unicode.ss")
  (include "conversions/conversions.ss")

  (include "posix/fd.ss")
  (include "posix/fs.ss")
  (include "posix/io.ss")
  (include "posix/pattern.ss")
  (include "posix/signal.ss")
  (include "posix/socket.ss")       ; requires posix/fd.ss
  (include "posix/status.ss")
  (include "posix/thread.ss")       ; requires posix/signal.ss posix/status.ss
  (include "posix/tty.ss")
  (include "posix/rlimit.ss")
  (include "posix/replacements.ss") ; requires posix/thread.ss
  (include "posix/pid.ss")
  (include "posix/posix.ss")

  (include "reflect/reflect.ss")

  (include "io/field.ss")           ; requires reflect/reflect.ss
  (include "io/http.ss")            ; requires io/posix/posix.ss
  (include "io/redir.ss")
  (include "io/stdio.ss")
  (include "io/port.ss")            ; requires io/stdio.ss
  (include "io/json/json.ss")       ; requires io/stdio.ss posix/fs.ss
  (include "io/table/table.ss")     ; requires reflect/reflect.ss
  (include "io/wire/wire.ss")       ; requires io/posix/status.ss
  (include "io/io.ss")

  (meta-cond
    ((threaded?) (include "ipc/queue-thread.ss"))
    (else        (include "ipc/queue-nothread.ss")))
  (include "ipc/wire.ss")           ; requires io/wire/wire.ss posix/fd.ss
  (include "ipc/ipc.ss")

  (include "os/process.ss")

  (include "vscreen/all.ss")

  (include "lineedit/ansi.ss")
  (include "lineedit/paren.ss")
  (include "lineedit/parenmatcher.ss")
  (include "lineedit/parser.ss")
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
  (include "shell/eval.ss")
  (include "shell/macros.ss")
  (include "shell/autocomplete.ss")
  (include "shell/utils.ss")
  (include "shell/replacements.ss")
  (include "shell/shell.ss")

  (include "repl/repl.ss")

  (include "utils/import.ss")

) ; close begin
