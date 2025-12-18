;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; This file loads and compiles the subset of schemesh sources
;;; that are licensed as LGPLv2+ into Scheme library libchez_batteries_0.9.2.so

(begin
  (include "bootstrap/arrow.ss")
  (include "bootstrap/functions.ss")
  (include "bootstrap/bootstrap.ss")

  (include "containers/bitmap.ss")
  (include "containers/bytevector.ss")
  (include "containers/flvector.ss")
  (include "containers/fxvector.ss")
  (include "containers/in.ss")
  (include "containers/list.ss")
  (include "containers/string.ss")
  (include "containers/vector.ss")
  (include "containers/hashtable.ss")   ; requires containers/list.ss
  (include "containers/bytespan.ss")    ; requires containers/bytevector.ss containers/list.ss
  (include "containers/charspan.ss")    ; requires containers/string.ss
  (include "containers/span.ss")        ; requires containers/vector.ss
  (include "containers/sort.ss")        ; requires containers/span.ss
  (include "containers/gbuffer.ss")     ; requires containers/span.ss
  (include "containers/utf8b.ss")       ; requires containers/bytespan.ss
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
  (include "posix/signal.ss")
  (include "posix/status.ss")
  (include "posix/thread.ss")       ; requires posix/signal.ss posix/status.ss
  (include "posix/tty.ss")
  (include "posix/rlimit.ss")
  (include "posix/replacements.ss") ; requires posix/thread.ss
  (include "posix/pid.ss")
  (include "posix/posix.ss")

  (include "port/http.ss")
  (include "port/redir.ss")
  (include "port/stdio.ss")
  (include "port/port.ss")

  (include "vscreen/all.ss")


  (include "ipc/channel.ss") ; requires wire/wire.ss posix/fd.ss
  (meta-cond
    ((threaded?) (include "ipc/fifo-thread.ss"))
    (else        (include "ipc/fifo-nothread.ss")))
  (include "ipc/ipc.ss")

  (include "lineedit/ansi.ss")
  (include "lineedit/paren.ss")
  (include "lineedit/parenmatcher.ss")
  (include "lineedit/parser.ss")
  (include "lineedit/lineedit.ss")
  (include "lineedit/all.ss")

  ;; library (schemesh batteries) collects and exports *all* bindings defined by all LGPLv2+ libschemesh sub-libraries,
  ;; including few bindings that intentionally conflict with some R6RS and Chez Scheme functions
  ;; because they are intended as replacements
  (library-reexport (schemesh batteries (0 9 2))
    (import
      (schemesh bootstrap)
      (schemesh containers)
      (schemesh containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
      (schemesh conversions)
      (schemesh wire)
      (schemesh vscreen)
      (schemesh lineedit)
      (schemesh port)
      (schemesh posix)
      (schemesh posix replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
      (schemesh ipc)))

) ; close begin
