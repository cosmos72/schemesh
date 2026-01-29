;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; This file loads and compiles the subset of schemesh sources
;;; that are licensed as LGPLv2+ into Scheme library libscheme2k_0.9.3.so

(begin
  (include "bootstrap/arrow.ss")
  (include "bootstrap/functions.ss")
  (include "bootstrap/bootstrap.ss")

  (include "containers/bitmap.ss")
  (include "containers/bytevector.ss")
  (include "containers/date.ss")
  (include "containers/flvector.ss")
  (include "containers/fxvector.ss")
  (include "containers/in.ss")
  (include "containers/list.ss")
  (include "containers/string.ss")
  (include "containers/vector.ss")
  (include "containers/time.ss")
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
  (include "containers/containers.ss")
  (include "containers/replacements.ss")

  (include "conversions/unicode.ss")
  (include "conversions/conversions.ss")

  (include "reflect/reflect.ss")

  (include "wire/wire.ss")

  (include "posix/fd.ss")
  (include "posix/dir.ss")
  (include "posix/io.ss")
  (include "posix/pattern.ss")
  (include "posix/signal.ss")
  (include "posix/socket.ss")       ; requires posix/fd.ss
  (include "posix/status.ss")       ; requires wire/wire.ss
  (include "posix/thread.ss")       ; requires posix/signal.ss posix/status.ss
  (include "posix/tty.ss")
  (include "posix/rlimit.ss")
  (include "posix/replacements.ss") ; requires posix/thread.ss
  (include "posix/pid.ss")
  (include "posix/posix.ss")

  (include "io/http.ss")
  (include "io/redir.ss")
  (include "io/stdio.ss")
  (include "io/json.ss")            ; requires io/stdio.ss
  (include "io/io.ss")

  (include "ipc/channel.ss") ; requires wire/wire.ss posix/fd.ss
  (meta-cond
    ((threaded?) (include "ipc/fifo-thread.ss"))
    (else        (include "ipc/fifo-nothread.ss")))
  (include "ipc/ipc.ss")

  (include "vscreen/all.ss")

  (include "lineedit/ansi.ss")
  (include "lineedit/paren.ss")
  (include "lineedit/parenmatcher.ss")
  (include "lineedit/parser.ss")
  (include "lineedit/lineedit.ss")
  (include "lineedit/all.ss")


  ;; library (scheme2k rnrs) exports the same bindings as (rnrs),
  ;; except for few bindings that are replaced with improved alternatives:
  ;;
  ;;   bytevector-sint-ref bytevector-sint-set!
  ;;   bytevector-uint-ref bytevector-uint-set!
  ;;   file-exists? delete-file
  ;;   get-bytevector-all get-bytevector-n get-bytevector-some
  ;;   get-char get-datum get-line get-string-all get-string-n get-u8
  ;;   put-bytevector put-char put-datum put-string put-u8
  ;;
  (library-reexport (scheme2k rnrs (0 9 3))
    (import
      (except (rnrs) bytevector-sint-ref bytevector-sint-set!
                     bytevector-uint-ref bytevector-uint-set!
                     file-exists? delete-file
                     get-char get-datum get-line get-string-all get-string-n
                     put-char put-datum put-string)
      (scheme2k containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
      (scheme2k posix replacements)))    ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.



  ;; library (scheme2k) collects and exports *all* bindings defined by all LGPLv2+ libschemesh sub-libraries,
  ;; including few bindings that intentionally conflict with some R6RS and Chez Scheme functions
  ;; because they are intended as replacements
  (library-reexport (scheme2k (0 9 3))
    (import
      (scheme2k bootstrap)
      (scheme2k containers)
      (scheme2k containers replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
      (scheme2k conversions)
      (scheme2k ipc)
      (scheme2k lineedit)
      (scheme2k io)
      (scheme2k posix)
      (scheme2k posix replacements) ;; intentionally conflicts with some R6RS and Chez Scheme functions, because it is intended to replace them.
      (scheme2k reflect)
      (scheme2k vscreen)
      (scheme2k wire)))

) ; close begin
