;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(let ((compile-and-load (lambda (path)
         (compile-file path)
         (load path)))
      (files '("bootstrap/bootstrap.ss"
               "containers/misc.ss"
               "containers/hashtable.ss"
               "containers/span.ss"
               "containers/bytespan.ss"
               "containers/charspan.ss"
               "containers/gbuffer.ss"
               "containers/chargbuffer.ss"
               "containers/utils.ss"
               "containers/containers.ss"
               "conversions/conversions.ss"
               "io/io.ss"
               "parser/base.ss"
               "parser/scheme.ss"
               "parser/shell.ss"
               "parser/parser.ss"
               "fd/fd.ss"
               ; "signals/signals.ss"
               "tty/tty.ss"
               "posix/posix.ss"
               "pid/pid.ss")))
  (for-each compile-and-load files))
