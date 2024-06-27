;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix (0 1))
  (export
    ; fd.ss
    c-errno c-errno->string raise-c-errno
    fd-close fd-dup fd-dup2 fd-read fd-write fd-select fd-setnonblock
    open-file-fd open-pipe-fds

    ; signal.ss
    signal-raise signal-number->name signal-name->number
    signal-consume-sigchld signal-consume-sigwinch signal-init-sigwinch signal-restore-sigwinch
    suspend-handler

    ; tty.ss
    tty-setraw! tty-restore! tty-size

    ; misc.ss
    c-hostname c-exit directory-u8-list

    ; pid.ss
    get-pid get-pgid spawn-pid pid-kill pid-wait exit-with-job-status)
  (import
    (rnrs)
    (schemesh posix fd)
    (schemesh posix signal)
    (schemesh posix tty)
    (schemesh posix misc)
    (schemesh posix pid))

) ; close library
