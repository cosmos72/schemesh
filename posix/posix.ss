;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix (0 7 1))
  (export
    ; fd.ss
    c-errno c-errno->string raise-c-errno
    c-errno c-errno->string c-exit c-hostname
    fd-open-max fd-close fd-close-list fd-dup fd-dup2 fd-read fd-write
    fd-read-until-eof fd-select fd-setnonblock open-file-fd open-pipe-fds
    raise-c-errno

    ; dir.ss
    directory-list directory-sort! file-stat

    ; pattern.ss
    sh-pattern sh-pattern? span->sh-pattern* sh-pattern->span*
    sh-pattern-front/string sh-pattern-back/string
    sh-pattern-match? sh-pattern-match-range? sh-wildcard?

    ; pid.ss
    pid-get pgid-get pid-kill pid-wait exit-with-job-status

    ; tty.ss
    tty-setraw! tty-restore! tty-size

    ; signal.ss
    signal-raise signal-number->name signal-name->number
    signal-consume-sigchld signal-consume-sigwinch signal-init-sigwinch signal-restore-sigwinch
    suspend-handler)

  (import
    (rnrs)
    (schemesh posix fd)
    (schemesh posix dir)
    (schemesh posix pattern)
    (schemesh posix signal)
    (schemesh posix tty)
    (schemesh posix pid))

) ; close library
