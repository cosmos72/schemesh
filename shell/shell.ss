;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell (0 1))
  (export
    ; jobs.ss
    sh-job? sh-job-ref sh-job-status sh-jobs sh-cmd sh-cmd<> sh-cmd? sh-multijob sh-multijob?
    sh-globals sh-global-env sh-env-copy sh-env-ref sh-env-set! sh-env-unset!
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->vector-of-bytevector0
    sh-cwd sh-consume-sigchld
    sh-start sh-bg sh-fg sh-run sh-run-capture-output sh-wait sh-and sh-or sh-and-or*
    sh-list sh-list* sh-fd-redirect! sh-fds-redirect!

    ; builtins.ss
    sh-true sh-false sh-cd sh-pwd

    ; parse
    sh sh-parse

    ; macros.ss
    shell shell-list shell-backquote

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx)

  (import
    (schemesh shell jobs)
    (schemesh shell builtins)
    (schemesh shell parse)
    (schemesh shell macros)
    (schemesh shell utils))

) ; close library
