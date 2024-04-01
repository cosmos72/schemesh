;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell (0 1))
  (export
    ; paths.ss
    sh-path sh-path? sh-path-absolute? sh-path-relative?
    sh-path-append sh-path-append! sh-path-iterate
    sh-subpath sh-subpath? sh-path->subpath

    ; jobs.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs
    sh-cmd sh-cmd* sh-cmd? sh-multijob sh-multijob?
    sh-globals sh-global-env sh-env-copy sh-env sh-env! sh-env-unset!
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->vector-of-bytevector0
    sh-cwd sh-cwd-set! sh-cd sh-consume-sigchld sh-start sh-bg sh-fg sh-wait sh-ok?
    sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-list sh-subshell
    sh-fd-redirect! sh-fds-redirect!

    ; builtins.ss
    sh-false sh-true sh-pwd

    ; parse.ss
    sh sh-parse

    ; macros.ss
    shell shell-list shell-backquote shell-subshell

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx)

  (import
    (schemesh shell paths)
    (schemesh shell jobs)
    (schemesh shell builtins)
    (schemesh shell parse)
    (schemesh shell macros)
    (schemesh shell utils))

) ; close library
