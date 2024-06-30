;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
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
    sh-subpath sh-subpath? sh-path->subpath text->sh-path

    ; aliases.ss
    sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases sh-builtin-alias sh-builtin-unalias

    ; builtins.ss
    sh-builtin sh-builtin-false sh-builtin-true
    sh-builtins sh-find-builtin sh-false sh-true

    ; jobs.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-cmd sh-cmd? sh-multijob?
    sh-concat sh-env-copy sh-env sh-env! sh-env-unset! sh-globals sh-global-env
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->argv
    sh-builtin-command sh-builtin-cd sh-builtin-pwd sh-cwd sh-cwd-set! sh-cd sh-pwd
    sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-not sh-list sh-subshell sh-redirect!
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string

    ; cmd.ss
    sh-cmd*

    ; parse.ss
    sh sh-parse

    ; macros.ss
    shell shell-backquote shell-concat shell-env shell-list shell-subshell

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx)

  (import
    (schemesh shell paths)
    (schemesh shell aliases)
    (schemesh shell builtins)
    (schemesh shell jobs)
    (schemesh shell cmd)
    (schemesh shell parse)
    (schemesh shell macros)
    (schemesh shell utils))

) ; close library
