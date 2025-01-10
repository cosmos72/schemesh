;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell (0 1))
  (export
    ; fds.ss
    sh-fd sh-fd* sh-fd? sh-fd->int sh-fd-copy sh-fd-allocate sh-fd-release sh-fd-stdin sh-fd-stdout sh-fd-stderr

    ; paths.ss
    sh-path sh-path? sh-path-absolute? sh-path-relative?
    sh-path-append sh-path-append! sh-path-iterate
    sh-subpath sh-subpath? sh-path->subpath sh-wildcard? text->sh-path*

    ; aliases.ss
    sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases

    ; builtins.ss
    sh-builtin sh-builtins sh-find-builtin sh-echo sh-error sh-false sh-true sh-history sh-repl-args

    ; jobs.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-cmd? sh-multijob?
    sh-wildcard sh-env-copy sh-env->argv sh-globals sh-global-env
    sh-cmd make-cmd sh-cwd
    sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/ok?

    ; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell

    ; display.ss
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string
    sh-job-display/summary? sh-job-display/summary sh-job-display/summary*

    ; env.ss
    sh-env sh-env! sh-env-unset! sh-env-exported? sh-env-export! sh-env-set+export! sh-env/lazy!
    sh-cwd-set! sh-cd sh-pwd

    ; redirect.ss
    sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-redirect!

    ; pipe.ss
    sh-pipe sh-pipe*

    ; parse.ss
    sh sh-parse sh-cmd* sh-list*

    ; macros.ss
    shell shell-backquote shell-env shell-list shell-subshell shell-wildcard

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx)

  (import
    (schemesh shell fds)
    (schemesh shell paths)
    (schemesh shell builtins)
    (schemesh shell jobs)
    (schemesh shell macros)
    (schemesh shell utils))

) ; close library
