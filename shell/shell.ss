;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell (0 7 1))
  (export
    ; aliases.ss
    sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases

    ; builtins.ss
    sh-alias sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases
    sh-builtin-alias sh-builtin-echo sh-builtin-error sh-builtin-false
    sh-builtin-history sh-builtin-true sh-builtin-unalias
    sh-builtins sh-find-builtin
    sh-echo sh-error sh-false sh-true sh-history sh-repl-args

    ; eval.ss
    sh-eval-file sh-eval-file* sh-eval-port* sh-eval-parsectx* sh-eval-string*
    sh-read-file sh-read-file* sh-read-port* sh-read-parsectx* sh-read-string*

    ; fds.ss
    sh-fd sh-fd* sh-fd? sh-fd->int sh-fd-copy sh-fd-allocate sh-fd-release sh-fd-stdin sh-fd-stdout sh-fd-stderr

    ; macros.ss
    include/lang include/lang*
    shell shell-backquote shell-env shell-list shell-subshell shell-wildcard

    ; paths.ss
    sh-path sh-path? sh-path-absolute? sh-path-relative?
    sh-path-append sh-path-append! sh-path-iterate
    sh-subpath sh-subpath? sh-path->subpath text->sh-path*

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx

    ;;;;;;;;;; job.ss and files included by it:

    ; builtins2.ss
    sh-builtin-bg sh-builtin-builtin sh-builtin-cd sh-builtin-command
    sh-builtin-exec sh-builtin-fg sh-builtin-jobs sh-builtin-pwd

    ; dir.ss
    sh-cwd-set! sh-cd sh-pwd sh-userhome sh-xdg-cache-home/ sh-xdg-config-home/

    ; display.ss
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string
    sh-job-display/summary? sh-job-display/summary sh-job-display/summary*

    ; env.ss
    sh-env-ref sh-env-set! sh-env-unset! sh-env-visibility-ref sh-env-visibility-set! sh-env-set/lazy!

    ; job.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-cmd? sh-multijob?
    sh-env-copy sh-env->argv sh-globals
    sh-cmd make-cmd sh-cwd
    sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/err? sh-run/ok?

    ; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell

    ; redirect.ss
    sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-redirect!

    ; params.ss
    sh-job-control-available? sh-job-control?

    ; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ; pipe.ss
    sh-pipe sh-pipe*

    ; wildcard
    sh-wildcard sh-wildcard/apply sh-wildcard/expand-~ sh-wildcard->string
    sh-wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (schemesh shell builtins)
    (schemesh shell eval)
    (schemesh shell fds)
    (schemesh shell job)
    (schemesh shell macros)
    (schemesh shell paths)
    (schemesh shell utils))

) ; close library
