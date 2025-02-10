;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh shell (0 7 4))
  (export
    ; autocomplete.ss
    sh-autocomplete-func sh-autocomplete-r6rs sh-autocomplete-scheme sh-autocomplete-shell

    ; aliases.ss
    sh-alias-delete! sh-alias-set! sh-aliases-expand sh-aliases

    ; builtins.ss
    sh-builtins sh-builtins-help sh-find-builtin sh-exception-handler
    sh-echo sh-error sh-false sh-help sh-history sh-repl-args sh-true

    ; eval.ss
    sh-eval-file sh-eval-file* sh-eval-port* sh-eval-parsectx* sh-eval-string*
    sh-read-file sh-read-file* sh-read-port* sh-read-parsectx* sh-read-string*

    ; fds.ss
    sh-fd sh-fd* sh-fd? sh-fd->int sh-fd-copy sh-fd-allocate sh-fd-release sh-fd-stdin sh-fd-stdout sh-fd-stderr

    ; macros.ss
    include/lang include/lang*
    shell shell-backquote shell-env shell-list shell-subshell shell-wildcard

    ; parameter1.ss
    sh-persistent-parameters

    ; parameters.ss
    sh-current-environment sh-current-eval sh-globals sh-pid-table
    sh-schemesh-reload-count sh-repl-restart sh-repl-restart?
    sh-eval sh-eval-string sh-eval->bytevector

    ; paths.ss
    sh-path sh-path? sh-path-absolute? sh-path-relative?
    sh-path-append sh-path-append! sh-path-iterate
    sh-subpath sh-subpath? sh-path->subpath text->sh-path*

    ; utils.ss
    sh-autocomplete sh-current-time sh-expand-ps1 sh-home->~ sh-make-linectx

    ;;;;;;;;;; job.ss and files included by it:

    ; dir.ss
    sh-cd sh-pwd sh-userhome sh-xdg-cache-home/ sh-xdg-config-home/

    ; display.ss
    sh-job-display sh-job-display* sh-job->string
    sh-job-write   sh-job-write*   sh-job->verbose-string
    sh-job-display-summary? sh-job-display-summary sh-job-display-summary*

    ; env.ss
    sh-env-ref sh-env-set! sh-env-delete! sh-env-visibility-ref sh-env-visibility-set!
    sh-env-iterate/direct sh-env-set/lazy! sh-env-copy sh-env->argv

    ; job.ss
    sh-job sh-job-id sh-job-status sh-jobs sh-find-job sh-job-exception
    sh-cmd make-cmd sh-cwd sh-consume-sigchld
    sh-globals sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-start* sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/err? sh-run/ok?

    ; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell

    ; redirect.ss
    sh-redirect! sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-start/fd-stdout

    ; params.ss
    sh-job-control-available? sh-job-control?

    ; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ; pipe.ss
    sh-pipe sh-pipe*

    ; types.ss
    sh-job? sh-job-copy sh-cmd? sh-multijob?

    ; wildcard
    sh-wildcard sh-wildcard* sh-wildcard/apply sh-wildcard/expand-tilde sh-wildcard->string
    sh-wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (schemesh shell autocomplete)
    (schemesh shell builtins)
    (schemesh shell eval)
    (schemesh shell fds)
    (schemesh shell job)
    (schemesh shell macros)
    (schemesh shell parameters)
    (schemesh shell paths)
    (schemesh shell utils))

) ; close library
