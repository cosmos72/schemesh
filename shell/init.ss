;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss

(begin
  (s-fd-allocate) ; mark highest fd as reserved: used by tty_fd

  ;; set the parameter (sh-globals) to the global job.
  ;; Jobs started with (sh-start) will be children of sh-globals.
  ;;
  ;; If it's already set, does not modify it.
  ;;
  ;; May be parameterized to a different value in subshells.
  (unless (sh-globals)
    (let ((stdin  (fd->port 0 'rw 'binary (buffer-mode block) "sh-stdin"))
          (stdout (fd->port 1 'rw 'binary (buffer-mode block) "sh-stdout"))
          (stderr (fd->port 2 'rw 'binary (buffer-mode block) "sh-stderr")))
      (sh-globals
        ;; assign job-id 0 to sh-globals itself.
        ;;
        ;; waiting for sh-globals to exit is not useful:
        ;; pretend it already finished successfully
        (%make-multijob
           0 #f                      ; id oid
           (pid-get) (pgid-get 0)    ; pid pgid
           (void) #f                 ; last-status exception
           (span) 0                  ; redirections

           (eqv-hashtable            ; fds-to-remap
             0 (s-fd 0) 1 (s-fd 1) 2 (s-fd 2))

           (eqv-hashtable            ; ports
             0 stdin  1 stdout  2 stderr
             ;; sanity: ignore attempts to close textual stdin/stdout/stderr ports of (sh-globals) job
             (fxnot 0) (port->utf8b-port stdin  'rw (buffer-mode block) '(close? #f))
             (fxnot 1) (port->utf8b-port stdout 'rw (buffer-mode block) '(close? #f))
             (fxnot 2) (port->utf8b-port stderr 'rw (buffer-mode block) '(close? #f)))
           #f #f                     ; start-proc step-proc
           (string->charspan* ((foreign-procedure "c_get_cwd" () ptr))) #f ; current directory, old working directory
           (make-hashtable string-hash string=?) ; env variables
           #f                        ; no env var assignments
           #f #f                     ; no temp parent, no default parent
           'sh-globals -1 (span #t))))) ; skip job-id 0, is used by (sh-globals) itself

  (c-environ->sh-global-env)

  ;; Replace (console-input-port) (console-output-port) (console-error-port)
  ;; with unbuffered UTF-8b textual input/output ports that can be interrupted
  ;; and are ordered with (current-input-port) (current-output-port) (current-error-port)
  (let ((port0 (sh-port #t 0 'text))
        (port1 (sh-port #t 1 'text))
        (port2 (sh-port #t 2 'text))
        (try-flush-port-lambda
          (lambda (port-lambda)
            (try
              (let ((port (port-lambda)))
                (when (output-port? port)
                  (flush-output-port port)))
              (catch (ex)
                (void))))))
    (console-input-port  (textual-port-lambda->port "console-input-port"  (lambda () port0) 'rw #t (lambda () (try-flush-port-lambda current-input-port))  0))
    (console-output-port (textual-port-lambda->port "console-output-port" (lambda () port1) 'rw #t (lambda () (try-flush-port-lambda current-output-port)) 0))
    (console-error-port  (textual-port-lambda->port "console-error-port"  (lambda () port2) 'rw #t (lambda () (try-flush-port-lambda current-error-port))  0)))


  ;; Replace (sh-stdin) (sh-stdout) (sh-stderr)
  ;; with buffered binary input/output ports that can be interrupted and honor current job redirections.
  ;;
  ;; trick: install closures that extract ports from current job, creating them on demand.
  (sh-stdin  (lambda () (sh-port #f 0 'binary)))
  (sh-stdout (lambda () (sh-port #f 1 'binary)))
  (sh-stderr (lambda () (sh-port #f 2 'binary)))


  ;; Replace (current-input-port) (current-output-port) (current-error-port)
  ;; with buffered UTF-8b textual input/output ports that can be interrupted and honor current job redirections.
  ;;
  ;; We cannot use the same trick as above for (sh-stdin) (sh-stdout) (sh-stderr) that extract ports from current job,
  ;; because (current-...-port) cannot execute arbitrary code:
  ;; they merely return the last saved value, which must be a port.
  ;;
  ;; Thus we install only once a triplet of global textual ports that forward each request to current job's ports.
  ;;
  ;; The alternative is calling (current-...-port some-port) i.e. installing a port each time the current job changes,
  ;; which requires eagerly creating textual i/o ports for each job: expensive both in RAM and CPU.
  ;;
  ;; sanity: ignore attempts to close (current-input-port) (current-output-port) (current-error-port)
  (current-input-port  (textual-port-lambda->port "current-input-port"  (lambda () (sh-port #f 0 'text)) 'rw #t))
  (current-output-port (textual-port-lambda->port "current-output-port" (lambda () (sh-port #f 1 'text)) 'rw #t))
  (current-error-port  (textual-port-lambda->port "current-error-port"  (lambda () (sh-port #f 2 'text)) 'rw #t))


  (let ((bt (sh-builtins))
        (ft (builtins-that-finish-immediately)))

    ; additional builtins
    (hashtable-set! bt "alias"      builtin-alias)
    (hashtable-set! bt "bg"         builtin-bg)
    (hashtable-set! bt "builtin"    builtin-builtin)
    (hashtable-set! bt "cd"         builtin-cd)
    (hashtable-set! bt "cd-"        builtin-cd-)
    (hashtable-set! bt "command"    builtin-command)
    (hashtable-set! bt "exec"       builtin-exec)
    (hashtable-set! bt "exit"       builtin-exit)
    (hashtable-set! bt "export"     builtin-export)
    (hashtable-set! bt "fg"         builtin-fg)
    (hashtable-set! bt "global"     builtin-global)
    (hashtable-set! bt "jobs"       builtin-jobs)
    (hashtable-set! bt "parent"     builtin-parent)
    (hashtable-set! bt "pwd"        builtin-pwd)
    (hashtable-set! bt "set"        builtin-set)
    (hashtable-set! bt "split-at-0" builtin-split-at-0)
    (hashtable-set! bt "status"     builtin-status)
    (hashtable-set! bt "threads"    builtin-threads)
    (hashtable-set! bt "unalias"    builtin-unalias)
    (hashtable-set! bt "unexport"   builtin-unexport)
    (hashtable-set! bt "unset"      builtin-unset)
    (hashtable-set! bt "wait"       builtin-wait)

    ;; mark builtins that finish immediately i.e. cannot run commands or aliases
    (for-list ((name '("alias" "cd" "cd-" "echo" "echo0" "exit" "false" "jobs"
                      "history" "pwd" "set" "status" "true" "unalias" "unset")))
      (let ((builtin (hashtable-ref bt name #f)))
        (when builtin
          (hashtable-set! ft builtin #t)))))


  (let ((t (sh-builtins-help)))

    (hashtable-set! t "alias"   (string->utf8 " [name [expansion ...]]
    define or display aliases.

    without arguments,          'alias' writes the list of defined aliases to standard output.
    with a single argument,     'alias NAME' writes the definition of alias NAME to standard output.
    with two or more arguments, 'alias NAME EXPANSION ...' defines an alias NAME such that,
                                 when NAME ARGS ... executed, it is substituted with EXPANSION ... ARGS ...

    return success, unless 'alias NAME' is executed and no such alias is defined.\n"))

    (hashtable-set! t "bg"      (string->utf8 " [job-id]
    move a job to the background.

    return updated job status, or failure if job-id was not found.\n"))

    (hashtable-set! t "builtin" (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with specified arguments.

    useful if BUILTIN-NAME has been shadowed by an alias with the same name.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "cd"      (string->utf8 " [dir]
    change the current directory of parent job.

    without arguments, 'cd' sets the current directory of parent job
                       to the value of its HOME environment variable.
    with one argument, 'cd DIR' sets the current directory of parent job to DIR.

    return success if the directory is successfully changed, otherwise raises an exception.\n"))

    (hashtable-set! t "cd-"      (string->utf8 "
    change the current directory of parent job, setting it to previous working directory.

    return success if the directory is successfully changed, otherwise raises an exception.\n"))

    (hashtable-set! t "command" (string->utf8 " [command-name [arg ...]]
    execute a command with specified arguments.

    useful if COMMAND-NAME has been shadowed by an alias or by a builtin with the same name.

    return exit status of executed command, or failure if no such command was found.\n"))

    (hashtable-set! t "exec" (string->utf8 " [cmd [arg ...]]
    replace the current shell with the command CMD ARG ...

    if CMD ARG ... are not specified, any redirections take effect in the current shell.

    if CMD is not specified, return success.
    if CMD is specified, on success does not return. On failure, returns failure error code.\n"))

    (hashtable-set! t "exit" (string->utf8 " [int ...]
    exit the shell with C exit status INT, or 0 if not specified.

    does not return.\n"))

    (hashtable-set! t "export" (string->utf8 " [var ...]
    show or export environment variables

    without arguments,          'export' writes all exported environment variables
                                 of parent job to standard output.
    with one or more arguments, 'export VAR ...' marks specified environment variables
                                 as exported in parent job.

    return success.\n"))

    (hashtable-set! t "fg"      (string->utf8 " [job-id]
    move a job to the foreground.

    return updated job status, or failure if job-id was not found.\n"))

    (hashtable-set! t "global"     (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with its parent temporarily set to the shell itself.

    useful mostly for builtins 'cd' 'export' 'set' 'pwd' 'unexport' 'unset'
    that show or alter the current directory or the environment variables of their parent job.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "jobs"       (string->utf8 " [arg ...]
    ignore args. write known jobs and their status to standard output.

    return success.\n"))

    (hashtable-set! t "parent"     (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with its parent temporarily set to its grandparent.
    if used multiple times, as for example \"parent parent cd ..\", the effects are cumulative.

    useful mostly for builtins 'cd' 'export' 'set' 'pwd' 'unexport' 'unset'
    that show or alter the current directory or the environment variables of their parent job.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "pwd"        (string->utf8 " [job-id]
    write the current directory of specified job to standard output.
    if job is not specified, defaults to parent job.

    return success if job-id was found or not specified, otherwise return failure.\n"))

    (hashtable-set! t "set"        (string->utf8 " [var [value]]'
    show or set environment variables of parent job.

    without arguments,  'set' writes all exported and private environment variables
                                 of parent job to standard output.
    with one argument,  'set VAR' writes specified environment variable of parent job
                                 to standard output.
    with two arguments, 'set VAR VALUE' sets specified environment variable of parent job.

    return success, unless 'set VAR' is executed and no such variable is found.\n"))

    (hashtable-set! t "split-at-0" (string->utf8 " alias-or-builtin-or-cmd [arg ...]
    split each ARG ... after each NUL character i.e. Unicode codepoint U+0000,
    and execute the specified alias, builtin or command
    with arguments set to the result of such splitting.

    useful to pass as arguments the NUL-terminated filenames produced by another command,
    as for example 'split-at-0 editor $(find -name \\*.txt -print0)'

    return exit status of executed alias, builtin or command.\n"))

    (hashtable-set! t "status"   (string->utf8 " [int ...]
    return INT value specified as first argument, or success if no arguments.\n"))

    (hashtable-set! t "threads"       (string->utf8 " [arg ...]
    ignore args. write known threads and their status to standard output.

    return success.\n"))

    (hashtable-set! t "unalias"    (string->utf8 " [name ...]
    remove each NAME ... from the list of defined aliases.

    return success.\n"))

    (hashtable-set! t "unexport"   (string->utf8 " [var ...]
    mark each VAR ... environment variable as private in parent job.

    return success.\n"))

    (hashtable-set! t "unset"      (string->utf8 " [var ...]
    remove each VAR ... environment variable from parent job.

    return success.\n"))

    (hashtable-set! t "wait"      (string->utf8 " [job-id]
    move a job to the foreground and wait for it to finish.
    does NOT return if job is stopped.

    return job exit status, or failure if job was not found.\n"))

  )

) ; close begin
