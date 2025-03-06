;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


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
    (sh-globals
      ;; assign job-id 0 to sh-globals itself.
      ;;
      ;; waiting for sh-globals to exit is not useful:
      ;; pretend it already finished successfully
      (%make-multijob
         0 #f                      ; id oid
         (pid-get) (pgid-get 0)    ; pid pgid
         (void) #f                 ; last-status exception
         (span) 0 #f               ; redirections
         #f #f                     ; start-proc step-proc
         (string->charspan* ((foreign-procedure "c_get_cwd" () ptr))) #f ; current directory, old working directory
         (make-hashtable string-hash string=?) ; env variables
         #f                        ; no env var assignments
         #f #f                     ; no temp parent, no default parent
         'sh-globals -1 (span #t)))) ; skip job-id 0, is used by (sh-globals) itself

  (c-environ->sh-global-env)

  ;; install a yield-handler
  (yield-handler sh-current-job-yield)

  ;; Replace (current-input-port) with an UTF-8b textual output port that honors current job redirections
  ;;
  ;; Cannot create a buffered port: input buffer would need to be per-job
  (current-input-port (make-utf8b-input/output-port (sh-stdin) (buffer-mode none)))

  ;; Replace (current-output-port) with an UTF-8b textual output port that honors current job redirections
  ;;
  ;; Cannot create a buffered port: input buffer would need to be per-job
  (current-output-port (make-utf8b-input/output-port (sh-stdout) (buffer-mode none)))

  ;; Replace (current-error-port) with an UTF-8b textual output port that honors current job redirections
  ;;
  ;; Cannot create a buffered port: input buffer would need to be per-job
  (current-error-port (make-utf8b-input/output-port (sh-stderr) (buffer-mode none)))


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
    (hashtable-set! bt "unalias"    builtin-unalias)
    (hashtable-set! bt "unexport"   builtin-unexport)
    (hashtable-set! bt "unsafe"     builtin-unsafe)
    (hashtable-set! bt "unset"      builtin-unset)
    (hashtable-set! bt "value"      builtin-value)

    ;; mark builtins that finish immediately i.e. cannot run commands or aliases
    (list-iterate '("alias" "cd" "cd-" "echo" "echo0" "exit" "false" "jobs"
                    "history" "pwd" "set" "true" "unalias" "unset" "value")
      (lambda (name)
        (let ((builtin (hashtable-ref bt name #f)))
          (when builtin
            (hashtable-set! ft builtin #t))))))


  (let ((t (sh-builtins-help)))

    (hashtable-set! t "alias"   (string->utf8 " [name [expansion ...]]
    define or display aliases.

    without arguments,          'alias' writes the list of defined aliases to standard output.
    with a single argument,     'alias NAME' writes the definition of alias NAME to standard output.
    with two or more arguments, 'alias NAME EXPANSION ...' defines an alias NAME such that,
                                 when NAME ARGS ... executed, it is substituted with EXPANSION ... ARGS ...

    return success, unless 'alias NAME' is executed and no such alias is defined.\n"))

    (hashtable-set! t "bg"      (string->utf8 " job-id
    move a job to the background.

    return success if job-id was found, otherwise return failure.\n"))

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

    (hashtable-set! t "fg"      (string->utf8 " job-id
    move a job to the foreground.

    return success if job-id was found, otherwise return failure.\n"))

    (hashtable-set! t "global"     (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with its parent temporarily set to the shell itself.

    useful mostly for builtins 'cd' 'export' 'set' 'pwd' 'unexport' 'unset'
    that show or alter the current directory or the environment variables of their parent job.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "jobs"       (string->utf8 " [arg ...]
    ignore arguments. write jobs and their status to standard output.

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

    (hashtable-set! t "unalias"    (string->utf8 " [name ...]
    remove each NAME ... from the list of defined aliases.

    return success.\n"))

    (hashtable-set! t "unexport"   (string->utf8 " [var ...]
    mark each VAR ... environment variable as private in parent job.

    return success.\n"))

    (hashtable-set! t "unsafe"     (string->utf8 " [alias-or-builtin-or-cmd [arg ...]]
    execute the specified alias, builtin or command.

    this builtin is only needed when ALIAS-OR-BUILTIN-OR-CMD is a non-constant expression,
    as for example a wildcard or the value of an environment variable.

    return exit status of executed alias, builtin or command.\n"))

    (hashtable-set! t "unset"      (string->utf8 " [var ...]
    remove each VAR ... environment variable from parent job.

    return success.\n"))

    (hashtable-set! t "value"   (string->utf8 " [int ...]
    return INT value specified as first argument, or success if no arguments.\n"))
  )

) ; close begin
