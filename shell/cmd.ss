;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss



(define (make-sh-cmd program-and-args)
  (let ((current-job (sh-current-job)))
    (%make-cmd
      #f #f #f #f     ; id oid pid pgid
      '(new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      cmd-start #f    ; start-proc resume-proc
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      (and current-job (job-parent current-job)) ; temp parent job
      (or current-job (sh-globals))              ; default parent job
      program-and-args
      #f)))           ; expanded arg-list


;; Create a cmd to later spawn it. Each argument must be a string.
;; If you want to use procedures as args, see (sh-cmd*)
(define (sh-cmd . program-and-args)
  (assert-string-list? 'make-sh-cmd program-and-args)
  (make-sh-cmd program-and-args))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Call any procedure found in (cmd-arg-list c),
;; then repeatedly expand aliases in the produced argument list.
;;
;; If the argument list is empty, copy overridden environment variables,
;; including lazy ones, to parent process. This implements the syntax "ENV_VAR" '= "VALUE"
;;
;; Otherwise, if the argument list starts with the name of a builtin,
;; execute such builtin.
;;
;; Otherwise, start a command i.e. fork() and exec() an external process,
;; optionally inserting it into an existing process group.
;;
;; The new process is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: an integer, if present and > 0 the new process will be inserted
;;   into the corresponding process group id - which must already exist.
;;
;; Returns job status, which is also stored in (sh-job-last-status)
;;   and may be one of (void) '(ok ...) '(failed ...) '(running ...) '(stopped ...) '(killed ...) '(exception ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (cmd-start c options)
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))
  (job-status-set! 'cmd-start c
    (let ((prog-and-args (cmd-arg-list c)))
      (if (string-list? prog-and-args)

        ;; all command line arguments are strings, proceed
        (start-command-or-builtin-or-alias c prog-and-args options)

        ;; some command line argument is a closure:
        ;; setup fds remapping before calling them, because they may want to use
        ;; (sh-fd-stdin) (sh-fd-stdout) (sh-fd-stderr) or more generally, (job-find-fd-remap)
        (with-remapped-fds c
          (start-command-or-builtin-or-alias c
            (cmd-arg-list-call-closures c prog-and-args) options))))))


;; internal function called by (cmd-start):
;; call procedures in prog-and-args.
;; Return the expanded command line, which is always a list of strings.
(define (cmd-arg-list-call-closures c prog-and-args)
  (let ((l '()))
    (list-iterate prog-and-args
      (lambda (arg)
        (set! l (cmd-arg-call-closure c arg l))))
    (set! l (reverse! l))
    (assert-string-list? 'sh-start l)
    l))


;; internal function called by (cmd-start):
;; if arg is a procedure then call it, optionally passing current job as the only argument.
;; Such procedure must return a string or list-of-strings, which are reverse-consed
;; at the beginning of list-of-strings l.
;; Return the updated list.
(define (cmd-arg-call-closure c arg l)
  (let ((expanded
          (cond
            ((not (procedure? arg)) arg)
            ((logbit? 1 (procedure-arity-mask arg)) (arg c)) ; call closure (lambda (job) ...)
            (else (arg)))))                                  ; call closure (lambda () ...)
    ; (debugf "cmd-arg-call-closure cmd=~s arg=~s expanded=~s l=~s" c arg expanded l)
    (cond
      ((eq? (void) expanded)
        l)
      ((null? expanded)
        l)
      ((pair? expanded)
        (list-iterate expanded
          (lambda (e)
            (assert* 'sh-start (string? e))
            (set! l (cons e l))))
        l)
      ((string? expanded)
        (cons expanded l))
      (else
        (raise-errorf 'sh-start "value ~s returned by closure ~s in job ~s is not a string, a list of strings, or (void)"
          expanded arg c)))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Repeatedly expand aliases in args
;;
;; If the expanded argument list is empty, copy overridden environment variables,
;; including lazy ones, to parent process. This implements the syntax "ENV_VAR" '= "VALUE"
;;
;; Otherwise, if the expand argument list starts with the name of a builtin,
;; execute such builtin.
;;
;; Otherwise, start a command i.e. fork() and exec() an external process,
;; optionally inserting it into an existing process group.
;;
;; Returns job status,
;;   which may be one of (void) '(failed ...) '(running ...) etc.
;;   For the complete list of possible returned job statuses, see (sh-job-status).
(define (start-command-or-builtin-or-alias c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

  ;; expand aliases in args
  ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
  (let ((prog-and-args (sh-aliases-expand program-and-args)))

    ;; (debugf "cmd-start expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))

    ;; apply lazy environment variables *after* expanding cmd-arg-list
    ;; and *after* setting job's parent
    (job-env/apply-lazy! c 'export)

    (if (null? prog-and-args)
      (job-env-copy-into-parent! c) ; return (void) job status = success
      (let ((builtin (sh-find-builtin prog-and-args)))
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          (builtin-start builtin c prog-and-args options)  ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (cmd-spawn c prog-and-args options)))))) ; returns job status


;; returns job status
(define (start-command-or-builtin-or-alias-from-another-builtin c program-and-args options)
  (assert* 'sh-cmd (sh-cmd? c))

  ;; expand aliases in args
  ;; sanity: (sh-aliases-expand) ignores aliases for "builtin"
  (let ((prog-and-args (sh-aliases-expand program-and-args)))
    ;; (debugf "cmd-start expanded-prog-and-args=~s builtin=~s" prog-and-args builtin)
    (unless (eq? prog-and-args (cmd-arg-list c))
      ;; save expanded cmd-arg-list for more accurate pretty-printing
      (cmd-expanded-arg-list-set! c prog-and-args))
    ; lazy environment was applied already by the outer (start-command-or-builtin-or-alias)
    (if (null? prog-and-args)
      (void) ; return success
      (let ((builtin (sh-find-builtin prog-and-args)))
        (if builtin
          ; expanded arg[0] is a builtin, call it.
          (builtin-start builtin c prog-and-args options) ; returns job status
          ; expanded arg[0] is a not builtin or alias, spawn a subprocess
          (cmd-spawn c prog-and-args options)))))) ; returns job status


;; internal function called by (cmd-start) to spawn a subprocess.
;; On errors spawning the subprocess, raises an exception.
;; On success, non-locally jumps to whoever requested this cmd start, by calling (job-yield) or (job-suspend).
;;
;; This function is then non-locally re-entered each time cmd is resumed:
;; if cmd has finished, normally returns job status.
(define cmd-spawn
  (let ((c-cmd-spawn (foreign-procedure "c_cmd_spawn" (ptr ptr ptr ptr int) int)))
    (lambda (c prog-and-args options)
      (let* ((process-group-id (options->process-group-id options))
             (job-dir (job-cwd-if-set c))
             (ret (c-cmd-spawn
                    (list->argv prog-and-args)
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export)
                    (or process-group-id -1))))
        ;;y (debugf "cmd-spawn: started job=~a\tpid=~s\tprog-and-args=~s" (sh-job->string c) ret prog-and-args)

        (when (< ret 0)
          (job-status-set! 'cmd-spawn c (list 'failed ret))
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c process-group-id)
        (job-status-set-running! 'cmd-spawn c)
        (cmd-loop c)))))


;; Wait for job's pid to exit or stop, calling (job-yield) or (job-suspend) each time it needs to wait.
;; NEVER returns normally - only yields.
(define (cmd-loop c)
  ;; (debugf "->   cmd-loop cmd=~a" (sh-job->string cmd))
  (forever
    ;; (debugf "... cmd-loop cmd=~a status=~s" (sh-job->string cmd) status)
    (case (job-last-status->kind c)
      ((running)
        ;; (debugf "... cmd-loop cmd=~a --- calling yield" (sh-job->string cmd))
        (job-yield c '(cmd-loop running))
        ;; (debugf "... cmd-loop cmd=~a --- yield returned,\n\t\tcalling resume on child, wait-flags=~s" (sh-job->string cmd))
       )
      ((stopped)
         ;; (debugf "... cmd-loop cmd=~a --- calling suspend" (sh-job->string cmd))
         (job-suspend c)
         ;; (debugf "... cmd-loop cmd=~a --- suspend returned,\n\t\tcalling resume on cmd, wait-flags=~s" (sh-job->string cmd))
         )
      (else
        ;; (debugf "<-  cmd-loop cmd=~a status=~s" (sh-job->string cmd) status)
        (job-finish c)))))




;; internal function called by (builtin-exec) to exec a subprocess.
;; if C exec() fails, returns job status.
(define cmd-exec
  (let ((c-cmd-exec (foreign-procedure "c_cmd_exec" (ptr ptr ptr ptr) int)))
    (lambda (c argv options)
      (let* ((job-dir (job-cwd-if-set c))
             (ret (c-cmd-exec
                    argv
                    (if job-dir (text->bytevector0 job-dir) #f)
                    (job-make-c-redirect-vector c)
                    (sh-env->argv c 'export))))
        ; (c-cmd-exec) returns only if it failed
        (list 'failed (if (and (integer? ret) (not (zero? ret))) ret -1))))))


;; internal function called by (cmd-spawn)
;; creates and fills a vector with job's redirections and its parents redirections
(define (job-make-c-redirect-vector job)
  (let* ((child-dir (job-cwd-if-set job))
         (n (job-count-c-redirect-vector job 0))
         (v (make-vector n)))
    (do ((parent job (job-parent parent)))
        ((not parent))
      (set! n (job-fill-c-redirect-vector/norecurse parent child-dir v n)))
    ; (debugf "job-make-c-redirect-vector job=~s redirect-vector=~s" job v)
    v))


;; count and return the total number of redirections (* 4) of a job,
;; including its parents redirections
(define (job-count-c-redirect-vector job n)
  (if job
    ; add job's redirect count to n, and recurse to parent
    (job-count-c-redirect-vector (job-parent job) (fx+ n (span-length (job-redirects job))))
    n))


;; copy job's redirections to vector v, without recursing to job's parents.
;; returns (fx- pos (number-of-copied-elements))
(define (job-fill-c-redirect-vector/norecurse job child-dir v end-pos)
  (let ((parent-dir (job-cwd-if-set job))
        (n          (span-length (job-redirects job))))
    (do ((index (fx- n 4)  (fx- index 4))
         (pos   end-pos    (fx- pos 4)))
        ((fx<? index 0) pos)
      (job-fill-c-redirect-vector/at job parent-dir child-dir v index (fx- pos 4)))))


;; copy a single job redirection to vector v, at v[pos] ... v[pos+3]
;;
;; note: must prefix any relative path with job's working directory,
;; because job may be a parent job with a different working directory
(define (job-fill-c-redirect-vector/at job parent-dir child-dir v index pos)
  (let* ((dir           (%parent-dir-if-different parent-dir child-dir))
         (redirects     (job-redirects job))
         (fd            (span-ref redirects index))
         (direction-ch  (span-ref redirects (fx1+ index)))
         ;; fd may need to be redirected to a different file descriptor due to fd remapping
         (remapped-fd   (job-find-fd-remap job fd))
         (to            (if (fx=? fd remapped-fd)
                          ; no remapping found, extract redirection.
                          (job-extract-redirection-to-fd-or-bytevector0 job dir redirects index)
                          ; remapping found, use it
                          remapped-fd)))

    ; (debugf "job-fill-c-redirect-vector job=~s redirect fd=~s -> fd=~s, remapped-fd = ~s" job fd to remapped-fd)
    (vector-set! v pos fd)
    (vector-set! v (fx1+  pos) direction-ch)
    ;; to-fd must be placed at pos + 2
    (vector-set! v (fx+ 2 pos) (if (fixnum? to) to #f))
    ;; to-bytevector0 must be placed at pos + 3
    (vector-set! v (fx+ 3 pos) (if (fixnum? to) #f to))))


;; return parent-dir if different from child-dir, otherwise return #f
;; Note: parent-dir and child-dir may be #f
(define (%parent-dir-if-different parent-dir child-dir)
  (cond
    ((and parent-dir child-dir)
      (if (charspan=? parent-dir child-dir) #f parent-dir))
    (child-dir
      (sh-cwd))
    (else
      parent-dir)))


;; Convert value returned by (pid-wait) to a symbolic job status.
;;
;; If (pid-wait) succeeds, it returns a pair (pid . wait-status).
;; Passing wait-status to this function converts it to a job status as follows:
;;   not a fixnum, or < 0 => return (list 'failed wait-status)
;;   0                    => return (void)
;;   1..255               => return (list 'failed  wait-status)
;;   256 + kill_signal    => return (list 'killed  signal-name)
;;   512 + stop_signal    => return (list 'stopped signal-name)
;;   768                  => return (list 'running)
;;   > 768                => return (list 'failed  (fx- wait-status 512))
;;
(define (pid-wait-result->status wait-status)
  (let ((x wait-status))
    (cond ((not (fixnum? x)) (list 'failed x))
          ((fx=? x   0) (void))
          ((fx<? x 256) (list 'failed  x))
          ((fx<? x 512) (list 'killed  (signal-number->name (fxand x 255))))
          ((fx<? x 768) (list 'stopped (signal-number->name (fxand x 255))))
          ((fx=? x 768) '(running))
          (else         (list 'failed  (fx- x 512))))))


(define %pgid-foreground
  (let ((c-pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int)))
    (lambda (old-pgid new-pgid)
      ;; (debugf "%pgid-foreground old-pgid=~s new-pgid=~s job-control?=~s job-control-available?=~s" old-pgid new-pgid (sh-job-control?) (sh-job-control-available?))
      (let ((err (c-pgid-foreground old-pgid new-pgid)))
        ; (c-pgid-foreground) may fail if new-pgid failed in the meantime
        ; (when (< err 0)
        ;  (raise-c-errno caller 'tcsetpgrp err new-pgid))
        err))))


(define (call/foreground-pgid wait-flags new-pgid proc)
   (proc))

(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  wait-flags new-pgid body1 body2 ...)
      (begin (body1 body2 ...)))))
