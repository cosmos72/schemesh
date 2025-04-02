;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss

(define msg-interrupted (string->utf8b "; interrupted\n"))
(define msg-quit        (string->utf8b "; quit\n"))
(define msg-suspended   (string->utf8b "; suspended\n"))
(define fd-stderr       2)

(define (fd-write-retry fd bvec)
  ;; called from signal handlers. intentionally does NOT call check-interrupts
  (while (eq? #t (fd-write-noretry fd bvec))
    (void)))


(define (signal-handler-sigint)
  ;; received a SIGINT, for example from a keyboard CTRL+C.
  ;; If there's a sh-expr job running, try to kill it.
  ;; If that fails and there's a non-trivial break handler,
  ;; show what's happening and invoke the break handler.
  (unless (sh-current-job-kill 'sigint)
    (unless (eq? nop (break-handler))
      (fd-write-retry fd-stderr msg-interrupted)
      (break))))


(define (signal-handler-sigquit sig)
  ;; received a SIGQUIT, for example from a keyboard CTRL+4 or CTRL+\.
  ;; If there's a sh-expr job running, try to kill it.
  ;; If that fails and there's a non-trivial break handler,
  ;; show what's happening and invoke the break handler.
  (unless (sh-current-job-kill 'sigquit)
    (unless (eq? nop (break-handler))
      (fd-write-retry fd-stderr msg-quit)
      (break))))


(define (signal-handler-sigtstp sig)
  ;; received a SIGTSTP, for example from a keyboard CTRL+Z.
  ;; If there's a sh-expr job running, try to suspend it.
  ;; If that fails and there's a non-trivial break handler,
  ;; show what's happening and invoke the break handler.
  (unless (sh-current-job-suspend 'sigtstp)
    (unless (eq? nop (break-handler))
      (fd-write-retry fd-stderr msg-suspended)
      (break))))


(define (signal-handler-sigchld sig)
  (unless (sh-current-job-sigchld)
    (unless (waiting-for-job)
      ;;z (debugf "; sigchld, no current job, not waiting for job => calling (scheduler-wait #f 'nonblocking)\n")
      (scheduler-wait #f 'nonblocking))))


;; install Scheme procedures invoked when process receives SIGQUIT or SIGTSTP
(define (install-signal-handlers)
  ;; Chez Scheme has a dedicated parameter for SIGINT handler
  (keyboard-interrupt-handler signal-handler-sigint)

  (for-list ((name    '(sigchld sigquit sigtstp))
            (handler (list signal-handler-sigchld
                           signal-handler-sigquit
                           signal-handler-sigtstp)))
    (let ((sig (signal-name->number name)))
      (register-signal-handler sig handler))))


;; Internal functions called by (sh-start)
(define (job-start caller job options)
  ;b (debugf "job-start ~a ~s" job options)
  (options-validate caller options)
  (job-raise-if-started/recursive caller job)
  (call/cc
    (lambda (k-continue)
      (with-exception-handler
        (lambda (ex)
          (job-start/on-exception caller job options k-continue ex))
        (lambda ()
          (job-start/may-throw caller job k-continue options)))))
  (when (and (job-started? job) (options->spawn? options))
    ; we can cleanup job's file descriptor, as it's running in a subprocess
    (job-unmap-fds! job)
    (job-unredirect/temp/all! job))
  (job-last-status job)) ; returns job status. also checks if job finished


;; raise an exception if a job or one of it recursive children is already started
(define (job-raise-if-started/recursive caller job)
  (when (job-started? job)
    (if (job-id job)
      (raise-errorf caller "job already started with job id ~s" (job-id job))
      (raise-errorf caller "job already started")))
  (when (sh-multijob? job)
    (span-iterate (multijob-children job)
      (lambda (i elem)
        (when (sh-job? elem)
          (job-raise-if-started/recursive caller elem))))))


;; set status 'new in job and all their recursive children.
;; also set child-index of all visited multijobs to -1.
(define (job-status-set-new/recursive! job)
  (%job-last-status-set! job (new))
  (job-exception-set! job #f)
  (cond
    ((sh-expr? job)
      (jexpr-resume-proc-set! job #f)
      (jexpr-suspend-proc-set! job #f))
    ((sh-multijob? job)
      (multijob-current-child-index-set! job -1)
      (span-iterate (multijob-children job)
        (lambda (i elem)
          (when (sh-job? elem)
            (job-status-set-new/recursive! elem)))))))


(define (job-start/may-throw caller job k-continue options)
  (let ((start-proc (job-start-proc job)))
    (unless (procedure? start-proc)
      (raise-errorf caller "cannot start job ~s, bad or missing job-start-proc: ~s" job start-proc))
    (job-oid-set! job #f)
    (job-exception-set! job #f)
    (unless (job-new? job)
      (job-status-set-new/recursive! job))
    (job-status-set/running! job)
    (parameterize ((sh-current-job job))
      (start-proc job options)))  ; may throw
  ;; ignore value returned by job-start-proc
  (void))


(define (job-start/on-exception caller job options k-continue ex)
  (job-default-parents-iterate job
    (lambda (parent)
      (unless (eq? parent (sh-globals))
        (job-exception-set! parent ex))))
  (job-status-set! caller job (exception ex))
  (if (options->catch? options)
    (k-continue (sh-exception-handler ex))
    (raise ex)))


(define (job-start/display-condition obj port)
  (put-string port "; ")
  (display-condition obj port)
  (newline port)
  (flush-output-port port))


;; Start a job and return immediately, without waiting for it to finish.
;; For possible values of options, see (sh-options)
;;
;; Returns job status, typically (running job-id) but other values are allowed.
;; For the complete list of possible returned job statuses, see (sh-job-status).
;;
;; Note that job may finish immediately, for example because it is a builtin,
;;   or a multijob that only (recursively) contains builtins,
;;   or a command that exits very quickly.
;;   For these reasons, the returned job status may be different from (running job-id)
;;   and may indicate that the job has already finished.
(define sh-start
  (case-lambda
    ((job)
      (sh-start job '()))
    ((job options)
      (job-start 'sh-start job options)
      (job-id-update! job)))) ; sets job-id if started, otherwise unsets it. also returns job status


;; Kill a job-or-id.
;;
;; If job is running, return its updated status. Note: may not return, i.e. non-locally jump to job's continuation.
;; If job is not found or not started, raise exception.
(define sh-kill
  (case-lambda
    ((job-or-id)
      (sh-kill job-or-id 'sigint))
    ((job-or-id signal-name-or-condition-object)
      (let ((job (sh-job job-or-id)))
        (if (job-kill job signal-name-or-condition-object)
          (job-last-status job)
          (raise-errorf 'sh-kill "job not started: ~s" job))))))


;; Recursively kill a job and all its children.
;; If job is a sh-expr, may also call its suspend-proc continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; If job is started, return #t. Note: may not return, i.e. non-locally jump to job's continuation.
;; If job is not a sh-job or is not started, immediately return #f.
(define (job-kill job signal-name-or-condition-object)
  ;;y (debugf "job-kill job=~s suspend-proc=~s" job suspend-proc)
  (if (and (sh-job? job) (job-started? job))
    (let* ((signal-name  (if (symbol? signal-name-or-condition-object)
                           signal-name-or-condition-object
                           'sigint))
           (ex           (and (not (symbol? signal-name-or-condition-object))
                              signal-name-or-condition-object))
           (fatal?       (or ex (signal-name-is-usually-fatal? signal-name)))
           (suspend-proc (and (sh-expr? job) (jexpr-suspend-proc job)))
           (pid  (job-pid job))
           (pgid (job-pgid job))
           ;; send signals to job's process group, if present.
           ;; otherwise send signals to job's process id.
           (target-pid (if (and pgid (> pgid 0)) (- pgid) pid)))
      ;; set job id and notify its status? causes verbose notifications...
      ;; (job-id-set! job)
      ;; (queue-job-display-summary job)

      (when ex
        (assert* 'job-kill (condition? ex)))

      (cond
        (target-pid ; also catches sh-pipe multijobs
          (when fatal?
            (pid-kill target-pid 'sigcont))
          (pid-kill target-pid signal-name)

          (cond
            ((and pid (or fatal? (eq? 'sigcont signal-name)))
              ;; if job has a pid, status will be set when subprocess exits
              (job-status-set/running! job))
            (fatal?
              (job-status-set! 'job-kill job (killed signal-name)))))
        (suspend-proc
          (when fatal?
            (job-status-set! 'job-kill job
              (if ex (exception ex) (killed signal-name)))
            ;; should not return
            (suspend-proc (void))))
        ((sh-multijob? job)
          (when fatal?
            (job-status-set! 'job-kill job
              (if ex (exception ex) (killed signal-name))))
          (multijob-kill job signal-name)))
        #t)
      #f))



;; Kill current job and call its suspend-proc continuation,
;; which non-locally jumps to whoever started or resumed the job.
;; If current job is not an sh-expr or is not running, immediately return #f.
;;
;; Used by (signal-handler-sigint) and (signal-handler-sigquit) to kill sh-expr jobs.
(define (sh-current-job-kill signal-name-or-condition-object)
  (job-kill (sh-current-job) signal-name-or-condition-object))


;; React to a SIGCHLD: if (sh-current-job) is an sh-expr job,
;; check whether some job stopped (TBD: or was killed?)
;; and in such case suspend (sh-current-job)
;;
;; Return #t if current job is a sh-expr, otherwise return #f.
;; May also not return i.e. non-locally jump to current job's suspend-proc.
(define (sh-current-job-sigchld)
  (jexpr-sigchld (sh-current-job)))


;; Suspend current job and call its suspend-proc continuation,
;; which non-locally jumps to whoever started or resumed the job.
;; If current job is not an sh-expr or is not running, immediately return #f.
;;
;; If current job is later resumed, it eventually returns #t to the caller of (sh-current-job-suspend).
(define (sh-current-job-suspend signal-name)
  (jexpr-suspend (sh-current-job) signal-name))




(meta begin
  ;; helper function used by macros (sh-wait-flag) and (sh-wait-flags)
  (define name->sh-wait-flag
    (let ((alist '((foreground-pgid . 1) (continue-if-stopped . 2)
                   (wait-until-stopped-or-finished . 4) (wait-until-finished . 8))))
      (lambda (name)
        (let ((pair (assq name alist)))
          (unless pair
            (raise-assertf 'sh-wait-flags "~s is not a sh-wait-flags name" name))
          (cdr pair)))))

  ;; helper function used by macros (sh-wait-flag) and (sh-wait-flags)
  (define (names->sh-wait-flags names)
    (let %loop ((names names) (ret 0))
      (if (null? names)
        ret
        (%loop (cdr names) (fxior ret (name->sh-wait-flag (car names)))))))

) ; close meta


;; create and return flags for (sh-wait),
;;   which subsumes (sh-bg) (sh-fg) (sh-wait) (sh-job-status).
;;
;; this macro converts zero or more symbols among
;;   foreground-pgid
;;   continue-if-stopped
;;   wait-until-finished
;;   wait-until-stopped-or-finished
;; to the corresponding wait-flags value, typically for passing it to (sh-wait).
;;
;; Returns wait-flags value.
;; Raises exception if an element in names is not one of the symbols above.
(define-syntax sh-wait-flags
  (lambda (stx)
    (syntax-case stx ()
      ((_ . names)
        (names->sh-wait-flags (syntax->datum #'names))))))


(define-syntax sh-wait-flags-add
  (lambda (stx)
    (syntax-case stx ()
      ((_ name-to-add ... wait-flags)
        #`(fxior #,(names->sh-wait-flags (syntax->datum #'(name-to-add ...)))
                 wait-flags)))))


(define-syntax sh-wait-flags-remove
  (lambda (stx)
    (syntax-case stx ()
      ((_ name-to-remove ... wait-flags)
        #`(fxand (fxnot #,(names->sh-wait-flags (syntax->datum #'(name-to-remove ...))))
                 wait-flags)))))


(define (sh-wait-flag-foreground-pgid? wait-flags)
  (not (fxzero? (fxand wait-flags 1))))

(define (sh-wait-flag-continue-if-stopped? wait-flags)
  (not (fxzero? (fxand wait-flags 2))))

(define (sh-wait-flag-wait-until-stopped-or-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 4))))

(define (sh-wait-flag-wait-until-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 8))))

(define (sh-wait-flag-wait? wait-flags)
  (not (fxzero? (fxand wait-flags 12))))

(define (notrace-call arg)
  arg)

(define (job-wait-once caller job wait-flags)
  ;; (debugf "job-wait-once\tcaller=~s\twait-flags=~s\tjob=~a\tid=~s\tpid=~s\tstatus=~s" caller wait-flags job (job-id job) (job-pid job) (job-last-status job))
  (case (job-last-status->kind job)
    ((ok exception failed killed)
      (void)) ; job finished
    ((running stopped)
      (cond
        ((job-pid job)
          ;; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
          ;; in all cases, we have a pid to wait on.
          (notrace-call (pid-advance     caller job wait-flags)))
        ((sh-expr? job)
          (notrace-call (jexpr-advance   caller job wait-flags)))
        ((sh-multijob-pipe? job)
          (notrace-call (mj-pipe-advance caller job wait-flags)))
        ((sh-multijob? job)
          (notrace-call (mj-advance      caller job wait-flags))))
      ;x (debugf "...job-wait-once job=~s\tstatus=~s" job (job-last-status job))
      (job-last-status job))
    (else
      (raise-errorf caller "job not started yet: ~s" job))))


;; Internal function called by (job-wait) when a job is stopped
;; and caller asked to wait until job finishes:
;;
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-break job)
   ;; subshells should not directly perform I/O,
   ;; they cannot write the "break> " prompt then read commands
   (when (sh-job-control?)
     (let ((current-job-swap (parameter-swapper sh-current-job job))
           (break-returned-normally? #f))
      (dynamic-wind
        current-job-swap ; before body
        (lambda () ; body
          ; (job-id-set! job) ; verbose
          (warnf "; suspended job: ~a\n" (sh-job->string job))
          (break)
          (set! break-returned-normally? #t))
        (lambda () ; after body
          (current-job-swap)
          (if break-returned-normally?
            (pgid-continue job)
            (job-kill job 'sigint)))))))


;; send sigcont to job's pgid or pid, if present
(define (pgid-continue job)
  (let* ((pgid (job-pgid job))
         ;; send signals to job's process group, if present.
         ;; otherwise send signals to job's process id.
         (target-pid (if (and pgid (> pgid 0)) (- pgid) (job-pid job))))
    (when target-pid
      (pid-kill target-pid 'sigcont))))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
;; Returns updated job status.
(define (job-wait caller job wait-flags)
  (when (job-started? job)
    (parameterize ((waiting-for-job job))
      (let %loop ()
        (job-wait-once caller job wait-flags)
        (let ((status (job-last-status job)))
          (case (status->kind status)
            ((running)
              (when (sh-wait-flag-wait? wait-flags)
                (%loop)))
            ((stopped)
              (when (sh-wait-flag-wait-until-finished? wait-flags)
                ;x (debugf "...job-wait\tcaller=~s\tjob=~a\tcurrent-job=~s\tcalling sh-current-job-suspend..." caller job (sh-current-job))
                (or (sh-current-job-suspend (status->value status))
                    ;; caller cannot be suspended, for example because it's not a sh-expr job,
                    ;; but we cannot return, because caller asked to wait until job finished - stopping it is not enough.
                    ;;
                    ;; the only way out is to notify the user that job was suspended
                    ;; then invoke break-handler, which lets them decide how to proceed.
                    (job-break job))
                ;x (debugf "...job-wait\tcaller=~s\tjob=~a\tcurrent-job=~s ... sh-current-job-suspend returned" caller job (sh-current-job))
                (%loop))))))))
  (job-last-status job))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;; Sets or unsets job's id as needed.
;;
;; Returns updated job status.
(define (job-wait/id+raise caller job-or-id wait-flags)
  (let ((job (sh-job job-or-id)))
    (job-wait caller job wait-flags)
    (let ((status (job-id-update! job)))
      (if (eq? 'exception (status->kind status))
	(raise (status->value status))
	status))))


;; Return up-to-date status of a job or job-id, which can be one of:
;;   (new)
;;   (running)              ; job is running, has no job-id
;;   (running   job-id)
;;   (void)                       ; job exited successfully, i.e. with C exit-status = 0
;;   (ok        result ...) ; job is a Scheme procedure that successfully returned zero or more results
;;   (failed    exit-status)
;;   (stopped   signal-name)
;;   (killed    signal-name)
;;   (exception condition-object)
;;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-or-id)
  (job-wait 'sh-job-status (sh-job job-or-id) (sh-wait-flags)))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible job statuses, see (sh-job-status)
(define (sh-bg job-or-id)
  (job-wait/id+raise 'sh-bg job-or-id (sh-wait-flags continue-if-stopped)))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
   (job-wait/id+raise 'sh-fg job-or-id
                (sh-wait-flags foreground-pgid continue-if-stopped wait-until-stopped-or-finished)))


;; General function to resume and optionally wait for a job.
;; Subsumes (sh-fg) (sh-bg) (sh-job-status)
;;
;; Continue a job or job-id by optionally setting the job as fg process group,
;; optionally sending SIGCONT to it,
;; then optionally wait for it to stop or exit,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; If wait-flags is not specified or does not contain wait-until-stopped-or-finished
;; and job gets stopped, calls (break). Then, if (break) raises an exception or resets scheme,
;; the job is interrupted with SIGINT; otherwise waits again for the job to exit.
;;
;; Returns updated job status.
(define sh-wait
  (case-lambda
    ((job-or-id)
      (sh-wait job-or-id
               (sh-wait-flags foreground-pgid continue-if-stopped wait-until-finished)))
    ((job-or-id wait-flags)
      (job-wait/id+raise 'sh-wait job-or-id wait-flags))))


;; Start a job and wait for it to exit or stop.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-fg)
(define sh-run/i
  (case-lambda
    ((job)
      (sh-run/i job '()))
    ((job options)
      (job-start 'sh-run/i job options)
      (sh-fg job))))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-wait)
(define sh-run
  (case-lambda
    ((job)
      (sh-run job '()))
    ((job options)
      (job-start 'sh-run job options)
      (sh-wait job))))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return #t if job failed successfully, otherwise return #f.
(define sh-run/ok?
  (case-lambda
    ((job)
      (sh-run/ok? job '()))
    ((job options)
      (ok? (sh-run job options)))))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return #f if job exited successfully,
;; otherwise return job exit status, which is a status object and hence truish.
(define sh-run/err?
  (case-lambda
    ((job)
      (sh-run/err? job '()))
    ((job options)
      (let ((status (sh-run job options)))
        (if (ok? status) #f status)))))
