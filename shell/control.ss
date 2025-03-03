;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Start a job and return immediately, without waiting for it to finish.
;; For possible values of options, see (sh-options)
;;
;; Returns job status, typically (list 'running job-id) but other values are allowed.
;; For the complete list of possible returned job statuses, see (sh-job-status).
;;
;; Note that job may finish immediately, for example because it is a builtin,
;;   or a multijob that only (recursively) contains builtins,
;;   or a command that exits very quickly.
;;   For these reasons, the returned job status may be different from (list 'running job-id)
;;   and may indicate that the job has already finished.
;;
(define (sh-start job . options)
  (sh-start* job options))


;; same as sh-start, options must be passed as a single association list.
(define (sh-start* job options)
  (job-start 'sh-start job options)
  (job-id-update! job)) ; sets job-id if started, otherwise unsets it. also returns job status



;; Internal functions called by (sh-start)
(define (job-start caller job options)
  ;b (debugf "job-start ~a ~s" (sh-job->string job) options)
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


;; set status '(new) in job and all their recursive children.
;; also set child-index of all visited multijobs to -1.
(define (job-status-set-new/recursive! job)
  (%job-last-status-set! job '(new))
  (job-exception-set! job #f)
  (job-resume-proc-set! job #f)
  (when (sh-multijob? job)
    (multijob-current-child-index-set! job -1)
    (span-iterate (multijob-children job)
      (lambda (i elem)
        (when (sh-job? elem)
          (job-status-set-new/recursive! elem))))))


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
  (job-status-set! caller job (list 'exception ex))
  (if (options->catch? options)
    (k-continue (sh-exception-handler ex))
    (raise ex)))


(define (job-start/display-condition obj port)
  (put-string port "; ")
  (display-condition obj port)
  (newline port)
  (flush-output-port port))



;; suspend a job and call its (job-suspend-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (job-suspend)
(define (job-suspend job)
  (let ((suspend-proc (and (sh-job? job) (job-suspend-proc job))))
    (when suspend-proc
      (call/cc
        ;; Capture the continuation representing THIS call to (job-suspend)
        (lambda (cont)
          ;; store it as job's resume-proc
          (job-resume-proc-set!  job cont)
          (job-suspend-proc-set! job #f)
          (%job-last-status-set! job '(stopped sigtstp))
          ;; suspend job, i.e. call its suspend-proc
          (suspend-proc (void)))))
    (if suspend-proc #t #f))) ; ignore value returned by continuations (suspend-proc) and (cont)


;; Suspend current job and call its (job-suspend-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (sh-current-job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (sh-current-job-suspend)
;;
;; Note: has effect only if (sh-current-job) is set
(define (sh-current-job-suspend)
  (job-suspend (sh-current-job)))


;; flags for (sh-resume), which subsumes (sh-bg) (sh-fg) (sh-wait) (sh-job-status)
;;
(define jr-flag-sigcont 1)
(define jr-flag-foreground 2)
(define jr-flag-wait-until-finished 4)
(define jr-flag-wait-until-stopped-or-finished 8)


(define (jr-flag-foreground? wait-flags)
  ;; (debugf "jr-flag-foreground? wait-flags=~s" wait-flags)
  (not (fxzero? (fxand wait-flags jr-flag-foreground))))

(define (jr-flag-sigcont? wait-flags)
  (not (fxzero? (fxand wait-flags jr-flag-sigcont))))

(define (jr-flag-wait? wait-flags)
  (not (fxzero? (fxand wait-flags
                       (fxior jr-flag-wait-until-finished
                              jr-flag-wait-until-stopped-or-finished)))))

(define (jr-flag-wait-until-finished? wait-flags)
  (not (fxzero? (fxand wait-flags jr-flag-wait-until-finished))))

(define (jr-flag-wait-until-stopped-or-finished? wait-flags)
  (not (fxzero? (fxand wait-flags jr-flag-wait-until-stopped-or-finished))))



;; General function to resume and optionally wait for a job.
;; Subsumes (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;;
;; Argument wait-flags must be (fxior ...) of zero or more constants jr-flag-...
;;
;; Returns updated job status.
(define (sh-resume job-or-id wait-flags)
  (job-resume 'sh-resume job-or-id wait-flags))


;; Common implementation of (sh-fg) (sh-bg) (sh-resume) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
;; Returns updated job status.
(define (job-resume caller job-or-id wait-flags)
  (let ((job (sh-job job-or-id)))
    ;; (debugf ">  job-resume caller=~s wait-flags=~s job=~a id=~s pid=~s status=~s" caller wait-flags (sh-job->string job) (job-id job) (job-pid job) (job-last-status job))
    (case (job-last-status->kind job)
      ((ok exception failed killed)
        (void)) ; job finished
      ((running stopped)
        (cond
          ((job-pid job)
            ;; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
            ;; in all cases, we have a pid to wait on.
            (advance-pid caller job wait-flags))
          ((job-resume-proc job)
            ;; we have a continuation to call for resuming the job
            (job-call-resume-proc job wait-flags))
          ((sh-multijob? job)
            (if (eq? 'sh-pipe (multijob-kind job))
              (mj-pipe-advance caller job wait-flags)
              (mj-advance      caller job wait-flags)))))
      (else
        (raise-errorf caller "job not started yet: ~s" job)))

    (job-id-update! job))) ; returns job status


;; call the continuation stored in job-resume-proc of a job for resuming it.
;; save the current continuation in its job-suspend-proc
(define (job-call-resume-proc job wait-flags)
  (call/cc
    ;; Capture the continuation representing THIS call to (job-call-resume-proc)
    (lambda (susp)
      (let ((resume-proc (job-resume-proc job))
            (pgid (job-pgid job)))
        (job-resume-proc-set!  job #f)
        (job-suspend-proc-set! job susp)
        (job-status-set/running! job)
        (with-foreground-pgid wait-flags pgid
          ;; send SIGCONT to job's process group, if present.
          (when (and pgid (jr-flag-sigcont? wait-flags))
            (pid-kill (- pgid) 'sigcont))
          (parameterize ((sh-current-job job))
            (resume-proc (void)))))))
  ;; ignore the value returned by (resume-proc) and by continuation (susp)
  (void))


;; Return up-to-date status of a job or job-id, which can be one of:
;;   (list 'new)
;;   (list 'running)              ; job is running, has no job-id
;;   (list 'running   job-id)
;;   (void)                       ; job exited successfully, i.e. with C exit-status = 0
;;   (list 'ok        result ...) ; job is a Scheme procedure that successfully returned zero or more results
;;   (list 'failed    exit-status)
;;   (list 'stopped   signal-name)
;;   (list 'killed    signal-name)
;;   (list 'exception condition-object)
;;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-or-id)
  (let* ((job    (sh-job job-or-id))
         (status (job-last-status job)))
    ; (debugf ">  sh-job-status job=~a" (sh-job->string job))
    (if (sh-started? status)
      (job-resume 'sh-job-status job 0)
      status)))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible job statuses, see (sh-job-status)
(define (sh-bg job-or-id)
  (job-resume 'sh-bg job-or-id jr-flag-sigcont))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (job-resume
    'sh-fg
    job-or-id
    (fxior jr-flag-foreground jr-flag-sigcont jr-flag-wait-until-stopped-or-finished)))


;; Continue a job or job-id by optionally sending SIGCONT to it, then wait for it to exit,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Does NOT return early if job gets stopped, use (sh-fg) for that.
;;
;; Instead if job gets stopped, calls (break).
;; if (break) raises an exception or resets scheme, the job is interrupted with SIGINT.
;; otherwise waits again for the job to exit.
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-wait job-or-id)
  (job-resume
    'sh-sigcont+wait
    job-or-id
    (fxior jr-flag-foreground jr-flag-sigcont jr-flag-wait-until-finished)))


;; Start a job and wait for it to exit or stop.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (if (sh-running? (job-start 'sh-run/i job options))
    (sh-fg job)
    (job-id-update! job))) ; sets job-id if started, otherwise unsets it. also returns job status


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (job-start 'sh-run job options)
  (sh-wait job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return #t if job failed successfully, otherwise return #f.
(define (sh-run/ok? job . options)
  (sh-ok? (apply sh-run job options)))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return #f if job exited successfully,
;; otherwise return job exit status, which is a cons and hence truish.
(define (sh-run/err? job . options)
  (let ((status (apply sh-run job options)))
    (if (sh-ok? status) #f status)))
