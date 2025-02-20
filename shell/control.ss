;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Start a job and return immediately, without waiting for it to finish.
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
;; For possible values of options, see (sh-options)
;;
(define (sh-start job . options)
  (sh-start* job options))

;; same as sh-start, options must be passed as a single association list.
(define (sh-start* job options)
  (start-any 'sh-start job options)
  (job-id-update! job)) ; sets job-id if started, otherwise unsets it. also returns job status



;; Internal functions called by (sh-start)
(define (start-any caller job options)
  ;b (debugf "start-any ~a ~s" (sh-job->string job) options)
  (options-validate caller options)
  (job-check-not-started caller job)
  (call/cc
    (lambda (k-continue)
      (with-exception-handler
        (lambda (ex)
          (start-any/on-exception caller job options k-continue ex))
        (lambda ()
          (start-any/may-throw caller job k-continue options)))))
  (when (and (job-started? job) (options->spawn? options))
    ; we can cleanup job's file descriptor, as it's running in a subprocess
    (job-unmap-fds! job)
    (job-unredirect/temp/all! job))
  (job-last-status job)) ; returns job status. also checks if job finished


(define (start-any/may-throw caller job k-continue options)
  (call/cc
    (lambda (susp)
      (let ((start-proc (job-start-proc job)))
        (unless (procedure? start-proc)
          (raise-errorf caller "cannot start job ~s, bad or missing job-start-proc: ~s" job start-proc))
        (job-status-set/running! job)
        (job-exception-set! job #f)
        (job-suspend-proc-set! job susp)
        (parameterize ((sh-current-job job))
          ;; set job's parent if requested.
          ;; must be done *before* calling procedures in (cmd-arg-list c)
          (let ((options (options->set-temp-parent! job options)))
            (start-proc job options))))))  ; may throw. ignore value returned by job-start-proc
  (void))


(define (start-any/on-exception caller job options k-continue ex)
  (job-default-parents-iterate job
    (lambda (parent)
      (unless (eq? parent (sh-globals))
        (job-exception-set! parent ex))))
  (job-status-set! caller job (list 'exception ex))
  (if (options->catch? options)
    (k-continue (sh-exception-handler ex))
    (raise ex)))


(define (start-any/display-condition obj port)
  (put-string port "; ")
  (display-condition obj port)
  (newline port)
  (flush-output-port port))



;; flags for (sh-resume), subsumes (sh-bg) (sh-fg) (sh-wait) (sh-job-status)
;;
(define wait-flag-foreground 1)
(define wait-flag-sigcont 2)
(define wait-flag-wait-until-finished 4)
(define wait-flag-wait-until-stopped-or-finished 8)


(define (wait-flag-foreground? wait-flags)
  (debugf "wait-flag-foreground? wait-flags=~s" wait-flags)
  (not (fxzero? (fxand wait-flags wait-flag-foreground))))

(define (wait-flag-sigcont? wait-flags)
  (not (fxzero? (fxand wait-flags wait-flag-sigcont))))

(define (wait-flag-wait? wait-flags)
  (not (fxzero? (fxand wait-flags
                       (fxior wait-flag-wait-until-finished
                              wait-flag-wait-until-stopped-or-finished)))))

(define (wait-flag-wait-until-finished? wait-flags)
  (not (fxzero? (fxand wait-flags wait-flag-wait-until-finished))))

(define (wait-flag-wait-until-stopped-or-finished? wait-flags)
  (not (fxzero? (fxand wait-flags wait-flag-wait-until-stopped-or-finished))))



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
      (resume-job 'sh-job-status 'sh-job-status 0 job)
      status)))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible job statuses, see (sh-job-status)
(define (sh-bg job-or-id)
  (resume-job 'sh-bg 'sh-bg wait-flag-sigcont job-or-id))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (resume-job 'sh-fg 'sh-fg (fxior wait-flag-foreground wait-flag-sigcont wait-flag-wait-until-stopped-or-finished) job-or-id))


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
  (resume-job
    'sh-sigcont+wait
    'sh-sigcont+wait
    (fxior wait-flag-foreground wait-flag-sigcont wait-flag-wait-until-finished)
    job-or-id))


;; General function to resume and optionally wait for a job.
;;
;; Subsumes (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;;
;; mode must be one of: sh-fg sh-bg sh-wait sh-sigcont+wait sh-job-status
(define (sh-resume mode wait-flags job-or-id)
  (resume-job 'sh-resume mode wait-flags job-or-id))


;; Common implementation of (sh-fg) (sh-bg) (sh-resume) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
(define (resume-job caller mode wait-flags job-or-id)
  (let ((job (sh-job job-or-id)))
    ; (debugf ">  sh-resume wait-flags=~s job=~a id=~s pid=~s status=~s" wait-flags (sh-job->string job) (job-id job) (job-pid job) (job-last-status job))
    (case (job-last-status->kind job)
      ((ok exception failed killed)
        (void)) ; job finished
      ((running stopped)
        (cond
          ((job-pid job)
            ; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
            ; in all cases, we have a pid to wait on.
            (advance-pid caller mode wait-flags job))
          ((sh-multijob? job)
            (if (eq? 'sh-pipe (multijob-kind job))
              (advance-multijob-pipe caller mode wait-flags job)
              (advance-multijob      caller mode wait-flags job)))))
      (else
        (raise-errorf mode "job not started yet: ~s" job)))

    (let ((status (job-id-update! job))) ; returns job status
      ;a (debugf "<  sh-resume mode=~s wait-flags=~s job=~a id=~s pid=~s status=~s" mode wait-flags (sh-job->string job) (job-id job) (job-pid job) status)
      status)))


;; Start a job and wait for it to exit or stop.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (start-any 'sh-run/i job options)
  (sh-fg job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (start-any 'sh-run job options)
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
