;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Start a job and return immediately, without waiting for it to finish.
;;
;; Returns job status, typically (cons 'running job-id) but other values are allowed.
;; For the complete list of possible returned job statuses, see (sh-job-status).
;;
;; Note that job may finish immediately, for example because it is a builtin,
;;   or a multijob that only (recursively) contains builtins,
;;   or a command that exits very quickly.
;;   For these reasons, the returned job status may be different from (cons 'running job-id)
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
          (start-any/may-throw caller job options)))))
  (when (and (job-status-started? job) (options->spawn? options))
    ; we can cleanup job's file descriptor, as it's running in a subprocess
    (job-unmap-fds! job)
    (job-unredirect/temp/all! job))
  (job-last-status job)) ; returns job status. also checks if job finished


(define (start-any/may-throw caller job options)
  (let ((start-proc (job-start-proc job)))
    (unless (procedure? start-proc)
      (raise-errorf caller "cannot start job ~s, bad or missing job-start-proc: ~s" job start-proc))
    (job-status-set/running! job)
    (job-exception-set! job #f)
    ;; set job's parent if requested.
    ;; must be done *before* calling procedures in (cmd-arg-list c)
    (let ((options (options->set-temp-parent! job options)))
      (start-proc job options)))  ; may throw. ignore value returned by job-start-proc
  (void))


(define (start-any/on-exception caller job options k-continue ex)
  (job-default-parents-iterate job
    (lambda (parent)
      (unless (eq? parent (sh-globals))
        (job-exception-set! parent ex))))
  (job-status-set! caller job '(killed . exception))
  (if (options->catch? options)
    (k-continue (sh-exception-handler ex))
    (raise ex)))


(define (start-any/display-condition obj port)
  (put-string port "; ")
  (display-condition obj port)
  (newline port)
  (flush-output-port port))


;; Return up-to-date status of a job or job-id, which can be one of:
;;   (cons 'new     0)
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-or-id)
  (let ((job (sh-job job-or-id)))
    ; (debugf ">  sh-job-status job=~a" (sh-job->string job))
    (if (job-has-status? job '(new))
      (job-last-status job)
      (advance-job 'sh-job-status job))))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status, which can be one of:
;;
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
(define (sh-bg job-or-id)
  (advance-job 'sh-bg job-or-id))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status, which can be one of:
;;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Note: if the current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (advance-job 'sh-fg job-or-id))


;; Continue a job or job-id by optionally sending SIGCONT to it,
;; then wait for it to exit, and finally return its status.
;;
;; Arguments are:
;;   job-or-id           ; a job or job-id
;;   send-sigcont?       ; if truthy, send SIGCONT to job before waiting for it to exit, default is #t
;;
;; Returned job status can be one of:
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'unknown ...)
;;
;; Does NOT return early if job gets stopped, use (sh-fg) for that.
;;
;; Instead if job gets stopped, calls (break).
;; if (break) raises an exception or resets scheme, the job is interrupted with SIGINT.
;; otherwise waits again for the job to exit.
;;
;; Note: if current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define sh-wait
  (case-lambda
    ((job-or-id)               (sh-wait* job-or-id #t))
    ((job-or-id send-sigcont?) (sh-wait* job-or-id send-sigcont?))))


;; Same as (sh-wait), but all arguments are mandatory
(define (sh-wait* job-or-id send-sigcont?)
  (advance-job (if send-sigcont? 'sh-sigcont+wait 'sh-wait) job-or-id))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Also called by (advance-multijob)
;;
;; mode must be one of: sh-fg sh-bg sh-wait sh-sigcont+wait sh-job-status
(define (advance-job mode job-or-id)
  (assert* 'advance-job (memq mode '(sh-fg sh-bg sh-wait sh-sigcont+wait sh-job-status)))
  (let ((job (sh-job job-or-id)))
    ; (debugf ">  advance-job mode=~s job=~a id=~s pid=~s status=~s" mode (sh-job->string job) (job-id job) (job-pid job) (job-last-status job))
    (case (job-last-status->kind job)
      ((exited killed unknown)
        (void)) ; job finished
      ((running stopped)
        (cond
          ((job-pid job)
            ; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
            ; in all cases, we have a pid to wait on.
            (advance-pid mode job))
          ((sh-multijob? job)
            (if (eq? 'sh-pipe (multijob-kind job))
              (advance-multijob-pipe mode job)
              (advance-multijob      mode job)))))
      (else
        (raise-errorf mode  "job not started yet: ~s" job)))

    (let ((status (job-id-update! job))) ; returns job status
      ;a (debugf "<  advance-job mode=~s job=~s id=~s pid=~s status=~s" mode job (job-id job) (job-pid job) status)
      status)))


;; Start a job and wait for it to exit or stop.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (start-any 'sh-run/i job options)
  (sh-fg job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (start-any 'sh-run job options)
  (sh-wait job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #t if job exited successfully, otherwise return #f.
(define (sh-run/ok? job . options)
  (sh-ok? (apply sh-run job options)))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #f if job exited successfully,
;; otherwise return job exit status, which is a cons and hence truish.
(define (sh-run/err? job . options)
  (let ((status (apply sh-run job options)))
    (if (eq? status (void)) #f status)))
