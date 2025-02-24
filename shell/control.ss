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



;; Internal functions called by (sh-start) to actually start a job.
;; Returns job status.
(define (job-start caller job options)
;;e (debugf "job-start caller=~s job=~a options=~s status=~s" caller (sh-job->string job) options (job-last-status job))
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
  (job-last-status job)) ; returns job status.


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
  (job-yield-proc-set! job #f)
  (when (sh-multijob? job)
    (multijob-current-child-index-set! job -1)
    (span-iterate (multijob-children job)
      (lambda (i elem)
        (when (sh-job? elem)
          (job-status-set-new/recursive! elem))))))


(define (job-start/may-throw caller job k-continue options)
  (let ((start-proc (job-start-proc job)))
     (unless (procedure? start-proc)
        (raise-errorf caller "~s is not a valid start-proc for job ~s" start-proc job))
     (job-oid-set! job #f)
     (job-exception-set! job #f)
     (unless (job-new? job)
       (job-status-set-new/recursive! job))
     (job-status-set-running! 'job-start/may-throw job)
     (check-not caller (job-yield-proc job))
     (job-yield-proc-set! job k-continue)
     (parameterize ((sh-current-job job))
       ;; set job's parent if requested.
       ;; must be done *before* calling procedures in (cmd-arg-list c)
       (let ((options (options->set-temp-parent! job options)))
         (start-proc job options))))  ; may throw
  ;; ignore value returned by job-start-proc,
;; and ignore value returned by continuation (yield)
  ;;
  ;; the yield continuation we set above is no longer useful
  (job-yield-proc-set! job #f)
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



;; suspend a job and call its (job-yield-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (job-suspend)
(define (job-suspend job status)
  (let ((yield-proc (and (sh-job? job) (job-yield-proc job))))
    ;;x (debugf "-> job-suspend\tjob=~a\tstatus=~s\tyield-proc=~s" (sh-job->string job) (job-last-status job) yield-proc)
    (when yield-proc
      ;; (debugf "job-suspend job=~a" (sh-job->string job))
      (call/cc
        ;; Capture the continuation representing THIS call to (job-suspend)
        (lambda (cont)
          ;; store it as job's resume-proc
          (check-not 'job-suspend (job-resume-proc job))
          (job-resume-proc-set! job cont)
          (job-yield-proc-set!  job #f)
          (unless (job-pid job)
            (%job-last-status-set! job (if (sh-stopped? status) status '(stopped sigtstp))))
          ;; suspend job, i.e. call its yield-proc
          (yield-proc (void)))))
    ;;x (debugf "<- job-suspend\tjob=~a\tstatus=~s" (sh-job->string job) (job-last-status job))
    ;; ignore value returned by continuations (yield-proc) and (cont)
    ;;
    ;; also, the resume continuation we set above is no longer useful
    (job-resume-proc-set! job #f)
    (if yield-proc #t #f)))


;; suspend a running job and call its (job-yield-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (job-suspend)
;; if there was no job to yield, immediately return #f to the caller of (job-yield)
(define (job-yield job caller)
  (let ((yield-proc (and (sh-job? job) (job-yield-proc job))))
    ;;x (debugf "-> job-yield\tjob=~a\tstatus=~s\tcaller=~s yield-proc=~s" (sh-job->string job) (job-last-status job) caller yield-proc)
    (when yield-proc
      (call/cc
        ;; Capture the continuation representing THIS call to (job-yield)
        (lambda (cont)
          ;; store it as job's resume-proc
          (check-not 'job-yield (job-resume-proc job))
          (job-resume-proc-set! job cont)
          (job-yield-proc-set! job #f)
          (when (job-running? job)
            ;; try to update status '(running) -> '(running id)
            (job-status-set-running! 'job-yield job))
          ;; suspend job, i.e. call its yield-proc
          (yield-proc (void)))))
    ;;x (debugf "<- job-yield\tjob=~a\tstatus=~s\tcaller=~s" (sh-job->string job) (job-last-status job) caller)
    ;; ignore value returned by continuations (yield-proc) and (cont)
    ;;
    ;; also, the resume continuation we set above is no longer useful
    (job-resume-proc-set! job #f)

    (if yield-proc #t #f)))


;; Suspend current job and call its (job-yield-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (sh-current-job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (sh-current-job-suspend)
;;
;; Note: has effect only if (sh-current-job) is set
(define (sh-current-job-suspend)
  (job-suspend (sh-current-job) '(stopped sigtstp)))


;; Yield current job and call its (job-yield-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (sh-current-job-yield)
;; if there was no job to yield, immediately return #f to the caller of (sh-current-job-yield)
;;
;; Note: has effect only if (sh-current-job) is set
(define (sh-current-job-yield)
  (job-yield (sh-current-job) 'sh-current-job-yield))


(meta begin

  ;; helper function used by macros (sh-wait-flag) and (sh-wait-flags)
  (define name->sh-wait-flag
    (let ((alist '((foreground-pgid . 1) (resume-if-stopped . 2)
                   (wait-until-finished . 4) (wait-until-stopped-or-finished . 8))))
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
;;   resume-if-stopped
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


(define-syntax sh-wait-flags-all
  (syntax-rules ()
    ((_) 15)))


(define (sh-wait-flag-foreground-pgid? wait-flags)
  (not (fxzero? (fxand wait-flags 1))))

(define (sh-wait-flag-resume-if-stopped? wait-flags)
  (not (fxzero? (fxand wait-flags 2))))

(define (sh-wait-flag-wait-until-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 4))))

(define (sh-wait-flag-wait-until-stopped-or-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 8))))

(define (sh-wait-flag-wait? wait-flags)
  (not (fxzero? (fxand wait-flags 12))))




;; General function to resume and wait for a job.
;; Subsumes (sh-fg) (sh-bg) (sh-job-status)
;;
;; Argument wait-flags must be a sh-wait-flags enum set.
;;
;; Returns updated job status.

;; if wait-flags contain 'resume-if-stopped then sends SIGCONT to job
;; if wait-flags contains 'wait-until-finished-or-stopped then waits until job finishes or gets stopped
;; Otherwise returns either immediately (if wait-flags do not contain any 'wait-flag-wait*)
;; or waits until job finishes (if wait-flags contain 'wait-until-finished).
;;
;; In the latter case, calls (break) if job gets stopped.
(define sh-wait
  (case-lambda
    ((job-or-id)
      (job-resume 'sh-wait job-or-id
       (sh-wait-flags foreground-pgid resume-if-stopped wait-until-finished)))
    ((job-or-id wait-flags)
      (job-resume 'sh-wait job-or-id wait-flags))))
     



;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
;; Returns updated job status.
(define (job-resume caller job-or-id wait-flags)
  (assert* caller (fixnum? wait-flags))
  (assert* caller (fx<=? 0 wait-flags (sh-wait-flags-all)))
  (let ((job (sh-job job-or-id)))
    (%job-resume caller job wait-flags)
    (job-id-update! job))) ; returns job status


;; actual implementation of (job-resume)
;; returns unspecified value.
(define (%job-resume caller job wait-flags)
  (let* ((status (job-last-status job))
         (kind   (sh-status->kind status)))
    ;;x (debugf "-> job-resume\tjob=~a\tstatus=~s\tcaller=~s\twait-flags=~s id=~s pid=~s resume-proc=~s" (sh-job->string job) (job-last-status job) caller wait-flags (job-id job) (job-pid job) (job-resume-proc job))
    (case kind
      ((ok exception failed killed)
        (void))

      ((running stopped)
        (cond
          ((job-pid job)
            ;; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
            ;; in all cases, we have a pid to wait on.
            ;;
            ;; Note: don't call job-resume-proc, because (pid-resume) already calls it when needed
            (pid-resume caller job wait-flags)

            (when (%job-resume-should-wait-again? job wait-flags)
              ;; caller asked to wait for job to finish (or to stop), cannot return yet: try again
              (%job-resume caller job wait-flags)))

          ((job-resume-proc job)
            ;; we have a continuation to call for resuming the job
            (when (or (eq? 'running kind)
                      (sh-wait-flag-wait? wait-flags)
                      (sh-wait-flag-resume-if-stopped? wait-flags))

              (job-call-resume-proc job wait-flags)

              (when (%job-resume-should-wait-again? job wait-flags)
                ;; caller asked to wait for job to finish (or to stop), cannot return yet: try again
                (%job-resume caller job wait-flags))))

          ((and (sh-multijob? job) (eq? 'sh-pipe (multijob-kind job)))
            (mj-pipe-continue caller job wait-flags))

          ; else job has no pid and no continuation, cannot resume it from here
          ))
      (else
        (raise-errorf caller "job not started yet: ~s" job))))
  ;;x (debugf "<- job-resume\tjob=~a\tstatus=~s\tcaller=~s\twait-flags=~s id=~s pid=~s resume-proc=~s" (sh-job->string job) (job-last-status job) caller wait-flags (job-id job) (job-pid job) (job-resume-proc job))
  )


;; check job status, and return #t if (%job-resume) should wait again for job to stop of finish.
;; Returns #f if (%job-resume) should NOT wait again.
(define (%job-resume-should-wait-again? job wait-flags)
  (let ((kind (job-last-status->kind job)))
    (or (and (eq? 'running kind) (sh-wait-flag-wait?                wait-flags))
        (and (eq? 'stopped kind) (sh-wait-flag-wait-until-finished? wait-flags)))))


;; call the continuation stored in job-resume-proc of a job for resuming it.
;; save the current continuation in its job-yield-proc
(define (job-call-resume-proc job wait-flags)
  (assert* 'job-call-resume-proc (job-resume-proc job))
  (call/cc
    ;; Capture the continuation representing THIS call to (job-call-resume-proc)
    (lambda (yield)
      (let ((resume-proc (job-resume-proc job))
            (pgid (job-pgid job)))
        (job-resume-flags-set! job wait-flags)
        (job-resume-proc-set!  job #f)
        (check-not 'job-call-resume-proc (job-yield-proc job))
        (job-yield-proc-set!   job yield)
        (when (or (job-running? job)
                  (and (job-stopped? job)
                       (not (job-pid job))
                       (sh-wait-flag-resume-if-stopped? wait-flags)))
          ;; try to update status '(running) -> '(running id)
          (job-status-set-running! 'job-call-resume-proc job))
        (with-foreground-pgid wait-flags pgid
          ;; send SIGCONT to job's process group, if present.
          (when (and pgid (sh-wait-flag-resume-if-stopped? wait-flags))
            (pid-kill (- pgid) 'sigcont))
          (resume-proc (void))))))
  ;; ignore the value returned by (resume-proc) and by continuation (yield)
  ;;
  ;; also, the yield continuation we set above is no longer useful
  (job-yield-proc-set! job #f)
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
    ; (debugf "->  sh-job-status job=~a" (sh-job->string job))
    (if (sh-started? status)
      (job-resume 'sh-job-status job (sh-wait-flags))
      status)))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible job statuses, see (sh-job-status)
(define (sh-bg job-or-id)
   (job-resume 'sh-bg job-or-id (sh-wait-flags resume-if-stopped)))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (job-resume 'sh-fg job-or-id
    (sh-wait-flags foreground-pgid resume-if-stopped wait-until-stopped-or-finished)))


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
