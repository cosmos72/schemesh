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
;; Returns job status, typically (running job-id) but other values are allowed.
;; For the complete list of possible returned job statuses, see (sh-job-status).
;;
;; Note that job may finish immediately, for example because it is a builtin,
;;   or a multijob that only (recursively) contains builtins,
;;   or a command that exits very quickly.
;;   For these reasons, the returned job status may be different from (running job-id)
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



;; suspend a sh-expr and call its (jexpr-suspend-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (job-suspend)
(define (jexpr-suspend job)
  (let ((suspend-proc (and (sh-expr? job) (jexpr-suspend-proc job))))
    (when suspend-proc
      (call/cc
        ;; Capture the continuation representing THIS call to (job-suspend)
        (lambda (cont)
          ;; store it as job's resume-proc
          (jexpr-resume-proc-set!  job cont)
          (jexpr-suspend-proc-set! job #f)
          (%job-last-status-set! job (stopped 'sigtstp))
          ;; suspend job, i.e. call its suspend-proc
          (suspend-proc (void)))))
    (if suspend-proc #t #f))) ; ignore value returned by continuations (suspend-proc) and (cont)



;; Yield current job: call (scheduler-wait job 'nonblocking) to detect stopped,
;; resumed and finished subprocesses and advance their parents.
;; If some child stopped, call (sh-current-job-suspend) and return its value.
;; Otherwise return #t.
;;
;; Note: if (sh-current-job) is not set, or (sh-job-control?) is #f,
;; does nothing and immediately returns #f
(define (sh-current-job-yield)
  (let ((job (sh-current-job)))
    ;; (debugf "sh-current-job-yield current-job=~s\tjob-control=~s" job (sh-job-control?))
    (if (and job (sh-job-control?))
      (if (stopped? (scheduler-wait #f 'nonblocking))
        (jexpr-suspend job)
        #t)
      #f)))



;; Suspend current job and call its (job-suspend-proc) continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; if job is later resumed, it then returns #t to the caller of (sh-current-job-suspend)
;; if there was no job to suspend, immediately return #f to the caller of (sh-current-job-suspend)
;;
;; Note: if (sh-current-job) is not set, or (sh-job-control?) is #f,
;; does nothing and immediately returns #f
(define (sh-current-job-suspend)
  (and (sh-job-control?) (jexpr-suspend (sh-current-job))))


(meta begin

  ;; helper function used by macros (sh-wait-flag) and (sh-wait-flags)
  (define name->sh-wait-flag
    (let ((alist '((foreground-pgid . 1) (continue-if-stopped . 2)
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


(define (sh-wait-flag-foreground-pgid? wait-flags)
  (not (fxzero? (fxand wait-flags 1))))

(define (sh-wait-flag-continue-if-stopped? wait-flags)
  (not (fxzero? (fxand wait-flags 2))))

(define (sh-wait-flag-wait-until-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 4))))

(define (sh-wait-flag-wait-until-stopped-or-finished? wait-flags)
  (not (fxzero? (fxand wait-flags 8))))

(define (sh-wait-flag-wait? wait-flags)
  (not (fxzero? (fxand wait-flags 12))))

(define (notrace-call arg)
  arg)

(define (%job-wait-once caller job wait-flags)
  ;; (debugf "job-wait-once\tcaller=~s\twait-flags=~s\tjob=~a\tid=~s\tpid=~s\tstatus=~s" caller wait-flags job (job-id job) (job-pid job) (job-last-status job))
  (case (job-last-status->kind job)
    ((ok exception failed killed)
      (void)) ; job finished
    ((running stopped)
      (cond
        ((job-pid job)
          ;; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
          ;; in all cases, we have a pid to wait on.
          (notrace-call (pid-advance caller job wait-flags)))
        ((sh-expr? job)
          (notrace-call (jexpr-advance caller job wait-flags)))
        ((sh-multijob-pipe? job)
          (notrace-call (mj-pipe-advance caller job wait-flags)))
        ((sh-multijob? job)
          (notrace-call (mj-advance      caller job wait-flags)))))
    (else
      (raise-errorf caller "job not started yet: ~s" job))))


(define (job-wait-once caller job wait-flags)
  (notrace-call (%job-wait-once caller job wait-flags)))


;; Internal function called by (job-wait) when job is stopped
;; and caller asked to wait until job finishes:
;;
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-break job)
   ;; subshells should not directly perform I/O,
   ;; they cannot write the "break> " prompt then read commands
   (when (sh-job-control?)
     (let* ((break-returned-normally? #f)
            (pgid       (job-pgid job))
            (target-pid (if (and pgid (> pgid 0)) (- pgid) (job-pid  job))))
      (dynamic-wind
        void
        (lambda () ; body
          (job-id-set! job)
          (break)
          (set! break-returned-normally? #t))
        (lambda ()
          ; send SIGCONT to job's process group, if present.
          ; otherwise send SIGCONT to job's process id.
          (when target-pid
            (pid-kill target-pid 'sigcont)
            (unless break-returned-normally?
              (pid-kill target-pid 'sigint))))))))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
;; Returns updated job status.
(define (job-wait caller job-or-id wait-flags)
  (let ((job (sh-job job-or-id)))
    (let %loop ()
      (job-wait-once caller job wait-flags)
      (case (job-last-status->kind job)
        ((running)
          (when (sh-wait-flag-wait? wait-flags)
            (%loop)))
        ((stopped)
          (when (sh-wait-flag-wait-until-finished? wait-flags)
            ;x (debugf "...job-wait\tcaller=~s\tjob=~a\tcurrent-job=~s\tcalling sh-current-job-suspend..." caller job (sh-current-job))
            (or (sh-current-job-suspend)
                (job-break job))
            ;x (debugf "...job-wait\tcaller=~s\tjob=~a\tcurrent-job=~s ... sh-current-job-suspend returned" caller job (sh-current-job))
            (%loop)))))
    (job-id-update! job))) ; returns job status


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
  (let* ((job    (sh-job job-or-id))
         (status (job-last-status job)))
    ; (debugf ">  sh-job-status job=~a" job)
    (if (started? status)
      (job-wait 'sh-job-status job (sh-wait-flags))
      status)))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible job statuses, see (sh-job-status)
(define (sh-bg job-or-id)
  (job-wait 'sh-bg job-or-id (sh-wait-flags continue-if-stopped)))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible job statuses, see (sh-job-status)
;;
;; Note: if job control is enabled,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (job-wait
    'sh-fg
    job-or-id
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
      (job-wait 'sh-wait job-or-id
        (sh-wait-flags foreground-pgid continue-if-stopped wait-until-finished)))
    ((job-or-id wait-flags)
      (job-wait 'sh-wait job-or-id wait-flags))))


;; Start a job and wait for it to exit or stop.
;;
;; For the possible options, see (sh-options)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (if (started? (job-start 'sh-run/i job options))
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
  (ok? (apply sh-run job options)))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; For the possible options, see (sh-options)
;;
;; Return #f if job exited successfully,
;; otherwise return job exit status, which is a cons and hence truish.
(define (sh-run/err? job . options)
  (let ((status (apply sh-run job options)))
    (if (ok? status) #f status)))
