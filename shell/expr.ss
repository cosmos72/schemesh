;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; Create a sh-job subclass that will run Scheme procedure proc
;; when the job is executed,
;; and return its value wrapped in a status.
;;
;; Procedure proc must accept zero or one arguments - the job itself.
(define sh-expr
  (case-lambda
    ((proc label)
      (assert* 'sh-expr (procedure? proc))
      (unless (logbit? 0 (procedure-arity-mask proc))
        (assert* 'sh-expr (logbit? 1 (procedure-arity-mask proc))))
      (when label
        (assert* 'sh-expr (string? label)))
      (let ((current-job (sh-current-job)))
        (%make-jexpr
          #f #f #f #f #f ; id oid pid pgid pgid-fg
          (new) #f       ; last-status exception
          (span) 0 #f #f ; redirections ports
          jexpr-start #f ; start-proc step-proc
          #f #f          ; working directory, old working directory - initially inherited from parent job
          #f             ; overridden environment variables - initially none
          #f             ; env var assignments - initially none
          (and current-job (job-parent current-job)) ; temp parent job
          (or current-job (sh-globals))              ; default parent job
          #f #f '()                                  ; resume-proc, suspend-proc, on-finish thunk list
          proc                                       ; procedure to call for executing the job
          label)))
    ((proc)
      (sh-expr proc #f))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Start a sh-expr job in a subprocess.
;;
;; Returns unspecified value.
(define (jexpr-start job options)
  (assert* 'sh-expr (eq? 'running (job-last-status->kind job)))
  ;; always start the sh-expr in a subprocess, because it may launch jobs:
  ;;
  ;; if the sh-expr is executed directly, launches some job(s),
  ;; then the sh-expr is suspended and resumed in background,
  ;; it gets moved to a new subprocess:
  ;; it loses the parent/child relationship with the jobs it already launched
  ;; and can no longer wait for them and receive their exit status.
  ;;
  ;; Solution: always execute the sh-expr in a subprocess. Slow but correct.
  ;;
  ;; An alternative solution - currently *not* implemented - is to move the sh-expr
  ;; to a new subprocess immediately *before* it launches its first job.
  ;; That's faster, but it has less clear semantics:
  ;; global side effects caused by the sh-expr, such as changing a parameter,
  ;; should propagate to the main shell or not?
  ;;
  (if #t ; was: (if (options->spawn? options)
    ;; execute the sh-expr in a subprocess
    (spawn-job-procedure job options jexpr-call-proc)
    ;; execute the sh-expr directly
    #;   ; commented out
    (begin
      (call/cc
        ;; Capture the continuation representing THIS call to (jexpr-start)
        (lambda (susp)
          (job-resume-proc-set!  job #f)
          (job-suspend-proc-set! job susp)
          (jexpr-call-proc job options)))
      (let ((status (job-last-status job)))
        ;c (debugf "jexpr-start ~s, pid ~s, status ~s" job (job-pid job) status)
        (cond
          ((or (job-pid job) (not (running? status)))
            status)
          ((job-resume-proc job)
            (job-status-set! 'jexpr-start job (stopped 'sigtstp)))
          (else
            status))))))


;; call jexpr-proc and store its results into job-status
;; returns job status
(define (jexpr-call-proc job options)
  ;; jexpr-proc may want to use (sh-fd N)
  (job-remap-fds! job)
  (job-env/apply-lazy! job 'export)

  (job-status-set! 'sh-expr job
    (try
      (call-with-values
        (lambda ()
          (let ((proc (jexpr-proc job)))
            (if (logbit? 1 (procedure-arity-mask proc))
              (proc job)
              (proc))))
        ok)
      (catch (ex)
        (debug-condition ex) ;; save obj into thread-parameter (debug-condition)
        (exception ex)))))


;; React to a SIGCHLD: if job is an sh-expr,
;; check whether some other job stopped (TBD: or was killed?)
;; and in such case suspend job.
;;
;; Return #t if job is a sh-expr, otherwise return #f.
;; May also not return i.e. non-locally jump to job's suspend-proc.
(define (jexpr-sigchld job)
  (if (sh-expr? job)
    (let* ((parent (job-default-parent job))
           (siblings-old-status (multijob-children-last-status parent)))

      ;; Note: calling (scheduler-wait) may not return:
      ;; when it detects that some job stopped, it advances the job's parents
      ;; which may suspend this sh-expr too.
      (scheduler-wait #f 'nonblocking)

      (let ((siblings-new-status (multijob-children-last-status parent))
            (some-sibling-stopped-status #f)) ; #f is not a job status
        (for-vector ((old siblings-old-status)
                     (new siblings-new-status))
          (when (and (stopped? new) (not (stopped? old)))
            (set! some-sibling-stopped-status new)))

        ;; (debugf "jexpr-sigchld job=~s\tsiblings-old-status=~s\tsiblings-new-status=~s" (sh-job->string job) siblings-old-status siblings-new-status)
        (when some-sibling-stopped-status
          (job-call-suspend-proc job (status->value some-sibling-stopped-status)))
        #t))
    #f))


;; continue a job via its resume-proc.
;; returns unspecified value
(define (proc-advance caller job wait-flags)
  ;; shell builtins and sh-expr jobs execute arbitrary Scheme code, which usually blocks:
  ;; if caller asked to resume job in background, we must spawn a subprocess
  ;; and resume the shell builtin or sh-expr there
  ; (debugf "proc-advance\tcaller=~s\tjob=~a\twait-flags=~s" caller job wait-flags)
  (let ((resume-proc (job-resume-proc job)))
    (when (and resume-proc
               (or (sh-wait-flag-continue-if-stopped? wait-flags)
                   (job-running? job)))
      (cond
        ((sh-wait-flag-background? wait-flags)
          ;; TRICKY: spawn a subprocess, and resume the stopped shell builtin or sh-expr in it
          ;; (debugf "> proc-advance ~s background" job)
          (let ((status (spawn-job-procedure job '()
                          (lambda (job options)
                            ;; executed in subprocess
                            (job-resume-proc-set! job #f)
                            (set! job-start-exit-from-spawned-subprocess? #t)
                            (resume-proc (void))))))
            ;; (debugf "< proc-advance ~s background, status ~s" job status)
            (job-resume-proc-set! job #f)
            (when (running? status)
              ; (job-id-update! job) ; verbose
              ; we can cleanup job's file descriptor, as it's running in a subprocess
              (job-unmap-fds! job)
              (job-unredirect/temp/all! job))
            (job-status-set! 'proc-advance job status)))

        ((sh-wait-flag-wait? wait-flags)
          ;; directly resume the stopped shell builtin or sh-expr
          (job-call-resume-proc job))))))



;; Call the continuation stored in job-resume-proc of a job for resuming it.
;; save the current continuation in its job-suspend-proc
(define (job-call-resume-proc job)
  (call/cc
    ;; Capture the continuation representing THIS call to (job-call-resume-proc)
    (lambda (susp)
      (let ((resume-proc (job-resume-proc job)))
        (job-resume-proc-set!  job #f)
        (job-suspend-proc-set! job susp)
        ;; (format (debugf-port) "-> jexpr job=~s\tstatus=~s\tcalling resume-proc ~s ...\n" job (job-last-status job) resume-proc)
        (when (job-stopped? job)
          (job-status-set/running! job))
        (resume-proc (void))
        ;; (format (debugf-port) "<- jexpr job=~s\tstatus=~s\t... resume-proc ~s returned\n" job (job-last-status job) resume-proc)
        )))
  ;; ignore the value returned by (resume-proc) and by continuation (susp)
  (void))


;; Try to suspend a sh-job by calling its suspend-proc continuation,
;; which non-locally jumps to whoever started or resumed the job.
;;
;; If job is later resumed, it eventually returns #t to the caller of (job-call-suspend-proc)
;; If job's suspend-proc is not set, immediately return #f.
(define (job-call-suspend-proc job signal-name)
  (let ((suspend-proc (and job (job-suspend-proc job))))
    ;; (debugf "job-call-suspend-proc job=~s suspend-proc=~s" job suspend-proc)
    (when suspend-proc
      (call/cc
        ;; Capture the continuation representing THIS call to (job-suspend)
        (lambda (cont)
          ;; store it as job's resume-proc
          (job-resume-proc-set!  job cont)
          (job-suspend-proc-set! job #f)
          (%job-last-status-set! job (stopped signal-name))
          ; (job-id-update! job) ; verbose
          ;; suspend job, i.e. call its suspend-proc
          (suspend-proc (void)))))
    ;;y (debugf "job-call-suspend-proc job ~s, status ~s, resume-proc ~s, suspend-proc ~s" job (job-last-status job) (job-resume-proc job) (job-suspend-proc job))
    (if suspend-proc #t #f))) ; ignore value returned by continuation (suspend-proc)
