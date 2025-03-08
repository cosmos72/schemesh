;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


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
        (%make-sh-expr
          #f #f #f #f    ; id oid pid pgid
          (new) #f       ; last-status exception
          (span) 0 #f    ; redirections
          jexpr-start #f ; start-proc step-proc
          #f #f          ; working directory, old working directory - initially inherited from parent job
          #f             ; overridden environment variables - initially none
          #f             ; env var assignments - initially none
          (and current-job (job-parent current-job)) ; temp parent job
          (or current-job (sh-globals))              ; default parent job
          proc                                       ; procedure to call for executing the job
          label
          #f #f)))                                   ; resume-proc suspend-proc
    ((proc)
      (sh-expr proc #f))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (sh-pid-table) nor into global job-id table.
;;
;; Description:
;; Start a sh-expr job.
;;
;; Options are ignored.
;;
;; Returns unspecified value.
(define (jexpr-start job options)
  (assert* 'sh-expr (eq? 'running (job-last-status->kind job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
    ;; jexpr-proc may want to use (sh-fd N)
    (job-remap-fds! job)

    (jexpr-resume-proc-set! job (jexpr-prepare-resume-proc job))

    ;; jobs are started non-blockingly,
    ;; but running a jexpr job always blocks
    ;; => stop the job and let caller decide whether to wait for it
    (job-status-set! 'sh-expr job (stopped 'sigtstp)))))


;; continue a jexpr job
(define (jexpr-advance caller job wait-flags)
  ;; jexpr jobs execute Scheme code, which always blocks:
  ;; continue only if caller asked to continue job and wait.
  ; (debugf "jexpr-advance\tcaller=~s\tjob=~a\twait-flags=~s" caller job wait-flags)
  (when (and (sh-wait-flag-wait? wait-flags)
             (sh-wait-flag-continue-if-stopped? wait-flags))
    (when (jexpr-resume-proc job)
      (jexpr-call-resume-proc job))))


;; call the continuation stored in jexpr-resume-proc of a job for resuming it.
;; save the current continuation in its jexpr-suspend-proc
(define (jexpr-call-resume-proc job)
  (call/cc
    ;; Capture the continuation representing THIS call to (jexpr-call-resume-proc)
    (lambda (susp)
      (let ((resume-proc (jexpr-resume-proc job)))
        (jexpr-resume-proc-set!  job #f)
        (jexpr-suspend-proc-set! job susp)
        ;; (format (debugf-port) "-> jexpr job=~s\tstatus=~s\tcalling resume-proc ~s ...\n" job (job-last-status job) resume-proc)
        (resume-proc (void))
        ;; (format (debugf-port) "<- jexpr job=~s\tstatus=~s\t... resume-proc ~s returned\n" job (job-last-status job) resume-proc)
        )))
  ;; ignore the value returned by (resume-proc) and by continuation (susp)
  (void))


;; prepare and return a closure for running jexpr-proc
(define (jexpr-prepare-resume-proc job)
  (let ((jexpr-initial-resume-proc
    (lambda (unused)
      ;; (debugf "jexpr-prepare-resume-proc job=~a remapping fd1 ~s -> ~s" job (sh-fd 1) (job-find-fd-remap job 1))
      (dynamic-wind
        (lambda ()
          (when (job-stopped? job)
            (job-status-set/running! job)))
        (lambda ()
          (parameterize ((sh-current-job job))
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
                  (exception ex))))))
        (lambda ()
          (when (job-running? job)
            (job-status-set! 'sh-expr job (stopped 'sigtstp))))))))
    jexpr-initial-resume-proc))
