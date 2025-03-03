;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Create a sh-job subclass that will run Scheme procedure proc
;; when the job is executed,
;; and return its value wrapped in a job-status.
;;
;; Procedure proc must accept zero or one arguments - the job itself.
(define (sh-expr proc)
  (assert* 'sh-expr (procedure? proc))
  (unless (logbit? 0 (procedure-arity-mask proc))
    (assert* 'sh-expr (logbit? 1 (procedure-arity-mask proc))))
  (let ((current-job (sh-current-job)))
    (%make-sh-expr
      #f #f #f #f    ; id oid pid pgid
      '(new) #f      ; last-status exception
      (span) 0 #f    ; redirections
      jexpr-start #f ; start-proc step-proc
      #f #f          ; working directory, old working directory - initially inherited from parent job
      #f             ; overridden environment variables - initially none
      #f             ; env var assignments - initially none
      (and current-job (job-parent current-job)) ; temp parent job
      (or current-job (sh-globals))              ; default parent job
      proc                                       ; procedure to call for executing the job
      #f #f)))                                   ; resume-proc suspend-proc



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
  ;; jexpr-proc may want to use (sh-fd-stdin) (sh-fd-stdout) (sh-fd-stderr)
  ;; or more generally, (job-find-fd-remap)
  (job-remap-fds! job))


;; continue a jexpr job
(define (jexpr-advance job wait-flags)
  ;; jexpr jobs execute Scheme code, which always blocks:
  ;; continue only if caller asked to wait.
  (when (sh-wait-flag-wait? wait-flags)
    (let ((pgid (job-pgid job)))
      (with-foreground-pgid wait-flags pgid
        ;; send SIGCONT to job's process group, if present.
        (when (and pgid (sh-wait-flag-continue-if-stopped? wait-flags))
          (pid-kill (- pgid) 'sigcont))
        (job-status-set/running! job)
        (unless (jexpr-resume-proc job)
          (jexpr-resume-proc-set! job (jexpr-resume-proc-prepare job)))
        (jexpr-call-resume-proc job)))))


;; call the continuation stored in jexpr-resume-proc of a job for resuming it.
;; save the current continuation in its jexpr-suspend-proc
(define (jexpr-call-resume-proc job)
  (call/cc
    ;; Capture the continuation representing THIS call to (jexpr-call-resume-proc)
    (lambda (susp)
      (let ((resume-proc (jexpr-resume-proc job)))
        (jexpr-resume-proc-set!  job #f)
        (jexpr-suspend-proc-set! job susp)
        (resume-proc (void)))))
  ;; ignore the value returned by (resume-proc) and by continuation (susp)
  (void))


;; prepare and return a closure for running jexpr-proc
(define (jexpr-resume-proc-prepare job)
  (lambda (unused)
    (parameterize ((sh-current-job job)
                   (sh-fd-stdin  (job-find-fd-remap job 0))
                   (sh-fd-stdout (job-find-fd-remap job 1))
                   (sh-fd-stderr (job-find-fd-remap job 2)))
        (job-status-set! 'sh-expr job
          (try
            (call-with-values
              (lambda ()
                (let ((proc (jexpr-proc job)))
                  (if (logbit? 1 (procedure-arity-mask proc))
                    (proc job)
                    (proc))))
              values->job-status)
            (catch (ex)
              (list 'exception ex)))))))


;; convert arbitrary Scheme values returned by jexpr-proc
;; to job status
(define (values->job-status . rets)
  (cond
    ((null? rets)
      (void))
    ((null? (cdr rets))
      (let ((ret (car rets)))
        (cond ((eq? (void) ret) (void))
              ((not ret)        '(failed #f))
              (else             (cons 'ok rets)))))
    (else
      (cons 'ok rets))))
