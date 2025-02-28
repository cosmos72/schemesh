;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-wait) (sh-job-status)
;; Resume and optionally wait for a job.
;;
;; Returns updated job status.
(define (job-wait caller job wait-flags)
  (assert* caller (sh-job? job))
  (assert* caller (fixnum? wait-flags))
  (assert* caller (fx<=? 0 wait-flags (sh-wait-flags-all)))

  (let* ((main-pgid (and (sh-wait-flag-foreground-pgid? wait-flags)
                         (sh-job-control?)
                         (job-pgid (sh-globals))))
         (job-pgid  (and main-pgid
                         (job-tree-find-pgid job))))
    ;; using (dynamic-wind) is useless here: (job-wait/impl) calls job's continuations,
    ;; which exit the (dynamic-wind) and later re-enter it.
    (when job-pgid
      (%pgid-foreground main-pgid job-pgid))

    (when (sh-wait-flag-continue-if-stopped? wait-flags)
      (job-tree-continue caller job wait-flags))

    (let ((other-yield-proc (default-yield-proc)))
      (call/cc
        ;; Capture the continuation representing THIS call to (job-wait)
        ;; and save it in (default-yield-proc): needed because (job-wait/impl) calls job-resume-proc,
        ;; which is a continuation and NEVER returns:
        ;; it only yields to parent job or to (default-yield-proc).
        ;;
        ;; since job-resume-proc is a continuation, here (parameterize ((default-yield-proc)))
        ;; and (dynamic-wind) are useless.
        default-yield-proc)

      (job-wait/impl caller job wait-flags)

      ;; restore previous value of (default-yield-proc)
      (default-yield-proc other-yield-proc))

    (when job-pgid
      ;; try really hard to restore (sh-globals) as the foreground process group
      (%pgid-foreground -1 main-pgid)))

  (job-id-update! job)) ; returns job status


;; Find the first running or stopped job in tree that has a pgid and return it.
;; Ignores sh-list children that are running asynchrously.
(define (job-tree-find-pgid job)
  (or
    (if (job-started? job)
      (job-pgid job)
      #f)
    (if (sh-multijob? job)
      (let ((is-sh-list? (eq? sh-list (multijob-kind job))))
        (span-any (multijob-children job)
          (lambda (i child)
            (and (sh-job? child) (child-is-sync? job i is-sh-list?) (job-tree-find-pgid job)))))
      #f)))


(define (child-is-sync? parent child-i parent-is-sh-list?)
  (not (and parent-is-sh-list?
            (eq? '& (sh-multijob-child-ref parent (fx1+ child-i))))))


;; send SIGCONT to job and all its children, and also mark them 'running if they are 'stopped
(define (job-tree-continue caller job wait-flags)
  (let ((pid (job-pid job))
        (pgid (job-pgid job)))
    (when pid
      (assert* caller (integer? pid))
      (assert* caller (> pid 0)))
    (when pgid
      (assert* caller (integer? pgid))
      (assert* caller (> pgid 0)))
    (when (or pid pgid)
      ;; if both pid and pgid are set, prefer pgid
      (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid) 'sigcont)))

  (when (sh-multijob? job)
    (span-iterate (multijob-children job)
      (lambda (i child)
        (when (and (sh-job? child) (job-started? child))
          (job-tree-continue caller child wait-flags)))))

  ;; assume job is now running
  (when (job-stopped? job)
    (job-status-set-running! 'job-tree-continue job)))


;; actual implementation of (job-wait): resume and optionally wait for a job.
;;
;; returns unspecified value.
(define (job-wait/impl caller job wait-flags)
  ;;x (debugf "-> job-wait\tjob=~a\tstatus=~s\tcaller=~s\twait-flags=~s id=~s pid=~s resume-proc=~s" (sh-job->string job) (job-last-status job) caller wait-flags (job-id job) (job-pid job) (job-resume-proc job))
  (case (job-last-status->kind job)
    ((ok exception failed killed)
      (void))

    ((running)
      (scheduler-wait wait-flags)

      (when (job-wait-should-wait-again? job wait-flags)
        ;; caller asked to wait for job to finish (or to stop), cannot return yet: try again
        (job-wait/impl caller job wait-flags))

        ;; ((and (sh-multijob? job) (eq? 'sh-pipe (multijob-kind job)))
        ;;   (mj-pipe-continue caller job wait-flags))
      )
    ((stopped)
      (cond
        ((sh-wait-flag-wait-until-finished? wait-flags)
          ;; caller asked to wait for job to finish, cannot return yet.
          ;; But waiting for a stopped job is not very useful => call break handler
          (job-wait/break job)
          (job-wait/impl caller job wait-flags))

        (else
          (scheduler-wait wait-flags)
          (when (job-wait-should-wait-again? job wait-flags)
            ;; caller asked to wait for job to finish (or to stop), cannot return yet: try again
            (job-wait/impl caller job wait-flags)))))

    (else
      (raise-errorf caller "job not started yet: ~s" job)))
  ;;x (debugf "<- job-wait\tjob=~a\tstatus=~s\tcaller=~s\twait-flags=~s id=~s pid=~s resume-proc=~s" (sh-job->string job) (job-last-status job) caller wait-flags (job-id job) (job-pid job) (job-resume-proc job))
  )


;; check job status, and return #t if (job-wait) should wait again for job to stop of finish.
;; Returns #f if (job-wait) should NOT wait again.
(define (job-wait-should-wait-again? job wait-flags)
  (case (job-last-status->kind job)
    ((running)
      (sh-wait-flag-wait? wait-flags))
    ((stopped)
      (sh-wait-flag-wait-until-finished? wait-flags))
    (else
      #f)))



;; Internal function called by (job-wait)
;; when job is stopped and wait-flags tell to wait until job finishes:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-wait/break job)
   ; subshells should not directly perform I/O,
   ; they cannot write the "break> " prompt then read commands
   (when (sh-job-control?)
     (let* ((break-returned-normally? #f)
            (main-pgid    (job-pgid (sh-globals)))
            (pgid         (job-tree-find-pgid job))
            (kill-target  (or pgid (job-pid job))))
      (dynamic-wind
        (lambda () ; before body
          ; grab the foreground
          (%pgid-foreground -1 main-pgid))
        (lambda () ; body
          (job-id-set! job)
          (break job)
          (set! break-returned-normally? #t))
        (lambda ()
          ; restore job to run in foreground
          (%pgid-foreground main-pgid pgid)
          ; send SIGCONT to job's process group, if present.
          ; otherwise send SIGCONT to job's process id. Both may raise error
          (pid-kill kill-target 'sigcont)
          (unless break-returned-normally?
            (pid-kill kill-target 'sigint)))))))



;; Core function in charge of waiting.
;;
;; Also executed when there's a job to wait for,
;; and no Scheme code to run immediately - not even the REPL prompt.
;;
;; If (sh-wait-flag-wait? wait-flags) is truish, call _once_ the C function wait4(),
;; waiting for *some* subprocess to change status,
;; and update the corresponding job status.
;; Then proceed as if (sh-wait-flag-wait? wait-flags) is #f - see immediately below.
;;
;;
;; If (sh-wait-flag-wait? wait-flags) is #f, call C function wait4(WNOHANG) in a loop,
;; as long as it tells us that *some* subprocess changed status,
;; and update the corresponding job status.
;; Return when wait4(WNOHANG) does not report any subprocess status change.
;;
;; Also, proc-notify-status-change may be truish: in such case it must be a procedure
;; and (proc-notify-status-change job) will be called for each job that changed status.
;;
;; Returns unspecified value.
(define (scheduler-wait wait-flags)
  (assert* 'scheduler-wait (default-yield-proc))

  (when (sh-wait-flag-wait? wait-flags)
    (scheduler-wait-once wait-flags))

  (while (scheduler-wait-once (sh-wait-flags))))


(define (maybe-queue-job-display-summary job)
  (when (or (job-id job) (job-oid job))
    (queue-job-display-summary job)))


;; Call _once_ the C function wait4(), passing flag WNOHANG if requested.
;; If some suprocess changed status, update the corresponding job status and call its (job-resume-proc) if set.
;;
;; Return #t is some job changed its status, otherwise return #f.
(define (scheduler-wait-once wait-flags)

  ;;x (debugf "->  scheduler-wait-once wait-flags=~s" wait-flags)

  (signal-consume-sigchld)

  (let* ((may-block   (if (sh-wait-flag-wait? wait-flags) 'blocking 'nonblocking))
         (wait-result (pid-wait -1 may-block))
         (job         (and (pair? wait-result) (pid->job (car wait-result)))))

    ;;x (debugf "... scheduler-wait-once job=~a\twait-flags=~s wait-result=~s" (and job (sh-job->string job)) wait-flags wait-result)

    (if job
      (let* ((old-status  (job-last-status job))
             (new-status  (pid-wait-result->status (cdr wait-result)))
             (changed?    (status-changed? old-status new-status)))

        (job-status-set! 'scheduler-wait-once job new-status)

        ;;x (debugf "... scheduler-wait-once job=~a\tstatus=~s -> ~s changed?=~s\twait-flags=~s\tresume-proc=~s" (sh-job->string job) old-status new-status changed? wait-flags (job-resume-proc job))

        (when changed?
          (maybe-queue-job-display-summary job)

          (check 'job-pids-wait (job-resume-proc job))

          (when (job-resume-proc job)
            ;;x (debugf "scheduler-wait-once job=~a\tcalling job-resume-proc... " (sh-job->string job))
            (job-call-resume-proc job (sh-wait-flags))
            ;;x (debugf "scheduler-wait-once job=~a\t...job-resume-proc returned" (sh-job->string job))
          ))

        ;;x (debugf "<-  scheduler-wait-once job=~a changed?=~s wait-flags=~s" (sh-job->string job) changed? wait-flags)
        changed?)
      (begin
        ;;x (debugf "<-  scheduler-wait-once job=~a changed?=~s wait-flags=~s" #f #f wait-flags)
        #f))))



;; call the continuation stored in job-resume-proc of a job for resuming it.
(define (job-call-resume-proc job wait-flags)
  (assert* 'job-call-resume-proc (job-resume-proc job))
  (let ((resume-proc (job-resume-proc job)))
    (job-resume-proc-set!  job #f)
    (resume-proc (void)))
  ;; ignore the value returned by (resume-proc)
  (void))
