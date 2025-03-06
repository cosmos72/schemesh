;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Convert value returned by (pid-wait) to a symbolic job status.
;;
;; If (pid-wait) succeeds, it returns a pair (pid . wait-status).
;; Passing wait-status to this function converts it to a job status as follows:
;;   not a fixnum, or < 0 => return (list 'failed wait-status)
;;   0                    => return (void)
;;   1..255               => return (list 'failed  wait-status)
;;   256 + kill_signal    => return (list 'killed  signal-name)
;;   512 + stop_signal    => return (list 'stopped signal-name)
;;   768                  => return (list 'running)
;;   > 768                => return (list 'failed  (fx- wait-status 512))
;;
(define (pid-wait-result->status wait-status)
  (let ((x wait-status))
    (cond ((not (fixnum? x)) (list 'failed x))
          ((fx=? x   0) (void))
          ((fx<? x 256) (list 'failed  x))
          ((fx<? x 512) (list 'killed  (signal-number->name (fxand x 255))))
          ((fx<? x 768) (list 'stopped (signal-number->name (fxand x 255))))
          ((fx=? x 768) '(running))
          (else         (list 'failed  (fx- x 512))))))


(define %pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int))


(define (call/foreground-pgid wait-flags new-pgid proc)
  (let* ((new-pgid  new-pgid)
         (our-pgid  (and new-pgid
                         (sh-wait-flag-foreground-pgid? wait-flags)
                         (sh-job-control?)
                         (job-pgid (sh-globals)))))
    (if our-pgid
      ;; cannot use (dynamic-wind) here: some job to be resumed may be an sh-expr
      ;; which is resumed by calling a continuation that exits
      ;; the (dynamic-wind) scope and later re-enters it
      (with-exception-handler
        (lambda (ex)
          ;; try really hard to restore (sh-globals) as the foreground process group
          (%pgid-foreground -1 our-pgid)
          (raise ex))
        (lambda ()
          (%pgid-foreground our-pgid new-pgid)
          (proc)
          ;; try really hard to restore (sh-globals) as the foreground process group
          (%pgid-foreground -1 our-pgid)))
      (proc))))


(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  wait-flags new-pgid body1 body2 ...)
      (call/foreground-pgid wait-flags new-pgid
        (lambda () body1 body2 ...)))))


;; Internal function called by (job-wait)
(define (pid-advance caller job wait-flags)
  ; (debugf "> pid-advance wait-flags=~s job=~a pid=~s status=~s" wait-flags (sh-job->string job) (job-pid job) (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job finished, exit status already available
    ((not (job-started? job))
      (raise-errorf caller "job not started yet: ~s" job))
    (else
      (let ((pid  (job-pid job))
            (pgid (job-pgid job)))
        (with-foreground-pgid wait-flags pgid
          (pid-advance/maybe-sigcont caller job wait-flags pid pgid)
          (pid-advance/maybe-wait    caller job wait-flags pid pgid))))))


;; Internal function called by (pid-advance)
(define (pid-advance/maybe-sigcont caller job wait-flags pid pgid)
  (assert* caller (> pid 0))
  (when pgid
    (assert* caller (> pgid 0)))
  (when (sh-wait-flag-continue-if-stopped? wait-flags)
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "pid-advance/sigcont wait-flags=~s job=~s" job wait-flags)
    (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid)
              'sigcont)
    ;; assume job is now running
    (job-status-set/running! job)))



;; Internal function called by (pid-advance)
(define (pid-advance/maybe-wait caller job wait-flags pid pgid)
  ;; cannot call (sh-job-status), it would recurse back here.
  (let* ((blocking?  (sh-wait-flag-wait? wait-flags))
         (old-status (job-last-status job))
         (new-status (if (sh-finished? old-status)
                       old-status
                       (scheduler-wait job
                         (if blocking? 'blocking 'nonblocking)))))
    ;; (debugf "pid-advance/maybe-wait old-status=~s new-status=~s pid=~s job=~a" old-status new-status (job-pid job) (sh-job->string job))
    ;; (sleep (make-time 'time-duration 0 1))

    ;; if blocking? is #f, new-status may be '(running)
    ;; indicating job status did not change i.e. it's (expected to be) still running
    (case (sh-status->kind new-status)
      ((running)
        ; if new-status is '(running), try to return '(running job-id)
        (let ((new-status2 (job-status-set/running! job)))
           (if blocking?
             ;; if wait-flags tell to wait until job stops or finishes, then wait again for pid
             (pid-advance/maybe-wait caller job wait-flags pid pgid)
             ;; otherwise return job status
             new-status2)))
      ((ok exception failed killed)
        ; job finished, clean it up. Also allows user to later start it again.
        (pid->job-delete! (job-pid job))
        (job-status-set! 'pid-advance/maybe-wait job new-status)
        (job-id-unset! job)
        (job-pid-set!  job #f)
        (job-pgid-set! job #f)
        new-status)
      ((stopped)
        ;; process is stopped.
        ;; if wait-flags tell to wait until job finishes,
        ;;   call (break) then wait for it again (which blocks until it changes status again)
        ;; otherwise propagate process status and return.
        (if (sh-wait-flag-wait-until-finished? wait-flags)
          (begin
            (pid-advance/break              job            pid pgid)
            (pid-advance/maybe-wait  caller job wait-flags pid pgid))
          (begin
            (job-status-set! 'pid-advance/maybe-wait job new-status)
            new-status)))
      (else
        (raise-errorf caller "job not started yet: ~s" job)))))


(define (maybe-queue-job-display-summary job)
  (when (or (job-id job) (job-oid job))
    (queue-job-display-summary job)))


;; Internal function called by (pid-advance/maybe-wait):
;;
;; If may-block is 'nonblocking, call C function wait4(WNOHANG) in a loop,
;; as long as it tells us that *some* subprocess changed status,
;; and update the corresponding job status.
;; Return when wait4(WNOHANG) does not report any subprocess status change.
;;
;; Otherwise, if may-block is 'blocking, call C function wait4() in a loop,
;; waiting for *some* subprocess to change status,
;; and update the corresponding job status.
;; Return when the preferred-pid happens to change status.
;;
;; In all cases, if preferred-job is set, return its updated status.
(define (scheduler-wait preferred-job may-block)
  ;c (debugf ">   scheduler-wait may-block=~s preferred-job=~a" may-block (if preferred-job (sh-job->string preferred-job) preferred-job))
  (let ((current-job   (sh-current-job))
        (done? #f))
    (until done?
      (let ((wait-result (pid-wait -1 may-block)))
        (if (pair? wait-result)
          (let* ((job        (pid->job (car wait-result)))
                 (old-status (if job (job-last-status job) (void)))
                 (new-status (pid-wait-result->status (cdr wait-result))))

            ;; (debugf "... scheduler-wait wait-result=~s new-status=~s job=~a preferred-job=~a current-job=~a" wait-result new-status (if job (sh-job->string job) #f) (if preferred-job (sh-job->string preferred-job) #f) (if current-job (sh-job->string current-job) #f))

            (when job
              (job-status-set! 'scheduler-wait job new-status)

              ;; (debugf "... scheduler-wait old-status new-status=~s job=~a" old-status new-status (sh-job->string job))

              (if (or (eq? job preferred-job) (eq? job current-job))
                ;; the job we are interested in changed status => don't block again
                (when (eq? may-block 'blocking)
                  (set! done? #t))

                ;; advance job that changed status and its parents, before waiting again.
                ;; do NOT advance preferred-job or current-job, because that's what our callers are already doing.
                (let* ((observe-preferred-job?   (and (eq? may-block 'blocking) (job-default-parents-contain? job preferred-job)))
                       (observe-current-job?     (and (eq? may-block 'blocking) (job-default-parents-contain? job current-job)))
                       (preferred-job-old-status (and observe-preferred-job? (job-last-status preferred-job)))
                       (current-job-old-status   (and observe-current-job?   (job-last-status current-job))))

                  (when (status-changed? old-status new-status)
                    (maybe-queue-job-display-summary job))

                  (let ((parent (job-default-parent job)))
                    ;; (sh-job-status) behaves badly on (sh-expr) and their parents: it stops them, so avoid it
                    (unless (or (not parent)
                                (eq? parent preferred-job)
                                (eq? parent current-job)
                                (eq? parent (sh-globals))
                                (sh-expr? job)
                                (sh-expr? parent))
                      (sh-job-status parent))) ; may recursively call scheduler-wait

                  (when observe-preferred-job?
                    (let ((preferred-job-new-status (job-last-status preferred-job)))
                      (when (status-changed? preferred-job-old-status preferred-job-new-status)
                        (set! done? #t))))

                  (when observe-current-job?
                    (let ((current-job-new-status (job-last-status current-job)))
                      (when (status-changed? current-job-old-status current-job-new-status)
                        (set! done? #t))))))))

          (set! done? #t))))) ; (pid-wait) did not report any status change => return
  (let ((ret (if preferred-job (job-last-status preferred-job) (void))))
    ;c (debugf "<   scheduler-wait ret=~s" ret)
    ret))


;; Internal function called by (pid-advance/maybe-wait)
;; when job is stopped and wait-flags tell to wait until job finishes:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (pid-advance/break job pid pgid)
   ; subshells should not directly perform I/O,
   ; they cannot write the "break> " prompt then read commands
   (when (sh-job-control?)
     (let ((break-returned-normally? #f)
           (our-pgid    (job-pgid (sh-globals)))
           (kill-target (if (and pgid (> pgid 0)) (- pgid) pid)))
      (dynamic-wind
        (lambda () ; before body
          ; grab the foreground
          (%pgid-foreground -1 our-pgid))
        (lambda () ; body
          (job-id-set! job)
          (break)
          (set! break-returned-normally? #t))
        (lambda ()
          ; restore job to run in foreground
          (%pgid-foreground our-pgid pgid)
          ; send SIGCONT to job's process group, if present.
          ; otherwise send SIGCONT to job's process id. Both may raise error
          (pid-kill kill-target 'sigcont)
          (unless break-returned-normally?
            (pid-kill kill-target 'sigint)))))))
