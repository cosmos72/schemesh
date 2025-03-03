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


(define %pgid-foreground
  (let ((c-pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int)))
    (lambda (old-pgid new-pgid)
      (let ((err (c-pgid-foreground old-pgid new-pgid)))
        ; (c-pgid-foreground) may fail if new-pgid failed in the meantime
        ; (when (< err 0)
        ;  (raise-c-errno caller 'tcsetpgrp err new-pgid))
        err))))


(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  wait-flags new-pgid body ...)
      ;; hygienic macros sure are handy :)
      (let* ((new-pgid  new-pgid)
             (our-pgid  (and new-pgid
                          (jr-flag-foreground? wait-flags)
                          (sh-job-control?)
                          (job-pgid (sh-globals)))))
        (dynamic-wind
          (lambda () ; run before body
            (when our-pgid
              (%pgid-foreground our-pgid new-pgid)))
          (lambda ()
            body ...)
          (lambda () ; run after body
            ;; try really hard to restore (sh-globals) as the foreground process group
            (when our-pgid
              (%pgid-foreground -1 our-pgid))))))))


;; Internal function called by (sh-resume)
(define (advance-pid caller job wait-flags)
  ; (debugf "> advance-pid wait-flags=~s job=~a pid=~s status=~s" wait-flags (sh-job->string job) (job-pid job) (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job finished, exit status already available
    ((not (job-started? job))
      (raise-errorf caller "job not started yet: ~s" job))
    (else
      (let ((pid  (job-pid job))
            (pgid (job-pgid job)))
        (with-foreground-pgid wait-flags pgid
          (advance-pid/maybe-sigcont caller job wait-flags pid pgid)
          (advance-pid/maybe-wait    caller job wait-flags pid pgid))))))


;; Internal function called by (advance-pid)
(define (advance-pid/maybe-sigcont caller job wait-flags pid pgid)
  (assert* caller (> pid 0))
  (when pgid
    (assert* caller (> pgid 0)))
  (when (jr-flag-sigcont? wait-flags)
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "advance-pid/sigcont wait-flags=~s job=~s" job wait-flags)
    (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid) 'sigcont)
    ;; assume job is now running
    (job-status-set/running! job)))



;; Internal function called by (advance-pid)
(define (advance-pid/maybe-wait caller job wait-flags pid pgid)
  ;; cannot call (sh-job-status), it would recurse back here.
  (let* ((blocking?  (jr-flag-wait? wait-flags))
         (old-status (job-last-status job))
         (new-status (if (sh-finished? old-status)
                       old-status
                       (job-pids-wait job
                         (if blocking? 'blocking 'nonblocking)))))
    ;; (debugf "advance-pid/maybe-wait old-status=~s new-status=~s pid=~s job=~a" old-status new-status (job-pid job) (sh-job->string job))
    ;; (sleep (make-time 'time-duration 0 1))

    ;; if blocking? is #f, new-status may be '(running)
    ;; indicating job status did not change i.e. it's (expected to be) still running
    (case (sh-status->kind new-status)
      ((running)
        ; if new-status is '(running), try to return '(running job-id)
        (let ((new-status2 (job-status-set/running! job)))
           (if blocking?
             ;; if wait-flags tell to wait until job stops or finishes, then wait again for pid
             (advance-pid/maybe-wait caller job wait-flags pid pgid)
             ;; otherwise return job status
             new-status2)))
      ((ok exception failed killed)
        ; job finished, clean it up. Also allows user to later start it again.
        (pid->job-delete! (job-pid job))
        (job-status-set! 'advance-pid/maybe-wait job new-status)
        (job-id-unset! job)
        (job-pid-set!  job #f)
        (job-pgid-set! job #f)
        new-status)
      ((stopped)
        ; process is stopped.
        ; if wait-flags tell to wait until job finishes,
        ;   call (break) then wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (jr-flag-wait-until-finished? wait-flags)
          (begin
            (advance-pid/break              job            pid pgid)
            (advance-pid/maybe-wait  caller job wait-flags pid pgid))
          (begin
            (job-status-set! 'advance-pid/maybe-wait job new-status)
            new-status)))
      (else
        (raise-errorf caller "job not started yet: ~s" job)))))


(define (maybe-queue-job-display-summary job)
  (when (or (job-id job) (job-oid job))
    (queue-job-display-summary job)))


;; Internal function called by (advance-pid/maybe-wait):
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
(define (job-pids-wait preferred-job may-block)
  ;c (debugf ">   job-pids-wait may-block=~s preferred-job=~a" may-block (if preferred-job (sh-job->string preferred-job) preferred-job))
  (let ((done? #f))
    (until done?
      (let ((wait-result (pid-wait -1 may-block)))
        (if (pair? wait-result)
          (let* ((job        (pid->job (car wait-result)))
                 (old-status (if job (job-last-status job) (void)))
                 (new-status (pid-wait-result->status (cdr wait-result))))
            ;; (debugf "... job-pids-wait wait-result=~s new-status=~s job=~a preferred-job=~a" wait-result new-status (if job (sh-job->string job) #f) (if preferred-job (sh-job->string preferred-job) #f))
            (when job
              (job-status-set! 'job-pids-wait job new-status)

              ;; (debugf "... job-pids-wait old-status new-status=~s job=~a" old-status new-status (sh-job->string job))

              (if (eq? job preferred-job)
                ;; the job we are interested in changed status => don't block again
                (when (eq? may-block 'blocking)
                  (set! done? #t))

                ;; advance job that changed status and *all* it parents, before waiting again.
                ;; do NOT advance preferred-job, because that's what our callers are already doing.
                (let ((globals (sh-globals)))
                  (when (status-changed? old-status new-status)
                    (maybe-queue-job-display-summary job))

                  (job-default-parents-iterate (job-parent job)
                    (lambda (parent)
                      (if (eq? parent globals)
                        #f
                        (let ((old-status (job-last-status parent))
                              (new-status (sh-job-status parent)))
                          ; (debugf "... job-pids-wait old-status=~s new-status=~s parent=~a" old-status new-status (sh-job->string parent))
                          (when (status-changed? old-status new-status)
                            (maybe-queue-job-display-summary job))))))))))

          (set! done? #t))))) ; (pid-wait) did not report any status change => return
  (let ((ret (if preferred-job (job-last-status preferred-job) (void))))
    ;c (debugf "<   job-pids-wait ret=~s" ret)
    ret))


;; Internal function called by (advance-pid/maybe-wait)
;; when job is stopped and wait-flags tell to wait until job finishes:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (advance-pid/break job pid pgid)
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
