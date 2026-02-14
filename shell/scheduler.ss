;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; Convert value returned by (pid-wait) to a symbolic job status.
;;
;; If (pid-wait) succeeds, it returns a pair (pid . wait-status).
;; Passing wait-status to this function converts it to a job status as follows:
;;   not a fixnum, or < 0 => return (failed wait-status)
;;   0                    => return (void)
;;   1..255               => return (failed  wait-status)
;;   256 + kill_signal    => return (killed  signal-name)
;;   512 + stop_signal    => return (stopped signal-name)
;;   768                  => return (running)
;;   > 768                => return (failed  (fx- wait-status 512))
;;
(define (pid-wait-result->status wait-status)
  (let ((x wait-status))
    (cond ((not (fixnum? x)) (failed x))
          ((fx=? x   0) (void))
          ((fx<? x 256) (failed  x))
          ((fx<? x 512) (killed  (signal-number->name (fxand x 255))))
          ((fx<? x 768) (stopped (signal-number->name (fxand x 255))))
          ((fx=? x 768) (running))
          (else         (failed  (fx- x 512))))))


(define %foreground-pgid-cas  (foreign-procedure "c_pgid_foreground_cas" (int int) int))


(define foreground-pgid-get
  (let ((c-foreground-pgid-get (foreign-procedure "c_pgid_foreground_get" () int)))
    (lambda ()
      (if (sh-job-control?)
        (c-foreground-pgid-get)
        -1))))


(define foreground-pgid-set!
  (let ((c-foreground-pgid-set! (foreign-procedure "c_pgid_foreground_set" (int) int)))
    (lambda (pgid)
      (if (and pgid (sh-job-control?))
        (c-foreground-pgid-set! pgid)
        -1))))


(define sh-foreground-pgid
  (sh-make-volatile-parameter foreground-pgid-get foreground-pgid-set!))

(define (global-pgid-if-fg wait-flags pgid)
   (and pgid
        (sh-wait-flag-foreground-pgid? wait-flags)
        (sh-job-control?)
        (job-pgid (sh-globals))))


(define (call-with-foreground-job wait-flags job proc)
  (let* ((new-pgid (job-pgid-fg job))
         (our-pgid (global-pgid-if-fg wait-flags new-pgid)))
    (if our-pgid
      (dynamic-wind
        (lambda () ; before body
          ;; (debugf "call-dynamic-wind setting fg job pgid=~s" new-pgid)
          (%foreground-pgid-cas our-pgid new-pgid))
        proc       ; run   body
        (lambda () ; after body
          ;; foreground job may have created some other foreground pgid:
          ;; detect it and save inside job, in case user wants to resume it later
          (%job-pgid-fg-set! job (foreground-pgid-get))
          ;; try really hard to restore (sh-globals) as the foreground process group
          ;; (debugf "call-dynamic-wind restoring main pgid=~s" our-pgid)
          (%foreground-pgid-cas -1 our-pgid)))
      (proc))))


(define-syntax with-foreground-job
  (syntax-rules ()
    ((_  wait-flags job body1 body2 ...)
      (call-with-foreground-job wait-flags job
        (lambda () body1 body2 ...)))))


;; Internal function called by (job-wait)
(define (pid-advance caller job wait-flags)
  ; (debugf "> pid-advance wait-flags=~s job=~s pid=~s status=~s" wait-flags job (job-pid job) (job-last-status job))
  (cond
    ((job-finished? job)
      (job-last-status job)) ; job finished, exit status already available
    ((not (job-started? job))
      (raise-errorf caller "job not started yet: ~s" job))
    (else
      (with-foreground-job wait-flags job
        (pid-advance-wait caller job wait-flags (job-pid job) (job-pgid-fg job) 'sigcont)))))


;; Internal function called by (pid-advance)
(define (pid-advance-signal caller job wait-flags pid pgid signal-name)
  (assert* caller (> pid 0))
  (when pgid
    (assert* caller (> pgid 0)))
  (when (sh-wait-flag-continue-if-stopped? wait-flags)
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "pid-advance/sigcont wait-flags=~s job=~s" job wait-flags)
    (pid-kill (if (and pgid (> pgid 0)) (- pgid) pid)
              signal-name)
    ;; assume job is now running
    (job-status-set/running! job)))


;; Internal function called by (pid-advance)
(define (pid-advance-wait caller job wait-flags pid pgid signal-name)
  (when signal-name
    (pid-advance-signal caller job wait-flags pid pgid signal-name))

  ;; cannot call (sh-job-status), it would recurse back here.
  (let* ((blocking?  (sh-wait-flag-wait? wait-flags))
         (old-status (job-last-status job))
         (new-status (if (finished? old-status)
                       old-status
                       (scheduler-wait job
                         (if blocking? 'blocking 'nonblocking)))))
    ;; (debugf "pid-advance-wait old-status=~s new-status=~s pid=~s job=~s" old-status new-status (job-pid job) job)
    ;; (sleep (make-time 'time-duration 0 1))

    ;; if blocking? is #f, new-status may be 'running
    ;; indicating job status did not change i.e. it's (expected to be) still running
    (case (status->kind new-status)
      ((running stopped ok exception failed killed)
        new-status)
      (else
        (raise-errorf caller "job not started yet: ~s" job)))))


(define (maybe-queue-job-display-summary job)
  (when (or (job-id job) (job-oid job))
    (queue-job-display-summary job)))


;; Core scheduler function, in charge of waiting:
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
;; Otherwise return #f, which is intentionally not a job status.
(define (scheduler-wait preferred-job may-block)
  ;; (debugf ">   scheduler-wait may-block=~s preferred-job=~s" may-block preferred-job)
  (let ((current-job   (sh-current-job))
        (done? #f))
    (until done?
      (let ((wait-result (pid-wait -1 may-block)))
        (if (pair? wait-result)
          (let* ((job        (pid->job (car wait-result)))
                 (old-status (if job (job-last-status job) (void)))
                 (new-status (pid-wait-result->status (cdr wait-result))))

            ;; (debugf "... scheduler-wait job=~s\told-status=~s\tnew-status=~s\twait-result=~s\tpreferred-job=~s\tcurrent-job=~s" job old-status new-status wait-result preferred-job current-job)

            (when job
              (job-status-set! 'scheduler-wait job new-status)

              ;; (debugf "... scheduler-wait old-status new-status=~s job=~s" old-status new-status job)

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

          (set! done? #t)))) ; (pid-wait) did not report any status change => return
    (if preferred-job (job-last-status preferred-job) #f)))
