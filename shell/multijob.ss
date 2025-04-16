;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; Return number of children in specified multijob.
;; Return 0 if mj is not a multijob.
(define (sh-multijob-child-length mj)
  (if (sh-multijob? mj)
    (span-length (multijob-children mj))
    0))


;; Return i-th child in specified multijob.
;; Return #f if mj is not a multijob, or i is out-of-bounds.
;; If i-th child is not a job (may be a job separator as ; &), return it.
(define (sh-multijob-child-ref mj idx)
  ;; (debugf "sh-multijob-child-ref mj=~a\tidx=~s" mj idx)
  (if (sh-multijob? mj)
    (let ((children (multijob-children mj)))
      (if (fx<? -1 idx (span-length children))
        (span-ref children idx)
        #f))
    #f))


;; Return status of i-th child in specified multijob.
;; Return #f if mj is not a multijob, or i is out-of-bounds,
;; or i-th child is not a job (may be a job separator as ; &).
(define (sh-multijob-child-status mj idx)
  (let ((child (sh-multijob-child-ref mj idx)))
    (if (sh-job? child)
      (job-last-status child)
      #f)))


;; Create and return vector containing status of each child of multijob mj,
;; or empty vector if mj is not a multijob
(define (multijob-children-last-status mj)
  (if (sh-multijob? mj)
    (let* ((children (multijob-children mj))
           (vec      (make-vector (span-length children) (void))))
      (span-iterate children
        (lambda (i child)
          (when (sh-job? child)
            (vector-set! vec i (job-last-status child)))))
      vec)
    '#()))


;; Create a multijob to later start it. Each element in children-jobs must be a sh-job or subtype.


;; Create an "and" multijob
(define (sh-and . children-jobs)
  (make-multijob 'sh-and assert-is-job mj-and-start mj-and-step children-jobs))


;; Create an "or" multijob
(define (sh-or . children-jobs)
  (make-multijob 'sh-or  assert-is-job mj-or-start mj-or-step children-jobs))


;; Create a "not" multijob
(define (sh-not child-job)
  (make-multijob 'sh-not assert-is-job mj-not-start mj-not-step (list child-job)))


(define (assert-is-job who job)
  (assert* who (sh-job? job)))


;; Create a "list" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (make-multijob 'sh-list
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    mj-list-start
    mj-list-step
    children-jobs-with-colon-ampersand))


;; Create a "subshell" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (make-multijob 'sh-subshell
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    mj-subshell-start
    #f
    children-jobs-with-colon-ampersand))


;; Return #t if token is a shell job terminator: ; &
(define (job-terminator? token)
  (and (symbol? token)
       (or (eq? token '&) (eq? token '\x3B;))))




;; Create a multijob to later start it.
;; Internal function, accepts an optional function to validate each element in children-jobs
(define (make-multijob kind validate-job-proc start-proc step-proc children-jobs)
  (assert* 'make-multijob (symbol? kind))
  (assert* 'make-multijob (procedure? start-proc))
  (when step-proc
    (assert* 'make-multijob (procedure? step-proc)))
  (when validate-job-proc
    (do ((tail children-jobs (cdr tail)))
        ((null? tail))
      (validate-job-proc kind (car tail))))
  (let* ((current-job (sh-current-job))
         (mj
    (%make-multijob
      #f #f #f #f     ; id oid pid pgid
      (new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      start-proc      ; executed to start the job
      step-proc       ; executed when a child job changes status
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      (and current-job (job-parent current-job)) ; temp parent job
      (or current-job (sh-globals))              ; default parent job
      kind
      -1              ; no child running yet
      (list->span children-jobs))))

    ;; set the parent of children-jobs
    (for-list ((elem children-jobs))
      (when (sh-job? elem)
        (job-default-parent-set! elem mj)))
    mj))


;; Internal function stored in (job-start-proc job) by (sh-list),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-list-start job options)
  (assert* 'sh-list (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-list (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-job-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ; Do not yet assign a job-id.
      (mj-list-step job (void)))))


;; internal function stored in (job-start-proc job) by (sh-subshell) multijobs
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (job-step-proc job)
;; passing the job job as only argument,
;;
;; Options are the same as described in (sh-start).
;; Option 'spawn? #t is enabled by default, because this function always spawns a subprocess.
(define (mj-subshell-start job options)
  (assert* 'sh-subshell (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-subshell (fx=? -1 (multijob-current-child-index job)))
  ;; run closure in a subprocess
  (spawn-job-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)

      ;; pretend that this subshell is equivalent to an sh-list:
      ;; since we are in a subprocess, this does not alter the original object
      ;; and (sh-wait) needs it to know what to do with children jobs.
      (job-step-proc-set! job mj-list-step)

      ;; no need to wait for job, (spawn-job-procedure) already does that
      (sh-wait job))))




;; Internal function stored in (job-start-proc job) by (sh-and),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-and-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-and (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-and (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-job-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      (if (span-empty? (multijob-children job))
        ; (sh-and) with zero children -> job completes successfully
        (job-status-set! 'mj-and-start job (void))
        ; Do not yet assign a job-id.
        (mj-and-step job (void))))))




;; Internal function stored in (job-start-proc job) by (sh-or),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-or-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-or (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-or (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-job-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; (debugf "mj-or-start ~s empty children? = ~s" job (span-empty? (multijob-children job)))
      (if (span-empty? (multijob-children job))
        ;; (sh-or) with zero children -> job fails with (failed 256)
        (job-status-set! 'mj-or-start job (failed 256))
        ;; Do not yet assign a job-id.
        (mj-or-step job (failed 256))))))


;; Internal function stored in (job-start-proc job) by (sh-not),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-not-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-not (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-not (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-job-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; Do not yet assign a job-id.
      (mj-not-step job (void)))))


;; executed in subprocesses for setting up their parameters:
;;   prepare to run silently and without job control
;;   store new pid and pgid into (sh-globals)
(define (spawn-job-procedure-child-before job)
  ;; in child process, deactivate job control
  ;;
  ;; a. do not create process groups => all child processes will
  ;;    inherit process group from the subshell itself
  ;; b. do not change the foregroud process group
  ;;
  ;; note that commands executed by the subprocess CAN reactivate job control:
  ;; in such case, (sh-job-control? #t) will self-suspend the subshell with SIGTTIN
  ;; until the user resumes it in the foreground.
  (sh-job-control? #f)

  ;; in child process, suppress messages about started/completed jobs
  (sh-job-display-summary? #f)

  (let ((pid  (pid-get))
        (pgid (pgid-get 0)))
    ;; this process now "is" the job => update (sh-globals)' pid and pgid
    (%job-pid-set!  (sh-globals) pid)
    (%job-pgid-set! (sh-globals) pgid)
    ;; cannot wait on our own process.
    (%job-pid-set!  job #f)
    (%job-pgid-set! job #f)

    ;; warning: do not call (job-status-set! job ...)
    ;; because it detects that job is running, and assigns a job-id to it,
    ;; which is only annoying - cannot do anything useful with such job-id.
  (%job-last-status-set! job (running))))


;; Internal function called by (job-wait) called by (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
(define (mj-advance caller mj wait-flags)
  ; (debugf ">  mj-advance wait-flags=~s job=~s id=~s status=~s" wait-flags mj (job-id mj) (job-last-status mj))
  (job-status-set/running! mj)
  (let* ((child (sh-multijob-child-ref mj (multijob-current-child-index mj)))
         ;; call (job-wait) on child
         (child-status (if (sh-job? child) (job-wait caller child wait-flags) (void)))
         (step-proc   (job-step-proc mj)))
    ;; (debugf ">  mj-advance job=~s child=~s child-status=~s" mj child child-status)
    (cond
      ((or (not step-proc) (status-stops-or-ends-multijob? child-status))
        ;; propagate child exit status and return
        (job-status-set! 'mj-advance mj child-status)
        child-status)
      ((finished? child-status)
        ;; child failed: advance multijob by calling (job-step-proc)
        ;; then call (mj-advance) again multijob if job is still running.
        ;; (debugf "... mj-advance > step-proc ~s status=~s" mj (job-last-status mj))
        (step-proc mj child-status)
        ;; (debugf "... mj-advance < step-proc ~s status=~s" mj (job-last-status mj))
        (if (job-running? mj)
          (mj-advance caller mj wait-flags)
          (job-last-status mj)))
      ((running? child-status)
        ;; child is still running.
        ;; if wait-flags tell to wait, then wait for child to change status again.
        ;; otherwise propagate child status and return.
        (if (sh-wait-flag-wait? wait-flags)
           (mj-advance caller mj wait-flags)
           (job-last-status mj)))
      ((stopped? child-status)
        ;; child is stopped: propagate child status and return.
        ;; if caller wants to wait until child or mj finished, he will invoke (mj-advance) again
        (job-status-set! 'mj-advance mj child-status))
      (else
        (raise-errorf caller "child job not started yet: ~s" child)))))

(define options-catch '(catch? #t))

;; Run next child job in a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
(define (mj-and-step mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    ;; (debugf "mj-and-step idx=~s child=~s prev-child-status=~s" idx child prev-child-status)
    (if (and (ok? prev-child-status) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (job-start 'sh-and child options-catch)))
          (when (finished? child-status)
            ; child job already finished, iterate
            (mj-and-step mj child-status))))
      (begin
        ; previous child failed, or interrupted, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'mj-and-step mj prev-child-status)))))



;; Run next child job in a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (mj-or-step mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (not (ok? prev-child-status))
             (not (status-ends-multijob? prev-child-status))
             (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (job-start 'sh-or child options-catch)))
          (when (finished? child-status)
            ; child job already finished, iterate
            (mj-or-step mj child-status))))
      (begin
        ; previous child successful, or interrupted, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'mj-or-step mj prev-child-status)))))



;; Run the child job in a multijob containing a "not" and one child job,
;; or collect the exit status of the child job after it failed.
;; Used by (sh-not), implements runtime behavior of shell syntax ! foo
(define (mj-not-step mj prev-child-status)
  (assert* 'sh-not (fx=? 1 (sh-multijob-child-length mj)))

  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (sh-job? child)
      (begin
        ; start child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (job-start 'sh-not child options-catch)))
          (when (finished? child-status)
            ; child job already finished, iterate
            (mj-not-step mj child-status))))
      (begin
        ; child job failed, negate its exit status
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'mj-not-step mj
          (cond
            ((ok? prev-child-status) (failed 1))
            ((status-ends-multijob? prev-child-status) prev-child-status)
            (else (void))))))))



;; Run first or next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (mj-list-step mj prev-child-status)
  (let* ((idx      (fx1+ (multijob-current-child-index mj)))
         (child-n  (span-length (multijob-children mj)))
         (iterate? #t)
         (interrupted? #f))
    ; (debugf "mj-list-step > ~s idx=~s prev-child-status=~s" mj (fx1- idx) prev-child-status)
    (assert* 'mj-list-step (finished? prev-child-status))
    ; idx = 0 if called by (mj-list-start)
    (assert* 'mj-list-step (fx>=? idx 0))
    (while (and iterate? (not interrupted?) (fx<=? idx child-n))
      (multijob-current-child-index-set! mj idx)
      (let ((child (sh-multijob-child-ref mj idx)))
        (when (sh-job? child)
          ;; start next child job
          (let* ((child-async? (eq? '& (sh-multijob-child-ref mj (fx1+ idx))))
                 (child-status (job-start 'sh-list child options-catch))
                 (child-started? (started? child-status)))
            ; iterate on subsequent child jobs in two cases:
            ; if child job is followed by '&
            ; if child job has already finished
            ; (debugf "mj-list-step~a started child ~s" (if child-async? " async" "") child)
            (if child-async?
              ; run child job asynchronously
              (when child-started?
                ; child job is running or stopped, assign a job-id to it
                (job-id-set! child))
              ; run child job synchronously:
              (begin
                (set! interrupted? (status-ends-multijob? child-status))
                (if child-started?
                  ; stop iterating if child job is still running or is stopped
                  (set! iterate? #f)
                  ; remember exit status of last sync child, and keep iterating
                  (set! prev-child-status child-status)))))))
      ; in any case, advance idx after each iteration
      (set! idx (fx1+ idx)))
    (when (or interrupted?
              (and (fx>? idx child-n)
                   (finished? prev-child-status)))
      ; end of children reached, or sync child interrupted.
      ; propagate status of last sync child
      (multijob-current-child-index-set! mj -1)
      (job-status-set! 'mj-list-step mj prev-child-status))))


;; recursively kill a multijob and all its children jobs.
;; return unspecified value.
(define (multijob-kill mj signal-name)
  (let ((is-list? (eq? 'sh-list (multijob-kind mj))))
    (span-iterate (multijob-children mj)
      (lambda (i elem)
        (when (and (sh-job? elem) (job-started? elem))
          (unless (and is-list? (eq? '& (sh-multijob-child-ref mj (fx1+ i))))
            ;; try to kill children sh-expr jobs only if they have a pid or pgid.
            ;; Reason: killing other sh-expr jobs may non-locally jump to their continuation
            ;;         and this loop would not continue.
            (when (or (job-pid elem) (job-pgid elem) (not (sh-expr? elem)))
              (job-kill elem signal-name))))
        #t)))) ; continue iteration
