;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss



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



;; Create a multijob to later start it. Each element in children-jobs must be a sh-job or subtype.


;; Create an "and" multijob
(define (sh-and . children-jobs)
  (make-multijob 'sh-and assert-is-job start-multijob-and step-multijob-and children-jobs))


;; Create an "or" multijob
(define (sh-or . children-jobs)
  (make-multijob 'sh-or  assert-is-job start-multijob-or step-multijob-or children-jobs))


;; Create a "not" multijob
(define (sh-not child-job)
  (make-multijob 'sh-not assert-is-job start-multijob-not step-multijob-not (list child-job)))


(define (assert-is-job who job)
  (assert* who (sh-job? job)))


;; Create a "list" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (make-multijob 'sh-list
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    start-multijob-list
    step-multijob-list
    children-jobs-with-colon-ampersand))


;; Create a "subshell" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (make-multijob 'sh-subshell
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    start-multijob-subshell
    step-multijob-subshell
    children-jobs-with-colon-ampersand))


;; Return #t if token is a shell job terminator: ; &
(define (job-terminator? token)
  (and (symbol? token)
       (or (eq? token '&) (eq? token '\x3B;))))




;; Create a multijob to later start it.
;; Internal function, accepts an optional function to validate each element in children-jobs
(define (make-multijob kind validate-job-proc start-proc next-proc children-jobs)
  (assert* 'make-multijob (symbol? kind))
  (assert* 'make-multijob (procedure? start-proc))
  (when next-proc
    (assert* 'make-multijob (procedure? next-proc)))
  (when validate-job-proc
    (do ((tail children-jobs (cdr tail)))
        ((null? tail))
      (validate-job-proc kind (car tail))))
  (let ((mj
    (%make-multijob
      #f #f #f        ; id pid pgid
      '(new . 0) #f   ; last-status exception
      (span) 0 #f '() ; redirections
      start-proc      ; executed to start the job
      next-proc       ; executed when a child job changes status
      #f              ; working directory - initially inherited by parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      (sh-globals)    ; parent job - initially the global job
      kind
      -1              ; no child running yet
      (list->span children-jobs))))

    ;; set the parent of children-jobs
    (list-iterate children-jobs
      (lambda (elem)
        (when (sh-job? elem)
          (job-parent-set! elem mj))))
    mj))


;; Internal function stored in (job-start-proc job) by (sh-list),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-list job options)
  (assert* 'sh-list (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-list (fx=? -1 (multijob-current-child-index job)))
  (let ((%proc-start-multijob-list
          (lambda (job)
            (job-remap-fds! job)
            (job-env/apply-lazy! job 'export)
            ; Do not yet assign a job-id.
            (step-multijob-list job (void)))))
    (if (memq 'spawn options)
      ;; spawn a subprocess and run (%proc... job) and (job-step-proc job) inside it
      (spawn-procedure job %proc-start-multijob-list options)
      ;; run (proc) in the caller's process
      (%proc-start-multijob-list job))))



;; internal function stored in (job-start-proc job) by (sh-subshell) multijobs
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (job-step-proc job)
;; passing the job job as only argument,
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new subshell will be inserted
;;     into the corresponding process group id - which must already exist.
;;   'spawn: a symbol. enabled by default, because this function always spawns a subprocess.
(define (start-multijob-subshell job options)
  (assert* 'sh-subshell (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-subshell (fx=? -1 (multijob-current-child-index job)))
  (let ((%proc-start-multijob-subshell
          (lambda (job)
            ; this will be executed in a subprocess.
            ;
            ; deactivate job control:
            ; a. do not create process groups => all child processes will
            ;    inherit process group from the subshell itself
            ; b. do not change the foregroud process group
            ;
            ; note that commands executed by the subshell CAN reactivate job control:
            ; in such case, (sh-job-control? #t) will self-suspend the subshell with SIGTTIN
            ; until the user resumes it in the foreground.
            (sh-job-control? #f)
            ; do not output status changes of children jobs.
            (sh-job-display/summary? #f)

            (job-remap-fds! job)
            (job-env/apply-lazy! job 'export))))
    ;; spawn a subprocess and run (%proc... job) and (job-step-proc job) inside it
    (spawn-procedure job %proc-start-multijob-subshell options)))



;; Internal function stored in (job-start-proc job) by (sh-and),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-and job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-and (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-and (fx=? -1 (multijob-current-child-index job)))
  (let ((%proc-start-multijob-and
          (lambda (job)
            (job-remap-fds! job)
            (job-env/apply-lazy! job 'export)
            (if (span-empty? (multijob-children job))
              ; (sh-and) with zero children -> job completes successfully
              (job-status-set! 'start-multijob-and job (void))
              ; Do not yet assign a job-id.
              (step-multijob-and job (void))))))
    (if (memq 'spawn options)
      ;; spawn a subprocess and run (%proc... job) inside it
      (spawn-procedure job %proc-start-multijob-and options)
      ;; run (%proc... job) in the caller's process
      (%proc-start-multijob-and job))))


;; Internal function stored in (job-start-proc job) by (sh-or),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-or job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-or (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-or (fx=? -1 (multijob-current-child-index job)))
  (let ((%proc-start-multijob-or
          (lambda (job)
            (job-remap-fds! job)
            (job-env/apply-lazy! job 'export)
            ; (debugf "start-multijob-or ~s empty children? = ~s" job (span-empty? (multijob-children job)))
            (if (span-empty? (multijob-children job))
              ; (sh-or) with zero children -> job fails with '(exited . 256)
              (job-status-set! 'start-multijob-or job '(exited . 256))
              ; Do not yet assign a job-id.
              (step-multijob-or job '(exited . 256))))))
    (if (memq 'spawn options)
      ;; spawn a subprocess and run (%proc... job) and (job-step-proc job) inside it
      (spawn-procedure job %proc-start-multijob-or options)
      ;; run (%proc... job) in the caller's process
      (%proc-start-multijob-or job))))


;; Internal function stored in (job-start-proc job) by (sh-not),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-not job options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (assert* 'sh-not (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-not (fx=? -1 (multijob-current-child-index job)))
  (let ((%proc-start-multijob-not
          (lambda (job)
            (job-remap-fds! job)
            (job-env/apply-lazy! job 'export)
            ; Do not yet assign a job-id.
            (step-multijob-not job (void)))))
    (if (memq 'spawn options)
      ;; spawn a subprocess and run (%proc... job) and (job-step-proc job) inside it
      (spawn-procedure job %proc-start-multijob-not options)
      ;; run (%proc... job) in the caller's process
      (%proc-start-multijob-not job))))



;; Fork a new subprocess, and in the child subprocess
;; call (proc job) once, then call (sh-wait job) repeatedly - which calls (job-step-proc job) if set -
;; until (job-finished? job) returns truish.
;;
;; The new subprocess is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created subprocess.
;;
;; Note: does not call (job-env/apply-lazy! job).
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: an integer,
;;     if present and > 0 the new subprocess will be inserted
;;       into the corresponding process group id - which must already exist.
;;     Otherwise, if present and = 0 a new process group will be created
;;       and the new subprocess will be moved into it.
;;   'spawn: a symbol. enabled by default, because this function always spawns a subprocess.
;;
;; Return job status, which is usually '(running ...)
;;   for a complete list of possible job statuses, see (sh-job-status)
(define spawn-procedure
  (let ((c-fork-pid (foreign-procedure "c_fork_pid" (int) int)))
    (lambda (job proc options)
      (assert* 'sh-start (sh-job? job))
      (assert* 'sh-start (procedure? proc))
      (assert* 'sh-start (logbit? 1  (procedure-arity-mask proc)))
      (assert* 'sh-start (list? options))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-fork-pid (or process-group-id -1))))
        (cond
          ((< ret 0) ; fork() failed
            (raise-c-errno 'sh-start 'fork ret))
          ((= ret 0) ; child
            (let ((status '(exited . 255)))
              (dynamic-wind
                (lambda () ; run before body
                  ; in child process, suppress messages about started/completed jobs
                  (sh-job-display/summary? #f)
                  (let ((pid  (pid-get))
                        (pgid (pgid-get 0)))
                    ; this process now "is" the job => update (sh-globals)' pid and pgid
                    (job-pid-set!  (sh-globals) pid)
                    (job-pgid-set! (sh-globals) pgid)
                    ; cannot wait on our own process.
                    (job-pid-set!  job #f)
                    (job-pgid-set! job #f)

                    ; we would like to set status to '(unknown . 0)
                    ; but that causes (sh-wait job) below to think that job already exited,
                    ; and skips calls to (job-step-proc job) to start nested jobs.
                    ;
                    ; warning: do not call (job-status-set! job ...)
                    ; because it detects that job is running, and assigns a job-id to it,
                    ; which is only annoying - cannot do anything useful with such job-id.
                    (%job-last-status-set! job '(running . #f))))
                (lambda () ; body
                  ;b (debugf ">   spawn-procedure job=~a subprocess calling proc ~s" (sh-job-display/string job) proc)
                  (let ((ret (proc job)))
                    ;b (debugf "... spawn-procedure job=~a subprocess proc returned ~s" (sh-job-display/string job) ret)
                    (void))
                  (set! status (sh-wait job)))
                (lambda () ; run after body, even if it raised a condition
                  ;b (debugf "< spawn-procedure job=~a subprocess exiting with status=~s" (sh-job-display/string job) status)
                  (exit-with-job-status status)))))
          ((> ret 0) ; parent
            (job-pid-set! job ret)
            (job-pgid-set! job process-group-id)
            (cons 'running #f)))))))


;; Internal function called by (advance-job) called by (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
(define (advance-multijob mode mj)
  ; (debugf ">  advance-multijob mode=~s job=~a id=~s status=~s" mode (sh-job-display/string mj) (job-id mj) (job-last-status mj))
  (job-status-set/running! mj)
  (let* ((child (sh-multijob-child-ref mj (multijob-current-child-index mj)))
         ;; call (advance-job) on child
         (child-status (if (sh-job? child) (advance-job mode child) (void)))
         (step-proc (job-step-proc mj)))
    ;a (debugf ">  advance-multijob mode=~s job=~s child=~s child-status=~s" mode mj child child-status)
    (cond
      ((or (not step-proc) (job-status-stops-or-ends-multijob? child-status))
        ; propagate child exit status and return
        (job-status-set! 'advance-multijob mj child-status)
        child-status)
      ((job-status-member? child-status '(exited killed unknown))
        ; child exited: advance multijob by calling (job-step-proc)
        ; then call (advance-multijob) again multijob job is still running.
        ; (debugf "... advance-multijob > step-proc ~s status=~s" mj (job-last-status mj))
        (step-proc mj child-status)
        ; (debugf "... advance-multijob < step-proc ~s status=~s" mj (job-last-status mj))
        (if (job-has-status? mj '(running))
          (advance-multijob mode mj)
          (job-last-status mj)))
      ((job-status-member? child-status '(running))
        ; child is still running. propagate child status and return
        (job-status-set! 'advance-multijob mj (cons 'running (job-id mj)))) ; (job-id mj) may still be #f
      ((job-status-member? child-status '(stopped))
        ; child is stopped.
        ; if mode is sh-wait or sh-sigcont+wait, wait for it again.
        ; otherwise propagate child status and return
        (if (memq mode '(sh-wait sh-sigcont+wait))
          (advance-multijob mode mj)
          (job-status-set! 'advance-multijob mj child-status)))
      (#t
        (raise-errorf mode "child job not started yet: ~s" child)))))


;; Run next child job in a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
(define (step-multijob-and mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    ;a (debugf "step-multijob-and idx=~s child=~s prev-child-status=~s" idx child prev-child-status)
    (if (and (sh-ok? prev-child-status) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (start-any 'sh-and child '(catch))))
          (when (job-status-finished? child-status)
            ; child job already finished, iterate
            (step-multijob-and mj child-status))))
      (begin
        ; previous child failed, or interrupted, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'step-multijob-and mj prev-child-status)))))



;; Run next child job in a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (step-multijob-or mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (not (sh-ok? prev-child-status))
             (not (job-status-ends-multijob? prev-child-status))
             (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (start-any 'sh-or child '(catch))))
          (when (job-status-finished? child-status)
            ; child job already finished, iterate
            (step-multijob-or mj child-status))))
      (begin
        ; previous child successful, or interrupted, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'step-multijob-or mj prev-child-status)))))



;; Run the child job in a multijob containing a "not" and one child job,
;; or collect the exit status of the child job after it exited.
;; Used by (sh-not), implements runtime behavior of shell syntax ! foo
(define (step-multijob-not mj prev-child-status)
  (assert* 'sh-not (fx=? 1 (sh-multijob-child-length mj)))

  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (sh-job? child)
      (begin
        ; start child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (start-any 'sh-not child '(catch))))
          (when (job-status-finished? child-status)
            ; child job already finished, iterate
            (step-multijob-not mj child-status))))
      (begin
        ; child job exited, negate its exit status
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'step-multijob-not mj
          (cond
            ((sh-ok? prev-child-status) '(exited . 1))
            ((job-status-ends-multijob? prev-child-status) prev-child-status)
            (#t (void))))))))



;; Run first or next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (step-multijob-list mj prev-child-status)
  (let* ((idx      (fx1+ (multijob-current-child-index mj)))
         (child-n  (span-length (multijob-children mj)))
         (iterate? #t)
         (interrupted? #f))
    ; (debugf "step-multijob-list > ~s idx=~s prev-child-status=~s" mj (fx1- idx) prev-child-status)
    (assert* 'step-multijob-list (job-status-member? prev-child-status '(exited killed unknown)))
    ; idx = 0 if called by (start-multijob-list)
    (assert* 'step-multijob-list (fx>=? idx 0))
    (while (and iterate? (not interrupted?) (fx<=? idx child-n))
      (multijob-current-child-index-set! mj idx)
      (let ((child (sh-multijob-child-ref mj idx)))
        ; (debugf "job-step-list status = ~s, start child ~s = ~s" (job-last-status mj) idx child)
        (when (sh-job? child)
          ; start next child job
          (let* ((child-status (start-any 'sh-list child '(catch)))
                 (child-started? (job-status-started? child-status)))
            ; iterate on subsequent child jobs in two cases:
            ; if child job is followed by '&
            ; if child job has already finished
            (if (eq? '& (sh-multijob-child-ref mj (fx1+ idx)))
              ; run child job asynchronously
              (when child-started?
                ; child job is running or stopped, assign a job-id to it
                (job-id-set! child))
              ; run child job synchronously:
              (begin
                (set! interrupted? (job-status-ends-multijob? child-status))
                (if child-started?
                  ; stop iterating if child job is still running or is stopped
                  (set! iterate? #f)
                  ; remember exit status of last sync child, and keep iterating
                  (set! prev-child-status child-status)))))))
      ; in any case, advance idx after each iteration
      (set! idx (fx1+ idx)))
    (when (or interrupted?
              (and (fx>? idx child-n)
                   (job-status-finished? prev-child-status)))
      ; end of children reached, or sync child interrupted.
      ; propagate status of last sync child
      (multijob-current-child-index-set! mj -1)
      (job-status-set! 'step-multijob-list mj prev-child-status))))


;; Run first or next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-subshell), implements runtime behavior of shell syntax [ ... ]
(define (step-multijob-subshell mj prev-child-status)
  (step-multijob-list mj prev-child-status))
