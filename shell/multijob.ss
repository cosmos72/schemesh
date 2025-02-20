;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


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
  (let ((mj
    (%make-multijob
      #f #f #f        ; id pid pgid
      '(new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      start-proc      ; executed to start the job
      step-proc       ; executed when a child job changes status
      #f #f           ; resume-proc suspend-proc
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      #f (sh-globals) ; no temp parent. default parent job is initially the global job
      kind
      -1              ; no child running yet
      (list->span children-jobs))))

    ;; set the parent of children-jobs
    (list-iterate children-jobs
      (lambda (elem)
        (when (sh-job? elem)
          (job-default-parent-set! elem mj))))
    mj))


;; Internal function stored in (job-start-proc job) by (sh-list),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-list job options)
  (assert* 'sh-list (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-list (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ; Do not yet assign a job-id.
      (step-multijob-list job (void)))))


;; internal function stored in (job-start-proc job) by (sh-subshell) multijobs
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (job-step-proc job)
;; passing the job job as only argument,
;;
;; Options are the same as described in (sh-start).
;; Option '(spawn? . #t) is enabled by default, because this function always spawns a subprocess.
(define (start-multijob-subshell job options)
  (assert* 'sh-subshell (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-subshell (fx=? -1 (multijob-current-child-index job)))
  (spawn-procedure job options
    (lambda (job options)
      ;; this will be executed in a subprocess.
      ;;
      ;; deactivate job control:
      ;; a. do not create process groups => all child processes will
      ;;    inherit process group from the subshell itself
      ;; b. do not change the foregroud process group
      ;;
      ;; note that commands executed by the subshell CAN reactivate job control:
      ;; in such case, (sh-job-control? #t) will self-suspend the subshell with SIGTTIN
      ;; until the user resumes it in the foreground.
      (sh-job-control? #f)

      ;; do not output status changes of children jobs.
      (sh-job-display-summary? #f)

      ;; do not save history when subshell exits.
      (let ((lctx (sh-repl-args-linectx)))
        (when (linectx? lctx)
          (let ((history (linectx-history lctx)))
            (when history
              (charhistory-path-set! history #f)))))

      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)

      ;; pretend that this subshell is equivalent to an sh-list:
      ;; since we are in a subprocess, this does not alter the original object
      ;; and (sh-wait) needs it to know what to do with children jobs.
      (job-step-proc-set! job step-multijob-list)

      (sh-wait job)))) ; execute and wait each child job




;; Internal function stored in (job-start-proc job) by (sh-and),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-and job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-and (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-and (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure
    job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      (if (span-empty? (multijob-children job))
        ; (sh-and) with zero children -> job completes successfully
        (job-status-set! 'start-multijob-and job (void))
        ; Do not yet assign a job-id.
        (step-multijob-and job (void))))))




;; Internal function stored in (job-start-proc job) by (sh-or),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-or job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-or (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-or (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; (debugf "start-multijob-or ~s empty children? = ~s" job (span-empty? (multijob-children job)))
      (if (span-empty? (multijob-children job))
        ;; (sh-or) with zero children -> job fails with '(failed 256)
        (job-status-set! 'start-multijob-or job '(failed 256))
        ;; Do not yet assign a job-id.
        (step-multijob-or job '(failed 256))))))


;; Internal function stored in (job-start-proc job) by (sh-not),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (start-multijob-not job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-not (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-not (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; Do not yet assign a job-id.
      (step-multijob-not job (void)))))



;; Fork a new subprocess, and in the child subprocess
;; call (proc job options) once, then call (sh-wait job) repeatedly - which calls (job-step-proc job) if set -
;; until (job-finished? job) returns truish.
;;
;; The new subprocess is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created subprocess.
;;
;; Note: does not call (job-env/apply-lazy! job).
;;
;; Options is an association list, see (sh-start) for allowed keys and values.
;;   'spawn: a symbol. enabled by default, because this function always spawns a subprocess.
;;
;; Return job status, which is usually '(running ...)
;;   for a complete list of possible job statuses, see (sh-job-status)
(define spawn-procedure
  (let ((c-fork-pid (foreign-procedure "c_fork_pid" (int) int)))
    (lambda (job options proc)
      (assert* 'sh-start (sh-job? job))
      (assert* 'sh-start (procedure? proc))
      (assert* 'sh-start (logbit? 2  (procedure-arity-mask proc)))
      (assert* 'sh-start (list? options))
      (let* ((process-group-id (options->process-group-id options))
             (_                (options->set-temp-parent! job options))
             (ret              (c-fork-pid (or process-group-id -1))))
        (cond
          ((< ret 0) ; fork() failed
            (raise-c-errno 'sh-start 'fork ret))
          ((= ret 0) ; child
            (let ((status '(exception #f)))
              (dynamic-wind
                (lambda () ; run before body
                  ; in child process, suppress messages about started/completed jobs
                  (sh-job-display-summary? #f)
                  (let ((pid  (pid-get))
                        (pgid (pgid-get 0)))
                    ; this process now "is" the job => update (sh-globals)' pid and pgid
                    (%job-pid-set!  (sh-globals) pid)
                    (%job-pgid-set! (sh-globals) pgid)
                    ; cannot wait on our own process.
                    (%job-pid-set!  job #f)
                    (%job-pgid-set! job #f)

                    ; we would like to set status to (void)
                    ; but that causes (sh-wait job) below to think that job already finished,
                    ; and skips calls to (job-step-proc job) to start nested jobs.
                    ;
                    ; warning: do not call (job-status-set! job ...)
                    ; because it detects that job is running, and assigns a job-id to it,
                    ; which is only annoying - cannot do anything useful with such job-id.
                    (%job-last-status-set! job '(running))))
                (lambda () ; body
                  ;c (debugf "> [child] spawn-procedure job=~a subprocess calling proc ~s" (sh-job->string job) proc)
                  (let ((ret (proc job options)))
                    ;c (debugf ". [child] spawn-procedure job=~a subprocess proc returned ~s pid=~s" (sh-job->string job) ret (job-pid job))
                    (void))
                  (set! status (sh-wait job)))
                (lambda () ; run after body, even if it raised a condition
                  ;c (debugf "< [child] spawn-procedure job=~a subprocess exiting with pid=~s status=~s" (sh-job->string job) (job-pid job) status)
                  (exit-with-job-status status)))))
          ((> ret 0) ; parent
            (job-pid-set! job ret)
            (job-pgid-set! job process-group-id)
            '(running)))))))


;; if options contain '(spawn? . #t) then remove such options and call (spawn-procedure job options proc)
;; otherwise directly call (proc job options)
;;
;; WARNING (proc job options) must call (sh-job-status-set! job), because the return value of (proc ...) is ignored
(define (call-or-spawn-procedure job options proc)
  ;c (debugf "call-or-spawn-procedure options=~s proc=~s job=~a" options proc (sh-job->string job))
  (if (options->spawn? options)
    ;; spawn a subprocess and run (%proc... job) inside it
    (spawn-procedure job (options-filter-out options '(spawn?)) proc)
    ;; directly call (proc job options) in the caller's process
    (let ((options (options->set-temp-parent! job options)))
      (proc job options))))


;; Internal function called by (job-resume) called by (sh-fg) (sh-bg) (sh-resume) (sh-wait) (sh-job-status)
(define (advance-multijob caller mj wait-flags)
  ; (debugf ">  advance-multijob wait-flags=~s job=~a id=~s status=~s" wait-flags (sh-job->string mj) (job-id mj) (job-last-status mj))
  (job-status-set/running! mj)
  (let* ((child (sh-multijob-child-ref mj (multijob-current-child-index mj)))
         ;; call (job-resume) on child
         (child-status (if (sh-job? child) (job-resume caller child wait-flags) (void)))
         (step-proc (job-step-proc mj)))
    ;a (debugf ">  advance-multijob job=~s child=~s child-status=~s" mj child child-status)
    (cond
      ((or (not step-proc) (status-stops-or-ends-multijob? child-status))
        ; propagate child exit status and return
        (job-status-set! 'advance-multijob mj child-status)
        child-status)
      ((sh-finished? child-status)
        ; child failed: advance multijob by calling (job-step-proc)
        ; then call (advance-multijob) again multijob if job is still running.
        ; (debugf "... advance-multijob > step-proc ~s status=~s" mj (job-last-status mj))
        (step-proc mj child-status)
        ; (debugf "... advance-multijob < step-proc ~s status=~s" mj (job-last-status mj))
        (if (job-running? mj)
          (advance-multijob caller mj wait-flags)
          (job-last-status mj)))
      ((sh-running? child-status)
        ;; child is still running.
        ;; if wait-flags tell to wait, then wait for child to change status again.
        ;; otherwise propagate child status and return.
        (if (jr-flag-wait? wait-flags)
           (advance-multijob caller mj wait-flags)
           (job-last-status mj)))
      ((sh-stopped? child-status)
        ;; child is stopped.
        ;; if wait-flags tell to wait until child finishes, then wait for child to change status again.
        ;; otherwise propagate child status and return
        (if (jr-flag-wait-until-finished? wait-flags)
          (advance-multijob caller mj wait-flags)
          (job-status-set! 'advance-multijob mj child-status)))
      (else
        (raise-errorf caller "child job not started yet: ~s" child)))))

(define options-catch '((catch? . #t)))

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
        (let ((child-status (job-start 'sh-and child options-catch)))
          (when (sh-finished? child-status)
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
             (not (status-ends-multijob? prev-child-status))
             (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (job-start 'sh-or child options-catch)))
          (when (sh-finished? child-status)
            ; child job already finished, iterate
            (step-multijob-or mj child-status))))
      (begin
        ; previous child successful, or interrupted, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'step-multijob-or mj prev-child-status)))))



;; Run the child job in a multijob containing a "not" and one child job,
;; or collect the exit status of the child job after it failed.
;; Used by (sh-not), implements runtime behavior of shell syntax ! foo
(define (step-multijob-not mj prev-child-status)
  (assert* 'sh-not (fx=? 1 (sh-multijob-child-length mj)))

  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (sh-job? child)
      (begin
        ; start child job
        (multijob-current-child-index-set! mj idx)
        (let ((child-status (job-start 'sh-not child options-catch)))
          (when (sh-finished? child-status)
            ; child job already finished, iterate
            (step-multijob-not mj child-status))))
      (begin
        ; child job failed, negate its exit status
        (multijob-current-child-index-set! mj -1)
        (job-status-set! 'step-multijob-not mj
          (cond
            ((sh-ok? prev-child-status) '(failed 1))
            ((status-ends-multijob? prev-child-status) prev-child-status)
            (else (void))))))))



;; Run first or next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (step-multijob-list mj prev-child-status)
  (let* ((idx      (fx1+ (multijob-current-child-index mj)))
         (child-n  (span-length (multijob-children mj)))
         (iterate? #t)
         (interrupted? #f))
    ; (debugf "step-multijob-list > ~s idx=~s prev-child-status=~s" mj (fx1- idx) prev-child-status)
    (assert* 'step-multijob-list (sh-finished? prev-child-status))
    ; idx = 0 if called by (start-multijob-list)
    (assert* 'step-multijob-list (fx>=? idx 0))
    (while (and iterate? (not interrupted?) (fx<=? idx child-n))
      (multijob-current-child-index-set! mj idx)
      (let ((child (sh-multijob-child-ref mj idx)))
        (when (sh-job? child)
          ;; start next child job
          (let* ((child-async? (eq? '& (sh-multijob-child-ref mj (fx1+ idx))))
                 (child-status (job-start 'sh-list child options-catch))
                 (child-started? (sh-started? child-status)))
            ; iterate on subsequent child jobs in two cases:
            ; if child job is followed by '&
            ; if child job has already finished
            ; (debugf "step-multijob-list~a started child ~s" (if child-async? " async" "") child)
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
                   (sh-finished? prev-child-status)))
      ; end of children reached, or sync child interrupted.
      ; propagate status of last sync child
      (multijob-current-child-index-set! mj -1)
      (job-status-set! 'step-multijob-list mj prev-child-status))))


(define (loop-start-resume-child-with-suspend caller mj options child)
  (let %loop ((status '(new)))
    ; (debugf "loop-start-resume-child-with-suspend child=~s status=~s" child status)
    (case (sh-status->kind status)
      ((new)
        (%loop (job-start caller child options)))
      ((running)
        ;; TODO: we should mark mj as running, and yield it.
        (%loop (sh-fg child)))
      ((stopped)
        (job-suspend mj)
        (%loop (job-last-status child)))
      (else
        status))))


;; Run the children jobs in a multijob list.
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
;; each child job can be optionally followed by '& or ';
(define (run-multijob-list mj options)
  (let* ((options  (cons '(catch? . #t) options))
         (children (multijob-children mj))
         (n        (span-length children)))
    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (let ((child (span-ref children i)))
        (when (sh-job? child)
          (multijob-current-child-index-set! mj i)
          (if (eq? '& (sh-multijob-child-ref mj (fx1+ i)))
            (job-start 'sh-list child options)
            (loop-start-resume-child-with-suspend 'sh-list mj options child)))))))
