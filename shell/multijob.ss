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
  (make-multijob 'sh-and assert-is-job mj-and-start children-jobs))


;; Create an "or" multijob
(define (sh-or . children-jobs)
  (make-multijob 'sh-or  assert-is-job mj-or-start children-jobs))


;; Create a "not" multijob
(define (sh-not child-job)
  (make-multijob 'sh-not assert-is-job mj-not-start (list child-job)))


(define (assert-is-job who job)
  (assert* who (sh-job? job)))


;; Create a "list" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (make-multijob
    'sh-list
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    mj-list-start
    children-jobs-with-colon-ampersand))


;; Create a "subshell" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (make-multijob
    'sh-subshell
    (lambda (caller job) ; validate-job-proc
      (unless (job-terminator? job)
        (assert* caller (sh-job? job))))
    mj-subshell-start
    children-jobs-with-colon-ampersand))


;; Return #t if token is a shell job terminator: ; &
(define (job-terminator? token)
  (and (symbol? token)
       (or (eq? token '&) (eq? token '\x3B;))))




;; Create a multijob to later start it.
;; Internal function, accepts an optional function to validate each element in children-jobs
(define (make-multijob kind validate-job-proc start-proc children-jobs)
  (assert* 'make-multijob (symbol? kind))
  (assert* 'make-multijob (procedure? start-proc))
  (when validate-job-proc
    (do ((tail children-jobs (cdr tail)))
        ((null? tail))
      (validate-job-proc kind (car tail))))
  (let ((mj
    (%make-multijob
      #f #f #f #f     ; id oid pid pgid
      '(new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      start-proc #f   ; start-proc resume-proc
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      #f              ; no temp parent.
      (or (sh-current-job) (sh-globals)) ; default parent job
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
(define (mj-list-start job options)
  (assert* 'sh-list (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-list (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ; Do not yet assign a job-id.
      (mj-list-loop job options))))


;; internal function stored in (job-start-proc job) by (sh-subshell) multijobs
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (mj-list-loop job options)
;;
;; Options are the same as described in (sh-start).
;; Option '(spawn? . #t) is enabled by default, because this function always spawns a subprocess.
(define (mj-subshell-start job options)
  (assert* 'sh-subshell (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-subshell (fx=? -1 (multijob-current-child-index job)))
  (spawn-procedure job options
    (lambda (job options)
      ;; this will be executed in a subprocess.

      ;; do not save history when subshell exits.
      (let ((lctx (repl-args-linectx)))
        (when (linectx? lctx)
          (let ((history (linectx-history lctx)))
            (when history
              (charhistory-path-set! history #f)))))

      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)

      ;; pretend that running a sh-subshell is equivalent to running an sh-list in a subprocess.
      ;; actually, that's quite accurate
      (mj-list-loop job options))))




;; Internal function stored in (job-start-proc job) by (sh-and),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-and-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-and (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-and (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure
    job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      (mj-and-loop job options))))




;; Internal function stored in (job-start-proc job) by (sh-or),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-or-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-or (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-or (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; (debugf "mj-or-start ~s empty children? = ~s" job (span-empty? (multijob-children job)))
      ;; Do not yet assign a job-id.
      (mj-or-loop job options))))


;; Internal function stored in (job-start-proc job) by (sh-not),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors.
(define (mj-not-start job options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-not (eq? 'running (job-last-status->kind job)))
  (assert* 'sh-not (fx=? -1 (multijob-current-child-index job)))
  (call-or-spawn-procedure job options
    (lambda (job options)
      (job-remap-fds! job)
      (job-env/apply-lazy! job 'export)
      ;; Do not yet assign a job-id.
      (mj-not-loop job options))))


;; called in subprocess by (spawn-procedure)
;;
;; returns job status
(define (spawn-procedure/subprocess job options proc)
  ;; cannot use (dynamic-wind) here, because (sh-wait) non-locally jumps to job's continuation
  ;;
  ;; deactivate job control in subprocess:
  ;; a. do not create process groups => all child processes will
  ;;    inherit process group from the subprocess itself
  ;; b. do not change the foregroud process group
  ;;
  ;; note that code executed by the subprocess CAN reactivate job control,
  ;; and in such case it will suspend itself until someone moves it to the foreground.
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
    (%job-pgid-set! job #f))

  ;; warning: do not call (job-status-set! job ...)
  ;; because it detects that job is running, and assigns a job-id to it,
  ;; which is only annoying - cannot do anything useful with such job-id.
  (%job-last-status-set! job '(running))

  ;c (debugf "-> spawn-procedure job=~a subprocess calling proc ~s" (sh-job->string job) proc)

  ;; if proc attempts to suspend or yield job,
  ;; call (sh-wait job) below for resuming it until it finishes.
  (call/cc
    (lambda (yield)
      (override-yield-proc yield)
      ;; ignore value returned by (proc)
      (proc job options)))
        ;; (debugf ". spawn-procedure job=~a subprocess proc returned" (sh-job->string job))

  ;; cleanup parameters before calling (sh-wait)
  (override-yield-proc #f)
  (sh-current-job #f)
  ;;f (debugf "... spawn-procedure job=~a waiting for it to finish, pid=~s status=~s" (sh-job->string job) (job-pid job) (job-last-status job))
  (let ((status (sh-wait job)))
    ;;f (debugf "<-  spawn-procedure job=~a subprocess exiting with pid=~s status=~s" (sh-job->string job) (job-pid job) status)
    status))



;; Fork a new subprocess, and in the child subprocess
;; call (proc job options) once, then call (sh-wait job) repeatedly - which calls (job-resume-proc job) -
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

      (let* ((options          (options-filter-out        options '(spawn?)))
             (process-group-id (options->process-group-id options))
             (_                (options->set-temp-parent! job options))
             (ret              (c-fork-pid (or process-group-id -1))))

        (cond
          ((< ret 0) ; fork() failed
            (raise-c-errno 'sh-start 'fork ret))
          ((= ret 0) ; child
            ;; cannot use (dynamic-wind) here, because (spawn-procedure/subprocess)
            ;; calls (sh-wait) which non-locally jumps to job's continuation
            (with-exception-handler
              (lambda (ex)
                (exit-with-job-status (cons 'exception ex)))
              (lambda ()
                (exit-with-job-status
                  (spawn-procedure/subprocess job options proc)))))
          ((> ret 0) ; parent
            (job-pid-set! job ret)
            (job-pgid-set! job process-group-id)
            ;; we can cleanup job's file descriptor, as it's running in a subprocess
            (job-unmap-fds! job)
            (job-unredirect/temp/all! job)
            '(running)))))))


;; if options contain '(spawn? . #t) then remove such options and call (spawn-procedure job options proc)
;; otherwise directly call (proc job options)
;;
;; WARNING (proc job options) must call (sh-job-status-set! job), because the return value of (proc ...) is ignored
(define (call-or-spawn-procedure job options proc)
  ;c (debugf "call-or-spawn-procedure options=~s proc=~s job=~a" options proc (sh-job->string job))
  (if (options->spawn? options)
    ;; spawn a subprocess and run (%proc... job) inside it
    (spawn-procedure job options proc)
    ;; directly call (proc job options) in the caller's process
    (let ((options (options->set-temp-parent! job options)))
      (proc job options))))



(define options-catch '((catch? . #t)))


;; start a child job and resume it until it finishes,
;; yielding or suspending parent job as appropriate.
;;
;; returns child job exit status
(define (mj-child-loop-with-yield caller mj options child-i)
  (multijob-current-child-index-set! mj child-i)
  (let ((child (span-ref (multijob-children mj) child-i)))
    ;;x (debugf "->  mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a" caller (sh-job->string mj) (sh-job->string child))
    (let %loop ()
      (case (job-last-status->kind mj)
        ((running)
          (let ((status (job-last-status child)))
            ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\tchild-status=~s" caller (sh-job->string mj) (sh-job->string child) status)
            (case (sh-status->kind status)
              ((new)
                (job-start caller child options)
                ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- child started with status=~s" caller (sh-job->string mj) (sh-job->string child) (job-last-status child))
                (%loop))
              ((running)
                ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- calling yield" caller (sh-job->string mj) (sh-job->string child))
                (job-yield mj (list caller 'mj-child-loop-with-yield 'running))
                ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- yield returned, looping" caller (sh-job->string mj) (sh-job->string child))
                (%loop))
              ((stopped)
                ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- calling suspend" caller (sh-job->string mj) (sh-job->string child))
                (job-suspend mj)
                ;;x (debugf "... mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- suspend returned, looping" caller (sh-job->string mj) (sh-job->string child))
                (%loop))
              (else
                ;; child has finished, return its status
                ;;x (debugf "<-  mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- returning, child-status=~s" caller (sh-job->string mj) (sh-job->string child) status)
                status))))
        ((stopped)
          ;;x (debugf "<-  mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- calling suspend, parent-status=~s\t" caller (sh-job->string mj) (sh-job->string child) (job-last-status mj))
          (job-suspend mj)
          (%loop))
        (else
          ;;x (debugf "<-  mj-child-loop-with-yield\tcaller=~s\tmj=~a\tchild=~a\t--- returning, parent-status=~s\tchild-status=~s" caller (sh-job->string mj) (sh-job->string child) (job-last-status mj) (job-last-status child))
          (job-last-status child))))))


;; Run the children jobs in an "and" multijob .
;; Used by (sh-and), implements runtime behavior of shell syntax {foo && bar && baz}
;; NEVER returns normally, only yields
(define (mj-and-loop mj options)
  (forever
    ;;x (debugf "... mj-and-loop mj=~a\tstatus=~s" (sh-job->string mj) (job-last-status mj))
    (case (job-last-status->kind mj)
      ((running)
        (let ((options       (cons '(catch? . #t) options))
              (child-status '(failed 1)))
          (span-iterate (multijob-children mj)
            (lambda (i child)
              (set! child-status (mj-child-loop-with-yield 'mj-and-loop mj options i))
              (check 'mj-and-loop (sh-finished? child-status))
              (sh-ok? child-status)))
          (job-status-set! 'mj-and-loop mj child-status)))
      ((stopped)
        (job-suspend mj))
      (else
        (job-finish mj)))))



;; Run the children jobs in an "or" multijob.
;; Used by (sh-or), implements runtime behavior of shell syntax {foo || bar || baz}
;; NEVER returns normally, only yields
(define (mj-or-loop mj options)
  (forever
    ;;x (debugf "... mj-or-loop  mj=~a\tstatus=~s" (sh-job->string mj) (job-last-status mj))
    (case (job-last-status->kind mj)
      ((running)
        (let ((options       (cons '(catch? . #t) options))
              (child-status '(failed 1)))
          (span-iterate (multijob-children mj)
            (lambda (i child)
              (set! child-status (mj-child-loop-with-yield 'mj-or-loop mj options i))
              (check 'mj-or-loop (sh-finished? child-status))
              (sh-err? child-status)))
          (job-status-set! 'mj-or-loop mj child-status)))
      ((stopped)
        (job-suspend mj))
      (else
        (job-finish mj)))))



;; Run the children jobs in a "not" multijob.
;; Used by (sh-not), implements runtime behavior of shell syntax {! foo}
;; NEVER returns normally, only yields
(define (mj-not-loop mj options)
  (forever
    (case (job-last-status->kind mj)
      ((running)
        (let ((options  (cons '(catch? . #t) options))
              (children (multijob-children mj)))
          (assert* 'mj-not-loop (fx=? 1 (span-length children)))
          (let ((child-status (mj-child-loop-with-yield 'mj-not-loop mj options 0)))
            (check 'mj-not-loop (sh-finished? child-status))
            (job-status-set! 'mj-not-loop mj
              (if (sh-ok? child-status)
                '(failed 1)
                (void))))))
      ((stopped)
        (job-suspend mj))
      (else
        (job-finish mj)))))



;; Run the children jobs in a multijob list.
;; Used by (sh-list), implements runtime behavior of shell syntax {foo; bar & baz}
;; each child job can be optionally followed by '& or '; or some other symbol.
;;
;; Also used by (sh-pipe), implements runtime behavior of shell syntax {foo | bar | baz}
;;
;; NEVER returns normally, only yields
(define (mj-list-loop mj options)
  (forever
    (case (job-last-status->kind mj)
      ((running)
        (let ((options      (cons '(catch? . #t) options))
              (child-status (void)))
          (span-iterate (multijob-children mj)
            (lambda (i child)
              (when (sh-job? child)
                (if (eq? '& (sh-multijob-child-ref mj (fx1+ i)))
                  (when (job-new? child)
                    (unless (sh-finished? (job-start 'sh-list child options))
                      (job-id-set! child)))
                  (begin
                    (set! child-status (mj-child-loop-with-yield 'mj-list-loop mj options i))
                    (check 'mj-list-loop (sh-finished? child-status)))))
              #t))
          (job-status-set! 'mj-or-loop mj child-status)))
      ((stopped)
        (job-suspend mj))
      (else
        (job-finish mj)))))
