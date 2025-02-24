;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss



;; Create a pipe multijob to later start it. Each element in children-jobs must be a sh-job or subtype.
(define (sh-pipe . children-jobs)
  (make-multijob 'sh-pipe assert-is-job-or-pipe-symbol mj-pipe-start
    (validate-convert-pipe-args children-jobs)))


;; Create a pipe multijob to later start it.
;; Odd elements in children-jobs-with-pipe must be sh-job or subtype.
;; Even elements in children-jobs-with-pipe must be one of the symbols '\ '|&
(define (sh-pipe* . children-jobs-with-pipe)
  (validate-pipe*-args children-jobs-with-pipe)
  (make-multijob 'sh-pipe assert-is-job-or-pipe-symbol mj-pipe-start children-jobs-with-pipe))


;; check that args is a list of jobs, and insert a '| between each pair of jobs
(define (validate-convert-pipe-args args)
  (let %again ((tail args)
               (ret '()))
    (if (null? tail)
      (if (null? ret)
        ret
        (reverse! (cdr ret))) ; remove last extra '|
      (let ((arg (car tail)))
        (assert-is-job 'sh-pipe arg)
        (%again (cdr args) (cons '\x7C; (cons arg ret)))))))


;; check that args is an alternating list of jobs and symbols '| '|&
(define (validate-pipe*-args args)
  (let ((i 0))
    (list-iterate args
      (lambda (arg)
        (assert-is-job-or-pipe-symbol 'sh-pipe* arg)
        (if (fxeven? i)
          (unless (sh-job? arg)
            (raise-errorf 'sh-pipe* "even-indexed arguments must be sh-job or subtype, found instead ~s" arg))
          (unless (pipe-sym? arg)
            (raise-errorf 'sh-pipe* "odd-indexed arguments must pipe symbol '| or '|& found instead ~s" arg)))
        (set! i (fx1+ i))))))


(define (assert-is-job-or-pipe-symbol who arg)
  (unless (or (pipe-sym? arg) (sh-job? arg))
    (raise-errorf who "~s is not a sh-job or a pipe symbol '| '|&" arg)))


;; Internal function stored in (job-start-proc job) by (sh-pipe) and (sh-pipe*),
;; and called by (sh-start) to actually start a pipe multijob.
;;
;; Does not redirect file descriptors.
(define (mj-pipe-start mj options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-pipe (eq? 'running (job-last-status->kind mj)))
  (assert* 'sh-pipe (fx=? -1 (multijob-current-child-index mj)))
  (job-remap-fds! mj)
  (job-env/apply-lazy! mj 'export)
  ; Do not yet assign a job-id.
  (let ((pgid (options->process-group-id options))
        (n    (span-length (multijob-children mj)))
        (pipe-fd -1))
    (job-pgid-set! mj pgid)

    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (when (sh-job? (sh-multijob-child-ref mj i))
        (set! pipe-fd (mj-pipe-start-i mj i n pipe-fd))))
  (multijob-current-child-index-set! mj 0)))


;; Start i-th child job of a pipe multijob.
;; Argument in-pipe-fd is the pipe fd job must read from, or -1 to read from stdin.
;;
;; Return the i-th child job output pipe fd,
;; or -1 if this is the last job and its output should not be redirected to a pipe.
(define (mj-pipe-start-i mj i n in-pipe-fd)
  (let* ((job           (sh-multijob-child-ref mj i))
         (out-pipe-fd/read -1)
         (out-pipe-fd/write -1)
         (pgid          (job-pgid mj)) ; #f if not set
         (redirect-in?  (fx>=? in-pipe-fd 0))
         (redirect-out? (fx<? i (fx1- n)))
         (redirect-err? (and redirect-out?
                             (eq? '\x7C;& (sh-multijob-child-ref mj (fx1+ i)))))
         ; optimization: no need to run the last job in a subprocess
         ; TO DO: investigate wrong exit value if spawn? is unconditionally #t
         (spawn?   redirect-out?)
         (options  (sh-options
                     (and spawn? '(spawn? . #t))
                     (and pgid   (cons 'process-group-id pgid))
                     '(catch? . #t))))


    ; Apply redirections. Will be removed by (mj-pipe-continue/maybe-wait) when job finishes.
    (when redirect-in?
      ; we must redirect job fd 0 *before* any redirection configured in the job itself
      (job-redirect/temp/fd! job 0 '<& in-pipe-fd))
    (when redirect-out?
      (let-values (((fd/read fd/write) (open-pipe-fds #t #t)))
        (set! out-pipe-fd/read  fd/read)
        (set! out-pipe-fd/write fd/write)
        ; we must redirect job's fd 1 *before* any redirection configured in the job itself
        (job-redirect/temp/fd! job 1 '>& fd/write)
        (when redirect-err?
          ; we must redirect job's fd 2 *before* any redirection configured in the job itself
          (job-redirect/temp/fd! job 2 '>& fd/write))))

    ; (debugf "... mj-pipe-start-i starting job=~a, options=~s, redirect-in=~s, redirect-out=~s" (sh-job->string job) options redirect-in? redirect-out?)

    ; Do not yet assign a job-id. Reuse mj process group id
    (job-start 'sh-pipe job options)

    ; if not present yet, set mj process group id for reuse by all other children
    (unless pgid
      (job-pgid-set! mj (job-pgid job)))

    ; Close pipes after starting job.
    ; Either the job has one or more remapped-fd pointing to a dup2() of them,
    ; or the job is a child process owning a copy of a dup2() of them.
    ; In both cases, the original pipes are no longer needed.
    (when (fx>=? in-pipe-fd 0)
      (fd-close in-pipe-fd))
    (when (fx>=? out-pipe-fd/write 0)
      (fd-close out-pipe-fd/write))

    (when spawn?
      ; we can cleanup job's file descriptor, as it's running in a subprocess
      (job-unmap-fds! job)
      (job-unredirect/temp/all! job))


    out-pipe-fd/read))


;; Internal function called by (job-resume) called by (sh-wait) (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; for waiting on children jobs of a sh-pipe.
;;
;; returns unspecified value.
(define (mj-pipe-continue caller mj wait-flags)
  (if (span-empty? (multijob-children mj))
    (job-status-set! caller mj (void))
    (let ((pgid (job-pgid mj)))
      (with-foreground-pgid wait-flags pgid
        (pid-resume/maybe-sigcont          caller mj wait-flags #f pgid)
        (mj-pipe-continue/maybe-wait caller mj wait-flags)))))



;; Internal function called by (job-resume) called by (sh-wait) (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; for waiting on children jobs of a sh-pipe.
;;
;; returns unspecified value.
(define (mj-pipe-continue/maybe-wait caller mj wait-flags)
  ; (debugf "->   mj-pipe-continue/maybe-wait wait-flags=~s mj=~s" mj wait-flags)
  (let* ((children  (multijob-children mj))
         (n         (span-length children)))

    ; call (sh-wait wait-flags ...) on each child job,
    ; skipping the ones that already finished.
    (let %again ((i (multijob-current-child-index mj)))
      (let ((job (sh-multijob-child-ref mj i)))
        (cond
          ((not job)
            (void))
          ((symbol? job)
            (%again (fx1+ i)))
          (else
            (multijob-current-child-index-set! mj i)
            (when (sh-finished? (sh-wait job wait-flags))
              (%again (fx1+ i)))))))


    (let ((child (sh-multijob-child-ref mj (multijob-current-child-index mj))))
      (assert* 'mj-pipe-continue/maybe-wait (sh-job? child))
      (let* ((status (job-last-status child))
             (kind   (sh-status->kind status)))
        (case kind
          ((running)
            ;; a child is still running => set multijob status to running too.
            (job-status-set-running! 'mj-pipe-continue/maybe-wait mj)

            ;; if wait-flags tell to wait until job stops or finishes, then wait for child.
            ;; otherwise return.
            (when (sh-wait-flag-wait? wait-flags)
               (mj-pipe-continue/maybe-wait caller mj wait-flags)))

          ((stopped)
            ;; a child is stopped.
            ;; if wait-flags tell to wait until job finishes, then wait for child.
            ;; otherwise set multijob status to stopped and return.
            (if (sh-wait-flag-wait-until-finished? wait-flags)
               (mj-pipe-continue/maybe-wait caller mj wait-flags)
               (job-status-set! 'mj-pipe-continue/maybe-wait mj status)))

          (else
            ;; all children finished
            (multijob-current-child-index-set! mj -1)
            (job-status-set! 'mj-pipe-continue/maybe-wait mj status)))))))
