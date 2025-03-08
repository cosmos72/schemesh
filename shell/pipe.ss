;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; return #t if job is a pipe multijob, otherwise return #f
(define (sh-multijob-pipe? job)
  (and (sh-multijob? job) (memq (multijob-kind job) '(sh-pipe sh-pipe*)) #t))


;; Create a pipe multijob to later start it. Each element in children-jobs must be a sh-job or subtype.
(define (sh-pipe . children-jobs)
  (make-multijob 'sh-pipe assert-is-job-or-pipe-symbol mj-pipe-start #f
    (validate-convert-pipe-args children-jobs)))


;; Create a pipe multijob to later start it.
;; Odd elements in children-jobs-with-pipe must be sh-job or subtype.
;; Even elements in children-jobs-with-pipe must be one of the symbols '\ '|&
(define (sh-pipe* . children-jobs-with-pipe)
  (let ((kind (validate-pipe*-args children-jobs-with-pipe)))
    (make-multijob kind
      assert-is-job-or-pipe-symbol
      mj-pipe-start
      #f
      children-jobs-with-pipe)))


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
        (%again (cdr tail) (cons '\x7C; (cons arg ret)))))))


;; check that args is an alternating list of jobs and symbols '| '|&
;; return 'sh-pipe* if some '|& symbols are present, otherwise return 'sh-pipe
(define (validate-pipe*-args args)
  (let ((i 0)
        (kind 'sh-pipe))
    (list-iterate args
      (lambda (arg)
        (assert-is-job-or-pipe-symbol 'sh-pipe* arg)
        (if (fxeven? i)
          (unless (sh-job? arg)
            (raise-errorf 'sh-pipe* "even-indexed arguments must be sh-job or subtype, found instead ~s" arg))
          (case arg
            ((\x7C;
               ) (void))
            ((\x7C;&
               ) (set! kind 'sh-pipe*))
            (else
              (raise-errorf 'sh-pipe* "odd-indexed arguments must be a pipe symbol '| '|& found instead ~s" arg))))
        (set! i (fx1+ i))))
    kind))


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


    ; Apply redirections. Will be removed by (mj-pipe-advance-wait) when job finishes.
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

    ; (debugf "... mj-pipe-start-i starting job=~s, options=~s, redirect-in=~s, redirect-out=~s" job options redirect-in? redirect-out?)

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


;; Internal function called by (job-wait) called by (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
(define (mj-pipe-advance caller mj wait-flags)
  ;; (debugf "->  mj-pipe-advance\tcaller=~s\tjob=~a\twait-flags=~s\tstatus=~s" caller mj wait-flags (job-last-status mj))
  (let ((pgid (job-pgid mj)))
    ;; we must run children in foreground,
    ;; otherwise they will not receive SIGTSTP when user types CTRL+Z
    (with-foreground-pgid wait-flags pgid
      ;; (debugf "mj-pipe-advance before sigcont job=~s\tstatus=~s" mj (job-last-status mj))
      (mj-pipe-advance-sigcont        mj wait-flags pgid)
      (job-status-set/running! mj)
      ;; (debugf "mj-pipe-advance after  sigcont job=~s\tstatus=~s" mj (job-last-status mj))
      (call/cc
        (lambda (k)
          (let ((k-stop
                  (lambda (status)
                    (mj-pipe-advance-stop mj status)
                    (k status))))
            (mj-pipe-advance-wait caller mj wait-flags k-stop))))))
  ;; (debugf "<-  mj-pipe-advance\tcaller=~s\tjob=~a\twait-flags=~s\tjob-status=~s" caller (job-last-status mj) wait-flags (job-last-status mj))
  )


(define (mj-pipe-advance-sigcont mj wait-flags pgid)
  ;; send SIGCONT to job's process group, if present.
  ;; (debugf "mj-pipe-advance-sigcont job=~s\twait-flags=~s" mj wait-flags)
  (when (and pgid (sh-wait-flag-continue-if-stopped? wait-flags))
    ;;
    ;; if SIGCHLD arrives late, i.e. after we later resume mj children jobs,
    ;; any child sh-expr that is already resumed may receive an EINTR error
    ;; from system calls such as read() invoked by (fd-read),
    ;; which triggers a call to (yield).
    ;;
    ;; Luckily, (yield) is sufficiently robust and will detect that no subprocess was stopped,
    ;; deducing that it does not need to suspend the sh-expr.
    ;;
    ;; Thus we do not need to pause() until SIGCHLD arrives, which is inherently racy.
    (pid-kill (- pgid) 'sigcont))) ; (if (job-stopped? mj) 'pause #f)


(define (mj-pipe-wait-flags-for-sh-expr wait-flags)
  (if (sh-wait-flag-wait-until-finished? wait-flags)
    (sh-wait-flags-remove
      wait-until-finished
      (sh-wait-flags-add wait-until-stopped-or-finished
                         wait-flags))
    wait-flags))


(define (mj-pipe-advance-stop mj status)
  (mj-pipe-signal-sigtstp mj)
  ;x (debugf "... mj-pipe-advance-stop status=~s" status)
  (job-status-set! 'mj-pipe-advance-wait-stop mj status))



;; returns updated job status
(define (mj-pipe-advance-wait caller mj wait-flags k-stop)
  ; (debugf ">   mj-pipe-advance-wait wait-flags=~s mj=~s" mj wait-flags)

  ;; cannot use a local variable mj-stop and set/test it,
  ;; because it would be preserved across continuations
  ;; called by resuming sh-expr children jobs
  ;; => if we need to suspend this sh-pipe, call the continuation (k-stop)
  (let* ((children  (multijob-children mj))
         (n         (span-length children)))

    ;; if last child is a sh-expr, call (job-wait wait-flags ...) on it:
    ;; it's the only child possibly running in main process,
    ;; and sh-exprs need to be explicitly continued.
    (let ((job (sh-multijob-child-ref mj (fx1- n))))
      ;; (debugf "->  mj-pipe-advance/w\tcaller=~s\tlast-job=~s\twait-flags=~s\tlast-job-status=~s\tsh-expr?=~s" caller job wait-flags (job-last-status job) (sh-expr? job))
      (when (and (sh-expr? job) (job-started? job))
        ;; cannot wait until sh-expr finishes: if user presses CTRL+Z,
        ;; both the sh-expr and the other foreground jobs get stopped,
        ;; and before waiting again for sh-expr we must send SIGCONT to the other foreground jobs
        (let* ((expr-wait-flags (mj-pipe-wait-flags-for-sh-expr wait-flags))
               (status          (job-wait 'mj-pipe-advance-wait-expr job expr-wait-flags)))
           ;x (debugf "... mj-pipe-advance-wait-expr\tcaller=~s\twait-flags=~s\tlast-job=~s\tlast-job-status=~s" caller wait-flags job status)
           (when (stopped? status)
             ;x (debugf "... mj-pipe-advance-wait **STOP** expr=~s\tstatus=~s" job status)
             (k-stop status)))))

    ;; call (job-wait wait-flags ...) on each child job,
    ;; skipping the ones that already finished.
    (let %again ((i (fxmax 0 (multijob-current-child-index mj))))
      (multijob-current-child-index-set! mj i)
      (let ((job (sh-multijob-child-ref mj i)))
        (cond
          ((fx>=? i n)
            (void)) ; stop iterating
          ((sh-job? job)
            (let ((status (job-wait 'mj-pipe-advance-wait job wait-flags)))
              ;x (debugf "... mj-pipe-advance-wait\tcaller=~s\twait-flags=~s\tchild-job=~s\tchild-status=~s" caller wait-flags job status)
              (case (status->kind status)
                ((ok exception failed killed)
                  (%again (fx1+ i)))
                ((stopped) ; stop iterating
                  ;x (debugf "... mj-pipe-advance-wait **STOP** child=~s\tstatus=~s" job status)
                  (k-stop status))
                ))) ; else stop iterating
          (else
            (%again (fx1+ i))))))

    (cond
      ((fx<? (multijob-current-child-index mj) n)
        ;; some child is still running
        ;x (debugf "... mj-pipe-advance-wait-running")
        (job-status-set/running! mj))
      (else
        (let ((status (if (span-empty? children)
                        (ok)
                        (job-last-status (span-ref-right children)))))
          ;x (debugf "... mj-pipe-advance-wait-finish status=~s" status)
          (job-status-set! 'mj-pipe-advance-wait mj status))))))


(define (mj-pipe-signal-sigtstp mj)
  (let ((pgid (job-pgid mj)))
    (when pgid
      (pid-kill (- pgid) 'sigtstp)))
  (job-status-set! 'mj-pipe-signal-sigtstp mj (stopped 'sigtstp)))
