;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition



;; Create a pipe multijob to later start it. Each element in children-jobs must be a sh-job or subtype.
(define (sh-pipe . children-jobs)
  (make-multijob 'sh-pipe assert-is-job job-start/pipe #f children-jobs))


;; Create a pipe multijob to later start it.
;; Each element in children-jobs-with-pipe be a sh-job (or subtype) or the symbols '\ '|&
(define (sh-pipe* . children-jobs-with-pipe)
  (validate-pipe*-args children-jobs-with-pipe)
  (make-multijob 'sh-pipe
    assert-is-job-or-pipe-symbol
    job-start/pipe
    #f
    children-jobs-with-pipe))


(define (validate-pipe*-args args)
  (let ((prev-arg #f))
    (list-iterate args
      (lambda (arg)
        (assert-is-job-or-pipe-symbol 'sh-pipe* arg)
        (when (symbol? arg)
          (unless prev-arg
            (raise-errorf 'sh-pipe* "missing job before operator ~a" arg))
          (when (symbol? prev-arg)
            (raise-errorf 'sh-pipe* "missing job between consecutive operators ~a ~a" prev-arg arg)))
        (set! prev-arg arg)))
    (when (symbol? prev-arg)
      (raise-errorf 'sh-pipe* "missing job after operator ~a" prev-arg))))


(define (assert-is-job-or-pipe-symbol who arg)
  (unless (or (pipe-sym? arg) (sh-job? arg))
    (raise-errorf who "~s is not a sh-job or a pipe symbol '| '|&" arg)))


;; Internal function stored in (job-start-proc job) by (sh-pipe) and (sh-pipe*),
;; and called by (sh-start) to actually start a pipe multijob.
;;
;; Does not redirect file descriptors.
(define (job-start/pipe mj options)
  ;; this runs in the main process, not in a subprocess.
  (assert* 'sh-pipe (eq? 'running (job-last-status->kind mj)))
  (assert* 'sh-pipe (fx=? -1 (multijob-current-child-index mj)))
  (job-remap-fds! mj)
  (job-env/apply-lazy! mj)
  ; Do not yet assign a job-id.
  (let ((pgid (job-start-options->process-group-id options))
        (n    (span-length (multijob-children mj)))
        (pipe-fd -1))
    (when (> pgid 0)
      (job-pgid-set! mj pgid))

    (do ((i 0 (fx1+ i)))
        ((fx>=? i n))
      (when (sh-job? (sh-multijob-child-ref mj i))
        (set! pipe-fd (job-start/pipe-i mj i n pipe-fd))))
  (multijob-current-child-index-set! mj 0)))


;; Start i-th child job of a pipe multijob.
;; Argument in-pipe-fd is the pipe fd job must read from, or -1 to read from stdin.
;;
;; Return the i-th child job output pipe fd,
;; or -1 if this is the last job and its output should not be redirected to a pipe.
(define (job-start/pipe-i mj i n in-pipe-fd)
  (let* ((job (sh-multijob-child-ref mj i))
         (out-pipe-fd/read -1)
         (out-pipe-fd/write -1)
         (pgid          (job-pgid mj)) ; < 0 if not set
         (options       (if (< pgid 0) '() (list pgid)))
         (redirect-in?  (fx>=? in-pipe-fd 0))
         (redirect-out? (fx<? i (fx1- n)))
         (redirect-err? (and redirect-out?
                             (eq? '\x7c;& (sh-multijob-child-ref mj (fx1+ i))))))

    ; Apply redirections. Will be removed by job-advance/pipe/wait) when job finishes.
    (when redirect-in?
      ; we must redirect job fd 0 *before* any redirection configured in the job itself
      (job-redirect/front/fd! job 0 '<& in-pipe-fd))
    (when redirect-out?
      (let-values (((fd/read fd/write) (open-pipe-fds #t #t)))
        (set! out-pipe-fd/read  fd/read)
        (set! out-pipe-fd/write fd/write)
        ; we must redirect job's fd 1 *before* any redirection configured in the job itself
        (job-redirect/front/fd! job 1 '>& fd/write)
        (when redirect-err?
          ; we must redirect job's fd 2 *before* any redirection configured in the job itself
          (job-redirect/front/fd! job 2 '>& fd/write))))

    ; (debugf "job-start/pipe-i starting job=~s, options=~s" job options)

    ; Do not yet assign a job-id. Reuse mj process group id
    (start/any job options)

    ; set mj process group id for reuse by all other children
    (when (< pgid 0)
      (job-pgid-set! mj (job-pgid job)))

    ; Close pipes after starting job.
    ; Either the job has one or more remapped-fd pointing to a dup2() of them,
    ; or the job is a child process owning a copy of a dup2() of them.
    ; In both cases, the original pipes are no longer needed.
    (when (fx>=? in-pipe-fd 0)
      (fd-close in-pipe-fd))
    (when (fx>=? out-pipe-fd/write 0)
      (fd-close out-pipe-fd/write))


    out-pipe-fd/read))


;; Internal function called by (job-advance) called by (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;;
;; mode must be one of: sh-fg sh-bg sh-wait sh-sigcont+wait sh-subshell sh-job-status
(define (job-advance/pipe mode mj)
  ; (debugf ">   job-advance/pipe mode=~s mj=~s" mode mj)
  (let ((pgid (job-pgid mj)))
    (if (and (> pgid 0) (memq mode '(sh-fg sh-wait sh-sigcont+wait sh-subshell)))
      (with-foreground-pgid mode (job-pgid (sh-globals)) pgid
        (job-advance/pipe/maybe-sigcont mode mj pgid)
        (job-advance/pipe/wait mode mj))
      (begin
        (job-advance/pipe/maybe-sigcont mode mj pgid)
        (job-advance/pipe/wait mode mj))))
  ; (debugf "< job-advance/pipe job-status=~s" (job-last-status mj))
  )


(define (job-advance/pipe/maybe-sigcont mode mj pgid)
  ; send SIGCONT to job's process group, if present.
  ; It may raise error.
  (when (and (> pgid 0) (memq mode '(sh-fg sh-bg sh-sigcont+wait)))
    ; (debugf "job-advance/pipe/sigcont > ~s ~s" mode mj)
    (pid-kill (- pgid) 'sigcont)))


(define (job-advance/pipe/wait mode mj)
  ; (debugf ">   job-advance/pipe/wait mode=~s mj=~s" mode mj)
  (let* ((children  (multijob-children mj))
         (n         (span-length children))
         (running-i (multijob-current-child-index mj)))
    ; call (job-advance mode ...) on each child job,
    ; skipping the ones that already finished.
    (let %again ((i running-i))
      (let ((job (sh-multijob-child-ref mj i)))
        (when job
          (when (or (symbol? job) (job-status-finished? (job-advance mode job)))
            (set! running-i (fx1+ i))
            (%again (fx1+ i))))))
    (cond
      ((fx<? running-i n)
        (multijob-current-child-index-set! mj running-i))
      (#t
        (multijob-current-child-index-set! mj -1)
        (job-pgid-set! mj -1)
        (when (job-status-finished?
                (job-status-set! mj
                  (if (span-empty? children)
                    (void)
                    (job-last-status (span-back children)))))
          (job-advance/pipe/remove-children-redirections mj))))))


;; WARNING: fragile, assumes user did not modify children jobs redirections
;; while the jobs where running.
(define (job-advance/pipe/remove-children-redirections mj)
  (let* ((children  (multijob-children mj))
         (n         (span-length children)))
    (span-iterate children
      (lambda (i job)
        (when (sh-job? job)
          (let* ((redirect-in?  (fx>? i 0))
                 (redirect-out? (fx<? i (fx1- n)))
                 (redirect-err? (and redirect-out?
                                     (eq? '\x7c;& (sh-multijob-child-ref mj (fx1+ i))))))

            (when redirect-err?
              (job-unredirect/front! job))
            (when redirect-out?
              (job-unredirect/front! job))
            (when redirect-in?
              (job-unredirect/front! job))))))))
