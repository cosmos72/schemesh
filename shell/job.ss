;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; Define the record types "sh-job" "sh-cmd" "sh-multijob" "sh-expr" and functions operating on them.
;; Define the functions (sh-env...) and (sh-fd...)
;;
;; Convention: (sh) and (sh-...) are functions
;;             (shell) and (shell-...) are macros

(library (schemesh shell job (0 8 3))
  (export
    ;; aliases.ss
    sh-alias-ref sh-alias-delete! sh-alias-set! sh-aliases sh-aliases-expand

    ;; builtins.ss
    sh-builtins sh-builtins-help sh-find-builtin sh-exception-handler
    sh-echo sh-false sh-help repl-history repl-history-display repl-args repl-args-linectx sh-true


    ;; cmd.ss
    fork-process make-sh-cmd sh-cmd

    ;; control.ss
    sh-current-job-kill sh-current-job-suspend sh-preferred-job-id
    sh-start sh-bg sh-fg sh-kill sh-run sh-run/i sh-run/err? sh-run/ok? sh-wait

    ; sh-wait-flag-foreground-pgid? sh-wait-flag-continue-if-stopped?
    ; sh-wait-flag-wait? sh-wait-flag-wait-until-finished? sh-wait-flag-wait-until-stopped-or-finished?

    ;; dir.ss
    sh-cd sh-cd- sh-pwd sh-userhome xdg-cache-home/ xdg-config-home/

    ;; display.ss
    sh-job-display sh-job-display* sh-job->string
    sh-job-write   sh-job-write* sh-job->verbose-string
    sh-job-display-summary? sh-job-display-summary sh-job-display-summary*
    sh-job-display-style

    ;; env.ss
    sh-env-ref sh-env-set! sh-env-delete! sh-env-visibility-ref sh-env-visibility-set!
    sh-env-iterate/direct sh-env-set/lazy! sh-env-copy sh-env->argv

    ;; expr.ss
    sh-expr

    ;; job.ss
    sh-consume-signals sh-cwd
    sh-job sh-job-id sh-job-pid sh-job-pgid sh-job-status sh-jobs sh-find-job sh-job-exception

    ;; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell
    sh-globals sh-multijob-child-length sh-multijob-child-ref

    ;; options.ss
    sh-options

    ;; redirect.ss
    sh-fd sh-binary-port sh-textual-port sh-redirect!
    sh-run/bvector sh-run/string sh-run/string-rtrim-newlines sh-run/string-split-after-nuls sh-start/fd-stdout


    ;; params.ss
    sh-job-control-available? sh-job-control?

    ;; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ;; pipe.ss
    sh-pipe sh-pipe*

    ;; scheduler.ss
    sh-foreground-pgid

    ;; types.ss
    sh-cmd? sh-expr? sh-job? sh-job-copy sh-multijob? sh-current-job

    ;; wildcard
    wildcard wildcard1 wildcard* wildcard/apply wildcard/expand-tilde
    wildcard->string wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (except (rnrs)     current-input-port current-output-port current-error-port)
    (rnrs mutable-pairs)
    (only (chezscheme) append! break break-handler
                       console-input-port console-output-port console-error-port
                       current-input-port current-output-port current-error-port
                       current-time debug debug-condition debug-on-exception display-condition
                       foreign-procedure format fx1+ fx1- hashtable-cells include inspect
                       keyboard-interrupt-handler list-copy logand logbit? make-format-condition meta
                       open-fd-output-port parameterize procedure-arity-mask record-writer
                       register-signal-handler reverse! sort!
                       string-copy! string-truncate! void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix)
    (schemesh port redir)
    (schemesh port stdio)
    (only (schemesh lineedit charhistory) charhistory-path-set!)
    (only (schemesh lineedit linectx) linectx? linectx-history linectx-save-history linectx-wbuf)
    (only (schemesh lineedit lineedit) lineedit-display-table lineedit-flush lineedit-undraw)
    (schemesh shell fds)
    (schemesh shell parameters)
    (schemesh shell paths))


;; record types "job" "cmd" "multijob" and their accessors
(include "shell/types.ss")

;; functions to operate on job status
(include "shell/status.ss")



;; call (proc job) on given job and each of its parents.
;; Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc job) returned truish,
;; otherwise returns #f.
(define (job-parents-iterate job-or-id proc)
  (do ((parent (sh-job job-or-id) (job-parent parent)))
      ((not (and (sh-job? parent) (proc parent)))
       (not (sh-job? parent)))))


;; call (proc job) on given job and each of its default parents.
;; Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc job) returned truish,
;; otherwise returns #f.
(define (job-default-parents-iterate job proc)
  (let %loop ((job job))
    (and (sh-job? job)
         (proc job)
         (%loop (job-default-parent job)))))


;; call (proc job) on given job and each of its default parents.
;; Stops iterating if (proc ...) returns truish.
;;
;; Returns value of last (proc ...) call.
(define (job-default-parents-iterate-any job proc)
  (let %any ((job job))
    (and (sh-job? job)
         (or (proc job)
             (%any (job-default-parent job))))))


;; Return #t if job or one if its default parents are eq? other-job,
;; otherwise return #f
(define (job-default-parents-contain? job other-job)
  (if (job-default-parents-iterate-any job
        (lambda (parent)
          (eq? parent other-job)))
    #t
    #f))


;; return list containing all job's parents,
;; starting from (sh-globals), until job itself.
(define (job-parents-revlist job-or-id)
  (let ((jlist '()))
    (job-parents-iterate job-or-id
      (lambda (job)
        (set! jlist (cons job jlist))))
    jlist))


#|
;; unused. return list containing job followed by all its parents.
(define (job-parents-list job-or-id)
  (reverse! (job-parents-revlist job-or-id)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (job-ports-flush job)
  (let ((ports (job-ports job)))
    (when ports
      (for-hash ((fd port ports))
        (flush-output-port port)))))

(define (job-default-parents-ports-flush job)
  (job-default-parents-iterate job job-ports-flush))


;; internally used by (sh-current-job)
(define %current-job (sh-make-thread-parameter #f))


;; Thread parameter containing the current job.
;; It is truish only if called from one of the dynamic contexts listed below,
;; or from some code called directly or indirectly by them:
;;
;; * one of the procedures stored in (job-start-proc)
;; * a closure injected in a sh-job, as for example {echo (lambda () ...)}
;; * an expression inside (shell-expr ...)
;;
(define sh-current-job
  (case-lambda
    (()
      (%current-job))
    ((job)
      (when (and job (not (sh-job? job)))
        (raise-errorf 'sh-current-job "invalid current job, must be #f or a sh-job: ~s" job))
      ;; when current job changes, we must flush stdin, stdout and stderr and unset their eof flag
      ;; because the underlying port may change and their eof status may differ
      (let ((old-job (%current-job)))
        (unless (eq? job old-job)
          (sh-stdio-cleanup)
          (sh-stdio-flush))
        (when old-job
          (job-default-parents-ports-flush old-job))
        (unless (eq? job old-job)
          (%current-job job))))))


;; Parameter set to truish when inside (job-wait).
;; Needed to avoid re-entering (job-wait) from signal handlers.
(define waiting-for-job (sh-make-thread-parameter #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; set the status of a job and return it.
;; if specified status indicates that job finished,
;;   i.e. (finished? status) returns true
;;   also close the job fds that need to be closed.
(define (job-status-set! caller job new-status)
  (let* ((status new-status)
         (kind   (status->kind status)))
    ;; (debugf "job-status-set! caller=~s job=~s status=~s normalized-status=~s" caller job new-status status)
    (case kind
      ((running)
        (job-status-set/running! job))
      ((ok exception failed killed)
        (%job-last-status-set! job status)

        (flush-output-port (current-output-port))
        (flush-output-port (current-error-port))
        (flush-output-port (sh-stdout))
        (flush-output-port (sh-stderr))

        (job-pid-set!  job #f) ; also updates (sh-pid-table)
        (job-pgid-set! job #f)

        ; (debugf "job-status-set! caller=~s job=~s status=~s" caller job status)
        (job-unmap-fds! job)
        (job-unredirect/temp/all! job) ; remove temporary redirections
        (job-temp-parent-set!  job #f) ; remove temporary parent job

        (cond
          ((sh-cmd? job)
            ;; unset expanded arg-list, because next expansion may differ
            (cmd-expanded-arg-list-set! job #f))
          ((sh-expr? job)
            (jexpr-resume-proc-set!  job #f)
            (jexpr-suspend-proc-set! job #f))
          ((sh-multijob? job)
            (multijob-current-child-index-set! job -1)))
        status)
      (else
        (%job-last-status-set! job status)))))


;; set job status to (running job-id)
;; and return such status
(define (job-status-set/running! job)
  (let* ((id     (job-id job))
         (status (job-last-status job))
         (kind   (status->kind status))
         (old-id (status->value status)))
    (if (and (eq? 'running kind) (eqv? id old-id))
      status
      (let ((new-status (running id)))
        (%job-last-status-set! job new-status)
        new-status))))


;; unset the job-id of a job,
;; and remove it from (multijob-children (sh-globals)).
;; Return job status
(define (job-id-unset! job)
  (assert* 'job-id-unset! (sh-job? job))
  (let ((id (job-id job)))
    (when id
      (let* ((jobs (multijob-children (sh-globals)))
             (n    (span-length jobs)))
        (when (fx<? -1 id n)
          (span-set! jobs id #f)
          (until (or (span-empty? jobs) (span-ref-right jobs))
            (span-erase-right! jobs 1)))
      (%job-id-set! job #f)
      (job-oid-set! job id) ;; needed for later displaying it
      (queue-job-display-summary job))))
  (job-last-status job))



;; If job has no job-id, assign a job-id to it, by appending it to (multijob-children (sh-globals)).
;; If job status is (running) update it to (running job-id)
;; Return updated job status
(define (job-id-set! job)
  (assert* 'job-id-set! (sh-job? job))
  ;; the children of (sh-pipe) jobs are not supposed to be started/stopped individually:
  ;; the parent (sh-pipe) job always starts/stops all of them collectively.
  ;; thus assigning a job-id to such children usually just adds noise.
  (unless (sh-multijob-pipe? (job-default-parent job))
    (let* ((old-id (job-id job))
           (id     (or old-id (%job-id-assign! job)))
           (status (job-last-status job))
           (kind   (status->kind status)))
      (when (and (eq? kind 'running) (not (eqv? id (status->value status))))
        ;; replace job status (running) -> (running job-id)
        (job-status-set! 'job-id-set! job (running id)))
      (when (fx<? (sh-preferred-job-id) 0)
        ;; no preferred job id, set it to this job id
        (sh-preferred-job-id-set! id))
      (unless (eqv? id old-id)
        (queue-job-display-summary job))))
  ;; (debugf "job-id-set! job=~s\tid=~s" job (job-id job))
  (job-last-status job))


;; Assumes job has no job-id, and assigns a job-id to it, by appending it to (multijob-children (sh-globals)).
;; Return job-id
(define (%job-id-assign! job)
  (let* ((children (multijob-children (sh-globals)))
         (id       (span-length children)))
    (span-insert-right! children job)
    (%job-id-set! job id)
    id))


(define (job-parent job)
  (or (job-temp-parent job) (job-default-parent job)))


;; if job is running or stopped, then create a new job-id for it.
;; if job has terminated, clear its job id and close its fds.
;; Also replace any job status (running) -> (running job-id)
;; Return updated job status.
;;
;; Note: does not create job-id for children of (sh-pipe) jobs.
(define (job-id-update! job)
  (let ((status (job-last-status job)))
    ;; (debugf "job-id-update! status=~s\tjob=~a" status job)
    (case (status->kind status)
      ((running stopped)
        (job-id-set! job))
      ((ok exception failed killed)
        (job-id-unset! job))
      (else
        status))))




;; Convert job-or-id to job.
;; job-or-id can be either a job,
;; or #t which means (sh-globals),
;; or #f which means (or (sh-current-job) (sh-globals))
;; or a fixnum indicating the job-id of one of the running jobs
;;   stored in (multijob-children (sh-globals))
;;
;; Returns job object, or #f if job was not found.
(define (sh-find-job job-or-id)
  (cond
    ((eq? #t job-or-id)
      (sh-globals))
    ((not job-or-id)
      (or (sh-current-job) (sh-globals)))
    ((fixnum? job-or-id)
      (let ((all-jobs (multijob-children (sh-globals))))
        (if (fx<? 0 job-or-id (span-length all-jobs)) ; job-ids start at 1
          (span-ref all-jobs job-or-id)
           #f)))
    ((sh-job? job-or-id)
      job-or-id)
    (else
      #f)))


;; Convert job-or-id to job.
;; job-or-id can be either a job,
;; or #t which means (sh-globals),
;; or #f which means (or (sh-current-job) (sh-globals))
;; or a fixnum indicating the job-id of one of the running jobs
;;    stored in (multijob-children (sh-globals))
;;
;; Raises error if no job matches job-or-id.
(define (sh-job job-or-id)
  (cond
    ((eq? #t job-or-id)
      (sh-globals))
    ((not job-or-id)
      (or (sh-current-job) (sh-globals)))
    ((fixnum? job-or-id)
      (let* ((all-jobs (multijob-children (sh-globals)))
             (job (when (and (fx>? job-or-id 0) ; job-ids start at 1
                             (fx<? job-or-id (span-length all-jobs)))
                    (span-ref all-jobs job-or-id))))
        (unless (sh-job? job)
          (raise-errorf 'sh-job "job not found: ~s" job-or-id))
        job))
    ((sh-job? job-or-id)
      job-or-id)
    (else
      (raise-errorf 'sh-job "~s is not a job-id" job-or-id))))


;; return currently running jobs
;; as a span of pairs (job-id . job) sorted by job-id
(define (sh-jobs)
  (let ((src (multijob-children (sh-globals)))
        (dst (span)))
    (span-iterate src
      (lambda (job-id job)
        (when (sh-job? job)
          (span-insert-right! dst (cons job-id job)))))
    dst))


;; Return the exception that terminated a job-or-id.
;; Return #f if job was not terminated by an exception.
;;
;; Raises error if no job matches job-or-id.
(define (sh-job-exception job-or-id)
  (job-exception (sh-job job-or-id)))



(define (sh-consume-signals lctx)
  (check-interrupts)
  (when lctx
    (display-status-changes lctx)))


(define (display-status-changes lctx)
  (let ((job-list (queue-job-display-summary)))
    (unless (null? job-list)
      (lineedit-undraw lctx 'flush)
      (let ((port (console-output-port)))
        (for-list ((job (list-remove-consecutive-duplicates! (sort! sh-job<? job-list) eq?)))
          (display-status-change job port))
        (flush-output-port port)))))



(define (display-status-change job port)
  (let ((id  (job-id job))
        (oid (job-oid job)))
    (when (or id oid)
      (unless (or (eqv? -1 id) (eqv? -1 oid))
        (sh-job-display-summary job port))
      (job-oid-set! job #f)))) ; no longer needed, clear it




(include "shell/options.ss")
(include "shell/params.ss")
(include "shell/redirect.ss")
(include "shell/builtins.ss")
(include "shell/cmd.ss")
(include "shell/expr.ss")
(include "shell/multijob.ss")
(include "shell/env.ss")
(include "shell/dir.ss")
(include "shell/pipe.ss")
(include "shell/control.ss")
(include "shell/parse.ss")
(include "shell/scheduler.ss")
(include "shell/builtins2.ss")
(include "shell/aliases.ss")
(include "shell/wildcard.ss")
(include "shell/display.ss") ; must be next-to-last one, contains (record-writer ...)

(include "shell/init.ss")    ; must be last one, contains expressions


) ; close library
