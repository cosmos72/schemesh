;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; Define the record types "job" "cmd" "multijob" and functions operating on them.
;; Define the functions (sh-env...) and (sh-fd...)
;
;; Convention: (sh) and (sh-...) are functions
;              (shell) and (shell-...) are macros

(library (schemesh shell job (0 8 0))
  (export
    ;; aliases.ss
    sh-alias-ref sh-alias-delete! sh-alias-set! sh-aliases sh-aliases-expand

    ;; builtins.ss
    sh-builtins sh-builtins-help sh-find-builtin sh-exception-handler
    sh-echo sh-false sh-help sh-history repl-args repl-args-linectx sh-true


    ;; cmd.ss
    make-sh-cmd sh-cmd

    ;; control.ss
    sh-current-job-suspend
    sh-start sh-start* sh-bg sh-fg sh-run sh-run/i sh-run/err? sh-run/ok? sh-wait

    ; sh-wait-flag-foreground-pgid? sh-wait-flag-continue-if-stopped?
    ; sh-wait-flag-wait? sh-wait-flag-wait-until-finished? sh-wait-flag-wait-until-stopped-or-finished?

    ;; dir.ss
    sh-cd sh-cd- sh-pwd sh-userhome sh-xdg-cache-home/ sh-xdg-config-home/

    ;; display.ss
    sh-job-display sh-job-display* sh-job->string
    sh-job-write   sh-job-write* sh-job->verbose-string
    sh-job-display-summary? sh-job-display-summary sh-job-display-summary*

    ;; env.ss
    sh-env-ref sh-env-set! sh-env-delete! sh-env-visibility-ref sh-env-visibility-set!
    sh-env-iterate/direct sh-env-set/lazy! sh-env-copy sh-env->argv

    ;; expr.ss
    sh-expr

    ;; job.ss
    sh-consume-signals sh-cwd
    sh-job sh-job-id sh-job-status sh-jobs sh-find-job sh-job-exception

    ;; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell
    sh-globals sh-multijob-child-length sh-multijob-child-ref

    ;; options.ss
    sh-options

    ;; redirect.ss
    sh-fd sh-stdin sh-stdout sh-stderr sh-redirect!
    sh-run/bvector sh-run/string sh-run/string-rtrim-newlines sh-run/string-split-after-nuls sh-start/fd-stdout


    ;; params.ss
    sh-job-control-available? sh-job-control?

    ;; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ;; pipe.ss
    sh-pipe sh-pipe*

    ;; status.ss
    sh-ok? sh-new? sh-started? sh-running? sh-stopped? sh-finished?
    sh-status->kind sh-status->value sh-status->value-list

    ;; types.ss
    sh-cmd? sh-expr? sh-job? sh-job-copy sh-multijob? sh-current-job

    ;; wildcard
    sh-wildcard sh-wildcard* sh-wildcard/apply sh-wildcard/expand-tilde sh-wildcard->string
    sh-wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (except (rnrs)     current-error-port current-output-port)
    (rnrs mutable-pairs)
    (only (chezscheme) append! break
                       console-error-port console-output-port current-error-port current-output-port
                       debug debug-condition debug-on-exception display-condition
                       foreign-procedure format fx1+ fx1- hashtable-cells include inspect
                       logand logbit? make-format-condition meta open-fd-output-port
                       parameterize procedure-arity-mask record-writer reverse! sort!
                       string-copy! string-truncate! void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix)
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



;; set the status of a job and return it.
;; if specified status indicates that job finished,
;;   i.e. (sh-finished? status) returns true
;;   also close the job fds that need to be closed.
(define (job-status-set! caller job new-status)
  (let* ((status (status-normalize new-status))
         (kind   (sh-status->kind status)))
    ;; (debugf "job-status-set! caller=~s job=~a status=~s normalized-status=~s" caller (sh-job->string job) new-status status)
    (case kind
      ((running)
        (job-status-set/running! job))
      ((ok exception failed killed)
        (%job-last-status-set! job status)
        (when (sh-finished? status)
          (when (sh-cmd? job)
            ; unset expanded arg-list, because next expansion may differ
            (cmd-expanded-arg-list-set! job #f))
          ; (debugf "job-status-set! caller=~s job=~s status=~s" caller job status)
          (job-unmap-fds! job)
          (job-unredirect/temp/all! job) ; remove temporary redirections
          (job-temp-parent-set!  job #f) ; remove temporary parent job
          (cond
            ((sh-expr? job)
              (jexpr-resume-proc-set!  job #f)
              (jexpr-suspend-proc-set! job #f))
            ((sh-multijob? job)
              (multijob-current-child-index-set! job -1))))
        status)
      (else
        (%job-last-status-set! job status)))))


;; set job status to (list 'running job-id)
;; and return such status
(define (job-status-set/running! job)
  (let* ((id     (job-id job))
         (status (job-last-status job))
         (kind   (sh-status->kind status))
         (old-id (if (and (pair? status) (not (null? (cdr status))))
                   (cadr status)
                   #f)))
    (if (and (eq? 'running status) (eqv? id old-id))
      status
      (let ((new-status (list 'running id)))
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
;; If job status is '(running) update it to '(running job-id)
;; Return updated job status
(define (job-id-set! job)
  (assert* 'job-id-set! (sh-job? job))
  (let* ((old-id (job-id job))
         (id     (or old-id (%job-id-assign! job)))
         (status (job-last-status job))
         (kind   (sh-status->kind status)))
    (when (and (eq? kind 'running) (or (null? (cdr status)) (not (eqv? id (cadr status)))))
      ;; replace job status '(running) -> '(running job-id)
      (job-status-set! 'job-id-set! job (list 'running id)))
    (unless (eqv? id old-id)
      (queue-job-display-summary job)))
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
;; Also replace any job status '(running) -> '(running job-id)
;; Return updated job status.
;;
;; Note: does not create job-id for children of (sh-pipe) jobs.
(define (job-id-update! job)
  (let ((parent (job-parent job))
        (status (job-last-status job)))
    (if (and (sh-multijob? parent) (eq? 'sh-pipe (multijob-kind parent)))
      ;; the children of (sh-pipe) jobs are not supposed to be started/stopped individually:
      ;; the parent (sh-pipe) job always starts/stops all of them collectively.
      ;; thus assigning a job-id to such children usually just adds noise.
      (job-id-unset! job)
      (case (sh-status->kind status)
        ((running stopped)
          (job-id-set! job))
        ((ok exception failed killed)
          (job-id-unset! job))
        (else
          status)))))




;; Convert job-or-id to job.
;; job-or-id can be either a job,
;; or #t which means (sh-globals),
;; or a fixnum indicating the job-id of one of the running jobs
;;   stored in (multijob-children (sh-globals))
;;
;; Returns job object, or #f if job was not found.
(define (sh-find-job job-or-id)
  (cond
    ((eq? #t job-or-id)
      (sh-globals))
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
;; or a fixnum indicating the job-id of one of the running jobs
;;   stored in (multijob-children (sh-globals))
;;
;; Raises error if no job matches job-or-id.
(define (sh-job job-or-id)
  (cond
    ((eq? #t job-or-id) (sh-globals))
    ((fixnum? job-or-id)
      (let* ((all-jobs (multijob-children (sh-globals)))
             (job (when (and (fx>? job-or-id 0) ; job-ids start at 1
                             (fx<? job-or-id (span-length all-jobs)))
                    (span-ref all-jobs job-or-id))))
        (unless (sh-job? job)
          (raise-errorf 'sh-job "job not found: ~s" job-or-id))
        job))
    ((sh-job? job-or-id) job-or-id)
    (else (raise-errorf 'sh-job "not a job-id: ~s" job-or-id))))


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


(define (sh-consume-signals lctx)
  (while (signal-consume-sigchld)
    (scheduler-wait #f 'nonblocking))
  (display-status-changes lctx))


(define (display-status-changes lctx)
  (let ((job-list (queue-job-display-summary)))
    (unless (null? job-list)
      (lineedit-undraw lctx 'flush)
      (list-iterate (list-remove-consecutive-duplicates! (sort! sh-job<? job-list) eq?)
        (lambda (job)
          (display-status-change job lctx))))))


(define (display-status-change job lctx)
  (when (or (job-id job) (job-oid job))
    (sh-job-display-summary job)
    (job-oid-set! job #f))) ; no longer needed, clear it


;; called when starting a builtin or multijob:
;; create fd redirections and store them into (job-fds-to-remap)
;;
;; Reason: builtins and multijobs are executed in main schemesh process,
;; a redirection may overwrite fds 0 1 2 or some other fd already used
;; by main schemesh process, and we don't want to alter them:
;;
;; we need an additional layer of indirection that keeps track of the job's redirected fds
;; and to which (private) fds they are actually mapped to
(define (job-remap-fds! job)
  (let ((n (span-length (job-redirects job))))
    (unless (or (fxzero? n) (job-fds-to-remap job)) ; if fds are already remapped, do nothing
      (let ((job-dir (job-cwd-if-set job))
            (remaps  (make-eqv-hashtable n)))
        (job-fds-to-remap-set! job remaps)
        (do ((i 0 (fx+ i 4)))
            ((fx>? i (fx- n 4)))
          (job-remap-fd! job job-dir i))))))


;; called by (job-remap-fds!)
(define (job-remap-fd! job job-dir index)
  ;; redirects is span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
  (let* ((redirects            (job-redirects job))
         (fd                   (span-ref redirects index))
         (direction-ch         (span-ref redirects (fx1+ index)))
         (to-fd-or-bytevector0 (job-extract-redirection-to-fd-or-bytevector0 job job-dir redirects index))
         (remap-fd             (s-fd-allocate)))
    ;; (debugf "job-remap-fd! fd=~s dir=~s remap-fd=~s to=~s" fd direction-ch remap-fd to-fd-or-bytevector0)
    (let* ((fd-int (s-fd->int remap-fd))
           (ret (fd-redirect fd-int direction-ch to-fd-or-bytevector0 #t))) ; #t close-on-exec?
      (when (< ret 0)
        (s-fd-release remap-fd)
        (raise-c-errno 'sh-start 'c_fd_redirect ret fd-int direction-ch to-fd-or-bytevector0)))
    (hashtable-set! (job-fds-to-remap job) fd remap-fd)))



;; extract the destination fd or bytevector0 from a redirection
(define (job-extract-redirection-to-fd-or-bytevector0 job job-dir redirects index)
  (%prefix-job-dir-if-relative-path job-dir
    (or (span-ref redirects (fx+ 3 index))
        (let ((to (span-ref redirects (fx+ 2 index))))
          (if (procedure? to)
            (if (logbit? 1 (procedure-arity-mask to)) (to job) (to))
            to)))))


(define (%prefix-job-dir-if-relative-path job-dir path-or-fd)
  (cond
    ((fixnum? path-or-fd)
      path-or-fd)
    ((or (string? path-or-fd) (bytevector? path-or-fd))
      (let ((bvec (text->bytevector0 path-or-fd))
            (slash 47))
        (if (and job-dir (not (fx=? slash (bytevector-u8-ref bvec 0))))
          (let ((bspan (charspan->utf8b job-dir)))
            (unless (or (bytespan-empty? bspan) (fx=? slash (bytespan-ref-right/u8 bspan)))
              ;; append / after job's directory if missing
              (bytespan-insert-right/u8! bspan slash))
            (bytespan-insert-right/bvector! bspan bvec)
            (bytespan->bytevector bspan))
          bvec)))
    (else
      (raise-assert1 'job-remap-fds
        "(or (fixnum? path-or-fd) (string? path-or-fd) (bytevector? path-or-fd))"
        path-or-fd))))


;; redirect a file descriptor. returns < 0 on error
;; arguments: fd direction-ch to-fd-or-bytevector0 close-on-exec?
(define fd-redirect
  (foreign-procedure "c_fd_redirect" (ptr ptr ptr ptr) int))


;; return the remapped file descriptor for specified fd,
;; or fd itself if no remapping was found
(define (job-find-fd-remap job fd)
  (do ((parent job (job-parent parent)))
      ((not parent) fd)
    (let* ((remap-fds (job-fds-to-remap parent))
           (remap-fd  (and remap-fds (hashtable-ref remap-fds fd #f))))
      (when remap-fd
        (set! fd (s-fd->int remap-fd))))))


;; release job's remapped fds and unset (job-fds-to-remap job)
(define (job-unmap-fds! job)
  (let ((remap-fds (job-fds-to-remap job)))
    (when remap-fds
      (hashtable-iterate remap-fds
        (lambda (cell)
          (let ((fd (cdr cell)))
            (when (s-fd-release fd)
              ;; (debugf "job-unmap-fds! fd-close ~s" (s-fd->int fd))
              (fd-close (s-fd->int fd))))))
      (job-fds-to-remap-set! job #f))))




(include "shell/options.ss")
(include "shell/params.ss")
(include "shell/builtins.ss")
(include "shell/cmd.ss")
(include "shell/expr.ss")
(include "shell/multijob.ss")
(include "shell/env.ss")
(include "shell/dir.ss")
(include "shell/redirect.ss")
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
