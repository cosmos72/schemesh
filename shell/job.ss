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
;             (shell) and (shell-...) are macros

(library (schemesh shell job (0 7 2))
  (export
    ; alias.ss
    sh-alias-ref sh-alias-delete! sh-alias-set! sh-aliases-expand sh-aliases

    ; dir.ss
    sh-cd sh-pwd sh-userhome sh-xdg-cache-home/ sh-xdg-config-home/

    ; display.ss
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string
    sh-job-display/summary? sh-job-display/summary sh-job-display/summary*

    ; env.ss
    sh-env-ref sh-env-set! sh-env-delete! sh-env-visibility-ref sh-env-visibility-set!
    sh-env-iterate/direct sh-env-set/lazy! sh-env-copy sh-env->argv

    ; job.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-find-job sh-job-exception
    sh-cmd? sh-multijob? sh-cmd make-cmd sh-cwd sh-consume-sigchld
    sh-globals sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-start* sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/err? sh-run/ok?

    ; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell

    ; options
    sh-options

    ; redirect.ss
    sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-redirect! sh-start/fd-stdout

    ; params.ss
    sh-job-control-available? sh-job-control?

    ; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ; pipe.ss
    sh-pipe sh-pipe*

    ; wildcard
    sh-wildcard sh-wildcard* sh-wildcard/apply sh-wildcard/expand-tilde sh-wildcard->string
    sh-wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! break console-output-port console-error-port
                       debug-condition display-condition foreign-procedure format fx1+ fx1-
                       include inspect logand logbit? make-format-condition
                       open-fd-output-port parameterize procedure-arity-mask record-writer reverse!
                       string-copy! string-truncate! define void)
    (schemesh bootstrap)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix dir)
    (schemesh posix fd)
    (schemesh posix pattern)
    (schemesh posix pid)
    (schemesh posix signal)
    (only (schemesh lineedit charhistory) charhistory-path-set!)
    (only (schemesh lineedit linectx) linectx? linectx-history linectx-save-history)
    (schemesh shell builtins)
    (schemesh shell fds)
    (schemesh shell parameters)
    (schemesh shell paths))


;; define the record types "job" "cmd" "multijob" and their accessors
(include "shell/types.ss")


;; return the job-id of a job, or #f if not set
(define (sh-job-id job)
  (job-id job))


;; set the process group id of specified job
(define (job-pgid-set! job pgid)
  (%job-pgid-set! job
    (cond
      ((eqv? pgid 0)
        ; pgid is zero: it means a new process group id was created
        ; for this job, and it is numerically equal to the job's pid.
        (job-pid job))
      ((and (integer? pgid) (> pgid 0))
        ; set job's process group id to a pre-existing group's id
        pgid)
      (#t
        ; unset job's process group id
        #f))))


;; Convert pid to job, return #f if job not found
(define (pid->job pid)
  (assert* 'pid->job (fixnum? pid))
  (hashtable-ref (sh-pid-table) pid #f))

;; Adds an entry to the global hashtable pid -> job
(define (pid->job-set! pid job)
  (assert* 'pid->job-set! (fixnum? pid))
  (assert* 'pid->job-set! (sh-job? job))
  (hashtable-set! (sh-pid-table) pid job))

;; Removes an entry from the global hashtable pid -> job
(define (pid->job-delete! pid)
  (assert* 'pid->job-delete! (fixnum? pid))
  (hashtable-delete! (sh-pid-table) pid))



;; return #t if job-status is (void), i.e. if job exited with exit status 0,
;; otherwise return #f
;;
;; job-status must be one of the possible values returned by
;; (sh-fg) (sh-fg) (sh-wait) or (sh-job-status)
(define (sh-ok? job-status)
  (eq? job-status (void)))


;; Convert a job-status to one of: 'new 'running 'stopped 'exited 'killed 'unknown
(define (job-status->kind job-status)
  ;; job-status is either (void) or a pair
  (cond
    ((eq? (void) job-status)  'exited)
    ((pair? job-status)       (car job-status))
    (#t                       'unknown)))

;; Convert job's last-status to one of: 'new 'running 'stopped 'exited 'killed 'unknown
(define (job-last-status->kind job)
  (job-status->kind (job-last-status job)))


;; Return #t if job-status is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if job-status is (void) and allowed-list also contains 'exited
;; then return #t because (void) is a shortcut for '(exited . 0)
(define (job-status-member? job-status allowed-list)
  (memq (job-status->kind job-status) allowed-list))

;; Return #t if job-status is started, otherwise return #f
(define (job-status-started? job-status)
  (job-status-member? job-status '(running stopped)))

;; Return #t if job-status is finished, otherwise return #f
(define (job-status-finished? job-status)
  (job-status-member? job-status '(exited killed unknown)))


;; Return #t if (job-last-status job) is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if (job-last-status job) is (void) and allowed-list also contains 'exited
;; then return #t because (void) is a shortcut for '(exited . 0)
(define (job-has-status? job allowed-list)
  (job-status-member? (job-last-status job) allowed-list))

;; Return #t if job was already started, otherwise return #f
(define (job-started? job)
  (job-status-started? (job-last-status job)))

;; Return #t if job has already finished, otherwise return #f
(define (job-finished? job)
  (job-status-finished? (job-last-status job)))




;; Return #t if job-status represents a child job status
;; that causes a parent multijob to stop or end, i.e. one of:
;; '(unknown . *)
;; '(stopped . *)
;; '(killed  . sigint)
;; '(killed  . sigquit)
;; '(killed  . exception)
;;
(define (job-status-stops-or-ends-multijob? job-status)
  (let ((kind (job-status->kind job-status)))
    (or (memq kind '(unknown stopped))
        (and (eq? kind 'killed)
             (memq (cdr job-status) '(sigint sigquit exception))))))


;; Return #t if job-status represents a child job status
;; that causes a parent multijob to end, i.e. one of:
;; '(unknown . *)
;; '(killed  . sigint)
;; '(killed  . sigquit)
;; '(killed  . exception)
;;
(define (job-status-ends-multijob? job-status)
  (let ((kind (job-status->kind job-status)))
    (or (eq? kind 'unknown)
        (and (eq? kind 'killed)
             (memq (cdr job-status) '(sigint sigquit exception))))))


;; set the status of a job and return it.
;; if specified status indicates that job finished,
;;   i.e. (job-status->kind status) is one of 'exited 'killed 'unknown,
;;   also close the job fds that need to be closed.
(define (job-status-set! caller job status)
  ;a (debugf "job-status-set! caller=~s job=~s status=~s" caller job status)
  (let ((status (job-status-normalize status)))
    (if (job-status-member? status '(running))
      (job-status-set/running! job)
      (begin
        (%job-last-status-set! job status)
        (when (job-status-finished? status)
          (when (sh-cmd? job)
            ; unset expanded arg-list, because next expansion may differ
            (cmd-expanded-arg-list-set! job #f))
          ; (debugf "job-status-set! caller=~s job=~s status=~s" caller job status)
          (job-unmap-fds! job)
          (job-close-fds-to-close! job)
          (job-unredirect/temp/all! job) ; remove temporary redirections
          (job-temp-parent-set! job #f)) ; remove temporary parent job
        status))))


;; close fd list (job-fds-to-close job) and set it to the empty list.
(define (job-close-fds-to-close! job)
  (let ((fds (job-fds-to-close job)))
    (unless (null? fds)
      (fd-close-list fds)
      (job-fds-to-close-set! job '()))))


;; normalize job status, converting unexpected status values to '(unknown . 0)
(define (job-status-normalize status)
  (cond
    ((eq? (void) status)
      status)
    ((and (pair? status) (memq (car status) '(new running stopped exited killed unknown)))
      status)
    (#t
      '(unknown . 0))))


(define (job-status-set/running! job)
  (let* ((id     (job-id job))
         (status (job-last-status job))
         (kind   (job-status->kind status))
         (old-id (if (pair? status) (cdr status) #f)))
    (if (and (eq? 'running status) (eqv? id old-id))
      status
      (let ((new-status (cons 'running id)))
        (%job-last-status-set! job new-status)
        new-status))))


;; unset the job-id of a job,
;; and remove it from (multijob-children (sh-globals)).
;; Return job status
(define (job-id-unset! job)
  (assert* 'job-id-unset! (sh-job? job))
  (when (job-id job)
    (let* ((children (multijob-children (sh-globals)))
           (child-n  (span-length children))
           (id       (job-id job)))
      (when (fx<? -1 id child-n)
        (span-set! children id #f)
        (until (or (span-empty? children) (span-back children))
          (span-erase-back! children 1)))
      (sh-job-display/summary job)
      (%job-id-set! job #f)))
  (job-last-status job))



;; If job has no job-id, assign a job-id to it, by appending it to (multijob-children (sh-globals)).
;; If job status is '(running . #f) update it to '(running . job-id)
;; Return updated job status
(define (job-id-set! job)
  (assert* 'job-id-set! (sh-job? job))
  (let* ((old-id (job-id job))
         (id     (or old-id (%job-id-assign! job)))
         (status (job-last-status job))
         (kind   (job-status->kind status)))
    (when (and (eq? kind 'running) (not (eqv? id (cdr status))))
      ;; replace job status '(running . #f) -> '(running . job-id)
      (job-status-set! 'job-id-set! job (cons 'running id)))
    (unless (eqv? id old-id)
      (sh-job-display/summary job)))
  (job-last-status job))


;; Assumes job has no job-id, and assigns a job-id to it, by appending it to (multijob-children (sh-globals)).
;; Return job-id
(define (%job-id-assign! job)
  (let* ((children (multijob-children (sh-globals)))
         (id       (span-length children)))
    (span-insert-back! children job)
    (%job-id-set! job id)
    id))


(define (job-parent job)
  (or (job-temp-parent job) (job-default-parent job)))


;; if job is running or stopped, then create a new job-id for it.
;; if job has terminated, clear its job id and close its fds.
;; Also replace any job status '(running . #f) -> '(running . job-id)
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
      (case (job-status->kind status)
        ((running stopped)
          (job-id-set! job))
        ((exited killed unknown)
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
    (#t
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
    (#t (raise-errorf 'sh-job "not a job-id: ~s" job-or-id))))


;; return currently running jobs
;; as a span of pairs (job-id . job) sorted by job-id
(define (sh-jobs)
  (let ((src (multijob-children (sh-globals)))
        (dst (span)))
    (span-iterate src
      (lambda (job-id job)
        (when (sh-job? job)
          (span-insert-back! dst (cons job-id job)))))
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
(define (job-default-parents-iterate job-or-id proc)
  (do ((parent (sh-job job-or-id) (job-default-parent parent)))
      ((not (and (sh-job? parent) (proc parent)))
       (not (sh-job? parent)))))



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

(define (sh-consume-sigchld)
  (while (signal-consume-sigchld)
    (job-pids-wait #f 'nonblocking)))



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
         (remap-fd             (sh-fd-allocate)))
    ; (debugf "fd-redirect fd=~s dir=~s to=~s" remap-fd direction-ch to-fd-or-bytevector0)
    (let* ((fd-int (sh-fd->int remap-fd))
           (ret (fd-redirect fd-int direction-ch to-fd-or-bytevector0 #t))) ; #t close-on-exec?
      (when (< ret 0)
        (sh-fd-release remap-fd)
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
            (unless (or (bytespan-empty? bspan) (fx=? slash (bytespan-back/u8 bspan)))
              ;; append / after job's directory if missing
              (bytespan-insert-back/u8! bspan slash))
            (bytespan-insert-back/bvector! bspan bvec)
            (bytespan->bytevector bspan))
          bvec)))
    (#t
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
        (set! fd (sh-fd->int remap-fd))))))


;; release job's remapped fds and unset (job-fds-to-remap job)
(define (job-unmap-fds! job)
  (let ((remap-fds (job-fds-to-remap job)))
    (when remap-fds
      (hashtable-iterate remap-fds
        (lambda (cell)
          (let ((fd (cdr cell)))
            (when (sh-fd-release fd)
              ; (debugf "pid ~s: job-unmap-fds! -> fd-close ~s" (pid-get) (sh-fd->int fd))
              (fd-close (sh-fd->int fd))))))
      (job-fds-to-remap-set! job #f))))





;; Start a job and return immediately, without waiting for it to finish.
;;
;; Returns job status, typically (cons 'running job-id) but other values are allowed.
;; For the complete list of possible returned job statuses, see (sh-job-status).
;;
;; Note that job may finish immediately, for example because it is a builtin,
;;   or a multijob that only (recursively) contains builtins,
;;   or a command that exits very quickly.
;;   For these reasons, the returned job status may be different from (cons 'running job-id)
;;   and may indicate that the job has already finished.
;;
;; For possible values of options, see (sh-options)
;;
(define (sh-start job . options)
  (sh-start* job options))

;; same as sh-start, options must be passed as a single association list.
(define (sh-start* job options)
  (start-any 'sh-start job options)
  (job-id-update! job)) ; sets job-id if started, otherwise unsets it. also returns job status



;; Internal functions called by (sh-start)
(define (start-any caller job options)
  ;b (debugf "start-any ~a ~s" (sh-job-display/string job) options)
  (options-validate caller options)
  (call/cc
    (lambda (k-continue)
      (with-exception-handler
        (lambda (ex)
          (start-any/on-exception caller job options k-continue ex))
        (lambda ()
          (start-any/may-throw caller job options)))))
  (when (job-pid job)
    (pid->job-set! (job-pid job) job))  ; add job to pid->job table
  (when (and (job-status-started? job) (options->spawn? options))
    ; we can cleanup job's file descriptor, as it's running in a subprocess
    (job-unmap-fds! job)
    (job-close-fds-to-close! job)
    (job-unredirect/temp/all! job))
  (job-last-status job)) ; returns job status. also checks if job finished


(define (start-any/may-throw caller job options)
  (when (job-started? job)
    (if (job-id job)
      (raise-errorf caller "job already started with job id ~s" (job-id job))
      (raise-errorf caller "job already started")))
  (let ((start-proc (job-start-proc job)))
    (unless (procedure? start-proc)
      (raise-errorf caller "cannot start job ~s, bad or missing job-start-proc: ~s" job start-proc))
    (job-status-set/running! job)
    (job-exception-set! job #f)
    ;; set job's parent if requested.
    ;; must be done *before* calling procedures in (cmd-arg-list c)
    (let ((options (options->set-temp-parent! job options)))
      (start-proc job options)))  ; may throw. ignore value returned by job-start-proc
  (void))


(define (start-any/on-exception caller job options k-continue ex)
  (job-default-parents-iterate job
    (lambda (parent)
      (unless (eq? parent (sh-globals))
        (job-exception-set! parent ex))))
  (job-status-set! caller job '(killed . exception))
  (if (options->catch? options)
    (k-continue (sh-exception-handler ex))
    (raise ex)))


(define (start-any/display-condition obj port)
  (put-string port "; ")
  (display-condition obj port)
  (newline port)
  (flush-output-port port))


;; Return up-to-date status of a job or job-id, which can be one of:
;;   (cons 'new     0)
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-or-id)
  (let ((job (sh-job job-or-id)))
    ; (debugf ">  sh-job-status job=~a" (sh-job-display/string job))
    (if (job-has-status? job '(new))
      (job-last-status job)
      (advance-job 'sh-job-status job))))


;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status, which can be one of:
;;
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
(define (sh-bg job-or-id)
  (advance-job 'sh-bg job-or-id))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status, which can be one of:
;;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Note: if the current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (advance-job 'sh-fg job-or-id))


;; Continue a job or job-id by optionally sending SIGCONT to it,
;; then wait for it to exit, and finally return its status.
;;
;; Arguments are:
;;   job-or-id           ; a job or job-id
;;   send-sigcont?       ; if truthy, send SIGCONT to job before waiting for it to exit, default is #t
;;
;; Returned job status can be one of:
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name) or (cons 'killed 'exception)
;;   (cons 'unknown ...)
;;
;; Does NOT return early if job gets stopped, use (sh-fg) for that.
;;
;; Instead if job gets stopped, calls (break).
;; if (break) raises an exception or resets scheme, the job is interrupted with SIGINT.
;; otherwise waits again for the job to exit.
;;
;; Note: if current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define sh-wait
  (case-lambda
    ((job-or-id)               (sh-wait* job-or-id #t))
    ((job-or-id send-sigcont?) (sh-wait* job-or-id send-sigcont?))))


;; Same as (sh-wait), but all arguments are mandatory
(define (sh-wait* job-or-id send-sigcont?)
  (advance-job (if send-sigcont? 'sh-sigcont+wait 'sh-wait) job-or-id))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Also called by (advance-multijob)
;;
;; mode must be one of: sh-fg sh-bg sh-wait sh-sigcont+wait sh-job-status
(define (advance-job mode job-or-id)
  (assert* 'advance-job (memq mode '(sh-fg sh-bg sh-wait sh-sigcont+wait sh-job-status)))
  (let ((job (sh-job job-or-id)))
    ; (debugf ">  advance-job mode=~s job=~a id=~s pid=~s status=~s" mode (sh-job-display/string job) (job-id job) (job-pid job) (job-last-status job))
    (case (job-last-status->kind job)
      ((exited killed unknown)
        (void)) ; job finished
      ((running stopped)
        (cond
          ((job-pid job)
            ; either the job is a sh-cmd, or a builtin or multijob spawned in a child subprocess.
            ; in all cases, we have a pid to wait on.
            (advance-pid mode job))
          ((sh-multijob? job)
            (if (eq? 'sh-pipe (multijob-kind job))
              (advance-multijob-pipe mode job)
              (advance-multijob      mode job)))))
      (else
        (raise-errorf mode  "job not started yet: ~s" job)))

    (let ((status (job-id-update! job))) ; returns job status
      ;a (debugf "<  advance-job mode=~s job=~s id=~s pid=~s status=~s" mode job (job-id job) (job-pid job) status)
      status)))


;; Start a job and wait for it to exit or stop.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (start-any 'sh-run/i job options)
  (sh-fg job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (start-any 'sh-run job options)
  (sh-wait job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #t if job exited successfully, otherwise return #f.
(define (sh-run/ok? job . options)
  (sh-ok? (apply sh-run job options)))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #f if job exited successfully,
;; otherwise return job exit status, which is a cons and hence truish.
(define (sh-run/err? job . options)
  (let ((status (apply sh-run job options)))
    (if (eq? status (void)) #f status)))



(include "shell/options.ss")
(include "shell/params.ss")
(include "shell/cmd.ss")
(include "shell/multijob.ss")
(include "shell/env.ss")
(include "shell/dir.ss")
(include "shell/redirect.ss")
(include "shell/pipe.ss")
(include "shell/parse.ss")
(include "shell/builtins2.ss")
(include "shell/aliases.ss")
(include "shell/wildcard.ss")
(include "shell/display.ss") ; must be last one, contains (record-writer ...)



(begin
  (sh-fd-allocate) ; mark highest fd as reserved: used by tty_fd

  ;; set the parameter (sh-globals) to the global job.
  ;; Jobs started with (sh-start) will be children of sh-globals.
  ;;
  ;; If it's already set, does not modify it.
  ;;
  ;; May be parameterized to a different value in subshells.
  (unless (sh-globals)
    (sh-globals
      ;; assign job-id 0 to sh-globals itself.
      ;;
      ;; waiting for sh-globals to exit is not useful:
      ;; pretend it already exited with unknown exit status
      (%make-multijob
         0 (pid-get) (pgid-get 0)  ; id pid pgid
         '(unknown . 0) #f         ; last-status exception
         (span) 0 #f '()           ; redirections
         #f #f                     ; start-proc step-proc
         (string->charspan* ((foreign-procedure "c_get_cwd" () ptr))) ; current directory
         (make-hashtable string-hash string=?) ; env variables
         #f                        ; no env var assignments
         #f #f                     ; no temp parent, no default parent
         '\x23;<global> -1 (span #t)))) ; skip job-id 0, is used by (sh-globals) itself

  (c-environ->sh-global-env)

  (let ((bt (sh-builtins))
        (ft (builtins-that-finish-immediately)))

    ; additional builtins
    (hashtable-set! bt "alias"      builtin-alias)
    (hashtable-set! bt "bg"         builtin-bg)
    (hashtable-set! bt "builtin"    builtin-builtin)
    (hashtable-set! bt "cd"         builtin-cd)
    (hashtable-set! bt "command"    builtin-command)
    (hashtable-set! bt "exec"       builtin-exec)
    (hashtable-set! bt "fg"         builtin-fg)
    (hashtable-set! bt "global"     builtin-global)
    (hashtable-set! bt "jobs"       builtin-jobs)
    (hashtable-set! bt "pwd"        builtin-pwd)
    (hashtable-set! bt "split-at-0" builtin-split-at-0)
    (hashtable-set! bt "unalias"    builtin-unalias)
    (hashtable-set! bt "unsafe"     builtin-unsafe)

    (list-iterate '("alias" "cd" "echo" "echo0" "error" "false" "jobs" "history" "pwd" "true" "unalias")
      (lambda (name)
        (let ((builtin (hashtable-ref bt name #f)))
          (when builtin
            (hashtable-set! ft builtin #t)))))))



) ; close library
