;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
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

(library (schemesh shell jobs (0 1))
  (export
    ; jobs.ss
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-cmd? sh-multijob?
    sh-env-copy sh-env->argv sh-globals sh-global-env
    sh-cmd make-cmd sh-cwd
    sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/ok?

    ; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell

    ; display.ss
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string
    sh-job-display/summary? sh-job-display/summary sh-job-display/summary*

    ; env.ss
    sh-env sh-env! sh-env-unset! sh-env-exported? sh-env-export! sh-env-set+export! sh-env/lazy!
    sh-cwd-set! sh-cd sh-pwd

    ; redirect.ss
    sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-redirect!

    ; pipe.ss
    sh-pipe sh-pipe*

    ; parse.ss
    sh sh-parse sh-cmd* sh-list*

    ; wildcard
    sh-wildcard sh-wildcard/apply sh-wildcard/expand-tilde!
    sh-wildcard/prepare-patterns sh-wildcard/expand-patterns)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! break display-string eval foreign-procedure format fx1+ fx1-
                       include inspect logand logbit? make-format-condition make-thread-parameter
                       open-fd-output-port parameterize procedure-arity-mask record-writer reverse! void)
    (only (schemesh bootstrap) assert* debugf sh-eval raise-assertv raise-errorf until while)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix fd)
    (schemesh posix pattern)
    (schemesh posix pid)
    (schemesh posix signal)
    (only (schemesh posix tty) tty-inspect)
    (schemesh shell fds)
    (schemesh shell paths)
    (schemesh shell builtins))


;; define the record types "job" "cmd" "multijob" and their accessors
(include "shell/types.ss")


;; return the job-id of a job, or #f if not set
(define (sh-job-id job)
  (job-id job))


;; Define the variable sh-globals, contains the global job.
;; Jobs started with (sh-start) will be children of sh-globals.
;
;; Variable may be set! to a different value in subshells.
(define sh-globals
  ;; assign job-id 0 to sh-globals itself.
  ;;
  ;; waiting for sh-globals to exit is not useful:
  ;; pretend it already exited with unknown exit status
  (%make-multijob 0 (get-pid) (get-pgid 0) '(unknown . 0)
    (span) #f '() ; redirections
    #f #f ; start-proc step-proc
    (string->charspan* ((foreign-procedure "c_get_cwd" () ptr))) ; current directory
    (make-hashtable string-hash string=?) ; env variables
    #f                        ; no env var assignments
    #f                        ; no parent
    'sh-global -1 (span #t))) ; skip job-id 0, is used by sh-globals itself

;; Global hashtable pid -> job
(define %table-pid->job (make-eq-hashtable))

;; Convert pid to job, return #f if job not found
(define (pid->job pid)
  (assert* 'pid->job (fixnum? pid))
  (hashtable-ref %table-pid->job pid #f))

;; Adds an entry to the global hashtable pid -> job
(define (pid->job-set! pid job)
  (assert* 'pid->job-set! (fixnum? pid))
  (assert* 'pid->job-set! (sh-job? job))
  (hashtable-set! %table-pid->job pid job))

;; Removes an entry from the global hashtable pid -> job
(define (pid->job-delete! pid)
  (assert* 'pid->job-delete! (fixnum? pid))
  (hashtable-delete! %table-pid->job pid))


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
;;
(define (job-status-stops-or-ends-multijob? job-status)
  (let ((kind (job-status->kind job-status)))
    (or (memq kind '(unknown stopped))
        (and (eq? kind 'killed)
             (memq (cdr job-status) '(sigint sigquit))))))


;; Return #t if job-status represents a child job status
;; that causes a parent multijob to end, i.e. one of:
;; '(unknown . *)
;; '(killed  . sigint)
;; '(killed  . sigquit)
;;
(define (job-status-ends-multijob? job-status)
  (let ((kind (job-status->kind job-status)))
    (or (eq? kind 'unknown)
        (and (eq? kind 'killed)
             (memq (cdr job-status) '(sigint sigquit))))))


;; set the status of a job and return it.
;; if (job-status->kind status) is one of 'exited 'killed 'unknown, also close the job fds
(define (job-status-set! job status)
  (let ((status (job-status-normalize status)))
    (if (job-status-member? status '(running))
      (job-status-set/running! job)
      (begin
        (%job-last-status-set! job status)
        (when (job-status-member? status '(exited killed unknown))
          (job-unmap-fds! job)
          (let ((fds (job-fds-to-close job)))
            (unless (null? fds)
              (fd-close-list fds)
              (job-fds-to-close-set! job '()))))
        status))))


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
;; and remove it from (multijob-children sh-globals).
;; Return job status
(define (job-id-unset! job)
  (assert* 'job-id-unset! (sh-job? job))
  (when (job-id job)
    (let* ((children (multijob-children sh-globals))
           (child-n  (span-length children))
           (id       (job-id job)))
      (when (fx<? -1 id child-n)
        (span-set! children id #f)
        (until (or (span-empty? children) (span-back children))
          (span-erase-back! children 1)))
      (sh-job-display/summary job)
      (%job-id-set! job #f)))
  (job-last-status job))



;; If job has no job-id, assign a job-id to it, by appending it to (multijob-children sh-globals).
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
      (job-status-set! job (cons 'running id)))
    (unless (eqv? id old-id)
      (sh-job-display/summary job)))
  (job-last-status job))


;; Assumes job has no job-id, and assigns a job-id to it, by appending it to (multijob-children sh-globals).
;; Return job-id
(define (%job-id-assign! job)
  (let* ((children (multijob-children sh-globals))
         (id       (span-length children)))
    (span-insert-back! children job)
    (%job-id-set! job id)
    id))



;; if job is running or stopped, then create a new job-id for it.
;; if job has terminated, clear its job id and close its fds.
;; Also replace any job status '(running . #f) -> '(running . job-id)
;; Return updated job status.
(define (job-id-update! job)
  (let ((status (job-last-status job)))
    (case (job-status->kind status)
      ((running stopped)
        (job-id-set! job))
      ((exited killed unknown)
        (job-id-unset! job))
      (else
        status))))




;; Convert job-or-id to job.
;; job-or-id can be either a job,
;; or #t which means sh-globals,
;; or a fixnum indicating the job-id of one of the running jobs
;;   stored in (multijob-children sh-globals)
;;
;; Raises error if no job matches job-or-id.
(define (sh-job job-or-id)
  (cond
    ((eq? #t job-or-id) sh-globals)
    ((fixnum? job-or-id)
      (let* ((all-jobs (multijob-children sh-globals))
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
  (let ((src (multijob-children sh-globals))
        (dst (span)))
    (span-iterate src
      (lambda (job-id job)
        (when (sh-job? job)
          (span-insert-back! dst (cons job-id job)))))
    dst))

;; the "jobs" builtin: list currently running jobs
(define (builtin-jobs job prog-and-args options)
  (assert-string-list? 'sh-builtin-jobs prog-and-args)
  (let ((src (multijob-children sh-globals)))
    (unless (span-empty? src)
      ;; do NOT close port, it would close the fd!
      (let ((port (open-fd-output-port (sh-fd-stdout) (buffer-mode line) transcoder-utf8)))
        (span-iterate src
          (lambda (job-id job)
            (when (sh-job? job)
              (sh-job-display/summary* job port)))))))
  (void))


;; call (proc job) on given job and each of its
;; parents. Stops iterating if (proc ...) returns #f.
;;
;; Returns #t if all calls to (proc job) returned truish,
;; otherwise returns #f.
(define (job-parents-iterate job-or-id proc)
  (do ((parent (sh-job job-or-id) (job-parent parent)))
      ((not (and (sh-job? parent) (proc parent)))
       (not (sh-job? parent)))))



;; return list containing all job's parents,
;; starting from sh-globals, until job itself.
(define (job-parents-revlist job-or-id)
  (let ((jlist '()))
    (job-parents-iterate job-or-id
      (lambda (job)
        (set! jlist (cons job jlist))))
    jlist))


;; return list containing job followed by all its parents.
(define (job-parents-list job-or-id)
  (reverse! (job-parents-revlist job-or-id)))


(define (sh-consume-sigchld)
  ; TODO: call (signal-consume-sigchld) and (pid-wait) to reap zombies
  ;        and collect exit status of child processes
  (void))


(define (job-start-options->process-group-id options)
  (let ((existing-pgid -1))
    (list-iterate options
      (lambda (option)
        (when (fixnum? option)
          (set! existing-pgid option)
          #f))) ; stop iterating on options
    existing-pgid))


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
    (unless (fxzero? n)
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
            (bytespan-insert-back/bvector! bspan bvec 0 (bytevector-length bvec))
            (bytespan->bytevector bspan))
          bvec)))
    (#t
      (raise-assertv 'job-remap-fds
        '(or (fixnum? path-or-fd) (string? path-or-fd) (bytevector? path-or-fd))
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
              (fd-close (sh-fd->int fd))))))
      (job-fds-to-remap-set! job #f))))





;; Start a cmd or a job and return immediately, without waiting for it to finish.
;; If job finishes immediately, return its exit status (happens for builtins).
;; Otherwise, return '(running . job-id)
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 then the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define (sh-start job . options)
  (start/any job options)
  (job-id-update! job)) ; sets job-id if started, otherwise unsets it


;; Internal functions called by (sh-start)
(define (start/any job options)
  ; (debugf "start/any ~s ~s" job options)
  (when (job-started? job)
    (if (job-id job)
      (raise-errorf 'sh-start "job already started with job id ~s" (job-id job))
      (raise-errorf 'sh-start "job already started")))
  (let ((proc (job-start-proc job)))
    (unless (procedure? proc)
      (raise-errorf 'sh-start "cannot start job, it has bad or missing job-start-proc: ~s" job))
    (job-status-set/running! job)
    (proc job options)) ; ignore value returned by job-start-proc
  (when (fx>? (job-pid job) 0)
    (pid->job-set! (job-pid job) job))        ; add job to pid->job table
  (job-last-status job))



;; Return up-to-date status of a job or job-id, which can be one of:
;;   (cons 'new     0)
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-or-id)
  (let ((job (sh-job job-or-id)))
    (if (job-has-status? job '(new))
      (job-last-status job)
      (job-advance 'sh-job-status job))))


;; Continue a job or job-id in background by sending SIGCONT to it.
;; Return job status, which can be one of:
;;
;;   (cons 'running job-id)
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
(define (sh-bg job-or-id)
  (job-advance 'sh-bg job-or-id))


;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status, which can be one of:
;;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Note: if the current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-or-id)
  (job-advance 'sh-fg job-or-id))


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
;;   (cons 'killed  signal-name)
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
  (job-advance (if send-sigcont? 'sh-sigcont+wait 'sh-wait) job-or-id))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
;; Also called by (job-advance/multijob)
;;
;; mode must be one of: sh-fg sh-bg sh-wait sh-sigcont+wait sh-subshell sh-job-status
(define (job-advance mode job-or-id)
  (assert* 'job-advance (memq mode '(sh-fg sh-bg sh-wait sh-sigcont+wait sh-subshell sh-job-status)))
  (let ((job (sh-job job-or-id)))
    ; (debugf "job-advance... mode=~s job=~s id=~s status=~s" mode job (job-id job) (job-last-status job))
    (case (job-last-status->kind job)
      ((exited killed unknown)
        (void)) ; job finished
      ((running stopped)
        (cond
          ((fx>? (job-pid job) 0)
            (job-advance/pid mode job))
          ((sh-multijob? job)
            (if (eq? 'sh-pipe (multijob-kind job))
              (job-advance/pipe     mode job)
              (job-advance/multijob mode job)))))
      (else
        (raise-errorf mode  "job not started yet: ~s" job)))
    ; returns job status
    (job-id-update! job)))


;; Start a job and wait for it to exit or stop.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (start/any job options)
  (sh-fg job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (start/any job options)
  (sh-wait job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #t if job exited successfully, otherwise return #f.
(define (sh-run/ok? job . options)
  (sh-ok? (apply sh-run job options)))


(include "shell/cmd.ss")
(include "shell/multijob.ss")
(include "shell/env.ss")
(include "shell/dir.ss")
(include "shell/redirect.ss")
(include "shell/pipe.ss")
(include "shell/parse.ss")
(include "shell/wildcard.ss")
(include "shell/display.ss")



(begin
  (sh-fd-allocate) ; mark highest fd as reserved: used by tty_fd
  (c-environ->sh-global-env)

  (let ((t (sh-builtins)))
    ; additional builtins
    (hashtable-set! t "cd"      builtin-cd)
    (hashtable-set! t "command" builtin-command)
    (hashtable-set! t "jobs"    builtin-jobs)
    (hashtable-set! t "pwd"     builtin-pwd)))


) ; close library
