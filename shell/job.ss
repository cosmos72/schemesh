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

(library (schemesh shell job (0 7 5))
  (export
    ;; aliases.ss
    sh-alias-ref sh-alias-delete! sh-alias-set! sh-aliases sh-aliases-expand

    ;; builtins2.ss
    sh-bool

    ;; cmd.ss
    make-sh-cmd sh-cmd

    ;; dir.ss
    sh-cd sh-pwd sh-userhome sh-xdg-cache-home/ sh-xdg-config-home/

    ;; display.ss
    sh-job-display sh-job-display* sh-job->string
    sh-job-write   sh-job-write* sh-job->verbose-string
    sh-job-display-summary? sh-job-display-summary sh-job-display-summary*

    ;; env.ss
    sh-env-ref sh-env-set! sh-env-delete! sh-env-visibility-ref sh-env-visibility-set!
    sh-env-iterate/direct sh-env-set/lazy! sh-env-copy sh-env->argv

    ;; job.ss
    sh-consume-sigchld sh-cwd
    sh-job sh-job-id sh-job-status sh-jobs sh-find-job sh-job-exception
    sh-start sh-start* sh-bg sh-fg sh-wait sh-run sh-run/i sh-run/err? sh-run/ok? sh-ok?

    ;; multijob.ss
    sh-and sh-or sh-not sh-list sh-subshell
    sh-globals sh-multijob-child-length sh-multijob-child-ref

    ;; options.ss
    sh-options

    ;; redirect.ss
    sh-redirect! sh-run/bspan sh-run/string sh-run/string-rtrim-newlines sh-run/string-split-after-nuls
    sh-start/fd-stdout

    ;; params.ss
    sh-job-control-available? sh-job-control?

    ;; parse.ss
    sh sh-parse-datum sh-cmd* sh-list*

    ;; pipe.ss
    sh-pipe sh-pipe*

    ;; types.ss
    sh-cmd? sh-job? sh-job-copy sh-multijob?

    ;; wildcard
    sh-wildcard sh-wildcard* sh-wildcard/apply sh-wildcard/expand-tilde sh-wildcard->string
    sh-wildcard->sh-patterns sh-patterns/expand
  )
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! break console-output-port console-error-port
                       debug-condition display-condition foreign-procedure format fx1+ fx1-
                       hashtable-cells include inspect logand logbit? make-format-condition
                       open-fd-output-port parameterize procedure-arity-mask record-writer
                       reverse! sort! string-copy! string-truncate! void)
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
    (only (schemesh lineedit lineedit) lineedit-flush lineedit-undraw)
    (schemesh shell builtins)
    (schemesh shell fds)
    (schemesh shell parameters)
    (schemesh shell paths))


;; define the record types "job" "cmd" "multijob" and their accessors
(include "shell/types.ss")




;; return #t if job-status is (void), i.e. if job exited with exit status 0,
;; otherwise return #f
;;
;; intentionally identical to function (ok?) exported by library (schemesh posix)
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


;; Return  if job-status is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if job-status is (void) and allowed-list also contains 'exited
;; then return truish because (void) is a shortcut for '(exited . 0)
(define (job-status-member? job-status allowed-list)
  (memq (job-status->kind job-status) allowed-list))

;; Return truish if job-status is started, otherwise return #f
(define (job-status-started? job-status)
  (job-status-member? job-status '(running stopped)))

;; Return truish if job-status is finished, otherwise return #f
(define (job-status-finished? job-status)
  (job-status-member? job-status '(exited killed unknown)))

;; Return truish if old-status and new-status have different
;; otherwise return #f
(define (job-status-changed? old-status new-status)
  (not (eq? (job-status->kind old-status)
            (job-status->kind new-status))))

;; Return truish if (job-last-status job) is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if (job-last-status job) is (void) and allowed-list also contains 'exited
;; then return truish because (void) is a shortcut for '(exited . 0)
(define (job-has-status? job allowed-list)
  (job-status-member? (job-last-status job) allowed-list))

;; Return truish if job was already started, otherwise return #f
(define (job-started? job)
  (job-status-started? (job-last-status job)))

;; Return truish if job has already finished, otherwise return #f
(define (job-finished? job)
  (job-status-finished? (job-last-status job)))




;; Return truish if job-status represents a child job status
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


;; Return truish if job-status represents a child job status
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
          (job-unredirect/temp/all! job) ; remove temporary redirections
          (job-temp-parent-set! job #f)) ; remove temporary parent job
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
      (sh-job-display-summary job)))
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


(define (sh-consume-sigchld lctx)
  (let ((proc-notify-status-change
          (lambda (job)
            (when (job-id job)
              (lineedit-undraw lctx 'flush)
              (sh-job-display-summary job)
              (when (job-finished? job)
                (job-id-unset! job))))))
    (let ((job-list (queue-job-display-summary)))
      (unless (null? job-list)
        (list-iterate (list-remove-consecutive-duplicates! (sort! sh-job<? job-list) eq?)
          proc-notify-status-change)))
    (while (signal-consume-sigchld)
      (job-pids-wait #f 'nonblocking proc-notify-status-change))))


;; raise an exception if a job or one of it recursive children is already started
(define (job-check-not-started caller job)
  (when (job-started? job)
    (if (job-id job)
      (raise-errorf caller "job already started with job id ~s" (job-id job))
      (raise-errorf caller "job already started")))
  (when (sh-multijob? job)
    (span-iterate (multijob-children job)
      (lambda (i elem)
        (when (sh-job? elem)
          (job-check-not-started caller elem))))))


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




(include "shell/options.ss")
(include "shell/params.ss")
(include "shell/cmd.ss")
(include "shell/multijob.ss")
(include "shell/env.ss")
(include "shell/dir.ss")
(include "shell/redirect.ss")
(include "shell/pipe.ss")
(include "shell/control.ss")
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
         (span) 0 #f               ; redirections
         #f #f                     ; start-proc step-proc
         (string->charspan* ((foreign-procedure "c_get_cwd" () ptr))) #f ; current directory, old working directory
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
    (hashtable-set! bt "cd-"        builtin-cd-)
    (hashtable-set! bt "command"    builtin-command)
    (hashtable-set! bt "exec"       builtin-exec)
    (hashtable-set! bt "exit"       builtin-exit)
    (hashtable-set! bt "export"     builtin-export)
    (hashtable-set! bt "fg"         builtin-fg)
    (hashtable-set! bt "global"     builtin-global)
    (hashtable-set! bt "jobs"       builtin-jobs)
    (hashtable-set! bt "parent"     builtin-parent)
    (hashtable-set! bt "pwd"        builtin-pwd)
    (hashtable-set! bt "set"        builtin-set)
    (hashtable-set! bt "split-at-0" builtin-split-at-0)
    (hashtable-set! bt "unalias"    builtin-unalias)
    (hashtable-set! bt "unexport"   builtin-unexport)
    (hashtable-set! bt "unsafe"     builtin-unsafe)
    (hashtable-set! bt "unset"      builtin-unset)

    ;; mark builtins that finish immediately i.e. cannot run commands or aliases
    (list-iterate '("alias" "cd" "cd-" "echo" "echo0" "exit" "expr" "false"
                    "jobs" "history" "pwd" "set" "true" "unalias" "unset")
      (lambda (name)
        (let ((builtin (hashtable-ref bt name #f)))
          (when builtin
            (hashtable-set! ft builtin #t))))))


  (let ((t (sh-builtins-help)))

    (hashtable-set! t "alias"   (string->utf8 " [name [expansion ...]]
    define or display aliases.

    without arguments,          'alias' writes the list of defined aliases to standard output.
    with a single argument,     'alias NAME' writes the definition of alias NAME to standard output.
    with two or more arguments, 'alias NAME EXPANSION ...' defines an alias NAME such that,
                                 when NAME ARGS ... executed, it is substituted with EXPANSION ... ARGS ...

    return success, unless 'alias NAME' is executed and no such alias is defined.\n"))

    (hashtable-set! t "bg"      (string->utf8 " job-id
    move a job to the background.

    return success if job-id was found, otherwise return failure.\n"))

    (hashtable-set! t "builtin" (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with specified arguments.

    useful if BUILTIN-NAME has been shadowed by an alias with the same name.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "cd"      (string->utf8 " [dir]
    change the current directory of parent job.

    without arguments, 'cd' sets the current directory of parent job
                       to the value of its HOME environment variable.
    with one argument, 'cd DIR' sets the current directory of parent job to DIR.

    return success if the directory is successfully changed, otherwise raises an exception.\n"))

    (hashtable-set! t "cd-"      (string->utf8 "
    change the current directory of parent job, setting it to previous working directory.

    return success if the directory is successfully changed, otherwise raises an exception.\n"))

    (hashtable-set! t "command" (string->utf8 " [command-name [arg ...]]
    execute a command with specified arguments.

    useful if COMMAND-NAME has been shadowed by an alias or by a builtin with the same name.

    return exit status of executed command, or failure if no such command was found.\n"))

    (hashtable-set! t "exec" (string->utf8 " [cmd [arg ...]]
    replace the current shell with the command CMD ARG ...

    if CMD ARG ... are not specified, any redirections take effect in the current shell.

    if CMD is not specified, return success.
    if CMD is specified, on success does not return. On failure, returns failure error code.\n"))

    (hashtable-set! t "exit" (string->utf8 " [int ...]
    exit the shell with C exit status INT, or 0 if not specified.

    does not return.\n"))

    (hashtable-set! t "export" (string->utf8 " [var ...]
    show or export environment variables

    without arguments,          'export' writes all exported environment variables
                                 of parent job to standard output.
    with one or more arguments, 'export VAR ...' marks specified environment variables
                                 as exported in parent job.

    return success.\n"))

    (hashtable-set! t "fg"      (string->utf8 " job-id
    move a job to the foreground.

    return success if job-id was found, otherwise return failure.\n"))

    (hashtable-set! t "global"     (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with its parent temporarily set to the shell itself.

    useful mostly for builtins 'cd' 'export' 'set' 'pwd' 'unexport' 'unset'
    that show or alter the current directory or the environment variables of their parent job.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "jobs"       (string->utf8 " [arg ...]
    ignore arguments. write jobs and their status to standard output.

    return success.\n"))

    (hashtable-set! t "parent"     (string->utf8 " [builtin-name [arg ...]]
    execute a builtin with its parent temporarily set to its grandparent.
    if used multiple times, as for example \"parent parent cd ..\", the effects are cumulative.

    useful mostly for builtins 'cd' 'export' 'set' 'pwd' 'unexport' 'unset'
    that show or alter the current directory or the environment variables of their parent job.

    return exit status of executed builtin, or failure if no such builtin was found.\n"))

    (hashtable-set! t "pwd"        (string->utf8 " [job-id]
    write the current directory of specified job to standard output.
    if job is not specified, defaults to parent job.

    return success if job-id was found or not specified, otherwise return failure.\n"))

    (hashtable-set! t "set"        (string->utf8 " [var [value]]'
    show or set environment variables of parent job.

    without arguments,  'set' writes all exported and private environment variables
                                 of parent job to standard output.
    with one argument,  'set VAR' writes specified environment variable of parent job
                                 to standard output.
    with two arguments, 'set VAR VALUE' sets specified environment variable of parent job.

    return success, unless 'set VAR' is executed and no such variable is found.\n"))

    (hashtable-set! t "split-at-0" (string->utf8 " alias-or-builtin-or-cmd [arg ...]
    split each ARG ... after each NUL character i.e. Unicode codepoint U+0000,
    and execute the specified alias, builtin or command
    with arguments set to the result of such splitting.

    useful to pass as arguments the NUL-terminated filenames produced by another command,
    as for example 'split-at-0 editor $(find -name \\*.txt -print0)'

    return exit status of executed alias, builtin or command.\n"))

    (hashtable-set! t "unalias"    (string->utf8 " [name ...]
    remove each NAME ... from the list of defined aliases.

    return success.\n"))

    (hashtable-set! t "unexport"   (string->utf8 " [var ...]
    mark each VAR ... environment variable as private in parent job.

    return success.\n"))

    (hashtable-set! t "unsafe"     (string->utf8 " [alias-or-builtin-or-cmd [arg ...]]
    execute the specified alias, builtin or command.

    this builtin is only needed when ALIAS-OR-BUILTIN-OR-CMD is a non-constant expression,
    as for example a wildcard or the value of an environment variable.

    return exit status of executed alias, builtin or command.\n"))

    (hashtable-set! t "unset"      (string->utf8 " [var ...]
    remove each VAR ... environment variable from parent job.

    return success.\n"))

  )

) ; close begin

) ; close library
