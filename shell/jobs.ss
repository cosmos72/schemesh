;;; Copyright (C) 2023 by Massimiliano Ghilardi
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
    sh-job? sh-job sh-job-id sh-job-status sh-jobs
    sh-cmd sh-cmd* sh-cmd? sh-multijob?
    sh-concat sh-env-copy sh-env sh-env! sh-env-unset! sh-globals sh-global-env
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->vector-of-bytevector0
    sh-cwd sh-cwd-set! sh-cd sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-list sh-subshell
    sh-fd-redirect! sh-fds-redirect!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) break foreign-procedure format fx1+ fx1-
                       make-format-condition record-writer reverse! void)
    (only (schemesh bootstrap) assert* debugf raise-errorf until while)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix fd)
    (schemesh posix pid)
    (schemesh posix signal)
    (schemesh shell paths))

;; Define the record type "job"
(define-record-type
  (job %make-job sh-job?)
  (fields
    (mutable id job-id %job-id-set!) ; fixnum: job id in (sh-globals), #f if not set
    (mutable pid)               ; fixnum: process id,       -1 if unknown
    (mutable pgid)              ; fixnum: process group id, -1 if unknown
    (mutable last-status)       ; cons: last known status
    (mutable to-redirect-fds)   ; vector: fds to redirect between fork() and
                                ;         (subshell-proc)
    (mutable to-redirect-files) ; vector of files to open before fork()
    (mutable to-close-fds)      ; list: fds to close after spawn
    start-proc      ; #f or procedure to run in main process.
                    ; receives as argument job followed by options.
    step-proc       ; #f or procedure.
                    ; For multijobs, will be called when a child job changes status.
                    ; For cmds, will be called in fork()ed child process and
                    ; receives as argument job followed by options.
                    ; For cmds, its return value is passed to (exit-with-job-status)
    (mutable cwd)               ; charspan: working directory
    (mutable env)               ; hashtable: overridden env variables, or '()
    (mutable parent)))          ; parent job, contains default values of env variables
                                ; and default redirections

;; return the job-id of a job, or #f if not set
(define (sh-job-id j)
  (job-id j))

;; Define the record type "cmd"
(define-record-type
  (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields argv)) ; vector of bytevectors, each #\nul terminated

;; Define the record type "multijob"
(define-record-type
  (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-list 'sh-subshell 'sh-global
    (mutable current-child-index) ; -1 or index of currently running child job
    children))          ; span: children jobs.


;; Define the variable sh-globals, contains the global job.
;; Jobs started with (sh-start) will be children of sh-globals.
;
;; Variable may be set! to a different value in subshells.
(define sh-globals
  ;; assign job-id 0 to sh-globals itself.
  ;;
  ;; waiting for sh-globals to exit is not useful:
  ;; pretend it already exited with unknown exit status
  (%make-multijob 0 (get-pid) (get-pgid 0) '(unknown . 0) (vector 0 1 2) (vector) '()
    #f #f ; start-proc step-proc
    ; current directory
    (string->charspan* ((foreign-procedure "c_get_cwd" () scheme-object)))
    (make-hashtable string-hash string=?) #f
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


;; Return #t if job-status is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if job-status is (void) and allowed-list also contains 'exited
;; then return #t because (void) is a shortcut for '(exited . 0)
(define (job-status-member? job-status allowed-list)
  (memq (job-status->kind job-status) allowed-list))


;; Return #t if (job-last-status j) is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if (job-last-status j) is (void) and allowed-list also contains 'exited
;; then return #t because (void) is a shortcut for '(exited . 0)
(define (job-has-status? j allowed-list)
  (job-status-member? (job-last-status j) allowed-list))


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



;; unset the job-id of a job,
;; and remove it from (multijob-children sh-globals).
;; Return job status
(define (job-id-unset! j)
  (assert* 'job-id-unset! (sh-job? j))
  (when (job-id j)
    (let* ((children (multijob-children sh-globals))
           (child-n  (span-length children))
           (id       (job-id j)))
      (when (fx<? -1 id child-n)
        (span-set! children id #f)
        (until (or (span-empty? children) (span-back children))
          (span-erase-back! children 1))))
    (%job-id-set! j #f))
  (job-last-status j))




;; If job has no job-id, assign a job-id to it, by appending it to (multijob-children sh-globals).
;; If job status is '(running . #f) update it to '(running . job-id)
;; Return updated job status
(define (job-id-set! j)
  (assert* 'job-id-set! (sh-job? j))
  (unless (job-id j)
    (let* ((mjob     sh-globals)
           (children (multijob-children mjob))
           (id       (span-length children))
           (status   (job-last-status j))
           (kind     (if (pair? status) (car status) 'exited)))
      (span-insert-back! children j)
      (%job-id-set! j id)
      ;; replace job status '(running . #f) -> '(running . job-id)
      (when (and (eq? kind 'running) (not (eqv? id (cdr status))))
        (job-last-status-set! j (cons 'running id)))))
  (job-last-status j))


;; if job is running or stopped, then create a new job-id for it.
;; if job has terminated, clear its job id.
;; Also replace any job status '(running . #f) -> '(running . job-id)
;; Return updated job status.
(define (job-id-set-or-unset-as-needed! j)
  (let ((status (job-last-status j)))
    (case (job-status->kind status)
      ((running stopped)
        (job-id-set! j))
      ((exited killed unknown)
        (job-id-unset! j))
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


;; return charspan containing current directory,
;; or charspan containing current directory of specified job-or-id.
(define sh-cwd
  (case-lambda
    (()          (job-cwd sh-globals))
    ((job-or-id) (job-cwd (sh-job job-or-id)))))


;; set the current directory of specified job or job-id to specified path.
;; path must be a string or charspan.
;;
;; if job-or-id resolves to sh-globals, it is equivalent to (sh-cd path).
;;
;; in all other cases, path is taken as-is, i.e. it is not normalized
;; and is not validated against filesystem contents.
(define (sh-cwd-set! job-or-id path)
  (let ((j (sh-job job-or-id)))
    (if (eq? j sh-globals)
      (sh-cd path)
      (job-cwd-set! j (if (charspan? path) path (string->charspan* path))))))


;; change current directory to specified path.
;; path must be a string or charspan.
(define sh-cd
  (let ((c_chdir (foreign-procedure "c_chdir" (scheme-object) int)))
    (lambda (path)
      (let* ((suffix (if (charspan? path) path (string->charspan* path)))
             (dir (if (sh-path-absolute? suffix)
                      (sh-path->subpath suffix)
                      (sh-path-append (sh-cwd) suffix)))
             (err (c_chdir (text->bytevector0 (charspan->string dir)))))
        (if (= err 0)
          (job-cwd-set! sh-globals dir)
          (raise-errorf 'cd "~a: ~a" path (c-errno->string err)))))))



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


;; call (proc j) on given job and each of its
;; parents. Stops iterating if (proc) returns #f.
(define (job-parents-iterate job-or-id proc)
  (do ((parent (sh-job job-or-id) (job-parent parent)))
      ((or (not (sh-job? parent)) (not (proc parent))))))


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


;; Create a cmd to later spawn it. Each argument must be a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd . program-and-args)
  (%make-cmd #f -1 -1 '(new . 0) (vector 0 1 2) (vector) '()
    cmd-spawn #f  ; start-proc step-proc
    (sh-cwd)      ; job working directory - initially current directory
    '()           ; overridden environment variables - initially none
    sh-globals    ; parent job - initially the global job
    (list->cmd-argv program-and-args)))


;; Create a cmd to later spawn it. Each argument must be a string, bytevector or symbol.
;; Symbol '= indicates an environment variable assignment, and must be followed
;; by the variable name (a string or bytevector) and its value (a string or bytevector).
;; All other symbols indicates a redirection and must be followed by a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd* . program-and-args)
  ;; FIXME: implement environment variable assignments NAME = VALUE
  ;; FIXME: implement redirections [N]< [N]<> [N]<&M [N]> [N]>> [N]>&M
  (apply sh-cmd program-and-args))


;; concatenate strings and/or closures (lambda (job) ...) that return strings
(define (sh-concat job . args)
  (let ((strings '()))
    (list-iterate args
      (lambda (arg)
        (set! strings
          (cons
            (if (procedure? arg) (arg job) arg)
            strings))))
    (apply string-append (reverse! strings))))



;; return global environment variables
(define (sh-global-env)
  (job-env sh-globals))


;; Return direct environment variables of job, creating them if needed.
;; Returned hashtable does not include default variables,
;; i.e. the ones inherited from parent jobs.
(define (job-direct-env job-or-id)
  (let* ((job (sh-job job-or-id))
         (vars (job-env job)))
    (unless (hashtable? vars)
      (set! vars (make-hashtable string-hash string=?))
      (job-env-set! job vars))
    vars))


;; Return a copy of job's environment variables,
;; including default variables inherited from parent jobs.
;; Argument which must be one of:
;;   'exported: only exported variables are returned.
;;   'all : unexported variables are returned too.
(define (sh-env-copy job-or-id which)
  (assert* 'sh-env-copy (memq which '(exported all)))
  (let* ((jlist (job-parents-revlist job-or-id))
         (vars (make-hashtable string-hash string=?))
         (also-unexported? (eq? 'all which))
         (only-exported? (not also-unexported?)))
    (list-iterate jlist
      (lambda (j)
        (let ((env (job-env j)))
          (when (hashtable? env)
            (hashtable-iterate env
              (lambda (cell)
                (let ((name (car cell))
                      (flag (cadr cell))
                      (val  (cddr cell)))
                  (cond
                    ((or (eq? 'delete flag)
                         (and only-exported? (eq? 'private flag)))
                      (hashtable-delete! vars name))
                    ((or (eq? 'export flag)
                         (and also-unexported? (eq? 'private flag)))
                      (hashtable-set! vars name val))))))))))
    vars))


;; Return string environment variable named "name" of specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;; If name is not found, return default

(define (sh-env* job-or-id name default)
  (job-parents-iterate job-or-id
    (lambda (j)
      (let* ((vars (job-env j))
             (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))
        (when (pair? elem)
          (unless (eq? 'delete (car elem))
            (set! default (cdr elem)))
          #f)))) ; name found, stop iterating
  default)


;; Return string environment variable named "name" of specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;; If name is not found, return default if specified - otherwise return ""
(define sh-env
  (case-lambda
    ((job-or-id name)         (sh-env* job-or-id name ""))
    ((job-or-id name default) (sh-env* job-or-id name default))))

(define (sh-env! job-or-id name val)
  (assert* 'sh-env! (string? val))
  (let* ((vars (job-direct-env job-or-id))
         (elem (hashtable-ref vars name #f)))
    (if (pair? elem)
      (set-cdr! elem val)
      (hashtable-set! vars name (cons 'private val)))))


;; Note: (sh-env-unset!) inserts an entry that means "deleted",
;; in order to override any parent job's environment variable
;; with the same name.
(define (sh-env-unset! job-or-id name)
  (let ((vars (job-direct-env job-or-id)))
    (hashtable-set! vars name (cons 'delete ""))))

(define (sh-env-exported? job-or-id name)
  (let ((ret #f))
    (job-parents-iterate job-or-id
      (lambda (j)
        (let* ((vars (job-env j))
               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))
          (when (pair? elem)
            (set! ret (eq? 'export (car elem)))
            #f)))) ; name found, stop iterating
    ret))

(define (sh-env-export! job-or-id name exported?)
  (assert* 'sh-env-export! (boolean? exported?))
  (let* ((j (sh-job job-or-id))
         ; val may be in a parent environment
         (val (sh-env j name))
         (export (if exported? 'export 'private)))
    ; (job-direct-env j) creates job environment if not yet present
    (hashtable-set! (job-direct-env j) name (cons export val))))

;; combined sh-env! and sh-env-export!
(define (sh-env-set+export! job-or-id name val exported?)
  (assert* 'sh-env-set+export! (string? val))
  (assert* 'sh-env-set+export! (boolean? exported?))
  (let* ((vars (job-direct-env job-or-id))
         (export (if exported? 'export 'private)))
    (hashtable-set! vars name (cons export val))))

;; Repeatedly call C function c_environ_ref() and store returned (key . value)
;; environment variables into (sh-global-env).

;; This function is usually only called once, during initialization of Scheme library
; (schemesh shell) below.
(define c-environ->sh-global-env
  (let ((c-environ-ref (foreign-procedure "c_environ_ref" (uptr) scheme-object)))
    (lambda ()
      (do ((i 1 (fx+ i 1))
           (entry (c-environ-ref 0) (c-environ-ref i)))
          ((not (pair? entry)))
        (sh-env-set+export! sh-globals (car entry) (cdr entry) #t)))))


;; Extract environment variables from specified job and all its parents,
;; and convert them to a vector of bytevector0.
;; Argument which must be one of:
;; 'exported: only exported variables are returned.
;; 'all : unexported variables are returned too.
(define (sh-env->vector-of-bytevector0 job-or-id which)
  (string-hashtable->vector-of-bytevector0 (sh-env-copy job-or-id which)))


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



;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, does not close (job-to-close-fds j)
;; - thus calling it manually leaks file descriptors - and does not register job
;;   into global (pid->job) table nor into global job-id table.
;;
;; Description:
;; Start a cmd i.e. fork() and exec() an external process, optionally inserting it into
;; an existing process group.
;;
;; The new process is started in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define cmd-spawn
  (let ((c-spawn-pid (foreign-procedure "c_spawn_pid"
                        (scheme-object scheme-object scheme-object int) int)))
    (lambda (c . options)
      (assert* 'sh-start (sh-cmd? c))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-spawn-pid
                    (cmd-argv c)
                    (job-to-redirect-fds c)
                    (sh-env->vector-of-bytevector0 c 'exported)
                    process-group-id)))
        (when (< ret 0)
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c (if (> process-group-id 0) process-group-id ret))))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not redirect file descriptors, does not close (job-to-close-fds j)
;; - thus calling it manually leaks file descriptors - and does not register job
;; into global (pid->job) table nor into global job-id table.
;;
;; Description:
;; Start a generic job. Stored in (job-start-proc j) and called by (sh-start).
;;
;; Options are ignored.
(define (multijob-start j . options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (let ((children (multijob-children j)))
    (if (span-empty? children)
      ; FIXME: a lonely (sh-or) with no children should fail with '(exited . 255)
      (job-last-status-set! j (void)) ; no children => nothing to do => job exited successfully
      (begin
        (multijob-current-child-index-set! j 0)
        (job-last-status-set! j '(running . #f))
        (apply start/any (span-ref children 0) options)))))
        ; set job status as running. Do not assign a job-id.


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, does not close (job-to-close-fds j)
;; - thus calling it manually leaks file descriptors - and does not register job
;; into global (pid->job) table nor into global job-id table.
;;
;; Description:
;; Start a subshell job, optionally inserting it into an existing process group.
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (job-subshell-proc j)
;; passing the job j as only argument,
;; then will call (exit-with-job-status) with the value returned by (job-subshell-proc j)
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new subshell will be inserted
;;     into the corresponding process group id - which must already exist.
(define multijob-spawn-subshell
  (let ((c-fork-pid (foreign-procedure "c_fork_pid" (scheme-object int) int)))
    (lambda (j . options)
      (assert* 'sh-start (procedure? (job-step-proc j)))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-fork-pid
                    (job-to-redirect-fds j)
                    process-group-id)))
        (cond
          ((< ret 0) ; fork() failed
            (raise-c-errno 'sh-start 'fork ret))
          ((= ret 0) ; child
            (let ((status '(exited . 255)))
              (dynamic-wind
                (lambda () ; run before body
                  (let ((pid  (get-pid))
                        (pgid (get-pgid 0)))
                    (job-pid-set!  j pid)
                    (job-pgid-set! j pgid)
                    ; this process now "is" the job j => update sh-globals' pid and pgid
                    (job-pid-set!  sh-globals pid)
                    (job-pgid-set! sh-globals pgid)
                    ; cannot wait on our own process
                    (job-last-status-set! j '(unknown . 0))))
                (lambda () ; body
                  ; sh-subshell stores multijob-run-subshell in (job-step-proc j)
                  (set! status ((job-step-proc j) j (void))))
                (lambda () ; run after body, even if it raised a condition
                  (exit-with-job-status status)))))
          ((> ret 0) ; parent
            (job-pid-set! j ret)
            (job-pgid-set! j (if (> process-group-id 0) process-group-id ret))))))))


;; Return #t if job was already started, otherwise return #f
(define (job-started? j)
  (or
    ;; a job with valid pid and valid pgid is surely started
    (and (fx>? (job-pid j) 0) (fx>? (job-pgid j) 0))
    ;; a job with status 'running or 'stopped is started
    (job-has-status? j '(running stopped))))


;; Start a cmd or a job.
;; If job parent is sh-globals, return '(running . job-id).
;; Otherwise return '(running . #f)
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 then the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define (sh-start j . options)
  (job-id-set! j)
  (apply start/any j options))

;; Internal functions called by (sh-start)
(define (start/any j . options)
  ; (debugf "start/any ~s ~s~%" j options)
  (when (job-started? j)
    (if (job-id j)
      (raise-errorf 'sh-start "job already started with job id ~s" (job-id j))
      (raise-errorf 'sh-start "job already started")))
  (let ((proc (job-start-proc j)))
    (unless (procedure? proc)
      (raise-errorf 'sh-start "cannot start job, it has bad or missing job-start-proc: ~s" j))
    (apply proc j options))
  (fd-close-list (job-to-close-fds j))
  (when (fx>? (job-pid j) 0)
    (pid->job-set! (job-pid j) j))        ; add job to pid->job table
  (let ((ret (cons 'running (job-id j)))) ; job can now be waited-for
    (job-last-status-set! j ret)
    ret))

;; Convert pid-wait-result to a symbolic job-status:
;;
;; If pid-wait-result is '() i.e. process status did not change,
;; return '(running . #f) indicating process is still running.
;;
;; If pid-wait-result is a pair (pid . exit-status) where exit-status is:
;;   not a fixnum, or < 0 => return (cons 'unknown exit-status)
;;   0                    => return (void)
;;   1..255               => return (cons 'exited  exit-status)
;;   256 + kill_signal    => return (cons 'killed  signal-name)
;;   512 + stop_signal    => return (cons 'stopped signal-name)
;;   >= 768               => return (cons 'unknown (fx- exit-status 768))
;;
;; Otherwise return (cons 'unknown pid-wait-result)
(define (pid-wait->job-status pid-wait-result)
  (cond
    ((pair? pid-wait-result)
      (let ((num (cdr pid-wait-result)))
        (cond ((or (not (fixnum? num)) (fx<? num 0)) (cons 'unknown num))
              ((fx=? num   0) (void))
              ((fx<? num 256) (cons 'exited  num))
              ((fx<? num 512) (cons 'killed  (signal-number->name (fxand num 255))))
              ((fx<? num 768) (cons 'stopped (signal-number->name (fxand num 255))))
              (#t             (cons 'unknown (fx- num 768))))))
    ((null? pid-wait-result)
      '(running . #f))
    (#t
      (cons 'unknown pid-wait-result))))


;; Common implementation of (sh-fg) (sh-bg) (sh-wait) (sh-job-status)
(define (advance-job-or-id mode job-or-id)
  (assert* 'advance-job-or-id (memq mode '(sh-fg sh-bg sh-wait sh-wait+sigcont sh-subshell sh-job-status)))
  (let ((j (sh-job job-or-id)))
    (when (job-has-status? j '(new running stopped))
      (advance-job/any mode j)
    (job-id-set-or-unset-as-needed! j))))


;; Internal function called by (advance-job-or-id) (advance-job/multijob)
(define (advance-job/any mode j)
  (cond
    ((job-has-status? j '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (raise-errorf mode  "job not started yet: ~s" j))
    ((fx>? (job-pid j) 0)
      (advance-job/pid mode j))
    ((sh-multijob? j)
      (advance-job/multijob mode j))
    (#t
      ; unexpected job type or status, just assume it exited successfully
      (job-last-status-set! j (void))
      (void))))


(define %pgid-foreground
  (let ((c-pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int)))
    (lambda (caller expected-pgid new-pgid)
      (let ((err (c-pgid-foreground expected-pgid new-pgid)))
        (when (< err 0)
          (raise-c-errno caller 'tcsetpgrp err))
        err))))


(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  caller expected-pgid new-pgid body ...)
      (let ((_caller caller)
            (_expected-pgid expected-pgid)
            (_new-pgid new-pgid))
        (dynamic-wind
          (lambda () ; run before body
            (%pgid-foreground _caller _expected-pgid _new-pgid))
          (lambda ()
            body ...)
          (lambda () ; run after body
            ; try to restore sh-globals as the foreground process group
            (%pgid-foreground _caller _new-pgid _expected-pgid)))))))


;; Internal function called by (advance-job/any)
(define (advance-job/pid mode j)
  ; (debugf "advance-job/pid > ~s ~s~%" mode j)
  (cond
    ((job-has-status? j '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (raise-errorf mode "job not started yet: ~s" j))
    (#t
      (let ((pid  (job-pid j))
            (pgid (job-pgid j)))
        (if (memq mode '(sh-fg sh-wait sh-wait+sigcont sh-subshell))
          (with-foreground-pgid mode (job-pgid sh-globals) pgid
            (advance-job/pid/maybe-sigcont mode j pid pgid)
            (advance-job/pid/wait mode j pid pgid))
          (begin
            (advance-job/pid/maybe-sigcont mode j pid pgid)
            (advance-job/pid/wait mode j pid pgid)))))))


;; Internal function called by (advance-job/pid)
(define (advance-job/pid/maybe-sigcont mode j pid pgid)
  (assert* mode (fx>? pid 0))
  (assert* mode (fx>? pgid 0))
  (when (memq mode '(sh-fg sh-bg sh-wait+sigcont))
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont)))



;; Internal function called by (advance-job/pid)
(define (advance-job/pid/wait mode j pid pgid)
  ; no need to wait for ALL processes in job's process group:
  ; the only case where we spawn multiple processes in the same process group
  ; is a pipe i.e {a | b | c ...} and in such case we separately wait on the process id
  ; of each spawned process
  (let* ((may-block (if (memq mode '(sh-bg sh-job-status)) 'nonblocking 'blocking))
         (status    (pid-wait->job-status (pid-wait (job-pid j) may-block)))
         (kind      (job-status->kind status)))
    ; if may-block is 'non-blocking, status may be '(running . #f)
    ; indicating job status did not change i.e. it's (expected to be) still running
    (case kind
      ((running)
        ; if status is '(running . #f), try to return '(running . job-id)
        (job-last-status j))
      ((exited killed unknown)
        ; job exited, clean it up in case user wants to later respawn it
        (pid->job-delete! (job-pid j))
        (job-pid-set!  j -1)
        (job-pgid-set! j -1)
        (job-last-status-set! j status)
        ;; returns job status
        (job-id-unset! j))
      ((stopped)
        ; process is stopped.
        ; if mode is sh-wait or sh-wait+sigcont, call (break)
        ; then, if mode is sh-wait sh-wait+sigcont or sh-subshell, wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (memq mode '(sh-wait sh-wait+sigcont sh-subshell))
          (begin
            (when (memq mode '(sh-wait sh-wait+sigcont))
              (advance-job/pid/break mode j pid pgid))
            (advance-job/pid/wait mode j pid pgid))
          (begin
            (job-last-status-set! j status)
            status)))
      (else
        (raise-errorf mode "job not started yet: ~s" j)))))


;; Internal function called by (advance-job/pid/wait)
;; when job is stopped in mode 'sh-wait or 'sh-wait+sigcont:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (advance-job/pid/break mode j pid pgid)
  (let ((break-returned-normally? #f)
        (global-pgid (job-pgid sh-globals)))
    (dynamic-wind
      (lambda () ; before body
        (%pgid-foreground mode pgid global-pgid))
      (lambda () ; body
        (format #t "; job ~s pid ~s stopped        ~s~%" pid pgid j)
        (break)
        (set! break-returned-normally? #t))
      (lambda ()
        (%pgid-foreground mode global-pgid pgid)
        ; send SIGCONT to job's process group, if present.
        ; otherwise send SIGCONT to job's process id. Both may raise error
        (when (eq? mode 'sh-wait+sigcont)
          (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont))
        (unless break-returned-normally?
          (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigint))))))



;; Internal function called by (advance-job/any)
(define (advance-job/multijob mode mj)
  (let* ((child (sh-multijob-child-ref mj (multijob-current-child-index mj)))
         ;; call (advance-job/any) on child
         (child-status (if (sh-job? child) (advance-job/any mode child) (void)))
         (step-proc (job-step-proc mj)))
    ; (debugf "advance-job/multijob > ~s ~s child=~s child-status=~s step-proc=~s~%" mj may-block child child-status step-proc)
    (cond
      ((or (not step-proc) (job-status-stops-or-ends-multijob? child-status))
        ; propagate child exit status and return
        (job-last-status-set! mj child-status)
        child-status)
      ((job-status-member? child-status '(exited killed unknown))
        ; child exited: advance multijob by calling (job-step-proc)
        ; then call (advance-job/multijob) again
        (step-proc mj child-status)
        (if (job-has-status? mj '(running))
          (advance-job/multijob mode mj)
          (job-last-status mj)))
      ((job-status-member? child-status '(running))
        ; child is still running. propagate child status and return
        (let ((status (cons 'running (job-id mj)))) ; (job-id mj) may still be #f
          (job-last-status-set! mj status)
          status))
      ((job-status-member? child-status '(stopped))
        ; child is stopped.
        ; if mode is sh-wait or sh-subshell, wait for it again.
        ; otherwise propagate child status and return
        (if (memq mode '(sh-wait sh-wait+sigcont sh-subshell))
          (advance-job/multijob mode mj)
          (begin
            (job-last-status-set! mj child-status)
            child-status)))
      (#t
        (raise-errorf mode "child job not started yet: ~s" child)))))


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
  (let ((j (sh-job job-or-id)))
    (if (job-has-status? j '(new))
      (job-last-status j)
      (advance-job-or-id 'sh-job-status j))))


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
  (advance-job-or-id 'sh-bg job-or-id))


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
  (advance-job-or-id 'sh-fg job-or-id))


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
  (advance-job-or-id (if send-sigcont? 'sh-wait+sigcont 'sh-wait) job-or-id))


;; Start a job and wait for it to exit or stop.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i job . options)
  (apply sh-start job options)
  (sh-fg job))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run job . options)
  (apply sh-start job options)
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
;; Reads job's standard output and returns it converted to bytevector.
(define (sh-run/bytes job . options)
  (apply sh-run job options)
  ; TODO: implement
  #vu8())


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Reads job's standard output and returns it converted to UTF-8b string.
(define (sh-run/string job . options)
  (apply sh-run job options)
  ; TODO: implement
  "")



;; Create or remove a file description redirection for cmd or job
(define (sh-fd-redirect! job-id child-fd existing-fd-or-minus-1)
  (when (or (not (fixnum? child-fd)) (< child-fd 0))
    (raise-errorf 'sh-fd-redirect! "invalid redirect fd: ~s" child-fd))
  (let* ((j (sh-job job-id))
         (old-fds (job-to-redirect-fds j))
         (old-n (vector-length old-fds)))
    (when (fx<=? old-n child-fd)
      (let* ((new-n (max (fx1+ child-fd) (fx* 2 old-n)))
             (new-fds (make-vector new-n -1))) ; fill with -1 i.e. no redirection
        (do ((i 0 (fx1+ i)))
            ((>= i old-n))
          (vector-set! new-fds i (vector-ref old-fds i)))
        (job-to-redirect-fds-set! j new-fds)))
    (vector-set! (job-to-redirect-fds j) child-fd existing-fd-or-minus-1)))

;; Create or remove multiple file description redirections for cmd or job
(define (sh-fds-redirect! j child-fds existing-fd-or-minus-1)
  (do ((child-cons child-fds (cdr child-cons)))
      ((eq? '() child-cons))
    (sh-fd-redirect! j (car child-cons) existing-fd-or-minus-1)))


;; Create a multijob to later start it.
;; Internal function, accepts an optional function to validate each element in children-jobs

(define (make-multijob kind validate-job-proc start-proc next-proc . children-jobs)
  (assert* 'make-multijob (symbol? kind))
  (assert* 'make-multijob (procedure? start-proc))
  (when next-proc
    (assert* 'make-multijob (procedure? next-proc)))
  (when validate-job-proc
    (do ((tail children-jobs (cdr tail)))
        ((null? tail))
      (validate-job-proc kind (car tail))))
  (%make-multijob #f -1 -1 '(new . 0) (vector 0 1 2) (vector) '()
    start-proc    ; executed to start the job
    next-proc     ; executed when a child job changes status
    (sh-cwd)      ; job working directory - initially current directory
    '()           ; overridden environment variables - initially none
    sh-globals    ; parent job - initially the global job
    kind
    -1            ; no child running yet
    (list->span children-jobs)))

(define (assert-is-job who j)
  (assert* who (sh-job? j)))

;; Create a multijob to later start it. Each element in children-jobs must be a sh-job or subtype.

(define (sh-and . children-jobs)
  (apply make-multijob 'sh-and assert-is-job multijob-start multijob-step/and children-jobs))

(define (sh-or . children-jobs)
  (apply make-multijob 'sh-or  assert-is-job multijob-start multijob-step/or children-jobs))

;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-list
    (lambda (caller j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* caller (sh-job? j))))
    multijob-start
    multijob-step/list
    children-jobs-with-colon-ampersand))

;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-subshell
    (lambda (caller j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* caller (sh-job? j))))
    multijob-spawn-subshell
    multijob-run-subshell ; executed in child process
    children-jobs-with-colon-ampersand))


;; Run next child job in a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
(define (multijob-step/and mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child-n (span-length (multijob-children mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (sh-ok? prev-child-status) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (start/any child))
      (begin
        ; previous child failed, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-last-status-set! mj prev-child-status)))))





;; Run next child job in a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (multijob-step/or mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child-n (span-length (multijob-children mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (not (sh-ok? prev-child-status)) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (start/any child))
      (begin
        ; previous child successful, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-last-status-set! mj prev-child-status)))))




;; Run next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (multijob-step/list mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child-n (span-length (multijob-children mj)))
         (done?   #f))
    ; (debugf "multijob-step/list > ~s idx=~s prev-child-status=~s~%" mj (fx1- idx) prev-child-status)
    (assert* 'multijob-step/list (job-status-member? prev-child-status '(exited killed unknown)))
    (assert* 'multijob-step/list (fx>? idx 0))
    (until (or done? (fx>? idx child-n))
      (let ((child (sh-multijob-child-ref mj idx)))
        ; (debugf "multijob-step-list status = ~s, start child ~s = ~s~%" (job-last-status mj) idx child)
        (cond
          ((sh-job? child)
            ; start next child job
            (multijob-current-child-index-set! mj idx)
            (start/any child)
            (set! idx (fx1+ idx))
            ; all done, unless job is followed by '&
            ; in such case, continue spawning jobs
            (set! done? (not (eq? '& (sh-multijob-child-ref mj idx)))))
          (child
            ; child is a symbol, either & or ;
            (set! idx (fx1+ idx)))
          (#t
            ; end of children reached. propagate status of last sync child
            (multijob-current-child-index-set! mj -1)
            (job-last-status-set! mj prev-child-status)
            (set! done? #t)))))))



;; Executed in child process:
;; run a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-subshell), implements runtime behavior of shell syntax [ ... ]
(define (multijob-run-subshell mj dummy-prev-child-status)
  ; (debugf "%multijob-subshell/run ~s status = ~s~%" mj (job-last-status mj))
  (let ((children   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status (void)))
    (span-iterate children
      (lambda (i job)
        (when (sh-job? job)
          ; run child job in parent's process group
          (start/any job pgid)
          ; wait for child job to exit, unless it's followed by '&
          (unless (eq? '& (sh-multijob-child-ref mj (fx1+ i)))
            (set! status (advance-job-or-id 'sh-subshell job))))
        #t)) ; keep iterating
    status))



;; customize how "job" objects are printed
(record-writer (record-type-descriptor job)
  (lambda (obj port writer)
    (display "(sh-job " port)
    (writer (job-step-proc obj) port)
    (display ")" port)))

;; customize how "cmd" objects are printed
(record-writer (record-type-descriptor cmd)
  (lambda (obj port writer)
    (display "(sh-cmd" port)
    (vector-iterate (cmd-argv obj)
       (lambda (i arg)
         (display #\space port)
         (write-bytevector0 arg port)))
    (display ")" port)))

;; customize how "multijob" objects are printed
(record-writer (record-type-descriptor multijob)
  (lambda (obj port writer)
    (display #\( port)
    (display (multijob-kind obj) port)
    (span-iterate (multijob-children obj)
       (lambda (i child)
         (display #\space port)
         (display child port)))
    (display #\) port)))

(begin
  (c-environ->sh-global-env))


) ; close library
