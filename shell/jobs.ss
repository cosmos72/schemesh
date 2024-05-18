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
    sh-cwd sh-cwd-set! sh-cd sh-consume-sigchld sh-start sh-bg sh-fg sh-wait sh-ok?
    sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-list sh-subshell
    sh-fd-redirect! sh-fds-redirect!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) break foreign-procedure format fx1+ fx1-
                       make-format-condition record-writer reverse! void)
    (only (schemesh bootstrap) assert* debugf raise-errorf until)
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
    (mutable id)                ; fixnum: job id in (sh-globals), #f if not set
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
    (mutable current-child-index) ; #f or index of currently running child job
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
    'sh-global #f (span #t))) ; skip job-id 0, is used by sh-globals itself

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

;; Remove a job or job-id from a multijob
(define (multijob-child-delete! mjob job-or-id)
  (let* ((arr (multijob-children mjob))
         (j   (sh-job job-or-id))
         (idx
           (if (fixnum? job-or-id)
             (when (and (fx>=? job-or-id 0) (fx<? job-or-id (span-length arr)))
               job-or-id)
             (span-find arr 0 (span-length arr) (lambda (elem) (eq? elem j))))))
    (when idx
      (span-set! arr idx #f)
      (until (or (span-empty? arr) (span-back arr))
        (span-erase-back! arr 1))
      (when (eq? sh-globals mjob)
        (job-id-set! j #f)))))


;; Add a job to a multijob, extending (multijob-children mjob) as needed.
;; Return index assigned to job, which is the job-id if mjob is sh-globals.
(define (multijob-child-put! mjob j)
  (let* ((arr     (multijob-children mjob))
         (job-id  (span-length arr)))
    (span-insert-back! arr j)
    (when (eq? sh-globals mjob)
      (job-id-set! j job-id))
    job-id))


;; If job has no job-id, assign a job-id to it, by appending it to (multijob-children sh-globals)
;; Return index assigned to job, which is the job-id.
(define (job-id-ensure! j)
  (assert* 'job-id-ensure! (sh-job? j))
  (or (job-id j) (multijob-child-put! sh-globals j)))


;; unset the job-id of a job
(define (job-id-unset! j)
  (when (job-id j)
    (multijob-child-delete! sh-globals (job-id j))))


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
(define (sh-cmd program . args)
  (%make-cmd #f -1 -1 '(new . 0) (vector 0 1 2) (vector) '()
    %cmd-spawn #f ; start-proc step-proc
    (sh-cwd)      ; job working directory - initially current directory
    '()           ; overridden environment variables - initially none
    sh-globals    ; parent job - initially the global job
    (list->cmd-argv (cons program args))))


;; Create a cmd to later spawn it. Each argument must be a string, bytevector or symbol.
;; Symbol '= indicates an environment variable assignment, and must be followed
;; by the variable name (a string or bytevector) and its value (a string or bytevector).
;; All other symbols indicates a redirection and must be followed by a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd* program . args)
  ;; FIXME: implement environment variable assignments NAME = VALUE
  ;; FIXME: implement redirections [N]< [N]<> [N]<&M [N]> [N]>> [N]>&M
  (apply sh-cmd program args))


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

;; return #t if job-status is (void), i.e. if job exited with exit status 0,
;; otherwise return #f
;;
;; job-status must be one of the possible values returned by (sh-fg) or (sh-wait)
(define (sh-ok? job-status)
  (cond
    ((eq? (void) job-status) #t)
    (else
      (assert* 'sh-ok? (pair? job-status))
      #f)))


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
(define %cmd-spawn
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
(define (%multijob-start j . options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (let ((children (multijob-children j)))
    (if (span-empty? children)
      (job-last-status-set! j (void)) ; no children => nothing to do => job exited successfully
      (begin
        (multijob-current-child-index-set! j 0)
        (job-last-status-set! j '(running . #f))
        (apply sh-start (span-ref children 0) options)))))
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
(define %multijob-spawn-subshell
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
                  ; sh-subshell stores %multijob-run-subshell in (job-step-proc j)
                  (set! status ((job-step-proc j) j)))
                (lambda () ; run after body, even if it raised a condition
                  (exit-with-job-status status)))))
          ((> ret 0) ; parent
            (job-pid-set! j ret)
            (job-pgid-set! j (if (> process-group-id 0) process-group-id ret))))))))

;; Return #t if job was already started, otherwise return #f
(define (job-started? j)
  (or
    ;; a cmd with valid pid and valid pgid is surely started
    (and (fx>=? (job-pid j) 0) (fx>=? (job-pgid j) 0))
    ;; a job with status 'running or 'stopped is started
    (job-status-member? (job-last-status j) '(running stopped))))


;; Start a cmd or a job.
;; If job parent is sh-globals, return '(running . job-id).
;; Otherwise return '(running . #f)
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 then the new process will be inserted
;;   into the corresponding process group id - which must already exist.
(define (sh-start j . options)
  ; (debugf "; sh-start ~s~%" j)
  (when (job-started? j)
    (cond
      ((fixnum? (job-id j))
        (raise-errorf 'sh-start "job already started with job id ~s" (job-id j)))
      ((fx>=? (job-pid j) 0)
        (raise-errorf 'sh-start "job already started with pid ~s" (job-pid j)))
      (#t
        (raise-errorf 'sh-start "job already started"))))
  (let ((proc (job-start-proc j)))
    (unless (procedure? proc)
      (raise-errorf 'sh-start "cannot start job, it has bad or missing job-start-proc: ~s" j))
    (apply proc j options))
  (fd-close-list (job-to-close-fds j))
  (pid->job-set! (job-pid j) j)           ; add job to pid->job table
  (when (eq? sh-globals (job-parent j))
    (multijob-child-put! sh-globals j))
  (let ((ret (cons 'running (job-id j)))) ; job can now be waited-for
    (job-last-status-set! j ret)
    ret))


;; Convert pid-wait-result to a symbolic job-status:
;
;; If pid-wait-result is a pair (pid . exit-status) where exit-status is:
;;   not a fixnum, or < 0 => return (cons 'unknown exit-status)
;;   0                    => return (void)
;;   1..255               => return (cons 'exited  exit-status)
;;   256 + kill_signal    => return (cons 'killed  signal-name)
;;   512 + stop_signal    => return (cons 'stopped signal-name)
;;   >= 768               => return (cons 'unknown (fx- exit-status 768))
;
;; If pid-wait-result is '() i.e. process status did not change,
;; return '(running . job-id) indicating process is still running.
;
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


;; Return #t if job-status is a pair whose car is in allowed-list,
;; otherwise return #f;
;;
;; if job-status is (void) and allowed-list also contains 'exited
;; then return #t because (void) is a shortcut for '(exited . 0)
(define (job-status-member? job-status allowed-list)
  (cond ((eq? (void) job-status) (memq 'exited allowed-list))
        ((pair? job-status)      (memq (car job-status) allowed-list))
        (#t #f)))


;; Wait for a cmd or job to exit or stop and return its status, which can be one of:
;;   (cons 'running job-id)   ; may happen only if may-block is 'nonblocking
;;   (void)                   ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Argument may-block must be one of: 'blocking 'nonblocking
;
;; Warning: this is an internal function, and does not set the job
;; as foreground process group. consider calling (sh-fg j) or (sh-wait j) instead.
(define (job-pid-wait j may-block)
  (assert* 'job-pid-wait (memq may-block '(blocking nonblocking)))
  (cond
    ((job-status-member? (job-last-status j) '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (raise-errorf 'job-pid-wait "job not started yet: ~s" j))
    (#t
      ; TODO: wait for ALL processes in job's process group?
      (let* ((ret    (pid-wait (job-pid j) may-block))
             (status (pid-wait->job-status ret)))
        ; if may-block is 'non-blocking, ret may be '() and status will be '(running . #f)
        ; indicating job status did not change i.e. it's (expected to be) still running
        (cond
          ((job-status-member? status '(running))
            (set! status (job-last-status j)))
          ((job-status-member? status '(exited killed unknown))
            ; job exited. it can now be spawned again
            (pid->job-delete! (job-pid j))
            (job-id-unset! j)
            (job-pid-set! j -1)
            (job-pgid-set! j -1)
            (job-last-status-set! j status))
          ((job-status-member? status '(stopped))
            (job-id-ensure! j)))
        status))))


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
    (when (job-status-member? (job-last-status j) '(running))
      ; nonblocking wait for job's pid to exit or stop.
      ; TODO: wait for ALL pids in process group?
      (job-pid-wait j 'nonblocking))
    (job-last-status j)))


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
  (let ((j (sh-job job-or-id)))
    (cond
      ; if job already exited, return its exit status.
      ; if job is stopped, consider as running: we'll send SIGCONT to it below
      ((job-status-member? (job-last-status j) '(exited killed unknown))
        (job-last-status j))
      ((not (job-started? j))
        (raise-errorf 'sh-bg "job not started yet: ~s" j))
      (#t
        ; send SIGCONT to job's process group. may raise error
        (pid-kill (fx- (job-pgid j)) 'sigcont)
        ; nonblocking wait for job's pid to exit or stop.
        ; TODO: wait for ALL pids in process group?
        (job-pid-wait j 'nonblocking)))))

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
  (let ((j (sh-job job-or-id)))
    (cond
      ((fx>=? (job-pid j) 0)
        ; job is a sh-cmd or a sh-subshell: we have a pid to wait for
        (sh-fg/job-pid j))
      ((sh-multijob? j)
        (sh-fg/multijob j))
      (#t
        (job-last-status-set! j (void))
        (void)))))


;; Continue subprocess by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status.
(define (sh-fg/job-pid j)
  (let ((j-pgid (job-pgid j)))
    (cond
      ; if job already exited, return its exit status.
      ; if job is stopped, consider as running: we'll send SIGCONT to it below
      ((job-status-member? (job-last-status j) '(exited killed unknown))
        (job-last-status j))
      ((not (job-started? j))
        (raise-errorf 'sh-fg "job not started yet: ~s" j))
      (#t
        (with-foreground-pgid 'sh-fg (job-pgid sh-globals) j-pgid
          ; send SIGCONT to job's process group. may raise error
          (pid-kill (fx- j-pgid) 'sigcont)
          ; blocking wait for job's pid to exit or stop.
          ; TODO: wait for ALL pids in process group?
          (job-pid-wait j 'blocking))))))

;; Continue a multijob by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status.
(define (sh-fg/multijob j)
  ;; TODO implement
  (void))



;; Continue a job or job-id by sending SIGCONT to it, then wait for it to exit,
;; and finally return its status, which can be one of:
;;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'unknown ...)
;;
;; Does NOT return early if job gets stopped, use (sh-fg) for that.
;;
;; Instead if job gets stopped, calls (proc-on-job-stopped), which defaults to (break),
;; then waits again for the job to exit.
;;
;; Note: if current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define sh-wait
  (case-lambda
    ((job-or-id)                     (sh-wait* job-or-id break))
    ((job-or-id proc-on-job-stopped) (sh-wait* job-or-id proc-on-job-stopped))))


;; Same as (sh-wait), but all arguments are mandatory
(define (sh-wait* job-or-id proc-on-job-stopped)
  ;; TODO: support multijobs
  (when proc-on-job-stopped
    (assert* 'sh-wait (procedure? proc-on-job-stopped)))
  (let* ((j           (sh-job job-or-id))
         (j-pgid      (job-pgid j))
         (global-pgid (job-pgid sh-globals)))
  ; (debugf "; sh-wait ~s~%" j)
  (cond
    ; if job already exited, return its exit status.
    ; if job is stopped, consider as running
    ((job-status-member? (job-last-status j) '(exited killed unknown))
      (job-last-status j))
    ((not (job-started? j))
      (raise-errorf 'sh-wait "job not started yet: " j))
    (#t
      (job-pid-wait-loop j j-pgid global-pgid proc-on-job-stopped)))))




;; internal function used by (sh-wait) to actually wait for a subprocess to exit.
;; return job exit status.
(define (job-pid-wait-loop j j-pgid global-pgid proc-on-job-stopped)
  ; blocking wait for job's pid to exit.
  ; TODO: wait for ALL pids in process group?
  (dynamic-wind
    (lambda () ; before body
      (%pgid-foreground 'sh-wait global-pgid j-pgid))
    (lambda () ; body
      (do ((status #f (begin
                        ; send SIGCONT to job's process group. may raise error
                        (pid-kill (fx- j-pgid) 'sigcont)
                        (job-pid-wait j 'blocking))))
          ((job-status-member? status '(exited killed unknown)) status)
        (when (and proc-on-job-stopped (pair? status) (eq? 'stopped (car status)))
          (format #t "; job ~s pid ~s stopped        ~s~%" (job-id j) (job-pid j) j)
            (with-foreground-pgid 'sh-wait j-pgid global-pgid
              (proc-on-job-stopped)))))
    (lambda () ; after body
      (%pgid-foreground 'sh-wait j-pgid global-pgid)
      (let ((j-pid (job-pid j)))
        (when (fx>? j-pid 0)
          ; (proc-on-job-stopped) above or some other function raised an exception,
          ; or (proc-on-job-stopped) called a continuation, but the job is still running
          ; -> interrupt the job's whole process group before returning
          (pid-kill (fx- j-pgid) 'sigcont)
          (pid-kill (fx- j-pgid) 'sigint))))))



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
    (raise-errorf 'job-redirect! "invalid redirect fd: ~s" child-fd))
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
    #f            ; no child running yet
    (list->span children-jobs)))

(define (assert-is-job who j)
  (assert* who (sh-job? j)))

;; Create a multijob to later start it. Each element in children-jobs must be a sh-job or subtype.

(define (sh-and . children-jobs)
  (apply make-multijob 'sh-and assert-is-job %multijob-start %multijob-step-and children-jobs))

(define (sh-or . children-jobs)
  (apply make-multijob 'sh-or  assert-is-job %multijob-start %multijob-step-or children-jobs))

;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-list
    (lambda (kind j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* kind (sh-job? j))))
    %multijob-start
    %multijob-step-list
    children-jobs-with-colon-ampersand))

;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-subshell
    (lambda (kind j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* kind (sh-job? j))))
    %multijob-spawn-subshell
    %multijob-run-subshell ; executed in child process
    children-jobs-with-colon-ampersand))


;; Run next child job in a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
(define (%multijob-step-and mj)
  (void))

;; Run next child job in a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (%multijob-step-or mj)
  (void))


;; Run next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (%multijob-step-list mj)
  ; TODO: check for ; among mj and ignore them
  ; TODO: check for & among mj and implement them
  (void))


;; Executed in child process:
;; run a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-subshell), implements runtime behavior of shell syntax [ ... ]
(define (%multijob-run-subshell mj)
  ; TODO: check for ; among mj and ignore them
  ; TODO: check for & among mj and implement them
  ; (debugf "; %multijob-subshell/run ~s status = ~s~%" mj (job-last-status mj))
  (let ((jobs   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status (void)))
    (span-iterate jobs
      (lambda (i job)
        (when (sh-job? job)
          (sh-start job pgid)             ; run child job in parent's process group
          (set! status (sh-wait job #f))) ; wait for child job to exit
        #t))                              ; keep iterating
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
