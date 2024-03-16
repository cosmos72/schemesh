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
    sh-job? sh-job-ref sh-job-status sh-jobs sh-cmd sh-cmd<> sh-cmd? sh-multijob sh-multijob?
    sh-globals sh-global-env sh-env-copy sh-env sh-env! sh-env-unset!
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->vector-of-bytevector0
    sh-cwd sh-consume-sigchld sh-start sh-bg sh-fg sh-wait sh-ok?
    sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-list sh-subshell
    sh-fd-redirect! sh-fds-redirect!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) break foreign-procedure format fx1+ fx1- record-writer reverse! void)
    (only (schemesh bootstrap) assert* while)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix fd)
    (schemesh posix pid)
    (schemesh posix signal))

;; Define the record type "job"
(define-record-type
  (job %make-job sh-job?)
  (fields
    (mutable pid)               ; fixnum: process id,       -1 if unknown
    (mutable pgid)              ; fixnum: process group id, -1 if unknown
    (mutable last-status)       ; cons: last known status
    (mutable to-redirect-fds)   ; vector: fds to redirect between fork() and
                                ;         (subshell-proc)
    (mutable to-redirect-files) ; vector of files to open before fork()
    (mutable to-close-fds)      ; list: fds to close after spawn
    proc                        ; #f or procedure to run in main process.
                                ; receives as argument job followed by options.
    subshell-proc               ; #f or procedure to run in fork()ed child.
                                ; receives as argument job followed by options.
                                ; its return value is passed to (exit-with-job-status)
    (mutable cwd)               ; charspan: working directory
    (mutable env)               ; hashtable: overridden env variables, or '()
    (mutable parent)))          ; parent job, contains default values of env variables
                                ; and default redirections
; Define the record type "cmd"
(define-record-type
  (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields argv)) ; vector of bytevectors, each #\nul terminated

;; Define the record type "multijob"
(define-record-type
  (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'and 'or 'list 'subshell 'global
    children            ; span:   children jobs.
    (mutable next-id))) ; fixnum: first available index in span of children jobs


;; Define the variable sh-globals, contains the global job.
;; Jobs started with (sh-start) will be children of sh-globals.
;
;; Variable may be set! to a different value in subshells.
(define sh-globals
  ;; waiting for sh-globals to exit is not useful:
  ;; pretend it already exited with unknown exit status
  (%make-multijob (get-pid) (get-pgid 0) '(unknown . 0) (vector 0 1 2) (vector) '()
    #f #f ; proc subshell-proc
    ; current directory
    (string->charspan* ((foreign-procedure "c_get_cwd" () scheme-object)))
    (make-hashtable string-hash string=?) #f
    'global (span #t) 1)) ; skip job-id 0

;; Define the global hashtable pid -> job
(define %table-pid->job (make-eq-hashtable))

;; Define function (pid->job) to convert pid to job, return #f if job not found
(define (pid->job pid)
  (assert* (fixnum? pid))
  (hashtable-ref %table-pid->job pid #f))

;; Define function (pid->job-set!) adds entries to the global hashtable pid -> job
(define (pid->job-set! pid job)
  (assert* (fixnum? pid))
  (assert* (sh-job? job))
  (hashtable-set! %table-pid->job pid job))

;; Define function (pid->job-delete!) removes entries from the global hashtable pid -> job
(define (pid->job-delete! pid)
  (assert* (fixnum? pid))
  (hashtable-delete! %table-pid->job pid))

;; Define the function (multijob-child-delete!), removes a job-id from a multijob
(define (multijob-child-delete! globals job-id)
  (let* ((arr (multijob-children globals))
         (job-id
           (if (fixnum? job-id)
             (when (and (fx>=? job-id 0) (fx<? job-id (span-length arr)))
               job-id)
             (span-find arr 0 (span-length arr) (lambda (elem) (eq? elem job-id))))))
    (when job-id
      (span-set! arr job-id #f)
      (multijob-next-id-set! globals
                              (fxmin job-id (multijob-next-id globals))))))


;; Define the function (multijob-child-put!), adds a job to a multijob
;; extending (multijob-span globals) as needed.
;; Return job-id assigned to job.
(define (multijob-child-put! mjob j)
  (let* ((arr     (multijob-children mjob))
         (len     (span-length arr))
         (next-id (multijob-next-id mjob))
         (job-id (span-find arr next-id (fx- len next-id) not)))
    (if job-id
      (span-set! arr job-id j) ; found a free job-id
      (begin                   ; no free job-id, enlarge span
        (span-insert-back! arr j)
        (set! job-id len)))
    (let* ((start   (multijob-next-id mjob))
           (len     (span-length arr))
           (next-id (span-find arr start (fx- len job-id) not)))
      (multijob-next-id-set! mjob
                             (or next-id len)))
    job-id))


;; Converts job-id to job.
;; Job-id can be either a job,
;; or #t which means sh-globals,
;; or a fixnum indicating one of the running jobs stored in (multijob-children sh-globals)
;;
;; Raises error if no job matches job-id.
(define (sh-job-ref job-id)
  (cond
    ((eq? #t job-id) sh-globals)
    ((fixnum? job-id)
      (let* ((all-jobs (multijob-children sh-globals))
             (job (when (and (fx>? job-id 0) ; job-ids start at 1
                             (fx<? job-id (span-length all-jobs)))
                    (span-ref all-jobs job-id))))
        (unless (sh-job? job)
          (error 'sh-job-ref "job not found:" job-id))
        job))
    ((sh-job? job-id) job-id)
    (#t (error 'sh-job-ref "not a job-id:" job-id))))

;; return charspan containing current directory,
;; or charspan containing current directory of specified job-id.
(define sh-cwd
  (case-lambda
    (()       (job-cwd sh-globals))
    ((job-id) (job-cwd (sh-job-ref job-id)))))


;; return currently running jobs
;; as an span of pairs (job-id . job) sorted by job-id
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
(define (job-parents-iterate job-id proc)
  (do ((parent (sh-job-ref job-id) (job-parent parent)))
      ((or (not (sh-job? parent)) (not (proc parent))))))


;; return list containing all job's parents,
;; starting from sh-globals, until job itself.
(define (job-parents-revlist job-id)
  (let ((jlist '()))
    (job-parents-iterate job-id
      (lambda (job)
        (set! jlist (cons job jlist))))
    jlist))


;; return list containing job followed by all its parents.
(define (job-parents-list job-id)
  (reverse! (job-parents-revlist job-id)))


;; Create a cmd to later spawn it. Each argument must be a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd program . args)
  (%make-cmd -1 -1 '(new . 0) (vector 0 1 2) (vector) '()
    %cmd-spawn #f ; proc subshell-proc
    (sh-cwd)      ; job working directory - initially current directory
    '()           ; overridden environment variables - initially none
    sh-globals    ; parent job - initially the global job
    (list->cmd-argv (cons program args))))


;; Create a cmd to later spawn it. Each argument must be a string, bytevector or symbol.
;; Each symbols indicates a redirection and must be followed by a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd<> program . args)
;; FIXME: implement redirections: [N]< [N]<> [N]<&M [N]> [N]>> [N]>&M
  (apply sh-cmd program args))

;; return global environment variables
(define (sh-global-env)
  (job-env sh-globals))


;; Return direct environment variables of job, creating them if needed.
;; Returned hashtable does not include default variables,
;; i.e. the ones inherited from parent jobs.
(define (job-direct-env job-id)
  (let* ((job (sh-job-ref job-id))
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
(define (sh-env-copy job-id which)
  (assert* (memq which '(exported all)))
  (let* ((jlist (job-parents-revlist job-id))
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

(define (sh-env* job-id name default)
  (job-parents-iterate job-id
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
    ((job-id name)         (sh-env* job-id name ""))
    ((job-id name default) (sh-env* job-id name default))))

(define (sh-env! job-id name val)
  (assert* (string? val))
  (let* ((vars (job-direct-env job-id))
         (elem (hashtable-ref vars name #f)))
    (if (pair? elem)
      (set-cdr! elem val)
      (hashtable-set! vars name (cons 'private val)))))


;; Note: (sh-env-unset!) inserts an entry that means "deleted",
;; in order to override any parent job's environment variable
;; with the same name.
(define (sh-env-unset! job-id name)
  (let ((vars (job-direct-env job-id)))
    (hashtable-set! vars name (cons 'delete ""))))

(define (sh-env-exported? job-id name)
  (let ((ret #f))
    (job-parents-iterate job-id
      (lambda (j)
        (let* ((vars (job-env j))
               (elem (if (hashtable? vars) (hashtable-ref vars name #f) #f)))
          (when (pair? elem)
            (set! ret (eq? 'export (car elem)))
            #f)))) ; name found, stop iterating
    ret))

(define (sh-env-export! job-id name exported?)
  (assert* (boolean? exported?))
  (let* ((j (sh-job-ref job-id))
         ; val may be in a parent environment
         (val (sh-env j name))
         (export (if exported? 'export 'private)))
    ; (job-direct-env j) creates job environment if not yet present
    (hashtable-set! (job-direct-env j) name (cons export val))))

;; combined sh-env! and sh-env-export!
(define (sh-env-set+export! job-id name val exported?)
  (assert* (string? val))
  (assert* (boolean? exported?))
  (let* ((vars (job-direct-env job-id))
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
(define (sh-env->vector-of-bytevector0 job-id which)
  (string-hashtable->vector-of-bytevector0 (sh-env-copy job-id which)))

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
      (assert* (pair? job-status))
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
; into global (pid->job) table nor into global job-id table.
;
;; Description:
;; Start a cmd i.e. fork() and exec() an external process, optionally inserting it into
; an existing process group.

;; The new process is started in background, i.e. the foreground process group is NOT set
; to the process group of the newly created process.

;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new process will be inserted
;     into the corresponding process group id - which must already exist.
(define %cmd-spawn
  (let ((c-spawn-pid (foreign-procedure "c_spawn_pid"
                        (scheme-object scheme-object scheme-object int) int)))
    (lambda (c . options)
      (assert* (sh-cmd? c))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-spawn-pid
                    (cmd-argv c)
                    (job-to-redirect-fds c)
                    (sh-env->vector-of-bytevector0 c 'exported)
                    process-group-id)))
        (when (< ret 0)
          (raise-errno-condition 'sh-start ret))
        (job-pid-set! c ret)
        (job-pgid-set! c (if (> process-group-id 0) process-group-id ret))))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, does not close (job-to-close-fds j)
;; - thus calling it manually leaks file descriptors - and does not register job
; into global (pid->job) table nor into global job-id table.
;
;; Description:
;; Start a generic job, optionally inserting it into an existing process group.
;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
; to the process group of the newly created process.
;
;; The subshell process will execute the Scheme function (job-subshell-proc j)
;; passing the job j as only argument,
;; then will call (exit-with-job-status) with the value returned by (job-subshell-proc j)
;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new subshell will be inserted
;     into the corresponding process group id - which must already exist.
(define %job-spawn
  (let ((c-fork-pid (foreign-procedure "c_fork_pid" (scheme-object int) int)))
    (lambda (j . options)
      (assert* (procedure? (job-subshell-proc j)))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-fork-pid
                    (job-to-redirect-fds j)
                    process-group-id)))
        (cond
          ((< ret 0)
            (raise-errno-condition 'sh-start ret)) ; fork() failed
          ((= ret 0)                               ; child
            (let ((status '(exited . 255)))
              (dynamic-wind
                void       ; run before body
                (lambda () ; body
                  (job-pid-set!  j (get-pid))
                  (job-pgid-set! j (get-pgid 0))
      ;                 this process now "is" the job j => update sh-globals' pid and pgid
                  (job-pid-set!  sh-globals (job-pid j))
                  (job-pgid-set! sh-globals (job-pgid j))
      ;                 cannot wait on our own process
                  (job-last-status-set! j '(unknown . 0))
                  (set! status ((job-subshell-proc j) j)))
                (lambda () ; run after body, even if it raised a condition
                  (exit-with-job-status status)))))
          ((> ret 0) ; parent
            (job-pid-set! j ret)
            (job-pgid-set! j (if (> process-group-id 0) process-group-id ret))))))))

;; Return #t if job was already started, otherwise return #f
(define (job-started? j)
  (and (fx>=? (job-pid j) 0) (fx>=? (job-pgid j) 0)))

;; Start a cmd or a job.
;; If job parent is sh-globals, return job-id assigned to job.
;; Otherwise return (void).
;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 then the new process will be inserted
;     into the corresponding process group id - which must already exist.
(define (sh-start j . options)
  (when (fx>=? (job-pid j) 0)
    (error 'sh-start "job already started" (job-pid j)))
  (unless (procedure? (job-proc j))
    (error 'sh-start "cannot start job, it has bad or missing job-proc" j))
  (apply (job-proc j) j options)
  (fd-close-list (job-to-close-fds j))
  (job-last-status-set! j '(running . 0)) ; job can now be waited-for
  (pid->job-set! (job-pid j) j)           ; add job to pid->job table
  (if (eq? sh-globals (job-parent j))
    (multijob-child-put! sh-globals j)
    (void)))


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
;; return '(running . 0) indicating process is still running.
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
      '(running . 0))
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
;;   (cons 'running ...)   ; may happen only if may-block is 'nonblocking
;;   (void)                ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Argument may-block must be one of: 'blocking 'nonblocking
;
;; Warning: this is an internal function, and does not set the job
;; as foreground process group. consider calling (sh-fg j) or (sh-wait j) instead.
(define (job-wait j may-block)
  (assert* (memq may-block '(blocking nonblocking)))
  (cond
    ((job-status-member? (job-last-status j) '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (error 'job-wait "job not started yet" j))
    (#t
      ; TODO: wait for ALL processes in job's process group?
      (let* ((ret    (pid-wait (job-pid j) may-block))
             (status (pid-wait->job-status ret)))
        ; if may-block is 'non-blocking, ret may be '() and status will be '(running . 0)
        ; indicating job status did not change i.e. it's (expected to be) still running
        (job-last-status-set! j status)
        (when (job-status-member? status '(exited killed unknown))
          ; job exited. it can now be spawned again
          (when (eq? sh-globals (job-parent j))
            (multijob-child-delete! sh-globals j))
          (pid->job-delete! (job-pid j))
          (job-pid-set! j -1)
          (job-pgid-set! j -1))
        status))))


;; Return up-to-date status of a job or job-id, which can be one of:
;;   (cons 'new     0)
;;   (cons 'running 0)
;;   (void)                      ; if process exited with exit-status = 0
;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Note: this function also non-blocking checks if job status changed.
(define (sh-job-status job-id)
  (let ((j (sh-job-ref job-id)))
    (when (job-status-member? (job-last-status j) '(running))
      ; nonblocking wait for job's pid to exit or stop.
      ; TODO: wait for ALL pids in process group?
      (job-wait j 'nonblocking))
    (job-last-status j)))


;; Continue a job or job-id in background by sending SIGCONT to it.
;; Return job status, which can be one of:
;
;;   (cons 'running 0)
;;   (void)                      ; if process exited with exit-status = 0
;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
(define (sh-bg job-id)
  (let ((j (sh-job-ref job-id)))
    (cond
;    if job already exited, return its exit status.
;;     if job is stopped, consider as running: we'll send SIGCONT to it below
      ((job-status-member? (job-last-status j) '(exited killed unknown))
        (job-last-status j))
      ((not (job-started? j))
        (error 'sh-bg "job not started yet" j))
      (#t
;      send SIGCONT to job's process group. may raise error
        (pid-kill (fx- (job-pgid j)) 'sigcont)
;;      nonblocking wait for job's pid to exit or stop.
;;       TODO: wait for ALL pids in process group?
        (job-wait j 'nonblocking)))))

(define %pgid-foreground
  (let ((c-pgid-foreground (foreign-procedure "c_pgid_foreground" (int int) int)))
    (lambda (caller expected-pgid new-pgid)
      (let ((err (c-pgid-foreground expected-pgid new-pgid)))
        (when (< err 0)
          (raise-errno-condition caller err))
        err))))

(define-syntax with-foreground-pgid
  (syntax-rules ()
    ((_  caller expected-pgid new-pgid body ...)
      (let ((_caller caller)
            (_expected-pgid expected-pgid)
            (_new-pgid new-pgid))
        (%pgid-foreground _caller _expected-pgid _new-pgid)
        (dynamic-wind
          void       ; run before body
          (lambda () body ...)
          (lambda ()
            ; run after body, even if it raised a condition:
            ; try to restore sh-globals as the foreground process group
            (%pgid-foreground _caller _new-pgid _expected-pgid)))))))


;; Continue a job or job-id by sending SIGCONT to it, wait for it to exit or stop,
;; and finally return its status, which can be one of:
;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'stopped signal-name)
;;   (cons 'unknown ...)
;
;; Note: if the current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-fg job-id)
  (let* ((j      (sh-job-ref job-id))
         (j-pgid (job-pgid j)))
    (cond
      ; if job already exited, return its exit status.
      ; if job is stopped, consider as running: we'll send SIGCONT to it below
      ((job-status-member? (job-last-status j) '(exited killed unknown))
        (job-last-status j))
      ((not (job-started? j))
        (error 'sh-fg "job not started yet" j))
      (#t
        (with-foreground-pgid 'sh-fg (job-pgid sh-globals) j-pgid
          ; send SIGCONT to job's process group. may raise error
          (pid-kill (fx- j-pgid) 'sigcont)
          ; blocking wait for job's pid to exit or stop.
          ; TODO: wait for ALL pids in process group?
          (job-wait j 'blocking))))))


;; Wait for a job or job-id to exit.
;;
;; Does NOT send SIGCONT in case the job is already stopped, use (sh-fg) for that.
;; Does NOT return early if the job gets stopped, use (sh-fg) for that.
;;
;; Return job status, which can be one of:
;;
;;   (void)                      ; if process exited with exit-status = 0
;;   (cons 'exited  exit-status)
;;   (cons 'killed  signal-name)
;;   (cons 'unknown ...)
;;
;; Note: if current shell is in the fg process group,
;;   upon invocation, sets the job as fg process group.
;;   And before returning, restores current shell as fg process group.
(define (sh-wait job-id)
  (let* ((j (sh-job-ref job-id))
         (j-pgid      (job-pgid j))
         (global-pgid (job-pgid sh-globals)))
    (cond
      ; if job already exited, return its exit status.
      ; if job is stopped, consider as running
      ((job-status-member? (job-last-status j) '(exited killed unknown))
        (job-last-status j))
      ((not (job-started? j))
        (error 'sh-wait "job not started yet" j))
      (#t
        (with-foreground-pgid 'sh-wait global-pgid j-pgid
          ; blocking wait for job's pid to exit.
          ; TODO: wait for ALL pids in process group?
          (do ((status #f (job-wait j 'blocking)))
              ((job-status-member? status '(exited killed unknown)) status)
            (when (and (pair? status) (eq? 'stopped (car status)))
              (if (fixnum? job-id)
                (format #t "; ~s pid ~s stopped        ~s~%" job-id (job-pid j) j)
                (format #t "; pid ~s stopped        ~s~%" (job-pid j) j))
              (with-foreground-pgid 'sh-wait j-pgid global-pgid
                (break)
                ; send SIGCONT to job's process group. may raise error
                (pid-kill (fx- j-pgid) 'sigcont)))))))))



;; Start a job and wait for it to exit or stop.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-fg)
(define (sh-run/i j . options)
  (apply sh-start j options)
  (sh-fg j))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return job status, possible values are the same as (sh-wait)
(define (sh-run j . options)
  (apply sh-start j options)
  (sh-wait j))


;; Start a job and wait for it to exit.
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;;
;; Options are the same as (sh-start)
;;
;; Return #t if job exited successfully, otherwise return #f.
(define (sh-run/ok? j . options)
  (sh-ok? (apply sh-run j options)))

;; Create or remove a file description redirection for cmd or job
(define (sh-fd-redirect! job-id child-fd existing-fd-or-minus-1)
  (when (or (not (fixnum? child-fd)) (< child-fd 0))
    (error 'job-redirect! "invalid redirect fd" child-fd))
  (let* ((j (sh-job-ref job-id))
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


;; convert job-status to 8-bit exit status suitable for C function exit().
;; if job-status is (void) return 0
; if job-status is '(exited . n) return n
; if job-status is '(killed . signal_name) return 128 + signal_number
; otherwise return 255

(define (job-approx-exit-status job-status)
  (cond
    ((eq? (void) job-status) 0)
    ((pair? job-status)
      (cond
        ((eq? 'exited (car job-status))
          (cdr job-status))
        ((eq? 'killed (car job-status))
          (fx+ 128 (signal-name->number (cdr job-status))))
        (#t 255))) ; (car job-status) is 'new 'running 'stopped etc
    (#t 255)))     ; job-status is not (void) nor a cons


;; Create a multijob to later start it.
;; Internal function, accepts an optional function to validate each element in children-jobs

(define (make-multijob kind validate-job-proc subshell-proc . children-jobs)
  (assert* (symbol? kind))
  (assert (or (not subshell-proc) (procedure? subshell-proc)))
  (when validate-job-proc
    (list-iterate children-jobs validate-job-proc))
  (%make-multijob -1 -1 '(new . 0) (vector 0 1 2) (vector) '()
    %job-spawn subshell-proc
    (sh-cwd)   ; job working directory - initially current directory
    '()        ; overridden environment variables - initially none
    sh-globals ; parent job - initially the global job
    kind
    (list->span children-jobs)
    0))

(define (assert-is-job j)
  (assert* (sh-job? j)))

;; Create a multijob to later start it. Each argument must be a sh-job or subtype.
(define (sh-multijob kind subshell-proc . children-jobs)
  (apply make-multijob kind assert-is-job subshell-proc children-jobs))


;; Run a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz

(define (%multijob-run-and mj)
  (let ((jobs   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status (void)))
    (span-iterate jobs
      (lambda (i job)
        (sh-start job pgid)         ; run child job in parent's process group
        (set! status (sh-wait job)) ; wait for child job to exit
        (eq? (void) status)))       ; keep iterating only if job succeeded
    status))


;; Run a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (%multijob-run-or mj)
  (let ((jobs   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status '(exited . 1)))
    (span-iterate jobs
      (lambda (i job)
        (sh-start job pgid)         ; run child job in parent's process group
        (set! status (sh-wait job)) ; wait for child job to exit
        (not (eq? (void) status)))) ; keep iterating only if job failed
    status))


;; Run a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list) and (sh-subshhell), implements runtime behavior of shell syntax foo; bar & baz
(define (%multijob-run-list mj)
  ; TODO: check for ; among mj and ignore them
  ; TODO: check for & among mj and implement them
  (let ((jobs   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status (void)))
    (span-iterate jobs
      (lambda (i job)
        (sh-start job pgid)         ; run child job in parent's process group
        (set! status (sh-wait job)) ; wait for child job to exit
        #t))                        ; keep iterating
    status))

(define (sh-and . children-jobs)
  (apply make-multijob 'and assert-is-job %multijob-run-and children-jobs))

(define (sh-or . children-jobs)
  (apply make-multijob 'or  assert-is-job %multijob-run-or  children-jobs))


;; Each argument must be a sh-job, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  ; TODO: do not fork a subshell to run children jobs
  (apply make-multijob 'list
    (lambda (j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* (sh-job? j))))
    %multijob-run-list children-jobs-with-colon-ampersand))

;; Each argument must be a sh-job, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (apply make-multijob 'subshell
    (lambda (j) ; validate-job-proc
      (unless (memq j '(& \x3b;
                       ))
        (assert* (sh-job? j))))
    %multijob-run-list children-jobs-with-colon-ampersand))

(define (sh-run/bytes job)
  ; TODO: implement (sh-run/bytes)
  #vu8())

(define (sh-run/string job)
  ; TODO: implement (sh-run/string)
  "")

;; customize how "job" objects are printed
(record-writer (record-type-descriptor job)
  (lambda (obj port writer)
    (display "(sh-job " port)
    (writer (job-subshell-proc obj) port)
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
    (display "(sh-" port)
    (display (multijob-kind obj) port)
    (span-iterate (multijob-children obj)
       (lambda (i child)
         (display #\space port)
         (display child port)))
    (display ")" port)))

(begin
  (c-environ->sh-global-env))

) ; close library
