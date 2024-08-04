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
    sh-job? sh-job sh-job-id sh-job-status sh-jobs sh-cmd sh-cmd? sh-multijob?
    sh-concat sh-env-copy sh-env sh-env! sh-env-unset! sh-globals sh-global-env
    sh-env-exported? sh-env-export! sh-env-set+export! sh-env->argv
    sh-builtin-command sh-builtin-cd sh-builtin-pwd sh-cwd sh-cwd-set! sh-cd sh-pwd
    sh-consume-sigchld sh-multijob-child-length sh-multijob-child-ref
    sh-start sh-bg sh-fg sh-wait sh-ok? sh-run sh-run/i sh-run/ok? sh-run/bytes sh-run/string
    sh-and sh-or sh-not sh-list sh-subshell sh-redirect!
    sh-job-display sh-job-display* sh-job-display/string
    sh-job-write sh-job-write* sh-job-write/string)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) break display-string foreign-procedure format fx1+ fx1-
                       inspect logand logbit? make-format-condition parameterize
                       procedure-arity-mask record-writer reverse! void)
    (only (schemesh bootstrap) assert* debugf raise-errorf until while)
    (schemesh containers)
    (schemesh conversions)
    (schemesh posix fd)
    (schemesh posix pid)
    (schemesh posix signal)
    (schemesh shell fds)
    (schemesh shell paths)
    (schemesh shell aliases)
    (schemesh shell builtins))


;; Define the record type "job"
(define-record-type
  (job %make-job sh-job?)
  (fields
    (mutable id job-id %job-id-set!) ; fixnum: job id in (sh-globals), #f if not set
    (mutable pid)               ; fixnum: process id,       -1 if unknown
    (mutable pgid)              ; fixnum: process group id, -1 if unknown
     ; cons: last known status, or (void) if job exited successfully
    (mutable last-status job-last-status %job-last-status-set!)
    ; span of quadruplets (fd mode to-fd-or-bytevector0 path-or-closure)
    ; to open and redirect between fork() and exec()
    (mutable redirects)
    (mutable fds-to-remap) ; for builtins or multijobs, #f or hashmap job-logical-fd -> actual-fd-to-use
    (mutable fds-to-close) ; for builtins or multijobs, '() or list of fds to close at job exit
    start-proc      ; #f or procedure to run in main process.
                    ; receives as argument job followed by options.
    step-proc       ; #f or procedure.
                    ; For multijobs, will be called when a child job changes status.
                    ; For cmds, will be called in fork()ed child process and
                    ; receives as argument job followed by options.
                    ; For cmds, its return value is passed to (exit-with-job-status)
    (mutable cwd)               ; charspan: working directory
    (mutable env)               ; hashtable: overridden env variables, or '()
    (mutable parent))           ; parent job, contains default values of env variables
                                ; and default redirections
  (nongenerative #{job ghm1j1xb9o5tkkhhucwauly2c-1175}))


;; return the job-id of a job, or #f if not set
(define (sh-job-id j)
  (job-id j))

;; Define the record type "cmd"
(define-record-type
  (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields arg-list) ; list of strings: program-name and args
  (nongenerative #{cmd ghm1j1xb9o5tkkhhucwauly2c-1176}))

;; Define the record type "multijob"
(define-record-type
  (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-not 'sh-list 'sh-subshell 'sh-global
    (mutable current-child-index) ; -1 or index of currently running child job
    children)           ; span: children jobs.
  (nongenerative #{multijob ghm1j1xb9o5tkkhhucwauly2c-1177}))


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
    ; current directory
    (string->charspan* ((foreign-procedure "c_get_cwd" () scheme-object)))
    (make-hashtable string-hash string=?) ; env variables
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


;; set the status of a job and return it.
;; if (job-status->kind status) is one of 'exited 'killed 'unknown, also close the job fds
(define (job-status-set! j status)
  (let ((status (job-status-normalize status)))
    (if (job-status-member? status '(running))
      (job-status-set/running! j)
      (begin
        (%job-last-status-set! j status)
        (when (job-status-member? status '(exited killed unknown))
          (job-unmap-fds! j)
          (let ((fds (job-fds-to-close j)))
            (unless (null? fds)
              (fd-close-list fds)
              (job-fds-to-close-set! j '()))))
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


(define (job-status-set/running! j)
  (let* ((id     (job-id j))
         (status (job-last-status j))
         (kind   (job-status->kind status))
         (old-id (if (pair? status) (cdr status) #f)))
    (if (and (eq? 'running status) (eqv? id old-id))
      status
      (let ((new-status (cons 'running id)))
        (%job-last-status-set! j new-status)
        new-status))))


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
        (job-status-set! j (cons 'running id)))))
  (job-last-status j))


;; if job is running or stopped, then create a new job-id for it.
;; if job has terminated, clear its job id and close its fds.
;; Also replace any job status '(running . #f) -> '(running . job-id)
;; Return updated job status.
(define (job-id-update! j)
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
;; path must be a a string or charspan.
(define sh-cd
  (case-lambda
    (()     (sh-cd* (sh-env sh-globals "HOME")))
    ((path) (sh-cd* path))
    ((path . extra-args) (raise-errorf 'cd "too many arguments"))))

;; internal function called by (sh-cd)
(define sh-cd*
  (let ((c_chdir (foreign-procedure "c_chdir" (scheme-object) int)))
    (lambda (path)
      (let* ((suffix (text->sh-path path))
             (dir (if (sh-path-absolute? suffix)
                      (sh-path->subpath suffix)
                      (sh-path-append (sh-cwd) suffix)))
             (err (c_chdir (string->utf8b/0 (charspan->string dir)))))
        (if (= err 0)
          (job-cwd-set! sh-globals dir)
          (raise-errorf 'cd "~a: ~a" path (c-errno->string err)))))))


(define sh-pwd
  (case-lambda
    (()     (sh-pwd* (current-output-port)))
    ((port) (sh-pwd* port))))

(define (sh-pwd* port)
  (put-string port (charspan->string (sh-cwd)))
  (newline port))


;; the "cd" builtin
(define (sh-builtin-cd job prog-and-args options)
  (assert-string-list? 'sh-builtin-cd prog-and-args)
  (apply sh-cd (cdr prog-and-args)))


;; the "pwd" builtin
(define (sh-builtin-pwd job prog-and-args options)
  (assert-string-list? 'sh-builtin-pwd prog-and-args)
  ;; TODO: support redirections
  (sh-pwd))

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


;; Create a cmd to later spawn it. Each argument must be a string.
(define (sh-cmd . program-and-args)
  (assert-string-list? 'sh-cmd program-and-args)
  (%make-cmd #f -1 -1 '(new . 0)
    (span) #f '() ; redirections
    job-start/cmd #f  ; start-proc step-proc
    (sh-cwd)      ; job working directory - initially current directory
    '()           ; overridden environment variables - initially none
    sh-globals    ; parent job - initially the global job
    program-and-args))


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
(define (sh-env->argv job-or-id which)
  (string-hashtable->argv (sh-env-copy job-or-id which)))


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
(define (job-remap-fds! j)
  (let* ((n (span-length (job-redirects j))))
    (unless (fxzero? n)
      (let ((remaps (make-eqv-hashtable n)))
        (job-fds-to-remap-set! j remaps)
        (do ((i 0 (fx+ i 4)))
            ((fx>? i (fx- n 4)))
          (job-remap-fd! j i))))))


;; called by (job-remap-fds!)
(define (job-remap-fd! j index)
  ;; refirects is span of quadruplets (fd mode to-fd-or-bytevector0 path-or-closure)
  (let* ((redirects           (job-redirects j))
         (fd                  (span-ref redirects index))
         (direction-ch        (span-ref redirects (fx1+ index)))
         (to                  (span-ref redirects (fx+ 3 index)))
         (to-fd-or-bytevector
          (if (procedure? to)
            (if (logbit? 1 (procedure-arity-mask to))
              (to j)
              (to))
            to))
         (remap-fd (sh-fd-allocate)))
    ; (debugf "fd-redirect fd=~s dir=~s to=~s~%" remap-fd direction-ch to-fd-or-bytevector)
    (let ((ret (fd-redirect (sh-fd->int remap-fd) direction-ch to-fd-or-bytevector #t))) ; #t close-on-exec?
      (when (< ret 0)
        (sh-fd-release remap-fd)
        (raise-c-errno 'sh-start 'fd-redirect ret)))
    (hashtable-set! (job-fds-to-remap j) fd remap-fd)))


;; redirect a file descriptor. returns < 0 on error
(define fd-redirect
  (foreign-procedure "c_fd_redirect" (scheme-object scheme-object scheme-object scheme-object) int))


;; return the remapped file descriptor for specified fd,
;; or fd itself if no remapping was found
(define (job-find-fd-remap j fd)
  (while j
    (let ((remap-fds (job-fds-to-remap j)))
      (when remap-fds
        (let ((remap-fd (hashtable-ref remap-fds fd #f)))
          (when remap-fd
            (set! fd (sh-fd->int remap-fd))))))
    (set! j (job-parent j)))
  fd)


;; release job's remapped fds and unset (job-fds-to-remap j)
(define (job-unmap-fds! j)
  (let ((remap-fds (job-fds-to-remap j)))
    (when remap-fds
      (hashtable-iterate remap-fds
        (lambda (cell)
          (let ((fd (cdr cell)))
            (when (sh-fd-release fd)
              (fd-close (sh-fd->int fd))))))
      (job-fds-to-remap-set! j #f))))


;; NOTE: this is an internal implementation function, use (sh-start) instead.
;; This function does not update job's status, and does not register job
;; into global (pid->job) table nor into global job-id table.
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
(define (job-start/cmd c options)
  (assert* 'sh-cmd (sh-cmd? c))
  (assert* 'sh-cmd (eq? 'running (job-last-status->kind c)))

  ; expand aliases. sanity: (sh-alias-expand) ignores aliases for "builtin"
  (let* ((prog-and-args (sh-alias-expand (cmd-arg-list c)))
         (builtin  (sh-find-builtin prog-and-args)))
    ; check for builtins
    (if builtin
      ; expanded arg[0] is a builtin, call it.
      (job-run/builtin builtin c prog-and-args options)
       ; expanded arg[0] is a not builtin or alias, spawn a subprocess
      (job-start/cmd/spawn c (list->argv prog-and-args) options))))


;; internal function called by (job-start/cmd) to execute a builtin
(define (job-run/builtin builtin c prog-and-args options)
  (job-remap-fds! c)
  (job-status-set! c
    (parameterize ((sh-fd-stdin  (job-find-fd-remap c 0))
                   (sh-fd-stdout (job-find-fd-remap c 1))
                   (sh-fd-stderr (job-find-fd-remap c 2)))
      (builtin c prog-and-args options)))
  (job-id-update! c)) ; returns job status


;; internal function called by (job-start/cmd) to spawn a subprocess
(define job-start/cmd/spawn
  (let ((c-spawn-pid (foreign-procedure "c_spawn_pid"
                        (scheme-object scheme-object scheme-object int) int)))
    (lambda (c argv options)
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-spawn-pid
                    argv
                    (span->vector (job-redirects c))
                    (sh-env->argv c 'exported)
                    process-group-id)))
        (when (< ret 0)
          (raise-c-errno 'sh-start 'fork ret))
        (job-pid-set! c ret)
        (job-pgid-set! c (if (> process-group-id 0) process-group-id ret))))))



;; the "command" builtin
(define (sh-builtin-command job prog-and-args options)
  (assert-string-list? 'sh-builtin-command prog-and-args)
  (job-start/cmd/spawn job (list->argv (cdr prog-and-args)) options)
  (job-last-status job))


;; Internal function stored in (job-start-proc j) by (sh-list),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors. Options are ignored.
(define (job-start/list j options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (assert* 'sh-list (eq? 'running (job-last-status->kind j)))
  (assert* 'sh-list (fx=? -1 (multijob-current-child-index j)))
  (job-remap-fds! j)
  ; Do not yet assign a job-id.
  (job-step/list j (void)))


;; Internal function stored in (job-start-proc j) by (sh-and),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors. Options are ignored.
(define (job-start/and j options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (assert* 'sh-and (eq? 'running (job-last-status->kind j)))
  (assert* 'sh-and (fx=? -1 (multijob-current-child-index j)))
  (job-remap-fds! j)
  (if (span-empty? (multijob-children j))
    ; (sh-and) with zero children -> job completes successfully
    (job-status-set! j (void))
    ; Do not yet assign a job-id.
    (job-step/and j (void))))


;; Internal function stored in (job-start-proc j) by (sh-or),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors. Options are ignored.
(define (job-start/or j options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (assert* 'sh-or (eq? 'running (job-last-status->kind j)))
  (assert* 'sh-or (fx=? -1 (multijob-current-child-index j)))
  (job-remap-fds! j)
  ; (debugf "job-start/or ~s empty children? = ~s~%" j (span-empty? (multijob-children j)))
  (if (span-empty? (multijob-children j))
    ; (sh-or) with zero children -> job fails with '(exited . 255)
    (job-status-set! j '(exited . 255))
    ; Do not yet assign a job-id.
    (job-step/or j (void))))


;; Internal function stored in (job-start-proc j) by (sh-not),
;; and called by (sh-start) to actually start the multijob.
;;
;; Does not redirect file descriptors. Options are ignored.
(define (job-start/not j options)
  ;; this runs in the main process, not in a subprocess.
  ;; TODO: how can we redirect file descriptor?
  (assert* 'sh-not (eq? 'running (job-last-status->kind j)))
  (assert* 'sh-not (fx=? -1 (multijob-current-child-index j)))
  (job-remap-fds! j)
  ; Do not yet assign a job-id.
  (job-step/not j (void)))


;; internal function stored in (job-start-proc j) by (sh-subshell) multijobs
;;
;; Forks a new subshell process in background, i.e. the foreground process group is NOT set
;; to the process group of the newly created process.
;;
;; The subshell process will execute the Scheme function (job-step-proc j)
;; passing the job j as only argument,
;;
;; Options is a list of zero or more of the following:
;;   process-group-id: a fixnum, if present and > 0 the new subshell will be inserted
;;     into the corresponding process group id - which must already exist.
(define job-start/subshell
  (let ((c-fork-pid (foreign-procedure "c_fork_pid" (scheme-object int) int)))
    (lambda (j options)
      (assert* 'sh-start (procedure? (job-step-proc j)))
      (let* ((process-group-id (job-start-options->process-group-id options))
             (ret (c-fork-pid
                    (span->vector (job-redirects j))
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
                    (job-status-set! j '(unknown . 0))))
                (lambda () ; body
                  ; sh-subshell stores job-run/subshell in (job-step-proc j)
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
  (start/any j options))

;; Internal functions called by (sh-start)
(define (start/any j options)
  ; (debugf "start/any ~s ~s~%" j options)
  (when (job-started? j)
    (if (job-id j)
      (raise-errorf 'sh-start "job already started with job id ~s" (job-id j))
      (raise-errorf 'sh-start "job already started")))
  (let ((proc (job-start-proc j)))
    (unless (procedure? proc)
      (raise-errorf 'sh-start "cannot start job, it has bad or missing job-start-proc: ~s" j))
    (job-status-set/running! j)
    (proc j options)) ; ignore value returned by job-start-proc
  (when (fx>? (job-pid j) 0)
    (pid->job-set! (job-pid j) j))        ; add job to pid->job table
  (job-last-status j))

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
  (assert* 'advance-job-or-id (memq mode '(sh-fg sh-bg sh-wait sh-sigcont+wait sh-subshell sh-job-status)))
  (let ((j (sh-job job-or-id)))
    (when (job-has-status? j '(new running stopped))
      (job-advance/any mode j))
    (job-id-update! j)))


;; Internal function called by (advance-job-or-id) (job-advance/multijob)
(define (job-advance/any mode j)
  ; (debugf "job-advance/any > ~s ~s status=~s~%" mode j (job-last-status j))
  (cond
    ((job-has-status? j '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (raise-errorf mode  "job not started yet: ~s" j))
    ((fx>? (job-pid j) 0)
      (job-advance/pid mode j))
    ((sh-multijob? j)
      (job-advance/multijob mode j))
    (#t
      ; unexpected job type or status, just assume it exited successfully
      (job-status-set! j (void))
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


;; Internal function called by (job-advance/any)
(define (job-advance/pid mode j)
  ; (debugf "job-advance/pid > ~s ~s status=~s~%" mode j (job-last-status j))
  (cond
    ((job-has-status? j '(exited killed unknown))
      (job-last-status j)) ; job exited, and exit status already available
    ((not (job-started? j))
      (raise-errorf mode "job not started yet: ~s" j))
    (#t
      (let ((pid  (job-pid j))
            (pgid (job-pgid j)))
        (if (memq mode '(sh-fg sh-wait sh-sigcont+wait sh-subshell))
          (with-foreground-pgid mode (job-pgid sh-globals) pgid
            (job-advance/pid/maybe-sigcont mode j pid pgid)
            (job-advance/pid/wait mode j pid pgid))
          (begin
            (job-advance/pid/maybe-sigcont mode j pid pgid)
            (job-advance/pid/wait mode j pid pgid)))))))


;; Internal function called by (job-advance/pid)
(define (job-advance/pid/maybe-sigcont mode j pid pgid)
  (assert* mode (fx>? pid 0))
  (assert* mode (fx>? pgid 0))
  (when (memq mode '(sh-fg sh-bg sh-sigcont+wait))
    ; send SIGCONT to job's process group, if present.
    ; otherwise send SIGCONT to job's process id. Both may raise error
    ; (debugf "job-advance/pid/sigcont > ~s ~s~%" mode j)
    (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont)))



;; Internal function called by (job-advance/pid)
(define (job-advance/pid/wait mode j pid pgid)
  ; no need to wait for ALL processes in job's process group:
  ; the only case where we spawn multiple processes in the same process group
  ; is a pipe i.e {a | b | c ...} and in such case we separately wait on the process id
  ; of each spawned process
  (let* ((may-block   (if (memq mode '(sh-bg sh-job-status)) 'nonblocking 'blocking))
         (wait-status (pid-wait->job-status (pid-wait (job-pid j) may-block)))
         (kind        (job-status->kind wait-status)))
    ; (debugf "job-advance/pid/wait > ~s ~s wait-status=~s~%" mode j wait-status)
    ; if may-block is 'non-blocking, wait-status may be '(running . #f)
    ; indicating job status did not change i.e. it's (expected to be) still running
    (case kind
      ((running)
        ; if wait-status is '(running . #f), try to return '(running . job-id)
        (job-last-status j))
      ((exited killed unknown)
        ; job exited, clean it up in case user wants to later respawn it
        (pid->job-delete! (job-pid j))
        (job-pid-set!  j -1)
        (job-pgid-set! j -1)
        (job-status-set! j wait-status)
        ;; returns job status
        (job-id-unset! j))
      ((stopped)
        ; process is stopped.
        ; if mode is sh-wait or sh-sigcont+wait, call (break)
        ; then, if mode is sh-wait sh-sigcont+wait or sh-subshell, wait for it again (which blocks until it changes status again)
        ; otherwise propagate process status and return.
        (if (memq mode '(sh-wait sh-sigcont+wait sh-subshell))
          (begin
            (when (memq mode '(sh-wait sh-sigcont+wait))
              (job-advance/pid/break mode j pid pgid))
            (job-advance/pid/wait mode j pid pgid))
          (begin
            (job-status-set! j wait-status)
            wait-status)))
      (else
        (raise-errorf mode "job not started yet: ~s" j)))))


;; Internal function called by (job-advance/pid/wait)
;; when job is stopped in mode 'sh-wait or 'sh-sigcont+wait:
;; call (break) then send 'sigcont to job
;; if (break) raises an exception or resets scheme, then send 'sigint to job
(define (job-advance/pid/break mode j pid pgid)
  (let ((break-returned-normally? #f)
        (global-pgid (job-pgid sh-globals)))
    (dynamic-wind
      (lambda () ; before body
        (%pgid-foreground mode pgid global-pgid))
      (lambda () ; body
        (job-id-set! j)
        (format #t "; job ~s pid ~s stopped        " (job-id j) pid)
        (sh-job-display j)
        (put-char (current-output-port) #\newline)
        (break)
        (set! break-returned-normally? #t))
      (lambda ()
        (%pgid-foreground mode global-pgid pgid)
        ; send SIGCONT to job's process group, if present.
        ; otherwise send SIGCONT to job's process id. Both may raise error
        (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigcont)
        (unless break-returned-normally?
          (pid-kill (if (fx>? pgid 0) (fx- pgid) pid) 'sigint))))))



;; Internal function called by (job-advance/any)
(define (job-advance/multijob mode mj)
  (job-status-set/running! mj)
  (let* ((child (sh-multijob-child-ref mj (multijob-current-child-index mj)))
         ;; call (job-advance/any) on child
         (child-status (if (sh-job? child) (job-advance/any mode child) (void)))
         (step-proc (job-step-proc mj)))
    ; (debugf "job-advance/multijob > ~s ~s child=~s child-status=~s~%" mode mj child child-status)
    (cond
      ((or (not step-proc) (job-status-stops-or-ends-multijob? child-status))
        ; propagate child exit status and return
        (job-status-set! mj child-status)
        child-status)
      ((job-status-member? child-status '(exited killed unknown))
        ; child exited: advance multijob by calling (job-step-proc)
        ; then call (job-advance/multijob) again multijob job is still running.
        ; (debugf "step-proc > ~s status=~s~%" mj (job-last-status mj))
        (step-proc mj child-status)
        ; (debugf "step-proc < ~s status=~s~%" mj (job-last-status mj))
        (if (job-has-status? mj '(running))
          (job-advance/multijob mode mj)
          (job-last-status mj)))
      ((job-status-member? child-status '(running))
        ; child is still running. propagate child status and return
        (job-status-set! mj (cons 'running (job-id mj)))) ; (job-id mj) may still be #f
      ((job-status-member? child-status '(stopped))
        ; child is stopped.
        ; if mode is sh-wait or sh-subshell, wait for it again.
        ; otherwise propagate child status and return
        (if (memq mode '(sh-wait sh-sigcont+wait sh-subshell))
          (job-advance/multijob mode mj)
          (job-status-set! mj child-status)))
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
  (advance-job-or-id (if send-sigcont? 'sh-sigcont+wait 'sh-wait) job-or-id))


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


;; Add multiple redirections for cmd or job. Return cmd or job.
;; Each redirection must be a two-argument DIRECTION TO-FD-OR-FILE-PATH
;; or a three-argument FROM-FD DIRECTION TO-FD-OR-FILE-PATH
(define (sh-redirect! job-or-id . redirections)
  (let ((job (sh-job job-or-id))
        (args redirections))
    (until (null? args)
      (when (null? (cdr args))
        (raise-errorf 'sh-redirect! "invalid redirect, need two or three arguments, found one: ~s" args))
      (let ((arg (car args)))
        (cond
          ((fixnum? arg)
            (when (null? (cddr args))
              (raise-errorf 'sh-redirect! "invalid three-argument redirect, found only two arguments: ~s" args))
            (job-redirect! job arg (cadr args) (caddr args))
            (set! args (cdddr args)))
          ((redirection-sym? arg)
            (job-redirect! job (if (eq? '<& arg) 0 1) arg (cadr args))
            (set! args (cddr args)))
          (#t
            (raise-errorf 'sh-redirect! "invalid redirect, first argument must a fixnum or a redirection symbol: ~s" args)))))
    job))


;; Return #t if token is a shell redirection operator: < <> <& > >> >&
(define (redirection-sym? token)
  (and (symbol? token)
       (memq token '(< <> > >> <& >&))))


;; Add a single redirection to a job
(define (job-redirect! job fd direction to)
  (if (or (eq? '<& direction) (eq? '>& direction))
    (job-redirect/fd!   job fd direction to)
    (job-redirect/file! job fd direction to)))


;; Add a single fd redirection to a job
(define (job-redirect/fd! job fd direction to)
  (unless (fx>=? to -1)
    (raise-errorf 'sh-redirect! "invalid redirect to fd, must be -1 or an unsigned fixnum: ~a" to))
  (span-insert-back! (job-redirects job)
    fd
    (redirect/fd-symbol->char 'sh-redirect! direction)
    #f
    to))


;; Add a single file redirection to a job
(define (job-redirect/file! job fd direction to)
  (span-insert-back! (job-redirects job)
    fd
    (redirect/file-symbol->char 'sh-redirect! direction)
    to
    (cond
      ((string? to)
        (when (fxzero? (string-length to))
          (raise-errorf 'sh-redirect! "invalid redirect to file, string must be non-empty: ~s" to))
        (string->utf8b/0 to))
      ((bytevector? to)
        (let ((to0 (bytevector->bytevector0)))
          (when (fxzero? (string-length to0))
            (raise-errorf 'sh-redirect! "invalid redirect to file, bytevector must be non-empty: ~a" to))
          to0))
      ((procedure? to)
        (when (zero? (logand 3 (procedure-arity-mask to)))
          (raise-errorf 'sh-redirect! "invalid redirect to procedure, must accept 0 or 1 arguments: ~a" to))
        #f)
      (#t
        (raise-errorf 'sh-redirect! "invalid redirect to fd or file, target must be a string, bytevector or procedure: ~s" to)))))


(define (redirect/fd-symbol->char caller symbol)
  (case symbol
    ((<&) #\<)
    ((>&) #\>)
    (else
      (raise-errorf caller "invalid redirect to fd direction, must be <& or >&: ~a" symbol))))


(define (redirect/file-symbol->char caller symbol)
  (case symbol
    ((<) #\<)
    ((>) #\>)
    ((<>) (integer->char #x2276)) ; #\≶
    ((>>) (integer->char #x00bb)) ; #\»
    (else
      (raise-errorf caller "invalid redirect to file direction, must be < > <> or >>: ~a" symbol))))


(define (redirect/fd-char->symbol caller ch)
  (case ch
    ((#\<) '<&)
    ((#\>) '>&)
    (else
      (raise-errorf caller "invalid redirect to fd character, must be <& or >&: ~a" ch))))


(define (redirect/file-char->symbol caller ch)
  (case (char->integer ch)
    ((#x3c) '<)
    ((#x3e) '>)
    ((#x2276) '<>)
    ((#x00bb) '>>)
    (else
      (raise-errorf caller "invalid redirect to file character, must be < <> > or >>: ~a" ch))))


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
  (let ((mj
    (%make-multijob #f -1 -1 '(new . 0)
      (span) #f '() ; redirections
      start-proc    ; executed to start the job
      next-proc     ; executed when a child job changes status
      (sh-cwd)      ; job working directory - initially current directory
      '()           ; overridden environment variables - initially none
      sh-globals    ; parent job - initially the global job
      kind
      -1            ; no child running yet
      (list->span children-jobs))))

    ;; set the parent of children-jobs
    (do ((tail children-jobs (cdr tail)))
        ((null? tail))
      (let ((elem (car tail)))
        (when (sh-job? elem)
          (job-parent-set! elem mj))))
    mj))


(define (assert-is-job who j)
  (assert* who (sh-job? j)))

;; Create a multijob to later start it. Each element in children-jobs must be a sh-job or subtype.


;; Create an "and" multijob
(define (sh-and . children-jobs)
  (apply make-multijob 'sh-and assert-is-job job-start/and job-step/and children-jobs))


;; Create an "or" multijob
(define (sh-or . children-jobs)
  (apply make-multijob 'sh-or  assert-is-job job-start/or job-step/or children-jobs))


;; Create a "not" multijob
(define (sh-not child-job)
  (make-multijob 'sh-not  assert-is-job job-start/not job-step/not child-job))


;; Create a "list" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-list . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-list
    (lambda (caller j) ; validate-job-proc
      (unless (job-terminator? j)
        (assert* caller (sh-job? j))))
    job-start/list
    job-step/list
    children-jobs-with-colon-ampersand))


;; Create a "subshell" multijob
;; Each argument must be a sh-job or subtype, possibly followed by a symbol ; &
(define (sh-subshell . children-jobs-with-colon-ampersand)
  (apply make-multijob 'sh-subshell
    (lambda (caller j) ; validate-job-proc
      (unless (job-terminator? j)
        (assert* caller (sh-job? j))))
    job-start/subshell
    job-run/subshell ; executed in child process
    children-jobs-with-colon-ampersand))


;; Return #t if token is a shell job terminator: ; &
(define (job-terminator? token)
  (and (symbol? token)
       (or (eq? token '&) (eq? token '\x3b;))))


;; Run next child job in a multijob containing an "and" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo && bar && baz
(define (job-step/and mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child-n (span-length (multijob-children mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (sh-ok? prev-child-status) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (start/any child '()))
      (begin
        ; previous child failed, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! mj prev-child-status)))))



;; Run next child job in a multijob containing an "or" of children jobs.
;; Used by (sh-and), implements runtime behavior of shell syntax foo || bar || baz
(define (job-step/or mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (and (not (sh-ok? prev-child-status)) (sh-job? child))
      (begin
        ; start next child job
        (multijob-current-child-index-set! mj idx)
        (start/any child '()))
      (begin
        ; previous child successful, or end of children
        (multijob-current-child-index-set! mj -1)
        (job-status-set! mj prev-child-status)))))



;; Run the child job in a multijob containing a "not" and one child job,
;; or collect the exit status of the child job after it exited.
;; Used by (sh-not), implements runtime behavior of shell syntax ! foo
(define (job-step/not mj prev-child-status)
  (assert* 'sh-not (fx=? 1 (sh-multijob-child-length mj)))

  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child   (sh-multijob-child-ref mj idx)))
    (if (sh-job? child)
      (begin
        ; start child job
        (multijob-current-child-index-set! mj idx)
        (start/any child '()))
      (begin
        ; child job exited, negate its exit status
        (multijob-current-child-index-set! mj -1)
        (job-status-set! mj (if (sh-ok? prev-child-status) '(exited . 1) (void)))))))



;; Run first or next child job in a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-list), implements runtime behavior of shell syntax foo; bar & baz
(define (job-step/list mj prev-child-status)
  (let* ((idx     (fx1+ (multijob-current-child-index mj)))
         (child-n (span-length (multijob-children mj)))
         (done?   #f))
    ; (debugf "job-step/list > ~s idx=~s prev-child-status=~s~%" mj (fx1- idx) prev-child-status)
    (assert* 'job-step/list (job-status-member? prev-child-status '(exited killed unknown)))
    ; idx = 0 if called by (job-start/list)
    (assert* 'job-step/list (fx>=? idx 0))
    (until (or done? (fx>? idx child-n))
      (let ((child (sh-multijob-child-ref mj idx)))
        ; (debugf "job-step-list status = ~s, start child ~s = ~s~%" (job-last-status mj) idx child)
        (cond
          ((sh-job? child)
            ; start next child job
            (multijob-current-child-index-set! mj idx)
            (start/any child '())
            (set! idx (fx1+ idx))
            ; all done, unless job is followed by '&
            ; in such case, continue spawning jobs after assigning a job-id to child job if it's running or stopped
            (if (eq? '& (sh-multijob-child-ref mj idx))
              (when (job-started? child)
                (job-id-set! child))
              (set! done? #t)))
          (child
            ; child is a symbol, either & or ;
            (set! idx (fx1+ idx)))
          (#t
            ; end of children reached. propagate status of last sync child
            (multijob-current-child-index-set! mj -1)
            (job-status-set! mj prev-child-status)
            (set! done? #t)))))))



;; Executed in child process:
;; run a multijob containing a sequence of children jobs optionally followed by & ;
;; Used by (sh-subshell), implements runtime behavior of shell syntax [ ... ]
(define (job-run/subshell mj dummy-prev-child-status)
  ; (debugf "%multijob-subshell/run ~s status = ~s~%" mj (job-last-status mj))
  (let ((children   (multijob-children mj))
        (pgid   (job-pgid mj))
        (status (void)))
    (span-iterate children
      (lambda (i job)
        (when (sh-job? job)
          ; run child job in parent's process group
          (start/any job (list pgid))
          ; wait for child job to exit, unless it's followed by '&
          (unless (eq? '& (sh-multijob-child-ref mj (fx1+ i)))
            (set! status (advance-job-or-id 'sh-subshell job))))
        #t)) ; keep iterating
    status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define precedence-lowest  0)
(define precedence-list    1)
(define precedence-or      2)
(define precedence-and     3)
(define precedence-pipe    4)
(define precedence-highest 4)

;; display a job using terse shell syntax {foo && bar || baz ...}
(define sh-job-display
  (case-lambda
    ((job-or-id)      (sh-job-display* job-or-id (current-output-port)))
    ((job-or-id port) (sh-job-display* job-or-id port))))


;; same as (sh-job-display), except that all arguments are mandatory
(define (sh-job-display* job-or-id port)
  (put-char port #\{)
  (job-display/any (sh-job job-or-id) port precedence-lowest)
  (put-char port #\}))


;; same as (sh-job-display), except that outputs to a string, which is returned
(define (sh-job-display/string job-or-id)
  (let-values (((port get-string) (open-string-output-port)))
    (sh-job-display* job-or-id port)
    (get-string)))


(define (job-display/any j port outer-precedence)
  (cond
    ((sh-multijob? j) (job-display/multijob j port outer-precedence))
    ((sh-cmd? j)      (job-display/cmd j port))
    (#t               (put-string port "???"))))


(define (job-display/multijob j port outer-precedence)
  (let* ((kind (multijob-kind j))
         (precedence
           (case kind
             ((sh-or)   precedence-or)
             ((sh-and)  precedence-and)
             ((sh-pipe) precedence-pipe)
             (else      precedence-list)))
         (separator
           (case kind
             ((sh-or)   " || ")
             ((sh-and)  " && ")
             ((sh-pipe) " | ")
             (else      " "))))
    (when (fx<=? precedence outer-precedence)
      (put-char port #\{))
    (span-iterate (multijob-children j)
      (lambda (i child)
        (unless (fxzero? i)
          (put-string port separator))
        (if (sh-job? child)
          (job-display/any child port precedence)
          (display child port))))
    (when (fx<=? precedence outer-precedence)
      (put-char port #\}))))


(define (job-display/cmd j port)
  (do ((tail (cmd-arg-list j) (cdr tail))
       (first? #t #f))
      ((null? tail))
    (unless first?
      (put-char port #\space))
    (let ((arg (car tail)))
      (if (string-is-shell-identifier? arg)
        (put-string port arg)
        (put-datum  port arg))))
  (job-display/redirects j port))


(define (job-display/redirects j port)
  (let ((redirects (job-redirects j)))
    (do ((i 0 (fx+ i 4))
         (n (span-length redirects)))
        ((fx>? (fx+ i 4) n))
      (job-display/redirect redirects i port))))


(define (job-display/redirect redirects i port)
  (let ((ch (span-ref redirects (fx1+ i)))
        (to (span-ref redirects (fx+ i 3))))
    (put-char port #\space)
    (put-datum port (span-ref redirects i))
    (put-string port (symbol->string (if (fixnum? to)
                                       (redirect/fd-char->symbol 'sh-job-write ch)
                                       (redirect/file-char->symbol 'sh-job-write ch))))
    (if (string-is-shell-identifier? to)
      (put-string port to)
      (put-datum port to))))


(define (string-is-shell-identifier? str)
  (and
    (string? str)
    (do ((i 0 (fx1+ i))
         (n (fx1- (string-length str))))
        ((or (fx>=? i n) (not (char-is-shell-identifier? (string-ref str i))))
         (fx>=? i n)))))


(define (char-is-shell-identifier? ch)
  (and (char? ch)
    (or (char<=? #\a ch #\z)
        (char<=? #\A ch #\Z)
        (char<=? #\0 ch #\9)
        (char<=? #\+ ch #\/)  ; i.e. one of #\+ #\, #\- #\. #\/
        (char=?  #\_ ch))))   ; i.e. #\_


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display a job using verbose Scheme syntax (sh-or (sh-and (sh-cmd "foo") (sh-cmd "bar")) ...)
(define sh-job-write
  (case-lambda
    ((job-or-id)      (sh-job-write* job-or-id (current-output-port)))
    ((job-or-id port) (sh-job-write* job-or-id port))))


;; same as (sh-job-display), except that all arguments are mandatory
(define (sh-job-write* job-or-id port)
  (job-write/any (sh-job job-or-id) port))

;; same as (sh-job-display), except that outputs to a string, which is returned
(define (sh-job-write/string job-or-id)
  (let-values (((port get-string) (open-string-output-port)))
    (sh-job-write* job-or-id port)
    (get-string)))


(define (job-write/any j port)
  (cond
    ((sh-multijob? j) (job-write/multijob j port))
    ((sh-cmd? j)      (job-write/cmd j port))
    (#t               (put-string port "???"))))


(define (job-write/multijob j port)
  (let ((kind (multijob-kind j)))
    (cond
      ((span-empty? (job-redirects j))
        (put-char port #\()
        (display kind port)
        (job-write/children j port)
        (put-char port #\)))
      (#t
        (job-write/multijob* j port)))))


(define (job-write/children j port)
  (span-iterate (multijob-children j)
    (lambda (i child)
      (if (symbol? child)
        (put-string port " '")
        (put-char   port #\space))
      (put-datum port child))))


(define (job-write/multijob* j port)
  (put-string port "(sh-redirect! (")
  (display (multijob-kind j) port)
  (job-write/children j port)
  (put-string port ")")
  (job-write/redirects j port)
  (put-string port ")"))


(define (job-write/cmd j port)
  (put-string port (if (span-empty? (job-redirects j)) "(sh-cmd" "(sh-cmd*"))
  (list-iterate (cmd-arg-list j)
    (lambda (arg)
      (put-char port #\space)
      (put-datum port arg)))
  (job-write/redirects j port)
  (put-string port ")"))


(define (job-write/redirects j port)
  (let ((redirects (job-redirects j)))
    (do ((i 0 (fx+ i 4))
         (n (span-length redirects)))
        ((fx>? (fx+ i 4) n))
      (job-write/redirect redirects i port))))


(define (job-write/redirect redirects i port)
  (let ((ch (span-ref redirects (fx1+ i)))
        (to (or (span-ref redirects (fx+ 2 i)) ; string, bytevector or procedure
                (span-ref redirects (fx+ 3 i))))) ; fd
    (put-char port #\space)
    (put-datum port (span-ref redirects i))
    (put-string port " '")
    (put-string port (symbol->string (if (fixnum? to)
                                       (redirect/fd-char->symbol 'sh-job-write ch)
                                       (redirect/file-char->symbol 'sh-job-write ch))))
    (put-char port #\space)
    (put-datum port to)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define job-print-with-scheme-syntax? #t)

;; customize how "job" and subtype objects are printed
(record-writer (record-type-descriptor job)
  (lambda (obj port writer)
    (if job-print-with-scheme-syntax?
      (sh-job-write* obj port)
      (sh-job-display* obj port))))

(begin
  (c-environ->sh-global-env)

  (sh-fd-allocate) ; mark highest fd as reserved: used by tty_fd

  (let ((t (sh-builtins)))
    ; additional builtins
    (hashtable-set! t "cd"      sh-builtin-cd)
    (hashtable-set! t "command" sh-builtin-command)
    (hashtable-set! t "pwd"     sh-builtin-pwd)))


) ; close library
