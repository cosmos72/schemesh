;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


(define-record-type (job %make-job sh-job?)
  (fields
    (mutable id job-id %job-id-set!)       ; #f or fixnum >= 0: job id in (sh-globals)
    (mutable oid)                          ; #f or fixnum >= 0: previous value of job id
    (mutable pid  job-pid  %job-pid-set!)  ; #f or integer > 0: process id
    (mutable pgid job-pgid %job-pgid-set!) ; #f or integer > 0: process group id
    (mutable pgid-fg %job-pgid-fg %job-pgid-fg-set!) ; #f or integer > 0: current fg process group id of job, may be a child process spawned by job
    ;; status: last known status
    (mutable last-status job-last-status %job-last-status-set!)
    ;; #f or exception that caused the job to terminate
    (mutable exception)
    ;; span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
    ;; to open and redirect between fork() and exec()
    (mutable redirects)
    (mutable redirects-temp-n) ; fixnum, number elements at front of (job-redirects)
                               ; inserted by temporary redirections
    (mutable fds-to-remap) ; for builtins or multijobs, #f or hashmap job-logical-fd -> (s-fd) to actually use
    (mutable ports)        ; #f or hashtable fd -> binary input/output port,
                           ; and (fxnot fd) -> textual input/output port.
    start-proc      ; #f or procedure to run in main process.
                    ; receives as argument job followed by options.
    (mutable step-proc) ; #f or procedure.
                    ; For multijobs, will be called when a child job changes status.
                    ; For cmds, will be called in fork()ed child process and
                    ; receives as argument job followed by options.
                    ; For cmds, its return value is passed to (exit-with-status)
    (mutable cwd %job-cwd %job-cwd-set!) ; charspan: working directory. if #f, use parent's cwd
    (mutable owd %job-owd %job-owd-set!) ; #f or charspan: previous working directory
    (mutable env)         ; #f or hashtable of overridden env variables: name -> value
    (mutable env-lazy)    ; #f or span of env variable name each followed by string or procedure
    (mutable temp-parent) ; #f or temporary parent job, contains default values of env variables.
                          ; Set to #f when job finishes
    (mutable default-parent)) ; default parent job, contains default values of env variables, cwd and redirections
  (nongenerative job-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; Define the record type "cmd"
(define-record-type (cmd %make-cmd sh-cmd?)
  (parent job)
  (fields
    arg-list                     ; list of strings and closures: program-name and args
    (mutable expanded-arg-list)) ; #f or list of strings: program-name and args after applying closures and expanding aliases
  (nongenerative cmd-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; Define the record type "jexpr"
(define-record-type (jexpr %make-sh-expr sh-expr?)
  (parent job)
  (fields
    proc                    ; procedure to call for executing the job
    label                   ; #f or source fragment that compiled to proc
    (mutable resume-proc)   ; #f or continuation to resume job
    (mutable suspend-proc)) ; #f or continuation to suspend job and return to whoever started/resumed it
  (nongenerative jexpr-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;; Define the record type "multijob"
(define-record-type (multijob %make-multijob sh-multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-not 'sh-list 'sh-subshell '#<global>
    (mutable current-child-index) ; -1 or index of currently running child job
    children)           ; span: children jobs.
  (nongenerative multijob-7c46d04b-34f4-4046-b5c7-b63753c1be40))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    convert  pid -> job     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convert pid to job, return #f if job not found
(define (pid->job pid)
  (assert* 'pid->job (fixnum? pid))
  (hashtable-ref (sh-pid-table) pid #f))

;; Adds an entry to the global hashtable pid -> job
(define (pid->job-set! pid job)
  (when pid
    (assert* 'pid->job-set! (fixnum? pid))
    (assert* 'pid->job-set! (sh-job? job))
    (hashtable-set! (sh-pid-table) pid job)))

;; Removes an entry from the global hashtable pid -> job
(define (pid->job-delete! pid)
  (when pid
    (assert* 'pid->job-delete! (fixnum? pid))
    (hashtable-delete! (sh-pid-table) pid)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;   manage job's id, pid and pgid   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; return the id of a job-or-id, or #f if not set
(define (sh-job-id job-or-id)
  (job-id   (sh-job job-or-id)))


;; return the process group id of a job-or-id, or #f if not set
(define (sh-job-pid job-or-id)
  (job-pid (sh-job job-or-id)))


;; return the process group id of a job-or-id, or #f if not set
(define (sh-job-pgid job-or-id)
  (job-pgid (sh-job job-or-id)))




;; set the process id of specified job
(define (job-pid-set! job new-pid)
  (let ((old-pid (job-pid job)))
    (when old-pid
      (pid->job-delete! old-pid)))
  (when new-pid
    (pid->job-set! new-pid job))
  (%job-pid-set! job new-pid))

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
      (else
        ; unset job's process group id
        #f))))

;; get the foreground process group id of specified job
(define (job-pgid-fg job)
  (or (%job-pgid-fg job)
      (job-pgid     job)))


(define (sh-job<? job1 job2)
  (let ((id1 (job-id job1))
        (id2 (job-id job2)))
    (cond
      ((and id1 id2) (fx<? id1 id2))
      (id1   #t) ; jobs with id compare "smaller than" jobs without id
      (id2   #f)
      (else
        (let ((pid1 (job-pid job1))
              (pid2 (job-pid job2)))
          (cond
            ((and pid1 pid2) (< pid1 pid2))
            (pid1   #t) ; jobs with pid compare "smaller than" jobs without pid
            (pid2   #f)
            (else   #f))))))) ; no id, no pid => compare "equal"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   sh-job-copy   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Create a copy of job and all its children jobs, and return it.
;; Returned job will have no id, status 'new and specified parent, or (sh-globals) if not specified
(define sh-job-copy
  (case-lambda
    ((job parent)
      (cond ((sh-cmd?      job) (cmd-copy      job parent))
            ((sh-multijob? job) (multijob-copy job parent))
            (raise-errorf 'sh-job-copy "~s is not a sh-cmd or a sh-multijob" job)))
    ((job) (sh-job-copy job (sh-globals)))))


;; Create a copy of sh-cmd j, and return it.
;; Returned job will have no id, status 'new and specified parent.
(define (cmd-copy j parent)
  (%make-cmd
    #f #f #f #f #f       ; id oid pid pgid pgid-fg
    (new) #f             ; status exception
    (let ((redirects (job-redirects j)))
      (span-copy redirects (job-redirects-temp-n j) (span-length redirects)))
    0 #f #f              ; redirects-temp-n fds-to-remap ports
    (job-start-proc j)
    (job-step-proc  j)
    (let ((cwd (%job-cwd j)))
      (and cwd (charspan-copy cwd)))
    (let ((owd (job-owd j)))
      (and owd (charspan-copy owd)))
    (let ((env (job-env j)))
      (and env (hashtable-copy env)))
    (let ((env-lazy (job-env-lazy j)))
      (and env-lazy (span-copy env-lazy)))
    #f                   ; temp-parent
    parent               ; default-parent
    (list-copy (cmd-arg-list j)) ; copy arg-list
    #f))                 ; expanded-arg-list


;; Create a copy of sh-multijob j, and return it.
;; Returned job will have no id, status 'new and specified parent.
;; Children jobs will be copied recursively.
(define (multijob-copy j parent)
  (let* ((children (job-span-copy (multijob-children j)))
         (ret
    (%make-multijob
      #f #f #f #f #f   ; id oid pid pgid pgid-fg
      (new) #f         ; status exception
      (let ((redirects (job-redirects j)))
        ;; skip temporary redirects, copy the rest
        (span-copy redirects (job-redirects-temp-n j) (span-length redirects)))
      0 #f #f              ; redirects-temp-n fds-to-remap ports
      (job-start-proc j)
      (job-step-proc  j)
      (let ((cwd (%job-cwd j)))
        (and cwd (charspan-copy cwd)))
      (let ((owd (job-owd j)))
        (and owd (charspan-copy owd)))
      (let ((env (job-env j)))
        (and env (hashtable-copy env)))
      (let ((env-lazy (job-env-lazy j)))
        (and env-lazy (span-copy env-lazy)))
      #f                   ; temp-parent
      parent               ; default-parent
      (multijob-kind j)
      -1                   ; current-child-index
      children)))
    (span-iterate children
      (lambda (i elem)
        (when (sh-job? elem)
          (job-default-parent-set! elem ret))))
    ret))


;; Create a copy of span containing jobs and symbols, and return it.
;; Returned jobs will have no id, status 'new and parent (sh-globals)
;; Children jobs will be copied recursively.
(define (job-span-copy src)
  (let ((dst (make-span (span-length src))))
    (span-iterate src
      (lambda (i elem)
        (span-set! i dst (if (sh-job? elem)
                           (sh-job-copy elem)
                           elem))))
    dst))
