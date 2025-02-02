;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss




;; Return string value of environment variable named "name" for specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;;
;; Also returns the value of environment variables with visibility 'private.
;; To retrieve an environment variable *and* its visibility, use (sh-env-visibility-ref)
;;
;; If name is not found, return default if specified - otherwise return ""
;; Returned value is always a string or the specified default
(define sh-env-ref
  (case-lambda
    ((job-or-id name)         (sh-env-ref* job-or-id name ""))
    ((job-or-id name default) (sh-env-ref* job-or-id name default))))


;; Get the value of environment variable named "name" for specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;;
;; Also returns the value of environment variables with visibility 'private.
;; To retrieve an environment variable *and* its visibility, use (sh-env-visibility-ref)
;;
;; If name is not found, return default
;; Returned value is always a string or the specified default.
(define (sh-env-ref* job-or-id name default)
  (job-parents-iterate job-or-id
    (lambda (job)
      (let* ((vars (job-env job))
             (elem (if vars (hashtable-ref vars name #f) #f)))
        (when (pair? elem)
          (unless (eq? 'delete (car elem))
            (set! default (cdr elem)))
          #f)))) ; name found, stop iterating
  default)


;; Set an environment variable for specified job.
;; Optionally also sets the visibility flag to 'export or 'private
;; Note: visibility flag 'maintain preserves pre-existing visibility flag,
;;       or creates the variable as 'private if it does not exist.
(define sh-env-set!
  (case-lambda
   ((job-or-id name val)
    (sh-env-set*! job-or-id name val 'maintain))
   ((job-or-id name val visibility)
    (sh-env-set*! job-or-id name val visibility))))


;; Set an environment variable for specified job.
;; Also sets the visibility flag to 'export or 'private
;; Note: visibility flag 'maintain preserves pre-existing visibility flag,
;;       or creates the variable as 'private if it does not exist.
(define (sh-env-set*! job-or-id name val visibility)
  (assert* 'sh-env-set! (string? name))
  (assert* 'sh-env-set! (string? val))
  (assert* 'sh-env-set! (memq visibility '(export private maintain)))
  (let* ((vars (job-direct-env job-or-id))
         (elem (hashtable-ref vars name #f)))
    (if (pair? elem)
      (begin
        ; env variable already exist, overwrite it
        (set-cdr! elem val)
        (unless (eq? 'maintain visibility)
          (set-car! elem visibility)))
      (let ((visibility (if (eq? 'maintain visibility) 'private visibility)))
        ; env variable does not exist, create it
        (hashtable-set! vars name (cons visibility val))))))


;; Unset an environment variable for specified job.
;; Implementation note: inserts an entry with visibility 'delete,
;; in order to override any parent job's environment variable
;; with the same name.
(define (sh-env-delete! job-or-id name)
  (assert* 'sh-env-delete! (string? name))
  (let ((vars (job-direct-env job-or-id)))
    (hashtable-set! vars name (cons 'delete ""))))


;; Return the value and visibility of an environment variable for specified job.
;; First returned value is a string or #f: the value of environment variable,
;;   or #f if not found or has been deleted.
;; Second returned value is one of: 'export 'private #f
;;   where #f means the variable was not found or has been deleted.
(define (sh-env-visibility-ref job-or-id name)
  (let ((ret-val #f)
        (ret-visibility #f))
    (job-parents-iterate job-or-id
      (lambda (job)
        (let* ((vars (job-env job))
               (elem (if vars (hashtable-ref vars name #f) #f)))
          (when (pair? elem)
            (let ((visibility (car elem)))
              (unless (eq? 'delete visibility)
                (set! ret-visibility visibility)
                (set! ret-val        (cdr elem))))
            #f)))) ; name found, possibly deleted. stop iterating
    (values ret-val ret-visibility)))


;; Set ONLY the visibility of an environment variable for specified job to 'export or 'private
;; Return #t if succesful, or #f if the variable was not found or has been deleted.
(define (sh-env-visibility-set! job-or-id name visibility)
  (assert* 'sh-env-visibility! (string? name))
  (assert* 'sh-env-visibility! (memq visibility '(export private)))
  (let* ((job (sh-job job-or-id))
         ;; variable may be in a parent environment
         (val (sh-env-ref* job name #f)))
    (when val
      ;; (job-direct-env job) creates job environment if not yet present
      (hashtable-set! (job-direct-env job) name (cons visibility val)))
    (if val #t #f)))


;; Iterate on environment variables for specified job,
;; and call (proc name val visibility) on each variable.
;; Does *not* iterate on environment variables inherited from parent jobs.
;;
;; Stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc ...) returned truish,
;; otherwise returns #f.
(define (sh-env-iterate/direct job-or-id proc)
  (assert* 'sh-env-iterate/direct (procedure? proc))
  (assert* 'sh-env-iterate/direct (logbit? 3 (procedure-arity-mask proc)))
  (let ((vars (job-env (sh-job job-or-id))))
    (if vars
      (hashtable-iterate vars
        (lambda (cell)
          (proc (car cell) (cddr cell) (cadr cell))))
      #t)))



;; Set a lazy environment variable for specified job.
;; Note: lazy environment variables are copied into job's direct environment
;; only upon starting the job, *after* expanding the job's command line.
;;
;; To unset a lazy environment variable, call (sh-env-set/lazy! job-or-id name #f)
(define (sh-env-set/lazy! job-or-id name value-or-procedure)
  (let ((j (sh-job job-or-id)))
    (assert* 'sh-env-set/lazy! (string? name))
    (unless (string? value-or-procedure)
      (unless (procedure? value-or-procedure)
        (raise-errorf 'sh-env-set/lazy!
          "invalid environment variable value, must be a string or procedure: ~s"
          value-or-procedure))
      (when (zero? (logand 3 (procedure-arity-mask value-or-procedure)))
        (raise-errorf 'sh-env-set/lazy!
          "invalid environment variable procedure, must accept 0 or 1 arguments: ~s"
          value-or-procedure)))
    (let ((vars (job-env-lazy j)))
      (unless vars
        (set! vars (span))
        (job-env-lazy-set! j vars))
      (span-insert-back! vars name value-or-procedure))))


;; Execute the procedures in lazy environment of a job,
;; and copy the resulting env names and values into (job-direct-env)
;;
;; called automatically while starting a job.
(define (job-env/apply-lazy! j visibility)
  (assert* 'job-env/apply-lazy! (memq visibility '(maintain export private)))
  (let ((env-lazy (job-env-lazy j)))
    (when env-lazy
      (do ((i 0 (fx+ i 2))
           (n (span-length env-lazy)))
          ((fx>? (fx+ i 2) n))
        (let ((name  (span-ref env-lazy i))
              (value (job-env/apply1 j (span-ref env-lazy (fx1+ i)))))
          ; (debugf "job-env/apply-lazy! env name=~s, value=~s" name value)
          (if (eq? #f value)
            (sh-env-delete! j name)
            (sh-env-set*! j name value visibility)))))))


;; internal function called by job-env/apply-lazy!
(define (job-env/apply1 j value-or-procedure)
  (if (procedure? value-or-procedure)
    (if (logbit? 1 (procedure-arity-mask value-or-procedure))
      (value-or-procedure j)
      (value-or-procedure))
    value-or-procedure))


;; Return direct environment variables of job, creating them if needed.
;; Returned hashtable does not include default variables,
;; i.e. the ones inherited from parent jobs.
(define (job-direct-env job-or-id)
  (let* ((job (sh-job job-or-id))
         (vars (job-env job)))
    (unless vars
      (set! vars (make-hashtable string-hash string=?))
      (job-env-set! job vars))
    vars))


;; Repeatedly call C function c_environ_ref() and store returned (key . value)
;; environment variables into job (sh-globals).
;;
;; This function is usually only called once, during initialization of Scheme library
;; (schemesh shell) below.
(define c-environ->sh-global-env
  (let ((c-environ-ref (foreign-procedure "c_environ_ref" (uptr) ptr)))
    (lambda ()
      (do ((i 1 (fx+ i 1))
           (entry (c-environ-ref 0) (c-environ-ref i)))
          ((not (pair? entry)))
        (sh-env-set*! #t (car entry) (cdr entry) 'export)))))




;; Return a copy of job's environment variables,
;; including default variables inherited from parent jobs.
;; Argument which must be one of:
;;   'export: only exported variables are returned.
;;   'all : private variables are returned too.
;;
;; In both cases, job-env-lazy is included too.
(define (sh-env-copy job-or-id which)
  (assert* 'sh-env-copy (memq which '(export all)))
  (let* ((vars           (make-hashtable string-hash string=?))
         (also-private?  (eq? 'all which))
         (only-exported? (not also-private?)))
    (list-iterate (job-parents-revlist job-or-id)
      (lambda (job)
        (sh-env-iterate/direct job
          (lambda (name val visibility)
            (cond
              ((or (eq? 'delete visibility)
                   (and only-exported? (eq? 'private visibility)))
                (hashtable-delete! vars name))
              ((or (eq? 'export visibility)
                   (and also-private? (eq? 'private visibility)))
                (hashtable-set! vars name val)))))))
    vars))


;; Extract environment variables from specified job and all its parents,
;; and convert them to a vector of bytevector0.
;;
;; Argument "which" must be one of:
;;   'export - only exported variables are returned.
;;   'all    - private variables are returned too.
;;
;; In both cases, job-env-lazy is included too.
(define (sh-env->argv job-or-id which)
  (string-hashtable->argv (sh-env-copy job-or-id which)))


;; Copy overridden environment variables from specified job to its parent.
;; Ignores inherited environment variables.
;;
;; Called by (cmd-start) to implement the syntax "ENV_VAR" '= "VALUE"
;;   i.e. a command with environment variables but no arguments.
;;
;; Always returns (void)
(define (job-env-copy-into-parent! job)
  (let ((parent (job-parent job)))
    ;; (debugf ">  job-env-copy-into-parent! job=~s parent=~s" job parent)
    (when parent
      (sh-env-iterate/direct job
        (lambda (name val visibility)
          ;; (debugf "... job-env-copy-into-parent! name=~s visibility=~s val=~s " name visibility val)
          (if (eq? 'delete visibility)
            (sh-env-delete! parent name)
            (sh-env-set*! parent name val visibility))))))
    ;; (debugf "<  job-env-copy-into-parent!")
  (void))
