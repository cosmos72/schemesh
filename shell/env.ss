;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.





;; Return string value of environment variable named "name" for specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;; If name is not found, return default if specified - otherwise return ""
(define sh-env
  (case-lambda
    ((job-or-id name)         (sh-env* job-or-id name ""))
    ((job-or-id name default) (sh-env* job-or-id name default))))


;; Return string value of environment variable named "name" for specified job.
;; If name is not found in job's direct environment, also search in environment
;; inherited from parent jobs.
;; If name is not found, return default
(define (sh-env* job-or-id name default)
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
(define (sh-env! job-or-id name val)
  (assert* 'sh-env! (string? val))
  (let* ((vars (job-direct-env job-or-id))
         (elem (hashtable-ref vars name #f)))
    (if (pair? elem)
      (set-cdr! elem val)
      (hashtable-set! vars name (cons 'private val)))))


;; Unset an environment variable for specified job.
;; Implementation note: inserts an entry that means "deleted",
;; in order to override any parent job's environment variable
;; with the same name.
(define (sh-env-unset! job-or-id name)
  (let ((vars (job-direct-env job-or-id)))
    (hashtable-set! vars name (cons 'delete ""))))


(define (sh-env-exported? job-or-id name)
  (let ((ret #f))
    (job-parents-iterate job-or-id
      (lambda (job)
        (let* ((vars (job-env job))
               (elem (if vars (hashtable-ref vars name #f) #f)))
          (when (pair? elem)
            (set! ret (eq? 'export (car elem)))
            #f)))) ; name found, stop iterating
    ret))


(define (sh-env-export! job-or-id name exported?)
  (assert* 'sh-env-export! (boolean? exported?))
  (let* ((job (sh-job job-or-id))
         ; val may be in a parent environment
         (val (sh-env job name))
         (export (if exported? 'export 'private)))
    ; (job-direct-env job) creates job environment if not yet present
    (hashtable-set! (job-direct-env job) name (cons export val))))


;; combined sh-env! and sh-env-export!
(define (sh-env-set+export! job-or-id name val exported?)
  (assert* 'sh-env-set+export! (string? val))
  (assert* 'sh-env-set+export! (boolean? exported?))
  (let* ((vars (job-direct-env job-or-id))
         (export (if exported? 'export 'private)))
    (hashtable-set! vars name (cons export val))))


;; Set a lazy environment variable for specified job.
;; Note: lazy environment variables are copied into job's direct environment
;; only upon starting the job, *after* expanding the job's command line.
;;
;; To unset a lazy environment variable, call (sh-env/lazy! job-or-id name #f)
(define (sh-env/lazy! job-or-id name value-or-procedure)
  (let ((j (sh-job job-or-id)))
    (assert* 'sh-env/lazy! (string? name))
    (unless (string? value-or-procedure)
      (unless (procedure? value-or-procedure)
        (raise-errorf 'sh-env/lazy!
          "invalid environment variable value, must be a string or procedure: ~s"
          value-or-procedure))
      (when (zero? (logand 3 (procedure-arity-mask value-or-procedure)))
        (raise-errorf 'sh-env/lazy!
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
(define (job-env/apply-lazy! j)
  (let ((env-lazy (job-env-lazy j)))
    (when env-lazy
      (do ((i 0 (fx+ i 2))
           (n (span-length env-lazy)))
          ((fx>? (fx+ i 2) n))
        (let ((name  (span-ref env-lazy i))
              (value (job-env/apply1 j (span-ref env-lazy (fx1+ i)))))
          (debugf "job-env/apply-lazy! env name=~s, value=~s~%" name value)
          (if (eq? #f value)
            (sh-env-unset! j name)
            (sh-env-set+export! j name value #t)))))))


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
;; environment variables into (sh-global-env).
;;
;; This function is usually only called once, during initialization of Scheme library
;; (schemesh shell) below.
(define c-environ->sh-global-env
  (let ((c-environ-ref (foreign-procedure "c_environ_ref" (uptr) ptr)))
    (lambda ()
      (do ((i 1 (fx+ i 1))
           (entry (c-environ-ref 0) (c-environ-ref i)))
          ((not (pair? entry)))
        (sh-env-set+export! sh-globals (car entry) (cdr entry) #t)))))




;; return global environment variables
(define (sh-global-env)
  (job-env sh-globals))


;; Return a copy of job's environment variables,
;; including default variables inherited from parent jobs.
;; Argument which must be one of:
;;   'exported: only exported variables are returned.
;;   'all : unexported variables are returned too.
;;
;; In both cases, job-env-lazy is included too.
(define (sh-env-copy job-or-id which)
  (assert* 'sh-env-copy (memq which '(exported all)))
  (let* ((jlist (job-parents-revlist job-or-id))
         (vars (make-hashtable string-hash string=?))
         (also-unexported? (eq? 'all which))
         (only-exported? (not also-unexported?)))
    (list-iterate jlist
      (lambda (job)
        (let ((env (job-env job)))
          (when env
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


;; Extract environment variables from specified job and all its parents,
;; and convert them to a vector of bytevector0.
;;
;; Argument "which" must be one of:
;; 'exported: only exported variables are returned.
;; 'all : unexported variables are returned too.
;;
;; In both cases, job-env-lazy is included too.
(define (sh-env->argv job-or-id which)
  (string-hashtable->argv (sh-env-copy job-or-id which)))
