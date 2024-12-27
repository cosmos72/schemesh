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

(library (schemesh shell env (0 1))
  (export
    sh-env sh-env! sh-env-unset! sh-env-exported? sh-env-export! sh-env-set+export!
    sh-env/lazy!
    sh-builtin-cd sh-builtin-pwd sh-cwd-set! sh-cd sh-pwd)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)               foreign-procedure include logand procedure-arity-mask void)
    (only (schemesh bootstrap)       assert* raise-errorf)
    (only (schemesh posix fd)        c-errno->string fd-write)
    (schemesh containers bytespan)
    (schemesh containers charspan)
    (only (schemesh containers span) span span-insert-back!)
    (only (schemesh containers misc) assert-string-list?)
    (schemesh containers utf8b)
    (schemesh containers utils)
    (only (schemesh shell fds)       sh-fd-stdout)
    (only (schemesh shell builtins)  sh-builtins)
    (schemesh shell paths)
    (schemesh shell jobs))


;; define the record type "job" and its accessors
(include "shell/internals.ss")


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
;; To clear a lazy environment variable, users can call (sh-env/lazy! job-or-id name "")
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


;; Copy-pasted from shell/jobs.ss:
;; call (proc job) on given job and each of its
;; parents. Stops iterating if (proc) returns #f.
(define (job-parents-iterate job-or-id proc)
  (do ((parent (sh-job job-or-id) (job-parent parent)))
      ((or (not (sh-job? parent)) (not (proc parent))))))


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


;; set the current directory of specified job or job-id to specified path.
;; path must be a string or charspan.
;;
;; if job-or-id resolves to sh-globals, it is equivalent to (sh-cd path).
;;
;; in all other cases, path is taken as-is, i.e. it is not normalized
;; and is not validated against filesystem contents.
(define (sh-cwd-set! job-or-id path)
  (let ((job (sh-job job-or-id)))
    (if (eq? job sh-globals)
      (sh-cd path)
      (job-cwd-set! job (if (charspan? path) path (string->charspan* path))))))



;; change current directory to specified path.
;; path must be a a string or charspan.
(define sh-cd
  (case-lambda
    (()     (sh-cd* (sh-env sh-globals "HOME")))
    ((path) (sh-cd* path))
    ((path . extra-args) (raise-errorf 'cd "too many arguments"))))

;; internal function called by (sh-cd)
(define sh-cd*
  (let ((c_chdir (foreign-procedure "c_chdir" (ptr) int)))
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
    (()   (sh-pwd* (sh-fd-stdout)))
    ((fd) (sh-pwd* fd))))


(define (sh-pwd* fd)
  (let ((wbuf (make-bytespan 0)))
    (bytespan-insert-back/cspan! wbuf (sh-cwd))
    (bytespan-insert-back/u8! wbuf 10) ; newline
     ; TODO: loop on short writes
    (fd-write fd (bytespan-peek-data wbuf)
              (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf))
    (void)))

;; the "cd" builtin
(define (sh-builtin-cd job prog-and-args options)
  (assert-string-list? 'sh-builtin-cd prog-and-args)
  (apply sh-cd (cdr prog-and-args)))


;; the "pwd" builtin
(define (sh-builtin-pwd job prog-and-args options)
  (assert-string-list? 'sh-builtin-pwd prog-and-args)
  (sh-pwd))


(begin
  (c-environ->sh-global-env)

  (let ((t (sh-builtins)))
    ; additional builtins
    (hashtable-set! t "cd"      sh-builtin-cd)
    (hashtable-set! t "pwd"     sh-builtin-pwd)))

) ; close library
