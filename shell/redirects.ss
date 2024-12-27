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

(library (schemesh shell redirects (0 1))
  (export
    sh-run/bspan sh-run/string sh-redirect!)
  (import
    (rnrs)
    (only (chezscheme) logand procedure-arity-mask)
    (only (schemesh bootstrap) raise-errorf until)
    (schemesh containers bytespan)
    (schemesh containers span)
    (schemesh containers utf8b)
    (schemesh conversions)
    (schemesh posix fd)
    (only (schemesh shell jobs) sh-job sh-start sh-wait)
    (schemesh shell internals))


;; Copy-pasted from jobs.ss: define the record type "job"
(define-record-type
  (job %make-job %job?)
  (fields
    (mutable id job-id %job-id-set!) ; fixnum: job id in (sh-globals), #f if not set
    (mutable pid)               ; fixnum: process id,       -1 if unknown
    (mutable pgid)              ; fixnum: process group id, -1 if unknown
     ; cons: last known status, or (void) if job exited successfully
    (mutable last-status job-last-status %job-last-status-set!)
    ; span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
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
    (mutable env)               ; #f or hashtable of overridden env variables: name -> value
    (mutable env-assignments)   ; #f or span of env variable name followed by #<procedure>
    (mutable parent))           ; parent job, contains default values of env variables
                                ; and default redirections
  (nongenerative #{job ghm1j1xb9o5tkkhhucwauly2c-1175}))


;; Copy-pasted from jobs.ss: define the record type "cmd"
(define-record-type
  (cmd %make-cmd %cmd?)
  (parent job)
  (fields arg-list) ; list of strings: program-name and args
  (nongenerative #{cmd ghm1j1xb9o5tkkhhucwauly2c-1176}))


;; Copy-pasted from jobs.ss: define the record type "multijob"
(define-record-type
  (multijob %make-multijob %multijob?)
  (parent job)
  (fields
    kind                ; symbol: one of 'sh-and 'sh-or 'sh-not 'sh-list 'sh-subshell 'sh-global
    (mutable current-child-index) ; -1 or index of currently running child job
    children)           ; span: children jobs.
  (nongenerative #{multijob ghm1j1xb9o5tkkhhucwauly2c-1177}))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to bytespan.
;;
;; FIXME: unfinished!
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
(define (sh-run/bspan job . options)
  (let ((redirect-len (span-length (job-redirects job))))
    ; create pipe fds
    (let-values (((read-fd write-fd) (open-pipe-fds #t #t))) ; both fds are close-on-exec?
      (dynamic-wind
        (lambda ()
          ; add temporarary redirection 1 >& write-fd
          (sh-redirect! job 1 '>& write-fd))
        (lambda ()
          (apply sh-start job options)
          ; close our copy of write-fd: needed to detect eof on read-fd
          (fd-close write-fd)
          (set! write-fd #f)
          ;; WARNING: job may internally dup write-fd into (job-fds-to-remap)
          ;; FIXME: (sh-wait) also advances multijob - we should automatically
          ;;        do that from (sh-consume-signals) and call it periodically,
          ;;        including when a syscall returns EINTR
          (let ((ret (fd-read-until-eof read-fd)))
            (sh-wait job)
            ret))
        (lambda ()
          ; remove temporary redirection
          (span-resize-back! (job-redirects job) redirect-len)
          ; close pipe fds
          (when write-fd
            (fd-close write-fd))
          (fd-close read-fd))))))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to UTF-8b string.
;;
;; FIXME: unfinished!
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
(define (sh-run/string job . options)
  (let* ((bsp (apply sh-run/bspan job options))
         (beg (bytespan-peek-beg bsp))
         (end (bytespan-peek-end bsp)))
    (utf8b-range->string (bytespan-peek-data bsp) beg (fx- end beg))))


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
  (unless (fx>=? fd 0)
    (raise-errorf 'sh-redirect! "invalid redirect fd, must be an unsigned fixnum: ~a" fd))
  (if (or (eq? '<& direction) (eq? '>& direction))
    (job-redirect/fd!   job fd direction to)
    (job-redirect/file! job fd direction to)))


;; Add a single fd redirection to a job
(define (job-redirect/fd! job fd direction to)
  (unless (fx>=? to -1)
    (raise-errorf 'sh-redirect! "invalid redirect to fd, must be -1 or an unsigned fixnum: ~a" to))
  (span-insert-back! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect! direction)
    to
    #f))


;; Add a single file redirection to a job
(define (job-redirect/file! job fd direction to)
  (span-insert-back! (job-redirects job)
    fd
    (%sh-redirect/file-symbol->char 'sh-redirect! direction)
    to
    (cond
      ((string? to)
        (when (fxzero? (string-length to))
          (raise-errorf 'sh-redirect! "invalid redirect to file, string must be non-empty: ~s" to))
        (string->utf8b/0 to))
      ((bytevector? to)
        (let ((to0 (bytevector->bytevector0 to)))
          (when (fx<=? (bytevector-length to0) 1)
            (raise-errorf 'sh-redirect! "invalid redirect to file, bytevector must be non-empty: ~a" to))
          to0))
      ((procedure? to)
        (when (zero? (logand 3 (procedure-arity-mask to)))
          (raise-errorf 'sh-redirect! "invalid redirect to procedure, must accept 0 or 1 arguments: ~a" to))
        #f)
      (#t
        (raise-errorf 'sh-redirect! "invalid redirect to fd or file, target must be a string, bytevector or procedure: ~s" to)))))



) ; close library
