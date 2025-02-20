;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; low-level utilities
(define (%sh-redirect/fd-symbol->char caller symbol)
  (case symbol
    ((<&) #\<)
    ((>&) #\>)
    (else
      (raise-errorf caller "invalid redirect to fd direction, must be <& or >&: ~a" symbol))))


(define (%sh-redirect/file-symbol->char caller symbol)
  (case symbol
    ((<) #\<)
    ((>) #\>)
    ((<>) (integer->char #x2276)) ; #\≶
    ((>>) (integer->char #x00bb)) ; #\»
    (else
      (raise-errorf caller "invalid redirect to file direction, must be < > <> or >>: ~a" symbol))))


(define (%sh-redirect/fd-char->symbol caller ch)
  (case ch
    ((#\<) '<&)
    ((#\>) '>&)
    (else
      (raise-errorf caller "invalid redirect to fd character, must be <& or >&: ~a" ch))))


(define (%sh-redirect/file-char->symbol caller ch)
  (case (char->integer ch)
    ((#x3c) '<)
    ((#x3e) '>)
    ((#x2276) '<>)
    ((#x00bb) '>>)
    (else
      (raise-errorf caller "invalid redirect to file character, must be < <> > or >>: ~a" ch))))


;; Start a job and return immediately.
;; Redirects job's standard output to a pipe and returns the read side of that pipe,
;; which is an integer file descriptor.
;;
;; May raise exceptions. On errors, return #f.
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-start/fd-stdout job . options)
  (options-validate 'sh-start/fd-stdout options)
  (let ((fds (cons #f #f))
        (err? #t))
    (dynamic-wind
      (lambda () ; run before body
        ; create pipe fds, both are close-on-exec
        (let-values (((read-fd write-fd) (open-pipe-fds #t #t)))
          (set-car! fds read-fd)
          (set-cdr! fds write-fd)))

      (lambda () ; body
        ; temporarily redirect job's stdout to write-fd.
        ; redirection is automatically removed by (job-status-set!) when job finishes.
        (job-redirect/temp/fd! job 1 '>& (cdr fds))
        ; always start job in a subprocess, see above for reason.
        (sh-start* job (cons '(spawn? . #t) options))

        ; close our copy of write-fd: needed to detect eof on read-fd
        (fd-close (cdr fds))
        (set-cdr! fds #f)

        ; job no longer needs fd remapping:
        ; they also may contain a dup() of write-fd
        ; which prevents detecting eof on read-fd
        ; (debugf "pid ~s: sh-start/fd-stdout calling (job-unmap-fds) job=~s" (pid-get) job)

        (job-unmap-fds! job)
        (set! err? #f))

      (lambda () ; after body
        ; close our copy of write-fd: needed to detect eof on read-fd
        (when (cdr fds)
          (fd-close (cdr fds))
          (set-cdr! fds #f))

        (when (and err? (car fds))
          (fd-close (car fds))
          (set-car! fds #f))))

    (car fds))) ; return read-fd or #f


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to bytespan.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-run/bvector job . options)
  (let ((read-fd #f))
    ; temporarily suppress messages about started/completed jobs
    (parameterize ((sh-job-display-summary? #f))
      (dynamic-wind
        void
        (lambda ()
          (set! read-fd (apply sh-start/fd-stdout job options))
          ;; WARNING: job may internally dup write-fd into (job-fds-to-remap)
          (fd-read-until-eof read-fd))
        (lambda ()
          (when read-fd
            (fd-close read-fd))
          (when (job-started? job)
            (sh-wait job)))))))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to UTF-8b string.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is started from a subshell,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-run/string job . options)
  (utf8b->string (apply sh-run/bvector job options)))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to UTF-8b string,
;; removing final newlines.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is started from a subshell,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-run/string-rtrim-newlines job . options)
  (string-rtrim-newlines! (utf8b->string (apply sh-run/bvector job options))))


;; Start a job and wait for it to exit.
;; Reads job's standard output, converts it to UTF-8b string,
;; splits such string after each #\nul character
;; and returns the list of strings produced by such splitting.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is started from a subshell,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-run/string-split-after-nuls job . options)
  (string-split-after-nuls (utf8b->string (apply sh-run/bvector job options))))


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
          (else
            (raise-errorf 'sh-redirect! "invalid redirect, first argument must a fixnum or a redirection symbol: ~s" args)))))
    job))


;; Append a single redirection to a job
(define (job-redirect! job fd direction to)
  (unless (fx>=? fd 0)
    (raise-errorf 'sh-redirect! "invalid redirect fd, must be an unsigned fixnum: ~a" fd))
  (if (or (eq? '<& direction) (eq? '>& direction))
    (job-redirect/fd!   job fd direction to)
    (job-redirect/file! job fd direction to)))


;; Append a single fd redirection to a job
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
      (else
        (raise-errorf 'sh-redirect! "invalid redirect to fd or file, target must be a string, bytevector or procedure: ~s" to)))))


;; Prefix a single temporary fd redirection to a job
(define (job-redirect/temp/fd! job fd direction to)
  (unless (fx>=? to -1)
    (raise-errorf 'sh-redirect! "invalid redirect to fd, must be -1 or an unsigned fixnum: ~a" to))
  (span-insert-front! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect! direction)
    to
    #f)
  (job-redirects-temp-n-set! job (fx+ 4 (job-redirects-temp-n job))))


;; Remove all temporary redirections from a job
(define (job-unredirect/temp/all! job)
  (span-erase-front! (job-redirects job) (job-redirects-temp-n job))
  (job-redirects-temp-n-set! job 0))
