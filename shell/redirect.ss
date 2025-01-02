;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should only be included inside a (library ...) definition



;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to bytespan.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is started from a subshell,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define (sh-run/bspan job . options)
  (let ((parent (job-parent job)))
    ; create pipe fds, both are close-on-exec
    (let-values (((read-fd write-fd) (open-pipe-fds #t #t)))
      ; temporarily suppress messages about started/completed jobs
      (parameterize ((sh-job-display/summary? #f))
        (dynamic-wind
          void ; run before body
          (lambda ()
            (let ((jj (sh-subshell job))) ; also sets job's parent to jj

              (sh-redirect! jj 1 '>& write-fd) ; redirect jj stdout to write-fd

              (apply sh-start jj options)
              ; close our copy of write-fd: needed to detect eof on read-fd
              (fd-close write-fd)
              (set! write-fd #f)
              ;; WARNING: job may internally dup write-fd into (job-fds-to-remap)
              (let ((ret (fd-read-until-eof read-fd)))
                (sh-wait jj)
                ret)))
          (lambda ()
            ; restore job's original parent
            (job-parent-set! job parent)
            ; close pipe fds
            (when write-fd
              (fd-close write-fd))
            (fd-close read-fd)))))))


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
  (let* ((bsp (apply sh-run/bspan job options))
         (beg (bytespan-peek-beg bsp))
         (end (bytespan-peek-end bsp)))
    (utf8b-range->string (bytespan-peek-data bsp) beg (fx- end beg))))


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
  (let* ((bsp (apply sh-run/bspan job options))
         (bv  (bytespan-peek-data bsp))
         (beg (bytespan-peek-beg bsp))
         (end (bytespan-peek-end bsp)))
    (while (and (fx>? end beg) (fx=? 10 (bytevector-u8-ref bv (fx1- end))))
      (set! end (fx1- end)))
    (utf8b-range->string bv beg (fx- end beg))))


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
