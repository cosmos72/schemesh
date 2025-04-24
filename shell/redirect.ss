;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

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
(define sh-start/fd-stdout
  (case-lambda
    ((job)
      (sh-start/fd-stdout job '()))
    ((job options)
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
            (sh-start job `(spawn? #t fd-close ,(car fds) ,@options))

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

        (car fds))))) ; return read-fd or #f


;; if status is one of:
;;   (exception ...)
;;   (killed 'sigint)
;;   (killed 'sigquit)
;; tries to kill (sh-current-job) then raises exception
(define (try-kill-current-job-or-raise status)
  ;; (debugf "try-kill-current-job-or-raise status=~s" status)
  (case (status->kind status)
    ((exception)
      (let ((ex (status->value status)))
        (sh-current-job-kill ex)
        ;; in case (sh-current-job-kill) returns
        (raise ex)))
    ((killed)
      (let ((signal-name (status->value status)))
        (when (signal-name-is-usually-fatal? signal-name)
          (sh-current-job-kill signal-name)
          ;; in case (sh-current-job-kill) returns
          (raise-condition-received-signal 'sh-run/bvector signal-name
                                           (case signal-name ((sigint) "user interrupt")
                                                             ((sigquit) "user quit")
                                                             (else #f))))))))


;; Simultaneous (fd-read-all read-fd) and (sh-wait job)
;; assumes that job writes to the peer of read-fd.
;;
;; Closes read-fd before returning.
;;
;; if job finishes with a status
;;   (exception ...)
;;   (killed 'sigint)
;;   (killed 'sigquit)
;; tries to kill (sh-current-job) then raises exception
(define (sh-wait/fd-read-all job read-fd)
  (parameterize ((sh-foreground-pgid (job-pgid job)))
    (let %loop ((bsp (make-bytespan 0)))
      (bytespan-reserve-right! bsp (fx+ 4096 (bytespan-length bsp)))
      (let* ((beg (bytespan-peek-beg bsp))
             (end (bytespan-peek-end bsp))
             (cap (bytespan-capacity-right bsp))
             (n   (fd-read-noretry read-fd (bytespan-peek-data bsp) end (fx+ beg cap))))
        (cond
          ((and (integer? n) (> n 0))
            (bytespan-resize-right! bsp (fx+ (fx- end beg) n))
            (%loop bsp))
          ((eq? #t n)
            (check-interrupts)
            (when (stopped? (job-last-status job))
              ;; react as is we received a SIGTSTP
              (signal-handler-sigtstp (signal-name->number 'sigtstp)))
            (job-kill job 'sigcont)
            (%loop bsp))
          (else ; end-of-file or I/O error
            ;; cannot move (fd-close) to the "after" section of a dynamic-wind,
            ;; because (check-interrupts) above may suspend us (= exit dynamic scope)
            ;; and resume us (= re-enter dynamic scope) multiple times
            (fd-close read-fd)
            (let ((status (job-wait 'sh-run/bvector job (sh-wait-flags continue-if-stopped wait-until-finished))))
              (try-kill-current-job-or-raise status))
            (bytespan->bytevector*! bsp)))))))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to bytespan.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start).
;;
;; If job finishes with a status
;;   (exception ...)
;;   (killed 'sigint)
;;   (killed 'sigquit)
;; tries to kill (sh-current-job) then raises exception.
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define sh-run/bvector
  (case-lambda
    ((job)
      (sh-run/bvector job '()))
    ((job options)
      (job-raise-if-started/recursive 'sh-run/bvector job)
      (%job-id-set! job -1) ;; prevents showing job notifications
      (let ((read-fd (sh-start/fd-stdout job options)))
        ;; WARNING: job may internally dup write-fd into (job-fds-to-remap)
        (sh-wait/fd-read-all job read-fd)))))



;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to UTF-8b string.
;;
;; Does NOT return early if job is stopped, use (sh-run/i) for that.
;; Options are the same as (sh-start)
;;
;; Implementation note: job is started from a subshell,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define sh-run/string
  (case-lambda
    ((job)
      (sh-run/string job '()))
    ((job options)
      (utf8b->string (sh-run/bvector job options)))))


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
(define sh-run/string-rtrim-newlines
  (case-lambda
    ((job)
      (sh-run/string-rtrim-newlines job '()))
    ((job options)
      (string-rtrim-newlines! (utf8b->string (sh-run/bvector job options))))))


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
(define sh-run/string-split-after-nuls
  (case-lambda
    ((job)
      (sh-run/string-split-after-nuls job '()))
    ((job options)
      (string-split-after-nuls (utf8b->string (sh-run/bvector job options))))))


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
  (span-insert-right! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect! direction)
    to
    #f))


;; Add a single file redirection to a job
(define (job-redirect/file! job fd direction to)
  (span-insert-right! (job-redirects job)
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
  (span-insert-left! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect! direction)
    to
    #f)
  (job-redirects-temp-n-set! job (fx+ 4 (job-redirects-temp-n job))))


;; Remove all temporary redirections from a job
(define (job-unredirect/temp/all! job)
  (span-delete-left! (job-redirects job) (job-redirects-temp-n job))
  (job-redirects-temp-n-set! job 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; called when starting a builtin or multijob:
;; create fd redirections and store them into (job-fds-to-remap)
;;
;; Reason: builtins and multijobs are executed in main schemesh process,
;; a redirection may overwrite fds 0 1 2 or some other fd already used
;; by main schemesh process, and we don't want to alter them:
;;
;; we need an additional layer of indirection that keeps track of the job's redirected fds
;; and to which (private) fds they are actually mapped to
(define (job-remap-fds! job)
  (let ((n (span-length (job-redirects job))))
    (unless (or (fxzero? n) (job-fds-to-remap job)) ; if fds are already remapped, do nothing
      (let ((job-dir (job-cwd-if-set job))
            (remaps  (make-eqv-hashtable n)))
        (job-fds-to-remap-set! job remaps)
        (do ((i 0 (fx+ i 4)))
            ((fx>? i (fx- n 4)))
          (job-remap-fd! job job-dir i))))))


;; redirect a file descriptor. returns < 0 on error
;; arguments: fd direction-ch to-fd-or-bytevector0 close-on-exec?
(define fd-redirect
  (foreign-procedure "c_fd_redirect" (ptr ptr ptr ptr) int))


;; called by (job-remap-fds!)
(define (job-remap-fd! job job-dir index)
  ;; redirects is span of quadruplets (fd mode to-fd-or-path-or-closure bytevector0)
  (let* ((redirects            (job-redirects job))
         (fd                   (span-ref redirects index))
         (direction-ch         (span-ref redirects (fx1+ index)))
         (to-fd-or-bytevector0 (job-extract-redirection-to-fd-or-bytevector0 job job-dir redirects index))
         (remap-fd             (s-fd-allocate)))
    ;; (debugf "job-remap-fd! fd=~s dir=~s remap-fd=~s to=~s" fd direction-ch remap-fd to-fd-or-bytevector0)
    (let* ((fd-int (s-fd->int remap-fd))
           (ret (fd-redirect fd-int direction-ch to-fd-or-bytevector0 #t))) ; #t close-on-exec?
      (when (< ret 0)
        (s-fd-release remap-fd)
        (raise-c-errno 'sh-start 'c_fd_redirect ret fd-int direction-ch to-fd-or-bytevector0)))
    (hashtable-set! (job-fds-to-remap job) fd remap-fd)))



;; extract the destination fd or bytevector0 from a redirection
(define (job-extract-redirection-to-fd-or-bytevector0 job job-dir redirects index)
  (%prefix-job-dir-if-relative-path job-dir
    (or (span-ref redirects (fx+ 3 index))
        (let ((to (span-ref redirects (fx+ 2 index))))
          (if (procedure? to)
            (if (logbit? 1 (procedure-arity-mask to)) (to job) (to))
            to)))))


(define (%prefix-job-dir-if-relative-path job-dir path-or-fd)
  (cond
    ((fixnum? path-or-fd)
      path-or-fd)
    ((or (string? path-or-fd) (bytevector? path-or-fd))
      (let ((bvec (text->bytevector0 path-or-fd))
            (slash 47))
        (if (and job-dir (not (fx=? slash (bytevector-u8-ref bvec 0))))
          (let ((bspan (charspan->utf8b job-dir)))
            (unless (or (bytespan-empty? bspan) (fx=? slash (bytespan-ref-right/u8 bspan)))
              ;; append / after job's directory if missing
              (bytespan-insert-right/u8! bspan slash))
            (bytespan-insert-right/bvector! bspan bvec)
            (bytespan->bytevector bspan))
          bvec)))
    ;; wildcards may expand to a list of strings: accept them if they have length 1
    ((and (pair? path-or-fd) (null? (cdr path-or-fd)) (string? (car path-or-fd)))
      (%prefix-job-dir-if-relative-path job-dir (car path-or-fd)))
    (else
      (raise-assert1 'job-remap-fds
        "(or (fixnum? path-or-fd) (string? path-or-fd) (bytevector? path-or-fd))"
        path-or-fd))))


;; release job's remapped fds and unset (job-fds-to-remap job)
(define (job-unmap-fds! job)
  (let ((remap-fds (job-fds-to-remap job)))
    (when remap-fds
      (for-hash-values ((fd remap-fds))
        (when (s-fd-release fd)
          ;; (debugf "job-unmap-fds! fd-close ~s" (s-fd->int fd))
          (fd-close (s-fd->int fd))))
      (job-fds-to-remap-set! job #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; return three values:
;;   the ancestor job, its target file descriptor for specified fd, and its remapped file descriptor for specified fd.
;;   or #f fd if no remapping was found
(define (job-remap-find-fd* job fd)
  (let %again ((job job) (job-with-redirect #f) (target-fd fd) (last-remap-fd fd))
    (let* ((remap-fds (and job (job-fds-to-remap job)))
           (remap-fd  (and remap-fds (hashtable-ref remap-fds last-remap-fd #f))))
      (if remap-fd
        (%again (job-parent job) job last-remap-fd (s-fd->int remap-fd))
        (let ((parent (and job (job-parent job))))
          (if parent
            (%again parent job-with-redirect target-fd last-remap-fd)
            (values job-with-redirect target-fd last-remap-fd)))))))


;; return the remapped file descriptor for specified job's fd,
;; or fd itself if no remapping was found
(define (job-remap-find-fd job fd)
  (let-values (((unused1 unused2 ret-fd) (job-remap-find-fd* job fd)))
    ret-fd))


;; return (job-ports job), creating it if needed
(define (job-ensure-ports job)
  (or (job-ports job)
      (let ((ports (make-eqv-hashtable)))
        (job-ports-set! job ports)
        ports)))


(define (job-ensure-binary-port job target-fd remapped-fd)
  (let ((ports (job-ensure-ports job)))
    (or (hashtable-ref ports remapped-fd #f)
        (let ((port (fd->port remapped-fd 'rw 'binary (buffer-mode block)
                              (string-append "fd " (number->string target-fd)))))
          (hashtable-set! ports remapped-fd port)
          port))))


(define (job-ensure-textual-port job target-fd remapped-fd)
  (let ((ports (job-ensure-ports job)))
    (or (hashtable-ref ports (fxnot remapped-fd) #f)
        (let* ((binary-port (job-ensure-binary-port job target-fd remapped-fd))
               (port        (port->utf8b-port binary-port (buffer-mode block))))
          (hashtable-set! ports (fxnot remapped-fd) port)
          port))))


;; return the binary input/output port for specified job's fd (crearing it if needed)
;; or raise exception if no remapping was found
(define (job-remap-find-binary-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (unless parent
      (raise-errorf 'sh-binary-port "port not found for file descriptor ~s in job ~s" fd job))
    (job-ensure-binary-port parent target-fd remapped-fd)))


;; return the textual input/output port for specified job's fd (crearing it if needed)
;; or raise exception if no remapping was found
(define (job-remap-find-textual-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (unless parent
      (raise-errorf 'sh-textual-port "port not found for file descriptor ~s in job ~s" fd job))
    (job-ensure-textual-port parent target-fd remapped-fd)))




;; Return the actual file descriptor to use inside a job
;; for reading from, or writing to, logical file descriptor N.
;; Needed because jobs can run in main process and have per-job redirections.
(define sh-fd
  (case-lambda
    ((job-or-id fd)
      (job-remap-find-fd (sh-job job-or-id) fd))
    ((fd)
      (sh-fd #f fd))))


;; Return the binary input/output port to use inside a job for reading/writing specified file descriptor.
;; Needed because jobs can run in main process and have per-job redirections.
(define sh-binary-port
  (case-lambda
    ((job-or-id fd)
      (job-remap-find-binary-port (sh-job job-or-id) fd))
    ((fd)
      (sh-binary-port #f fd))))


;; Return the binary input/output port to use inside a job for reading/writing specified file descriptor.
;; Needed because jobs can run in main process and have per-job redirections.
(define sh-textual-port
  (case-lambda
    ((job-or-id fd)
      (job-remap-find-textual-port (sh-job job-or-id) fd))
    ((fd)
      (sh-textual-port #f fd))))
