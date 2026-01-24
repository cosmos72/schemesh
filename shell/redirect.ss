;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
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
    ((<>&) (integer->char #x2276)) ; #\≶
    (else
      (raise-errorf caller "invalid redirect to fd direction ~s is not one of <& >& <>&" symbol))))


(define (%sh-redirect/file-symbol->char caller symbol)
  (case symbol
    ((<) #\<)
    ((>) #\>)
    ((<>) (integer->char #x2276)) ; #\≶
    ((>>) (integer->char #x00bb)) ; #\»
    (else
      (raise-errorf caller "invalid redirect to file direction ~s is not one of < > <> >>" symbol))))


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


(define (%sh-redirect/char->direction ch)
  (case (char->integer ch)
    ((#x3c)        'read)
    ((#x3e #x00bb) 'write)
    (else          'rw)))


(define (fd? obj)
  (and (fixnum? obj) (fx<? -1 obj (fd-open-max))))


;; return normally if redirections is a plist where keys are file descriptors
;; and values are one of '< '> '<>
;; otherwise raise exception.
(define (assert-redirection-plist caller redirections)
  (unless (null? redirections)
    (assert* caller (pair? redirections))
    (assert* caller (pair? (cdr redirections)))
    (let ((fd  (car redirections))
          (dir (cadr redirections)))
      (assert* caller (fd? fd))
      (%sh-redirect/fd-symbol->char caller dir)
      (assert-redirection-plist caller (cddr redirections)))))


;; create pipe or socket fds to redirect child process file descriptors.
;; all created fds are close-on-exec: child will get a dup2() of them
(define (open-redirection-fds redirections fdv i)
  (unless (null? redirections)
    (let ((from-fd (car redirections))
          (dir     (cadr redirections)))
      (if (memq dir '(<& >&))
        ;; create pipe fds, all are close-on-exec
        (let-values (((read-fd write-fd) (pipe-fds #t #t)))
          (vector-set! fdv i
            ;; in fdv, each pair is (our-fd . child-fd)
            (if (eq? dir '<&) (cons write-fd read-fd)
                              (cons read-fd write-fd))))
        ;; direction is '<>&
        ;; create socketpair fds, all are close-on-exec
        (let-values (((sock1 sock2) (socketpair-fds #t #t)))
          (vector-set! fdv i (cons sock1 sock2)))))
    ;; iterate on remaining redirections
    (open-redirection-fds (cddr redirections) fdv (fx1+ i))))


;; temporarily redirect job's file descriptors.
;; redirections are automatically removed by (job-status-set!) when job finishes.
(define (job-redirect-temp-fds! job redirections fdv i options)
  (if (null? redirections)
    `(spawn? #t ,@options)
    (let ((to-fds  (vector-ref fdv i))
          (from-fd (car redirections))
          (dir     (cadr redirections)))
      ;; in fdv, each pair is (our-fd . child-fd)
      (job-redirect-temp-fd! job from-fd dir (cdr to-fds)) ; child to-fd

      ;; iterate on remaining redirections, and instruct child to close (car to-fds) i.e. our-fd
      (job-redirect-temp-fds! job (cddr redirections) fdv (fx1+ i)
                              `(fd-close ,(car to-fds) ,@options)))))


(define (close-redirection-fds redirections fdv i err?)
  (unless (null? redirections)
    (let* ((from-fd  (car redirections))
           (to-fds   (vector-ref fdv i))
           (our-fd   (car to-fds))
           (child-fd (cdr to-fds)))
      (when (and err? our-fd (fx>=? our-fd 0))
        ;; close our side of each pair
        (fd-close our-fd)
        (set-car! to-fds #f))
      (when (and child-fd (fx>=? child-fd 0))
        ;; close child side of each pair
        (fd-close child-fd)
        (set-cdr! to-fds #f)))
    (close-redirection-fds (cddr redirections) fdv (fx1+ i) err?)))


;; given a vector of pairs, return a list containing the car of each pair.
(define (extract-vector-cars v)
  (let %extract-vector-cars ((ret '()) (i (fx1- (vector-length v))))
     (if (fx<? i 0)
       ret
       (%extract-vector-cars (cons (car (vector-ref v i)) ret) (fx1- i)))))


;; Start a job and return immediately.
;; Redirects job's file descriptors to pipes and returns the other side of such pipes,
;; which are a list of integer file descriptors.
;;
;; Optional argument redirections must be a plist containing zero or more pairs, whose
;;   car is file-descriptor-to-redirect: a small fixnum, usually 0, 1 or 2
;;   cdr is direction: a symbol, must be one of: '<& '>& '<>&
;; If redirections is not specified, it defaults to '(0 <& 1 >& 2 >&)
;;
;; May raise exceptions.
;; On errors redirecting a file descriptor, such file descriptor may be returned as #f.
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define sh-start/fds
  (case-lambda
    ((job redirections options)
      (assert-redirection-plist 'sh-start/fds redirections)
      (options-validate 'sh-start/fd-stdout options)
      (let* ((n    (fxarithmetic-shift-right (length redirections) 1))
             ;; vector of pipe pairs. car is our side, cdr is child side
             (fdv  (make-vector n))
             (err? #t))
        (dynamic-wind
          (lambda () ; run before body
            (open-redirection-fds redirections fdv 0))

          (lambda () ; body
            (let ((options (job-redirect-temp-fds! job redirections fdv 0 options)))
              ;; always start job in a subprocess, see above for reason.
              (sh-start job options))

            ;; close the fds we don't use: needed to detect eof on read fds
            (close-redirection-fds redirections fdv 0 #f)

            ;; job no longer needs fd remapping:
            ;; they also may contain a dup() of write-fd
            ;; which prevents detecting eof on read-fd
            ;; (debugf "pid ~s: sh-start/fd-stdout calling (job-unmap-fds) job=~s" (pid-get) job)

            (job-unmap-fds! job)
            (set! err? #f))

          (lambda () ; after body
            ;; close the fds we don't use: needed to detect eof on read fds
            (close-redirection-fds redirections fdv 0 err?)))

        (extract-vector-cars fdv))) ; return list of our fds, or list of multiple #f on error
    ((job redirections)
      (sh-start/fds job redirections '()))
    ((job)
      (sh-start/fds job '(0 <& 1 >& 2 >&) '()))))


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
    ((job job-options)
      ;; return read-fd or #f
      (car (sh-start/fds job '(1 >&) job-options)))
    ((job)
      ;; return read-fd or #f
      (car (sh-start/fds job '(1 >&) '())))))


;; Start a job and return immediately.
;; Redirects job's file descriptors to pipes and returns the other side of such pipes,
;; converted to binary or textual ports.
;;
;; Arguments:
;;   mandatory job            the sh-job to start
;;   optional redirections    a plist containing zero or more pairs,
;;                            whose car is file-descriptor-to-redirect: a small fixnum, usually 0, 1 or 2
;;                            whose cdr is direction: a symbol, must be one of: '<& '>& '<>&
;;                            if not specified, defaults to '(0 <& 1 >& 2 >&)
;;   optional transcoder-sym  must be one of: 'binary 'textual 'utf8b and defaults to 'textual
;;   optional b-mode          a buffer-mode, defaults to 'block
;;   optional job-options     a possibly empty list as described in (sh-options)
;;
;; May raise exceptions.
;; On errors redirecting a file descriptor, such file descriptor may be returned as #f.
;;
;; Implementation note: job is always started in a subprocess,
;; because we need to read its standard output while it runs.
;; Doing that from the main process may deadlock if the job is a multijob or a builtin.
(define sh-start/ports
  (case-lambda
    ((job redirections transcoder-sym b-mode job-options)
      (let ((ret (sh-start/fds job redirections job-options))
            (err? #t))
        (dynamic-wind
          void
          (lambda ()
            (do ((l ret (cdr l))
                 (redirs redirections (cddr redirs)))
                ((null? l))
              (let ((fd (car l)))
                (when fd
                  ;; we return the other side of pipes created for child job => reverse direction
                  (set-car! l (fd->port fd (case (cadr redirs) ((<&) 'write) ((>&) 'read) (else 'rw))
                                        transcoder-sym b-mode
                                        (string-append "job-pipe-fd " (number->string (car redirs)))
                                        (lambda () (fd-close fd)))))))
            (set! err? #f)
            ret)
          (lambda ()
            (when err?
              (for-list ((x ret))
                (cond
                  ((fixnum? x) (fd-close x))
                  ((port? x)   (close-port x)))))))))
    ((job redirections transcoder-sym b-mode)
      (sh-start/ports job redirections transcoder-sym b-mode '()))
    ((job redirections transcoder-sym)
      (sh-start/ports job redirections transcoder-sym 'block '()))
    ((job redirections)
      (sh-start/ports job redirections 'textual 'block '()))
    ((job)
      (sh-start/ports job '(0 <& 1 >& 2 >&) 'textual 'block '()))))



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
          (raise-condition-received-signal 'sh-run/bytevector signal-name
                                           (case signal-name ((sigint) "user interrupt")
                                                             ((sigquit) "user quit")
                                                             (else #f))))))))


;; Simultaneous (fd-read-all read-fd) and (sh-wait job)
;; assumes that job writes to the peer of read-fd.
;;
;; Blocks until read-fd reaches #!eof, then blocks again until job exits.
;;
;; Closes read-fd before returning, and returns bytevector produced by (fd-read-all read-fd)
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
          ((and (fixnum? n) (fx>? n 0))
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
            (let ((status (job-wait 'sh-run/bytevector job (sh-wait-flags continue-if-stopped wait-until-finished))))
              (try-kill-current-job-or-raise status))
            (bytespan->bytevector*! bsp)))))))


;; Start a job and wait for it to exit.
;; Reads job's standard output and returns it converted to bytevector.
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
(define sh-run/bytevector
  (case-lambda
    ((job options)
      (job-raise-if-started/recursive 'sh-run/bytevector job)
      (%job-id-set! job -1) ;; prevents showing job notifications
      (let ((read-fd (sh-start/fd-stdout job options)))
        ;; WARNING: job may internally dup write-fd into (job-fds-to-remap)
        (sh-wait/fd-read-all job read-fd)))
    ((job)
      (sh-run/bytevector job '()))))



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
      (utf8b->string (sh-run/bytevector job options)))))


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
      (string-rtrim-newlines! (utf8b->string (sh-run/bytevector job options))))))


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
      (string-split-after-nuls (utf8b->string (sh-run/bytevector job options))))))


;; Add zero or more redirections to a job. Return the job.
;; Each redirection must be a two-argument DIRECTION TO-FD-OR-FILE-PATH
;; or a three-argument FROM-FD DIRECTION TO-FD-OR-FILE-PATH
(define sh-redirect
  (case-lambda
    ((job dir to)
      (job-redirect! job (if (memq dir '(< <&)) 0 1) dir to))
    ((job fd dir to)
      (job-redirect! job fd dir to))
    ((job dir1 to1 dir2 to2)
      (job-redirect! job (if (memq dir1 '(< <&)) 0 1) dir1 to1)
      (job-redirect! job (if (memq dir2 '(< <&)) 0 1) dir2 to2))
    ((job . args)
      (let %sh-redirect ((job job) (args args))
        (cond
          ((null? args)
            job)
          ((null? (cdr args))
            (raise-errorf 'sh-redirect "invalid redirect, need two or three arguments, found one: ~s" args))
          ((fixnum? (car args))
            (when (null? (cddr args))
              (raise-errorf 'sh-redirect "invalid three-argument redirect, found only two arguments: ~s" args))
            (job-redirect! job (car args) (cadr args) (caddr args))
            (%sh-redirect job (cdddr args)))
          ((redirection-sym? (car args))
            (let ((dir (car args)))
              (job-redirect! job (if (eq? '<& dir) 0 1) dir (cadr args)))
            (%sh-redirect job (cddr args)))
          (else
            (raise-errorf 'sh-redirect "invalid redirect, first argument must a fixnum or a redirection symbol: ~s" args)))))))


;; Append a single fd or file redirection to a job
(define (job-redirect! job fd direction to)
  (unless (sh-job? job)
    (raise-errorf 'sh-redirect "~s is not a job" job))
  (unless (and (fixnum? fd) (fx>=? fd 0))
    (raise-errorf 'sh-redirect "invalid redirect fd, must be an unsigned fixnum: ~a" fd))
  (if (or (eq? '<& direction) (eq? '>& direction))
    (job-redirect/fd!   job fd direction to)
    (job-redirect/file! job fd direction to))
  job)


;; Append a single fd redirection to a job
(define (job-redirect/fd! job fd direction to)
  (unless (fx>=? to -1)
    (raise-errorf 'sh-redirect "invalid redirect to fd, must be -1 or an unsigned fixnum: ~a" to))
  (span-insert-right! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect direction)
    to
    #f))


;; Add a single file redirection to a job
(define (job-redirect/file! job fd direction to)
  (span-insert-right! (job-redirects job)
    fd
    (%sh-redirect/file-symbol->char 'sh-redirect direction)
    to
    (cond
      ((string? to)
        (when (fxzero? (string-length to))
          (raise-errorf 'sh-redirect "invalid redirect to file, string must be non-empty: ~s" to))
        (string->utf8b/0 to))
      ((bytevector? to)
        (let ((to0 (bytevector->bytevector0 to)))
          (when (fx<=? (bytevector-length to0) 1)
            (raise-errorf 'sh-redirect "invalid redirect to file, bytevector must be non-empty: ~a" to))
          to0))
      ((procedure? to)
        (when (zero? (logand 3 (procedure-arity-mask to)))
          (raise-errorf 'sh-redirect "invalid redirect to procedure, must accept 0 or 1 arguments: ~a" to))
        #f)
      (else
        (raise-errorf 'sh-redirect "invalid redirect to fd or file, target must be a string, bytevector or procedure: ~s" to)))))


;; Prefix a single temporary fd redirection to a job
(define (job-redirect-temp-fd! job fd direction to)
  (unless (fx>=? to -1)
    (raise-errorf 'sh-redirect "invalid redirect to fd, must be -1 or an unsigned fixnum: ~a" to))
  (span-insert-left! (job-redirects job)
    fd
    (%sh-redirect/fd-symbol->char 'sh-redirect direction)
    to
    #f)
  (job-redirects-temp-n-set! job (fx+ 4 (job-redirects-temp-n job))))


;; Remove all temporary redirections from a job
(define (job-unredirect/temp/all! job)
  (span-delete-left! (job-redirects job) (job-redirects-temp-n job))
  (job-redirects-temp-n-set! job 0))


;; find the job's last redirection for file descriptor fd,
;; and return index of such redirection in job-redirects,
;; or #f if not found
(define (job-find-redirection job fd)
  (let* ((sp (job-redirects job))
         (n  (fxand -4 (span-length sp))))
    (let %job-find-redirection ((i (fx- 4 n)))
      (cond
        ((fx<? i 0)
          #f)
        ((fx=? fd (span-ref sp i))
          i)
        (else
          (%job-find-redirection (fx- 4 i)))))))



;; given a job redirection index, return the direction of such redirection,
;; represented as one of: 'read 'write 'rw
;; or #f if index is out-of-range or not a fixnum
(define (job-redirection-dir job i)
  (and (fixnum? i)
    (let* ((sp (job-redirects job))
           (n  (span-length sp)))
      (and (fx<=? 0 i (fx- n 4))
        (%sh-redirect/char->direction (span-ref sp (fx1+ i)))))))


;; given a job redirection index, return the fd or string path of such redirection,
;; or #f if index is out-of-range or not a fixnum
(define (job-redirection-to job i)
  (and (fixnum? i)
    (let* ((sp (job-redirects job))
           (n  (span-length sp)))
      (and (fx<=? 0 i (fx- n 4))
        (let ((to (span-ref sp (fx+ i 2))))
          (if (or (fixnum? to) (string? to))
            to
            (let ((bv0 (span-ref sp (fx+ i 3))))
              (if (bytevector? bv0)
                (let* ((str (utf8b->string bv0))
                       (len (string-length str)))
                  (when (and (not (fxzero? len)) (char=? #\nul (string-ref str (fx1- len))))
                    (string-truncate! str (fx1- n)))
                  str)))))))))



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
            (bytespan-insert-right/bytevector! bspan bvec)
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
        (let* ((redirect-i (job-find-redirection job target-fd))
               (dir        (or (job-redirection-dir job redirect-i) 'rw))
               (to         (or (job-redirection-to  job redirect-i) target-fd))
               (port       (fd->port remapped-fd dir 'binary (buffer-mode block)
                              (if (string? to) to (string-append "sh-fd " (number->string target-fd))))))
          ;; (debugf "created binary  port ~s\tfor remapping ~s -> ~s\tof job ~a" port target-fd remapped-fd (sh-job->string job))
          (hashtable-set! ports remapped-fd port)
          port))))


(define (job-ensure-textual-port job target-fd remapped-fd)
  (let ((ports (job-ensure-ports job)))
    (or (hashtable-ref ports (fxnot remapped-fd) #f)
        (let* ((binary-port (job-ensure-binary-port job target-fd remapped-fd))
               (port        (port->utf8b-port binary-port (%port->direction binary-port) (buffer-mode block))))
          ;; (debugf "created textual port ~s\tfor remapping ~s -> ~s\tof job ~a" port target-fd remapped-fd (sh-job->string job))
          (hashtable-set! ports (fxnot remapped-fd) port)
          port))))

(define (%port->direction port)
  (let ((i (fxior (if (input-port?  port) 1 0)
                  (if (output-port? port) 2 0))))
    (vector-ref '#(rw read write rw) i)))

;; return the binary input/output port for specified job's fd (creating it if needed)
;; or raise exception if no remapping was found
(define (job-remap-ensure-binary-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (unless parent
      (raise-errorf 'sh-port "port not found for file descriptor ~s in job ~s" fd job))
    (job-ensure-binary-port parent target-fd remapped-fd)))


;; return the binary input/output port for specified job's fd, or #f if it was not created yet.
;; Return #f if no remapping was found
(define (job-remap-find-binary-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (and parent
         (let ((ports (job-ports parent)))
           (and ports
                (hashtable-ref ports remapped-fd #f))))))


;; return the textual input/output port for specified job's fd (creating it if needed)
;; or raise exception if no remapping was found
(define (job-remap-ensure-textual-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (unless parent
      (raise-errorf 'sh-port "port not found for file descriptor ~s in job ~s" fd job))
    (job-ensure-textual-port parent target-fd remapped-fd)))


;; return the textual input/output port for specified job's fd, or #f if it was not created yet.
;; Return #f if no remapping was found
(define (job-remap-find-textual-port job fd)
  (let-values (((parent target-fd remapped-fd) (job-remap-find-fd* job fd)))
    (and parent
         (let ((ports (job-ports parent)))
           (and ports
                (hashtable-ref ports (fxnot remapped-fd) #f))))))



;; Return the actual file descriptor to use inside a job
;; for reading from, or writing to, logical file descriptor N.
;; Needed because jobs can run in main process and have per-job redirections.
(define sh-fd
  (case-lambda
    ((job-or-id fd)
      (job-remap-find-fd (sh-job job-or-id) fd))
    ((fd)
      (sh-fd #f fd))))


;; Return the binary or textual input/output port to use inside a job for reading/writing specified file descriptor,
;; creating it if needed.
;; Needed because jobs can run in main process and have per-job redirections.
(define sh-port
  (case-lambda
    ((job-or-id fd transcoder-sym)
      (let ((job (sh-job job-or-id)))
        (case transcoder-sym
          ((binary)
            (job-remap-ensure-binary-port job fd))
          ((textual utf8b)
            (job-remap-ensure-textual-port job fd))
          (else
            (let ((allowed-transcoder-syms '(binary textual utf8b)))
              (assert* 'sh-port (memq transcoder-sym allowed-transcoder-syms)))))))
    ((job-or-id fd)
      (sh-port job-or-id fd 'textual))
    ((fd)
      (sh-port #f fd 'textual))))


;; flush current-...-port and corresponding per-job binary and textual ports if they have been created
(define (flush-ports-if-needed fd current-port)
  (if (fxzero? (textual-port-output-index current-port))
    ;; current-...-port has nothing to flush, only flush per-job ports
    (let* ((job       (or (sh-current-job) (sh-globals)))
           (bin-port  (job-remap-find-binary-port job fd)))
      (when (and bin-port (output-port? bin-port))
        (let ((txt-port (job-remap-find-textual-port job fd)))
          (cond
            ((and txt-port (not (fxzero? (textual-port-output-index txt-port))))
              (flush-output-port txt-port))
            ((not (fxzero? (binary-port-output-index bin-port)))
              (flush-output-port bin-port))))))
    ;; current-...-port has something to flush, flushing it will create per-job binary and textual ports if needed
    (flush-output-port current-port)))


(define (sh-stdio-flush)
  ;; the ports (console-input-port) (console-output-port) (console-error-port)
  ;; are unbuffered, no need to flush them
  ;;
  ;; flushing (current-...-port) also flushes the corresponding per-job binary and textual ports
  (flush-ports-if-needed 0 (current-input-port))
  (flush-ports-if-needed 1 (current-output-port))
  (flush-ports-if-needed 2 (current-error-port)))
