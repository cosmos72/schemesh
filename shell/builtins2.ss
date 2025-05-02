;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; write contents of bytespan wbuf to file descriptor fd,
;; then clear bytespan wbuf
;;
;; returns (void)
(define (fd-write/bytespan! fd wbuf)
  (fd-write-all fd (bytespan-peek-data wbuf)
                (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf))
  (bytespan-clear! wbuf))


;; write warning or error message to file descriptor fd.
;;
;; returns (void)
(define (fd-write-strings: fd prefix strings)
  (let ((wbuf (bytespan)))
    (bytespan-insert-right/string! wbuf prefix)
    (for-list ((arg strings))
      (bytespan-insert-right/u8! wbuf 58 32) ; ": "
      (bytespan-insert-right/string! wbuf arg)
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bytespan! fd wbuf)))
    (bytespan-insert-right/u8! wbuf 10)
    (fd-write/bytespan! fd wbuf)))


;; print warning message to (sh-fd 2)
;; always returns (void)
(define (write-builtin-warning . args)
  (fd-write-strings: (sh-fd 2) "; warning" args)
  (void))


;; print error message to (sh-fd 2)
;; always returns (failed 1)
(define (write-builtin-error . args)
  (fd-write-strings: (sh-fd 2) "schemesh" args)
  (failed 1))


;; the "builtin" builtin: run the builtin in the remaining command line.
;;
;; As all builtins do, must return job status.
;; returns (failed 1) if specified builtin is not found.
(define (builtin-builtin job prog-and-args options)
  ;; (debugf "builtin-builtin ~s" prog-and-args)
  (assert-string-list? 'builtin-builtin prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void)
    (let* ((args (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (if builtin
        (builtin-start builtin job args options)
        (write-builtin-error "builtin" "not a shell builtin" (car args))))))


;; the "command" builtin: spawn a subprocess and return immediately
;;
;; As all builtins do, must return job status.
(define (builtin-command job prog-and-args options)
  (assert-string-list? 'builtin-command prog-and-args)
  (assert* 'builtin-command (string=? "command" (car prog-and-args)))
  (cmd-spawn job (cdr prog-and-args) options)) ; returns job status


;; the "exec" builtin: replace the current process with specified command.
;; Returns only if command cannot be executed.
;;
;; As all builtins do, must return job status.
(define (builtin-exec job prog-and-args options)
  (assert-string-list? 'builtin-exec prog-and-args)
  (assert* 'builtin-exec (string=? "exec" (car prog-and-args)))
  ;; save history before this process is replaced by exec'd command
  (let ((lctx (repl-args-linectx)))
    (when (linectx? lctx)
      (linectx-save-history lctx)))
  ;; on success, does not return: this process does not exist anymore.
  ;; on failure, returns job status
  (cmd-exec job (list->argv (cdr prog-and-args)) options))


;; the "exit" builtin: call current exit-handler, optionally with specified exit status.
;;
;; Never returns normally.
(define (builtin-exit job prog-and-args options)
  (assert-string-list? 'builtin-exit prog-and-args)
  (let ((arg (list->integer-or-false (cdr prog-and-args))))
    (exit (or arg 0))))


;; the "export" builtin: show exported environment variables,
;; or export one or more environment variables of parent job
;;
;; As all builtins do, must return job status.
(define (builtin-export job prog-and-args options)
  (assert-string-list? 'builtin-export prog-and-args)
  (let ((parent (job-parent job)))
    (if (null? (cdr prog-and-args))
      (%env-display-vars parent 'export) ; returns (void)
      (begin
        (for-list ((name (cdr prog-and-args)))
          (sh-env-visibility-set! parent name 'export))
        (void)))))


(define (try-string->base10-integer str)
  (if (string-is-signed-base10-integer? str)
    (string->number str)
    str))


;; the "status" builtin: return specified exit status,
;; which must be a non-empty string containing only decimal digits.
;;
;; As all builtins do, must return job status.
(define (builtin-status job prog-and-args options)
  (assert-string-list? 'builtin-status prog-and-args)
  (let ((result (if (null? (cdr prog-and-args))
                  0
                  (try-string->base10-integer (cadr prog-and-args)))))
    (if (eqv? 0 result)
      (ok     (void))
      (failed result))))


;; extract a job-id from string list prog-and-args and return it,
;; or return preferred job id if prog-and-args is null
(define (prog-and-args->job-id prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (values (sh-preferred-job-id) #f)
    (let ((arg (cadr prog-and-args)))
      (values
        (if (string-is-unsigned-base10-integer? arg) (string->number arg) -1)
        arg))))


;; extract a job-id from string list prog-and-args and return the corresponding job,
;; or return preferred job if prog-and-args is null
(define (prog-and-args->job prog-and-args)
  (let-values (((id arg) (prog-and-args->job-id prog-and-args)))
    (values (sh-find-job id) arg)))


;; The "bg" builtin: continue a job-id by sending SIGCONT to it, and return immediately
;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;;
;; As all builtins do, must return job status. For possible returned statuses, see (sh-bg)
(define (builtin-bg job prog-and-args options)
  (assert-string-list? 'builtin-bg prog-and-args)
  (let-values (((job arg) (prog-and-args->job prog-and-args)))
      (if job
        (let ((new-status (sh-bg job)))
          (if (finished? new-status)
            ;; job finished, return its exit status as "bg" exit status.
            new-status
            ;; job still exists, show its running/stopped status.
            ;; return (void) i.e. builtin "fg" exiting successfully.
            (queue-job-display-summary job)))
        (write-builtin-error "bg" (or arg "\"\"") "no such job")))) ; returns (failed 1)


;; The "fg" builtin: continue a job-id by sending SIGCONT to it, then wait for it to exit or stop.
;;
;; As all builtins do, must return job status. For possible returned statuses, see (sh-fg)
(define (builtin-fg job prog-and-args options)
  (assert-string-list? 'builtin-fg prog-and-args)
  (let-values (((job arg) (prog-and-args->job prog-and-args)))
    (if job
      (let ((out (current-output-port)))
        (unless (or arg (job-finished? job))
          ;; show the preferred job being resumed
          (sh-job-display-summary job (running (job-id job)) out)
          (flush-output-port out))
        (let ((new-status (sh-fg job)))
          (if (finished? new-status)
            ;; job finished, return its exit status as "fg" exit status.
            new-status
            ;; job still exists, show its running/stopped status.
            ;; return (void) i.e. builtin "fg" exiting successfully.
            (queue-job-display-summary job))))
      (write-builtin-error "fg" (or arg "\"\"") "no such job")))) ; returns (failed 1)


;; the "global" builtin: run the builtin passed as first argument
;; with its parent job temporarily changed to (sh-globals)
;; Useful mostly for builtins "cd", "pwd" and "set"
;;
;; As all builtins do, must return job status.
(define (builtin-global job prog-and-args options)
  ;; (debugf "builtin-global ~s" prog-and-args)
  (assert-string-list? 'builtin-global prog-and-args)
  (if (null? (cdr prog-and-args))
    (void)
    (let* ((args    (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (if builtin
        (begin
          (job-temp-parent-set! job (sh-globals))
          (builtin-start builtin job args options))
        (write-builtin-error "global" "not a shell builtin" (car args))))))


;; the "jobs" builtin: list currently running jobs
;;
;; As all builtins do, must return job status.
(define (builtin-jobs job prog-and-args options)
  (assert-string-list? 'builtin-jobs prog-and-args)
  (let ((src (multijob-children (sh-globals))))
    (unless (span-empty? src)
      (let ((port (current-output-port)))
        (span-iterate src
          (lambda (job-id job)
            (when (sh-job? job)
              (sh-job-display-summary job port))))
        (flush-output-port port))))
  (void))


;; the "parent" builtin: run the builtin passed as first argument
;; with its parent job temporarily changed to current parent's parent.
;; Useful mostly for builtins "cd", "pwd" and "set"
;;
;; As all builtins do, must return job status.
(define (builtin-parent job prog-and-args options)
  ;; (debugf "builtin-parent ~s" prog-and-args)
  (assert-string-list? 'builtin-parent prog-and-args)
  (if (null? (cdr prog-and-args))
    (void)
    (let* ((args       (cdr prog-and-args))
           (builtin    (sh-find-builtin args))
           (old-parent (job-parent job))
           (new-parent (or (and old-parent (job-parent old-parent)) #t)))
      (builtin
        (begin
          (job-temp-parent-up! job)
          (builtin-start builtin job args options))
        (write-builtin-error "parent" "not a shell builtin" (car args))))))


;; set the temp-parent of job to job's grand-parent.
(define (job-temp-parent-up! job)
  (let* ((parent      (job-parent job))
         (grandparent (and parent (job-parent parent))))
    (when grandparent
      (job-temp-parent-set! job grandparent))))


;; display a single environment variable
(define (%env-display-var name val wbuf)
  (bytespan-insert-right/bytevector! wbuf #vu8(115 101 116 32)) ; "set "
  (bytespan-insert-right/string!  wbuf name)
  (bytespan-insert-right/bytevector! wbuf #vu8(32 39)) ; " '"
  (bytespan-insert-right/string!  wbuf val)
  (bytespan-insert-right/bytevector! wbuf #vu8(39 10))) ; "'\n"


;; display all environment variables of specified job - either all or only exported ones.
;; returns (void)
(define (%env-display-vars job which)
  (let ((wbuf (bytespan))
        (fd   (sh-fd 1))
        (vec  (hashtable-cells (sh-env-copy job which))))
    (unless (fxzero? (vector-length vec))
      (subvector-sort! (lambda (e1 e2) (string<? (car e1) (car e2))) vec)
      (bytespan-reserve-right! wbuf (fxmin 4096 (fx* 32 (vector-length vec))))
      (vector-iterate vec
        (lambda (i elem)
          (%env-display-var (car elem) (cdr elem) wbuf)
          (when (fx>=? (bytespan-length wbuf) 4096)
            (fd-write/bytespan! fd wbuf))))
      (when (eq? 'export which)
        (bytespan-insert-right/bytevector! wbuf #vu8(101 120 112 111 114 116)) ; "export"
        (vector-iterate vec
          (lambda (i elem)
            (bytespan-insert-right/u8! wbuf 32)   ; " "
            (bytespan-insert-right/string! wbuf (car elem))
            (when (fx>=? (bytespan-length wbuf) 4096)
              (fd-write/bytespan! fd wbuf))))
        (bytespan-insert-right/u8! wbuf 10))       ; "\n"
      (fd-write/bytespan! fd wbuf)))
  (void))


;; the "set" builtin: show environment variable(s),
;; or set a single environment variables of parent job
;;
;; As all builtins do, must return job status.
(define (builtin-set job prog-and-args options)
  (assert-string-list? 'builtin-set prog-and-args)
  (let ((parent (job-parent job)))
    (cond
      ((null? (cdr prog-and-args))
        (%env-display-vars parent 'all))
      ((null? (cddr prog-and-args))
        (let* ((name (cadr prog-and-args))
               (val  (sh-env-ref parent name #f)))
          (if val
            (let ((wbuf (bytespan)))
              (%env-display-var name val wbuf)
              (fd-write/bytespan! (sh-fd 1) wbuf)
              (void))          ; exit successfully
            (failed 1)))) ; env variable not found => fail
      ((null? (cdddr prog-and-args))
        (let ((name (cadr prog-and-args))
              (val  (caddr prog-and-args)))
          (sh-env-set! parent name val)
          (void))) ; exit successfully
      (else
        (write-builtin-error "set" "too many arguments"))))) ; returns (failed 1)



;; the "split-at-0" builtin: split second and subsequent strings of a command line at each #\nul
;; then run whatever command, builtin or alias is at the first string of the command line.
;;
;; As all builtins do, must return job status.
(define (builtin-split-at-0 job prog-and-args options)
  (assert-string-list? 'builtin-split-at-0 prog-and-args)
  (if (null? (cdr prog-and-args))
    (write-builtin-error "split-at-0" "too few arguments")
    (let ((args (cons (cadr prog-and-args) ;; copy first argument as-is
                       ;; split after each #\nul the second and subsequent arguments
                      (string-list-split-after-nuls (cddr prog-and-args)))))
      (start-command-or-builtin-or-alias-from-another-builtin job args options))))


;; the "unexport" builtin: unexport zero or more environment variables of parent job
;;
;; As all builtins do, must return job status.
(define (builtin-unexport job prog-and-args options)
  (assert-string-list? 'builtin-unexport prog-and-args)
  (let ((parent (job-parent job)))
    (for-list ((name (cdr prog-and-args)))
      (sh-env-visibility-set! parent name 'private)))
  (void))


;; the "unset" builtin: delete zero or more environment variables of parent job
;;
;; As all builtins do, must return job status.
(define (builtin-unset job prog-and-args options)
  (assert-string-list? 'builtin-unset prog-and-args)
  (let ((parent (job-parent job)))
    (for-list ((name (cdr prog-and-args)))
      (sh-env-delete! parent name))
    (void))) ; exit successfully


;; The "wait" builtin: continue a job-id by sending SIGCONT to it, then wait for it to exit.
;; Does NOT return if job stops.
;;
;; As all builtins do, must return job status. For possible returned statuses, see (sh-fg)
(define (builtin-wait job prog-and-args options)
  (assert-string-list? 'builtin-wait prog-and-args)
  (let-values (((job arg) (prog-and-args->job prog-and-args)))
      (if job
        (let ((out (current-output-port)))
          (when (and job (not arg))
            ;; show the preferred job being resumed
            (sh-job-display-summary job (running (job-id job)) out)
            (flush-output-port out))
          (let ((new-status (sh-wait job)))
            (if (finished? new-status)
              ;; job finished, return its exit status as "wait" exit status.
              new-status
              ;; job still exists, show its running/stopped status.
              ;; return (void) i.e. builtin "wait" exiting successfully.
              (queue-job-display-summary job))))
        (write-builtin-error "wait" (or arg "\"\"") "no such job")))) ; returns (failed 1)


;; start a builtin and return its status.
;; performs sanity checks on exit status returned by the call (builtin job args options)
;;
;; if options plist contain 'spawn? #t, then the builtin will be started asynchronously
;;   in a subprocess, thus the returned status can be (running ...)
;; if builtin is builtin-command, by design it spawns asynchronously
;;   an external subprocess and returns immediately,
;;   thus the returned status can be (running ...)
;; otherwise the builtin will be executed synchronously in the caller's process
;;   and the returned status can only be one of (void) (ok ...) (failed ...) (killed ...) or (exception ...)
(define (builtin-start builtin c args options)
  (assert* 'builtin-start (not (job-step-proc c)))
  (if (job-fds-to-remap c)
    ;; fd remapping already performed, proceed
    (%builtin-start-already-redirected builtin c args options)
    ;; perform fd remapping, then start the builtin
    (begin
      (job-remap-fds! c)
        (%builtin-start-already-redirected builtin c args options))))


;; filled at the end of job.ss
(define builtins-that-finish-immediately
  (let ((ht (make-eq-hashtable)))
    (lambda () ht)))


;; internal function called by (builtin-start) to execute a builtin.
;; returns job status.
(define (%builtin-start-already-redirected builtin job args options)
  (call-or-spawn-job-procedure job options
    (lambda (job options)
      ;; execute the builtin
      ;c (debugf "builtin-start options=~s args=~s job=~s" options args job)
      (job-status-set! 'builtin-start job
        (let ((status  (builtin job args options)))
          ;c (debugf "< builtin-start options=~s args=~s job=~s status=~s" options args job status)
          (if (or (finished? status) (options->spawn? options)
                  (not (hashtable-ref (builtins-that-finish-immediately) builtin #f)))
            status
            (%warn-bad-builtin-exit-status builtin args status))))))) ; returns (void)



;; always returns (void). Useful for (builtin-start)
(define (%warn-bad-builtin-exit-status builtin args status)
  (format (console-error-port)
    "; warning: invalid exit status ~s of builtin ~s called with arguments ~s\n"
    status builtin args)
  (void))
