;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; convert Scheme obj to exit status string:
;; if obj is truish. return "0", otherwise return "1"
;;
;; useful with builtin "test", as for example:
;;   test (sh-bool (and (some-scheme-expression) (another-scheme-expression))) || echo failed

;; or, to evaluate scheme expressions when builtin "test" is actually executed,
;;   test (lambda () (sh-bool (and (some-scheme-expression) (another-scheme-expression)))) || echo failed
;; which can be abbreviated to
;;   (shell-test (and (some-scheme-expression) (another-scheme-expression))) || echo failed
(define (sh-bool obj)
  (if obj "0" "1"))


;; write contents of bytespan wbuf to file descriptor fd,
;; then clear bytespan wbuf
;;
;; returns (void)
(define (fd-write/bspan! fd wbuf)
  ; TODO: loop on short writes and call sh-consume-sigchld
  (fd-write fd (bytespan-peek-data wbuf)
            (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf))
  (bytespan-clear! wbuf))


;; write warning or error message to file descriptor fd.
;;
;; returns (void)
(define (fd-write-strings: fd prefix strings)
  (let ((wbuf (bytespan)))
    (bytespan-insert-back/string! wbuf prefix)
    (list-iterate strings
      (lambda (arg)
        (bytespan-insert-back/u8! wbuf 58 32) ; ": "
        (bytespan-insert-back/string! wbuf arg)
        (when (fx>=? (bytespan-length wbuf) 4096)
          (fd-write/bspan! fd wbuf))))
    (bytespan-insert-back/u8! wbuf 10)
    (fd-write/bspan! fd wbuf)))


;; print warning message to (sh-fd-stderr)
;; always returns (void)
(define (write-builtin-warning . args)
  (fd-write-strings: (sh-fd-stderr) "; warning" args)
  (void))


;; print error message to (sh-fd-stderr)
;; always returns '(exited . 1)
(define (write-builtin-error . args)
  (fd-write-strings: (sh-fd-stderr) "schemesh" args)
  '(exited . 1))


;; the "builtin" builtin: run the builtin in the remaining command line.
;;
;; As all builtins do, must return job status.
;; returns '(exited . 1) if specified builtin is not found.
(define (builtin-builtin job prog-and-args options)
  ; (debugf "builtin-builtin ~s" prog-and-args)
  (assert-string-list? 'builtin-builtin prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void)
    (let* ((args (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (if builtin
        (start-builtin builtin job args options)
        (write-builtin-error "builtin" "not a shell builtin" (car args))))))


;; the "command" builtin: spawn a subprocess and return immediately
;;
;; As all builtins do, must return job status.
(define (builtin-command job prog-and-args options)
  (assert-string-list? 'builtin-command prog-and-args)
  (assert* 'builtin-command (string=? "command" (car prog-and-args)))
  (spawn-cmd job (cdr prog-and-args) options)) ; returns job status


;; the "exec" builtin: replace the current process with specified command.
;; Returns only if command cannot be executed.
;;
;; As all builtins do, must return job status.
(define (builtin-exec job prog-and-args options)
  (assert-string-list? 'builtin-exec prog-and-args)
  (assert* 'builtin-exec (string=? "exec" (car prog-and-args)))
  ;; save history before this process is replaced by exec'd command
  (let ((lctx (sh-repl-args-linectx)))
    (when (linectx? lctx)
      (linectx-save-history lctx)))
  ; on success, does not return: this process does not exist anymore.
  ; on failure, returns job status
  (exec-cmd job (list->argv (cdr prog-and-args)) options))


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
        (list-iterate (cdr prog-and-args)
          (lambda (name)
            (sh-env-visibility-set! parent name 'export)))
        (void)))))


;; The "bg" builtin: continue a job-id by sending SIGCONT to it, and return immediately
;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;;
;; As all builtins do, must return job status. For possible returned statuses, see (sh-bg)
(define (builtin-bg job prog-and-args options)
  (assert-string-list? 'builtin-bg prog-and-args)
  ;; TODO: implement (builtin-bg) with no args
  (let* ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args)))
         (job (and (string-contains-only-decimal-digits? arg)
                   (sh-find-job (string->number arg)))))
      (if job
        (let* ((old-status (job-last-status job))
               (new-status (sh-bg job)))
          (if (job-status-finished? new-status)
            ; job finished, return its exit status as "bg" exit status.
            new-status
            ; job still exists, show its running/stopped status.
            ; return (void) i.e. builtin "fg" exiting successfully.
            (queue-job-display-summary job)))
        (write-builtin-error "bg" arg "no such job")))) ; returns '(exited . 1)


;; The "fg" builtin: continue a job-id by sending SIGCONT to it, then wait for it to exit or stop.
;;
;; As all builtins do, must return job status. For possible returned statuses, see (sh-fg)
(define (builtin-fg job prog-and-args options)
  (assert-string-list? 'builtin-fg prog-and-args)
  ;; TODO: implement (builtin-fg) with no args
  (let* ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args)))
         (job (and (string-contains-only-decimal-digits? arg)
                   (sh-find-job (string->number arg)))))
      (if job
        (let* ((old-status (job-last-status job))
               (new-status (sh-fg job)))
          (if (job-status-finished? new-status)
            ; job finished, return its exit status as "fg" exit status.
            new-status
            ; job still exists, show its running/stopped status.
            ; return (void) i.e. builtin "fg" exiting successfully.
            (queue-job-display-summary job)))
        (write-builtin-error "fg" arg "no such job")))) ; returns '(exited . 1)


;; the "global" builtin: run the builtin passed as first argument
;; with its parent job temporarily changed to (sh-globals)
;; Useful mostly for builtins "cd", "pwd" and "set"
;;
;; As all builtins do, must return job status.
(define (builtin-global job prog-and-args options)
  ; (debugf "builtin-global ~s" prog-and-args)
  (assert-string-list? 'builtin-global prog-and-args)
  (if (null? (cdr prog-and-args))
    (void)
    (let* ((args    (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (if builtin
        (start-builtin builtin job args
            (cons '(parent-job . #t) options)) ; options will be processed again
        (write-builtin-error "global" "not a shell builtin" (car args))))))


;; the "jobs" builtin: list currently running jobs
;;
;; As all builtins do, must return job status.
(define (builtin-jobs job prog-and-args options)
  (assert-string-list? 'builtin-jobs prog-and-args)
  (let ((src (multijob-children (sh-globals))))
    (unless (span-empty? src)
      ;; do NOT close port, it would close the fd!
      (let ((port (open-fd-output-port (sh-fd-stdout) (buffer-mode block) transcoder-utf8)))
        (span-iterate src
          (lambda (job-id job)
            (when (sh-job? job)
              (sh-job-display-summary* job port))))
        (flush-output-port port))))
  (void))


;; the "parent" builtin: run the builtin passed as first argument
;; with its parent job temporarily changed to current parent's parent.
;; Useful mostly for builtins "cd", "pwd" and "set"
;;
;; As all builtins do, must return job status.
(define (builtin-parent job prog-and-args options)
  ; (debugf "builtin-parent ~s" prog-and-args)
  (assert-string-list? 'builtin-parent prog-and-args)
  (if (null? (cdr prog-and-args))
    (void)
    (let* ((args       (cdr prog-and-args))
           (builtin    (sh-find-builtin args))
           (old-parent (job-parent job))
           (new-parent (or (and old-parent (job-parent old-parent)) #t)))
      (if builtin
        (start-builtin builtin job args
            (cons (cons 'parent-job new-parent) options)) ; options will be processed again
        (write-builtin-error "parent" "not a shell builtin" (car args))))))


;; display a single environment variable
(define (%env-display-var name val wbuf)
  (bytespan-insert-back/bvector! wbuf #vu8(115 101 116 32)) ; "set "
  (bytespan-insert-back/string!  wbuf name)
  (bytespan-insert-back/bvector! wbuf #vu8(32 39)) ; " '"
  (bytespan-insert-back/string!  wbuf val)
  (bytespan-insert-back/bvector! wbuf #vu8(39 10))) ; "'\n"


;; display all environment variables of specified job - either all or only exported ones.
;; returns (void)
(define (%env-display-vars job which)
  (let ((wbuf (bytespan))
        (fd   (sh-fd-stdout))
        (vec  (hashtable-cells (sh-env-copy job which))))
    (unless (fxzero? (vector-length vec))
      (vector-sort*! (lambda (e1 e2) (string<? (car e1) (car e2))) vec)
      (bytespan-reserve-back! wbuf (fxmin 4096 (fx* 32 (vector-length vec))))
      (vector-iterate vec
        (lambda (i elem)
          (%env-display-var (car elem) (cdr elem) wbuf)
          (when (fx>=? (bytespan-length wbuf) 4096)
            (fd-write/bspan! fd wbuf))))
      (when (eq? 'export which)
        (bytespan-insert-back/bvector! wbuf #vu8(101 120 112 111 114 116)) ; "export"
        (vector-iterate vec
          (lambda (i elem)
            (bytespan-insert-back/u8! wbuf 32)   ; " "
            (bytespan-insert-back/string! wbuf (car elem))
            (when (fx>=? (bytespan-length wbuf) 4096)
              (fd-write/bspan! fd wbuf))))
        (bytespan-insert-back/u8! wbuf 10))       ; "\n"
      (fd-write/bspan! fd wbuf)))
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
              (fd-write/bspan! (sh-fd-stdout) wbuf)
              (void))          ; exit successfully
            '(exited . 1)))) ; env variable not found => fail
      ((null? (cdddr prog-and-args))
        (let ((name (cadr prog-and-args))
              (val  (caddr prog-and-args)))
          (sh-env-set! parent name val)
          (void))) ; exit successfully
      (#t
        (write-builtin-error "set" "too many arguments"))))) ; returns '(exited . 1)



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
    (list-iterate (cdr prog-and-args)
      (lambda (name)
        (sh-env-visibility-set! parent name 'private))))
  (void))


;; the "unsafe" builtin: run whatever command, builtin or alias is in the remaining command line.
;;
;; As all builtins do, must return job status.
(define (builtin-unsafe job prog-and-args options)
  (assert-string-list? 'builtin-unsafe prog-and-args)
  (if (null? (cdr prog-and-args))
    (void)
    (start-command-or-builtin-or-alias-from-another-builtin job (cdr prog-and-args) options)))


;; the "unset" builtin: delete zero or more environment variables of parent job
;;
;; As all builtins do, must return job status.
(define (builtin-unset job prog-and-args options)
  (assert-string-list? 'builtin-unset prog-and-args)
  (let ((parent (job-parent job)))
    (list-iterate (cdr prog-and-args)
      (lambda (name)
        (sh-env-delete! parent name)))
    (void))) ; exit successfully


;; start a builtin and return its status.
;; performs sanity checks on exit status returned by the call (builtin job args options)
;;
;; if options list contain '(spawn? . #t), then the builtin will be started asynchronously
;;   in a subprocess, thus the returned status can be '(running ...)
;; if builtin is builtin-command, by design it spawns asynchronously
;;   an external subprocess and returns immediately,
;;   thus the returned status can be '(running ...)
;; otherwise the builtin will be executed synchronously in the caller's process
;;   and the returned status can only be one of (void) '(exited ...) '(killed ...) or '(unknown ...)
(define (start-builtin builtin c args options)
  (assert* 'start-builtin (not (job-step-proc c)))
  (if (job-fds-to-remap c)
    ;; fd remapping already performed, proceed
    (%start-builtin-already-redirected builtin c args options)
    ;; perform fd remapping, then start the builtin
    (begin
      (job-remap-fds! c)
      (parameterize ((sh-fd-stdin  (job-find-fd-remap c 0))
                     (sh-fd-stdout (job-find-fd-remap c 1))
                     (sh-fd-stderr (job-find-fd-remap c 2)))
        (%start-builtin-already-redirected builtin c args options)))))


;; filled at the end of job.ss
(define builtins-that-finish-immediately
  (let ((ht (make-eq-hashtable)))
    (lambda () ht)))


;; internal function called by (start-builtin) to execute a builtin.
;; returns job status.
(define (%start-builtin-already-redirected builtin job args options)
  (call-or-spawn-procedure job options
    (lambda (job options)
      ;; execute the builtin
      ;c (debugf "start-builtin options=~s args=~s job=~a" options args (sh-job->string job))
      (job-status-set! 'start-builtin job
        (let ((status  (builtin job args options)))
          ;c (debugf "< start-builtin options=~s args=~s job=~a status=~s" options args (sh-job->string job) status)
          (if (or (job-status-finished? status) (options->spawn? options)
                  (not (hashtable-ref (builtins-that-finish-immediately) builtin #f)))
            status
            (%warn-bad-builtin-exit-status builtin args status))))))) ; returns (void)



;; always returns (void). Useful for (start-builtin)
(define (%warn-bad-builtin-exit-status builtin args status)
  (format (console-error-port)
    "; warning: invalid exit status ~s of builtin ~s called with arguments ~s\n"
    status builtin args)
  (void))
