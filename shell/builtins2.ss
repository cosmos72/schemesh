;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file shell/job.ss



;; write contents of bytespan bsp to (sh-fd-stderr)
;; then clear bytespan bsp
(define (fd-stderr-write/bspan! bsp)
  ; TODO: loop on short writes and call sh-consume-signals
  (fd-write (sh-fd-stderr) (bytespan-peek-data bsp)
            (bytespan-peek-beg bsp) (bytespan-peek-end bsp))
  (bytespan-clear! bsp))


;; the "command" builtin. spawns a subprocess and returns immediately
(define (builtin-command job prog-and-args options)
  (assert-string-list? 'builtin-command prog-and-args)
  (assert* 'builtin-command (string=? "command" (car prog-and-args)))
  (cmd-spawn job (list->argv (cdr prog-and-args)) options)
  (job-last-status job))


;; the "exec" builtin
(define (builtin-exec job prog-and-args options)
  (assert-string-list? 'builtin-exec prog-and-args)
  (assert* 'builtin-exec (string=? "exec" (car prog-and-args)))
  (cmd-exec job (list->argv (cdr prog-and-args)) options) ; returns only on error
  (job-last-status job))


;; The "bg" builtin: continue a job-id by sending SIGCONT to it, and return immediately
;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible returned statuses, see (sh-bg)
;;
(define (builtin-bg job prog-and-args options)
  (assert-string-list? 'builtin-bg prog-and-args)
  ;; TODO: implement (builtin-bg) with no args
  (let* ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args)))
         (job (and (string-contains-only-decimal-digits? arg)
                   (sh-find-job (string->number arg)))))
      (if job
        (let ((other-status (sh-bg job)))
          (if (job-status-finished? other-status)
            other-status
            ; job still exists, show its running/stopped status.
            ; returns (void) i.e. builtin "bg" exiting successfully.
            (sh-job-display/summary job)))
        (write-builtin-error "bg" arg "no such job")))) ; returns '(exited . 1)


;; The "fg" builtin: continue a job-id by sending SIGCONT to it, then wait for it to exit or stop,
;; and finally return its status. For possible returned statuses, see (sh-fg)
;;
(define (builtin-fg job prog-and-args options)
  (assert-string-list? 'builtin-fg prog-and-args)
  ;; TODO: implement (builtin-fg) with no args
  (let* ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args)))
         (job (and (string-contains-only-decimal-digits? arg)
                   (sh-find-job (string->number arg)))))
      (if job
        (let ((other-status (sh-fg job)))
          (if (job-status-finished? other-status)
            other-status
            ; job still exists, show its running/stopped status.
            ; returns (void) i.e. builtin "fg" exiting successfully.
            (sh-job-display/summary job)))
        (write-builtin-error "fg" arg "no such job")))) ; returns '(exited . 1)


;; print error message to (fd-stderr)
;; always returns '(exited . 1)
(define (write-builtin-error . args)
  (let ((msg (bytespan)))
    (bytespan-insert-back/string! msg "schemesh")
    (list-iterate args
      (lambda (arg)
        (bytespan-insert-back/u8! msg 58 32) ; ": "
        (bytespan-insert-back/string! msg arg)))
    (bytespan-insert-back/u8! msg 10)
    (fd-stderr-write/bspan! msg)
    '(exited . 1)))



;; the "jobs" builtin: list currently running jobs
(define (builtin-jobs job prog-and-args options)
  (assert-string-list? 'builtin-jobs prog-and-args)
  (let ((src (multijob-children (sh-globals))))
    (unless (span-empty? src)
      ;; do NOT close port, it would close the fd!
      (let ((port (open-fd-output-port (sh-fd-stdout) (buffer-mode line) transcoder-utf8)))
        (span-iterate src
          (lambda (job-id job)
            (when (sh-job? job)
              (sh-job-display/summary* job port)))))))
  (void))



;; the "builtin" builtin: find a builtin by name, and execute it.
;; returns builtin exit status, or '(exited . 1) if specified builtin is not found.
(define (builtin-builtin job prog-and-args options)
  ; (debugf "builtin-builtin ~s" prog-and-args)
  (assert-string-list? 'builtin-builtin prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void)
    (let* ((args (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (if builtin
        (builtin/start builtin job args options)
        (write-builtin-error "builtin" "not a shell builtin" (car args))))))


;; start a builtin and return its status.
;; performs sanity checks on exit status returned by the call (builtin job args options)
;;
;; if options list contain 'spawn, then the builtin will be started asynchronously
;;   in a subprocess, thus the returned status can be '(running ...)
;; if builtin is builtin-command, by design it spawns asynchronously
;;   an external subprocess and returns immediately,
;;   thus the returned status can be '(running ...)
;; otherwise the builtin will be executed synchronously in the caller's process
;;   and the returned status can only be one of (void) '(exited ...) '(killed ...) or '(unknown ...)
(define (builtin/start builtin job args options)
  (assert* 'builtin/start (not (job-step-proc job)))
  (let ((%proc-builtin/start
    (lambda (job)
      (let ((status (builtin job args options)))
        ; executing a builtin finishes immediately, and returns a (job-status-finished? status)
        ; with two exceptions:
        ; 1. if the builtin is started with 'spawn option, it is executed asynchronously in a spawned subprocess
        ; 2. the builtin "command" by design spawns asynchronously an external subprocess and returns immediately
        (if (or (memq 'spawn options) (eq? builtin builtin-command) (job-status-finished? status))
          status
          (%warn-bad-builtin-exit-status builtin args status)))))) ; returns (void)
    (if (memq 'spawn options)
      (job-start/spawn-proc job %proc-builtin/start options)
      (%proc-builtin/start job))))



;; always returns (void). Useful for (builtin/start)
(define (%warn-bad-builtin-exit-status builtin args status)
  (format (current-error-port)
    "; warning: invalid exit status ~s of builtin ~s called with arguments ~s\n"
    status builtin args)
  (void))
