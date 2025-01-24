;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only from file ../job.ss



;; write contents of bytespan bsp to (sh-fd-stderr)
;; then clear bytespan bsp
(define (fd-stderr-write/bspan! bsp)
  ; TODO: loop on short writes and call sh-consume-signals
  (fd-write (sh-fd-stderr) (bytespan-peek-data bsp)
            (bytespan-peek-beg bsp) (bytespan-peek-end bsp))
  (bytespan-clear! bsp))


;; the "command" builtin
(define (builtin-command job prog-and-args options)
  (assert-string-list? 'sh-builtin-command prog-and-args)
  (assert* 'sh-builtin-command (string=? "command" (car prog-and-args)))
  (cmd-spawn job (list->argv (cdr prog-and-args)) options)
  (job-last-status job))


;; the "exec" builtin
(define (builtin-exec job prog-and-args options)
  (assert-string-list? 'sh-builtin-exec prog-and-args)
  (assert* 'sh-builtin-exec (string=? "exec" (car prog-and-args)))
  (cmd-exec job (list->argv (cdr prog-and-args)) options) ; returns only on error
  (job-last-status job))


;; The "bg" builtin: continue a job-id by sending SIGCONT to it, and return immediately
;; Continue a job or job-id in background by sending SIGCONT to it, and return immediately.
;; Return job status. For possible returned statuses, see (sh-bg)
;;
(define (builtin-bg job prog-and-args options)
  (assert-string-list? 'sh-builtin-bg prog-and-args)
  ;; TODO: implement (builtin-bg) with no args
  (let ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args))))
    (if (string-contains-only-decimal-digits? arg)
      (let* ((job (sh-job (string->number arg)))
             (other-status (sh-bg job)))
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
  (assert-string-list? 'sh-builtin-fg prog-and-args)
  ;; TODO: implement (builtin-fg) with no args
  (let ((arg (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
                "\"\""
                (cadr prog-and-args))))
    (if (string-contains-only-decimal-digits? arg)
      (let* ((job (sh-job (string->number arg)))
             (other-status (sh-fg job)))
        (if (job-status-finished? other-status)
          other-status
          ; job still exists, show its running/stopped status.
          ; returns (void) i.e. builtin "fg" exiting successfully.
          (sh-job-display/summary job)))
      (write-builtin-error "fg" arg "no such job")))) ; returns '(exited . 1)


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
  (assert-string-list? 'sh-builtin-jobs prog-and-args)
  (let ((src (multijob-children (sh-globals))))
    (unless (span-empty? src)
      ;; do NOT close port, it would close the fd!
      (let ((port (open-fd-output-port (sh-fd-stdout) (buffer-mode line) transcoder-utf8)))
        (span-iterate src
          (lambda (job-id job)
            (when (sh-job? job)
              (sh-job-display/summary* job port)))))))
  (void))
