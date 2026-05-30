;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; more complete implementation for enabling/disabling job control.
;; installed into parameter (tty-job-control-proc)
(define sh-job-control-set!
  (let ((c-job-control-change (foreign-procedure "c_job_control_set" (int) int)))
    (lambda (old-flag new-flag)
      ;; (debugf "sh-job-control-set! ~s -> ~s" old-flag new-flag)
      (cond
        ((eq? old-flag new-flag)
          old-flag)
        ((not (boolean? new-flag))
          (format (console-error-port)
            "; schemesh (pid ~s) error: ~s is not a boolean, refusing to set tty-job-control? to such value\n"
              (pid-get) new-flag)
          old-flag)
        ((and new-flag (tty-job-control-available?))
          ;; try to activate job control
          (let ((pgid-or-error (c-job-control-change 1)))
            (if (>= pgid-or-error 0)
              (let ((pgid pgid-or-error))
                ;; install Scheme procedures invoked when process receives SIGTSTP or SIGQUIT
                (install-signal-handlers)
                ;; our process group id may have changed, reload it
                (job-pgid-set! (sh-globals) (if (zero? pgid) (pgid-get 0) pgid))
                new-flag)
              (let ((err pgid-or-error))
                (format (console-error-port) "; schemesh (pid ~s) error: failed activating job control: C function c_job_control_set(1) failed with error ~s: ~a\n"
                   (pid-get) err (c-errno->string err))
                old-flag))))
        (new-flag
          (format (console-error-port) "; schemesh (pid ~s) error: cannot activate job control, parameter tty-job-control-available? is #f\n"
            (pid-get))
          old-flag)
        (old-flag
          ;; try to deactivate job control
          (let ((err (c-job-control-change 0)))
            (when (< err 0)
              (format (console-error-port) "; schemesh (pid ~s) warning: failed deactivating job control: C function c_job_control_set(0) failed with error ~s: ~a\n"
                   (pid-get) err (c-errno->string err))))
          ;; set job-control to inactive even if c_job_control_set() failed
          new-flag)
        (else ; should not happen
          old-flag)))))
