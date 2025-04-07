;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; analogous to Chez Scheme (make-parameter), with two differences - see below
;;
;; full behavior:
;;   (make-stateful-parameter current-value) creates and returns a global parameter,
;;   i.e. a closure that captures a reference to current-value and behaves as follows:
;;     (parameter)           returns current-value
;;     (parameter new-value) sets current-value to new-value and returns it.
;;                           further calls to (parameter) will return updated current value.
;;
;;   (make-stateful-parameter current-value changer) creates and returns a global parameter,
;;   i.e. a closure that captures a reference to current-value and behaves as follows:
;;     (parameter)           returns current value
;;     (parameter new-value) calls (changer current-value new-value),
;;                           then sets current-value to the value returned by (changer ...)
;;                           and finally returns updated current-value.
;;                           further calls to (parameter) will return updated current value.
;;
;; differences from Chez Scheme (make-parameter):
;; 1. changer-proc receives both current value and the new value to set,
;; 2. calling (parameter new-value) returns the value actually set.
(define make-stateful-parameter
  (case-lambda
    ((initial-value changer-proc)
      (let ((value (changer-proc initial-value initial-value)))
        (case-lambda
          (()          value)
          ((new-value) (set! value (changer-proc value new-value)) value))))
    ((initial-value)
      (make-stateful-parameter initial-value (lambda (old-value new-value) new-value)))))



(define (job-control-available-change! old-flag new-flag)
  (cond
    ((not (boolean? new-flag))
      (format (console-error-port)
        "; error: ~s is not a boolean, refusing to set sh-job-control-available? to such value\n" new-flag)
      old-flag)
    ((eq? old-flag new-flag)
      old-flag)
    (new-flag
      (put-string (console-error-port)
        "; error: parameter sh-job-control-available? is #f, cannot be changed anymore\n")
      old-flag)
    (else
      ; disabling (sh-job-control-available?) -> also disable (sh-job-control?)
      (sh-job-control? #f)
      new-flag)))


;; Global parameter indicating whether job control can be activated.
;;
;; if (sh-job-control-available?) returns #t, job control can be activated.
;; if (sh-job-control-available?) returns #f, job control cannot be activated.
;;
;; Calling (sh-job-control-available? new-value) returns the value actually set.
;; Once set to #f, cannot be changed anymore.
(define sh-job-control-available?
  (make-stateful-parameter
    (eqv? 1 ((foreign-procedure "c_job_control_available" () int)))
    job-control-available-change!))


(define job-control-change!
  (let ((c-job-control-change (foreign-procedure "c_job_control_change" (int) int)))
    (lambda (old-flag new-flag)
      ; (debugf "job-control-change! ~s -> ~s" old-flag new-flag)
      (cond
        ((eq? old-flag new-flag)
          old-flag)
        ((not (boolean? new-flag))
          (format (console-error-port)
            "; schemesh (pid ~s) error: ~s is not a boolean, refusing to set sh-job-control? to such value\n"
              (pid-get) new-flag)
          old-flag)
        ((and new-flag (sh-job-control-available?))
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
                (format (console-error-port) "; schemesh (pid ~s) error: failed activating job control: C function c_job_control_change(1) failed with error ~s: ~a\n"
                   (pid-get) err (c-errno->string err))
                old-flag))))
        (new-flag
          (format (console-error-port) "; schemesh (pid ~s) error: cannot activate job control, parameter sh-job-control-available? is #f\n"
            (pid-get))
          old-flag)
        (old-flag
          ;; try to deactivate job control
          (let ((err (c-job-control-change 0)))
            (when (< err 0)
              (format (console-error-port) "; schemesh (pid ~s) warning: failed deactivating job control: C function c_job_control_change(0) failed with error ~s: ~a\n"
                   (pid-get) err (c-errno->string err))))
          ;; set job-control to inactive even if c_job_control_change() failed
          new-flag)
        (else ; should not happen
          old-flag)))))


;; Global parameter indicating whether job control is active.
;;
;; if (sh-job-control?) returns #t, job control is active:
;;   a. new process groups will be created as needed, and sub-processes may be moved into them.
;;   b. changing the foreground process group is allowed.
;;
;; if (sh-job-control?) returns #f, job control is inactive:
;;   a. sub-processes will inherit the parent's process group.
;;   b. changing the foreground process group is not allowed.
;;
;; Usually set to #t while (repl) is running, and to #f when executing scripts or in subshells.
;;
;; Job control can be activated i.e. set to #t only if (sh-job-control-available?) returns #t
;;
;; Activating job control requires taking control of the terminal foreground/background mechanism,
;; and thus for sanity will suspend this process if it's running in the background.
;;
(define sh-job-control? (make-stateful-parameter #f job-control-change!))
