;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix tty (1 0 0))
  (export tty-job-control? tty-job-control-available? tty-job-control-proc tty-setraw! tty-restore!
          tty-inspect tty-size with-cooked-tty with-raw-tty)
  (import
    (rnrs)
    (only (chezscheme)         foreign-procedure inspect logbit? procedure-arity-mask)
    (only (scheme2k bootstrap) assert* raise-errorf))


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


(define (job-control-available-set! old-flag new-flag)
  (assert* 'tty-job-control-available? (boolean? new-flag))
  (cond
    ((eq? old-flag new-flag)
      old-flag)
    (new-flag
      ;; (put-string (console-error-port)
      ;;   "; error: parameter tty-job-control-available? is #f, cannot be changed anymore\n")
      old-flag)
    (else
      ; disabling (tty-job-control-available?) -> also disable (tty-job-control?)
      (tty-job-control? #f)
      new-flag)))


;; Global parameter indicating whether job control can be activated.
;;
;; if (tty-job-control-available?) returns #t, job control can be activated.
;; if (tty-job-control-available?) returns #f, job control cannot be activated.
;;
;; Calling (tty-job-control-available? new-value) returns the value actually set.
;; Once set to #f, cannot be changed anymore.
(define tty-job-control-available?
  (make-stateful-parameter
    (eqv? 1 ((foreign-procedure "c_job_control_available" () int)))
    job-control-available-set!))


;; minimal procedure for enabling/disabling job control.
;; for a more complete implementation, see (job-control-proc) in shell/params.ss
(define minimal-job-control-set!
  (let ((c-job-control-proc (foreign-procedure "c_job_control_set" (int) int)))
    (lambda (old-flag new-flag)
      ;; (debugf "job-control-proc ~s -> ~s" old-flag new-flag)
      (assert* 'tty-job-control? (boolean? old-flag))
      (assert* 'tty-job-control? (boolean? new-flag))
      (cond
        ((eq? old-flag new-flag)
          old-flag)
        ((and new-flag (tty-job-control-available?))
          ;; try to activate job control
          (let ((pgid-or-error (c-job-control-proc 1)))
            (if (>= pgid-or-error 0)
              (let ((pgid pgid-or-error))
                ;; should install Scheme procedures invoked when process receives SIGTSTP or SIGQUIT...
                ;; (install-signal-handlers)
                ;; our process group id may have changed, we should reload it
                ;; (job-pgid-set! (sh-globals) (if (zero? pgid) (pgid-get 0) pgid))
                new-flag)
              (let ((err pgid-or-error))
                ;; (format (console-error-port) "; error: failed activating job control: C function c_job_control_set(1) failed with error ~s: ~a\n" err (c-errno->string err))
                old-flag))))
        (new-flag
          ;; (put-string (console-error-port) "; error: cannot activate job control, parameter tty-job-control-available? is #f\n")
          old-flag)
        (old-flag
          ;; try to deactivate job control
          ;; (let ((err (c-job-control-proc 0)))
          ;;   (when (< err 0)
          ;;     (format (console-error-port) "; warning: failed deactivating job control: C function c_job_control_set(0) failed with error ~s: ~a\n"
          ;;          err (c-errno->string err))))
          ;; set job-control to inactive even if c_job_control_set() failed
          new-flag)
        (else ; should not happen
          old-flag)))))


;; parameter containing user-defined procedure for enabling/disabling job control
(define tty-job-control-proc
  (let ((proc minimal-job-control-set!))
    (case-lambda
      (()
        proc)
      ((new-proc)
        (assert* 'tty-job-control-proc (procedure? new-proc))
        (assert* 'tty-job-control-proc (logbit? 2 (procedure-arity-mask new-proc)))
        (set! proc new-proc)))))


;; wrapper around user-defined procedure stored in parameter (tty-job-control-proc)
(define (job-control-proc old-value new-value)
  ((tty-job-control-proc) old-value new-value))


;; Global parameter indicating whether job control is active.
;;
;; if (tty-job-control?) returns #t, job control is active:
;;   a. new process groups will be created as needed, and sub-processes may be moved into them.
;;   b. changing the foreground process group is allowed.
;;
;; if (tty-job-control?) returns #f, job control is inactive:
;;   a. sub-processes will inherit the parent's process group.
;;   b. changing the foreground process group is not allowed.
;;
;; Usually set to #t while (repl) is running, and to #f when executing scripts or in subshells.
;;
;; Job control can be activated i.e. set to #t only if (tty-job-control-available?) returns #t
;;
;; Activating job control requires taking control of the terminal foreground/background mechanism,
;; and thus for sanity will suspend this process if it's running in the background.
;;
(define tty-job-control? (make-stateful-parameter #f job-control-proc))


(define tty-restore!
  (let ((c-tty-restore! (foreign-procedure "c_tty_restore" () int)))
    (lambda ()
      ;; (debugf "tty-restore! job-control ~s" (tty-job-control?))
      (when (tty-job-control?)
        (c-tty-restore!)))))


(define tty-setraw!
  (let ((c-tty-setraw! (foreign-procedure "c_tty_setraw" () int)))
    (lambda ()
      ;; (debugf "tty-setraw! job-control ~s" (tty-job-control?))
      (when (tty-job-control?)
        (c-tty-setraw!)))))


;; (tty-size) calls C functions c_tty_size(),
;; which returns tty size of specified file descriptor (or of controlling by default)
;; as pair (width . height), or c_errno() < 0 on error
(define tty-size
  (let ((c-tty-size (foreign-procedure "c_tty_size" (int) ptr)))
    (case-lambda
      ((fd)
        (c-tty-size fd))
      (()
        (c-tty-size -1)))))


(define-syntax with-cooked-tty
  (syntax-rules ()
    ((_ body1 body2 ...)
      (dynamic-wind
        tty-restore!       ; run before body
        (lambda () body1 body2 ...)
        tty-setraw!))))      ; run after body


(define-syntax with-raw-tty
  (syntax-rules ()
    ((_ body1 body2 ...)
      (dynamic-wind
        tty-setraw!       ; run before body
        (lambda () body1 body2 ...)
        tty-restore!))))      ; run after body


(define (tty-inspect obj)
  (with-cooked-tty (inspect obj)))


) ; close library
