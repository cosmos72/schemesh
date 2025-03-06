;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix pid (0 8 1))
  (export pid-get pgid-get pid-kill pid-wait exit-with-job-status)
  (import
    (rnrs)
    (only (chezscheme)            console-output-port console-error-port foreign-procedure format void)
    (only (schemesh bootstrap)    assert* debugf)
    (schemesh posix fd)
    (only (schemesh conversions)  list->argv)
    (only (schemesh posix signal) signal-name->number signal-raise)
    (only (schemesh posix fd)     c-exit))

;; (pid-get) returns pid of current process
(define pid-get
  (let ((c-pid-get (foreign-procedure "c_pid_get" () int)))
    (lambda ()
      (let ((ret (c-pid-get)))
        (when (< ret 0)
          (raise-c-errno 'pid-get 'getpid ret))
        ret))))

;; (pgid-get) returns process group of specified process (0 = current process)
(define pgid-get
  (let ((c-pgid-get (foreign-procedure "c_pgid_get" (int) int)))
    (lambda (pid)
      (let ((ret (c-pgid-get pid)))
        (when (< ret 0)
          (raise-c-errno 'pgid-get 'getpgid ret pid))
        ret))))


;; (pid-kill pid signal-name-or-number) calls C function kill(pid, sig)
;; i.e. sends specified signal to the process(es) identified by pid.
;; Notes:
;;   pid ==  0 means "all processes in the same process group as the caller".
;;   pid == -1 means "all processes".
;;   pid <  -1 means "all processes in process group -pid"
;
;; Returns 0 on success.
;; Otherwise < 0 if signal-name is unknown, or if C function kill() fails with C errno != 0.
(define pid-kill
  (let ((c-pid-kill (foreign-procedure "c_pid_kill" (int int) int))
        (c-errno-einval ((foreign-procedure "c_errno_einval" () int))))
    (lambda (pid signal-name-or-number)
      ; (format #t "pid-kill ~s ~s" pid signal-name)
      (let ((signal-number (if (fixnum? signal-name-or-number)
                             signal-name-or-number
                             (signal-name->number signal-name-or-number))))
        (if (fixnum? signal-number)
          (c-pid-kill pid signal-number)
          c-errno-einval)))))


;; (pid-wait pid may-block) calls waitpid(pid, WUNTRACED) i.e. checks if process specified by pid finished or stopped.
;;
;; Special cases:
;;   pid ==  0 means "any child process in the same process group as the caller"
;;   pid == -1 means "any child process"
;;   pid <  -1 means "any child process in process group -pid"
;
;; Argument may-block must be either 'blocking or 'nonblocking.
;; If may-block is 'blocking, wait until pid (or any child process, if pid == -1)
;; exits or stops, otherwise check for such conditions without blocking.
;
;; If waitpid() fails with C errno != 0, return < 0.
;; If no child process matches pid, or if may_block is 'nonblocking and no child finished or
;; stopped, return '().
;; Otherwise return a Scheme cons (pid . exit_flag), where exit_flag is one of:
;; process_exit_status, or 256 + signal, or 512 + stop_signal, or 768 if job continued.
(define pid-wait
  (let ((c-pid-wait (foreign-procedure "c_pid_wait" (int int) ptr)))
    (lambda (pid may-block)
      (assert* 'pid-wait (memq may-block '(blocking nonblocking)))
      (c-pid-wait pid (if (eq? may-block 'blocking) 1 0)))))


;; reduced version of (sh-status->kind) copy-pasted from shell/status.ss
;;
;; Extract the kind of a status and return it.
(define (status->kind status)
  (cond
    ((eq? (void) status)
      'ok)
    ((and (pair? status) (not (null? (cdr status))))
      (cadr status))
    (else
      'failed)))


;; reduced version of (sh-status->value) copy-pasted from shell/status.ss
;;
;; Extract the first result of a status and return it.
(define (status->value status)
  (cond
    ((eq? (void) status) 0)
    ((and (pair? status) (not (null? (cdr status))))
      (if (eq? 'ok status)
        0
        (cadr status)))
    (else
      255)))


;; Call C functions kill() or exit() to terminate current process with job-status,
;; which can be one of:
;;   (void)                       ; will call C function exit(0)
;;   (list 'failed  exit-status)  ; will call C function exit(exit_status)
;;   (list 'killed  signal-name)  ; will call C function kill(getpid(), signal_number)
;;               ; unless signal-name is one of: 'sigstop 'sigtstp 'sigcont 'sigttin 'sigttou
;;               ; if kill() returns, will call C function exit(128 + signal_number)
;;   ... any other value ... ;  will call C function exit(255)
(define (exit-with-job-status status)
  ;; (debugf "exit-with-job-status ~s" status)
  (let* ((kind   (status->kind status))
         (result (status->value status)))
    (dynamic-wind
      void       ; before body
      (lambda () ; body
        (flush-output-port (console-output-port))
        (flush-output-port (console-error-port))
        (flush-output-port (current-output-port))
        (flush-output-port (current-error-port))
        (when (eq? 'killed kind)
          (let ((signal-name result))
            (unless (memq signal-name '(sigstop sigtstp sigcont sigttin sigttou))
              (signal-raise signal-name))
            ; process did not die with (signal-raise)
            (let ((signal-number (signal-name->number signal-name)))
              (when (fixnum? signal-number)
                (set! result (fx+ 128 signal-number)))))))
      (lambda () ; after body
        (c-exit
          (if (and (fixnum? result) (fx<=? 0 result 255))
            result
            255))))))


) ; close library
