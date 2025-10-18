;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix pid (0 9 2))
  (export pid-get pgid-get pid-kill pid-wait)
  (import
    (rnrs)
    (only (chezscheme)            foreign-procedure)
    (only (schemesh bootstrap)    assert*)
    (only (schemesh posix fd)     raise-c-errno)
    (only (schemesh posix signal) signal-name->number))


;; return pid of current process
(define pid-get
  (let ((c-pid-get (foreign-procedure "c_pid_get" () int)))
    (lambda ()
      (let ((ret (c-pid-get)))
        (when (< ret 0)
          (raise-c-errno 'pid-get 'getpid ret))
        ret))))

;; return process group of specified process (0 = current process)
(define pgid-get
  (let ((c-pgid-get (foreign-procedure "c_pgid_get" (int) int)))
    (lambda (pid)
      (let ((ret (c-pgid-get pid)))
        (when (< ret 0)
          (raise-c-errno 'pgid-get 'getpgid ret pid))
        ret))))


;; call C function kill(pid, sig) i.e. send specified signal to the process(es) identified by pid.
;; Notes:
;;   pid ==  0 means "all processes in the same process group as the caller".
;;   pid == -1 means "all processes".
;;   pid <  -1 means "all processes in process group -pid"
;
;; Returns 0 on success.
;; Otherwise < 0 if signal-name is unknown, or if C function kill() fails with C errno != 0.
(define pid-kill
  (let ((c-pid-kill (foreign-procedure "c_pid_kill" (int int int) int))
        (c-errno-einval ((foreign-procedure "c_errno_einval" () int))))
    (case-lambda
      ((pid signal-name-or-number pause-if-successful?)
        ;; (format #t "pid-kill ~s ~s" pid signal-name)
        (let ((signal-number (if (fixnum? signal-name-or-number)
                               signal-name-or-number
                               (signal-name->number signal-name-or-number))))
          (if (fixnum? signal-number)
            (c-pid-kill pid signal-number (if pause-if-successful? 1 0))
            c-errno-einval)))
      ((pid signal-name-or-number)
        (pid-kill pid signal-name-or-number #f)))))


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
  (let ((c-pid-wait (foreign-procedure __collect_safe "c_pid_wait" (int int) ptr)))
    (lambda (pid may-block)
      (assert* 'pid-wait (memq may-block '(blocking nonblocking)))
      (c-pid-wait pid (if (eq? may-block 'blocking) 1 0)))))


) ; close library
