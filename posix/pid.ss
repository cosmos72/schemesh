;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh posix pid (0 1))
  (export get-pid get-pgid spawn-pid pid-kill pid-wait exit-with-job-status)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure format void)
    (only (schemesh bootstrap)    assert*)
    (schemesh posix fd)
    (only (schemesh conversions)  list->argv)
    (only (schemesh posix signal) signal-name->number signal-raise)
    (only (schemesh posix misc)   c-exit))

; (get-pid) returns pid of current process
(define get-pid
  (let ((c-get-pid (foreign-procedure "c_get_pid" () int)))
    (lambda ()
      (let ((ret (c-get-pid)))
        (when (< ret 0)
          (raise-c-errno 'get-pid 'getpid ret))
        ret))))

; (get-pgid) returns process group of specified process (0 = current process)
(define get-pgid
  (let ((c-get-pgid (foreign-procedure "c_get_pgid" (int) int)))
    (lambda (pid)
      (let ((ret (c-get-pgid pid)))
        (when (< ret 0)
          (raise-c-errno 'get-pgid 'getpgid ret))
        ret))))


; Spawn an external program in a new background process group (pgid) and return its pid.
;
; Parameter program is the program path to spawn;
; Parameter args is the list of arguments to pass to the program;
; The parameter program and each element in args must be either a string or a bytevector.
(define spawn-pid
  (let ((c-spawn-pid (foreign-procedure "c_spawn_pid"
                        (scheme-object scheme-object scheme-object int) int)))
    (lambda (program . args)
      (let ((ret (c-spawn-pid
                   (list->argv (cons program args))
                   (vector 0 1 2)
                   #f ; no environment override
                   0)))
        (when (< ret 0)
          (raise-c-errno 'spawn-pid 'fork ret))
        ret))))


; (pid-kill pid signal-name) calls C function kill(pid, sig) i.e. sends specified signal
; to the process(es) identified by pid.
; Notes:
;   pid ==  0 means "all processes in the same process group as the caller".
;   pid == -1 means "all processes".
;   pid <  -1 means "all processes in process group -pid"
;
; Returns < 0 if signal-name is unknown, or if C function kill() fails with C errno != 0.
(define pid-kill
  (let ((c-pid-kill (foreign-procedure "c_pid_kill" (int int) int))
        (c-errno-einval ((foreign-procedure "c_errno_einval" () int))))
    (lambda (pid signal-name)
      ; (format #t "pid-kill ~s ~s~%" pid signal-name)
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (c-pid-kill pid signal-number)
          c-errno-einval)))))


; (pid-wait pid may-block) calls waitpid(pid, WUNTRACED) i.e. checks if process specified by
; pid exited or stopped. Notes: pid ==  0 means "any process in the same process group as
; the caller". pid == -1 means "any child process". pid <  -1 means "any process in process
; group -pid".
;
; Argument may-block must be either 'blocking or 'nonblocking.
; If may-block is 'blocking, wait until pid (or any child process, if pid == -1) exits or
; stops, otherwise check for such conditions without blocking.
;
; If waitpid() fails with C errno != 0, return < 0.
; If no child process matches pid, or if may_block is 'nonblocking and no child exited or
; stopped, return '().
; Otherwise return a Scheme cons (pid . exit_flag), where exit_flag is one of:
; process_exit_status, or 256 + signal, or 512 + stop_signal.
(define pid-wait
  (let ((c-pid-wait (foreign-procedure "c_pid_wait" (int int) scheme-object)))
    (lambda (pid may-block)
      (assert* 'pid-wait (memq may-block '(blocking nonblocking)))
      (c-pid-wait pid (if (eq? may-block 'blocking) 1 0)))))


; Call kill() or exit() to terminate current process with job-status, which can be one of:
;   (void)                       ; will call C function exit(0)
;   (cons 'exited  exit-status)  ; will call C function exit(exit_status)
;   (cons 'killed  signal-name)  ; will call C function kill(getpid(), signal_number)
;               ; unless signal-name is one of: 'sigstop 'sigtstp 'sigcont 'sigttin 'sigttou
;               ; if kill() returns, will call C function exit(128 + signal_number)
;   ... any other value ... ;  will call C function exit(255)
(define (exit-with-job-status status)
  ; (debugf "exit-with-job-status ~s~%" status)
  (let ((exit-status
         (cond
            ((eq? (void) status) 0)
            ((and (pair? status) (eq? 'exited (car status))
                  (fixnum? (cdr status)) (fx=? (cdr status)
                                               (fxand 255 (cdr status))))
               (cdr status))
            (#t 255))))
    (dynamic-wind
      void       ; before body
      (lambda () ; body
        (when (and (pair? status) (eq? 'killed (car status)))
          (let ((signal-name (cdr status)))
            (unless (memq signal-name '(sigstop sigtstp sigcont
                                          sigttin sigttou))
              (signal-raise signal-name))
            ; process did not die with (signal-raise)
            (let ((signal-number (signal-name->number signal-name)))
              (when (fixnum? signal-number)
                (set! exit-status (fx+ 128 signal-number)))))))
      (lambda () ; after body
        (c-exit exit-status)))))
) ; close library
