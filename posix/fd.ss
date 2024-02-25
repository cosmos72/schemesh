;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix fd (0 1))
  (export
    errno make-errno-condition raise-errno-condition
    fd-close fd-close-list fd-dup fd-dup2 fd-read fd-write fd-select fd-setnonblock
    open-file-fd open-pipe-fds)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure void)
    (only (schemesh bootstrap)       assert*)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh conversions)     string->bytevector0))

(define errno
  (foreign-procedure "c_errno" () int))

(define (make-errno-condition who c-errno)
  (condition
    (make-error)
    (make-who-condition who)
    (make-message-condition "error in C function")
    (make-irritants-condition c-errno)))

(define (raise-errno-condition who c-errno)
  ; (debugf "raise-errno-condition ~s ~s~%" who c-errno)
  (raise (make-errno-condition who c-errno)))

(define fd-close
  (let ((c-fd-close (foreign-procedure "c_fd_close" (int) int)))
    (lambda (x)
      (let ((ret (c-fd-close x)))
        (if (>= ret 0)
          (void)
          (make-errno-condition 'fd-close ret))))))

(define (fd-close-list fd-list)
  (list-iterate fd-list fd-close))

(define fd-dup
  (let ((c-fd-dup (foreign-procedure "c_fd_dup" (int) int)))
    (lambda (old-fd)
      (let ((ret (c-fd-dup old-fd)))
        (if (>= ret 0)
          ret
          (raise-errno-condition 'fd-dup ret))))))

(define fd-dup2
  (let ((c-fd-dup2 (foreign-procedure "c_fd_dup2" (int int) int)))
    (lambda (old-fd new-fd)
      (let ((ret (c-fd-dup2 old-fd new-fd)))
        (if (>= ret 0)
          (void)
          (raise-errno-condition 'fd-dup2 ret))))))

(define fd-read
  (let ((c-fd-read (foreign-procedure "c_fd_read" (int ptr iptr iptr) iptr)))
    (lambda (fd bytevector-result start end)
      (let ((ret (c-fd-read fd bytevector-result start end)))
        (if (>= ret 0)
          ret
          (raise-errno-condition 'fd-read ret))))))

(define fd-write
  (let ((c-fd-write (foreign-procedure "c_fd_write" (int ptr iptr iptr) iptr)))
    (lambda (fd bytevector-towrite start end)
      (let ((ret (c-fd-write fd bytevector-towrite start end)))
        (if (>= ret 0)
          ret
          (raise-errno-condition 'fd-write ret))))))

; (fd-select fd direction timeout-milliseconds) waits up to timeout-milliseconds
; for file descriptor fd to become ready for input, output or both.
;
; direction must be one of: 'read 'write 'rw
; timeout-milliseconds < 0 means infinite timeout
;
; On success, returns one of: 'timeout 'read 'write 'rw
; On error, raises condition.
(define fd-select
  (let ((c-fd-select (foreign-procedure "c_fd_select" (int int int) int))
        (c-errno-eio ((foreign-procedure "c_errno_eio" () int)))
        (c-errno-eintr ((foreign-procedure "c_errno_eintr" () int))))
    (lambda (fd direction timeout-milliseconds)
      (assert* (memq direction '(read write rw)))
      (let* ((rw-mask (cond ((eq? 'rw    direction) 3)
                            ((eq? 'write direction) 2)
                            ((eq? 'read  direction) 1)
                            (#t (error 'fd-select "direction must be one of 'read 'write 'rw"))))
              (ret (c-fd-select fd rw-mask timeout-milliseconds)))
        (cond
          ; if c_fd_select() returns EINTR, consider it a timeout
          ((eqv? ret c-errno-eintr) 'timeout)
          ((< ret 0) (raise-errno-condition 'fd-select ret))
          ((< ret 4) (vector-ref '#(timeout read write rw) ret))
          ; c_fd_select() called poll() which set (revents & POLLERR)
          (#t        (raise-errno-condition 'fd-select c-errno-eio)))))))

(define fd-setnonblock
  (let ((c-fd-setnonblock (foreign-procedure "c_fd_setnonblock" (int) int)))
    (lambda (fd)
      (let ((ret (c-fd-setnonblock fd)))
        (if (>= ret 0)
          ret
          (raise-errno-condition 'fd-setnonblock ret))))))

; filepath must be string or bytevector.
; each flag must be one of: 'read 'write 'rw 'create 'truncate 'append
; at least one of 'read 'write 'rw must be present
(define open-file-fd
  (let ((c-open-file-fd (foreign-procedure "c_open_file_fd"
                          (scheme-object int int int int) int)))
    (lambda (filepath . flags)
      (let* ([filepath0 (string->bytevector0 filepath)]
             [flag-rw (cond ((memq 'rw    flags) 2)
                            ((memq 'write flags) 1)
                            ((memq 'read  flags) 0)
                            (#t (error 'open-file-fd
                                 "flags must contain one of 'read 'write 'rw" flags)))]
             [flag-create   (if (memq 'create   flags) 1 0)]
             [flag-truncate (if (memq 'truncate flags) 1 0)]
             [flag-append   (if (memq 'append   flags) 1 0)]
             [ret (c-open-file-fd filepath0 flag-rw flag-create flag-truncate flag-append)])
        (if (>= ret 0)
          ret
          (raise-errno-condition 'open-file-fd ret))))))

(define open-pipe-fds
  (let ((c-open-pipe-fds (foreign-procedure "c_open_pipe_fds" () scheme-object)))
    (lambda ()
      (let ((ret (c-open-pipe-fds)))
        (if (pair? ret)
          ret
          (raise-errno-condition 'open-pipe-fds ret))))))

) ; close library
