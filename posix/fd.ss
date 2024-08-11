;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix fd (0 1))
  (export
    c-errno c-errno->string raise-c-errno
    fd-open-max fd-close fd-close-list fd-dup fd-dup2 fd-read fd-write
    fd-read-until-eof fd-select fd-setnonblock open-file-fd open-pipe-fds)
  (import
    (rnrs)
    (only (chezscheme) foreign-procedure void)
    (only (schemesh bootstrap)       assert* raise-errorf until)
    (schemesh containers bytespan)
    (only (schemesh containers misc) list-iterate)
    (only (schemesh conversions)     text->bytevector0))

(define c-errno
  (foreign-procedure "c_errno" () int))

(define c-errno->string
  (foreign-procedure "c_strerror" (int) ptr))


(define (raise-c-errno who c-who c-errno)
  ; (debugf "raise-c-errno ~s ~s~%" who c-errno)
  (raise-errorf who "C function ~s failed with error ~s" c-who c-errno))

;; return the maximum number of open file descriptors for a process
(define fd-open-max
  (let ((c-fd-open-max (foreign-procedure "c_fd_open_max" () int)))
    (lambda ()
      (let ((ret (c-fd-open-max)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-open-max 'sysconf ret))))))

(define fd-close
  (let ((c-fd-close (foreign-procedure "c_fd_close" (int) int)))
    (lambda (fd)
      (c-fd-close fd)))) ; used in cleanups, do NOT raise exceptions here

(define fd-close-list
  (let ((c-fd-close-list (foreign-procedure "c_fd_close_list" (ptr) int)))
    (lambda (fd-list)
      (if (null? fd-list)
        0
        (c-fd-close-list fd-list))))) ; used in cleanups, do NOT raise exceptions here

(define fd-dup
  (let ((c-fd-dup (foreign-procedure "c_fd_dup" (int) int)))
    (lambda (old-fd)
      (let ((ret (c-fd-dup old-fd)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-dup 'dup ret))))))

(define fd-dup2
  (let ((c-fd-dup2 (foreign-procedure "c_fd_dup2" (int int) int)))
    (lambda (old-fd new-fd)
      (let ((ret (c-fd-dup2 old-fd new-fd)))
        (if (>= ret 0)
          (void)
          (raise-c-errno 'fd-dup2 'dup2 ret))))))

;; read from fd until end-of-file.
;; return read bytes as a bytespan
(define (fd-read-until-eof fd)
  (let ((bsp (make-bytespan 0))
        (eof? #f))
    (until eof?
      (bytespan-reserve-back! bsp 4096)
      (let* ((beg (bytespan-peek-beg bsp))
             (end (bytespan-peek-end bsp))
             (cap (bytespan-capacity-back bsp))
             (n   (fd-read fd (bytespan-peek-data bsp) end (fx+ beg cap))))
        (if (fx>? n 0)
          (bytespan-resize-back! bsp (fx+ (fx- end beg) n))
          (set! eof? #t))))
    bsp))

(define fd-read
  (let ((c-fd-read (foreign-procedure "c_fd_read" (int ptr iptr iptr) iptr)))
    (lambda (fd bytevector-result start end)
      (let ((ret (c-fd-read fd bytevector-result start end)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-read 'read ret))))))

(define fd-write
  (let ((c-fd-write (foreign-procedure "c_fd_write" (int ptr iptr iptr) iptr)))
    (lambda (fd bytevector-towrite start end)
      (let ((ret (c-fd-write fd bytevector-towrite start end)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-write 'write ret))))))

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
      (assert* 'fd-select (memq direction '(read write rw)))
      (let* ((rw-mask (cond ((eq? 'rw    direction) 3)
                            ((eq? 'write direction) 2)
                            ((eq? 'read  direction) 1)
                            (#t (error 'fd-select "direction must be one of 'read 'write 'rw"))))
              (ret (c-fd-select fd rw-mask timeout-milliseconds)))
        (cond
          ; if c_fd_select() returns EINTR, consider it a timeout
          ((eqv? ret c-errno-eintr) 'timeout)
          ((< ret 0) (raise-c-errno 'fd-select 'select ret))
          ((< ret 4) (vector-ref '#(timeout read write rw) ret))
          ; c_fd_select() called poll() which set (revents & POLLERR)
          (#t        (raise-c-errno 'fd-select 'select c-errno-eio)))))))

(define fd-setnonblock
  (let ((c-fd-setnonblock (foreign-procedure "c_fd_setnonblock" (int) int)))
    (lambda (fd)
      (let ((ret (c-fd-setnonblock fd)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-setnonblock 'fcntl ret))))))

; filepath must be string or bytevector.
; each flag must be one of: 'read 'write 'rw 'create 'truncate 'append
; at least one of 'read 'write 'rw must be present
(define open-file-fd
  (let ((c-open-file-fd (foreign-procedure "c_open_file_fd"
                          (ptr int int int int) int)))
    (lambda (filepath . flags)
      (let* ([filepath0 (text->bytevector0 filepath)]
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
          (raise-c-errno 'open-file-fd 'open ret))))))

(define open-pipe-fds
  (let ((c-open-pipe-fds (foreign-procedure "c_open_pipe_fds" (ptr ptr) ptr)))
    (lambda (read-fd-close-on-exec? write-fd-close-on-exec?)
      (let ((ret (c-open-pipe-fds read-fd-close-on-exec? write-fd-close-on-exec?)))
        (if (pair? ret)
          (values (car ret) (cdr ret))
          (raise-c-errno 'open-pipe-fds 'pipe ret))))))

) ; close library
