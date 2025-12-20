;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix fd (0 9 2))
  (export
    c-errno c-errno->string c-exit c-hostname
    fd-open-max fd-close fd-close-list fd-dup fd-dup2 fd-seek
    fd-read fd-read-all fd-read-insert-right! fd-read-noretry fd-read-u8
    fd-write fd-write-all fd-write-noretry fd-write-u8 fd-select fd-setnonblock
    file->fd open-pipe-fds open-socket-fd open-socketpair-fds
    raise-c-errno)
  (import
    (rnrs)
    (only (chezscheme)             foreign-procedure lock-object logbit? void procedure-arity-mask unlock-object)
    (only (scheme2k bootstrap)     assert* check-interrupts raise-errorf sh-make-thread-parameter with-locked-objects while)
    (scheme2k containers bytespan)
    (only (scheme2k containers hashtable) alist->eq-hashtable hashtable-transpose)
    (only (scheme2k conversions)   text->bytevector0 transcoder-utf8))

(define c-errno         (foreign-procedure "c_errno" () int))
(define c-errno-einval  ((foreign-procedure "c_errno_einval" () int))) ;; integer, not a procedure
(define c-errno->string (foreign-procedure "c_strerror_string" (int) ptr))
(define c-exit          (foreign-procedure "c_exit" (int) int))

(define c-hostname
  (let* ((hostname-or-error ((foreign-procedure "c_get_hostname" () ptr)))
         (hostname (if (string? hostname-or-error) hostname-or-error "???")))
    (lambda ()
      hostname)))

(define (raise-c-errno who c-who c-errno . c-args)
  ; (debugf "raise-c-errno ~s ~s" who c-errno)
  (raise-errorf who "C function ~s~s failed with error ~s: ~a"
    c-who c-args c-errno (if (integer? c-errno) (c-errno->string c-errno) "unknown error")))

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
          (raise-c-errno 'fd-dup 'dup ret old-fd))))))

(define fd-dup2
  (let ((c-fd-dup2 (foreign-procedure "c_fd_dup2" (int int) int)))
    (lambda (old-fd new-fd)
      (let ((ret (c-fd-dup2 old-fd new-fd)))
        (if (>= ret 0)
          (void)
          (raise-c-errno 'fd-dup2 'dup2 ret old-fd new-fd))))))


(define (int64? obj)
  (and (integer? obj) (exact? obj)
       (<= #x-10000000000000000 obj #xFFFFFFFFFFFFFFFF)))


;; change current position on file descriptor fd.
;; offset must be an exact integer representable as C int64_t
;; and from must be one of the symbols: 'seek-set 'seek-cur 'seek-end
;;
;; return updated position, counted in bytes from start of file,
;; or < 0 on error
(define fd-seek
  (let ((c-fd-seek (foreign-procedure __collect_safe "c_fd_seek" (int integer-64 fixnum) integer-64)))
    (lambda (fd offset from)
      (if (and (fixnum? fd)
               (int64? offset)
               (memq from '(seek-set seek-cur seek-end)))
        (c-fd-seek fd offset (case from ((seek-cur) 1) ((seek-end) 2) (else 0)))
        c-errno-einval))))


;; read from fd until end-of-file.
;; return read bytes as a bytevector,
;; or raise exception on I/O error.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define (fd-read-all fd)
  (let ((bsp (make-bytespan 0)))
    (let %loop ()
      (let ((n (fd-read-insert-right! fd bsp)))
        (when (and (integer? n) (> n 0))
          (%loop))))
    (bytespan->bytevector*! bsp)))


;; read some bytes from fd and append them to specified bytespan
;; return number of bytes actually read, which can be 0 only on end-of-file,
;; or raise exception on I/O error.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define (fd-read-insert-right! fd bsp)
  (bytespan-reserve-right! bsp (fx+ 4096 (bytespan-length bsp)))
  (let* ((beg (bytespan-peek-beg bsp))
         (end (bytespan-peek-end bsp))
         (cap (bytespan-capacity-right bsp))
         (n   (fd-read fd (bytespan-peek-data bsp) end (fx+ beg cap))))
    (when (and (integer? n) (> n 0))
       (bytespan-resize-right! bsp (fx+ (fx- end beg) n)))
     n))


;; read some bytes from fd and copy them into bytevector
;; return number of bytes read, which can be 0 only on end-of-file or if (fx=? start end)
;; or raise exception on I/O error.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define fd-read
  (case-lambda
    ((fd bytevector-result start end)
      (let %loop ()
        (check-interrupts)
        (let ((ret (fd-read-noretry fd bytevector-result start end)))
          (if (eq? #t ret)
            (%loop)
            ret))))
    ((fd bytevector-result)
      (fd-read fd bytevector-result 0 (bytevector-length bytevector-result)))))


;; read some bytes from fd and copy them into bytevector.
;; return number of bytes actually read, which can be 0 only on end-of-file or if (fx>=? start end)
;; or raise exception on I/O error.
;;
;; Note: if interrupted, returns #t
(define fd-read-noretry
  (let ((c-fd-read (foreign-procedure __collect_safe "c_fd_read" (int ptr fixnum fixnum) ptr)))
    (case-lambda
      ((fd bytevector-result start end)
        (let ((ret (with-locked-objects (bytevector-result)
                     (c-fd-read fd bytevector-result start end))))
          (if (or (eq? #t ret) (and (integer? ret) (>= ret 0)))
            ret
            (raise-c-errno 'fd-read 'read ret fd #vu8() start end))))
      ((fd bytevector-result)
        (fd-read fd bytevector-result 0 (bytevector-length bytevector-result))))))


;; read a single byte from fd and return it.
;; Return (eof-object) on end-of-file, or raise exception on I/O error.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define fd-read-u8
  (let ((c-fd-read-u8 (foreign-procedure __collect_safe "c_fd_read_u8" (int) ptr)))
    (lambda (fd)
      (let %loop ()
        (check-interrupts)
        (let ((ret (c-fd-read-u8 fd)))
          (cond
            ((and (fixnum? ret) (fx<=? 0 ret 255))
              ret)
            ((eq? #t ret)
              (%loop))
            ((eq? #f ret)
              (eof-object))
            (else
              (raise-c-errno 'fd-read-u8 'read ret fd #vu8()))))))))


;; write all specified bytevector range to fd, iterating in case of short writes.
;; return void if successful,
;; otherwise raise exception.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define fd-write-all
  (case-lambda
    ((fd bytevector-towrite start end)
      (let %loop ((pos start))
        (if (fx=? pos end)
          (void)
          (let ((ret (fd-write fd bytevector-towrite pos end)))
            (if (and (integer? ret) (> ret 0))
              (%loop (+ pos ret))
              (raise-c-errno 'fd-write 'write ret fd #vu8() start end))))))
    ((fd bytevector-towrite)
      (fd-write-all fd bytevector-towrite 0 (bytevector-length bytevector-towrite)))))


;; write a portion of bytevector into fd.
;; return number of bytes actually written, which may be less than (fx- end start)
;; or raise exception on I/O error.
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define fd-write
  (case-lambda
    ((fd bytevector-towrite start end)
      (let %loop ()
        (check-interrupts)
        (let ((ret (fd-write-noretry fd bytevector-towrite start end)))
          (if (eq? #t ret)
            (%loop)
            ret))))
    ((fd bytevector-towrite)
      (fd-write fd bytevector-towrite 0 (bytevector-length bytevector-towrite)))))


;; write a portion of bytevector into fd.
;; return number of bytes written,
;; or raise exception on I/O error.
;;
;; Note: if interrupted, returns #t
(define fd-write-noretry
  (let ((c-fd-write (foreign-procedure __collect_safe "c_fd_write" (int ptr fixnum fixnum) ptr)))
    (case-lambda
      ((fd bytevector-towrite start end)
        (let ((ret (with-locked-objects (bytevector-towrite)
                     (c-fd-write fd bytevector-towrite start end))))
          (if (or (eq? #t ret) (and (integer? ret) (>= ret 0)))
            ret
            (raise-c-errno 'fd-write 'write ret fd #vu8() start end))))
      ((fd bytevector-towrite)
        (fd-write fd bytevector-towrite 0 (bytevector-length bytevector-towrite))))))


;; write a single byte to fd, and return unspecified value.
;; Raise exception on I/O error.
;;
;; The byte must be an exact integer in [-128, 255]
;;
;; Note: if interrupted, calls (check-interrupts) then tries again if (check-interrupts) returns normally.
(define fd-write-u8
  (let ((c-fd-write-u8 (foreign-procedure __collect_safe "c_fd_write_u8" (int int) ptr)))
    (lambda (fd u8)
      (let %loop ()
        (check-interrupts)
        (let ((ret (c-fd-write-u8 fd u8)))
          (cond
            ((eqv? 0 ret)   (void))
            ((eq? #t ret)   (%loop))
            (else           (raise-c-errno 'fd-write-u8 'write ret fd #vu8()))))))))


;; (fd-select fd direction timeout-milliseconds) waits up to timeout-milliseconds
;; for file descriptor fd to become ready for input, output or both.
;;
;; direction must be one of: 'read 'write 'rw
;; timeout-milliseconds < 0 means infinite timeout
;;
;; On success, returns one of: 'timeout 'read 'write 'rw
;; On error, raises condition.
;;
;; If interrupted, returns 'timeout
(define fd-select
  (let ((c-fd-select (foreign-procedure __collect_safe "c_fd_select" (int int int) int))
        (c-errno-eio ((foreign-procedure "c_errno_eio" () int)))
        (c-errno-eintr ((foreign-procedure "c_errno_eintr" () int))))
    (lambda (fd direction timeout-milliseconds)
      (assert* 'fd-select (memq direction '(read write rw)))
      (let* ((rw-mask (cond ((eq? 'rw    direction) 3)
                            ((eq? 'write direction) 2)
                            ((eq? 'read  direction) 1)
                            (else (error 'fd-select "direction must be one of 'read 'write 'rw"))))
              (ret (c-fd-select fd rw-mask timeout-milliseconds)))
        (cond
          ; if c_fd_select() returns EINTR, consider it a timeout
          ((eqv? ret c-errno-eintr) 'timeout)
          ((< ret 0) (raise-c-errno 'fd-select 'select ret fd rw-mask timeout-milliseconds))
          ((< ret 4) (vector-ref '#(timeout read write rw) ret))
          ; c_fd_select() called poll() which set (revents & POLLERR)
          (else      (raise-c-errno 'fd-select 'select c-errno-eio fd rw-mask timeout-milliseconds)))))))


(define fd-setnonblock
  (let ((c-fd-setnonblock (foreign-procedure __collect_safe "c_fd_setnonblock" (int) int)))
    (lambda (fd)
      (let ((ret (c-fd-setnonblock fd)))
        (if (>= ret 0)
          ret
          (raise-c-errno 'fd-setnonblock 'fcntl ret fd))))))


;; open a file and returns its file descriptor.
;; Arguments:
;;   mandatory filepath must be string, bytevector, bytespan or charspan.
;;   mandatory direction must be one of the symbols: 'read 'write 'rw
;;   optional flags must be a list containing zero or more: 'create 'truncate 'append
(define file->fd
  (let ((c-open-file-fd (foreign-procedure __collect_safe "c_open_file_fd"
                          (ptr int int int int) int)))
    (case-lambda
      ((filepath direction flags)
        (let* ((filepath0 (text->bytevector0 filepath))
               (dir (case direction
                      ((read) 0) ((write) 1) ((rw) 2)
                      (else (error 'file->fd
                              "direction must be one of 'read 'write 'rw" direction))))
               (flag-create   (if (memq 'create   flags) 1 0))
               (flag-truncate (if (memq 'truncate flags) 1 0))
               (flag-append   (if (memq 'append   flags) 1 0))
               (ret (with-locked-objects (filepath0)
                      (c-open-file-fd filepath0 dir flag-create flag-truncate flag-append))))
          (if (>= ret 0)
            ret
            (apply raise-c-errno 'file->fd 'open ret filepath direction flags))))
      ((filepath direction)
        (file->fd filepath direction '())))))


;; create a pipe.
;; Optional arguments:
;;   read-fd-close-on-exec?  if truish the read side of the pipe will be close-on-exec. Defaults to 'close-on-exec
;;   write-fd-close-on-exec? if truish the write side of the pipe will be close-on-exec. Defaults to 'close-on-exec
;; Returns two file descriptors:
;;   the read side of the pipe
;;   the write side of the pipe
;; On errors, raises an exception
(define open-pipe-fds
  (let ((c-open-pipe-fds (foreign-procedure "c_open_pipe_fds" (ptr ptr) ptr)))
    (case-lambda
      ((read-fd-close-on-exec? write-fd-close-on-exec?)
        (let ((ret (c-open-pipe-fds read-fd-close-on-exec? write-fd-close-on-exec?)))
          (if (pair? ret)
            (values (car ret) (cdr ret))
            (raise-c-errno 'open-pipe-fds 'pipe ret))))
      (()
        (open-pipe-fds 'close-on-exec 'close-on-exec)))))


;; create a pair of mutually connected AF_UNIX socket file descriptors.
;; Optional arguments:
;;   fd1-close-on-exec? if truish the first socket will be close-on-exec. Defaults to 'close-on-exec
;;   fd2-close-on-exec? if truish the second socket will be close-on-exec. Defaults to 'close-on-exec
;; Returns two file descriptors:
;;   the first socket
;;   the second socket
;; On errors, raises an exception
(define open-socketpair-fds
  (let ((c-open-socketpair-fds (foreign-procedure "c_open_socketpair_fds" (ptr ptr) ptr)))
    (case-lambda
      ((fd1-close-on-exec? fd2-close-on-exec?)
        (let ((ret (c-open-socketpair-fds fd1-close-on-exec? fd2-close-on-exec?)))
          (if (pair? ret)
            (values (car ret) (cdr ret))
            (raise-c-errno 'open-socketpair-fds 'socketpair ret))))
      (()
        (open-socketpair-fds 'close-on-exec 'close-on-exec)))))

(define socket-domain-number->name
  (alist->eq-hashtable ((foreign-procedure "c_socket_domain_list" () ptr))))

(define socket-domain-name->number
  (hashtable-transpose socket-domain-number->name (make-eqv-hashtable)))

(define socket-type-number->name
  (alist->eq-hashtable ((foreign-procedure "c_socket_type_list" () ptr))))

(define socket-type-name->number
  (hashtable-transpose socket-type-number->name (make-eqv-hashtable)))

;; create a socket.
;; Mandatory arguments:
;;   domain   a symbol among 'inet 'inet6 'unix ...
;;   type     a symbol among 'dgram 'raw 'stream ...
;; Optional arguments:
;;   protocol the symbol 'default
;;   close-on-exec? if truish, socket will be close-on-exec. Defaults to 'close-on-exec
;; Returns an integer file descriptor
;; On errors, raises an exception
(define open-socket-fd
  (let ((c-open-socket-fd (foreign-procedure "c_open_socket_fd" (int int int int) ptr)))
    (case-lambda
      ((domain type protocol close-on-exec?)
        (let ((domain-int (hashtable-ref socket-domain-name->number domain #f))
              (type-int   (hashtable-ref socket-type-name->number type #f)))
          (assert* 'open-socket-fd domain-int)
          (assert* 'open-socket-fd type-int)
          (assert* 'open-socket-fd (eq? protocol 'default))
        (let ((ret (c-open-socket-fd domain-int type-int 0 close-on-exec?)))
          (if (or (and (fixnum? ret) (fx>? ret 0))
                  (and (exact? ret) (integer? ret) (> ret 0)))
            ret
            (raise-c-errno 'open-socket-fd 'socket ret)))))
      ((domain type protocol)
        (open-socket-fd domain type protocol 'close-on-exec))
      ((domain type)
        (open-socket-fd domain type 'default 'close-on-exec)))))

) ; close library
