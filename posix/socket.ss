;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix socket (0 9 2))
  (export
    make-sockaddr sockaddr sockaddr? sockaddr-data sockaddr-family
    socket-fd socket-connect socket-bind socket-listen socket-accept socketpair-fds)
  (import
    (rnrs)
    (only (chezscheme)                    bytevector->immutable-bytevector foreign-procedure procedure-arity-mask void)
    (only (scheme2k bootstrap)            assert* check-interrupts)
    (only (scheme2k conversions)          text->bytevector text->bytevector0)
    (only (scheme2k containers hashtable) alist->eq-hashtable eq-hashtable hashtable-transpose)
    (only (scheme2k posix fd)             raise-c-errno))


(define-record-type (%sockaddr %make-sockaddr sockaddr?)
  (fields
    (immutable family sockaddr-family) ; a symbol among 'inet 'inet6 'unix ...
    (immutable data   sockaddr-data))  ; an immutable bytevector containing C sockaddr_* bytes
  (nongenerative sockaddr-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (make-sockaddr family data)
  (assert* 'make-sockaddr (symbol? family))
  (assert* 'make-sockaddr (bytevector? data))
  (%make-sockaddr family (bytevector->immutable-bytevector data)))


(define socket-family-number->name
  (alist->eq-hashtable ((foreign-procedure "c_socket_family_list" () ptr))))

(define socket-family-name->number
  (hashtable-transpose socket-family-number->name (make-eqv-hashtable)))

(define socket-type-number->name
  (alist->eq-hashtable ((foreign-procedure "c_socket_type_list" () ptr))))

(define socket-type-name->number
  (hashtable-transpose socket-type-number->name (make-eqv-hashtable)))


(define c-sockaddr-unix-path-max ((foreign-procedure "c_sockaddr_unix_path_max" () size_t)))


;; create an INET socket address.
;; ipaddr must be one of:
;;   - a bytevector, string, bytespan or charspan containing decimal dotted notation, as for example "127.0.0.1"
;;   - (unimplemented) a 32-bit unsigned integer, as for example #x7f000001
;; port must be a 16-bit unsigned integer
(define sockaddr-inet
  (let ((c-sockaddr-inet (foreign-procedure "c_sockaddr_inet" (u8* unsigned-16) ptr)))
    (lambda (ipaddr port)
      (assert* 'sockaddr-inet (fixnum? port))
      (assert* 'sockaddr-inet (fx<=? 0 port 65535))
      (let* ((data (c-sockaddr-inet (text->bytevector0 ipaddr) port)))
        (unless (bytevector? data)
          (raise-c-errno 'sockaddr-inet 'c_sockaddr_inet data ipaddr port))
        (make-sockaddr 'inet data)))))


;; create an INET6 socket address.
;; ipaddr6 must be one of:
;;   - a bytevector, string, bytespan or charspan containing either
;;        -- hexadecimal notation, as for example "::ffff:0001"
;;        -- an IPv4-mapped IPv6 address, as for example "::ffff:204.152.189.116"
;;   - (unimplemented) a 128-bit unsigned integer, as for example #xffff0001000200030004000500060007
;; port must be a 16-bit unsigned integer
(define sockaddr-inet6
  (let ((c-sockaddr-inet6 (foreign-procedure "c_sockaddr_inet6" (u8* unsigned-16) ptr)))
    (lambda (ipaddr6 port)
      (assert* 'sockaddr-inet6 (fixnum? port))
      (assert* 'sockaddr-inet6 (fx<=? 0 port 65535))
      (let ((data (c-sockaddr-inet6 (text->bytevector0 ipaddr6) port)))
        (unless (bytevector? data)
          (raise-c-errno 'sockaddr-inet6 'c_sockaddr_inet6 data ipaddr6 port))
        (make-sockaddr 'inet6 data)))))


;; create a UNIX socket address. path must be a bytevector, string, bytespan or charspan
(define sockaddr-unix
  (let ((c-sockaddr-unix (foreign-procedure "c_sockaddr_unix" (ptr) ptr)))
    (lambda (path)
      (let ((path (text->bytevector path)))
        (assert* 'sockaddr-unix (fx<? (bytevector-length path) c-sockaddr-unix-path-max))
        (let ((data (c-sockaddr-unix path)))
          (unless (bytevector? data)
            (raise-c-errno 'sockaddr-unix 'c_sockaddr_unix data path))
          (make-sockaddr 'unix data))))))


(define sockaddr-constructors
  (let ((htable (eq-hashtable 'inet sockaddr-inet 'inet6 sockaddr-inet6 'unix sockaddr-unix)))
    (lambda ()
      htable)))


(define (sockaddr-constructor family)
  (let ((constructor (hashtable-ref (sockaddr-constructors) family #f)))
    (assert* 'sockaddr (procedure? constructor))
    constructor))


;; create a socket address
(define sockaddr
  (case-lambda
    ((family ipaddr port)
      ((sockaddr-constructor family) ipaddr port))
    ((family path)
      ((sockaddr-constructor family) path))))


(define c-errno-eintr       ((foreign-procedure "c_errno_eintr" () int)))
(define c-errno-eagain      ((foreign-procedure "c_errno_eagain" () int)))
(define c-errno-einprogress ((foreign-procedure "c_errno_einprogress" () int)))

;; create a socket.
;; Mandatory arguments:
;;   family   a symbol among 'inet 'inet6 'unix ...
;; Optional arguments:
;;   type     a symbol among 'dgram 'raw 'stream ... Defaults to 'stream
;;   protocol the symbol 'default
;;   close-on-exec? if truish, socket will be close-on-exec. Defaults to 'close-on-exec
;; Returns an integer file descriptor
;; On errors, raises an exception
(define socket-fd
  (let ((c-socket-fd (foreign-procedure "c_socket_fd" (int int int ptr) int)))
    (case-lambda
      ((family type protocol close-on-exec?)
        (let ((family-int (hashtable-ref socket-family-name->number family #f))
              (type-int   (hashtable-ref socket-type-name->number type #f)))
          (assert* 'socket-fd family-int)
          (assert* 'socket-fd type-int)
          (assert* 'socket-fd (eq? protocol 'default))
        (let ((ret (c-socket-fd family-int type-int 0 close-on-exec?)))
          (if (>= ret 0)
            ret
            (raise-c-errno 'socket-fd 'socket ret)))))
      ((family type protocol)
        (socket-fd family type protocol 'close-on-exec))
      ((family type)
        (socket-fd family type 'default 'close-on-exec))
      ((family)
        (socket-fd family 'stream 'default 'close-on-exec)))))


;; Connect a socket to a remote address.
;; If socket is in blocking mode, blocks until connection either succeeds or fails.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;;   sockaddr a sockaddr object
;; On success, returns a truish value.
;; Otherwise:
;;   If system call is interrupted and returns -EINTR, retries.
;;   If socket is non-blocking mode and system call would block and returns -EAGAIN or -EINPROGRESS, returns #f.
;;   On other errors, raises an exception
(define socket-connect
  (let ((c-socket-connect (foreign-procedure "c_socket_connect" (int u8* size_t) int)))
    (lambda (socket sockaddr)
      (assert* 'socket-connect (sockaddr? sockaddr))
      (check-interrupts)
      (let* ((data (sockaddr-data sockaddr))
             (err  (c-socket-connect socket data (bytevector-length data))))
        (check-interrupts)
        (cond
          ((zero? err)
            (void))
          ((eqv? err c-errno-eintr)
            (socket-connect socket sockaddr))
          ((or (eqv? err c-errno-eagain) (eqv? err c-errno-einprogress))
            #f)
          (else
            (raise-c-errno 'socket-connect 'connect err socket sockaddr)))))))


;; Bind a socket to a local address.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;;   sockaddr a sockaddr object
;; Returns unspecified value.
;; On errors, raises an exception
(define socket-bind
  (let ((c-socket-bind (foreign-procedure "c_socket_bind" (int u8* size_t) int)))
    (lambda (socket sockaddr)
      (assert* 'socket-bind (sockaddr? sockaddr))
      (let* ((data (sockaddr-data sockaddr))
             (err  (c-socket-bind socket data (bytevector-length data))))
        (unless (zero? err)
          (raise-c-errno 'socket-bind 'bind err socket sockaddr))))))


;; Listen on a socket.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;; Optional arguments:
;;   backlog  the integer length of pending incoming connections queue. Defaults to 3.
;; Returns unspecified value.
;; On errors, raises an exception
(define socket-listen
  (let ((c-socket-listen (foreign-procedure "c_socket_listen" (int int) int)))
    (case-lambda
      ((socket backlog)
        (let ((err (c-socket-listen socket backlog)))
          (unless (zero? err)
            (raise-c-errno 'socket-listen 'listen err socket backlog))))
      ((socket)
        (socket-listen socket 3)))))


;; Accept an incoming connection to a listening socket.
;; If socket is in blocking mode, blocks until an incoming connection is available.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open, listening socket
;;
;; On success, returns file descriptor of newly accepted socket.
;; Otherwise:
;;   If system call is interrupted and returns -EINTR, retries.
;;   If socket is non-blocking mode and system call would block and returns -EAGAIN, returns #f.
;;   On other errors, raises an exception
(define socket-accept
  (let ((c-socket-accept (foreign-procedure __collect_safe "c_socket_accept" (int) int)))
    (lambda (socket)
      (check-interrupts)
      (let ((ret (c-socket-accept socket)))
        (check-interrupts)
        (cond
          ((>= ret 0)
            ret)
          ((eqv? ret c-errno-eintr)
            (socket-accept socket))
          ((eqv? ret c-errno-eagain)
            #f)
          (else
            (raise-c-errno 'socket-accept 'accept ret socket)))))))


;; create a pair of mutually connected AF_UNIX socket file descriptors.
;; Optional arguments:
;;   fd1-close-on-exec? if truish the first socket will be close-on-exec. Defaults to 'close-on-exec
;;   fd2-close-on-exec? if truish the second socket will be close-on-exec. Defaults to 'close-on-exec
;; Returns two file descriptors:
;;   the first socket
;;   the second socket
;; On errors, raises an exception
(define socketpair-fds
  (let ((c-socketpair-fds (foreign-procedure "c_socketpair_fds" (ptr ptr) ptr)))
    (case-lambda
      ((fd1-close-on-exec? fd2-close-on-exec?)
        (let ((ret (c-socketpair-fds fd1-close-on-exec? fd2-close-on-exec?)))
          (if (pair? ret)
            (values (car ret) (cdr ret))
            (raise-c-errno 'socketpair-fds 'socketpair ret))))
      (()
        (socketpair-fds 'close-on-exec 'close-on-exec)))))

) ; close library
