;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix socket (0 9 2))
  (export
    make-endpoint endpoint endpoint? endpoint-bytes endpoint-family endpoint-port endpoint->text
    socket-fd socket-connect socket-bind socket-listen socket-accept socket-endpoint socket-peer-endpoint
    socketpair-fds)
  (import
    (rnrs)
    (only (chezscheme)                    bytevector->immutable-bytevector foreign-procedure procedure-arity-mask record-writer void)
    (only (scheme2k bootstrap)            assert* check-interrupts)
    (only (scheme2k conversions)          text->bytevector text->bytevector0)
    (only (scheme2k containers utf8b)     utf8b->string)
    (only (scheme2k containers hashtable) alist->eqv-hashtable eq-hashtable hashtable-transpose)
    (only (scheme2k posix fd)             raise-c-errno))


(define-record-type (%endpoint %make-endpoint endpoint?)
  (fields
    (immutable family  endpoint-family) ; a symbol among 'inet 'inet6 'unix ...
    (immutable bytes   endpoint-bytes))  ; an immutable bytevector containing C sockaddr_* bytes
  (nongenerative endpoint-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (make-endpoint family bytes)
  (assert* 'make-endpoint (symbol? family))
  (assert* 'make-endpoint (bytevector? bytes))
  (%make-endpoint family (bytevector->immutable-bytevector bytes)))


;; return the port stored in specified endpoint
;; On errors, raise condition
(define endpoint-port
  (let ((c-endpoint-port (foreign-procedure "c_sockaddr_port" (ptr) int)))
    (lambda (endpoint)
      (let ((ret (c-endpoint-port (endpoint-bytes endpoint))))
        (unless (fx<=? 0 ret 65535)
          (raise-c-errno 'endpoint->port 'c_sockaddr_port ret endpoint))
        ret))))


;; return a newly allocated string containing the textual representation of address stored in endpoint:
;; - if family is 'inet, return IPv4 network address in dotted-decimal format, "ddd.ddd.ddd.ddd"
;; - if family is 'inet6, return IPv6 network address in colon-separated hexadecimal format "xxxx:xxxx:..."
;;     or in IPv6-mapped IPv4 format "xxxx:xxxx:...:ddd.ddd.ddd.ddd"
;; - if family is 'unix, return unix path
;; On errors, raise condition
(define endpoint->text
  (let ((c-endpoint-to-data (foreign-procedure "c_sockaddr_to_text" (ptr) ptr)))
    (lambda (endpoint)
      (let ((bv (c-endpoint-to-data (endpoint-bytes endpoint))))
        (unless (bytevector? bv)
          (raise-c-errno 'endpoint->text 'inet_ntop bv endpoint))
        (utf8b->string bv)))))

  

(define socket-family-number->name
  (alist->eqv-hashtable ((foreign-procedure "c_socket_family_list" () ptr))))

(define socket-family-name->number
  (hashtable-transpose socket-family-number->name (make-eqv-hashtable)))

(define socket-type-number->name
  (alist->eqv-hashtable ((foreign-procedure "c_socket_type_list" () ptr))))

(define socket-type-name->number
  (hashtable-transpose socket-type-number->name (make-eqv-hashtable)))


(define c-sockaddr-unix-path-max ((foreign-procedure "c_sockaddr_unix_path_max" () size_t)))


;; create an INET socket address.
;; ipaddr must be one of:
;;   - a bytevector, string, bytespan or charspan containing decimal dotted notation, as for example "127.0.0.1"
;;   - (unimplemented) a 32-bit unsigned integer, as for example #x7f000001
;; port must be a 16-bit unsigned integer
;; On errors, raise condition
(define endpoint-inet
  (let ((c-sockaddr-inet (foreign-procedure "c_sockaddr_inet" (u8* unsigned-16) ptr)))
    (lambda (ipaddr port)
      (assert* 'endpoint-inet (fixnum? port))
      (assert* 'endpoint-inet (fx<=? 0 port 65535))
      (let* ((bytes (c-sockaddr-inet (text->bytevector0 ipaddr) port)))
        (unless (bytevector? bytes)
          (raise-c-errno 'endpoint-inet 'c_sockaddr_inet bytes ipaddr port))
        (make-endpoint 'inet bytes)))))


;; create an INET6 socket address.
;; ipaddr6 must be one of:
;;   - a bytevector, string, bytespan or charspan containing either
;;        -- hexadecimal notation, as for example "::ffff:0001"
;;        -- an IPv4-mapped IPv6 address, as for example "::ffff:204.152.189.116"
;;   - (unimplemented) a 128-bit unsigned integer, as for example #xffff0001000200030004000500060007
;; port must be a 16-bit unsigned integer
;; On errors, raise condition
(define endpoint-inet6
  (let ((c-sockaddr-inet6 (foreign-procedure "c_sockaddr_inet6" (u8* unsigned-16) ptr)))
    (lambda (ipaddr6 port)
      (assert* 'endpoint-inet6 (fixnum? port))
      (assert* 'endpoint-inet6 (fx<=? 0 port 65535))
      (let ((bytes (c-sockaddr-inet6 (text->bytevector0 ipaddr6) port)))
        (unless (bytevector? bytes)
          (raise-c-errno 'endpoint-inet6 'c_sockaddr_inet6 bytes ipaddr6 port))
        (make-endpoint 'inet6 bytes)))))


;; create a UNIX socket address. path must be a bytevector, string, bytespan or charspan
;; On errors, raise condition
(define endpoint-unix
  (let ((c-sockaddr-unix (foreign-procedure "c_sockaddr_unix" (ptr) ptr)))
    (lambda (path)
      (let ((path (text->bytevector path)))
        (assert* 'endpoint-unix (fx<? (bytevector-length path) c-sockaddr-unix-path-max))
        (let ((bytes (c-sockaddr-unix path)))
          (unless (bytevector? bytes)
            (raise-c-errno 'endpoint-unix 'c-sockaddr-unix bytes path))
          (make-endpoint 'unix bytes))))))


;; return hashtable family -> procedure of known endpoint constructors
(define endpoint-constructors
  (let ((htable (eq-hashtable 'inet endpoint-inet 'inet6 endpoint-inet6 'unix endpoint-unix)))
    (lambda ()
      htable)))


;; return procedure to construct endpoint for specified family
(define (endpoint-constructor family)
  (let ((constructor (hashtable-ref (endpoint-constructors) family #f)))
    (assert* 'endpoint (procedure? constructor))
    constructor))


;; create and return a socket address
;; On errors, raise condition
(define endpoint
  (case-lambda
    ((family ipaddr port)
      ((endpoint-constructor family) ipaddr port))
    ((family path)
      ((endpoint-constructor family) path))))


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
;; Return an integer file descriptor
;; On errors, raise condition
(define socket-fd
  (let ((c_socket_fd (foreign-procedure "c_socket_fd" (int int int ptr) int)))
    (case-lambda
      ((family type protocol close-on-exec?)
        (let ((family-int (hashtable-ref socket-family-name->number family #f))
              (type-int   (hashtable-ref socket-type-name->number type #f)))
          (assert* 'socket-fd family-int)
          (assert* 'socket-fd type-int)
          (assert* 'socket-fd (eq? protocol 'default))
        (let ((ret (c_socket_fd family-int type-int 0 close-on-exec?)))
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
;;   endpoint an endpoint object
;; On success, returns a truish value.
;; Otherwise:
;;   If system call is interrupted and returns -EINTR, retries.
;;   If socket is non-blocking mode and system call would block and returns -EAGAIN or -EINPROGRESS, returns #f.
;;   On other errors, raise condition
(define socket-connect
  (let ((c-socket-connect (foreign-procedure "c_socket_connect" (int u8* size_t) int)))
    (lambda (socket endpoint)
      (assert* 'socket-connect (endpoint? endpoint))
      (check-interrupts)
      (let* ((bytes (endpoint-bytes endpoint))
             (err   (c-socket-connect socket bytes (bytevector-length bytes))))
        (check-interrupts)
        (cond
          ((zero? err)
            (void))
          ((eqv? err c-errno-eintr)
            (socket-connect socket endpoint))
          ((or (eqv? err c-errno-eagain) (eqv? err c-errno-einprogress))
            #f)
          (else
            (raise-c-errno 'socket-connect 'connect err socket endpoint)))))))


;; Bind a socket to a local address.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;;   endpoint an endpoint object
;; Return unspecified value.
;; On errors, raise condition
(define socket-bind
  (let ((c-socket-bind (foreign-procedure "c_socket_bind" (int u8* size_t) int)))
    (lambda (socket endpoint)
      (assert* 'socket-bind (endpoint? endpoint))
      (let* ((bytes (endpoint-bytes endpoint))
             (err   (c-socket-bind socket bytes (bytevector-length bytes))))
        (unless (zero? err)
          (raise-c-errno 'socket-bind 'bind err socket endpoint))))))


;; Listen on a socket.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;; Optional arguments:
;;   backlog  the integer length of pending incoming connections queue. Defaults to 3.
;; Return unspecified value.
;; On errors, raise condition
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
;;   On other errors, raise condition
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


(define socket-sockaddr2
  (let ((c-socket-sockaddr2 (foreign-procedure "c_socket_sockaddr2" (int int) ptr)))
    (lambda (socket peer?)
      (let ((ret (c-socket-sockaddr2 socket (if peer? 1 0))))
        (if (pair? ret)
          (let ((family (hashtable-ref socket-family-number->name (car ret) 'unknown)))
            (make-endpoint family (cdr ret)))
          (raise-c-errno
            (if peer? 'socket-peer-endpoint 'socket-endpoint)
            (if peer? 'getpeername 'getsockname)
            ret socket))))))


(define (socket-endpoint socket)
  (socket-sockaddr2 socket #f))


(define (socket-peer-endpoint socket)
  (socket-sockaddr2 socket 'peer))


;; create a pair of mutually connected AF_UNIX socket file descriptors.
;; Optional arguments:
;;   fd1-close-on-exec? if truish the first socket will be close-on-exec. Defaults to 'close-on-exec
;;   fd2-close-on-exec? if truish the second socket will be close-on-exec. Defaults to 'close-on-exec
;; Return two file descriptors:
;;   the first socket
;;   the second socket
;; On errors, raise condition
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


;; customize how "endpoint" objects are printed
(record-writer (record-type-descriptor %endpoint)
  (lambda (e port writer)
    (display "(endpoint '" port)
    (write (endpoint-family e) port)
    (display #\space port)
    (write (endpoint->text e) port)
    (when (memq (endpoint-family e) '(inet inet6))
      (display #\space port)
      (display (endpoint-port e) port))
    (display ")" port)))

) ; close library
