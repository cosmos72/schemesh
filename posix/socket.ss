;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix socket (0 9 3))
  (export
    make-endpoint endpoint-type endpoint endpoint?
    endpoint-address endpoint-bytes endpoint-family endpoint-port endpoint-constructor
    hostname->endpoint hostname->endpoint-list url->endpoint url->endpoint-list url-split
    socket-fd socket-connect socket-bind socket-listen socket-accept socket-endpoint socket-peer-endpoint
    socketpair-fds)
  (import
    (rnrs)
    (only (chezscheme)                     bytevector->immutable-bytevector foreign-procedure procedure-arity-mask
                                           fx1+ record-writer string->immutable-string void)
    (only (scheme2k bootstrap)             assert* check-interrupts raise-assert* raise-errorf)
    (only (scheme2k conversions)           bytevector->bytevector0 text->bytevector text? text->bytevector0 text->string)
    (only (scheme2k containers bytevector) bytevector-index)
    (only (scheme2k containers bytespan)   bytevector->bytespan*)
    (only (scheme2k containers hashtable)  alist->eqv-hashtable eq-hashtable hashtable-transpose)
    (only (scheme2k containers utf8b)      utf8b->string)
    (only (scheme2k posix fd)              raise-c-errno))


;; generic socket endpoint
(define-record-type (endpoint-type make-endpoint endpoint?)
  (fields
    (immutable family  endpoint-family)  ; symbol: one of 'inet 'inet6 'unix ...
    (immutable address endpoint-address) ; immutable string. one of:
                                         ;   IPv4 addess in dotted decimal notation "ddd.ddd.ddd.ddd"
                                         ;   IPv6 addess in colon-separated hexadecimal notation "xxxx:xxxx:..."
                                         ;   unix socket path
    (immutable port    endpoint-port)    ; unsigned-16 TCP or UDP port, or -1
    (immutable bytes   endpoint-bytes))  ; immutable bytevector containing C sockaddr_* bytes
  (protocol
    (lambda (new)
      (lambda (family address port bytes)
        (assert* 'make-endpoint (symbol? family))
        (assert* 'make-endpoint (bytevector? bytes))
        (assert* 'make-endpoint (fixnum? port))
        (assert* 'make-endpoint (fx<=? -1 port 65535))
        (new family
             (string->immutable-string (text->string address))
             port
             (bytevector->immutable-bytevector bytes)))))
  (nongenerative endpoint-type-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define table-socket-family-number->name
  (alist->eqv-hashtable ((foreign-procedure "c_socket_family_list" () ptr))))

(define table-socket-family-name->number
  (hashtable-transpose table-socket-family-number->name (make-eq-hashtable)))

(define table-socket-type-number->name
  (alist->eqv-hashtable ((foreign-procedure "c_socket_type_list" () ptr))))

(define table-socket-type-name->number
  (hashtable-transpose table-socket-type-number->name (make-eq-hashtable)))


(define (socket-family-number->name number)
  (hashtable-ref table-socket-family-number->name number 'unknown))


(define raise-c-hostname-error
  (let ((c-hostname-error->string (foreign-procedure "c_hostname_error_to_string" (int) ptr)))
    (lambda (who c-who c-error hostname family service)
      (raise-errorf who "C function ~s~s failed with error ~s: ~a"
        c-who (list (if (text? hostname) (text->string hostname) hostname)
                    family
                    (if (text? service)  (text->string service) service))
        c-error (if (integer? c-error) (c-hostname-error->string c-error) "unknown error")))))


(define c-hostname->endpoint      (foreign-procedure "c_hostname_to_endpoint"      (ptr int ptr) ptr))
(define c-hostname->endpoint-list (foreign-procedure "c_hostname_to_endpoint_list" (ptr int ptr) ptr))

(define (%vector->endpoint v)
  (let ((family (socket-family-number->name (vector-ref v 0))))
    (make-endpoint family (vector-ref v 1) (vector-ref v 2) (vector-ref v 3))))


(define (uint16? obj)
  (and (fixnum? obj) (fx<=? 0 obj 65535)))


;; resolve specified hostname and service to a single IPv4 or IPv6 endpoint.
;; both hostname and service must be bytevector, string, bytespan or charspan.
;; service may also be #f or a fixnum in 0 ... 65535
;;
;; Return an endpoint
;; On errors, raise condition
(define hostname->endpoint
  (case-lambda
    ((hostname service preferred-socket-family)
      (assert* 'hostname->endpoint (text? hostname))
      (when (and service (not (uint16? service)))
        (assert* 'hostname->endpoint (text? service)))
      (when preferred-socket-family
        (assert* 'hostname->endpoint (symbol? preferred-socket-family)))
      (let* ((hostname0  (text->bytevector0 hostname))
             (service0   (if (text? service) (text->bytevector0 service) service))
             (family-int (hashtable-ref table-socket-family-name->number preferred-socket-family 0))
             (item       (c-hostname->endpoint hostname0 family-int service0)))
        (unless (vector? item)
          (raise-c-hostname-error 'hostname->endpoint 'getaddrinfo item hostname preferred-socket-family service))
        (%vector->endpoint item)))
    ((hostname service)
      (hostname->endpoint hostname service #f))
    ((hostname)
      (hostname->endpoint hostname #f #f))))


;; resolve specified hostname and service to list of IPv4 and/or IPv6 endpoints.
;; both hostname and service must be bytevector, string, bytespan or charspan.
;; service may also be #f or a fixnum in 0 ... 65535
;;
;; Return a list of endpoints
;; On errors, raise condition
(define hostname->endpoint-list
  (case-lambda
    ((hostname service preferred-socket-family)
      (assert* 'hostname->endpoint-list (text? hostname))
      (when (and service (not (uint16? service)))
        (assert* 'hostname->endpoint-list (text? service)))
      (when preferred-socket-family
        (assert* 'hostname->endpoint-list (symbol? preferred-socket-family)))
      (let* ((hostname0  (text->bytevector0 hostname))
             (service0   (if (text? service) (text->bytevector0 service) service))
             (family-int (hashtable-ref table-socket-family-name->number preferred-socket-family 0))
             (l          (c-hostname->endpoint-list hostname0 family-int service0)))
        (unless (or (null? l) (pair? l))
          (raise-c-hostname-error 'hostname->endpoint-list 'getaddrinfo l hostname preferred-socket-family service))
        (let %endpoint-list ((tail l) (ret '()))
          (if (null? tail)
            ret
            ;; unreverse list returned by C
            (%endpoint-list (cdr tail) (cons (%vector->endpoint (car tail)) ret))))))
    ((hostname service)
      (hostname->endpoint-list hostname service #f))
    ((hostname)
      (hostname->endpoint-list hostname #f #f))))


;; convert URL to endpoint, possibly resolving the hostname contained in the URL
(define url->endpoint
  (case-lambda
    ((url preferred-socket-family)
      (let-values (((hostname service) (url-split url)))
        (hostname->endpoint hostname service preferred-socket-family)))
    ((url)
      (url->endpoint url #f))))


;; convert URL to endpoint list, possibly resolving the hostname contained in the URL
(define url->endpoint-list
  (case-lambda
    ((url preferred-socket-family)
      (let-values (((hostname service) (url-split url)))
        (hostname->endpoint-list hostname service preferred-socket-family)))
    ((url)
      (url->endpoint-list url #f))))


;; common implementation of (url->endpoint) and (url->endpoint-list)
(define (url-split url)
  (let* ((url (text->bytevector url))
         (len (bytevector-length url)))
    (let-values (((protocol-start protocol-end) (parse-url-protocol url len)))
      (let-values (((host-start host-end)       (parse-url-hostname url protocol-end len)))
        (let-values (((port-start port-end)     (parse-url-port     url host-end len)))
          (values
            (bytevector->bytespan* url host-start host-end)
            (cond
              ((fx<? port-start port-end)
                (bytevector->bytespan* url port-start port-end))
              ((fx<? protocol-start protocol-end)
                (bytevector->bytespan* url protocol-start protocol-end))
              (else
                #f))))))))

(define (parse-url-protocol bv len)
  (values 0 (or (bytevector-index bv 58) 0)))

(define (skip-prefix/u8 bv start end u8)
  (if (and (fx<? start end)
           (fx=? u8 (bytevector-u8-ref bv start)))
    (fx1+ start) ; skip u8
    start))

(define (parse-url-hostname bv start end)
  (if (fx>=? start end)
    (values start end)
    (let* ((start (skip-prefix/u8 bv start end 58)) ; skip ":"
           (start (skip-prefix/u8 bv start end 47)) ; skip "/"
           (start (skip-prefix/u8 bv start end 47)) ; skip "/"
           (slash (bytevector-index bv start end 47))  ; #\/
           (colon (bytevector-index bv start end 58))  ; #\:
           (bra   (bytevector-index bv start end 91))  ; #\[
           (ket   (bytevector-index bv start end 93))) ; #\]
      (if (and bra ket (fx=? start bra) (fx<? bra ket (or slash end)))
        (values (fx1+ bra) ket) ;; IPv6 address in brackets
        (values start (if (and slash colon) (fxmin slash colon) (or slash colon end)))))))

(define (parse-url-port bv start end)
  (if (fx>=? start end)
    (values start end)
    (let* ((start (skip-prefix/u8 bv start end 93)) ; skip "]"
           (start (skip-prefix/u8 bv start end 58)) ; skip ":"
           (slash (bytevector-index bv start end 47))  ; #\/
           (quest (bytevector-index bv start end 63))) ; #\?
      (values start (if (and slash quest) (fxmin slash quest) (or slash quest end))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; create and return an INET (i.e. IPv4) socket endpoint
;; address must be one of:
;;   - a bytevector, string, bytespan or charspan containing decimal dotted notation, as for example "127.0.0.1"
;;   - (unimplemented) a 32-bit unsigned integer, as for example #x7f000001
;; port must be a 16-bit unsigned integer
;; On errors, raise condition
(define make-endpoint-inet
  (let ((c-endpoint-inet (foreign-procedure "c_endpoint_inet"  (u8* unsigned-16) ptr)))
    (lambda (address port)
      (assert* 'endpoint-inet (fixnum? port))
      (assert* 'endpoint-inet (fx<=? 0 port 65535))
      (let* ((address-bv0 (text->bytevector0 address))
             (bytes       (c-endpoint-inet address-bv0 port)))
        (make-endpoint 'inet address port bytes)))))


;; create and return an INET6 (i.e. IPv6) socket endpoint
;; address must be one of:
;;   - a bytevector, string, bytespan or charspan containing either
;;        -- hexadecimal notation, as for example "::ffff:0001"
;;        -- an IPv4-mapped IPv6 address, as for example "::ffff:204.152.189.116"
;;   - (unimplemented) a 128-bit unsigned integer, as for example #xffff0001000200030004000500060007
;; port must be a 16-bit unsigned integer
;; On errors, raise condition
(define make-endpoint-inet6
  (let ((c-endpoint-inet6 (foreign-procedure "c_endpoint_inet6"  (u8* unsigned-16) ptr)))
    (lambda (address port)
      (assert* 'endpoint-inet6 (fixnum? port))
      (assert* 'endpoint-inet6 (fx<=? 0 port 65535))
      (let* ((address-bv0 (text->bytevector0 address))
             (bytes       (c-endpoint-inet6 address-bv0 port)))
        (make-endpoint 'inet6 address port bytes)))))


;; create and return a UNIX socket endpoint.
;; path must be a bytevector, string, bytespan or charspan
;; On errors, raise condition
(define make-endpoint-unix
  (let ((c-endpoint-unix           (foreign-procedure "c_endpoint_unix"  (ptr) ptr))
        (c-endpoint-unix-path-max ((foreign-procedure "c_endpoint_unix_path_max" () size_t))))
    (lambda (path)
      (let ((path-bv  (text->bytevector path)))
        (assert* 'make-endpoint-unix (fx<? (bytevector-length path-bv) c-endpoint-unix-path-max))
        (let ((bytes (c-endpoint-unix path-bv)))
          (make-endpoint 'unix path -1 bytes))))))


;; hashtable family -> procedure for creating endpoint of such family
(define table-endpoint-constructor
  (eq-hashtable 'inet make-endpoint-inet 'inet6 make-endpoint-inet6 'unix make-endpoint-unix))


;; set or return procedure for creating endpoint for specified family
;; Raise condition if procedure for family is not found
(define endpoint-constructor
  (case-lambda
    ((family)
      ;; return procedure for creating endpoint for family
      (let ((constructor (hashtable-ref table-endpoint-constructor family #f)))
        (unless (procedure? constructor)
          (raise-assert* 'endpoint "(endpoint-constructor family)" family))
        constructor))
    ((family constructor)
      ;; set procedure for creating endpoint for family
      (assert* 'endpoint-constructor (symbol? family))
      (assert* 'endpoint-constructor (procedure? constructor))
      ;; procedure must accept 1 or more args i.e. not zero
      (assert* 'endpoint-constructor (> (procedure-arity-mask constructor) 1))
      (hashtable-set! table-endpoint-constructor family constructor))))


;; create and return an endpoint
;; On errors, raise condition
(define endpoint
  (case-lambda
    ((family path)
      (let ((e ((endpoint-constructor family) path)))
        (assert* 'endpoint (endpoint? e))
        e))
    ((family address port)
      (let ((e ((endpoint-constructor family) address port)))
        (assert* 'endpoint (endpoint? e))
        e))
    ((family arg1 arg2 arg3 . args)
      (let ((e (apply (endpoint-constructor family) arg1 arg2 arg3 args)))
        (assert* 'endpoint (endpoint? e))
        e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let ((c-socket-fd (foreign-procedure "c_socket_fd" (int int int ptr) int)))
    (case-lambda
      ((family type protocol close-on-exec?)
        (let ((family-int (hashtable-ref table-socket-family-name->number family #f))
              (type-int   (hashtable-ref table-socket-type-name->number type #f)))
          (assert* 'socket-fd family-int)
          (assert* 'socket-fd type-int)
          (assert* 'socket-fd (eq? protocol 'default))
        (let ((ret (c-socket-fd family-int type-int 0 close-on-exec?)))
          (if (>= ret 0)
            ret
            (raise-c-errno 'socket-fd 'socket ret family type protocol close-on-exec?)))))
      ((family type protocol)
        (socket-fd family type protocol 'close-on-exec))
      ((family type)
        (socket-fd family type 'default 'close-on-exec))
      ((family)
        (socket-fd family 'stream 'default 'close-on-exec)))))


;; Connect a socket to an endpoint.
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
    (lambda (socket e)
      (assert* 'socket-connect (endpoint? e))
      (check-interrupts)
      (let* ((bytes (endpoint-bytes e))
             (err   (c-socket-connect socket bytes (bytevector-length bytes))))
        (cond
          ((zero? err)
            (void))
          ((eqv? err c-errno-eintr)
            (socket-connect socket e))
          ((or (eqv? err c-errno-eagain) (eqv? err c-errno-einprogress))
            #f)
          (else
            (raise-c-errno 'socket-connect 'connect err socket e)))))))


;; Bind a socket to a local address.
;; Mandatory arguments:
;;   socket   an integer file descriptor corresponding to an open socket
;;   endpoint an endpoint object
;; Return unspecified value.
;; On errors, raise condition
(define socket-bind
  (let ((c-socket-bind (foreign-procedure "c_socket_bind" (int u8* size_t) int)))
    (lambda (socket e)
      (assert* 'socket-bind (endpoint? e))
      (let* ((bytes (endpoint-bytes e))
             (err   (c-socket-bind socket bytes (bytevector-length bytes))))
        (unless (zero? err)
          (raise-c-errno 'socket-bind 'bind err socket e))))))


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
        (cond
          ((>= ret 0)
            ret)
          ((eqv? ret c-errno-eintr)
            (socket-accept socket))
          ((eqv? ret c-errno-eagain)
            #f)
          (else
            (raise-c-errno 'socket-accept 'accept ret socket)))))))


(define socket-endpoint2
  (let ((c-socket-endpoint2 (foreign-procedure "c_socket_endpoint2" (int int) ptr)))
    (lambda (socket peer?)
      (let ((vec (c-socket-endpoint2 socket (if peer? 1 0))))
        (if (vector? vec)
          (let ((family (socket-family-number->name (vector-ref vec 0))))
            (make-endpoint family (vector-ref vec 1) (vector-ref vec 2) (vector-ref vec 3)))
          (raise-c-errno
            (if peer? 'socket-peer-endpoint 'socket-endpoint)
            (if peer? 'getpeername 'getsockname)
            vec socket))))))


;; return the endpoint of a bound or connected socket
(define (socket-endpoint socket)
  (socket-endpoint2 socket #f))


;; return the peer's endpoint of a bound or connected socket
(define (socket-peer-endpoint socket)
  (socket-endpoint2 socket 'peer))


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


;; customize how "endpoint-type" objects are printed
(record-writer (record-type-descriptor endpoint-type)
  (lambda (e out writer)
    (display "(endpoint '" out)
    (write (endpoint-family e) out)
    (display #\space out)
    (write (endpoint-address e) out)
    (let ((port (endpoint-port e)))
      (when (and (fixnum? port) (fx<=? 0 port 65535))
        (display #\space out)
        (display port out)))
    (display ")" out)))


) ; close library
