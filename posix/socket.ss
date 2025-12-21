;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix socket (0 9 2))
  (export
    socket-fd socketpair-fds)
  (import
    (rnrs)
    (only (chezscheme)             foreign-procedure procedure-arity-mask)
    (only (scheme2k bootstrap)     assert*)
    (only (scheme2k containers hashtable) alist->eq-hashtable hashtable-transpose)
    (only (scheme2k posix fd)      raise-c-errno))


;; create a pair of mutually connected AF_UNIX socket file descriptors.
;; Optional arguments:
;;   fd1-close-on-exec? if truish the first socket will be close-on-exec. Defaults to 'close-on-exec
;;   fd2-close-on-exec? if truish the second socket will be close-on-exec. Defaults to 'close-on-exec
;; Returns two file descriptors:
;;   the first socket
;;   the second socket
;; On errors, raises an exception
(define socketpair-fds
  (let ((c-socketpair-fds (foreign-procedure "c_open_socketpair_fds" (ptr ptr) ptr)))
    (case-lambda
      ((fd1-close-on-exec? fd2-close-on-exec?)
        (let ((ret (c-socketpair-fds fd1-close-on-exec? fd2-close-on-exec?)))
          (if (pair? ret)
            (values (car ret) (cdr ret))
            (raise-c-errno 'socketpair-fds 'socketpair ret))))
      (()
        (socketpair-fds 'close-on-exec 'close-on-exec)))))

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
(define socket-fd
  (let ((c-socket-fd (foreign-procedure "c_open_socket_fd" (int int int ptr) int)))
    (case-lambda
      ((domain type protocol close-on-exec?)
        (let ((domain-int (hashtable-ref socket-domain-name->number domain #f))
              (type-int   (hashtable-ref socket-type-name->number type #f)))
          (assert* 'socket-fd domain-int)
          (assert* 'socket-fd type-int)
          (assert* 'socket-fd (eq? protocol 'default))
        (let ((ret (c-socket-fd domain-int type-int 0 close-on-exec?)))
          (if (>= ret 0)
            ret
            (raise-c-errno 'socket-fd 'socket ret)))))
      ((domain type protocol)
        (socket-fd domain type protocol 'close-on-exec))
      ((domain type)
        (socket-fd domain type 'default 'close-on-exec)))))

) ; close library
