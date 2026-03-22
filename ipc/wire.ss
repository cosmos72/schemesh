;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; inter-process communication library:
;;;
;;; exchanges serialized data through binary ports or file descriptors (sockets, pipes ...).
;;;
;;; data is serialized/deserialized with library (scheme2k io wire)
;;;
(library (scheme2k ipc wire (1 0 0))
  (export wire-pipe-pair wire-socketpair-pair)
  (import
    (rnrs)
    (only (chezscheme)                 box)
    (only (scheme2k posix fd)          pipe-fds)
    (only (scheme2k posix socket)      socketpair-fds)
    (only (scheme2k io wire)           make-wire-reader make-wire-writer))


;; create and return a wire-reader and a wire-writer:
;;   the wire-reader reads serialized data from the read side of a newly created pipe file descriptor,
;;   the wire-writer writes serialized data to the write side of the same pipe (which is a different file descriptor).
;;
;; the returned wire-reader and wire-writer take ownership of the created pipe file descriptors,
;; and closing one of them closes the corresponding pipe file descriptor.
(define (wire-pipe-pair)
  (let-values (((in out) (pipe-fds #t #t))) ;; mark fds close-on-exec
    (values (make-wire-reader in  #t)
            (make-wire-writer out #t))))


;; create and return two wire-reader and two wire-writer, in the following order:
;;   wire-receiver1 reads  serialized data from the first socket file descriptor of a socket pair
;;   wire-sender1   writes serialized data to the same socket file descriptor as wire-receiver1
;;   wire-receiver2 reads  serialized data from the second socket file descriptor of the same socket pair
;;   wire-sender2   writes serialized data to the same socket file descriptor as wire-receiver2
;;
;; for bidirectional communication between two subsystems,
;;   give wire-receiver1 and wire-sender1 to one subsystem,
;;   and give wire-receiver2 and wire-sender2 to the other subsystem.
;;
;; the returned wire-readers and wire-writers take ownership of the created socket file descriptors,
;; and closing one of them closes the corresponding socket file descriptor.
(define (wire-socketpair-pair)
  (let-values (((fd1 fd2) (socketpair-fds #t #t))) ; mark fds close-on-exec
    (let ((box1 (box fd1))
          (box2 (box fd2)))
      ;; give the same box to each reader and writer pair sharing a single fd,
      ;; so that closing one also closes the other
      (values (make-wire-reader box1 #t)
              (make-wire-writer box1 #t)
              (make-wire-reader box2 #t)
              (make-wire-writer box2 #t)))))


) ; close library
