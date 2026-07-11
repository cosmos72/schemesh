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
(library (scheme2k ipc wire (1 0 1))
  (export wire-pipe-pair wire-socketpair-pair
          wire-shm? wire-shm-open wire-shm-close wire-shm-insert! wire-shm-delete!)
  (import
    (rnrs)
    (only (chezscheme)                 box foreign-procedure record-writer)
    (only (scheme2k posix base)        c-errno-einval raise-c-errno)
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


(define-record-type wire-shm
  (fields
    (mutable c-handle))
  (nongenerative wire-shm-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; return #t if obj is an exact integer representable by a C size_t
;; otherwise return #f
(define (c-size-t? obj)
  (and (integer? obj) (exact? obj)
       (<= 0 obj (if (fixnum? #xffffffff) #xffffffffffffffff #xffffffff))))


;; optional arguments:
;;   length: the number of shared-memory bytes to allocate. Default is 262144
;;
;; return a "wire-shm" object suitable for calling other (wire-shm...) functions,
;; or < 0 on error.
;; never throws
(define wire-shm-open
  (let ((c-shm-open (foreign-procedure __collect_safe "c_shm_open" (size_t) ptr)))
    (case-lambda
      ((length)
        (let ((ret (and (c-size-t? length) (c-shm-open length))))
          (cond
            ((not (and (integer? ret) (exact? ret)))
               c-errno-einval)
            ((> ret 0)
               (make-wire-shm ret))
            ((< ret 0)
               ret)
            (else
               c-errno-einval))))
      (()
        (wire-shm-open 262144)))))


;; arguments:
;;   wire-shm: obtained with (wire-shm-open)
;;
;; return 0 on success, or < 0 on error
;; never throws
(define wire-shm-close
  (let ((c-shm-close (foreign-procedure __collect_safe "c_shm_close" (void*) int)))
    (lambda (shm)
      (if (wire-shm? shm)
        (let ((handle (wire-shm-c-handle shm)))
          (wire-shm-c-handle-set! shm 0)
          (if (zero? handle)
            0
            (c-shm-close handle)))
        c-errno-einval))))


;; arguments:
;;   wire-shm: obtained with (wire-shm-open)
;;   key:      unsigned exact integer, maximum is 2^64 - 1
;;   value:    bytevector, usually created with (datum->wire obj)
;;
;; return 0 on success, or < 0 on error
;; never throws
(define wire-shm-insert!
  (let ((c-shm-insert! (foreign-procedure "c_shm_insert" (void* unsigned-64 ptr) int)))
    (lambda (shm key value)
      (if (and (wire-shm? shm) (integer? key) (exact? key) (<= 0 key #xffffffffffffffff))
        (c-shm-insert! (wire-shm-c-handle shm) key value)
        c-errno-einval))))



;; arguments:
;;   wire-shm: obtained with (wire-shm-open)
;;
;; returns two values:
;;   either one of the previously inserted key and value,
;;   or #f and #f if no entries are currently available
;;   or #f and < 0 on errors
;; never throws
(define wire-shm-delete!
  (let ((c-shm-lock          (foreign-procedure "c_shm_lock" (void*) int))
        (c-shm-unlock        (foreign-procedure "c_shm_unlock" (void*) int))
        (c-shm-locked-delete (foreign-procedure "c_shm_locked_delete" (void*) ptr)))
    (lambda (shm)
      (if (wire-shm? shm)
        (let ((c-handle (wire-shm-c-handle shm))
              (ret '())
              (locked? #f))
          (dynamic-wind
            (lambda ()
              (let ((err (c-shm-lock c-handle)))
                (set! locked? (eqv? 0 err))
                (set! ret err)))
            (lambda ()
              (when locked?
                (set! ret (c-shm-locked-delete c-handle))))
            (lambda ()
              (when locked?
                (c-shm-unlock c-handle))))
          (cond
            ((pair? ret)
              (values (car ret) (cdr ret)))
            ((null? ret)
              (values #f #f))
            ((and (integer? ret) (exact? ret) (< ret 0))
              (values #f ret))
            (else
              (values #f c-errno-einval))))
        (values #f c-errno-einval)))))


(record-writer (record-type-descriptor wire-shm)
  (lambda (shm port writer)
    (put-string port "#<wire-shm #x")
    (put-string port (number->string (wire-shm-c-handle shm) 16))
    (put-string port ">")))


) ; close library
