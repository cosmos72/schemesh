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
(library (scheme2k ipc wire (0 9 3))
  (export wire-reader wire-reader? make-wire-reader wire-reader-get wire-reader-eof? wire-reader-close
          wire-writer   wire-writer?   make-wire-writer   wire-writer-put   wire-writer-eof?   wire-writer-close
          in-wire-reader wire-pipe-pair wire-socketpair-pair)
  (import
    (rnrs)
    (only (chezscheme)                   record-writer void)
    (only (scheme2k bootstrap)           assert* check-interrupts raise-assertf raise-errorf)
          (scheme2k containers bytespan)
          (scheme2k posix fd)
    (only (scheme2k posix socket)        socketpair-fds)
          (scheme2k io wire)
    (only (scheme2k io port)             read-bytes-insert-right!))


(define-record-type (wire-reader %make-wire-reader wire-reader?)
  (fields
    (mutable in)        ; binary input port or read fd or #f
    (mutable read-eof?) ; boolean
    rbuf)               ; #f or bytespan, read buffer
  (nongenerative wire-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define-record-type (wire-writer %make-wire-writer wire-writer?)
  (fields
    (mutable out)       ; binary output port or write fd or #f
    wbuf)               ; #f or bytespan, write buffer
  (nongenerative wire-writer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; in-or-false must be one of:
;;   #f, indicating that wire-reader reached end-of-file
;;   a fixnum >= 0, indicating the file descriptor to read from
;;   a binary input port
(define (make-wire-reader in-or-false)
    (%validate-in in-or-false)
    (%make-wire-reader in-or-false
                         (not in-or-false)
                         (and in-or-false (bytespan))))


;; out-or-false must be one of:
;;   #f, indicating that wire-writer is closed
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
(define (make-wire-writer out-or-false)
    (%validate-out out-or-false)
    (%make-wire-writer out-or-false
                       (and out-or-false (bytespan))))


(define (%validate-in in)
  (cond
    ((not in)
      (void))
    ((number? in)
      (assert* 'make-wire-reader (fixnum? in))
      (assert* 'make-wire-reader (fx>=? in 0)))
    ((port? in)
      (assert* 'make-wire-reader (binary-port? in))
      (assert* 'make-wire-reader (input-port? in)))
    (else
      (raise-assertf 'make-wire-reader "(or (number? in) (port? in) (not in))"  in))))


(define (%validate-out out)
  (cond
    ((not out)
      (void))
    ((number? out)
      (assert* 'make-wire-writer (fixnum? out))
      (assert* 'make-wire-writer (fx>=? out 0)))
    ((port? out)
      (assert* 'make-wire-writer (binary-port? out))
      (assert* 'make-wire-writer (output-port? out)))
    (else
      (raise-assertf 'make-wire-writer "(or (number? out) (port? out) (not out))"  out))))


;; create and return a wire-reader and a wire-writer:
;;   the wire-reader reads serialized data from the read side of a newly created pipe file descriptor,
;;   the wire-writer writes serialized data to the write side of the same pipe (which is a different file descriptor).
(define (wire-pipe-pair)
  (let-values (((in out) (pipe-fds #t #t)))
    (values (make-wire-reader in)
            (make-wire-writer   out))))


;; create and return two wire-reader and two wire-writer, in the following order:
;;   wire-receiver1 reads  serialized data from the first socket file descriptor of a socket pair
;;   wire-sender1   writes serialized data to the same socket file descriptor as wire-receiver1
;;   wire-receiver2 reads  serialized data from the second socket file descriptor of the same socket pair
;;   wire-sender2   writes serialized data to the same socket file descriptor as wire-receiver2
(define (wire-socketpair-pair)
  (let-values (((fd1 fd2) (socketpair-fds #t #t)))
    (values (make-wire-reader fd1)
            (make-wire-writer   fd1)
            (make-wire-reader fd2)
            (make-wire-writer   fd2))))


;; read serialized data from the wire-reader's in,
;; repeating until a whole wire message is available,
;; then deserialize the message and return it.
;; may block while reading from file descriptor or port.
;;
;; return two values:
;;   deserialized datum, and #t
;;   or <unspecified> and #f on end-of-file, or if wire-reader's in is closed or not set.
;;
;; raise exception on I/O error or if serialized data cannot be parsed.
(define (wire-reader-get r)
  (if (wire-reader-eof? r)
    (values #f #f)
    (%wire-reader-get r (wire-reader-in r) (wire-reader-rbuf r))))


(define (%wire-reader-get r in rbuf)
  (let-values (((datum pos) (wire-get-from-bytespan rbuf)))
    (cond
      ((not (fixnum? pos))
        (raise-errorf 'wire-reader-get "failed parsing wire data read from ~s" in))
      ((fx>=? pos 0)
        (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
          (bytespan-delete-left! rbuf consumed-n))
        (values datum #t))
      (datum ; must discard (fx- pos) bytes and retry
        (bytespan-delete-left! rbuf (fx- pos))
        (%wire-reader-get r in rbuf))
      (else
        (bytespan-reserve-right! rbuf (fx+ (fxmax 4096 (fx- pos)) (bytespan-length rbuf)))
        (let ((read-n (%in-read-insert-right! in rbuf)))
          (if (fxzero? read-n)
            (begin
              (wire-reader-read-eof?-set! r #t)
              (bytespan-clear! rbuf)
              (values #f #f))
            (%wire-reader-get r in rbuf)))))))


;; read some bytes from in and append them to rbuf.
(define (%in-read-insert-right! in rbuf)
  (cond
    ((fixnum? in)
      (fd-read-insert-right! in rbuf))
    (else ; (port? in)
      (read-bytes-insert-right! in rbuf))))


;; serialize datum, and write its serialized representation to wire-writer's out.
;; may block while writing to wire-writer's out.
;;
;; return (void) if successful
;; return #f if wire-writer's out is closed or set to #f,
;;           or if library (scheme2k io wire) does not support serializing/deserializing datum
;; raise exception on I/O error
(define (wire-writer-put s datum)
  (let* ((out          (wire-writer-out s))
         (wbuf         (wire-writer-wbuf s))
         (serialized-n (and out wbuf (wire-put-to-bytespan wbuf datum))))
    (if serialized-n
      (begin
        (%out-write-all out wbuf)
        (bytespan-clear! wbuf))
      #f)))


(define (%out-write-all out wbuf)
  (let ((bv    (bytespan-peek-data wbuf))
        (start (bytespan-peek-beg  wbuf))
        (end   (bytespan-peek-end  wbuf)))
    (cond
      ((fixnum? out)
        (fd-write-all out bv start end))
      (else ; (port? out)
        (check-interrupts)
        (put-bytevector out bv start (fx- end start))))))


;; return #t if wire-reader's in is closed, not set, or reached end-of-file.
;; otherwise return #f
(define (wire-reader-eof? r)
  (or (wire-reader-read-eof? r)
      (not (wire-reader-in r))
      (not (wire-reader-rbuf r))))


;; return #t if wire-writer's out is closed or not set.
;; otherwise return #f
(define (wire-writer-eof? s)
  (or (not (wire-writer-out s))
      (not (wire-writer-wbuf s))))


;; close a wire-reader
(define (wire-reader-close r)
  (%close (wire-reader-in r))
  (wire-reader-in-set! r #f))


;; close a wire-writer. Helps detecting end-of-file at the receiver side.
(define (wire-writer-close s)
  (%close (wire-writer-out s))
  (wire-writer-out-set! s #f))


(define (%close obj)
  (cond ((fixnum? obj) (fd-close obj))
        ((port? obj)   (close-port obj))))


;; create and return a closure that iterates on data read from wire-reader r.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;;   either (values datum #t) i.e. the next datum read from wire-reader and #t,
;;   or (values #<unspecified> #f) if wire-reader reached end-of-file.
(define (in-wire-reader r)
  (assert* 'in-wire-reader (wire-reader? r))
  (lambda ()
    (wire-reader-get r)))


;; customize how "wire-reader" objects are printed
(record-writer (record-type-descriptor wire-reader)
  (lambda (r port writer)
    (put-string port "(wire-reader ")
    (writer (wire-reader-in r) port)
    (put-string port ")")))


;; customize how "wire-writer" objects are printed
(record-writer (record-type-descriptor wire-writer)
  (lambda (s port writer)
    (put-string port "(wire-writer ")
    (writer (wire-writer-out s) port)
    (put-string port ")")))


) ; close library
