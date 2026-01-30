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
  (export wire-receiver wire-receiver? make-wire-receiver wire-receiver-get wire-receiver-eof? wire-receiver-close
          wire-sender   wire-sender?   make-wire-sender   wire-sender-put   wire-sender-eof?   wire-sender-close
          in-wire-receiver wire-pipe-pair wire-socketpair-pair)
  (import
    (rnrs)
    (only (chezscheme)                   record-writer void)
    (only (scheme2k bootstrap)           assert* check-interrupts raise-assertf raise-errorf)
          (scheme2k containers bytespan)
          (scheme2k posix fd)
    (only (scheme2k posix socket)        socketpair-fds)
          (scheme2k io wire)
    (only (scheme2k io port)             read-bytes-insert-right!))


(define-record-type (wire-receiver %make-wire-receiver wire-receiver?)
  (fields
    (mutable in)        ; binary input port or read fd or #f
    (mutable read-eof?) ; boolean
    rbuf)               ; #f or bytespan, read buffer
  (nongenerative wire-receiver-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define-record-type (wire-sender %make-wire-sender wire-sender?)
  (fields
    (mutable out)       ; binary output port or write fd or #f
    wbuf)               ; #f or bytespan, write buffer
  (nongenerative wire-sender-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; in-or-false must be one of:
;;   #f, indicating that wire-receiver reached end-of-file
;;   a fixnum >= 0, indicating the file descriptor to read from
;;   a binary input port
(define (make-wire-receiver in-or-false)
    (%validate-in in-or-false)
    (%make-wire-receiver in-or-false
                         (not in-or-false)
                         (and in-or-false (bytespan))))


;; out-or-false must be one of:
;;   #f, indicating that wire-sender is closed
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
(define (make-wire-sender out-or-false)
    (%validate-out out-or-false)
    (%make-wire-sender out-or-false
                       (and out-or-false (bytespan))))


(define (%validate-in in)
  (cond
    ((not in)
      (void))
    ((number? in)
      (assert* 'make-wire-receiver (fixnum? in))
      (assert* 'make-wire-receiver (fx>=? in 0)))
    ((port? in)
      (assert* 'make-wire-receiver (binary-port? in))
      (assert* 'make-wire-receiver (input-port? in)))
    (else
      (raise-assertf 'make-wire-receiver "(or (number? in) (port? in) (not in))"  in))))


(define (%validate-out out)
  (cond
    ((not out)
      (void))
    ((number? out)
      (assert* 'make-wire-sender (fixnum? out))
      (assert* 'make-wire-sender (fx>=? out 0)))
    ((port? out)
      (assert* 'make-wire-sender (binary-port? out))
      (assert* 'make-wire-sender (output-port? out)))
    (else
      (raise-assertf 'make-wire-sender "(or (number? out) (port? out) (not out))"  out))))


;; create and return a wire-receiver and a wire-sender:
;;   the wire-receiver reads serialized data from the read side of a newly created pipe file descriptor,
;;   the wire-sender writes serialized data to the write side of the same pipe (which is a different file descriptor).
(define (wire-pipe-pair)
  (let-values (((in out) (pipe-fds #t #t)))
    (values (make-wire-receiver in)
            (make-wire-sender   out))))


;; create and return two wire-receiver and two wire-sender, in the following order:
;;   wire-receiver1 reads  serialized data from the first socket file descriptor of a socket pair
;;   wire-sender1   writes serialized data to the same socket file descriptor as wire-receiver1
;;   wire-receiver2 reads  serialized data from the second socket file descriptor of the same socket pair
;;   wire-sender2   writes serialized data to the same socket file descriptor as wire-receiver2
(define (wire-socketpair-pair)
  (let-values (((fd1 fd2) (socketpair-fds #t #t)))
    (values (make-wire-receiver fd1)
            (make-wire-sender   fd1)
            (make-wire-receiver fd2)
            (make-wire-sender   fd2))))


;; read serialized data from the wire-receiver's in,
;; repeating until a whole wire message is available,
;; then deserialize the message and return it.
;; may block while reading from file descriptor or port.
;;
;; return two values:
;;   deserialized datum, and #t
;;   or <unspecified> and #f on end-of-file, or if wire-receiver's in is closed or not set.
;;
;; raise exception on I/O error or if serialized data cannot be parsed.
(define (wire-receiver-get r)
  (if (wire-receiver-eof? r)
    (values #f #f)
    (%wire-receiver-get r (wire-receiver-in r) (wire-receiver-rbuf r))))


(define (%wire-receiver-get r in rbuf)
  (let-values (((datum pos) (wire-get-from-bytespan rbuf)))
    (cond
      ((not (fixnum? pos))
        (raise-errorf 'wire-receiver-get "failed parsing wire data read from ~s" in))
      ((fx>=? pos 0)
        (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
          (bytespan-delete-left! rbuf consumed-n))
        (values datum #t))
      (datum ; must discard (fx- pos) bytes and retry
        (bytespan-delete-left! rbuf (fx- pos))
        (%wire-receiver-get r in rbuf))
      (else
        (bytespan-reserve-right! rbuf (fx+ (fxmax 4096 (fx- pos)) (bytespan-length rbuf)))
        (let ((read-n (%in-read-insert-right! in rbuf)))
          (if (fxzero? read-n)
            (begin
              (wire-receiver-read-eof?-set! r #t)
              (bytespan-clear! rbuf)
              (values #f #f))
            (%wire-receiver-get r in rbuf)))))))


;; read some bytes from in and append them to rbuf.
(define (%in-read-insert-right! in rbuf)
  (cond
    ((fixnum? in)
      (fd-read-insert-right! in rbuf))
    (else ; (port? in)
      (read-bytes-insert-right! in rbuf))))


;; serialize datum, and write its serialized representation to wire-sender's out.
;; may block while writing to wire-sender's out.
;;
;; return (void) if successful
;; return #f if wire-sender's out is closed or set to #f,
;;           or if library (scheme2k io wire) does not support serializing/deserializing datum
;; raise exception on I/O error
(define (wire-sender-put s datum)
  (let* ((out          (wire-sender-out s))
         (wbuf         (wire-sender-wbuf s))
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


;; return #t if wire-receiver's in is closed, not set, or reached end-of-file.
;; otherwise return #f
(define (wire-receiver-eof? r)
  (or (wire-receiver-read-eof? r)
      (not (wire-receiver-in r))
      (not (wire-receiver-rbuf r))))


;; return #t if wire-sender's out is closed or not set.
;; otherwise return #f
(define (wire-sender-eof? s)
  (or (not (wire-sender-out s))
      (not (wire-sender-wbuf s))))


;; close a wire-receiver
(define (wire-receiver-close r)
  (%close (wire-receiver-in r))
  (wire-receiver-in-set! r #f))


;; close a wire-sender. Helps detecting end-of-file at the receiver side.
(define (wire-sender-close s)
  (%close (wire-sender-out s))
  (wire-sender-out-set! s #f))


(define (%close obj)
  (cond ((fixnum? obj) (fd-close obj))
        ((port? obj)   (close-port obj))))


;; create and return a closure that iterates on data read from wire-receiver r.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;;   either (values datum #t) i.e. the next datum read from wire-receiver and #t,
;;   or (values #<unspecified> #f) if wire-receiver reached end-of-file.
(define (in-wire-receiver r)
  (assert* 'in-wire-receiver (wire-receiver? r))
  (lambda ()
    (wire-receiver-get r)))


;; customize how "wire-receiver" objects are printed
(record-writer (record-type-descriptor wire-receiver)
  (lambda (r port writer)
    (display "(wire-receiver " port)
    (display (wire-receiver-in r) port)
    (display ")" port)))


;; customize how "wire-sender" objects are printed
(record-writer (record-type-descriptor wire-sender)
  (lambda (s port writer)
    (display "(wire-sender " port)
    (display (wire-sender-out s) port)
    (display ")" port)))


) ; close library
