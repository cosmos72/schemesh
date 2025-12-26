;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;;; inter-process communication library:
;;;
;;; exchanges serialized data through binary ports or file descriptors (sockets, pipes ...).
;;;
;;; data is serialized/deserialized with library (scheme2k wire)
;;;
(library (scheme2k ipc channel (0 9 2))
  (export (rename (%channel channel))
          channel? channel-close channel-pipe-pair channel-socket-pair
          channel-get channel-eof? channel-put in-channel)
  (import
    (rnrs)
    (only (chezscheme)            record-writer void)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-assertf raise-errorf)
    (scheme2k containers bytespan)
    (scheme2k posix fd)
    (only (scheme2k posix socket) socketpair-fds)
    (scheme2k wire)
    (only (scheme2k port)         read-bytes-insert-right!))


(define-record-type channel
  (fields
    (mutable in)        ; binary input port, or read file descriptor, or #f
    (mutable out)       ; binary output port, or write file descriptor, or #f
    (mutable read-eof?) ; boolean, #t if channel-in reached end-of-file
    rbuf                ; #f or bytespan, read buffer
    wbuf)               ; #f or bytespan, write buffer
  (nongenerative channel-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; close the file descriptor(s) used by channel
(define (channel-close c)
  (let ((in  (channel-in c))
        (out (channel-out c)))
    (%channel-close-in c in)
    (unless (eqv? in out)
      (%channel-close-out c out))))


(define (%channel-close-in c in)
  (%close in)
  (channel-in-set! c #f))


(define (%channel-close-out c out)
  (%close out)
  (channel-out-set! c #f))


(define (%close obj)
  (cond
    ((not obj)
      (void))
    ((fixnum? obj)
      (fd-close obj))
    ((port? obj)
      (close-port obj))))


;; create and return a channel that reads/writes serialized data from/to specified file descriptors or ports
(define %channel
  (case-lambda
    ((in-or-false out-or-false)
      (%channel-validate-in  in-or-false)
      (%channel-validate-out out-or-false)
      (make-channel in-or-false
                    out-or-false
                    (not in-or-false)
                    (and in-or-false (bytespan))
                    (and out-or-false (bytespan))))
    ((in-out-or-false)
      (%channel in-out-or-false in-out-or-false))))


(define (%channel-validate-in in)
  (cond
    ((not in)
      (void))
    ((number? in)
      (assert* 'channel (fixnum? in))
      (assert* 'channel (fx>=? in 0)))
    ((port? in)
      (assert* 'channel (binary-port? in))
      (assert* 'channel (input-port? in)))
    (else
      (raise-assertf 'channel "(or (number? in) (port? in) (not in))"  in))))


(define (%channel-validate-out out)
  (cond
    ((not out)
      (void))
    ((number? out)
      (assert* 'channel (fixnum? out))
      (assert* 'channel (fx>=? out 0)))
    ((port? out)
      (assert* 'channel (binary-port? out))
      (assert* 'channel (output-port? out)))
    (else
      (raise-assertf 'channel "(or (number? out) (port? out) (not out))"  out))))


;; create and return two connected channels:
;; the first channel reads serialized data from the read side of a newly created pipe file descriptor,
;; the second channel writes serialized data to the write side of the same pipe (which is a different file descriptor).
(define (channel-pipe-pair)
  (let-values (((in out) (pipe-fds #t #t))) ; mark both pipe file descriptors as close-on-exec
    (values (%channel in #f) (%channel #f out))))


;; create and return two connected channels:
;; the first channel reads and writes serialized data from/to the first socket of a socket pair
;; the second channel reads and writes serialized data from/to the second socket of the same socket pair (which is a different file descriptor).
(define (channel-socket-pair)
  (let-values (((socket1 socket2) (socketpair-fds #t #t))) ; mark both socket file descriptors as close-on-exec
    (values (%channel socket1) (%channel socket2))))


;; serialize datum, and write its serialized representation to channel's out.
;; may block while writing to channel's out.
;;
;; return (void) if successful
;; return #f if channel's out is closed or set to #f,
;;           or if library (scheme2k wire) does not support serializing/deserializing datum
;; raise exception on I/O error
(define (channel-put c datum)
  (let* ((out          (channel-out c))
         (wbuf         (channel-wbuf c))
         (serialized-n (and out wbuf (wire-put wbuf datum))))
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



;; read serialized data from the channel's read file descriptor,
;; repeating until a whole wire message is available,
;; then deserialize the message and return it.
;; may block while reading from file descriptor.
;;
;; return two values:
;;   deserialized datum, and #t
;;   or <unspecified> and #f on end-of-file, or if channel's in is closed or not set.
;;
;; raise exception on I/O error or if serialized data cannot be parsed.
(define (channel-get c)
  (if (channel-eof? c)
    (values #f #f)
    (%channel-get c (channel-in c) (channel-rbuf c))))


;; implementation of (channel-get)
(define (%channel-get c in rbuf)
  (let-values (((datum pos) (wire-get (bytespan-peek-data rbuf)
                                      (bytespan-peek-beg  rbuf)
                                      (bytespan-peek-end  rbuf))))
    (cond
      ((not (fixnum? pos))
        (raise-errorf 'channel-get "failed parsing wire-serialized data read from ~s ~s" in))
      ((fx>=? pos 0)
        (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
          (bytespan-delete-left! rbuf consumed-n))
        (values datum #t))
      (datum ; must discard (fx- pos) bytes and try again
        (bytespan-delete-left! rbuf (fx- pos))
        (%channel-get c in rbuf))
      (else ; must read at least (fx- pos) more bytes and try again
        (bytespan-reserve-right! rbuf (fx+ (fxmax 4096 (fx- pos)) (bytespan-length rbuf)))
        (let ((read-n (%in-read-insert-right! in rbuf)))
          (if (fxzero? read-n)
            (begin
              (channel-read-eof?-set! c #t)
              (bytespan-clear! rbuf)
              (values #f #f))
            (%channel-get c in rbuf)))))))


;; read some bytes from in and append them to rbuf.
(define (%in-read-insert-right! in rbuf)
  (cond
    ((fixnum? in)
      (fd-read-insert-right! in rbuf))
    (else ; (port? in)
      (read-bytes-insert-right! in rbuf))))


;; return #t if channel's in is closed, not set or reached end-of-file.
;; otherwise return #f
(define (channel-eof? c)
  (or (channel-read-eof? c)
      (not (channel-in c))
      (not (channel-rbuf c))))


;; create and return a closure that iterates on data read from channel c.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values datum #t) i.e. the next datum read from channel and #t,
;; or (values #<unspecified> #f) if channel reached end-of-file.
(define (in-channel c)
  (assert* 'in-channel (channel? c))
  (lambda ()
    (channel-get c)))


;; customize how "channel" objects are printed
(record-writer (record-type-descriptor channel)
  (lambda (c port writer)
    (display "(channel " port)
    (let ((in  (channel-in c))
          (out (channel-out c)))
      (display in port)
      (unless (eqv? in out)
        (display #\space port)
        (display (channel-out c) port)))
    (display ")" port)))



) ; close library
