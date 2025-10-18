;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; inter-process communication library:
;;;
;;; exchanges serialized data through sockets, pipes or other file descriptors.
;;;
;;; data is serialized/deserialized with library (schemesh wire)
;;;
(library (schemesh ipc channel (0 9 2))
  (export channel? channel-close channel-fd channel-pipe-pair channel-socket-pair
          channel-get channel-eof? channel-put in-channel)
  (import
    (rnrs)
    (only (chezscheme)         record-writer)
    (only (schemesh bootstrap) assert* raise-errorf)
    (schemesh containers bytespan)
    (schemesh posix fd)
    (schemesh wire))

(define-record-type channel
  (fields
    (mutable read-fd)   ; #f or unsigned fixnum, read file descriptor
    (mutable write-fd)  ; #f or unsigned fixnum, write file descriptor
    (mutable read-eof?) ; boolean, #t if read file descriptor reached end-of-file
    rbuf                ; #f or bytespan, read buffer
    wbuf)               ; #f or bytespan, write buffer
  (nongenerative channel-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; close the file descriptor(s) used by channel
(define (channel-close c)
  (let ((read-fd  (channel-read-fd c))
        (write-fd (channel-write-fd c)))
    (when read-fd
      (fd-close read-fd)
      (channel-read-fd-set! c #f))
    (when write-fd
      (unless (eqv? write-fd read-fd)
        (fd-close write-fd))
      (channel-write-fd-set! c #f))))


;; create and return a channel that reads/writes serialized data from/to specified file descriptors
(define channel-fd
  (case-lambda
    ((read-write-fd)
      (assert* 'channel-fd (fixnum? read-write-fd))
      (assert* 'channel-fd (fx>=? read-write-fd 0))
      (channel-fd read-write-fd read-write-fd))

    ((read-fd-or-false write-fd-or-false)
      (when read-fd-or-false
        (assert* 'channel-fd (fixnum? read-fd-or-false))
        (assert* 'channel-fd (fx>=? read-fd-or-false 0)))
      (when write-fd-or-false
        (assert* 'channel-fd (fixnum? write-fd-or-false))
        (assert* 'channel-fd (fx>=? write-fd-or-false 0)))
      (make-channel read-fd-or-false
                    write-fd-or-false
                    (not read-fd-or-false)
                    (and read-fd-or-false (bytespan))
                    (and write-fd-or-false (bytespan))))))


;; create and return two connected channels:
;; the first channel reads serialized data from the read side of a newly created pipe file descriptor,
;; the second channel writes serialized data to the write side of the same pipe (which is a different file descriptor).
(define (channel-pipe-pair)
  (let-values (((read-fd write-fd) (open-pipe-fds #t #t))) ; mark both pipe file descriptors as close-on-exec
    (values (channel-fd read-fd #f) (channel-fd #f write-fd))))


;; create and return two connected channels:
;; the first channel reads and writes serialized data from/to the first socket of a socket pair
;; the second channel reads and writes serialized data from/to the second socket of the same socket pair (which is a different file descriptor).
(define (channel-socket-pair)
  (let-values (((socket1 socket2) (open-socketpair-fds #t #t))) ; mark both socket file descriptors as close-on-exec
    (values (channel-fd socket1) (channel-fd socket2))))


;; serialize datum, and write its serialized representation to the channel's write file descriptor.
;; may block while writing to file descriptor.
;;
;; return (void) if successful
;; return #f if channel's write-fd is closed or not set,
;;           or if library (schemesh wire) does not support serializing/deserializing datum
;; raise exception on I/O error
(define (channel-put c datum)
  (let* ((write-fd     (channel-write-fd c))
         (wbuf         (channel-wbuf c))
         (serialized-n (and write-fd wbuf (wire-put wbuf datum))))
    (if serialized-n
      (begin
         (fd-write-all (channel-write-fd c)
                       (bytespan-peek-data wbuf)
                       (bytespan-peek-beg  wbuf)
                       (bytespan-peek-end  wbuf))
         (bytespan-clear! wbuf))
      #f)))


;; implementation of (channel-get)
(define (%channel-get c fd rbuf)
  (let-values (((datum pos) (wire-get (bytespan-peek-data rbuf)
                                      (bytespan-peek-beg  rbuf)
                                      (bytespan-peek-end  rbuf))))
    (cond
      ((not (fixnum? pos))
        (raise-errorf 'channel-get "failed parsing wire-serialized data read from file descriptor ~s" fd))
      ((fx>=? pos 0)
        (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
          (bytespan-delete-left! rbuf consumed-n))
        (values datum #t))
      (datum ; must discard (fx- pos) bytes and try again
        (bytespan-delete-left! rbuf (fx- pos))
        (%channel-get c fd rbuf))
      (else ; must read at least (fx- pos) more bytes and try again
        (bytespan-reserve-right! rbuf (fx+ (fxmax 4096 (fx- pos)) (bytespan-length rbuf)))
        (let ((read-n (fd-read-insert-right! fd rbuf)))
          (if (fxzero? read-n)
            (begin
              (channel-read-eof?-set! c #t)
              (bytespan-clear! rbuf)
              (values #f #f))
            (%channel-get c fd rbuf)))))))


;; read serialized data from the channel's read file descriptor,
;; repeating until a whole wire message is available,
;; then deserialize the message and return it.
;; may block while reading from file descriptor.
;;
;; return two values:
;;   deserialized datum, and #t
;;   or <unspecified> and #f on end-of-file, or if channel's read-fd is closed or not set.
;;
;; raise exception on I/O error or if serialized data cannot be parsed.
(define (channel-get c)
  (if (channel-eof? c)
    (values #f #f)
    (%channel-get c (channel-read-fd c) (channel-rbuf c))))


;; return #t if channel's read-fd is closed, not set or reached end-of-file.
;; otherwise return #f
(define (channel-eof? c)
  (or (channel-read-eof? c)
      (not (channel-read-fd c))
      (not (channel-rbuf c))))


;; create and return a closure that iterates on data read from channel c.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values datum #t) i.e. the next datum read from channel and #t,
;; or (values #<unspecified> #f) if channel reached end-of-file.
(define (in-channel c)
  (lambda ()
    (channel-get c)))


;; customize how "channel" objects are printed
(record-writer (record-type-descriptor channel)
  (lambda (c port writer)
    (display "(channel-fd " port)
    (let ((read-fd  (channel-read-fd c))
          (write-fd (channel-write-fd c)))
      (display read-fd port)
      (unless (eqv? read-fd write-fd)
        (display #\space port)
        (display (channel-write-fd c) port)))
    (display ")" port)))



) ; close library
