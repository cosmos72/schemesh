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
  (export make-wire-reader wire-reader wire-reader? wire-reader-get wire-reader-eof? wire-reader-close
          make-wire-writer wire-writer wire-writer? wire-writer-put wire-writer-eof? wire-writer-close
          in-wire-reader wire-pipe-pair wire-socketpair-pair)
  (import
    (rnrs)
    (only (chezscheme)                   box box-cas! record-writer unbox void)
    (only (scheme2k bootstrap)           assert* check-interrupts raise-assertf raise-errorf)
          (scheme2k containers bytespan)
          (scheme2k posix fd)
    (only (scheme2k posix socket)        socketpair-fds)
          (scheme2k io wire)
    (only (scheme2k io port)             read-bytes-insert-right!))


(define-record-type (wire-reader %make-wire-reader wire-reader?)
  (fields
    in-box              ; box containing one of: #f, or read fd, or binary input port
    (mutable read-eof?) ; boolean
    rbuf)               ; #f or bytespan, read buffer
  (nongenerative wire-reader-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define-record-type (wire-writer %make-wire-writer wire-writer?)
  (fields
    out-box             ; box containing one of: #f, or write fd, or binary output port
    wbuf)               ; #f or bytespan, write buffer
  (nongenerative wire-writer-7c46d04b-34f4-4046-b5c7-b63753c1be39))


;; in-or-false must be one of:
;;   #f, indicating that wire-reader reached end-of-file
;;   a fixnum >= 0, indicating the file descriptor to read from
;;   a binary input port
(define (make-wire-reader in-or-false)
    (%validate-in in-or-false)
    (%make-wire-reader (box in-or-false)
                       (not in-or-false)
                       (and in-or-false (bytespan))))


;; out-or-false must be one of:
;;   #f, indicating that wire-writer is closed
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
(define (make-wire-writer out-or-false)
    (%validate-out out-or-false)
    (%make-wire-writer (box out-or-false)
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
  (let-values (((in out) (pipe-fds #t #t))) ;; mark fds close-on-exec
    (values (%make-wire-reader (box in)  #f (bytespan))
            (%make-wire-writer (box out)    (bytespan)))))


;; create and return two wire-reader and two wire-writer, in the following order:
;;   wire-receiver1 reads  serialized data from the first socket file descriptor of a socket pair
;;   wire-sender1   writes serialized data to the same socket file descriptor as wire-receiver1
;;   wire-receiver2 reads  serialized data from the second socket file descriptor of the same socket pair
;;   wire-sender2   writes serialized data to the same socket file descriptor as wire-receiver2
(define (wire-socketpair-pair)
  (let-values (((fd1 fd2) (socketpair-fds #t #t))) ; mark fds close-on-exec
    (let ((box1 (box fd1))
          (box2 (box fd2)))
      ;; give the same box to each reader and writer pair sharing a single fd,
      ;; so that closing any one also closes the other
      (values (%make-wire-reader box1 #f (bytespan))
              (%make-wire-writer box1    (bytespan))
              (%make-wire-reader box2 #f (bytespan))
              (%make-wire-writer box2    (bytespan))))))


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
(define (wire-reader-get rx)
  (%wire-reader-get rx (wire-reader-in-box rx) (wire-reader-rbuf rx)))


(define (%wire-reader-get rx in-box rbuf)
  (if (wire-reader-eof? rx)
    (values #f #f)
    (let-values (((datum pos) (wire-get-from-bytespan rbuf)))
      (cond
        ((not (fixnum? pos))
          (raise-errorf 'wire-reader-get "failed parsing wire data read from ~tx" (unbox in-box)))
        ((fx>=? pos 0)
          (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
            (bytespan-delete-left! rbuf consumed-n))
          (values datum #t))
        (datum ; must discard (fx- pos) bytes and retry
          (bytespan-delete-left! rbuf (fx- pos))
          (%wire-reader-get rx in-box rbuf))
        (else
          (let ((read-n (%in-read-insert-right! (unbox in-box) rbuf)))
            (if (fxzero? read-n)
              (begin
                (wire-reader-read-eof?-set! rx #t)
                (bytespan-clear! rbuf)
                (values #f #f))
              (%wire-reader-get rx in-box rbuf))))))))


;; read some bytes from in and append them to rbuf.
;; return number of bytes actually read, which can be zero only on eof.
(define (%in-read-insert-right! in rbuf)
  (cond
    ((not in)
      0)
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
(define (wire-writer-put tx datum)
  (let* ((out-box      (wire-writer-out-box tx))
         (wbuf         (wire-writer-wbuf tx))
         (serialized-n (and (unbox out-box) wbuf (wire-put-to-bytespan wbuf datum))))
    (if serialized-n
      (let ((out (unbox out-box)))
        (when out
          (%out-write-all out wbuf))
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
(define (wire-reader-eof? rx)
  (or (wire-reader-read-eof? rx)
      (not (unbox (wire-reader-in-box rx)))
      (not (wire-reader-rbuf rx))))


;; return #t if wire-writer's out is closed or not set.
;; otherwise return #f
(define (wire-writer-eof? tx)
  (or (not (unbox (wire-writer-out-box tx)))
      (not (wire-writer-wbuf tx))))


;; close a wire-reader
(define (wire-reader-close rx)
  (%close-box (wire-reader-in-box rx)))


;; close a wire-writer. Helps detecting end-of-file at the receiver side.
(define (wire-writer-close tx)
  (%close-box (wire-writer-out-box tx)))


(define (%close-box obj-box)
  (let ((obj (unbox obj-box)))
    (when (and obj (box-cas! obj-box obj #f))
      (if (fixnum? obj)
        (fd-close obj)
        (close-port obj)))))


;; create and return a closure that iterates on data read from wire-reader rx.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;;   either (values datum #t) i.e. the next datum read from wire-reader and #t,
;;   or (values #<unspecified> #f) if wire-reader reached end-of-file.
(define (in-wire-reader rx)
  (assert* 'in-wire-reader (wire-reader? rx))
  (lambda ()
    (wire-reader-get rx)))


;; customize how "wire-reader" objects are printed
(record-writer (record-type-descriptor wire-reader)
  (lambda (rx port writer)
    (put-string port "(wire-reader ")
    (writer (unbox (wire-reader-in-box rx)) port)
    (put-string port ")")))


;; customize how "wire-writer" objects are printed
(record-writer (record-type-descriptor wire-writer)
  (lambda (tx port writer)
    (put-string port "(wire-writer ")
    (writer (unbox (wire-writer-out-box tx)) port)
    (put-string port ")")))


) ; close library
