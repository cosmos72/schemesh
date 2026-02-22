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
  (export make-wire-reader wire-reader wire-reader?
          make-wire-writer wire-writer wire-writer?
          wire-pipe-pair wire-socketpair-pair)
  (import
    (rnrs)
    (only (chezscheme)                   box box-cas! record-writer unbox void)
    (only (scheme2k bootstrap)           assert* check-interrupts raise-assertf raise-errorf)
          (scheme2k containers bytespan)
          (scheme2k posix fd)
    (only (scheme2k posix socket)        socketpair-fds)
    (only (scheme2k io obj)              reader reader-get reader-eof? reader-close reader-skip
                                         writer writer-put writer-eof? writer-close)
    (only (scheme2k io port)             read-bytes-insert-right!)
          (scheme2k io wire))


(define-record-type (wire-reader %make-wire-reader wire-reader?)
  (parent reader)
  (fields
    in-box              ; box containing one of: #f, or read fd, or binary input port
    rbuf                ; #f or bytespan, read buffer
    close-in?)          ; boolean, #t if closing the wire-reader must close in.
  (protocol
    (lambda (args->new)
      (lambda (in-box rbuf close-in?)
        ((args->new %wire-reader-get %wire-reader-skip %wire-reader-close)
          in-box rbuf (and close-in? #t)))))
  (nongenerative wire-reader-7c46d04b-34f4-4046-b5c7-b63753c1be42))


(define-record-type (wire-writer %make-wire-writer wire-writer?)
  (parent writer)
  (fields
    out-box             ; box containing one of: #f, or write fd, or binary output port
    wbuf                ; #f or bytespan, write buffer
    close-out?)         ; boolean, #t if closing the wire-writer must close out.
  (protocol
    (lambda (args->new)
      (lambda (out-box wbuf close-out?)
        ((args->new %wire-writer-put %wire-writer-close)
          out-box wbuf (and close-out? #t)))))
  (nongenerative wire-writer-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; Create and return a wire-reader that, at each call to (reader-get)
;; reads some bytes from the underlying file descriptor or binary input port,
;; parses the bytes and returns the deserialized data.
;;
;; Constructor argument in must be one of:
;;   a fixnum >= 0, indicating the file descriptor to read from
;;   a binary input port
;;   #f, indicating that wire-reader reached end-of-file
;;
;; Note: as per reader contract, by default closing a wire-reader does NOT close
;; the underlying file descriptor or binary input port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a wire-reader should take ownership of the file descriptor or binary input port
;; passed to the constructor, then pass a truish value as the optional argument close-in?
(define make-wire-reader
  (case-lambda
    ((in close-in?)
      (%validate-in in)
      (%make-wire-reader (box in) (and in (bytespan)) close-in?))
    ((in)
      (make-wire-reader in #f))))


;; Create and return a wire-writer that, at each call to (writer-put)
;; serializes the received data into bytes, and writes such bytes;;
;; to the underlying file descriptor or binary output port.
;;
;; Constructor argument out must be one of:
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
;;   #f, indicating that wire-writer is closed
;;
;; Note: as per writer contract, by default closing a wire-writer does NOT close
;; the underlying file descriptor or binary output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a wire-writer should take ownership of the file descriptor or binary output port
;; passed to the constructor, then pass a truish value as the optional argument close-out?
(define make-wire-writer
  (case-lambda
    ((out close-out?)
      (%validate-out out)
      (%make-wire-writer (box out) (and out (bytespan)) close-out?))
    ((out)
      (make-wire-writer out #f))))


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
;;
;; the returned wire-reader and wire-writer take ownership of the created pipe file descriptors,
;; and closing one of them closes the corresponding pipe file descriptor.
(define (wire-pipe-pair)
  (let-values (((in out) (pipe-fds #t #t))) ;; mark fds close-on-exec
    (values (%make-wire-reader (box in)  (bytespan) #t)
            (%make-wire-writer (box out) (bytespan) #t))))


;; create and return two wire-reader and two wire-writer, in the following order:
;;   wire-receiver1 reads  serialized data from the first socket file descriptor of a socket pair
;;   wire-sender1   writes serialized data to the same socket file descriptor as wire-receiver1
;;   wire-receiver2 reads  serialized data from the second socket file descriptor of the same socket pair
;;   wire-sender2   writes serialized data to the same socket file descriptor as wire-receiver2
;;
;; the returned wire-readers and wire-writers take ownership of the created socket file descriptors,
;; and closing one of them closes the corresponding socket file descriptor.
(define (wire-socketpair-pair)
  (let-values (((fd1 fd2) (socketpair-fds #t #t))) ; mark fds close-on-exec
    (let ((box1 (box fd1))
          (box2 (box fd2)))
      ;; give the same box to each reader and writer pair sharing a single fd,
      ;; so that closing one also closes the other
      (values (%make-wire-reader box1 (bytespan) #t)
              (%make-wire-writer box1 (bytespan) #t)
              (%make-wire-reader box2 (bytespan) #t)
              (%make-wire-writer box2 (bytespan) #t)))))


;; called by (reader-close)
(define (%wire-reader-close rx)
  (%close-box (wire-reader-in-box rx) (wire-reader-close-in? rx)))


;; called by (writer-close)
;; Helps detecting end-of-file at the receiver side.
(define (%wire-writer-close tx)
  (%close-box (wire-writer-out-box tx) (wire-writer-close-out? tx)))


(define (%close-box obj-box close?)
  (let ((obj (unbox obj-box)))
    ;; store #f in box, instead of fd or port.
    ;; close fd or port only if wire-reader or wire-writer own them
    (when (and obj (box-cas! obj-box obj #f) close?)
      (if (fixnum? obj)
        (fd-close obj)
        (close-port obj)))))


;; return the wire-writer output port, file descriptor, or #f
(define (%wire-writer-out-unbox rx)
  (let ((out (unbox (wire-writer-out-box rx))))
    (unless out
      ;; automatically close when out is #f. sets eof? flag
      (writer-close rx))
    out))


;; called by (%wire-reader-get) and (%wire-reader-skip)
(define (%wire-reader-get-or-skip rx caller skip?)
  (let ((rbuf (wire-reader-rbuf rx)))
    (let-values (((datum pos) (wire-get-from-bytespan rbuf 0 (bytespan-length rbuf) skip?)))
      (cond
        ((not (fixnum? pos))
          (raise-errorf caller "failed parsing wire data read from ~s" rx))
        ((fx>=? pos 0)
          (let ((consumed-n (fx- pos (bytespan-peek-beg rbuf))))
            (bytespan-delete-left! rbuf consumed-n))
          (values datum #t))
        (datum ; must discard (fx- pos) bytes and retry
          (bytespan-delete-left! rbuf (fx- pos))
          (%wire-reader-get-or-skip rx caller skip?))
        (else
          (let* ((in     (unbox (wire-reader-in-box rx)))
                 (read-n (%in-read-insert-right! in rbuf)))
            (if (fxzero? read-n)
              (begin
                (reader-close rx)
                (bytespan-clear! rbuf)
                (values #f #f))
              (%wire-reader-get-or-skip rx caller skip?))))))))


;; called by (reader-get)
(define (%wire-reader-get rx)
  (%wire-reader-get-or-skip rx 'wire-reader-get #f))


;; called by (reader-skip)
(define (%wire-reader-skip rx)
  (let-values (((obj ok?) (%wire-reader-get-or-skip rx 'wire-reader-skip #t)))
    ok?))


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


(define (unbox-raise-if-closed tx)
  (let ((out (%wire-writer-out-unbox tx)))
    (unless out
      (raise-errorf 'wire-writer-put "~s is already closed" tx))
    out))


(define (put-all tx wbuf)
  (let ((out   (unbox-raise-if-closed tx))
        (bv    (bytespan-peek-data wbuf))
        (start (bytespan-peek-beg  wbuf))
        (end   (bytespan-peek-end  wbuf)))
    (cond
      ((fixnum? out)
        (fd-write-all out bv start end))
      (else ; (port? out)
        (check-interrupts)
        (put-bytevector out bv start (fx- end start))
        ;; really send serialized data to its destination, in case some other program is waiting for it
        (flush-output-port out)))))


;; called by (wire-writer-put) and (writer-put)
(define (%wire-writer-put tx datum)
  (unbox-raise-if-closed tx)
  (let* ((wbuf         (wire-writer-wbuf tx))
         (serialized-n (wire-put-to-bytespan wbuf datum)))
    (unless serialized-n
      (raise-errorf 'wire-writer-put "unsupported datum: ~s" datum))
    (put-all tx wbuf)
    (bytespan-clear! wbuf)))


;; customize how "wire-reader" objects are printed
(record-writer (record-type-descriptor wire-reader)
  (lambda (rx port writer)
    (put-string port "#<wire-reader")
    (put-string port (if (reader-eof? rx) " eof " " ok "))
    (writer (unbox (wire-reader-in-box rx)) port)
    (put-string port ">")))


;; customize how "wire-writer" objects are printed
(record-writer (record-type-descriptor wire-writer)
  (lambda (tx port writer)
    (put-string port "#<wire-writer")
    (put-string port (if (writer-eof? tx) " eof " " ok "))
    (writer (unbox (wire-writer-out-box tx)) port)
    (put-string port ">")))


) ; close library
