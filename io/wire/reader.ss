;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


;; this file should be included only by file io/wire/wire.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader that at each call to (reader-get),
;; reads some bytes from the underlying file descriptor or binary input port,
;; parses the bytes according to WIRE protocol and returns the deserialized data.
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



;; Create and return a wire-reader that, at each call to (reader-get)
;; reads some bytes from the underlying file descriptor or binary input port,
;; parses the bytes according to WIRE protocol and returns the deserialized data.
;;
;; Constructor argument in must be one of:
;;   a fixnum >= 0, indicating the file descriptor to read from
;;   a binary input port
;;   #f, indicating that wire-reader reached end-of-file
;;   a box containing one of the cases above
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
      (let ((in-box   (if (box? in) in (box in)))
            (in-unbox (if (box? in) (unbox in) in)))
        (%make-wire-reader in-box (and in-unbox (bytespan)) close-in?)))
    ((in)
      (make-wire-reader in #f))))


(define (%validate-in in)
  (let ((in (if (box? in) (unbox in) in)))
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
        (raise-assertf 'make-wire-reader "(or (number? in) (port? in) (not in))"  in)))))


;; called by (reader-close)
(define (%wire-reader-close rx)
  (%close-box (wire-reader-in-box rx) (wire-reader-close-in? rx)))


(define (%close-box obj-box close?)
  (let ((obj (unbox obj-box)))
    ;; store #f in box, instead of fd or port.
    ;; close fd or port only if wire-reader or wire-writer own them
    (when (and obj (box-cas! obj-box obj #f) close?)
      (if (fixnum? obj)
        (fd-close obj)
        (close-port obj)))))


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
