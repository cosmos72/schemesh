;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/wire/wire.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writer that at each call to (writer-put)
;; serializes the received data into bytes according to WIRE protocol,
;; and writes such bytes to the underlying file descriptor or binary output port.
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


;; Create and return a wire-writer that, at each call to (writer-put)
;; serializes the received data into bytes according to WIRE protocol,
;; and writes such bytes to the underlying file descriptor or binary output port.
;;
;; Constructor argument out must be one of:
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
;;   #f, indicating that wire-writer is closed
;;   a box containing one of the cases above
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
      (let* ((out-box   (if (box? out) out (box out)))
             (out-unbox (if (box? out) (unbox out) out))
             (tx        (%make-wire-writer out-box (and out-unbox (bytespan)) close-out?)))
        (when out-unbox
          ;; immediately send wire magic, telling our wire protocol to any receiver.
          (let ((wbuf (wire-writer-wbuf tx)))
            (wire-put-magic-to-bytespan wbuf)
            (put-all tx wbuf)
            (bytespan-clear! wbuf)))
        tx))
    ((out)
      (make-wire-writer out #f))))


(define (%validate-out out)
  (let ((out (if (box? out) (unbox out) out)))
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
        (raise-assertf 'make-wire-writer "(or (number? out) (port? out) (not out))"  out)))))


;; called by (writer-close)
;; Helps detecting end-of-file at the receiver side.
(define (%wire-writer-close tx)
  (%close-box (wire-writer-out-box tx) (wire-writer-close-out? tx)))


;; return the wire-writer output port, file descriptor, or #f
(define (%wire-writer-out-unbox rx)
  (let ((out (unbox (wire-writer-out-box rx))))
    (unless out
      ;; automatically close when out is #f. sets eof? flag
      (writer-close rx))
    out))


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
