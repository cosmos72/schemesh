;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file io/cbor/cbor.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writer that at each call to (writer-put)
;; serializes the received data into bytes according to WIRE protocol,
;; and writes such bytes to the underlying file descriptor or binary output port.
(define-record-type (cbor-writer %make-cbor-writer cbor-writer?)
  (parent writer)
  (fields
    out-box             ; box containing one of: #f, or write fd, or binary output port
    wbuf                ; #f or bytespan, write buffer
    close-out?)         ; boolean, #t if closing the cbor-writer must close out.
  (protocol
    (lambda (args->new)
      (lambda (out-box wbuf close-out?)
        ((args->new %cbor-writer-put %cbor-writer-close)
          out-box wbuf (and close-out? #t)))))
  (nongenerative cbor-writer-7c46d04b-34f4-4046-b5c7-b63753c1be42))


;; Create and return a cbor-writer that, at each call to (writer-put)
;; serializes the received data into bytes according to WIRE protocol,
;; and writes such bytes to the underlying file descriptor or binary output port.
;;
;; Constructor argument out must be one of:
;;   a fixnum >= 0, indicating the file descriptor to write to
;;   a binary output port
;;   #f, indicating that cbor-writer is closed
;;   a box containing one of the cases above
;;
;; Note: as per writer contract, by default closing a cbor-writer does NOT close
;; the underlying file descriptor or binary output port,
;; because it is a pre-existing, borrowed resource passed to the constructor.
;;
;; If a cbor-writer should take ownership of the file descriptor or binary output port
;; passed to the constructor, then pass a truish value as the optional argument close-out?
(define make-cbor-writer
  (case-lambda
    ((out close-out?)
      (%validate-out out)
      (let* ((out-box   (if (box? out) out (box out)))
             (out-unbox (if (box? out) (unbox out) out))
             (tx        (%make-cbor-writer out-box (and out-unbox (bytespan)) close-out?)))
        (when out-unbox
          ;; immediately send cbor magic, telling our cbor protocol to any receiver.
          (let ((wbuf (cbor-writer-wbuf tx)))
            (cbor-put-magic-to-bytespan wbuf)
            (put-all tx wbuf)
            (bytespan-clear! wbuf)))
        tx))
    ((out)
      (make-cbor-writer out #f))))


(define (%validate-out out)
  (let ((out (if (box? out) (unbox out) out)))
    (cond
      ((not out)
        (void))
      ((number? out)
        (assert* 'make-cbor-writer (fixnum? out))
        (assert* 'make-cbor-writer (fx>=? out 0)))
      ((port? out)
        (assert* 'make-cbor-writer (output-port? out))
        (assert* 'make-cbor-writer (binary-port? out)))
      (else
        (raise-assertf 'make-cbor-writer "(or (number? out) (port? out) (not out))"  out)))))


;; called by (writer-close)
;; Helps detecting end-of-file at the receiver side.
(define (%cbor-writer-close tx)
  (%close-box (cbor-writer-out-box tx) (cbor-writer-close-out? tx)))


;; return the cbor-writer output port, file descriptor, or #f
(define (%cbor-writer-out-unbox rx)
  (let ((out (unbox (cbor-writer-out-box rx))))
    (unless out
      ;; automatically close when out is #f. sets eof? flag
      (writer-close rx))
    out))


(define (unbox-raise-if-closed tx)
  (let ((out (%cbor-writer-out-unbox tx)))
    (unless out
      (raise-errorf 'cbor-writer-put "~s is already closed" tx))
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


;; called by (cbor-writer-put) and (writer-put)
(define (%cbor-writer-put tx datum)
  (unbox-raise-if-closed tx)
  #|
  (let* ((wbuf         (cbor-writer-wbuf tx))
         (serialized-n (cbor-put-to-bytespan wbuf datum)))
    (unless serialized-n
      (raise-errorf 'cbor-writer-put "unsupported datum: ~s" datum))
    (put-all tx wbuf)
    (bytespan-clear! wbuf))
  |#
  )
