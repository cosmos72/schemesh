;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix io (0 8 1))
  (export
    make-utf8b-textual-input-port make-utf8b-textual-input/output-port make-utf8b-textual-output-port

    open-fd-redir-binary-input-port open-fd-redir-binary-input/output-port open-fd-redir-binary-output-port
    open-fd-redir-textual-input-port open-fd-redir-textual-input/output-port open-fd-redir-textual-output-port)
  (import
    (rnrs)
    (only (chezscheme)         make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port
                               make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port
                               console-error-port fx1+ include port-name)
    (only (schemesh bootstrap) assert*)
    (schemesh containers bytespan)
    (only (schemesh containers utf8b)       utf8b->string-copy!)
    (only (schemesh containers utf8b utils) bytespan-insert-right/string!)
    (only (schemesh posix fd)               fd-read fd-write-all))


(include "posix/io-utf8b.ss")


;; binary input and/or output port reading from/writing to a file descriptor returned by a closure.
(define-record-type bport
  (fields
    (immutable proc)  ; fd-proc
    (mutable   pos))  ; position
  (nongenerative #{bport n9keti0sj3bih8de7dh3r7n4y-0}))


(define (bport-read p bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (let ((ret (fd-read ((bport-proc p)) bv start (fx+ start n))))
      (cond
        ((and (integer? ret) (> ret 0))
          (bport-pos-set! p (+ ret (bport-pos p)))
          ret)
        (else 0)))
    0))


(define (bport-write p bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (begin
      (fd-write-all ((bport-proc p)) bv start (fx+ start n))
      (bport-pos-set! p (fx+ n (bport-pos p)))
      n)
    0))


;; create and return a binary input port that redirectably reads from a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-binary-input-port name fd-proc)
  (assert* 'open-fd-redir-binary-input-port (procedure? fd-proc))
  (let ((port (make-bport fd-proc 0)))
    (make-custom-binary-input-port
      name
      (lambda (bv start n) (bport-read port bv start n))
      (lambda ()           (bport-pos  port))
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd-in-proc and fd-out-proc must be no-argument procedures that return an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-input/output-port
  (case-lambda
    ((name fd-in-proc fd-out-proc)
      (assert* 'open-fd-redir-binary-input-port (procedure? fd-in-proc))
      (assert* 'open-fd-redir-binary-input-port (procedure? fd-out-proc))
      (let ((in-port (make-bport fd-in-proc 0))
            (out-port (make-bport fd-out-proc 0)))
        (make-custom-binary-input/output-port
          name
          (lambda (bv start n) (bport-read in-port bv start n))
          (lambda (bv start n) (bport-write out-port bv start n))
          #f    ; no pos: there is no "single" position, in/out file descriptors may differ and may be non-seekable
          #f    ; no pos-set!
          #f))) ; do nothing on close
    ((name fd-proc)
      (open-fd-redir-binary-input/output-port name fd-proc fd-proc))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-binary-output-port name fd-proc)
  (assert* 'open-fd-redir-binary-output-port (procedure? fd-proc))
  (let ((port (make-bport fd-proc 0)))
    (make-custom-binary-output-port
      name
      (lambda (bv start n) (bport-write port bv start n))
      (lambda ()           (bport-pos  port))
      #f    ; no pos-set!
      #f))) ; do nothing on close

) ; close library
