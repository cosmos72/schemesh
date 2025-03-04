;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix io (0 8 0))
  (export
    open-fd-redir-binary-input-port open-fd-redir-binary-input/output-port open-fd-redir-binary-output-port
    open-fd-redir-textual-input-port open-fd-redir-textual-input/output-port open-fd-redir-textual-output-port)
  (import
    (rnrs)
    (only (chezscheme) include make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port
                               make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port)
    (only (schemesh bootstrap) assert*)
    (schemesh containers bytespan)
    (schemesh containers utf8b utils)
    (only (schemesh posix fd)  fd-read fd-write))


(include "posix/io-utf8b.ss")


;; binary input and/or output port reading from/writing to a file descriptor returned by a closure.
(define-record-type bport
  (fields
    (immutable proc)  ; fd-proc
    (mutable   pos))  ; position
  (nongenerative #{bport n9keti0sj3bih8de7dh3r7n4y-0}))


(define (bport-read p bv start n)
  (assert* 'fd-redir-binary-input-port-read (fx>=? start 0))
  (assert* 'fd-redir-binary-input-port-read (fx>=? n 0))
  (assert* 'fd-redir-binary-input-port-read (fx<=? (fx+ start n) (bytevector-length bv)))
  (if (fxzero? n)
    0
    (let ((ret (fd-read ((bport-proc p)) bv start (fx+ start n))))
      (bport-pos-set! p (fx+ ret (bport-pos p)))
      ret)))


(define (bport-write p bv start n)
  (assert* 'fd-redir-binary-output-port-write (fx>=? start 0))
  (assert* 'fd-redir-binary-output-port-write (fx>=? n 0))
  (assert* 'fd-redir-binary-output-port-write (fx<=? (fx+ start n) (bytevector-length bv)))
  (if (fxzero? n)
    0
    (let ((ret (fd-write ((bport-proc p)) bv start (fx+ start n))))
      (bport-pos-set! p (fx+ ret (bport-pos p)))
      ret)))


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
