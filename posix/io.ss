;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh posix io (0 8 0))
  (export
    open-fd-redir-input-port open-fd-redir-input/output-port open-fd-redir-output-port)
  (import
    (rnrs)
    (only (chezscheme)         make-custom-binary-input-port make-custom-binary-input/output-port make-custom-binary-output-port)
    (only (schemesh bootstrap) assert*)
    (only (schemesh posix fd)  fd-read fd-write))


;; binary input and/or output port reading from/writing to a file descriptor returned by a closure.
(define-record-type fdport
  (fields
    (immutable proc)  ; fd-proc
    (mutable   pos))  ; position
  (nongenerative #{fdport n9keti0sj3bih8de7dh3r7n4y-0}))


(define (fdport-read p bv start n)
  (assert* 'fd-redir-input-port-read (fx>=? start 0))
  (assert* 'fd-redir-input-port-read (fx>=? n 0))
  (assert* 'fd-redir-input-port-read (fx<=? (fx+ start n) (bytevector-length bv)))
  (if (fxzero? n)
    0
    (let ((ret (fd-read ((fdport-proc p)) bv start (fx+ start n))))
      (fdport-pos-set! p (fx+ ret (fdport-pos p)))
      ret)))


(define (fdport-write p bv start n)
  (assert* 'fd-redir-output-port-write (fx>=? start 0))
  (assert* 'fd-redir-output-port-write (fx>=? n 0))
  (assert* 'fd-redir-output-port-write (fx<=? (fx+ start n) (bytevector-length bv)))
  (if (fxzero? n)
    0
    (let ((ret (fd-write ((fdport-proc p)) bv start (fx+ start n))))
      (fdport-pos-set! p (fx+ ret (fdport-pos p)))
      ret)))


;; create and return a binary input port that redirectably reads from a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-input-port name fd-proc)
  (assert* 'open-fd-redir-input-port (procedure? fd-proc))
  (let ((fdport (make-fdport fd-proc 0)))
    (make-custom-binary-input-port
      name
      (lambda (bv start n) (fdport-read fdport bv start n))
      (lambda ()           (fdport-pos  fdport))
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd-in-proc and fd-out-proc must be no-argument procedures that return an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-input/output-port
  (case-lambda
    ((name fd-in-proc fd-out-proc)
      (assert* 'open-fd-redir-input-port (procedure? fd-in-proc))
      (assert* 'open-fd-redir-input-port (procedure? fd-out-proc))
      (let ((fd-in-port (make-fdport fd-in-proc 0))
            (fd-out-port (make-fdport fd-out-proc 0)))
        (make-custom-binary-input/output-port
          name
          (lambda (bv start n) (fdport-read fd-in-port bv start n))
          (lambda (bv start n) (fdport-write fd-out-port bv start n))
          #f    ; no pos: there is no "single" position, in/out file descriptors may differ and may be non-seekable
          #f    ; no pos-set!
          #f))) ; do nothing on close
    ((name fd-proc)
      (open-fd-redir-input/output-port name fd-proc fd-proc))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-output-port name fd-proc)
  (assert* 'open-fd-redir-output-port (procedure? fd-proc))
  (let ((fdport (make-fdport fd-proc 0)))
    (make-custom-binary-output-port
      name
      (lambda (bv start n) (fdport-write fdport bv start n))
      (lambda ()           (fdport-pos  fdport))
      #f    ; no pos-set!
      #f))) ; do nothing on close


) ; close library
