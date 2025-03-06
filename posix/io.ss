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
    (only (chezscheme)         make-custom-binary-input-port  make-custom-binary-input/output-port  make-custom-binary-output-port
                               make-custom-textual-input-port make-custom-textual-input/output-port make-custom-textual-output-port
                               fx1+ include port-name
                               set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                               set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                               set-textual-port-input-buffer!  set-textual-port-input-index!  set-textual-port-input-size!
                               set-textual-port-output-buffer! set-textual-port-output-index! set-textual-port-output-size!)
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
(define open-fd-redir-binary-input-port
  (case-lambda
    ((name fd-proc buffer-size)
      (assert* 'open-fd-redir-binary-input-port (procedure? fd-proc))
      (when buffer-size
        (assert* 'open-fd-redir-binary-input-port (fx>=? buffer-size 0)))
      (let* ((bport (make-bport fd-proc 0))
             (ret   (make-custom-binary-input-port
                      name
                      (lambda (bv start n) (bport-read bport bv start n))
                      (lambda ()           (bport-pos  bport))
                      #f    ; no pos-set!
                      #f))) ; do nothing on close
        (when buffer-size
          (set-binary-port-input-size!   ret 0)
          (set-binary-port-input-index!  ret 0)
          (set-binary-port-input-buffer! ret (make-bytevector buffer-size)))
        ret))
    ((name fd-proc)
      (open-fd-redir-binary-input-port name fd-proc #f))))


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd-proc must be no-argument procedures that return an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-input/output-port
  (case-lambda
    ((name fd-proc buffer-size)
      (assert* 'open-fd-redir-binary-input/output-port (procedure? fd-proc))
      (when buffer-size
        (assert* 'open-fd-redir-binary-input/output-port (fx>=? buffer-size 0)))
      (let* ((bport (make-bport fd-proc 0))
             (ret   (make-custom-binary-input/output-port
                      name
                      (lambda (bv start n) (bport-read bport bv start n))
                      (lambda (bv start n) (bport-write bport bv start n))
                      #f    ; no pos: there is no "single" position, in/out file descriptors may differ and may be non-seekable
                      #f    ; no pos-set!
                      #f))) ; do nothing on close
        (when buffer-size
          (set-binary-port-input-size!   ret 0)
          (set-binary-port-input-index!  ret 0)
          (set-binary-port-input-buffer! ret (make-bytevector buffer-size))
          (set-binary-port-output-index!  ret 0)
          (set-binary-port-output-size!   ret 0)
          (set-binary-port-output-buffer! ret (make-bytevector buffer-size)))
        ret))
    ((name fd-proc)
      (open-fd-redir-binary-input/output-port name fd-proc #f))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-output-port
  (case-lambda
    ((name fd-proc buffer-size)
      (assert* 'open-fd-redir-binary-output-port (procedure? fd-proc))
      (let* ((bport (make-bport fd-proc 0))
             (ret   (make-custom-binary-output-port
                      name
                      (lambda (bv start n) (bport-write bport bv start n))
                      (lambda ()           (bport-pos  bport))
                      #f    ; no pos-set!
                      #f))) ; do nothing on close
        (when buffer-size
          (set-binary-port-output-size!   ret 0)
          (set-binary-port-output-index!  ret 0)
          (set-binary-port-output-buffer! ret (make-bytevector buffer-size)))
        ret))
    ((name fd-proc)
      (open-fd-redir-binary-input-port name fd-proc #f))))

) ; close library
