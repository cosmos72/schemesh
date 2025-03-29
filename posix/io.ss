;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix io (0 8 2))
  (export
    make-utf8b-input-port make-utf8b-input/output-port make-utf8b-output-port

    open-fd-redir-binary-input-port open-fd-redir-binary-input/output-port open-fd-redir-binary-output-port
    open-fd-redir-utf8b-input-port open-fd-redir-utf8b-input/output-port open-fd-redir-utf8b-output-port

    open-file-binary-input-port open-file-utf8b-input-port)
  (import
    (rnrs)
    (only (chezscheme)  enum-set? fx1+ include port-name
                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-textual-port-input-buffer!  set-textual-port-input-index!  set-textual-port-input-size!
                        set-textual-port-output-buffer! set-textual-port-output-index! set-textual-port-output-size!)
    (only (schemesh bootstrap) assert*)
    (schemesh containers bytespan)
    (only (schemesh containers utf8b)       utf8b->string utf8b->string-copy!)
    (only (schemesh containers utf8b utils) bytespan-insert-right/string!)
    (only (schemesh posix fd)               fd-close fd-seek fd-read fd-write open-file-fd))


(define (%set-buffer-mode! port b-mode)
  (let ((buffer-size (case b-mode
                       ((none) 1) ; Chez Scheme streams do not support zero buffer-size
                       ((line) 128)
                       (else   8192))))
    (when (textual-port? port)
      (when (input-port? port)
        (set-textual-port-input-buffer! port (make-string buffer-size))
        (set-textual-port-input-size!   port 0)
        (set-textual-port-input-index!  port 0))
      (when (output-port? port)
        (set-textual-port-output-buffer! port (make-string buffer-size))
        (set-textual-port-output-size!   port 0)
        (set-textual-port-output-index!  port 0)))
    (when (binary-port? port)
      (when (input-port? port)
        (set-binary-port-input-buffer! port (make-bytevector buffer-size))
        (set-binary-port-input-size!   port 0)
        (set-binary-port-input-index!  port 0))
      (when (output-port? port)
        (set-binary-port-output-buffer! port (make-bytevector buffer-size))
        (set-binary-port-output-size!   port 0)
        (set-binary-port-output-index!  port 0))))
  port)


;; binary input and/or output port reading from/writing to a file descriptor returned by a closure.
(define-record-type bport
  (fields
    (immutable proc))  ; fd-proc
  (nongenerative bport-7c46d04b-34f4-4046-b5c7-b63753c1be39))

(define (bport-fd p)
  ((bport-proc p)))

(define (bport-read p bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (let ((ret (fd-read (bport-fd p) bv start (fx+ start n))))
      (if (and (integer? ret) (> ret 0))
        ret
        0))
    0))


(define (bport-write p bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (let ((ret (fd-write (bport-fd p) bv start (fx+ start n))))
      (if (and (integer? ret) (>= ret 0))
        ret
        0))
    0))


(define (bport-seek p pos from)
  (let ((ret (fd-seek (bport-fd p) pos from)))
    (if (and (integer? ret) (>= ret 0))
      ret
      0)))


;; create and return a binary input port that redirectably reads from a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-input-port
  (case-lambda
    ((name fd-proc b-mode proc-on-close)
      (assert* 'open-fd-redir-binary-input-port (procedure? fd-proc))
      (assert* 'open-fd-redir-binary-input-port (buffer-mode? b-mode))
      (let* ((bport (make-bport fd-proc))
             (ret   (make-custom-binary-input-port
                      name
                      (lambda (bv start n) (bport-read bport bv start n))
                      (lambda ()           (bport-seek bport 0   'seek-cur))
                      (lambda (pos)        (bport-seek bport pos 'seek-set))
                      proc-on-close)))
        (%set-buffer-mode! ret b-mode)))

    ((name fd-proc b-mode)
      (open-fd-redir-binary-input-port name fd-proc b-mode #f))

    ((name fd-proc)
      (open-fd-redir-binary-input-port name fd-proc (buffer-mode block) #f))))


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd-proc must be no-argument procedures that return an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-input/output-port
  (case-lambda
    ((name fd-proc b-mode proc-on-close)
      (assert* 'open-fd-redir-binary-input/output-port (procedure? fd-proc))
      (assert* 'open-fd-redir-binary-input/output-port (buffer-mode? b-mode))
      (let* ((bport (make-bport fd-proc))
             (ret   (make-custom-binary-input/output-port
                      name
                      (lambda (bv start n) (bport-read bport bv start n))
                      (lambda (bv start n) (bport-write bport bv start n))
                      (lambda ()           (bport-seek bport 0   'seek-cur))
                      (lambda (pos)        (bport-seek bport pos 'seek-set))
                      proc-on-close)))
        (%set-buffer-mode! ret b-mode)))

    ((name fd-proc b-mode)
      (open-fd-redir-binary-input/output-port name fd-proc b-mode #f))

    ((name fd-proc)
      (open-fd-redir-binary-input/output-port name fd-proc (buffer-mode block) #f))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define open-fd-redir-binary-output-port
  (case-lambda
    ((name fd-proc b-mode proc-on-close)
      (assert* 'open-fd-redir-binary-output-port (procedure? fd-proc))
      (assert* 'open-fd-redir-binary-output-port (buffer-mode? b-mode))
      (let* ((bport (make-bport fd-proc))
             (ret   (make-custom-binary-output-port
                      name
                      (lambda (bv start n) (bport-write bport bv start n))
                      (lambda ()           (bport-seek bport 0   'seek-cur))
                      (lambda (pos)        (bport-seek bport pos 'seek-set))
                      proc-on-close)))
        (%set-buffer-mode! ret b-mode)))

    ((name fd-proc b-mode)
      (open-fd-redir-binary-output-port name fd-proc b-mode #f))

    ((name fd-proc)
      (open-fd-redir-binary-output-port name fd-proc (buffer-mode block) #f))))


;; create and return a binary input port that reads
;; bytes from specified file path.
;;
;; path must be a string or bytevector.
(define open-file-binary-input-port
  (case-lambda
    ((path f-options b-mode)
      (assert* 'open-file-binary-input-port (enum-set? f-options))
      (assert* 'open-file-binary-input-port (buffer-mode? b-mode))
      (let ((name (if (string? path) path (utf8b->string path)))
            (fd   (open-file-fd path 'read)))
        (open-fd-redir-binary-input-port
          name
          (lambda () fd)
          b-mode
          (lambda () (fd-close fd)))))
    ((path)
      (open-file-binary-input-port path (file-options) (buffer-mode block)))))


(include "posix/io-utf8b.ss")

) ; close library
