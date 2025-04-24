;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix io (0 8 3))
  (export
    make-utf8b-input-port make-utf8b-input/output-port make-utf8b-output-port

    fd->binary-input-port fd->binary-input/output-port fd->binary-output-port
    fd->textual-input-port fd->textual-input/output-port fd->textual-output-port

    open-file-binary-input-port open-file-textual-input-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)  assertion-violationf clear-input-port clear-output-port enum-set? fx1+ fx1- include input-port-ready?
                        make-input-port make-input/output-port make-output-port mark-port-closed! port-length port-name record-writer

                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-port-bol!
                        set-textual-port-input-buffer!  set-textual-port-input-index!  set-textual-port-input-size!
                        set-textual-port-output-buffer! set-textual-port-output-index! set-textual-port-output-size!

                        textual-port-input-buffer       textual-port-input-index       textual-port-input-size
                        textual-port-output-buffer      textual-port-output-index      textual-port-output-size
                        with-interrupts-disabled)
    (only (schemesh bootstrap)              assert* trace-define)
    (schemesh containers bytespan)
    (only (schemesh containers string)      substring-move!)
    (only (schemesh containers utf8b)       integer->char* utf8b->string utf8b->string-copy!)
    (only (schemesh containers utf8b utils) bytespan-insert-left/char! bytespan-insert-right/char!
                                            bytespan-insert-right/string! bytespan-ref/char)
    (only (schemesh posix fd)               fd-close fd-seek fd-read fd-write open-file-fd))



(define (b-mode->input-buffer-size b-mode)
  (case b-mode
    ((none) 1) ;; Chez Scheme custom ports do not support zero input-buffer-size
    ((line) 128)
    (else   4096)))


(define (b-mode->output-buffer-size b-mode)
  (case b-mode
    ((none) 0)
    ((line) 128)
    (else   4096)))



(define (%set-binary-buffer-mode! port b-mode)
  (when (input-port? port)
    (let ((in-buffer-size (b-mode->input-buffer-size b-mode)))
      (set-binary-port-input-buffer! port (make-bytevector in-buffer-size))
      (set-binary-port-input-size!   port in-buffer-size)
      (set-binary-port-input-index!  port in-buffer-size)))
  (when (output-port? port)
    (let ((out-buffer-size (b-mode->output-buffer-size b-mode)))
      (set-binary-port-output-buffer! port (make-bytevector out-buffer-size))
      (set-binary-port-output-size!   port out-buffer-size)
      (set-binary-port-output-index!  port 0)))
  port)


(define (bport-read fd bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (let ((ret (fd-read fd bv start (fx+ start n))))
      (if (and (integer? ret) (> ret 0))
        ret
        0))
    0))


(define (bport-write fd bv start n)
  (if (and (bytevector? bv) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (bytevector-length bv))))
    (let ((ret (fd-write fd bv start (fx+ start n))))
      (if (and (integer? ret) (>= ret 0))
        ret
        0))
    0))


(define (bport-seek fd pos from)
  (let ((ret (fd-seek fd pos from)))
    (if (and (integer? ret) (>= ret 0))
      ret
      0)))


;; create and return a binary input port that redirectably reads from a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->binary-input-port
  (case-lambda
    ((name fd b-mode proc-on-close)
      (assert* 'fd->binary-input-port (fx>=? fd 0))
      (assert* 'fd->binary-input-port (buffer-mode? b-mode))
      (let ((ret (make-custom-binary-input-port
                    name
                    (lambda (bv start n) (bport-read fd bv start n))
                    (lambda ()           (bport-seek fd 0   'seek-cur))
                    (lambda (pos)        (bport-seek fd pos 'seek-set))
                    proc-on-close)))
        (%set-binary-buffer-mode! ret b-mode)))

    ((name fd b-mode)
      (fd->binary-input-port name fd b-mode #f))

    ((name fd)
      (fd->binary-input-port name fd (buffer-mode block) #f))))


;; create and return a binary input/output port that redirectably reads from/writes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->binary-input/output-port
  (case-lambda
    ((name fd b-mode proc-on-close)
      (assert* 'fd->binary-input/output-port (fx>=? fd 0))
      (assert* 'fd->binary-input/output-port (buffer-mode? b-mode))
      (let ((ret (make-custom-binary-input/output-port
                   name
                   (lambda (bv start n) (bport-read fd bv start n))
                   (lambda (bv start n) (bport-write fd bv start n))
                   (lambda ()           (bport-seek fd 0   'seek-cur))
                   (lambda (pos)        (bport-seek fd pos 'seek-set))
                   proc-on-close)))
        (%set-binary-buffer-mode! ret b-mode)))

    ((name fd b-mode)
      (fd->binary-input/output-port name fd b-mode #f))

    ((name fd)
      (fd->binary-input/output-port name fd (buffer-mode block) #f))))


;; create and return a binary output port that redirectably writes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->binary-output-port
  (case-lambda
    ((name fd b-mode proc-on-close)
      (assert* 'fd->binary-output-port (fx>=? fd 0))
      (assert* 'fd->binary-output-port (buffer-mode? b-mode))
      (let ((ret (make-custom-binary-output-port
                   name
                   (lambda (bv start n) (bport-write fd bv start n))
                   (lambda ()           (bport-seek fd 0   'seek-cur))
                   (lambda (pos)        (bport-seek fd pos 'seek-set))
                   proc-on-close)))
        (%set-binary-buffer-mode! ret b-mode)))

    ((name fd b-mode)
      (fd->binary-output-port name fd b-mode #f))

    ((name fd)
      (fd->binary-output-port name fd (buffer-mode block) #f))))


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
        (fd->binary-input-port name fd b-mode (lambda () (fd-close fd)))))
    ((path)
      (open-file-binary-input-port path (file-options) (buffer-mode block)))))


(include "posix/io-utf8b.ss")

) ; close library
