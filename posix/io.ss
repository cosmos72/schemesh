;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix io (0 9 0))
  (export
    port->utf8b-port fd->port file->port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)  assertion-violationf clear-input-port clear-output-port enum-set? fx1+ fx1-
                        get-bytevector-some! include input-port-ready?
                        logbit? make-input-port make-input/output-port make-output-port mark-port-closed!
                        port-length port-name procedure-arity-mask record-writer

                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-port-bol!
                        set-textual-port-input-buffer!  set-textual-port-input-index!  set-textual-port-input-size!
                        set-textual-port-output-buffer! set-textual-port-output-index! set-textual-port-output-size!

                        textual-port-input-buffer       textual-port-input-index       textual-port-input-size
                        textual-port-output-buffer      textual-port-output-index      textual-port-output-size)
    (only (schemesh bootstrap)              assert*)
    (schemesh containers bytespan)
    (only (schemesh containers list)        plist? plist-ref)
    (only (schemesh containers string)      substring-move!)
    (only (schemesh containers utf8b)       integer->char* utf8b->string utf8b->string-copy!)
    (only (schemesh containers utf8b utils) bytespan-insert-left/char! bytespan-insert-right/char!
                                            bytespan-insert-right/string! bytespan-ref/char)
    (only (schemesh conversions)            text->string)
    (only (schemesh posix fd)               fd-close fd-seek fd-read fd-write file->fd))



(define (b-mode->input-buffer-size b-mode)
  (case b-mode
    ((none) 1) ;; Chez Scheme custom ports do not support zero input-buffer-size
    ((line) 128)
    (else   8192)))


(define (b-mode->output-buffer-size b-mode)
  (case b-mode
    ((none) 0)
    ((line) 128)
    (else   8192)))



(define (%set-binary-buffer-mode! port b-mode)
  (when (input-port? port)
    (let ((in-buffer-size (b-mode->input-buffer-size b-mode)))
      (set-binary-port-input-buffer! port (make-bytevector in-buffer-size))
      (set-binary-port-input-size!   port in-buffer-size)
      (set-binary-port-input-index!  port in-buffer-size)))
  (when (output-port? port)
    (let ((out-buffer-size (b-mode->output-buffer-size b-mode)))
      (set-binary-port-output-buffer! port (make-bytevector out-buffer-size))
      (set-binary-port-output-size!   port (fxmax 0 (fx1- out-buffer-size))) ; leave 1 byte for (put-u8)
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


;; create and return a binary input and/or output port that reads from and/or writes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define (fd->binary-port fd dir b-mode name proc-on-close)
  (assert* 'fd->binary-port (fx>=? fd 0))
  (assert* 'fd->binary-port (buffer-mode? b-mode))
  (let ((p
    (case dir
      ((read)
        (make-custom-binary-input-port
          name
          (lambda (bv start n) (bport-read fd bv start n))
          (lambda ()           (bport-seek fd 0   'seek-cur))
          (lambda (pos)        (bport-seek fd pos 'seek-set))
          proc-on-close))
      ((rw)
        (make-custom-binary-input/output-port
          name
          (lambda (bv start n) (bport-read fd bv start n))
          (lambda (bv start n) (bport-write fd bv start n))
          (lambda ()           (bport-seek fd 0   'seek-cur))
          (lambda (pos)        (bport-seek fd pos 'seek-set))
          proc-on-close))
      ((write)
        (make-custom-binary-output-port
          name
          (lambda (bv start n) (bport-write fd bv start n))
          (lambda ()           (bport-seek fd 0   'seek-cur))
          (lambda (pos)        (bport-seek fd pos 'seek-set))
          proc-on-close))
      (else
        (assert* 'fd->binary-port (memq dir '(read write rw)))))))
    (%set-binary-buffer-mode! p b-mode)
    p))


(include "posix/io-utf8b.ss")


;; create and return a binary or textual input and/or output port that reads from and/or writes to a file descriptor.
;;
;; Arguments:
;;   mandatory fd             must be an unsigned fixnum corresponding to an open file descriptor.
;;   optional dir             must be one of: 'read 'write 'rw and defaults to 'rw
;;   optional ?transcoder-sym must be one of: #f 'binary 'text 'utf8b and defaults to #f
;;   optional name            must be a string and defaults to (string-append "fd " (number->string fd))
;;   optional b-mode          must be a buffer-mode and defaults to 'block
;;   optional proc-on-close   must be a #f or a procedure and defaults to #f
(define fd->port
  (case-lambda
    ((fd dir ?transcoder-sym b-mode name proc-on-close)
      (case ?transcoder-sym
        ((#f binary)
          (fd->binary-port fd dir b-mode name proc-on-close))
        ((text utf8b)
          (fd->textual-port fd dir b-mode name proc-on-close))
        (else
          (assert* 'fd->port (memq ?transcoder-sym '(#f binary text utf8b))))))
    ((fd dir ?transcoder-sym b-mode name)
      (fd->port fd dir ?transcoder-sym b-mode name #f))
    ((fd dir ?transcoder-sym b-mode)
      (fd->port fd dir ?transcoder-sym b-mode (string-append "fd " (number->string fd)) #f))
    ((fd dir ?transcoder-sym)
      (fd->port fd dir ?transcoder-sym (buffer-mode block)))
    ((fd dir)
      (fd->port fd dir 'binary (buffer-mode block)))
    ((fd)
      (fd->port fd 'rw 'binary (buffer-mode block)))))


;; create and return a binary or textual input and/or output port that reads from/writes to
;; specified file path.
;;
;; Arguments:
;;   mandatory path           must be a string, bytevector, bytespan or charspan.
;;   optional dir             must be one of: 'read 'write 'rw and defaults to 'rw
;;   optional flags           must be a list containing zero or more: 'create 'truncate 'append
;;   optional ?transcoder-sym must be one of: #f 'binary 'text 'utf8b and defaults to #f
;;   optional b-mode          must be a buffer-mode and defaults to 'block
(define file->port
  (case-lambda
    ((path dir flags ?transcoder-sym b-mode)
      (assert* 'file->port (memq ?transcoder-sym '(#f binary text utf8b)))
      (assert* 'file->port (buffer-mode? b-mode))
      (let ((fd (file->fd path dir flags)))
        (fd->port fd dir ?transcoder-sym b-mode (text->string path) (lambda () (fd-close fd)))))
    ((path dir flags ?transcoder-sym)
      (file->port path dir flags ?transcoder-sym (buffer-mode block)))
    ((path dir flags)
      (file->port path dir flags 'binary (buffer-mode block)))
    ((path dir)
      (file->port path dir '() 'binary (buffer-mode block)))
    ((path)
      (file->port path 'rw '() 'binary (buffer-mode block)))))


;; customize how "tport" objects are printed
(record-writer (record-type-descriptor tport)
  (lambda (tp port writer)
    (display "#<tport " port)
    (display (tport-bin-port tp) port)
    (display ">" port)))


) ; close library
