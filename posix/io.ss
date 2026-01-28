;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k posix io (0 9 3))
  (export
    fd->port file->port port->utf8b-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)  assertion-violationf clear-input-port clear-output-port enum-set? fx1+ fx1-
                        get-bytevector-some! include input-port-ready?
                        logbit? make-input-port make-input/output-port make-output-port mark-port-closed!
                        port-closed? port-length port-name procedure-arity-mask record-writer

                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-port-bol!
                        set-textual-port-input-buffer!  set-textual-port-input-index!  set-textual-port-input-size!
                        set-textual-port-output-buffer! set-textual-port-output-index! set-textual-port-output-size!

                        textual-port-input-buffer       textual-port-input-index       textual-port-input-size
                        textual-port-output-buffer      textual-port-output-index      textual-port-output-size

                        void)
    (only (scheme2k bootstrap)              assert* debugf fx<=?* raise-errorf trace-define)
    (scheme2k containers bytespan)
    (only (scheme2k containers list)        plist? plist-ref)
    (only (scheme2k containers string)      substring-move!)
    (only (scheme2k containers utf8b)       integer->char* utf8b->string utf8b->string-copy!
                                            bytespan-insert-left/char! bytespan-insert-right/char!
                                            bytespan-insert-right/string! bytespan-ref/char)
    (only (scheme2k conversions)            text->string)
    (only (scheme2k posix fd)               fd-close fd-seek fd-read fd-write file->fd))



(define (b-mode->input-buffer-size b-mode)
  (case b-mode
    ((none) 1) ;; r6rs custom ports do not support zero input-buffer-size
    ((line) 128)
    (else   8192)))


(define (b-mode->output-buffer-size b-mode)
  (case b-mode
    ((none) 1) ;; r6rs custom ports do not support zero output-buffer-size
    ((line) 128)
    (else   8192)))



(define (%set-binary-buffer-mode! port b-mode)
  (when (input-port? port)
    (let ((cap (b-mode->input-buffer-size b-mode)))
      (set-binary-port-input-buffer! port (make-bytevector cap))
      (set-binary-port-input-size!   port cap)
      (set-binary-port-input-index!  port cap)))
  (when (output-port? port)
    (let ((cap (b-mode->output-buffer-size b-mode)))
      (set-binary-port-output-buffer! port (make-bytevector cap))
      (set-binary-port-output-size!   port (fxmax 0 (fx1- cap))) ; leave 1 byte for (put-u8)
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
          (lambda (bv start n) (bport-read  fd bv start n))
          (lambda ()           (bport-seek  fd 0   'seek-cur))
          (lambda (pos)        (bport-seek  fd pos 'seek-set))
          proc-on-close))
      ((rw)
        (make-custom-binary-input/output-port
          name
          (lambda (bv start n) (bport-read  fd bv start n))
          (lambda (bv start n) (bport-write fd bv start n))
          (lambda ()           (bport-seek  fd 0   'seek-cur))
          (lambda (pos)        (bport-seek  fd pos 'seek-set))
          proc-on-close))
      ((write)
        (make-custom-binary-output-port
          name
          (lambda (bv start n) (bport-write fd bv start n))
          (lambda ()           (bport-seek  fd 0   'seek-cur))
          (lambda (pos)        (bport-seek  fd pos 'seek-set))
          proc-on-close))
      (else
        (assert* 'fd->binary-port (memq dir '(read write rw)))))))
    (%set-binary-buffer-mode! p b-mode)
    p))


(include "posix/io-utf8b.ss")


;; if thunk returns abnormally i.e. raises condition or exits via a continuation
;; then call (proc-on-abnormal-return)
(define (dynamic-protect thunk proc-on-abnormal-return)
  (let ((ok? #f))
    (dynamic-wind
      void
      (lambda ()
        (let ((ret (thunk)))
          (set! ok? #t)
          ret))
      (lambda ()
        (unless ok?
          (proc-on-abnormal-return))))))


#| ; unused
(define-syntax dynamic-protect-macro
  (syntax-rules ()
    ((_ form proc-on-abnormal-return)
      (if proc-on-abnormal-return
        (dynamic-protect (lambda () form) proc-on-abnormal-return)
        form))))
|#

;; create and return a binary or textual input and/or output port that reads from and/or writes to a file descriptor.
;;
;; Arguments:
;;   mandatory fd             must be an unsigned fixnum corresponding to an open file descriptor.
;;   optional dir             must be one of: 'read 'write 'rw and defaults to 'rw
;;   optional transcoder-sym  must be one of: 'binary 'textual 'utf8b and defaults to 'textual
;;   optional b-mode          must be a buffer-mode and defaults to 'block
;;   optional name            must be a string and defaults to (string-append "fd " (number->string fd))
;;   optional proc-on-close   must be a #f or a procedure and defaults to #f
(define fd->port
  (case-lambda
    ((fd dir transcoder-sym b-mode name proc-on-close)
      (case transcoder-sym
        ((binary)
          (fd->binary-port fd dir b-mode name proc-on-close))
        ((textual utf8b)
          (fd->textual-port fd dir b-mode name proc-on-close))
        (else
          (let ((allowed-transcoder-syms '(binary textual utf8b)))
            (assert* 'fd->port (memq transcoder-sym allowed-transcoder-syms))))))
    ((fd dir transcoder-sym b-mode name)
      (fd->port fd dir transcoder-sym b-mode name #f))
    ((fd dir transcoder-sym b-mode)
      (fd->port fd dir transcoder-sym b-mode (string-append "fd " (number->string fd)) #f))
    ((fd dir transcoder-sym)
      (fd->port fd dir transcoder-sym (buffer-mode block)))
    ((fd dir)
      (fd->port fd dir 'textual (buffer-mode block)))
    ((fd)
      (fd->port fd 'rw 'textual (buffer-mode block)))))


;; create and return a binary or textual input and/or output port that reads from/writes to
;; specified file path.
;;
;; Arguments:
;;   mandatory path           must be a string, bytevector, bytespan or charspan.
;;   optional dir             must be one of: 'read 'write 'rw and defaults to 'read
;;   optional flags           must be a list containing zero or more: 'create 'truncate 'append
;;   optional transcoder-sym  must be one of: 'binary 'textual 'utf8b and defaults to 'textual
;;   optional b-mode          must be a buffer-mode and defaults to 'block
(define file->port
  (case-lambda
    ((path dir flags transcoder-sym b-mode)
      (assert* 'file->port (memq transcoder-sym '(binary textual utf8b)))
      (assert* 'file->port (buffer-mode? b-mode))
      (let* ((fd (file->fd path dir flags))
             (proc-on-close (lambda () (fd-close fd))))
        (dynamic-protect
          (lambda ()
            (fd->port fd dir transcoder-sym b-mode (text->string path) proc-on-close))
          proc-on-close)))
    ((path dir flags transcoder-sym)
      (file->port path dir flags transcoder-sym (buffer-mode block)))
    ((path dir flags)
      (file->port path dir flags 'textual (buffer-mode block)))
    ((path dir)
      (file->port path dir '() 'textual (buffer-mode block)))
    ((path)
      (file->port path 'read '() 'textual (buffer-mode block)))))


;; customize how "tport" objects are printed
(record-writer (record-type-descriptor tport)
  (lambda (tp port writer)
    (display "#<tport " port)
    (display (tport-bin-port tp) port)
    (display ">" port)))


) ; close library
