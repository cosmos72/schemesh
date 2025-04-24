;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh port redir (0 8 3))
  (export
    make-redir-binary-input/output-port make-redir-textual-input/output-port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)  assertion-violationf block-read block-write char-ready? clear-input-port clear-output-port
                        file-length file-position fx1- fx1+ get-bytevector-some! get-string-some!
                        make-input/output-port mark-port-closed! put-bytevector-some record-case
                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-port-bol! set-port-output-index!
                        textual-port-output-buffer textual-port-output-index textual-port-output-size
                        unread-char with-interrupts-disabled)
    (only (schemesh bootstrap) assert* with-interrupts-enabled))


(define (b-mode->input-buffer-size b-mode)
  (case b-mode
    ((none) 1) ; Chez Scheme custom streams do not support zero input-buffer-size
    ((line) 128)
    (else   8192)))


(define (b-mode->output-buffer-size b-mode)
  (case b-mode
    ((none) 0)
    ((line) 128)
    (else   8192)))


(define (set-binary-buffer-mode! port b-mode)
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


;; create and return a binary input/output port that redirectably reads from and writes to another binary port.
;;
;; proc-nested-port must be a no-argument procedure that returns another binary port;
;; the returned binary port *may* change from one call to the next.
(define make-redir-binary-input/output-port
  (case-lambda
    ((name proc-nested-port proc-on-close)
      (assert* 'make-redir-binary-input/output-port (procedure? proc-nested-port))
      (set-binary-buffer-mode!
        (make-custom-binary-input/output-port
          name
          (lambda (bv start n)
            (get-bytevector-some! (proc-nested-port) bv start n))
          (lambda (bv start n)
            (let ((port (proc-nested-port)))
              (put-bytevector port bv start n)
              (flush-output-port port)
              n))
          (lambda ()
            (port-position (proc-nested-port)))
          (lambda (pos)
            (set-port-position! (proc-nested-port) pos))
          (case proc-on-close
            ((#f) #f)
            ((#t) (lambda () (close-port (proc-nested-port))))
            (else proc-on-close)))
        'none))

    ((name proc-nested-port)
      (make-redir-binary-input/output-port name proc-nested-port #f))))


;; create and return a textual input/output port that redirectably reads from and writes to another textual port.
;;
;; proc-nested-port must be a no-argument procedure that returns another textual port;
;; the returned textual port *may* change from one call to the next.
(define make-redir-textual-input/output-port
  (case-lambda
    ((name proc-nested-port proc-on-close out-buffer-size)
      (assert* 'make-redir-textual-input/output-port (procedure? proc-nested-port))
      (make-input/output-port
        (make-textual-input/output-port-handler name proc-nested-port proc-on-close)
        (make-string 0)
        (make-string out-buffer-size)))

    ((name proc-nested-port proc-on-close)
      (make-redir-textual-input/output-port name proc-nested-port #f 4096))

    ((name proc-nested-port)
      (make-redir-textual-input/output-port name proc-nested-port #f))))


;; create and return a handler suitable for Chez Scheme (make-input/output-port)
(define (make-textual-input/output-port-handler name proc proc-on-close)
  ;; return a closure
  (case-lambda
    ((msg p)
      ;; (debugf "redir port handler for ~s: (~s ~s)" (proc) msg p)
      (case msg
        ((char-ready?)
          (char-ready? (proc)))
        ((clear-input-port)
          (clear-input-port (proc)))
        ((clear-output-port)
          (begin ; with-interrupts-disabled
            (set-port-output-index! p 0))
          (clear-output-port (proc)))
        ((close-port)
          (mark-port-closed! p)
          (when proc-on-close (proc-on-close)))
        ((flush-output-port)
          (begin ; with-interrupts-disabled
            (let ((buf (textual-port-output-buffer p))
                  (idx (textual-port-output-index  p)))
              (block-write (proc) buf idx) ;; also flushes iop
              (unless (fxzero? idx)
                (set-port-output-index! p 0)
                (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline))))))
        ((file-position)
          (file-position (proc)))
        ((file-length)
          (file-length (proc)))
        ((peek-char)
          (peek-char (proc)))
        ((port-name)
          name)
        ((read-char)
          (read-char (proc)))
        (else
          (raise-bad-msg msg))))
    ((msg c p)
      ;; (debugf "redir port handler for ~s: (~s ~s ~s)" (proc) msg c p)
      (case msg
        ((file-position)
          (file-position (proc) p))
        ((unread-char)
          (unread-char c (proc)))
        ((write-char)
          (begin ; with-interrupts-disabled
            (let* ((iop (proc))
                   (buf (textual-port-output-buffer p))
                   (idx (textual-port-output-index p))
                   (cap (textual-port-output-size p)))
              (when (fx=? idx cap)
                (put-string iop buf idx)
                (set-port-output-index! p 0)
                (set! idx 0))
              (cond
                ((fx=? idx cap)
                  (with-interrupts-enabled
                    (put-char iop c)))
                (else
                  (string-set! buf idx c)
                  (set-port-output-index! p (fx1+ idx))))
              ;; if p is unbuffered, behave as if iop is unbuffered too
              (when (fxzero? cap)
                (with-interrupts-enabled
                  (flush-output-port iop)))
            (set-port-bol! p (char=? c #\newline)))))
        (else
          (raise-bad-msg msg))))
    ((msg p str len)
      ;; (debugf "redir port handler for ~s: (~s ~s ~s ~s)" (proc) msg p str len)
      (case msg
        ((block-read)
          (get-string-some! (proc) str 0 len))
        ((block-write)
          (let ((iop (proc)))
            ;; Chez Scheme documentation for (block-write) states:
            ;; If the port is buffered and the buffer is nonempty, the buffer is flushed before the contents of string are written.
            ;; In any case, the contents of string are written immediately, without passing through the buffer.
            (begin ; with-interrupts-disabled
              (let ((buf (textual-port-output-buffer p))
                    (idx (textual-port-output-index  p)))
                (unless (fxzero? idx)
                  (block-write iop buf idx)
                  (set-port-output-index! p 0)
                  (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline)))))
            (with-interrupts-enabled
              (block-write iop str len))
            (unless (fxzero? len)
              (set-port-bol! p (char=? (string-ref str (fx1- len)) #\newline)))
            ;; if p is unbuffered, behave as if iop is unbuffered too
            (when (fxzero? (textual-port-output-size p))
              (with-interrupts-enabled
                (flush-output-port iop)))))
        (else
          (raise-bad-msg msg))))))


(define (raise-bad-msg msg)
  (assertion-violationf 'make-redir-textual-input/output-port "operation ~s not handled" msg))

) ; close library
