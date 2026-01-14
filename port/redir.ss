;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k port redir (0 9 3))
  (export
    binary-port-lambda->port textual-port-lambda->port)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme)  assertion-violationf block-read block-write char-ready? clear-input-port clear-output-port
                        file-length file-position fx1- fx1+ get-bytevector-some! get-string-some! logbit?
                        make-input-port make-input/output-port make-output-port mark-port-closed!
                        put-bytevector-some procedure-arity-mask record-case
                        set-binary-port-input-buffer!   set-binary-port-input-index!   set-binary-port-input-size!
                        set-binary-port-output-buffer!  set-binary-port-output-index!  set-binary-port-output-size!
                        set-port-bol! set-port-output-index!
                        textual-port-output-buffer textual-port-output-index textual-port-output-size unread-char void)
    (only (scheme2k bootstrap) assert*))


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


;; create and return an unbuffered binary input/output port that redirectably reads from and writes to another binary port.
;;
;; port-lambda must be a no-argument procedure that returns another binary port;
;; the returned binary port *may* change from one call to the next.
(define binary-port-lambda->port
  (case-lambda
    ((name port-lambda dir proc-on-close)
      (assert* 'binary-port-lambda->port (string? name))
      (assert* 'binary-port-lambda->port (procedure? port-lambda))
      (assert* 'binary-port-lambda->port (logbit? 0 (procedure-arity-mask port-lambda)))
      (unless (boolean? proc-on-close)
        (assert* 'binary-port-lambda->port (procedure? proc-on-close))
        (assert* 'binary-port-lambda->port (logbit? 0 (procedure-arity-mask proc-on-close))))
      (let*
        ((get-pos
           (lambda ()
             (port-position (port-lambda))))
         (set-pos!
           (lambda (pos)
             (set-port-position! (port-lambda) pos)))
         (close
           (case proc-on-close
             ((#f) #f)
             ((#t) (lambda () (close-port (port-lambda))))
             (else proc-on-close)))
         (p
           (case dir
             ((read)
               (make-custom-binary-input-port name
                 (lambda (bv start n)
                   (get-bytevector-some! (port-lambda) bv start n))
                 get-pos set-pos! close))
             ((rw)
               (make-custom-binary-input/output-port name
                 (lambda (bv start n)
                   (get-bytevector-some! (port-lambda) bv start n))
                 (lambda (bv start n)
                   (let ((port (port-lambda)))
                     (put-bytevector port bv start n)
                     (flush-output-port port)
                     n))
                 get-pos set-pos! close))
             ((write)
               (make-custom-binary-output-port name
                 (lambda (bv start n)
                   (let ((port (port-lambda)))
                     (put-bytevector port bv start n)
                     (flush-output-port port)
                     n))
                 get-pos set-pos! close))
             (else
               (assert* 'binary-port-lambda->port (memq dir '(read 'write rw)))))))

        ;; (make-custom-binary-input/output-port) does not run user code on (flush-output-port)
        ;; thus we cannot flush the underlying stream when needed => return an unbuffered stream
        (set-binary-buffer-mode! p 'none)))

    ((name port-lambda dir)
      (binary-port-lambda->port name port-lambda dir #f))

    ((name port-lambda)
      (binary-port-lambda->port name port-lambda 'rw #f))))


;; create and return a textual input/output port that redirectably reads from and writes to another textual port.
;;
;; port-lambda must be a no-argument procedure that returns another textual port;
;; the returned textual port *may* change from one call to the next.
;;
;; if proc-on-close is #t, attempts to close returned port are ignored
(define textual-port-lambda->port
  (case-lambda
    ((name port-lambda dir proc-on-close proc-before-write out-buffer-size)
      (assert* 'textual-port-lambda->port (string? name))
      (assert* 'textual-port-lambda->port (procedure? port-lambda))
      (assert* 'textual-port-lambda->port (logbit? 0 (procedure-arity-mask port-lambda)))
      (unless (boolean? proc-on-close)
        (assert* 'textual-port-lambda->port (procedure? proc-on-close))
        (assert* 'textual-port-lambda->port (logbit? 0 (procedure-arity-mask proc-on-close))))
      (when proc-before-write
        (assert* 'textual-port-lambda->port (procedure? proc-before-write))
        (assert* 'textual-port-lambda->port (logbit? 0 (procedure-arity-mask proc-before-write))))
      (let ((handler (textual-port-lambda-handler name port-lambda proc-on-close proc-before-write)))
        (case dir
          ((read)
            (make-input-port handler (make-string 0)))
          ((rw)
            (make-input/output-port handler (make-string 0) (make-string out-buffer-size)))
          ((write)
            (make-output-port handler (make-string out-buffer-size)))
          (else
            (assert* 'textual-port-lambda->port (memq dir '(read 'write rw)))))))

    ((name port-lambda dir proc-on-close proc-before-write)
      (textual-port-lambda->port name port-lambda dir proc-on-close proc-before-write 4096))

    ((name port-lambda dir proc-on-close)
      (textual-port-lambda->port name port-lambda dir proc-on-close #f))

    ((name port-lambda dir)
      (textual-port-lambda->port name port-lambda dir #f #f))

    ((name port-lambda)
      (textual-port-lambda->port name port-lambda 'rw #f #f))))


;; create and return a handler suitable for Chez Scheme (make-input/output-port)
(define (textual-port-lambda-handler name proc proc-on-close proc-before-write)
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
          (set-port-output-index! p 0)
          (clear-output-port (proc)))
        ((close-port)
          (case proc-on-close
            ((#t) (void))
            ((#f) (mark-port-closed! p))
            (else (mark-port-closed! p) (proc-on-close))))
        ((flush-output-port)
          (when proc-before-write
            (proc-before-write))
          (let ((buf (textual-port-output-buffer p))
                (idx (textual-port-output-index  p)))
            (block-write (proc) buf idx) ;; also flushes iop
            (unless (fxzero? idx)
              (set-port-output-index! p 0)
              (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline)))))
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
        ((write-char) ; called only if output buffer is full.
          (when proc-before-write
            (proc-before-write))
          (let* ((iop (proc))
                 (buf (textual-port-output-buffer p))
                 (idx (textual-port-output-index p))
                 (cap (textual-port-output-size p)))
            (when (and (not (fxzero? idx)) (fx=? idx cap))
              (put-string iop buf 0 idx)
              (set-port-output-index! p 0)
              (set! idx 0))
            (cond
              ((fx=? idx cap)
                (put-char iop c))
              (else
                (string-set! buf idx c)
                (set-port-output-index! p (fx1+ idx))))
            ;; if p is unbuffered, behave as if iop is unbuffered too
            (when (fxzero? cap)
              (flush-output-port iop)))
          (set-port-bol! p (char=? c #\newline)))
        (else
          (raise-bad-msg msg))))
    ((msg p str len)
      ;; (debugf "redir port handler for ~s: (~s ~s ~s ~s)" (proc) msg p str len)
      (case msg
        ((block-read)
          (get-string-some! (proc) str 0 len))
        ((block-write)
          (when proc-before-write
            (proc-before-write))
          (let ((iop (proc)))
            ;; Chez Scheme documentation for (block-write) states:
            ;; If the port is buffered and the buffer is nonempty, the buffer is flushed before the contents of string are written.
            ;; In any case, the contents of string are written immediately, without passing through the buffer.
            (let ((buf (textual-port-output-buffer p))
                  (idx (textual-port-output-index  p)))
              (unless (fxzero? idx)
                (block-write iop buf idx)
                (set-port-output-index! p 0)
                (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline))))
            (block-write iop str len)
            (unless (fxzero? len)
              (set-port-bol! p (char=? (string-ref str (fx1- len)) #\newline)))
            ;; if p is unbuffered, behave as if iop is unbuffered too
            (when (fxzero? (textual-port-output-size p))
              (flush-output-port iop))))
        (else
          (raise-bad-msg msg))))))


(define (raise-bad-msg msg)
  (assertion-violationf 'textual-port-lambda->port "operation ~s not handled" msg))

) ; close library
