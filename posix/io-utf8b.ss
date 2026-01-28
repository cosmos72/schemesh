;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; transcoder between UTF-8b textual port and binary port.
(define-record-type tport
  (fields
    bin-port      ; binary port
    bspan         ; bytespan buffer
    buffer-size   ; max bytespan length
    (mutable pos) ; position i.e. number of characters read or written
    (mutable eof?))
  (nongenerative tport-7c46d04b-34f4-4046-b5c7-b63753c1be40))



(define (tport-bspan-length t)
  (bytespan-length (tport-bspan t)))


(define (tport-read-refill t n)
  (let* ((bsp   (tport-bspan t))
         (len   (bytespan-length bsp))
         (delta (fxmax 0 (fx- n len))))
    (bytespan-reserve-right! bsp (fx+ len delta))
    ;; return as soon as 1 or more bytes have been read:
    ;; do NOT wait until delta bytes have been read.
    (let ((got (get-bytevector-some! (tport-bin-port t) (bytespan-peek-data bsp)
                                     (bytespan-peek-end bsp) delta)))
      (if (and (fixnum? got) (fx>? got 0))
        (bytespan-resize-right! bsp (fx+ len got))
        (tport-eof?-set! t #t)))))


(define (tport-read-consume t str start n)
  (let ((bsp (tport-bspan t)))
    (let-values (((byte-n char-n)
                    (utf8b->string-copy!
                       (bytespan-peek-data bsp) (bytespan-peek-beg bsp) (bytespan-peek-end bsp)
                       str start (fx+ start n) (tport-eof? t))))
      (bytespan-delete-left! bsp byte-n)
      ;; port may be redirected, or current job may finish,
      ;; and next time it may not be at EOF
      (tport-eof?-set! t #f)
      (tport-pos-set! t (fx+ char-n (tport-pos t)))
      char-n)))


;; may read fewer characters than requested, but at least one because zero means EOF
;; returns number of characters actually read.
(define (tport-read-some t str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (string-length str))))
    (let ((blen (tport-bspan-length t)))
      (when (and (fx<? blen n) (not (tport-eof? t)))
        (tport-read-refill t n))
      (tport-read-consume t str start n))
    0))


;; always writes exactly n characters
(define (tport-write t str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n))
    (begin
      ;; at least Chez Scheme 10.3.0 does not validate port output size & index,
      ;; and sometimes calls this function with (> n (string-length str))
      ;; => be paranoid and validate all arguments
      ;; (assert* 'tport-write (fx<=?* 0 start (fx+ start n) (string-length str)))
      (let* ((len       (string-length str))
             (n         (fxmax 0 (fxmin n len)))
             (start     (fxmax 0 (fxmin start (fx- len n))))
             (chunk-max (fxmax 128 (tport-buffer-size t))))
        (when (fx>? n 0)
          (let %write-loop ((start start) (n n))
            (let ((chunk-n (fxmin n chunk-max)))
              ;; (debugf ". tport-write chunk str ~s, n ~s, chunk-n ~s" (substring str start (fx+ start n)) n chunk-n)
              (bytespan-insert-right/string! (tport-bspan t) str start (fx+ start chunk-n))
              (tport-maybe-overflow t)
              (tport-pos-set! t (fx+ chunk-n (tport-pos t)))
              (unless (fx=? chunk-n n)
                (%write-loop (fx+ start chunk-n) (fx- n chunk-n))))))
        ;; (debugf "< tport-write n ~s" n)
        n))
    n))


(define (tport-flush t)
  (tport-overflow t)
  (flush-output-port (tport-bin-port t)))


(define (tport-maybe-overflow t)
  (when (fx>=? (tport-bspan-length t) (tport-buffer-size t))
    (tport-overflow t)))


(define (tport-overflow t)
  (let* ((bin-port (tport-bin-port t))
         (bsp (tport-bspan t))
         (blen (bytespan-length bsp)))
    (unless (fxzero? blen)
      (put-bytevector bin-port (bytespan-peek-data bsp) (bytespan-peek-beg bsp) blen)
      (bytespan-clear! bsp))))


;; implementation of (make-utf8b-port)
(define (%make-utf8b-port bport-in input-buffer-size bport-out output-buffer-size options)
  (when bport-in
    (assert* 'make-utf8b-port  (port? bport-in))
    (assert* 'make-utf8b-port  (binary-port? bport-in))
    (assert* 'make-utf8b-port  (input-port? bport-in)))
  (when bport-out
    (assert* 'make-utf8b-port  (port? bport-out))
    (assert* 'make-utf8b-port  (binary-port? bport-out))
    (assert* 'make-utf8b-port  (output-port? bport-out)))
  (letrec* ((name       (port-name (or bport-in bport-out)))
            (tport1     (and bport-in  (make-tport bport-in  (make-bytespan 0) input-buffer-size 0 #f)))
            (tport2     (and bport-out (make-tport bport-out (make-bytespan 0) output-buffer-size 0 #f)))
            (read-proc  (and tport1
                             (lambda (str start n)
                               (tport-read-some tport1 str start n))))
            (write-proc (and tport2
                             (lambda (str start n)
                               (let ((written (tport-write tport2 str start n)))
                                 (tport-flush tport2)
                                 written))))
            (close-proc (and (plist-ref options 'close? #t)
                             (lambda ()
                                (when bport-in
                                  (close-port bport-in))
                                (when bport-out
                                  (unless (eq? bport-in bport-out)
                                    (close-port bport-out))))))
            (position-in-proc (and bport-in
                                   (lambda (p)
                                     (let ((rx-n  (tport-pos tport1))
                                           (index (textual-port-input-index p))
                                           (size  (textual-port-input-size p)))
                                       (fxmax 0 (fx- rx-n (fxmax 0 (fx- size index))))))))
            (position-out-proc (and bport-out
                                   (lambda (p)
                                     (let ((tx-n  (tport-pos tport2))
                                           (index (textual-port-output-index p)))
                                       (fx+ tx-n (fxmax 0 index))))))
            (p
              (cond
                ((and bport-in bport-out)
                  (make-custom-textual-input/output-port
                    name
                    read-proc
                    write-proc
                    ;; position-proc
                    (lambda () (position-in-proc p))
                    ;; set-position-proc
                    #f
                    close-proc))

                (bport-in
                  (make-custom-textual-input-port
                    name
                    read-proc
                    ;; position-proc
                    (lambda () (position-in-proc p))
                    ;; set-position-proc
                    #f
                    close-proc))

                (bport-out
                  (make-custom-textual-output-port
                    name
                    write-proc
                    ;; position-proc
                    (lambda () (position-out-proc p))
                    ;; set-position-proc
                    #f
                    close-proc))

                (else
                  (raise-errorf 'make-utf8b-port "both bport-in and bport-out are #f")))))
      p))


(define (%set-textual-buffer-size! port input-buffer-size output-buffer-size)
  (when (input-port? port)
    (let ((cap (fxmax 1 input-buffer-size)))
      (set-textual-port-input-buffer! port (make-string cap))
      (set-textual-port-input-size!   port cap)
      (set-textual-port-input-index!  port cap)))
  (when (output-port? port)
    (let ((cap (fxmax 1 output-buffer-size)))
      (set-textual-port-output-buffer! port (make-string cap))
      (set-textual-port-output-size!   port (fxmax 0 (fx1- cap))) ; leave 1 byte for (put-char)
      (set-textual-port-output-index!  port 0)))
  port)


;; create and return a textual input port that reads/writes from/to an underlying binary port
;; and transcodes between characters and UTF-8b byte sequences
(define (make-utf8b-port bport-in input-buffer-size bport-out output-buffer-size options)
  (%set-textual-buffer-size!
    (%make-utf8b-port bport-in input-buffer-size bport-out output-buffer-size options)
    input-buffer-size
    output-buffer-size))


(define port->utf8b-port
  (case-lambda
    ((bin-port dir b-mode options)
      (assert* 'port->utf8b-port (binary-port? bin-port))
      (assert* 'port->utf8b-port (buffer-mode? b-mode))
      (assert* 'port->utf8b-port (plist? options))
      (case dir
        ((read)
          (let ((in-buffer-size (b-mode->input-buffer-size b-mode)))
            (make-utf8b-port bin-port in-buffer-size #f 0 options)))
        ((rw)
          (let ((in-buffer-size  (b-mode->input-buffer-size b-mode))
                (out-buffer-size (b-mode->output-buffer-size b-mode)))
            (make-utf8b-port bin-port in-buffer-size bin-port out-buffer-size options)))
        ((write)
          (let ((out-buffer-size (b-mode->output-buffer-size b-mode)))
            (make-utf8b-port #f 0 bin-port out-buffer-size options)))
        (else
          (assert* 'port->utf8b-port (memq dir '(read write rw))))))
    ((bin-port dir b-mode)
      (port->utf8b-port bin-port dir b-mode '()))
    ((bin-port dir)
      (port->utf8b-port bin-port dir (buffer-mode block) '()))
    ((bin-port)
      (port->utf8b-port
        bin-port
        (if (input-port? bin-port) (if (output-port? bin-port) 'rw 'read) 'write)
        (buffer-mode block)
        '()))))


;; create and return a textual input and/or output port that reads from and/or writes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define (fd->textual-port fd dir b-mode name proc-on-close)
  (port->utf8b-port
     (fd->binary-port fd dir b-mode name proc-on-close)
     dir b-mode))



