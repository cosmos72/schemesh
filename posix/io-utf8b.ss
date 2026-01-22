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
    bin-port    ; binary port
    bspan       ; bytespan buffer
    buffer-size ; max bytespan length
    (mutable eof?))
  (nongenerative tport-7c46d04b-34f4-4046-b5c7-b63753c1be39))



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
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (fx<? -1 start (fx+ start n) (fx1+ (string-length str))))
    (let ((chunk-max (tport-buffer-size t)))
      (let %write-loop ((start start) (n n))
        (let ((chunk-n (fxmin n chunk-max)))
          (bytespan-insert-right/string! (tport-bspan t) str start (fx+ start chunk-n))
          (tport-maybe-overflow t)
          (unless (fx=? chunk-n n)
            (%write-loop (fx+ start chunk-n) (fx- n chunk-n)))))
      n)
    0))

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


;; create and return a textual input port that reads/writes from/to an underlying binary port
;; and transcodes between characters and UTF-8b byte sequences

;; create and return a textual input port that reads/writes from/to an underlying binary port
;; and transcodes between characters and UTF-8b byte sequences
(define (make-utf8b-port in-port input-buffer-size out-port output-buffer-size options)
  (when in-port
    (assert* 'make-utf8b-port  (port? in-port))
    (assert* 'make-utf8b-port  (binary-port? in-port))
    (assert* 'make-utf8b-port  (input-port? in-port)))
  (when out-port
    (assert* 'make-utf8b-port  (port? out-port))
    (assert* 'make-utf8b-port  (binary-port? out-port))
    (assert* 'make-utf8b-port  (output-port? out-port)))
  (let* ((name       (port-name (or in-port out-port)))
         (tport1     (and in-port  (make-tport in-port  (make-bytespan 0) input-buffer-size #f)))
         (tport2     (and out-port (make-tport out-port (make-bytespan 0) output-buffer-size #f)))
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
                             (when in-port
                               (close-port in-port))
                             (when out-port
                               (unless (eq? in-port out-port)
                                 (close-port out-port)))))))
    (cond
      ((and in-port out-port)
        (make-custom-textual-input/output-port
          name
          read-proc
          write-proc
          ;; position-proc
          (and (eq? in-port out-port) (port-has-port-position? in-port)
               (lambda ()
                 (port-position in-port)))
          ;; set-position-proc
          (and (eq? in-port out-port) (port-has-set-port-position!? in-port)
               (lambda (pos)
                 (set-port-position! in-port pos)))
          close-proc))

      (in-port
        (make-custom-textual-input-port
          name
          read-proc
          ;; position-proc
          (and (port-has-port-position? in-port)
               (lambda ()
                 (port-position in-port)))
          ;; set-position-proc
          (and (port-has-set-port-position!? in-port)
               (lambda (pos)
                 (set-port-position! in-port pos)))
          close-proc))

      (out-port
        (make-custom-textual-output-port
          name
          write-proc
          ;; position-proc
          (and (port-has-port-position? out-port)
               (lambda ()
                 (port-position out-port)))
          ;; set-position-proc
          (and (port-has-set-port-position!? out-port)
               (lambda (pos)
                 (set-port-position! out-port pos)))
          close-proc))
          
      (else
        (raise-errorf 'make-utf8b-port "both in-port and out-port are #f")))))



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



