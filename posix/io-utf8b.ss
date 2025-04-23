;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; UTF-8b textual input and/or output port reading from/writing to a binary input and/our output port.
(define-record-type tport
  (fields
    bin-port ; binary port
    bspan    ; bytespan buffer
    (mutable eof?))
  (nongenerative tport-7c46d04b-34f4-4046-b5c7-b63753c1be39))


(define (tport-bspan-length p)
  (bytespan-length (tport-bspan p)))


(define (tport-read-refill p n)
  (let* ((bsp   (tport-bspan p))
         (len   (bytespan-length bsp))
         (delta (fxmax 0 (fx- n len))))
    (bytespan-reserve-right! bsp (fx+ len delta))
    (let ((got (get-bytevector-n! (tport-bin-port p) (bytespan-peek-data bsp)
                                  (bytespan-peek-end bsp) delta)))
      (if (and (fixnum? got) (fx>? got 0))
        (bytespan-resize-right! bsp (fx+ len got))
        (tport-eof?-set! p #t)))))


(define (tport-read-consume p str start n)
  (let ((bsp (tport-bspan p)))
    (let-values (((byte-n char-n)
                    (utf8b->string-copy!
                       (bytespan-peek-data bsp) (bytespan-peek-beg bsp) (bytespan-peek-end bsp)
                       str start (fx+ start n) (tport-eof? p))))
      (bytespan-erase-left! bsp byte-n)
      ;; port may be redirected, or current job may finish,
      ;; and next time it may not be at EOF
      (tport-eof?-set! p #f)
      char-n)))


(define (tport-read p str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (string-length str))))
    (let ((bsp-len (tport-bspan-length p)))
      ;; make-custom-input-port documentation states that read procedure may return
      ;; fewer characters than requested - but at least one, because zero means EOF.
      (when (and (fx<? bsp-len n) (not (tport-eof? p)))
        (tport-read-refill p n))
      (tport-read-consume p str start n))
    0))


(define (tport-write p str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (string-length str))))
    (let ((bin-port (tport-bin-port p))
          (bsp (tport-bspan p)))
      (bytespan-insert-right/string! bsp str start (fx+ start n))
      (put-bytevector bin-port (bytespan-peek-data bsp) (bytespan-peek-beg bsp) (bytespan-length bsp))
      ;; must flush every time :( both ports may be buffered
      ;; and tport has no mechanism to know when to flush
      (flush-output-port bin-port)
      (bytespan-clear! bsp)
      n)
    0))


;; create and return a textual input port that reads UTF-8b bytes from a binary input port
;; and transcodes them to characters
(define make-utf8b-input-port
  (case-lambda
    ((binary-in-port b-mode)
      (assert* 'make-utf8b-input-port (input-port? binary-in-port))
      (assert* 'make-utf8b-input-port (binary-port? binary-in-port))
      (assert* 'make-utf8b-input-port (buffer-mode? b-mode))
      (let* ((tport (make-tport binary-in-port (make-bytespan 0) #f))
             (ret (make-custom-textual-input-port
                    (string-append "utf8b " (port-name binary-in-port))
                    (lambda (str start n) (tport-read tport str start n))
                    (if (port-has-port-position? binary-in-port)
                      (lambda () (port-position binary-in-port))
                      #f)
                    (if (port-has-set-port-position!? binary-in-port)
                      (lambda (pos) (set-port-position! binary-in-port pos))
                      #f)
                    ;; on close, also close underlying binary port
                    (lambda () (close-port binary-in-port)))))
        (%set-buffer-mode! ret b-mode)))
    ((binary-in-port)
      (make-utf8b-input-port binary-in-port #f))))


;; create and return a textual input/output port transcodes between characters and UTF-8b bytes,
;; which are read/written to the specified binary-in/out-port.
(define make-utf8b-input/output-port
  (case-lambda
    ((binary-in/out-port b-mode)
      (assert* 'make-utf8b-input/output-port (input-port? binary-in/out-port))
      (assert* 'make-utf8b-input/output-port (output-port? binary-in/out-port))
      (assert* 'make-utf8b-input/output-port (binary-port? binary-in/out-port))
      (assert* 'make-utf8b-input/output-port (buffer-mode? b-mode))
      (let* ((tport1 (make-tport binary-in/out-port (make-bytespan 0) #f))
             (tport2 (make-tport binary-in/out-port (make-bytespan 0) #f))
             (ret (make-custom-textual-input/output-port
                    (string-append "utf8b " (port-name binary-in/out-port))
                    (lambda (str start n) (tport-read tport1 str start n))
                    (lambda (str start n) (tport-write tport2 str start n))
                    (if (port-has-port-position? binary-in/out-port)
                      (lambda () (port-position binary-in/out-port))
                      #f)
                    (if (port-has-set-port-position!? binary-in/out-port)
                      (lambda (pos) (set-port-position! binary-in/out-port pos))
                      #f)
                    ;; on close, also close underlying binary port
                    (lambda () (close-port binary-in/out-port)))))
        (%set-buffer-mode! ret b-mode)))
    ((binary-in/out-port)
      (make-utf8b-input/output-port binary-in/out-port (buffer-mode block)))))


;; create and return a textual output port that transcodes characters to UTF-8b bytes
;; and writes such bytes to binary-out-port.
(define make-utf8b-output-port
  (case-lambda
    ((binary-out-port b-mode)
      (assert* 'make-utf8b-output-port (output-port? binary-out-port))
      (assert* 'make-utf8b-output-port (binary-port? binary-out-port))
      (assert* 'make-utf8b-output-port (buffer-mode? b-mode))
      (let* ((tport (make-tport binary-out-port (make-bytespan 0) #f))
             (ret (make-custom-textual-output-port
                    (string-append "utf8b " (port-name binary-out-port))
                    (lambda (str start n) (tport-write tport str start n))
                    (if (port-has-port-position? binary-out-port)
                      (lambda () (port-position binary-out-port))
                      #f)
                    (if (port-has-set-port-position!? binary-out-port)
                      (lambda (pos) (set-port-position! binary-out-port pos))
                      #f)
                    ;; on close, also close underlying binary port
                    (lambda () (close-port binary-out-port)))))
        (%set-buffer-mode! ret b-mode)))
    ((binary-out-port)
      (make-utf8b-output-port binary-out-port (buffer-mode block)))))


;; create and return a textual input port that redirectably reads
;; UTF-8b sequences from a file descriptor and converts them to characters.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define open-fd-utf8b-input-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-input-port
        (open-fd-binary-input-port name fd b-mode)
        b-mode))
    ((name fd)
      (open-fd-utf8b-input-port fd (buffer-mode block)))))


;; create and return a textual input/output port that:
;; 1. redirectably reads UTF-8b bytes from a file descriptor and converts them to characters.
;; 2. converts characters to UTF-8b bytes and redirectably writes them to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define open-fd-utf8b-input/output-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-input/output-port
        (open-fd-binary-input/output-port name fd b-mode)
        b-mode))
    ((name fd)
      (open-fd-utf8b-input/output-port fd #f))))


;; create and return a textual output port that converts characters to UTF-8b bytes
;; and redirectably writes such bytes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define open-fd-utf8b-output-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-output-port
        (open-fd-binary-output-port name fd b-mode)
        b-mode))
    ((name fd)
      (open-fd-utf8b-output-port fd #f))))




;; create and return a textual input port that reads
;; UTF-8b sequences from a file and converts them to characters.
;;
;; path must be a string, bytevector, bytespan or charspan.
(define open-file-utf8b-input-port
  (case-lambda
    ((path f-options b-mode)
      (make-utf8b-input-port
        (open-file-binary-input-port path f-options b-mode)
        b-mode))
    ((path f-options)
      (open-file-utf8b-input-port path f-options (buffer-mode block)))
    ((path)
      (open-file-utf8b-input-port path (file-options) (buffer-mode block)))))
