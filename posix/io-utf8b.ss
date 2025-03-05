;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.



;; UTF-8b textual input and/or output port reading from/writing to a binary input and/our output port.
(define-record-type tport
  (fields
    bin-port ; binary port
    bspan    ; bytespan buffer
    (mutable eof?))
  (nongenerative #{tport ncfagw2gtqduasri71nnqdxvw-1}))


(define (tport-bspan-length p)
  (bytespan-length (tport-bspan p)))


(define (tport-read-refill p)
  (let* ((bsp   (tport-bspan p))
         (len   (bytespan-length bsp))
         (delta 8192))
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
      char-n)))


(define (tport-read p str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (string-length str))))
    (let ((bsp-len (tport-bspan-length p)))
      ;; make-custom-input-port documentation states that read procedure may return
      ;; fewer characters than requested - but at least one, because zero means EOF.
      (when (and (fx<? bsp-len (fxmax 4 n)) (not (tport-eof? p)))
        (tport-read-refill p))
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


;; create and return an UTF-8b textual input port that redirectably reads from a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (make-utf8b-textual-input-port binary-in-port)
  (assert* 'make-utf8b-textual-output-port (input-port? binary-in-port))
  (assert* 'make-utf8b-textual-output-port (binary-port? binary-in-port))
  (let* ((bsp (make-bytespan 0))
         (port (make-tport binary-in-port bsp #f)))
    (bytespan-reserve-right! bsp 8192)
    (make-custom-textual-input-port
      (string-append "utf8b " (port-name binary-in-port))
      (lambda (str start n) (tport-read port str start n))
      #f    ; no pos
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a textual input/output port that converts characters to UTF-8b bytes
;; and writes such bytes to a binary output port.
;; It also reads bytes
;;
;; binary-in/out-port must be a binary input/output port
(define (make-utf8b-textual-input/output-port binary-in/out-port)
  (assert* 'make-utf8b-textual-output-port (input-port? binary-in/out-port))
  (assert* 'make-utf8b-textual-output-port (output-port? binary-in/out-port))
  (assert* 'make-utf8b-textual-output-port (binary-port? binary-in/out-port))
  (let* ((bsp1 (make-bytespan 0))
         (bsp2 (make-bytespan 0))
         (port1 (make-tport binary-in/out-port bsp1 #f))
         (port2 (make-tport binary-in/out-port bsp2 #f)))
    (bytespan-reserve-right! bsp1 8192)
    (bytespan-reserve-right! bsp2 8192)
    (make-custom-textual-input/output-port
      (string-append "utf8b " (port-name binary-in/out-port))
      (lambda (str start n) (tport-read port1 str start n))
      (lambda (str start n) (tport-write port2 str start n))
      #f    ; no pos
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a textual output port that converts characters to UTF-8b bytes
;; and writes such bytes to a binary output port.
;;
;; binary-out-port must be a binary output port
(define (make-utf8b-textual-output-port binary-out-port)
  (assert* 'make-utf8b-textual-output-port (output-port? binary-out-port))
  (assert* 'make-utf8b-textual-output-port (binary-port? binary-out-port))
  (let* ((bsp (make-bytespan 0))
         (port (make-tport binary-out-port bsp #f)))
    (bytespan-reserve-right! bsp 8192)
    (make-custom-textual-output-port
      (string-append "utf8b " (port-name binary-out-port))
      (lambda (str start n) (tport-write port str start n))
      #f    ; no pos
      #f    ; no pos-set!
      #f))) ; do nothing on close


;; create and return a textual input port that redirectably reads
;; UTF-8b sequences from a file descriptor and converts them to characters.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-textual-input-port name fd-proc)
  (make-utf8b-textual-input-port
    (open-fd-redir-binary-input-port name fd-proc)))


;; create and return a textual input/output port that:
;; 1. redirectably reads UTF-8b bytes from a file descriptor and converts them to characters.
;; 2. converts characters to UTF-8b bytes and redirectably writes them to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-textual-input/output-port name fd-proc)
  (make-utf8b-textual-input/output-port
    (open-fd-redir-binary-input/output-port name fd-proc)))


;; create and return a textual output port that converts characters to UTF-8b bytes
;; and redirectably writes such bytes to a file descriptor.
;;
;; fd-proc must be a no-argument procedure that returns an integer file descriptor;
;; the returned file descriptor *may* change from one call to the next.
(define (open-fd-redir-textual-output-port name fd-proc)
  (make-utf8b-textual-output-port
    (open-fd-redir-binary-output-port name fd-proc)))
