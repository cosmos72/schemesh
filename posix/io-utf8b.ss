;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; transcoder between UTF-8b textual port and binary port.
(define-record-type tport
  (fields
    bin-port    ; binary port
    bspan       ; bytespan buffer
    buffer-size ; max bytespan length
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
      (bytespan-delete-left! bsp byte-n)
      ;; port may be redirected, or current job may finish,
      ;; and next time it may not be at EOF
      (tport-eof?-set! p #f)
      char-n)))


;; may read fewer characters than requested, but at least one because zero means EOF
;; returns number of characters actually read.
(define (tport-read-some p str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (< -1 start (+ start n) (fx1+ (string-length str))))
    (let ((blen (tport-bspan-length p)))
      (when (and (fx<? blen n) (not (tport-eof? p)))
        (tport-read-refill p n))
      (tport-read-consume p str start n))
    0))


(define (tport-read-char p)
  (let ((n 4))
    (when (and (fx<? (tport-bspan-length p) n) (not (tport-eof? p)))
      (tport-read-refill p n)))
  (let ((bsp (tport-bspan p)))
    (let-values (((ch n) (bytespan-ref/char bsp 0)))
      (cond
        ((char? ch)
          (bytespan-delete-left! bsp n)
          ch)
        ((bytespan-empty? bsp)
          (eof-object))
        (else
          ;; invalid or incomplete UTF-8 sequence at EOF, encode as UTF-8b
          (let ((ch (integer->char* (fxior #xdc80 (bytespan-ref/u8 bsp 0)))))
            (bytespan-delete-left! bsp 1)
            ch))))))


(define (tport-peek-char p)
  (let ((n 4))
    (when (and (fx<? (tport-bspan-length p) n) (not (tport-eof? p)))
      (tport-read-refill p n)))
  (let ((bsp (tport-bspan p)))
    (let-values (((ch n) (bytespan-ref/char bsp 0)))
      (cond
        ((char? ch)
          ch)
        ((fxzero? (bytespan-length bsp))
          (eof-object))
        (else
          ;; invalid or incomplete UTF-8 sequence at EOF, encode as UTF-8b
          (integer->char* (fxior #xdc80 (bytespan-ref/u8 bsp 0))))))))


;; always writes exactly n characters
(define (tport-write p str start n)
  (if (and (string? str) (fixnum? start) (fixnum? n)
           (fx<? -1 start (fx+ start n) (fx1+ (string-length str))))
    (begin
      (bytespan-insert-right/string! (tport-bspan p) str start (fx+ start n))
      (tport-maybe-flush p)
      n)
    0))


(define (tport-write-char p ch)
  (when (char? ch)
    (bytespan-insert-right/char! (tport-bspan p) ch)
    (tport-maybe-flush p)))


(define (tport-maybe-flush p)
  (when (fx>=? (tport-bspan-length p) (tport-buffer-size p))
    (tport-flush p)))


(define (tport-flush p)
  (let* ((bin-port (tport-bin-port p))
         (bsp (tport-bspan p))
         (blen (bytespan-length bsp)))
    (unless (fxzero? blen)
      (put-bytevector bin-port (bytespan-peek-data bsp) (bytespan-peek-beg bsp) blen)
      (bytespan-clear! bsp))
    (flush-output-port bin-port)))


(define (utf8b-port-peek-char p tport)
  (assert* 'utfb-port-peek-char tport)
  (with-interrupts-disabled
    (let ((buf (textual-port-input-buffer p))
          (idx (textual-port-input-index  p))
          (cap (textual-port-input-size   p)))
      (cond
        ((fx<? idx cap)
          (string-ref buf idx))
        ((fxzero? cap)
          (tport-peek-char tport))
        (else
          (let* ((n (tport-read-some tport buf 0 cap))
                 (idx (fx- cap n)))
            (unless (or (fxzero? n) (fxzero? idx))
              (substring-move! buf 0 n idx))
            (set-textual-port-input-index! p idx)
            (if (fxzero? n)
              (eof-object)
              (string-ref buf idx))))))))


(define (utf8b-port-read-char p tport)
  (assert* 'utfb-port-read-char tport)
  (with-interrupts-disabled
    (let ((buf (textual-port-input-buffer p))
          (idx (textual-port-input-index p))
          (cap (textual-port-input-size p)))
      (cond
        ((fx<? idx cap)
          (set-textual-port-input-index! p (fx1+ idx))
          (string-ref buf idx))
        ((fxzero? cap)
          (tport-read-char tport))
        (else
          (let* ((n (tport-read-some tport buf 0 cap))
                 (idx (fx- cap n)))
            (unless (or (fxzero? n) (fxzero? idx))
              (substring-move! buf 0 n idx))
            (cond
              ((fxzero? n)
                (set-textual-port-input-index! p idx)
                (eof-object))
              (else
                (set-textual-port-input-index! p (fx1+ idx))
                (string-ref buf idx)))))))))


(define (utf8b-port-block-read p tport str start len)
  (assert* 'utfb-port-block-read tport)
  (let ((n (tport-read-some tport str start len)))
    (if (and (fxzero? n) (not (fxzero? len)))
      (eof-object)
      n)))


(define (utf8b-port-write-char p tport ch)
  (assert* 'utfb-port-write-char tport)
  (with-interrupts-disabled
    (let* ((buf (textual-port-output-buffer p))
           (idx (textual-port-output-index p))
           (cap (textual-port-output-size p)))
      (when (fx=? idx cap)
        (tport-write tport buf 0 idx)
        (set-textual-port-output-index! p 0)
        (set! idx 0))
      (cond
        ((fx=? idx cap)
          (tport-write-char tport ch))
        (else
          (string-set! buf idx ch)
          (set-textual-port-output-index! p (fx1+ idx)))))
    (set-port-bol! p (char=? ch #\newline))))


(define (utf8b-port-block-write p tport str start n)
  ;; Chez Scheme documentation for (block-write) states:
  ;; If the port is buffered and the buffer is nonempty, the buffer is flushed before the contents of string are written.
  ;; In any case, the contents of string are written immediately, without passing through the buffer.
  (assert* 'utf8b-port-block-write tport)
  (with-interrupts-disabled
    (let ((buf (textual-port-output-buffer p))
          (idx (textual-port-output-index  p)))
      (unless (fxzero? idx)
        (tport-write tport buf 0 idx)
        (set-textual-port-output-index! p 0)
        (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline))))
    (unless (fxzero? n)
      (set-port-bol! p (char=? (string-ref str (fx1- n)) #\newline))))
  (tport-write tport str start n)
  (tport-flush tport))


(define (utf8b-port-flush p tport)
  (assert* 'utfb-port-flush tport)
  (with-interrupts-disabled
    (let ((buf (textual-port-output-buffer p))
          (idx (textual-port-output-index  p)))
      (unless (fxzero? idx)
        (tport-write tport buf 0 idx)
        (set-textual-port-output-index! p 0)
        (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline)))))
  (tport-flush tport))


(define (raise-bad-msg msg)
  (assertion-violationf 'utfb-port "operation ~s not handled" msg))


;; create and return a textual input port handler that reads/writes UTF-8b bytes from/to a binary input port
;; and transcodes them to characters
(define (make-utfb-port-handler in-port input-buffer-size out-port output-buffer-size)
  (let ((name   (string-append "utf8b " (port-name (or in-port out-port))))
        (tport1 (and in-port  (make-tport in-port  (make-bytespan 0) input-buffer-size #f)))
        (tport2 (and out-port (make-tport out-port (make-bytespan 0) output-buffer-size #f))))
    ;; return a closure
    (case-lambda
      ((msg p)
        ;; (debugf "utf8b port handler for ~s: (~s ~s)" (or in-port out-port) msg p)
        (case msg
          ((char-ready?)
            (assert* 'utfb-port-char-ready? in-port)
            (or (with-interrupts-disabled
                  (fx>? (textual-port-output-index p) 0))
                (fx>? (tport-bspan-length tport1) 0)
                (input-port-ready? in-port)))
          ((clear-input-port)
            (assert* 'utfb-port-clear-input-port in-port)
            (clear-input-port in-port))
          ((clear-output-port)
            (assert* 'utfb-port-clear-output-port out-port)
            (with-interrupts-disabled
              (set-textual-port-output-index! p 0))
            (clear-output-port out-port))
          ((close-port)
            (mark-port-closed! p)
            (when in-port
              (close-port in-port))
            (when out-port
              (close-port out-port)))
          ((flush-output-port)
            (utf8b-port-flush p tport2))
          ((file-position)
            (port-position (or in-port out-port)))
          ((file-length)
            (port-length (or in-port out-port)))
          ((peek-char)
            (utf8b-port-peek-char p tport1))
          ((port-name)
            name)
          ((read-char)
            (utf8b-port-read-char p tport1))
          (else
            (raise-bad-msg msg))))
      ((msg c p)
        ;; (debugf "utf8b port handler for ~s: (~s ~s ~s)" (or in-port out-port) msg c p)
        (case msg
          ((file-position)
            (when in-port
              (set-port-position! in-port p))
            (when (and out-port (not (eq? in-port out-port)))
              (set-port-position! out-port p)))
          ((unread-char)
            (assert* 'utfb-port-unread-char in-port)
            (bytespan-insert-left/char! (tport-bspan in-port) c))
          ((write-char)
            (utf8b-port-write-char p tport2 c))
          (else
            (raise-bad-msg msg))))
      ((msg p str len)
        ;; (debugf "utf8b port handler for ~s: (~s ~s ~s ~s)" (or in-port out-port) msg p str len)
        (case msg
          ((block-read)
            (utf8b-port-block-read p tport1 str 0 len))
          ((block-write)
            (utf8b-port-block-write p tport2 str 0 len))
          (else
            (raise-bad-msg msg)))))))



;; create and return a textual input port that reads UTF-8b bytes from a binary input port
;; and transcodes them to characters
(define make-utf8b-input-port
  (case-lambda
    ((binary-in-port b-mode)
      (assert* 'make-utf8b-input-port (input-port? binary-in-port))
      (assert* 'make-utf8b-input-port (binary-port? binary-in-port))
      (assert* 'make-utf8b-input-port (buffer-mode? b-mode))
      (let* ((in-buffer-size (b-mode->input-buffer-size b-mode))
             (p (make-input-port
                  (make-utfb-port-handler binary-in-port in-buffer-size #f 0)
                  (make-string in-buffer-size))))
        (set-textual-port-input-index! p in-buffer-size)
        p))
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
      (let* ((in-buffer-size (b-mode->input-buffer-size b-mode))
             (out-buffer-size (b-mode->output-buffer-size b-mode))
             (p (make-input/output-port
                  (make-utfb-port-handler binary-in/out-port in-buffer-size binary-in/out-port out-buffer-size)
                  (make-string in-buffer-size)
                  (make-string out-buffer-size))))
        (set-textual-port-input-index! p in-buffer-size)
        p))
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
      (let* ((out-buffer-size (b-mode->output-buffer-size b-mode))
             (p (make-output-port
                  (make-utfb-port-handler #f 0 binary-out-port out-buffer-size)
                  (make-string out-buffer-size))))
        p))
    ((binary-out-port)
      (make-utf8b-output-port binary-out-port (buffer-mode block)))))


;; create and return a textual input port that redirectably reads
;; UTF-8b sequences from a file descriptor and converts them to characters.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->textual-input-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-input-port
        (fd->binary-input-port name fd b-mode)
        b-mode))
    ((name fd)
      (fd->textual-input-port fd (buffer-mode block)))))


;; create and return a textual input/output port that:
;; 1. redirectably reads UTF-8b bytes from a file descriptor and converts them to characters.
;; 2. converts characters to UTF-8b bytes and redirectably writes them to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->textual-input/output-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-input/output-port
        (fd->binary-input/output-port name fd b-mode)
        b-mode))
    ((name fd)
      (fd->textual-input/output-port name fd (buffer-mode block)))))


;; create and return a textual output port that converts characters to UTF-8b bytes
;; and redirectably writes such bytes to a file descriptor.
;;
;; fd must be an unsigned fixnum corresponding to an open file descriptor.
(define fd->textual-output-port
  (case-lambda
    ((name fd b-mode)
      (make-utf8b-output-port
        (fd->binary-output-port name fd b-mode)
        b-mode))
    ((name fd)
      (fd->textual-output-port fd name (buffer-mode block)))))




;; create and return a textual input port that reads
;; UTF-8b sequences from a file and converts them to characters.
;;
;; path must be a string, bytevector, bytespan or charspan.
(define open-file-textual-input-port
  (case-lambda
    ((path f-options b-mode)
      (make-utf8b-input-port
        (open-file-binary-input-port path f-options b-mode)
        b-mode))
    ((path f-options)
      (open-file-textual-input-port path f-options (buffer-mode block)))
    ((path)
      (open-file-textual-input-port path (file-options) (buffer-mode block)))))



;; customize how "tport" objects are printed
(record-writer (record-type-descriptor tport)
  (lambda (tp port writer)
    (display "#<tport " port)
    (display (tport-bin-port tp) port)
    (display ">" port)))
