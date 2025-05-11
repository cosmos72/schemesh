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
    ;; return as soon as 1 or more bytes have been read:
    ;; do NOT wait until delta bytes have been read.
    (let ((got (get-bytevector-some! (tport-bin-port p) (bytespan-peek-data bsp)
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


;; called only if input buffer is empty.
(define (utf8b-port-peek-char p tport)
  (assert* 'utfb-port-peek-char tport)
  (let* ((buf (textual-port-input-buffer p))
         (cap (string-length buf)))
    (cond
      ((fxzero? cap)
        (tport-peek-char tport))
      (else
        (let ((n (tport-read-some tport buf 0 cap)))
          (set-textual-port-input-index! p 0)
          (set-textual-port-input-size! p n)
          (if (fxzero? n)
            (eof-object)
            (string-ref buf 0)))))))


;; called only if input buffer is empty.
(define (utf8b-port-read-char p tport)
  (assert* 'utfb-port-read-char tport)
  (let* ((buf (textual-port-input-buffer p))
         (cap (string-length buf)))
    (cond
      ((fxzero? cap)
        (tport-read-char tport))
      (else
        (let ((n (tport-read-some tport buf 0 cap)))
          (set-textual-port-input-size! p n)
          (cond
            ((fxzero? n)
              (set-textual-port-input-index! p 0)
              (eof-object))
            (else
              (set-textual-port-input-index! p 1)
              (string-ref buf 0))))))))


;; called only if input buffer is empty.
(define (utf8b-port-block-read p tport str start len)
  (assert* 'utfb-port-block-read tport)
  (let ((n (tport-read-some tport str start len)))
    (if (and (fxzero? n) (not (fxzero? len)))
      (eof-object)
      n)))


;; called only if output buffer is full.
(define (utf8b-port-write-char p tport ch)
  (assert* 'utfb-port-write-char tport)
  (let ((len (textual-port-output-size p)))
    ;; (debugf "utfb-port-write-char port=~s idx=~s len=~s cap=~s" (port-name p) (textual-port-output-index p) len (string-length (textual-port-output-buffer p)))
    (if (fxzero? len)
      (tport-write-char tport ch)
      (let ((buf (textual-port-output-buffer p))
            (idx (textual-port-output-index p)))
        (when (fx=? idx len)
          (tport-write tport buf 0 idx)
          (set-textual-port-output-index! p 0)
          (set! idx 0))
        (string-set! buf idx ch)
        (set-textual-port-output-index! p (fx1+ idx)))))
  (set-port-bol! p (char=? ch #\newline)))


;; Comply with Chez Scheme documentation for (block-write), that states:
;; If the port is buffered and the buffer is nonempty, the buffer is flushed before the contents of string are written.
;; In any case, the contents of string are written immediately, without passing through the buffer.
(define (utf8b-port-block-write p tport str start n)
  (assert* 'utf8b-port-block-write tport)
  (let ((buf (textual-port-output-buffer p))
        (idx (textual-port-output-index  p)))
    ;; (debugf "utfb-port-block-write port=~s idx=~s len=~s cap=~s string-to-write=~s" (port-name p) idx  (textual-port-output-size p) (string-length buf) (substring str start (fx+ start n)))
    (unless (fxzero? idx)
      (tport-write tport buf 0 idx)
      (set-textual-port-output-index! p 0)
      (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline))))
  (unless (fxzero? n)
    (set-port-bol! p (char=? (string-ref str (fx1- n)) #\newline)))
  (tport-write tport str start n)
  (tport-flush tport))


(define (utf8b-port-flush p tport)
  (assert* 'utfb-port-flush tport)
  (let ((buf (textual-port-output-buffer p))
        (idx (textual-port-output-index  p)))
    ;; (debugf "utfb-port-flush port=~s idx=~s len=~s cap=~s" (port-name p) idx (textual-port-output-size p) (string-length buf))
    (unless (fxzero? idx)
      (tport-write tport buf 0 idx)
      (set-textual-port-output-index! p 0)
      (set-port-bol! p (char=? (string-ref buf (fx1- idx)) #\newline))))
  (tport-flush tport))


(define (raise-bad-msg msg)
  (assertion-violationf 'utfb-port "operation ~s not handled" msg))


;; create and return a textual input port handler that reads/writes from/to an underlying binary port
;; and transcodes between characters and UTF-8b byte sequences
(define (make-utfb-port-handler in-port input-buffer-size out-port output-buffer-size options)
  (let ((name   (port-name (or in-port out-port)))
        (tport1 (and in-port  (make-tport in-port  (make-bytespan 0) input-buffer-size #f)))
        (tport2 (and out-port (make-tport out-port (make-bytespan 0) output-buffer-size #f))))
    ;; return a closure
    (case-lambda
      ((msg p)
        ;; (debugf "utf8b port handler for ~s: (~s ~s)" (or in-port out-port) msg p)
        (case msg
          ((char-ready?)
            (assert* 'utfb-port-char-ready? in-port)
            (or (fx>? (textual-port-output-index p) 0)
                (fx>? (tport-bspan-length tport1) 0)
                (input-port-ready? in-port)))
          ((clear-input-port)
            (assert* 'utfb-port-clear-input-port in-port)
            (clear-input-port in-port))
          ((clear-output-port)
            (assert* 'utfb-port-clear-output-port out-port)
            (set-textual-port-output-index! p 0)
            (clear-output-port out-port))
          ((close-port)
            (when (and tport2 (not (port-closed? p)))
              (utf8b-port-flush p tport2))
            (when (plist-ref options 'close? #t)
              (mark-port-closed! p)
              (when (plist-ref options 'close-inner? #t)
                (when in-port
                  (close-port in-port))
                (when out-port
                  (close-port out-port)))))
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





;; create and return a textual input and/or output port that reads/write from/to an underlying binary port
;; and transcodes between characters and UTF-8b byte sequences
;;
;; options must be a plist containing zero or more of:
;;   'close? BOOL       - if BOOL is #f (default is #t), attempts to close the returned port are ignored
;;   'close-inner? BOOL - if BOOL is #f (default is #t), closing the returned port does not close the underlying binary port
(define port->utf8b-port
  (case-lambda
    ((bin-port dir b-mode options)
      (assert* 'port->utf8b-port (binary-port? bin-port))
      (assert* 'port->utf8b-port (buffer-mode? b-mode))
      (assert* 'port->utf8b-port (plist? options))
      (case dir
        ((read)
          (let* ((in-buffer-size (b-mode->input-buffer-size b-mode))
                 (p (make-input-port
                      (make-utfb-port-handler bin-port in-buffer-size #f 0 options)
                      (make-string in-buffer-size))))
            (set-textual-port-input-index! p in-buffer-size)
            p))
        ((rw)
          (let* ((in-buffer-size  (b-mode->input-buffer-size b-mode))
                 (out-buffer-size (b-mode->output-buffer-size b-mode))
                 (p (make-input/output-port
                      (make-utfb-port-handler bin-port in-buffer-size bin-port out-buffer-size options)
                      (make-string in-buffer-size)
                      (make-string out-buffer-size))))
            (set-textual-port-input-index! p in-buffer-size)
            p))
        ((write)
          (let* ((out-buffer-size (b-mode->output-buffer-size b-mode))
                 (p (make-output-port
                      (make-utfb-port-handler #f 0 bin-port out-buffer-size options)
                      (make-string out-buffer-size))))
            p))
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
