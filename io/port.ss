;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

;;; high-level procedures for reading from and writing to ports.
;;;
;;; procedure names and effect are intentionally compatible with
;;; https://docs.racket-lang.org/reference/port-lib.html
;;;
(library (scheme2k io port (0 9 3))
  (export byte-lines->port lines->port open-input-nowhere open-output-nowhere
          peek-char2 port->list port->string port->bytes port->lines port->bytes-lines
          read-line read-bytes-line read-bytes-insert-right!)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (only (chezscheme)                 bytevector-truncate! bytevector-u24-ref bytevector-u24-set! endianness fx/ fx1+ fx1-
                                       get-bytevector-some! set-textual-port-input-buffer! set-textual-port-input-index! set-textual-port-input-size!
                                       string-copy!         textual-port-input-buffer textual-port-input-index textual-port-input-size void)
    (only (scheme2k bootstrap)         assert* assert-not* check-interrupts fx<=?* raise-errorf)
    (scheme2k containers bytespan)
    (only (scheme2k io stdio)          sh-stdin sh-stdout))


(define (open-binary-input-nowhere name)
  (make-custom-binary-input-port
    name
    (lambda (bv start n) 0) ; read-proc
    (lambda () 0)           ; get-pos-proc
    (lambda (pos) (void))   ; set-pos-proc
    #f))                    ; close-proc


(define (open-textual-input-nowhere name)
  (make-custom-textual-input-port
    name
    (lambda (str start n) 0) ; read-proc
    (lambda () 0)            ; get-pos-proc
    (lambda (pos) (void))    ; set-pos-proc
    #f))                     ; close-proc


(define (open-binary-output-nowhere name)
  (make-custom-binary-output-port
    name
    (lambda (bv start n) n) ; write-proc
    (lambda () 0)           ; get-pos-proc
    (lambda (pos) (void))   ; set-pos-proc
    #f))                    ; close-proc


(define (open-textual-output-nowhere name)
  (make-custom-textual-output-port
    name
    (lambda (str start n) n) ; write-proc
    (lambda () 0)            ; get-pos-proc
    (lambda (pos) (void))    ; set-pos-proc
    #f))                     ; close-proc


;; conforms to Racket (open-input-nowhere), adds optional second argument transcoder-sym
(define open-input-nowhere
  (case-lambda
    ((name transcoder-sym)
      (case transcoder-sym
        ((binary)
          (open-binary-input-nowhere name))
        ((textual utf8b)
          (open-textual-input-nowhere name))
        (else
          (raise-errorf 'open-output-nowhere "~s is not a supported transcoder symbol. Supported values are ~s"
            transcoder-sym '(binary textual utf8b)))))
    ((name)
      (open-textual-output-nowhere name))
    (()
      (open-textual-output-nowhere "/dev/null"))))


;; conforms to Racket (open-output-nowhere), replaces optional second argument special-ok? -> transcoder-sym
(define open-output-nowhere
  (case-lambda
    ((name transcoder-sym)
      (case transcoder-sym
        ((binary)
          (open-binary-output-nowhere name))
        ((textual utf8b)
          (open-textual-output-nowhere name))
        (else
          (raise-errorf 'open-output-nowhere "~s is not a supported transcoder symbol. Supported values are ~s"
            transcoder-sym '(binary textual utf8b)))))
    ((name)
      (open-textual-output-nowhere name))
    (()
      (open-textual-output-nowhere "/dev/null"))))


;; Peek the next-next character (i.e. the character after (peek-char))
;; from textual input port and return it, without consuming any character.
;;
;; This is needed by (parsectx-try-read-directive) and by (lex-sharp)
;; and implementing it is messy: we must fiddle with port's input buffer.
;;
;; if optional argument temp is a string with length > 0 and <= half port's input buffer length,
;; it is used as a temporary read buffer instead of allocating a string
(define peek-char2
  (case-lambda
    ((in)
      (%peek-char2 in 3 #f))
    ((in temp)
      (%peek-char2 in 3 temp))))


;; implementation of (peek-char2)
(define (%peek-char2 in retry-n temp)
  (cond
    ((port-eof? in)
      (eof-object))
    ((fxzero? retry-n)
      (raise-errorf 'peek-char2 "failed peeking two characters forward in ~s" in))
    (else
      (let* ((pos  (textual-port-input-index in))
             (size (textual-port-input-size in))
             (buf  (textual-port-input-buffer in))
             (cap  (string-length buf)))
        (assert* 'peek-char2 (fx<=?* 0 pos size cap)) ; sanity check
        (assert* 'peek-char2 (fx>=? cap 2)) ; we need at least a 2 char buffer
        (when temp
          (assert-not* 'peek-char2 (eq? buf temp)))
        (case (fx- size pos)
          ((0)
            (peek-char in)           ; read more characters
            (%peek-char2 in (fx1- retry-n) temp)) ; then retry
          ((1)
            ;; this is the messy case: we must read more characters, without consuming the buffered one
            (let* ((ch0   (read-char in))
                   (cap/2 (fx/ cap 2))
                   (temp  (if (and (string? temp) (fx<=? 1 (string-length temp) cap/2))
                            (begin
                              ;; (debugf "peek-char2 messy step 0 using caller-provided temp string")
                              temp)
                            (make-string cap/2)))
                   (half  (string-length temp)))
              ;; (debugf "peek-char2 messy step A pos ~s, size ~s, cap ~s, ch0 ~s, buf ~s" pos size cap ch0 buf)
              (dynamic-wind
                (lambda ()
                  (set-textual-port-input-buffer! in temp) ; guaranteed to set index = 0 and size = half
                  (set-textual-port-input-size!   in half) ; guaranteed to set index = 0
                  (set-textual-port-input-index!  in half))
                (lambda ()
                  ;; here's the char we wanted.
                  (peek-char in))
                (lambda ()
                  ;; reload everything, in case something changed
                  (let* ((pos  (textual-port-input-index in))
                         (size (textual-port-input-size in))
                         (temp (textual-port-input-buffer in))
                         (tcap (string-length temp)))
                    (assert-not* 'peek-char2-cleanup (eq? buf temp))
                    (assert* 'peek-char2-cleanup (fx<=?* 0 pos size tcap))
                    ;; (debugf "peek-char2 messy step B pos ~s, size ~s, tcap ~s, ch0 ~s, ch1 ~s, temp ~s" pos size tcap ch0 ch1 temp)
                    (let* ((avail   (fx- size pos))
                           (new-pos (fx- cap avail)))
                      ;; copy buffered data to end of buf
                      (string-copy! temp pos buf new-pos avail)
                      ;; copy back ch0
                      (string-set! buf (fx1- new-pos) ch0)
                      ;; restore buffer, size and index
                      (set-textual-port-input-buffer! in buf) ; guaranteed to set index = 0 and size = cap
                      (set-textual-port-input-size!   in cap) ; guaranteed to set index = 0
                      (set-textual-port-input-index!  in (fx1- new-pos))
                      ;; (debugf "peek-char2 messy step C ch0 ~s, ch1 ~s, buf ~s" ch0 ch1 buf)
                      ))))))
          (else
            ;; enough characters are already buffered
            (string-ref buf (fx1+ pos))))))))


;; Given a list of bytevectors, write each one to port out, appending a newline after each bytevector.
;; The line-mode argument is ignored.
;;
;; out defaults to (sh-stdout) and close? defaults to #f
(define byte-lines->port
  (case-lambda
    ((lines out line-mode close?)
      (do ((l lines (cdr l)))
          ((null? l))
        (put-bytevector out (car l))
        (put-u8 out 10))
      (when close?
        (close-port out)))
    ((lines out line-mode)
      (byte-lines->port lines out line-mode #f))
    ((lines out)
      (byte-lines->port lines out 'any #f))
    ((lines)
      (byte-lines->port lines (sh-stdout) 'any #f))))


;; Given a list of strings, write each one to port out, appending a newline after each string.
;; The line-mode argument is ignored.
;;
;; out defaults to (current-out-port) and close? defaults to #f
(define lines->port
  (case-lambda
    ((lines out line-mode close?)
      (do ((l lines (cdr l)))
          ((null? l))
        (put-string out (car l))
        (put-char out #\newline))
      (when close?
        (close-port out)))
    ((lines out line-mode)
      (lines->port lines out line-mode #f))
    ((lines out)
      (lines->port lines out 'any #f))
    ((lines)
      (lines->port lines (current-output-port) 'any #f))))


;; return a list whose elements are produced by calling (proc in) until it produces eof.
;;
;; proc defaults to read, and in defaults to (current-input-port)
;;
;; conforms to: Racket library (racket/port)
(define port->list
  (case-lambda
    ((proc in)
      (let ((head (cons #f '())))
        (let %port->list ((tail head))
          (let ((obj (proc in)))
            (if (eof-object? obj)
              (cdr head)
              (let ((next (cons obj '())))
                (set-cdr! tail next)
                (%port->list next)))))))
    ((proc)
      (port->list proc (current-input-port)))
    (()
      (port->list read (current-input-port)))))


;; reads all characters from in and returns them as a string.
;; The input port is closed if close? is truish.
;;
;; in defaults to (current-input-port) and close? defaults to #f
;;
;; conforms to: Racket library (racket/port) version >= 6.8.0.2
(define port->string
  (case-lambda
    ((in close?)
      (let ((s (get-string-all in)))
        (when close?
          (close-port in))
        s))
    ((in)
      (port->string in #f))
    (()
      (port->string (current-input-port) #f))))


;; reads all bytes from in and returns them as a bytevector.
;; The input port is closed if close? is truish.
;;
;; in defaults to (sh-stdin) and close? defaults to #f
;;
;; conforms to: Racket library (racket/port) version >= 6.8.0.2
(define port->bytes
  (case-lambda
    ((in close?)
      (let ((s (get-bytevector-all in)))
        (when close?
          (close-port in))
        s))
    ((in)
      (port->bytes in #f))
    (()
      (port->bytes (sh-stdin) #f))))


;; Read all characters from in, breaking them into lines.
;; The line-mode argument is ignored.
;; The input port is closed if close? is truish.
;;
;; in defaults to (current-input-port) and close? defaults to #f
;;
;; conforms to: Racket library (racket/port) version >= 6.8.0.2
(define port->lines
  (case-lambda
    ((in line-mode close?)
      (let ((l (port->list get-line in)))
        (when close?
          (close-port in))
        l))
    ((in line-mode)
      (port->lines in line-mode #f))
    ((in)
      (port->lines in 'any #f))
    (()
      (port->lines (current-input-port) 'any #f))))


;; Read all bytes from in, breaking them into lines.
;; The line-mode argument is ignored.
;; The input port is closed if close? is truish.
;;
;; in defaults to (sh-stdin) and close? defaults to #f
;;
;; conforms to: Racket library (racket/port) version >= 6.8.0.2
(define port->bytes-lines
  (case-lambda
    ((in line-mode close?)
      (let ((l (port->list read-bytes-line in)))
        (when close?
          (close-port in))
        l))
    ((in line-mode)
      (port->bytes-lines in line-mode #f))
    ((in)
      (port->bytes-lines in 'any #f))
    (()
      (port->bytes-lines (sh-stdin) 'any #f))))


;; Returns a string containing the next line of characters from in.
;; Characters are read from in until a line separator or an end-of-file is read.
;; The line separator is not included in the result string (but it is removed from the port’s stream).
;; If no characters are read before an end-of-file is encountered, eof is returned.
;;
;; in defaults to (current-input-port) and mode is ignored
;;
;; conforms to: Racket library (racket)
(define read-line
  (case-lambda
    ((in mode)
      (get-line in))
    ((in)
      (get-line in))
    (()
      (get-line (current-input-port)))))


;; Returns a bytevector containing the next line of bytes from in.
;; Bytes are read from in until a line separator or an end-of-file is read.
;; The line separator is not included in the result string (but it is removed from the port’s stream).
;; If no bytes are read before an end-of-file is encountered, #!eof is returned.
;;
;; Optional arguments:
;;   in:   binary input port. Defaults to (sh-stdin)
;;   mode: always ignored
;;
;; conforms to: Racket library (racket)
(define read-bytes-line
  (case-lambda
    ((in mode)
      (let %read-bytes-line ((bsp (make-bytespan 0)))
        (let ((b (get-u8 in)))
          (cond
            ((eof-object? b)
              (if (bytespan-empty? bsp)
                b
                (bytespan->bytevector*! bsp)))
            ((memv b '(10 13))
              (let ((next (lookahead-u8 in)))
                (when (and (memv next '(10 13)) (not (eqv? b next)))
                  ;; coalesce CR+LF and LF+CR
                  (get-u8 in)))
              (bytespan->bytevector*! bsp))
            (else
              (bytespan-insert-right/u8! bsp b)
              (%read-bytes-line bsp))))))
    ((in)
      (read-bytes-line in 'any))
    (()
      (read-bytes-line (sh-stdin) 'any))))


;; read some bytes from binary input port and append them to specified bytespan.
;; return number of bytes actually read, which can be 0 only on end-of-file,
;; or raise exception on I/O error.
;;
;; Added in scheme2k 0.9.3
(define (read-bytes-insert-right! in bsp)
  (bytespan-reserve-right! bsp (fx+ 4096 (bytespan-length bsp)))
  (check-interrupts)
  (let* ((beg   (bytespan-peek-beg bsp))
         (end   (bytespan-peek-end bsp))
         (cap   (bytespan-capacity-right bsp))
         (max-n (fx- (fx+ beg cap) end))
         (n     (get-bytevector-some! in (bytespan-peek-data bsp) end max-n)))
    (cond
      ((and (fixnum? n) (fx>? n 0))
        (bytespan-resize-right! bsp (fx+ (fx- end beg) n))
        n)
      (else
        0)))) ; eof


) ; close library
