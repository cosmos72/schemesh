;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
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
(library (schemesh port (0 9 2))
  (export port->list port->string port->bytes port->lines port->bytes-lines
          byte-lines->port lines->port read-line read-bytes-line

          ;; http.ss
          http-init http-open http-error-string http-read http-close http->port http-url->port

          ;; redir.ss
          binary-port-lambda->port textual-port-lambda->port

          ;; stdio.ss
          sh-stdio-cleanup sh-stdin sh-stdout sh-stderr)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (schemesh bootstrap) assert*)
    (schemesh containers bytespan)
    (schemesh port http)
    (schemesh port redir)
    (schemesh port stdio))


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


;; return a list whose elements are produced by calling proc on in until it produces eof.
;;
;; proc defaults to read, and in defaults to (current-input-port)
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
(define read-line
  (case-lambda
    ((in mode)
      (get-line in))
    ((in)
      (get-line in))
    (()
      (get-line (current-input-port)))))


;; Returns a bytevectro containing the next line of bytes from in.
;; Bytes are read from in until a line separator or an end-of-file is read.
;; The line separator is not included in the result string (but it is removed from the port’s stream).
;; If no bytes are read before an end-of-file is encountered, eof is returned.
;;
;; in defaults to (sh-stdin) and mode is ignored
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

) ; close library
