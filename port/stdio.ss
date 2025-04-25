;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; high-level procedures for reading from and writing to ports.
;;;
;;; procedure names and effect are intentionally compatible with
;;; https://docs.racket-lang.org/reference/port-lib.html
;;;
(library (schemesh port stdio (0 8 3))
  (export
          sh-stdio-cleanup sh-stdio-flush sh-stdin sh-stdout sh-stderr)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) set-port-eof! )
    (only (schemesh bootstrap) assert* sh-make-parameter))


;; Return binary input port that reads or writes bytes from current standard input
(define sh-stdin
  (sh-make-parameter
    #f
    (lambda (port)
      (when port
        (assert* 'sh-stdin (binary-port? port)))
      port)))


;; Return binary output port that reads or writes bytes to current standard output
(define sh-stdout
  (sh-make-parameter
    #f
    (lambda (port)
      (when port
        (assert* 'sh-stdout (binary-port? port)))
      port)))


;; Return binary output port that reads or writes bytes to current standard error
(define sh-stderr
  (sh-make-parameter
    #f
    (lambda (port)
      (when port
        (assert* 'sh-stderr (binary-port? port)))
      port)))


(define (try-port-cleanup port)
  (when (input-port? port)
    (set-port-eof! port #f)))

(define (try-port-flush port)
  (when (output-port? port)
    (flush-output-port port)))

(define (sh-stdio-cleanup)
  (try-port-cleanup (current-input-port))
  (try-port-cleanup (current-output-port))
  (try-port-cleanup (current-error-port))
  (try-port-cleanup (sh-stdin))
  (try-port-cleanup (sh-stdout))
  (try-port-cleanup (sh-stderr)))


(define (sh-stdio-flush)
  ;; the ports (console-input-port) (console-output-port) (console-error-port)
  ;; and (sh-stdin) (sh-stdout) (sh-stderr) are unbuffered, no need to flush them
  (try-port-flush (current-input-port))
  (try-port-flush (current-output-port))
  (try-port-flush (current-error-port)))



) ; close library
