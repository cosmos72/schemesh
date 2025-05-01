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
(library (schemesh port stdio (0 9 0))
  (export
          sh-stdio-cleanup sh-stdio-flush sh-stdin sh-stdout sh-stderr)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         logbit? procedure-arity-mask set-port-eof!)
    (only (schemesh bootstrap) assert*))


(define (validate-stdio-proc caller old-proc new-proc)
  (assert* caller (not old-proc))
  (assert* caller (procedure? new-proc))
  (assert* caller (logbit? 0 (procedure-arity-mask new-proc))))


;; Return binary input port that reads or writes bytes from current standard input
(define sh-stdin
  (let ((proc #f))
    (case-lambda
      (()
        ;; shell/init.ss will install a procedure returning binary standard input port of current job
        (and proc (proc)))
      ((new-proc)
        (validate-stdio-proc 'sh-stdin proc new-proc)
        (set! proc new-proc)))))


;; Return binary output port that reads or writes bytes to current standard output
(define sh-stdout
  (let ((proc #f))
    (case-lambda
      (()
        ;; shell/init.ss will install a procedure returning binary standard output port of current job
        (and proc (proc)))
      ((new-proc)
        (validate-stdio-proc 'sh-stdout proc new-proc)
        (set! proc new-proc)))))


;; Return binary output port that reads or writes bytes to current standard error
(define sh-stderr
  (let ((proc #f))
    (case-lambda
      (()
        ;; shell/init.ss will install a procedure returning binary standard error port of current job
        (and proc (proc)))
      ((new-proc)
        (validate-stdio-proc 'sh-stderr proc new-proc)
        (set! proc new-proc)))))


(define (try-port-cleanup port)
  (when (input-port? port)
    (set-port-eof! port #f)))

(define (try-port-flush port)
  (when (output-port? port)
    (flush-output-port port)))

(define (sh-stdio-cleanup)
  (try-port-cleanup (current-input-port))
  (try-port-cleanup (current-output-port))
  (try-port-cleanup (current-error-port)))


(define (sh-stdio-flush)
  ;; the ports (console-input-port) (console-output-port) (console-error-port)
  ;; are unbuffered, no need to flush them
  ;;
  ;; flushing (current-...-port) also flushes (sh-stdin) (sh-stdout) and (sh-stderr)
  (try-port-flush (current-input-port))
  (try-port-flush (current-output-port))
  (try-port-flush (current-error-port)))



) ; close library
