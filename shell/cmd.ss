;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; Define the function (sh-cmd*)

(library (schemesh shell cmd (0 1))
  (export
    sh-cmd*)
  (import
    (rnrs)
    (only (chezscheme)               reverse!)
    (only (schemesh bootstrap)       raise-errorf)
    (only (schemesh containers misc) list-iterate)
    (schemesh shell jobs))


;; Create a cmd to later spawn it. Each argument must be a string, bytevector or symbol.
;; Symbol '= indicates an environment variable assignment, and must be followed
;; by the variable name (a string or bytevector) and its value (a string or bytevector).
;; All other symbols indicates a redirection and must be followed by a string or bytevector.
;; TODO: also support closures (lambda (job) ...) that return a string or bytevector.
(define (sh-cmd* . program-and-args)
  (let-values (((program-and-args assignments redirections)
                  (cmd-parse-assignments-and-redirections program-and-args)))
    (let ((cmd (apply sh-cmd program-and-args)))
      ;; FIXME: apply parsed assignments NAME = VALUE
      (list-iterate redirections
        (lambda (redirection)
          (let ((from (car redirection))
                (op   (cadr redirection))
                (to   (caddr redirection)))
            (if (or (eq? op '<&) (eq? op '>&))
              (sh-fd-redirect! cmd from to)
              (sh-file-redirect! cmd from op to)))))
      cmd)))



;; parse environment variable assignments NAME = VALUE
;; parse redirections [N]<PATH [N]<>PATH [N]<&M [N]>PATH [N]>>PATH [N]>&M
(define (cmd-parse-assignments-and-redirections program-and-args)
  (let %again ((args program-and-args)
               (rets '())
               (assignments '())
               (redirections '()))
    (cond
      ((null? args)
        (values (reverse! rets) (reverse! assignments) (reverse! redirections)))
      ((and (not (null? (cdr args))) (eq? (cadr args) '=))
        (unless (null? rets)
          (raise-errorf 'sh-cmd* "env assignments are not allowed after arguments, found ~s after ~s" args (reverse! rets)))
        (unless (null? redirections)
          (raise-errorf 'sh-cmd* "env assignments are not allowed after redirections, found ~s after ~s" args (car redirections)))
        (let ((assignment (cmd-parse-assignment args)))
          (%again (cdddr args) rets (cons assignment assignments) redirections)))
      ((or (string? (car args)) (procedure? (car args)))
        (unless (null? redirections)
          (raise-errorf 'sh-cmd* "arguments are not allowed after redirections, found ~s after ~s" args (car redirections)))
        (%again (cdr args) (cons (car args) rets) assignments redirections))
      ((memq (car args) '(< <> <& > >> >&))
        (let ((redirection (cmd-parse-redirection args)))
          (%again (cddr args) rets assignments (cons redirection redirections))))
      ((and (fixnum? (car args)) (not (null? (cdr args))) (memq (cadr args) '(< <> <& > >> >&)))
        (let ((redirection (cmd-parse-fd-redirection args)))
          (%again (cdddr args) rets assignments (cons redirection redirections))))
      (#t
        (raise-errorf 'sh-cmd* "expecting assignment, argument or redirection, found: ~s" args)))))



;; parse a single redirection <PATH <>PATH <&M >PATH >>PATH >&M
(define (cmd-parse-redirection args)
  (when (null? (cdr args))
    (raise-errorf 'sh-cmd* "missing argument after redirection: " args))
  (let ((op (car args))
        (to (cadr args)))
    (case op
      ((< <> > >>)
        (unless (or (string? to) (procedure? to))
          (raise-errorf 'sh-cmd* "expecting string or closure after redirection, found: ~s ~s" op to)))
      ((<& >&)
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-cmd* "expecting -1 or unsigned fixnum after redirection, found: ~s ~s" op to)))
      (else
        (raise-errorf 'sh-cmd* "invalid redirection operator: ~s" op)))
    (list (if (memq op '(< <> <&)) 0 1) op to)))


;; parse a single redirection N<PATH N<>PATH N<&M N>PATH N>>PATH N>&M
(define (cmd-parse-fd-redirection args)
  (when (or (null? (cdr args)) (null? (cddr args)))
    (raise-errorf 'sh-cmd* "missing argument after redirection: " args))
  (let ((from (car args))
        (op   (cadr args))
        (to   (caddr args)))
    (unless (and (fixnum? from) (fx>=? from 0))
      (raise-errorf 'sh-cmd* "expecting unsigned fixnum before redirection, found: ~s ~s ~s" from op to))
    (case op
      ((< <> > >>)
        (unless (or (string? to) (procedure? to))
          (raise-errorf 'sh-cmd* "expecting string or closure after redirection, found: ~s ~s ~s" from op to)))
      ((<& >&)
        (unless (and (fixnum? to) (fx>=? to -1))
          (raise-errorf 'sh-cmd* "expecting -1 or unsigned fixnum after redirection, found: ~s ~s ~s" from op to)))
      (else
        (raise-errorf 'sh-cmd* "invalid redirection operator: ~s" op)))
    (list from op to)))


;; parse a single environment variable assignment NAME = VALUE
(define (cmd-parse-assignment args)
  (when (or (null? (cdr args)) (null? (cddr args)))
    (raise-errorf 'sh-cmd* "missing value after assignment: ~s" args))
  (let ((name  (car args))
        (op    (cadr args))
        (value (caddr args)))
    (unless (eq? op '=)
      (raise-errorf 'sh-cmd* "invalid assignment operator: ~s" op))
    (unless (and (string? name) (not (fxzero? (string-length name))))
      (raise-errorf 'sh-cmd* "expecting non-empty string before assignment, found: ~s ~s ~s" name op value))
    (unless (or (string? value) (procedure? value))
      (raise-errorf 'sh-cmd* "expecting string or closure after assignment, found: ~s ~s ~s" name op value))
    (list name op value)))


) ; close library
