;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix status (0 8 2))
  (export
       exit-with-status
       new running stopped exception failed killed ok
       status? status->kind status->value list->ok ok->list ok->values
       new? started? running? stopped? finished? ok?)
  (import
    (rnrs)
    (only (chezscheme)            console-output-port console-error-port fx1+ include record-writer void)
    (only (schemesh bootstrap)    assert*)
    (only (schemesh containers hashtable) for-hash plist->eq-hashtable)
    (schemesh wire)
    (only (schemesh posix fd)     c-exit)
    (only (schemesh posix signal) signal-name->number signal-raise))


;; Status of a sh-job or process.
;; Kind can be of: 'new 'running 'stopped 'ok 'failed 'killed 'exception
;; Possible values depend on kind:
;;   if kind is 'new       -> val is '()
;;   if kind is 'running   -> val is #f or job-id
;;   if kind is 'stopped   -> val is a signal-name represented as a symbol
;;   if kind is 'exception -> val is a Scheme condition object
;;   if kind is 'failed    -> val is an 8-bit C exit status or an arbitrary Scheme value
;;   if kind is 'killed    -> val is a signal-name represented as a symbol
;;   if kind is 'ok        -> val is a possibly empty list of arbitrary Scheme values
;;
(define-record-type
  (%status %make-status %status?)
  (fields
    (immutable kind %status->kind) ; symbol
    (immutable val  %status->val))
  (nongenerative %status-7c46d04b-34f4-4046-b5c7-b63753c1be39))



;; return #t if s is a status, otherwise return #f
(define (status? status)
  (or (eq? (void) status) (%status? status)))

(define s-new      (%make-status 'new #t))
(define s-running  (%make-status 'running  #f))
(define s-stopped-sigtstp (%make-status 'stopped 'sigtstp))
(define s-failed-f (%make-status 'failed #f))
(define s-failed-1 (%make-status 'failed 1))
(define s-ok-empty (%make-status 'ok '()))
(define list1-void (list (void)))

;; create a status 'new
(define (new)
  s-new)

;; create a status 'running with optional job-id
(define running
  (case-lambda
    ((job-id)
      (if job-id
        (begin
          (assert* 'running (fixnum? job-id))
          (assert* 'running (fx>? job-id 0))
          (%make-status 'running job-id))
        s-running))
    (()
      s-running)))


;; create a status 'stopped with signal-name
(define (stopped signal-name)
  (when signal-name
    (assert* 'stopped (symbol? signal-name)))
  (if (eq? 'sigtstp signal-name)
    s-stopped-sigtstp
    (%make-status 'stopped signal-name)))


;; create a status 'killed with signal-name
(define (killed signal-name)
  (when signal-name
    (assert* 'killed (symbol? signal-name)))
  (%make-status 'killed signal-name))


;; create a status 'exception with condition object ex
(define (exception ex)
  (when ex
    (assert* 'exception (condition? ex)))
  (%make-status 'exception ex))


;; create a status 'failed with value val
(define (failed val)
  (case val
    ((#f)  s-failed-f)
    ((1)   s-failed-1)
    (else  (%make-status 'failed val))))


;; create a status 'ok with zero or more values,
;; or (failed #f) if the only value is #f
(define (ok . vals)
  (list->ok vals))


(define (list->ok vals)
  (assert* 'list->ok (list? vals))
  (cond
    ((null? vals)
      s-ok-empty)
    ((and (pair? vals) (null? (cdr vals)))
      (let ((val (car vals)))
        (cond ((not val)      s-failed-f)
              ((status? val)  val) ;; also catches (void)
              (else           (%make-status 'ok vals)))))
    (else
      (%make-status 'ok vals))))


;; Extract the kind of a status and return it.
;; Possible returned kinds are: 'new 'running 'stopped 'ok 'exception 'failed 'killed
(define (status->kind status)
  (if (eq? (void) status)
    'ok
    (%status->kind status)))



;; if (ok? status) is #t, return the first result stored in status.
;; otherwise return the value stored in status
(define (status->value status)
  (if (eq? (void) status)
    (void)
    (let ((val (%status->val status)))
      (if (eq? 'ok (%status->kind status))
        (if (null? val) (void) (car val))
        val))))


;; if (ok? status) is #t, return the list of results stored in status
;;    Note: the status (void) represents a successfully finished job that returned (void)
;;          thus the returned results is a single-element list containing (void)
;; if (ok? status) is #f, return the empty list.
;;
;; DO NOT modify the returned list!
(define (ok->list status)
  (cond
    ((eq? (void) status)
      list1-void)
    ((eq? 'ok (%status->kind status))
      (%status->val status))
    (else
      '())))


;; if (ok? status) is #t, return the results as multiple values.
;; if (ok? status) is #f, return no values
(define (ok->values status)
  (if (eq? (void) status)
    (void)
    (apply values (ok->list status))))


;; Return #t if status represents a new job, i.e. its kind is 'new
;; otherwise return #f
(define (new? status)
  (eq? 'new (status->kind status)))


;; Return #t if status represents a started job, i.e. its kind is one of 'running 'stopped.
;; otherwise return #f
(define (started? status)
  (if (memq (status->kind status) '(running stopped))
    #t
    #f))


;; Return #t if status represents a running job, i.e. its kind is 'running.
;; otherwise return #f
(define (running? status)
  (eq? 'running (status->kind status)))


;; Return #t if status represents a stopped job, i.e. its kind is 'stopped.
;; otherwise return #f
(define (stopped? status)
  (eq? 'stopped (status->kind status)))


;; Return #t if status represents a finished job, i.e. its kind is one of 'ok 'exception 'failed 'killed.
;; otherwise return #f
(define (finished? status)
  (if (memq (status->kind status) '(ok exception failed killed))
    #t
    #f))


;; return #t if status kind is 'ok, i.e. if job finished successfully.
;; otherwise return #f
(define (ok? status)
  (eq? 'ok (status->kind status)))


;; Extract the first value of a status, convert it to a fixnum, and return it.
(define (status->c-exit-value status)
  (if (ok? status)
    0
    (let* ((val    (%status->val status))
           (value  (if (fixnum? val) (fxand 255 val) 255)))
      (if (fxzero? value) 1 value))))


;; Call C functions kill() or exit() to terminate current process with job-status,
;; which can be one of:
;;   (void)                       ; will call C function exit(0)
;;   (failed  exit-status)  ; will call C function exit(exit_status)
;;   (killed  signal-name)  ; will call C function kill(getpid(), signal_number)
;;               ; unless signal-name is one of: 'sigstop 'sigtstp 'sigcont 'sigttin 'sigttou
;;               ; if kill() returns, will call C function exit(128 + signal_number)
;;   ... any other value ... ;  will call C function exit(255)
(define (exit-with-status status)
  ;; (debugf "exit-with-status ~s" status)
  (let ((c-exit-value (status->c-exit-value status)))
    ;x (debugf "exit-with-status status=~s" status)
    (dynamic-wind
      void       ; before body
      (lambda () ; body
        (flush-output-port (console-output-port))
        (flush-output-port (console-error-port))
        (flush-output-port (current-output-port))
        (flush-output-port (current-error-port))
        (when (eq? 'killed (status->kind status))
          (let ((signal-name (%status->val status)))
            (unless (memq signal-name '(sigstop sigtstp sigcont sigttin sigttou))
              (signal-raise signal-name))
            ; process did not die with (signal-raise)
            (let ((signal-number (signal-name->number signal-name)))
              (when (fixnum? signal-number)
                (set! c-exit-value (fx+ 128 signal-number)))))))
      (lambda () ; after body
        (c-exit (fxand c-exit-value 255))))))


; customize how "status" objects are serialized/deserialized
(include "posix/wire-status.ss")


; customize how "status" objects are printed
(record-writer (record-type-descriptor %status)
  (lambda (status port writer)
    (let ((kind (%status->kind status))
          (val  (%status->val status)))
      (put-string port "(")
      (put-datum port kind)
      (case kind
        ((new)
          (void))
        ((ok)
          (do ((vals val (cdr vals)))
              ((null? vals))
            (put-char  port #\space)
            (put-datum port (car vals))))
        (else
          (when (or val (not (eq? 'running kind)))
            (put-char port #\space)
            (put-datum port val))))
      (put-string port ")"))))

(wire-register-rtd (record-type-descriptor %status) tag-status
                   wire-len/status wire-get/status wire-put/status)


) ; close library
