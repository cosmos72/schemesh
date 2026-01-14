;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; validate a single (sh-start) job option.
;;
;; return unspecified value.
;; if key or value is not valid, raise an exception.
(define (option-validate caller key value)
  (let ((job-supported-options '(catch? fd-close process-group-id spawn?)))
    (assert* caller (memq key job-supported-options)))
  (case key
    ((fd-close process-group-id)
      (assert* caller (integer? value))
      (assert* caller (>= value 0)))
    (else
      (assert* caller (boolean? value)))))


;; validate a property list of (sh-start) job options:
;; raise an exception if it contains one or more unsupported options.
(define (options-validate caller options)
  (assert* caller (plist? options))
  (for-plist ((key value options))
    (option-validate caller key value)))


;; create and return property list usable for (sh-start) job options.
;;
;; options must be a list containing zero or more:
;;
;;   (void) or #f followed by arbitrary value - ignored, and omitted from returned list.
;;
;;   'catch? flag - flag must be a boolean, otherwise an exception will be raised.
;;     If present and flag is #t, any Scheme condition raised by starting
;;     the job will be captured, and job status will be set to (list exception #<condition>)
;;
;;   'fd-close fd - fd must be an integer and >= 0, otherwise an exception will be raised.
;;     If present, specified file descriptor will be closed when starting the job.
;;     Useful mostly together with 'spawn? #t because the file descriptor will be closed
;;     only in the subprocess.
;;
;;   'process-group-id id - id must be an integer and >= 0, otherwise an exception will be raised.
;;     If present, the new process will be inserted into the corresponding
;;     process group id - which must be either 0 or an already exist one.
;;
;;   'spawn? flag - flag must be a boolean, otherwise an exception will be raised.
;;     If present and flag is #t, then job will be started in a subprocess.
;;     By design, commands and (sh-subshell) are always started in a subprocess,
;;     and for them the 'spawn option has no effect - it is enabled by default.
;;
;;     Instead builtins and multijobs such as (sh-and) (sh-or) (sh-list) (sh-pipe) (sh-expr) ...
;;     are usually started in the main schemesh process: this is convenient and fast,
;;     but may deadlock if their file descriptors contain pipes whose other end
;;     is read/written by the main schemesh process too.
;;
;;     The option 'spawn? #t causes builtins and multijobs to start in a subprocess too.
;;     It is slower, but has the beneficial effect that reading/writing
;;     their redirected file descriptors from main schemesh process will no longer deadlock.
;;
(define (sh-options options)
  (let ((ret '()))
    (for-plist ((key value options))
      (when (and key (not (eq? (void) key)))
        (option-validate 'sh-options key value)
        (set! ret (cons value (cons key ret)))))
    (reverse! ret)))


;; return a copy of property list options without any occurrence of keys-to-remove
(define (options-filter-out options keys-to-remove)
  (if (null? keys-to-remove)
    options
    (plist-delete/pred options (lambda (key) (memq key keys-to-remove)))))


;; if options contain 'catch? flag, return such flag.
;; otherwise return #f
(define (options->catch? options)
  (let ((val (plist-ref options 'catch?)))
    (assert* 'options->catch? (boolean? val))
    val))


;; if options contain 'spawn? flag, return such flag.
;; otherwise return #f
(define (options->spawn? options)
  (let ((val (plist-ref options 'spawn?)))
    (assert* 'options->spawn? (boolean? val))
    val))


;; if job control is active and options contain 'process-group-id id, return such id.
;; otherwise return #f
(define (options->process-group-id options)
  (if (sh-job-control?)
    (let ((val (plist-ref options 'process-group-id)))
      (if val
        (let ((caller 'options->process-group-id))
          (assert* caller (integer? val))
          (assert* caller (>= val 0))
          val)
        0)) ; default: create a new process group

    ; if job control is inactive, as for example in a subshell,
    ; ignore requests to move a process into a specific process group id
    ; or to create a new process group id
    #f))


;; for each option 'fd-close fd in options, call (fd-close fd)
;; return unspecified value
(define (options->call-fd-close options)
  (for-plist ((key val options))
    (when (eq? 'fd-close key)
      (fd-close val))))
