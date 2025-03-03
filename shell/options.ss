;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; validate a single (sh-start) job option.
;;
;; if option is valid, return it.
;; otherwise raise an exception.
(define (option-validate caller option)
  (assert* caller (pair?   option))
  (assert* caller (symbol? (car option)))
  (assert* caller (memq    (car option) '(catch? parent-job process-group-id spawn?)))
  (case (car option)
    ((catch? spawn?)
      (assert* caller (boolean? (cdr option))))
    ((process-group-id)
      (assert* caller (integer? (cdr option)))
      (assert* caller (>= (cdr option) 0))))
  option)


;; validate an association list of (sh-start) job options:
;; raise an exception if it contains one or more unsupported options.
(define (options-validate caller options)
  (assert* caller (list? options))
  (list-iterate options
    (lambda (option)
      (option-validate caller option))))


;; create and return association list usable for (sh-start) job options.
;;
;; each option must be one of the following:
;;
;;   (void) or #f - ignored, and omitted from returned list.
;;
;;   (cons 'catch? flag) - flag must be a boolean, otherwise an exception will be raised.
;;     If present and flag is #t, any Scheme condition raised by starting
;;     the job will be captured, and job status will be set to (list exception #<condition>)
;;
;;   (cons 'process-group-id id) - id must be an integer and >= 0, otherwise an exception will be raised.
;;     If present, the new process will be inserted into the corresponding
;;     process group id - which must be either 0 or an already exist one.
;;
;;   (cons 'spawn? flag) - flag must be a boolean, otherwise an exception will be raised.
;;     If present and flag is #t, then job will be started in a subprocess.
;;     By design, commands and (sh-subshell) are always started in a subprocess,
;;     and for them the 'spawn option has no effect - it is enabled by default.
;;
;;     Instead builtins and multijobs such as (sh-and) (sh-or) (sh-list) (sh-pipe) ...
;;     are usually started in the main schemesh process:
;;     this is convenient and fast, but may deadlock if their file descriptors
;;     contain pipes whose other end is read/written by the main schemesh process too.
;;
;;     The option '(spawn? . #t) causes builtins and multijobs to start in a subprocess too.
;;     It is slower, but has the beneficial effect that reading/writing
;;     their redirected file descriptors from main schemesh process will no longer deadlock.
;;
(define (sh-options . options-or-false)
  (filter
    (lambda (option)
      (if (and option (not (eq? (void) option)))
        (option-validate 'sh-options option)
        #f))
    options-or-false))


;; return a copy of association list options without any occurrence of keys-to-remove
(define (options-filter-out options keys-to-remove)
  (if (null? keys-to-remove)
    options
    (filter
      (lambda (option)
        (assert* 'options-filter-out (pair? option))
        (assert* 'options-filter-out (symbol? (car option)))
        (not (memq (car option) keys-to-remove)))
      options)))


;; if options contain '(catch? . flag), return such flag.
;; otherwise return #f
(define (options->catch? options)
  (let ((option (assq 'catch? options)))
    (if option
      (let ((caller 'options->catch))
        (assert* caller (boolean? (cdr option)))
        (cdr option))
      #f)))


;; if options contain '(spawn? . flag), return such flag.
;; otherwise return #f
(define (options->spawn? options)
  (let ((option (assq 'spawn? options)))
    (if option
      (let ((caller 'options->spawn))
        (assert* caller (boolean? (cdr option)))
        (cdr option))
      #f)))


;; if job control is active and options contain '(process-group-id . id), return such id.
;; otherwise return #f
(define (options->process-group-id options)
  (if (sh-job-control?)
    (let ((option (assq 'process-group-id options)))
      (if option
        (let ((caller 'options->process-group-id))
          (assert* caller (integer? (cdr option)))
          (assert* caller (>= (cdr option) 0))
          (cdr option))
        0)) ; default: create a new process group

    ; if job control is inactive, as for example in a subshell,
    ; ignore requests to move a process into a specific process group id
    ; or to create a new process group id
    #f))
