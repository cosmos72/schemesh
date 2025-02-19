;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss



;; Status of a job is represented by a list (kind result ...)
;; Kind can be of: 'new 'running 'stopped 'ok 'failed 'killed 'exception
;; Result possible values depend on kind:
;;   if kind is 'new       -> result is 0
;;   if kind is 'running   -> result is #f or job-id
;;   if kind is 'ok        -> result ... is a list of arbitrary Scheme values
;;   if kind is 'failed    -> result is an 8-bit C exit status or an arbitrary Scheme value
;;   if kind is 'stopped   -> result is signal-name represented as a symbol
;;   if kind is 'killed    -> result is signal-name represented as a symbol
;;   if kind is 'exception -> result is a Scheme condition object
;;
;; return #t if status is one of the combinations above, otherwise return #f
(define (status-valid? status)
  (cond
    ((eq? (void) status)
      #t)
    ((and (pair? status)
          (memq (car status) '(new running stopped ok exception failed killed)))
      #t)
    (else
      #f)))


;; normalize status, converting invalid status to '(failed ...)
(define (status-normalize status)
  (cond
    ((eq? (void) status)
      status)
    ((and (pair? status)
          (eq? 'ok (car status))
          (not (null? (cdr status)))
          (null? (cddr status))
          (eq? (void) (cadr status)))
      ;; convert (cons 'ok (void)) -> (void)
      (void))
    ((status-valid? status)
      status)
    (else
      (list 'failed status))))


;; Extract the kind of a status and return it.
;; Possible returned kinds are: 'new 'running 'stopped 'ok 'exception 'failed 'killed
(define (sh-status->kind status)
  (cond
    ((eq? (void) status)     'ok)
    ((status-valid? status)  (car status))
    (else                    'failed)))


;; if (sh-finished? status) is #t, return the first result stored in status.
;; if (sh-finished? status) is #f or status contains zero results, return (void)
(define (sh-status->result status)
  (if (and (pair? status)
           (not (null? (cdr status)))
           (sh-finished? status))
    (cadr status)
    (void)))


;; if (sh-finished? status) is #t, return the list of results stored in status
;;    Note: the status (void) represents a successfully finished job that returned (void)
;;          thus the returned results is a single-element list containing (void)
;; if (sh-finished? status) is #f, return the empty list.
;;
;; DO NOT modify the returned list!
(define sh-status->results
  (let ((list1-void (list (void))))
    (lambda (status)
      (cond
        ((eq? status (void))
          list1-void)
        ((and (pair? status)
              (sh-finished? status))
          (cdr status))
        (else
          '())))))


;; return #t if status is either (void) or '(ok ...), i.e. if job finished successfully.
;; otherwise return #f
;;
;; intentionally identical to function (ok?) exported by library (schemesh posix dir)
(define (sh-ok? status)
  (cond
    ((eq? status (void))
      #t)
    ((and (pair? status)
          (eq? 'ok (car status)))
      #t)
    (else
      #f)))

;; Return #t if status represents a started job, i.e. its kind is one of 'running 'stopped.
;; otherwise return #f
(define (sh-started? status)
  (if (memq (sh-status->kind status) '(running stopped))
    #t
    #f))


;; Return #t if status represents a running job, i.e. its kind is 'running.
;; otherwise return #f
(define (sh-running? status)
  (eq? 'running (sh-status->kind status)))


;; Return #t if status represents a stopped job, i.e. its kind is 'stopped.
;; otherwise return #f
(define (sh-stopped? status)
  (eq? 'stopped (sh-status->kind status)))


;; Return #t if status represents a finished job, i.e. its kind is one of 'ok 'exception 'failed 'killed.
;; otherwise return #f
(define (sh-finished? status)
  (if (memq (sh-status->kind status) '(ok exception failed killed))
    #t
    #f))


;; Return #t if old-status and new-status have different kind.
;; otherwise return #f
(define (status-changed? old-status new-status)
  (not (eq? (sh-status->kind old-status)
            (sh-status->kind new-status))))




;; Return #t if status represents a child job status
;; that causes a parent multijob to stop or end, i.e. one of:
;; '(exception ...)
;; '(stopped ...)
;; '(killed  sigint)
;; '(killed  sigquit)
;;
(define (status-stops-or-ends-multijob? status)
  (let ((kind (sh-status->kind status)))
    (if (or (memq kind '(exception stopped))
            (and (eq? kind 'killed)
                 (not (null? (cdr status)))
                 (memq (cadr status) '(sigint sigquit))))
      #t
      #f)))


;; Return #t if status represents a child job status
;; that causes a parent multijob to end, i.e. one of:
;; '(exception ...)
;; '(killed  sigint)
;; '(killed  sigquit)
;;
(define (status-ends-multijob? status)
  (let ((kind (sh-status->kind status)))
    (if (or (eq? kind 'exception)
          (and (eq? kind 'killed)
               (not (null? (cdr status)))
               (memq (cadr status) '(sigint sigquit))))
      #t
      #f)))


;; Convert job's last-status to one of: 'new 'running 'stopped 'ok 'failed 'exception 'killed
(define (job-last-status->kind job)
  (sh-status->kind (job-last-status job)))



;; Return truish if job was already started, otherwise return #f
(define (job-started? job)
  (sh-started? (job-last-status job)))

;; Return truish if job was started and is still running (not stopped or finished), otherwise return #f
(define (job-running? job)
  (sh-running? (job-last-status job)))

;; Return truish if job has already finished, otherwise return #f
(define (job-finished? job)
  (sh-finished? (job-last-status job)))
