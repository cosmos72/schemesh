;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file shell/job.ss


;; Return #t if old-status and new-status have different kind.
;; otherwise return #f
(define (status-changed? old-status new-status)
  (not (eq? (status->kind old-status)
            (status->kind new-status))))


;; Return #t if status represents a child job status
;; that causes a parent multijob to end, i.e. one of:
;; (exception ...)
;; (killed 'sigint)
;; (killed 'sigquit)
;;
(define (status-ends-multijob? status)
  (let ((kind (status->kind status)))
    (if (or (eq? kind 'exception)
            (and (eq? kind 'killed)
                 (memq (status->value status) '(sigint sigquit))))
      #t
      #f)))


;; Return #t if status represents a child job status
;; that causes a parent multijob to stop or end, i.e. one of:
;; (stopped ...)
;; (exception ...)
;; (killed 'sigint)
;; (killed 'sigquit)
;;
(define (status-stops-or-ends-multijob? status)
  (or (stopped? status)
      (status-ends-multijob? status)))


;; Convert job's last-status to one of: 'new 'running 'stopped 'ok 'failed 'exception 'killed
(define (job-last-status->kind job)
  (status->kind (job-last-status job)))



;; Return #t if job status is '(new ...), otherwise return #f
(define (job-new? job)
  (new? (job-last-status job)))

;; Return #t if job was already started, otherwise return #f
(define (job-started? job)
  (started? (job-last-status job)))

;; Return #t if job was started and is currently running (not stopped or finished), otherwise return #f
(define (job-running? job)
  (running? (job-last-status job)))

;; Return #t if job was started and is currently stopped (not running or finished), otherwise return #f
(define (job-stopped? job)
  (stopped? (job-last-status job)))

;; Return #t if job has already finished, otherwise return #f
(define (job-finished? job)
  (finished? (job-last-status job)))
