;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file shell/job.ss


;; Create a sh-job subclass that will run Scheme procedure proc
;; when the job is executed,
;; and return its value wrapped in a job-status.
;;
;; Procedure proc must accept zero or one arguments - the job itself.
(define (sh-expr proc)
  (assert* 'sh-expr (procedure? proc))
  (unless (logbit? 0 (procedure-arity-mask proc))
    (assert* 'sh-expr (logbit? 1 (procedure-arity-mask proc))))
  (let ((current-job (sh-current-job)))
    (%make-sh-expr
      #f #f #f #f     ; id oid pid pgid
      '(new) #f       ; last-status exception
      (span) 0 #f     ; redirections
      cmd-start #f    ; start-proc step-proc
      #f #f           ; working directory, old working directory - initially inherited from parent job
      #f              ; overridden environment variables - initially none
      #f              ; env var assignments - initially none
      (and current-job (job-parent current-job)) ; temp parent job
      (or current-job (sh-globals))              ; default parent job
      proc                                       ; procedure to call for executing the job
      #f #f)))                                   ; resume-proc suspend-proc
