;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define all the thread-related bindings required by SRFI 18 multithreading

(library (srfi :18 multithreading (0 9 1))
  (export
      current-thread make-thread thread? thread-name thread-specific thread-specific-set!
      thread-join! thread-sleep! thread-start! thread-terminate! thread-yield!

      #| unimplemented
      mutex? make-mutex mutex-name mutex-specific mutex-specific-set! mutex-state mutex-lock! mutex-unlock!
      condition-variable? make-condition-variable condition-variable-name condition-variable-specific condition-variable-specific-set!
      condition-variable-signal! condition-variable-broadcast!

      current-time time? time->seconds seconds->time

      current-exception-handler with-exception-handler raise

      join-timeout-exception? abandoned-mutex-exception? terminated-thread-exception? uncaught-exception? uncaught-exception-reason
      |#
    )
  (import
    (rnrs)
    (only (chezscheme)            foreign-procedure sleep void)
    (only (schemesh bootstrap)    assert*)
    (only (schemesh posix status) ok->values status->kind status->value)
    (schemesh posix thread))


(define thread-join!
  (case-lambda
    ((thread timeout timeout-val)
      (let ((status (thread-join thread timeout)))
        (case (status->kind status)
          ((running stopped)
            timeout-val)
          ((ok)
            (ok->values status))
          ((exception)
            (raise (status->value status)))
          (else
            (status->value status)))))
    ((thread timeout)
      (thread-join! thread timeout (void)))
    ((thread)
      (thread-join! thread #f (void)))))


) ; close library
