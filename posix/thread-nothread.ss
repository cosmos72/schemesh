;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


;; approximate reimplementation of Chez Scheme make-thread-parameter:
;; calls (make-parameter)
(define make-thread-parameter make-parameter)


;; acquire $tc-mutex, but don't disable interrupts
(define-syntax with-tc-mutex*
  (identifier-syntax begin))


;; disable interrupts and acquire $tc-mutex
(define-syntax with-tc-mutex
  (identifier-syntax begin))


(define (thread-count) 1)


(define (thread-find thread-id)
  (and thread-id
    (unless (fixnum? thread-id)
      (assert* 'thread-find (integer? thread-id))
      (assert* 'thread-find (exact? thread-id)))
    (and (eqv? 0 thread-id)
         (get-thread))))


;; return caller's thread
(define (get-thread)
  (car (threads)))


(define (%thread-timed-join thread timeout)
  (assert* 'thread-join (thread? thread))
  (assert* 'thread-join (time? timeout))
  (sleep (cond
           ((eq? 'time-duration (time-type timeout))
             timeout)
           (else
             (assert* 'thread-join (eq? 'time-utc (time-type timeout)))
             (time-difference timeout (current-time 'time-utc)))))
  #f)


(define long-duration (make-time 'time-duration 0 86400))


(define (%thread-join thread)
  (assert* 'thread-join (thread? thread))
  (do () (#f)
    (sleep long-duration)))


(define (fork-thread thunk)
  (raise-errorf 'fork-thread "compiled without thread support"))


(define thread-kill
  (let ((c-signal-raise (foreign-procedure "c_thread_signal_raise" (int int) int)))
    (lambda (thread signal-name)
      (assert* 'thread-kill (thread? thread))
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (let ((ret (c-signal-raise signal-number 0))) ; 0 = preserve signal handler
            (if (eqv? 0 ret) (void) ret))
          c-errno-einval)))))


(define (thread-status thread)
  (assert* 'thread-status (thread? thread))
  (running))


(define (thread-signal-handle)
  (void))
