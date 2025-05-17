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
  (identifier-syntax with-interrupts-disabled))


(define (thread-count) 1)


;; return alist (id status . name) of threads that changed status
(define (threads-status-changes)
  '())


(define (thread-find thread-id)
  (and thread-id
       (thread-id-validate thread-id)
       (eqv? 0 thread-id)
       (current-thread)))


;; return caller's thread
(define (current-thread)
  (with-tc-mutex
    (car ($threads))))


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


(define (%thread-create caller thunk name initial-signal-name)
  (raise-errorf caller "compiled without thread support"))


(define (thread-name thread)
  (assert* 'thread-status (thread? thread))
  (void))


(define thread-specific-value (void))


(define (thread-specific thread)
  (assert* 'thread-specific (thread? thread))
  thread-specific-value)


(define (thread-specific-set! thread value)
  (assert* 'thread-specific-set! (thread? thread))
  (set! thread-specific-value value))


(define thread-kill
  (let ((c-signal-raise (foreign-procedure "c_thread_signal_raise" (int int) int)))
    (lambda (thread-or-id signal-name)
      (datum->thread thread-or-id) ; validate thread-or-id
      (let ((signal-number (signal-name->number signal-name)))
        (if (fixnum? signal-number)
          (let ((ret (c-signal-raise signal-number 0))) ; 0 = preserve signal handler
            (if (eqv? 0 ret) (void) ret))
          c-errno-einval)))))


(define (thread-status thread)
  (assert* 'thread-status (thread? thread))
  (running))


;; return a fresh hashtable containing the known threads, their id and status
;; organized as id -> #(thread status name)
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define (threads-status)
  (let ((ret (make-eqv-hashtable)))
    (hashtable-set! ret 0 (vector (get-initial-thread) (running) (void)))
    ret))


(define (thread-signal-handle)
  (void))
