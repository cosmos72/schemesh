;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


(define make-thread-parameter
  (let ()
    (import (prefix (only (chezscheme) make-thread-parameter) chez:))
    chez:make-thread-parameter))


;; acquire mutex, execute body1 body2 ... finally release mutex
(define-syntax with-mutex
  (let ()
    (import (prefix (only (chezscheme) with-mutex) chez:))
    (identifier-syntax chez:with-mutex)))


;; acquire $tc-mutex, but don't disable interrupts
(define-syntax with-tc-mutex*
  (syntax-rules ()
    ((_ body1 body2 ...)
      (with-mutex ($primitive $tc-mutex)
        body1 body2 ...))))


;; disable interrupts and acquire $tc-mutex
(define-syntax with-tc-mutex
  (syntax-rules ()
    ((_ body1 body2 ...)
      (let ()
        (import (only (chezscheme) enable-interrupts disable-interrupts mutex-acquire mutex-release))
        (dynamic-wind
          (lambda () (disable-interrupts) (mutex-acquire ($primitive $tc-mutex)))
          (lambda () body1 body2 ...)
          (lambda () (mutex-release ($primitive $tc-mutex)) (enable-interrupts)))))))


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no thread with specified thread-id is found, return #f
(define (thread-find thread-id)
  (and thread-id
    (unless (fixnum? thread-id)
      (assert* 'thread-find (integer? thread-id))
      (assert* 'thread-find (exact? thread-id)))
    (with-tc-mutex
      (let %thread-find ((tl (%locked-threads)))
        (cond
          ((null? tl)
            #f)
          ((eqv? thread-id (%locked-thread-id (car tl)))
            (car tl))
          (else
            (%thread-find (cdr tl))))))))


;; return caller's thread
(define (get-thread)
  (thread (get-thread-id)))


(define short-timeout (make-time 'time-duration 500000000 0))


;; actual implementation of (thread-join) with no timeout
(define (%thread-join thread)
  (assert* 'thread-join (thread? thread))

  ;; Chez Scheme exports thread-join only in versions >= 10.0.0
  ;; and only in threaded builds, and it's not interruptible:
  ;; roll our own
  (meta-cond
    ((and (threaded?) (try (eval '($primitive $terminated-cond)) #t (catch (ex) #f)))
      (let ()
        (import (only (chezscheme) condition-wait))
        (with-tc-mutex*
          (let %%thread-join ((tc-cond  ($primitive $terminated-cond))
                              (tc-mutex ($primitive $tc-mutex)))
            (unless (eqv? 0 (%locked-thread-tc thread))
              (check-interrupts)
              ;; condition-wait is not interruptible, use a short timeout
              (condition-wait tc-cond tc-mutex short-timeout)
              (%%thread-join tc-cond tc-mutex))))))
    (else
      ;; there's no $terminated-cond we can wait for
      (until (eqv? 0 (with-tc-mutex* (%locked-thread-tc thread)))
        (check-interrupts)
        (sleep short-timeout)))) ; sleep is interruptible
  (void))


;; actual implementation of (thread-join) with timeout
(define (%thread-timed-join thread timeout)
  (assert* 'thread-join (thread? thread))
  (assert* 'thread-join (time? timeout))

  (let ((deadline (cond ((eq? 'time-utc (time-type timeout))
                          timeout)
                        (else
                          (assert* 'thread-join (eq? 'time-duration (time-type timeout)))
                          (add-duration (current-time 'time-utc) timeout)))))

    ;; Chez Scheme exports thread-join only in versions >= 10.0.0
    ;; and only in threaded builds, and it's not interruptible nor accepts timeout:
    ;; roll our own
    (meta-cond
      ((and (threaded?) (try (eval '($primitive $terminated-cond)) #t (catch (ex) #f)))
        (let ()
          (import (only (chezscheme) condition-wait))
          (with-tc-mutex*
            (let %%thread-timed-join ((tc-cond  ($primitive $terminated-cond))
                                      (tc-mutex ($primitive $tc-mutex))
                                      (now      (current-time 'time-utc)))
              (cond
                ((eqv? 0 (%locked-thread-tc thread))
                  (void))
                ((time<=? deadline now)
                  #f)
                (else
                  (check-interrupts)
                  ;; condition-wait is not interruptible, use a short timeout
                  (condition-wait tc-cond tc-mutex
                    (let ((short-deadline (add-duration now short-timeout)))
                      (if (time<=? short-deadline deadline) short-deadline deadline)))
                  (%%thread-timed-join tc-cond tc-mutex (current-time 'time-utc))))))))
      (else
        ;; there's no $terminated-cond we can wait for
        (let %%thread-timed-join ((now (current-time 'time-utc)))
          (cond
            ((eqv? 0 (with-tc-mutex* (%locked-thread-tc thread)))
              (void))
            ((time<=? deadline now)
              #f)
            (else
              (check-interrupts)
              ; sleep is interruptible
              (sleep
                (let ((max-timeout (time-difference deadline now)))
                  (if (time<=? short-timeout max-timeout) short-timeout max-timeout)))
              (%%thread-timed-join (current-time 'time-utc)))))))))


(define c-phtread-self (foreign-procedure "c_pthread_self" () uptr))


(define pthread-ids
  (let ((ht (make-eqv-hashtable)))
    (when (eqv? 0 (get-thread-id))
      ;; register main thread's pthread_id
      (hashtable-set! ht 0 (c-phtread-self)))
    ht))



(define pthread-ids-mutex
  (let ()
    (import (only (chezscheme) make-mutex))
    (make-mutex 'pthread-ids)))


(define (thread-register-pthread-id)
  (with-mutex pthread-ids-mutex
    (hashtable-set! pthread-ids (get-thread-id) (c-phtread-self))))


(define (thread-unregister-pthread-id)
  (with-mutex pthread-ids-mutex
    (hashtable-delete! pthread-ids (get-thread-id))))


;; in newly created thread, call (param (thunk)) for each alist element (param . thunk) in (thread-initial-bindings)
(define (apply-thread-initial-bindings)
  (do ((tail (thread-initial-bindings) (cdr tail)))
      ((null? tail))
    (let* ((a (car tail))
           (param (car a))
           (thunk (cdr a)))
      (param (thunk)))))



;; create a new thread, establish its initial thread parameters as specified by (thread-initial-bindings)
;; then call (thunk) in the new thread.
;;
;; the thread will exit when (thunk) returns
(define (fork-thread thunk)
  (import (prefix (only (chezscheme) fork-thread) chez:))
  (assert* 'fork-thread (procedure? thunk))
  (assert* 'fork-thread (logbit? 0 (procedure-arity-mask thunk)))

  (when (eqv? 0 (get-thread-id))
    ;; register main thread's pthread_id
    (thread-register-pthread-id))

  (chez:fork-thread
    (lambda ()
      (apply-thread-initial-bindings)
      (dynamic-wind
        thread-register-pthread-id
        thunk
        thread-unregister-pthread-id))))


;; send a POSIX signal to specified thread.
;;
;; Returns 0 if successful, or < 0 if thread or signal-name are unknown,
;; or if C function pthread_kill() fails with C errno != 0.
(define thread-kill
  (let ((c-pthread-kill (foreign-procedure "c_pthread_kill" (uptr int) int))
        (c-errno-einval ((foreign-procedure "c_errno_einval" () int))))
    (lambda (thread signal-name)
      (assert* 'thread-kill (thread? thread))
      (let ((id (thread-id thread))
            (signal-number (signal-name->number signal-name)))
        (if (and id (fixnum? signal-number))
          (with-mutex pthread-ids-mutex
            (let ((pthread-id (hashtable-ref pthread-ids id #f)))
              (if pthread-id
                (c-pthread-kill pthread-id signal-number)
                c-errno-einval)))
          c-errno-einval)))))
