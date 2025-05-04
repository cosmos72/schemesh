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


;; the global Chez Scheme mutex protecting thread creation and destruction
(define $tc-mutex ($primitive $tc-mutex))


;; acquire mutex, execute body1 body2 ... finally release mutex
(define-syntax with-mutex
  (let ()
    (import (prefix (only (chezscheme) with-mutex) chez:))
    (identifier-syntax chez:with-mutex)))


;; acquire $tc-mutex, but don't disable interrupts
(define-syntax with-tc-mutex*
  (syntax-rules ()
    ((_ body1 body2 ...)
      (with-mutex $tc-mutex
        body1 body2 ...))))


;; disable interrupts and acquire $tc-mutex
(define-syntax with-tc-mutex
  (syntax-rules ()
    ((_ body1 body2 ...)
      (let ()
        (import (only (chezscheme) enable-interrupts disable-interrupts mutex-acquire mutex-release))
        (dynamic-wind
          (lambda () (disable-interrupts) (mutex-acquire $tc-mutex))
          (lambda () body1 body2 ...)
          (lambda () (mutex-release $tc-mutex) (enable-interrupts)))))))


;; return current number of threads.
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define thread-count (foreign-procedure "c_thread_count" () uptr))


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no thread with specified thread-id is found, return #f
(define (thread-find thread-id)
  (and thread-id
    (unless (fixnum? thread-id)
      (assert* 'thread-find (integer? thread-id))
      (assert* 'thread-find (exact? thread-id)))
    (with-tc-mutex
      (let %thread-find ((tl ($threads)))
        (cond
          ((null? tl)
            #f)
          ((eqv? thread-id ($thread-id (car tl)))
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
          (let %%thread-join (($tc-cond ($primitive $terminated-cond)))
            (unless (eqv? 0 ($thread-tc thread))
              (check-interrupts)
              ;; condition-wait is not interruptible, use a short timeout
              (condition-wait $tc-cond $tc-mutex short-timeout)
              (%%thread-join $tc-cond))))))
    (else
      ;; there's no $terminated-cond we can wait for
      (until (eqv? 0 (with-tc-mutex* ($thread-tc thread)))
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
            (let %%thread-timed-join (($tc-cond ($primitive $terminated-cond))
                                      (now      (current-time 'time-utc)))
              (cond
                ((eqv? 0 ($thread-tc thread))
                  (void))
                ((time<=? deadline now)
                  #f)
                (else
                  (check-interrupts)
                  ;; condition-wait is not interruptible, use a short timeout
                  (condition-wait $tc-cond $tc-mutex
                    (let ((short-deadline (add-duration now short-timeout)))
                      (if (time<=? short-deadline deadline) short-deadline deadline)))
                  (%%thread-timed-join $tc-cond (current-time 'time-utc))))))))
      (else
        ;; there's no $terminated-cond we can wait for
        (let %%thread-timed-join ((now (current-time 'time-utc)))
          (cond
            ((eqv? 0 (with-tc-mutex* ($thread-tc thread)))
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


(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))

(define c-phtread-self (foreign-procedure "c_pthread_self" () uptr))

(define c-signal-setblocked! (foreign-procedure "c_signal_setblocked" (int int) int))


;; helper containing per-thread data: thread-id, pthread_t, and action protected by a mutex and condition
(define-record-type sthread
  (fields
    id ; thread-id, needed to check for sthread inherited from parent thread
    pthread-id
    (mutable action) ; one of: 'sigint 'sigtstp 'sigcont
    (mutable status) ; a status object
    changed)         ; condition
  (nongenerative sthread-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define (new-sthread thread-id)
  (let ((name (string->symbol (string-append "sthread-" (number->string thread-id)))))
    (import (only (chezscheme) make-condition))
    (make-sthread thread-id (c-phtread-self) 'sigcont (running) (make-condition name))))


(define sthread-parameter-index (($primitive $allocate-thread-parameter) #f))


;; extract and return the sthread parameter from specified thread-context tc,
;; or #f if tc is zero or sthread parameter is not set or is inherited from a parent thread.
(define ($tc-sthread tc)
  (if (eqv? 0 tc)
    #f
    (let ((sthread (vector-ref ($tc-field 'parameters tc) (car sthread-parameter-index))))
      (if (and (sthread? sthread) (eqv? ($tc-id tc) (sthread-id sthread)))
        sthread
        #f))))


;; set the sthread parameter of specified thread-context tc.
(define ($tc-sthread-set! tc sthread-obj)
  (unless (eqv? 0 tc)
    (vector-set! ($tc-field 'parameters tc) (car sthread-parameter-index) sthread-obj)))


(define (thread-register-self)
  (with-tc-mutex
    (let* ((tc          ($tc))
           (thread-id   ($tc-id tc))
           (old-sthread ($tc-sthread tc)))
      (unless old-sthread
        ($tc-sthread-set! tc (new-sthread thread-id))))))


(define (thread-unregister-self)
  (with-tc-mutex
    ($tc-sthread-set! ($tc) #f)))


;; return status of specified thread, i.e. a status object among (running) (stopped) or (void)
(define (thread-status thread)
  (assert* 'thread-status (thread? thread))
  (with-tc-mutex
    (let* ((tc      ($thread-tc thread))
           (sthread ($tc-sthread tc)))
      (cond
        ((sthread? sthread) (sthread-status sthread))
        ((eqv? 0 tc)        (void)) ; thread exited
        (else               (running))))))


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

  ;; register caller's thread pthread_id in case it's missing
  (thread-register-self)

  (chez:fork-thread
    (lambda ()
      ;; block SIGINT, SIGQUIT, SIGTSTP and SIGCHLD in new thread:
      ;; we need to receive them in main thread
      (c-signal-setblocked! (signal-name->number 'sigint)  1)
      (c-signal-setblocked! (signal-name->number 'sigquit) 1)
      (c-signal-setblocked! (signal-name->number 'sigtstp) 1)
      (c-signal-setblocked! (signal-name->number 'sigchld) 1)

      ;; allow receiving SIGCONT in new thread
      (c-signal-setblocked! (signal-name->number 'sigcont) 0)

      (apply-thread-initial-bindings)
      (keyboard-interrupt-handler thread-signal-apply)
      (dynamic-wind
        thread-register-self
        thunk
        thread-unregister-self))))


;; send a signal to specified thread or thread-id.
;; signal-name is optional and defaults to 'sigint.
;;
;; return (void) if successfull, otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define thread-kill
  (case-lambda
    ((thread-or-id signal-name)
      (assert* 'thread-kill (memq signal-name '(sigint sigtstp sigcont)))
      (let* ((t   (if (thread? thread-or-id) thread-or-id (thread thread-or-id)))
             (ret (with-tc-mutex* ($thread-kill t signal-name))))
        (if (sthread? ret)
          (let ()
            (import (only (chezscheme) condition-broadcast))
            (condition-broadcast (sthread-changed ret))
            (void))
          ret)))
    ((thread-or-id)
      (thread-kill thread-or-id 'sigint))))


;; send a signal to specified thread t.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated sthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define ($thread-kill t signal-name)
  (let* ((tc      ($thread-tc t))
         (sthread ($tc-sthread tc)))
    ;; check that sthread is set and not inherited from a parent thread
    (if sthread
      ($tc-kill tc sthread signal-name)
      (begin
        ;; trigger the thread's call to (thread-signal-apply)
        ;; so that it registers its own sthread object
        ($tc-field 'keyboard-interrupt-pending tc #t)
        ($tc-field 'something-pending tc #t)
        c-errno-einval))))


;; send a signal to specified thread-context tc.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated sthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define $tc-kill
  (let ((c-pthread-kill (foreign-procedure "c_pthread_kill" (uptr int) int))
        (sigcont        (signal-name->number 'sigcont)))
    (lambda (tc sthread signal-name)
      ;; set sthread-action indicating what thread should do
      (sthread-action-set! sthread signal-name)
      ;; set tc fields indicating a pending keyboard interrupt.
      ;; we cannot emulate any other POSIX signal,
      ;; because Chez Scheme ($event) checks for signals queued by C signal handlers,
      ;; and we have no simple way to enqueue them.
      ($tc-field 'keyboard-interrupt-pending tc #t)
      ($tc-field 'something-pending tc #t)
      ;; send SIGCONT to thread, to interrupt any blocking system call
      (c-pthread-kill (sthread-pthread-id sthread) sigcont)
      ;; success, return sthread
      sthread)))


;; execute the action corresponding to signal sent to current thread by (thread-kill)
;; should be called by (keyboard-interrupt-handler) in every secondary thread.
(define (thread-signal-apply)
  (with-tc-mutex
    (let* ((tc      ($tc))
           (sthread ($tc-sthread tc)))
      (if (sthread? sthread)
        ($tc-signal-apply tc sthread)
        ;; store missing sthread into tc
        ($tc-sthread-set! tc (new-sthread ($tc-id tc)))))))


;; implementation of (thread-signal-apply)
(define ($tc-signal-apply tc sthread)
  (let ((action (sthread-action sthread)))
    (import (only (chezscheme) condition-wait))
    (case (sthread-action sthread)
      ((sigint)
        (sthread-status-set! sthread (running))
        (raise-thread-interrupted 'thread-signal-apply ($tc-id tc) action))
      ((sigtstp)
        (sthread-status-set! sthread (stopped 'sigtstp))
        (condition-wait (sthread-changed sthread) $tc-mutex)
        ($tc-signal-apply tc sthread))
      (else
        (sthread-status-set! sthread (running))))))


(define (raise-thread-interrupted caller thread-id signal-name)
  (meta-cond
    ((threaded?)
      (call/cc
        (lambda (k)
          (raise
            (condition
              (make-error)
              (make-continuation-condition k)
              (make-non-continuable-violation)
              (make-who-condition caller)
              (make-format-condition)
              (make-message-condition "thread ~s interrupted by signal ~s")
              (make-irritants-condition (list thread-id signal-name)))))))))
