;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs


;; helper containing per-thread data: thread-id, pthread_t and signal protected by global $tc-mutex and a condition
(define-record-type xthread
  (fields
    id                   ; thread-id, needed to check for xthread inherited from parent thread
    (mutable pthread-id) ; pthread_t of thread, or #f if not known yet
    (mutable signal)     ; one of: 'sigint 'sigtstp 'sigcont
    changed)             ; condition
  (nongenerative xthread-7c46d04b-34f4-4046-b5c7-b63753c1be43))


(define c-pthread-self (foreign-procedure "c_pthread_self" () uptr))

(define c-thread-signals-block-most (foreign-procedure "c_thread_signals_block_most" () int))


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


;; actual implementation of (thread-join) with no timeout.
;; returns thread exit status: a status object
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
            (cond
              ((eqv? 0 ($thread-tc thread))
                ($thread-status thread))
              (else
                (check-interrupts)
                ;; condition-wait is not interruptible, use a short timeout
                (condition-wait $tc-cond $tc-mutex short-timeout)
                (%%thread-join $tc-cond)))))))
    (else
      ;; there's no $terminated-cond we can wait for
      (until (eqv? 0 (with-tc-mutex* ($thread-tc thread)))
        (check-interrupts)
        (sleep short-timeout)) ; sleep is interruptible
      (with-tc-mutex*
        ($thread-status thread)))))


;; actual implementation of (thread-join) with timeout
;; returns thread exit status: a status object,
;; or (running) if thread is still alive when timeout expires.
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
                  ($thread-status thread))
                ((time<=? deadline now)
                  (running))
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
              (with-tc-mutex*
                ($thread-status thread)))
            ((time<=? deadline now)
              (running))
            (else
              (check-interrupts)
              ; sleep is interruptible
              (sleep
                (let ((max-timeout (time-difference deadline now)))
                  (if (time<=? short-timeout max-timeout) short-timeout max-timeout)))
              (%%thread-timed-join (current-time 'time-utc)))))))))




(define (new-xthread tc)
  (import (only (chezscheme) make-condition))
  (let* ((id ($tc-id tc))
         (name (string->symbol (string-append "xthread-" (number->string id)))))
    ;; call pthread_self() only if xthread is for current thread
    (make-xthread id (if (eqv? tc ($tc)) (c-pthread-self) #f)
                  'sigcont (make-condition name))))


(define xthread-parameter-index (($primitive $allocate-thread-parameter) #f))


;; extract and return the xthread parameter from specified thread-context tc, creating it if needed.
;; Return #f if tc is not set.
(define ($tc-xthread tc)
  (if (eqv? 0 tc)
    #f
    (let* ((params  ($tc-field 'parameters tc))
           (xthread (vector-ref params (car xthread-parameter-index))))
      (cond
        ((not (and xthread (eqv? ($tc-id tc) (xthread-id xthread))))
          ;; xthread is not set, or it's inherited from a parent thread
          (let ((xthread (new-xthread tc)))
            (vector-set! params (car xthread-parameter-index)
            xthread)))
        ((not (xthread-pthread-id xthread))
          ;; pthread-id is not known yet, update it if xthread is for current thread
          (when (eqv? tc ($tc))
            (xthread-pthread-id-set! xthread (c-pthread-self)))
          xthread)
        (else
          xthread)))))


(define (thread-register-self)
  (with-tc-mutex
    ($tc-xthread ($tc))))


;; set or return status for a thread.
;; if thread was not found, return (void) if it's exited otherwise return (running)
;;
;; must be called with locked $tc-mutex.
(define $thread-status
  (let ((ht (make-ephemeron-eq-hashtable)))
    (case-lambda
      ((thread)
        (let ((status (hashtable-ref ht thread #f)))
          (cond
            (status
              status)
            ((eqv? 0 ($thread-tc thread)) ; thread has exited
              (void))
            (else
              (running)))))
      ((thread status)
        (hashtable-set! ht thread status)))))



;; return status of specified thread, i.e. a status object among (running) (stopped) or (void)
(define (thread-status thread)
  (assert* 'thread-status (thread? thread))
  (with-tc-mutex*
    ($thread-status thread)))


;; in newly created thread, call (param (thunk)) for each alist element (param . thunk) in (thread-initial-bindings)
(define (apply-thread-initial-bindings)
  (do ((tail (thread-initial-bindings) (cdr tail)))
      ((null? tail))
    (let* ((a     (car tail))
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
      ;; block most signals in new thread:
      ;; we need to receive them in main thread
      ;; Exception: allow receiving SIGCONT in new thread
      (c-thread-signals-block-most)
      (thread-register-self)
      (apply-thread-initial-bindings)
      (keyboard-interrupt-handler thread-signal-handle)
      ;; convert (thunk) return values to status
      (let ((status
              (try
                (call-with-values thunk ok)
                (catch (ex)
                  (exception ex))))
            (thread (get-thread)))
        ;; TODO queue thread exit notification
        ;; race condition, thread may exit before (fork-thread) sets thread variable
        (with-tc-mutex
          ($thread-status thread status))))))


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
        (if (xthread? ret)
          (let ()
            (import (only (chezscheme) condition-broadcast))
            (condition-broadcast (xthread-changed ret))
            (void))
          ret)))
    ((thread-or-id)
      (thread-kill thread-or-id 'sigint))))


;; send a signal to specified thread t.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated xthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define ($thread-kill t signal-name)
  (let ((tc ($thread-tc t)))
    (if (eqv? 0 tc)
      c-errno-esrch
      ($tc-kill tc ($tc-xthread tc) signal-name))))


;; send a signal to specified thread-context tc.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated xthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define $tc-kill
  (let ((c-pthread-kill (foreign-procedure "c_pthread_kill" (uptr int) int))
        (sigcont        (signal-name->number 'sigcont)))
    (lambda (tc xthread signal-name)
      (when xthread
        ;; set xthread-signal indicating what thread should do
        (xthread-signal-set! xthread signal-name))
      ;; set tc fields indicating a pending keyboard interrupt.
      ;; we cannot emulate any other POSIX signal,
      ;; because Chez Scheme ($event) checks for signals queued by C signal handlers,
      ;; and we have no simple way to enqueue them.
      ($tc-field 'keyboard-interrupt-pending tc #t)
      ($tc-field 'something-pending tc #t)
      (let ((pthread-id (and xthread (xthread-pthread-id xthread))))
        (cond
          (pthread-id
            ;; send SIGCONT to pthread, to interrupt any blocking system call
            (let ((ret (c-pthread-kill pthread-id sigcont)))
              (if (eqv? 0 ret)
                xthread ;; success, return xthread
                ret)))
          (xthread
            ;; thread exists, but its pthread-id is not known
            c-errno-eagain)
          (else
            c-errno-esrch))))))


;; handle the signal sent to current thread by (thread-kill)
;; should be called by (keyboard-interrupt-handler) in every secondary thread.
(define (thread-signal-handle)
  (let ((thread (get-thread)))
    (with-tc-mutex
      (let* ((tc      ($tc))
             (xthread ($tc-xthread tc)))
        (when xthread
          ($tc-signal-handle thread tc xthread))))))


;; implementation of (thread-signal-handle)
(define ($tc-signal-handle thread tc xthread)
  (import (only (chezscheme) condition-wait))
  (let ((signal-name (xthread-signal xthread)))
    (case signal-name
      ((sigint)
        ($thread-status thread (running))
        (xthread-signal-set! xthread 'sigcont) ; consume signal
        (raise-thread-interrupted 'thread-signal-handle ($tc-id tc) signal-name))
      ((sigtstp)
        ($thread-status thread (stopped 'sigtstp))
        (condition-wait (xthread-changed xthread) $tc-mutex)
        ($tc-signal-handle thread tc xthread))
      (else
        ($thread-status thread (running))))))


(define (raise-thread-interrupted caller thread-id signal-name)
  (meta-cond
    ((threaded?)
      (raise-condition-received-signal caller signal-name "thread ~s interrupted by signal ~s"
        thread-id signal-name))))
