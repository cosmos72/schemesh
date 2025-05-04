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

(define c-errno-esrch ((foreign-procedure "c_errno_esrch" () int)))

(define c-phtread-self (foreign-procedure "c_pthread_self" () uptr))

(define c-signal-setblocked! (foreign-procedure "c_signal_setblocked" (int int) int))


;; helper containing per-thread data: thread-id, pthread_t, and signal protected by a mutex and condition
(define-record-type xthread
  (fields
    id ; thread-id, needed to check for xthread inherited from parent thread
    pthread-id
    (mutable signal) ; one of: 'sigint 'sigtstp 'sigcont
    (mutable status) ; a status object
    changed)         ; condition
  (nongenerative xthread-7c46d04b-34f4-4046-b5c7-b63753c1be41))


(define (new-xthread thread-id)
  (let ((name (string->symbol (string-append "xthread-" (number->string thread-id)))))
    (import (only (chezscheme) make-condition))
    (make-xthread thread-id (c-phtread-self) 'sigcont (running) (make-condition name))))


(define xthread-parameter-index (($primitive $allocate-thread-parameter) #f))


;; extract and return the xthread parameter from specified thread-context tc,
;; or #f if tc is zero or xthread parameter is not set or is inherited from a parent thread.
(define ($tc-xthread tc)
  (if (eqv? 0 tc)
    #f
    (let ((xthread (vector-ref ($tc-field 'parameters tc) (car xthread-parameter-index))))
      ;; check that xthread is set and not inherited from a parent thread
      (if (and (xthread? xthread) (eqv? ($tc-id tc) (xthread-id xthread)))
        xthread
        #f))))


;; set the xthread parameter of specified thread-context tc.
(define ($tc-xthread-set! tc xthread-obj)
  (unless (eqv? 0 tc)
    (vector-set! ($tc-field 'parameters tc) (car xthread-parameter-index) xthread-obj)))


(define (thread-register-self)
  (with-tc-mutex
    (let* ((tc      ($tc))
           (xthread ($tc-xthread tc)))
      (unless xthread
        ($tc-xthread-set! tc (new-xthread ($tc-id tc)))))))


(define (thread-unregister-self)
  (with-tc-mutex
    ($tc-xthread-set! ($tc) #f)))


;; return status of specified thread, i.e. a status object among (running) (stopped) or (void)
(define (thread-status thread)
  (assert* 'thread-status (thread? thread))
  (with-tc-mutex
    (let* ((tc      ($thread-tc thread))
           (xthread ($tc-xthread tc)))
      (cond
        ((xthread? xthread) (xthread-status xthread))
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
      (keyboard-interrupt-handler thread-signal-handle)
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
      (if xthread
        ;; send SIGCONT to pthread, to interrupt any blocking system call
        (let ((ret (c-pthread-kill (xthread-pthread-id xthread) sigcont)))
          (if (eqv? 0 ret)
            xthread ;; success, return xthread
            ret))
        c-errno-einval))))


;; handle the signal sent to current thread by (thread-kill)
;; should be called by (keyboard-interrupt-handler) in every secondary thread.
(define (thread-signal-handle)
  (with-tc-mutex
    (let* ((tc      ($tc))
           (xthread ($tc-xthread tc)))
      (if (xthread? xthread)
        ($tc-signal-handle tc xthread)
        ;; store missing xthread into tc
        ($tc-xthread-set! tc (new-xthread ($tc-id tc)))))))


;; implementation of (thread-signal-handle)
(define ($tc-signal-handle tc xthread)
  (let ((signal (xthread-signal xthread)))
    (import (only (chezscheme) condition-wait))
    (case (xthread-signal xthread)
      ((sigint)
        (xthread-status-set! xthread (running))
        (xthread-signal-set! xthread 'sigcont) ; consume signal
        (raise-thread-interrupted 'thread-signal-handle ($tc-id tc) signal))
      ((sigtstp)
        (xthread-status-set! xthread (stopped 'sigtstp))
        (condition-wait (xthread-changed xthread) $tc-mutex)
        ($tc-signal-handle tc xthread))
      (else
        (xthread-status-set! xthread (running))))))


(define (raise-thread-interrupted caller thread-id signal-name)
  (meta-cond
    ((threaded?)
      (raise-condition-received-signal caller signal-name "thread ~s interrupted by signal ~s"
        thread-id signal-name))))
