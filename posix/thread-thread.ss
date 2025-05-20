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
    thread               ; thread, for speeding up (thread) function
    id                   ; thread-id, needed to check for xthread inherited from parent thread
    (mutable name)       ; thread-name, useful for debugging
    (mutable specific)   ; thread-specific, defined by SRFI 18
    (mutable pthread-id) ; pthread_t of thread, or #f if not known yet
    (mutable signal)     ; one of: 'sigint 'sigtstp 'sigcont
    changed)             ; condition
  (nongenerative xthread-7c46d04b-34f4-4046-b5c7-b63753c1be45))


(define c-pthread-kill (foreign-procedure "c_pthread_kill" (uptr int) int))

(define c-pthread-self (foreign-procedure "c_pthread_self" () uptr))

(define c-thread-signals-block-most (foreign-procedure "c_thread_signals_block_most" () int))

(define n-sigcont (signal-name->number 'sigcont))

(define s-running (running))

(define s-stopped (stopped 'sigtstp))

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
          (lambda () (mutex-release $tc-mutex) (enable-interrupts) (check-interrupts)))))))


;; return current number of threads.
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define thread-count (foreign-procedure "c_thread_count" () uptr))


;; alist ((id status . name) ...) of threads that changed status
(define status-changes '())


;; consume and return alist ((id status . name) ...) of threads that changed status
(define (threads-status-changes)
  (with-tc-mutex
    (let ((ret status-changes))
      (unless (null? ret)
        (set! status-changes '()))
      ret)))


;; must be called with with locked $tc-mutex.
(define ($threads-status-changes-insert! id status name)
  (set! status-changes (cons (cons id (cons status name)) status-changes))

  ;; wake up main thread, to let it display thread status changes
  (let* ((t  (get-initial-thread))
         (tc ($thread-tc t)))
    (unless (eqv? 0 tc)
      ; ($tc-field 'signal-interrupt-pending tc #t)
      ; ($tc-field 'something-pending tc #t)
      (let ((xthread ($thread-xthread t tc)))
        (when xthread
          (let ((pthread-id (xthread-pthread-id xthread)))
            (when pthread-id
              (c-pthread-kill pthread-id n-sigcont))))))))


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no thread with specified thread-id is found, return #f
(define (thread-find thread-id)
  (and thread-id
       (thread-id-validate thread-id)
       (with-tc-mutex
         (let %thread-find ((tl ($threads)))
           (cond
             ((null? tl)
               #f)
             ((eqv? thread-id ($thread-id (car tl)))
               (car tl))
             (else
               (%thread-find (cdr tl))))))))


;; return caller's thread.
;; must be called with locked $tc-mutex
(define ($current-thread)
  (let* ((xthread ($tc-xthread-nocreate ($tc))))
    (if xthread
      (xthread-thread xthread)
      ;; thread not found in ($tc) => scan all threads
      (let %current-thread ((tl ($threads)) (thread-id (get-thread-id)))
        (cond
          ((null? tl)
            (raise-errorf 'thread "thread not found: ~s" thread-id))
          ((eqv? thread-id ($thread-id (car tl)))
            (car tl))
          (else
            (%current-thread (cdr tl) thread-id)))))))


;; return caller's thread
(define (current-thread)
  (with-tc-mutex
     ($current-thread)))


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




(define ($new-xthread thread tc)
  (import (only (chezscheme) make-condition))
  (let* ((id   ($tc-id tc))
         (name (string->symbol (string-append "xthread-" (number->string id)))))
    ;; call pthread_self() only if xthread is for current thread
    (make-xthread thread id (void) (void)
                  (if (eqv? tc ($tc)) (c-pthread-self) #f)
                  'sigcont (make-condition name))))


(define xthread-parameter-index (($primitive $allocate-thread-parameter) #f))


;; extract and return the xthread parameter from specified thread, creating it if missing.
;; Return #f if tc is not set.
(define ($thread-xthread thread tc)
  (if (eqv? 0 tc)
    #f
    (let* ((params  ($tc-field 'parameters tc))
           (xthread (vector-ref params (car xthread-parameter-index))))
      (cond
        ((not (and xthread (eqv? ($tc-id tc) (xthread-id xthread))))
          ;; xthread is not set, or it's inherited from a parent thread: replace it
          (let ((xthread ($new-xthread thread tc)))
            (vector-set! params (car xthread-parameter-index) xthread)
            xthread))
        ((not (xthread-pthread-id xthread))
          ;; pthread-id is not known yet, update it if xthread is for current thread
          (when (eqv? tc ($tc))
            (xthread-pthread-id-set! xthread (c-pthread-self)))
          xthread)
        (else
          xthread)))))


;; extract and return the xthread parameter from specified thread-context.
;; does NOT create it if missing.
;; Return #f if tc or xthread are not set.
(define ($tc-xthread-nocreate tc)
  (if (eqv? 0 tc)
    #f
    (let* ((params  ($tc-field 'parameters tc))
           (xthread (vector-ref params (car xthread-parameter-index))))
      (if (and xthread (eqv? ($tc-id tc) (xthread-id xthread)))
        xthread
        ;; xthread is not set, or it's inherited from a parent thread: cannot return it
        #f))))


;; extract and return the name from specified thread.
;; does NOT create xthread it if missing.
;; Return (void) if tc or xthread are not set.
(define ($thread-name t)
  (let ((xthread ($tc-xthread-nocreate ($thread-tc t))))
    (if xthread
      (xthread-name xthread)
      (void))))


(define thread-register-self
  (case-lambda
    (()
      (with-tc-mutex
        ($thread-xthread ($current-thread) ($tc))))
    ((name initial-signal-name)
      (with-tc-mutex
        (let ((xthread ($thread-xthread ($current-thread) ($tc))))
          (when xthread
            (xthread-name-set!   xthread name)
            (xthread-signal-set! xthread initial-signal-name)))))))


;; hashtable thread -> (id status . name)
(define status-map (make-ephemeron-eq-hashtable))

;; return status for a thread.
;; if thread was not found, return (void) if thread exited otherwise return (running)
;;
;; must be called with locked $tc-mutex.
(define ($thread-status thread)
  (let ((pair (hashtable-ref status-map thread #f)))
    (cond
      (pair
        (cadr pair))
      ((eqv? 0 ($thread-tc thread)) ; thread exited
        (void))
      (else
        (running)))))


;; set status for a thread.
;; must be called with locked $tc-mutex.
(define ($thread-status-set! thread tc new-status)
  (let* ((old-pair     (hashtable-ref status-map thread #f))
         (thread-id    ($tc-id tc))
         (thread-name  ($thread-name thread))
         (same-status? (cond ((pair? old-pair) (eq? (cadr old-pair) new-status))
                             (else             (eq? s-running       new-status)))))
    (if old-pair
      (set-car! (cdr old-pair) new-status)
      (hashtable-set! status-map thread (cons thread-id (cons new-status thread-name))))

    (unless same-status?
      ;; queue thread status change notification
      ($threads-status-changes-insert! thread-id new-status thread-name))))


;; return name of specified thread
(define (thread-name thread-or-id)
  (let ((thread (datum->thread thread-or-id)))
    (with-tc-mutex*
      ($thread-name thread))))


;; return status of specified thread, i.e. a status object among (running) (stopped) or (void)
(define (thread-status thread-or-id)
  (let ((thread (datum->thread thread-or-id)))
    (with-tc-mutex*
      ($thread-status thread))))


(define (thread-specific thread)
  (assert* 'thread-specific (thread? thread))
  (with-tc-mutex*
    (let ((xthread ($thread-xthread thread ($thread-tc thread))))
      (if xthread
        (xthread-specific xthread)
        (void)))))


(define (thread-specific-set! thread value)
  (assert* 'thread-specific-set! (thread? thread))
  (with-tc-mutex*
    (let ((xthread ($thread-xthread thread ($thread-tc thread))))
      (when xthread
        (xthread-specific-set! xthread value)))))


;; return a fresh hashtable containing the known threads, their id, status and name
;; organized as id -> #(thread status name)
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define (threads-status)
  (let ((ret (make-eqv-hashtable)))
    (with-tc-mutex
      (for-hash ((t id.status.name status-map))
        (hashtable-set! ret (car id.status.name)
                            (vector t (cadr id.status.name) (cddr id.status.name))))
      (for-list ((t ($threads)))
        (let ((id ($thread-id t)))
          (when (and id (not (hashtable-ref ret id #f)))
            (hashtable-set! ret id (vector t ($thread-status t) ($thread-name t)))))))
    ret))


;; in newly created thread, call (param (thunk)) for each alist element (param . thunk) in (thread-initial-bindings)
(define (apply-thread-initial-bindings)
  (for-alist ((param thunk (thread-initial-bindings)))
    (param (thunk))))


;; create a new thread, establish its initial thread parameters as specified by (thread-initial-bindings)
;; then call (thunk) in the new thread.
;;
;; the thread will exit when (thunk) returns
(define (%thread-create caller thunk name initial-signal-name)
  (import (prefix (only (chezscheme) fork-thread) chez:))
  (assert* caller (procedure? thunk))
  (assert* caller (logbit? 0 (procedure-arity-mask thunk)))

  ;; register caller's thread pthread_id in case it's missing
  (thread-register-self)

  (let ((ret #f))
    (set! ret
          (chez:fork-thread
            (lambda ()
              ;; block most signals in new thread:
              ;; we need to receive them in main thread
              ;; Exception: allow receiving SIGCONT in new thread
              (c-thread-signals-block-most)
              (thread-register-self name initial-signal-name)
              (apply-thread-initial-bindings)
              (keyboard-interrupt-handler thread-signal-handle)
              (let ((status
		      ;; intercept raised conditions, the hard way.
		      ;;
		      ;; setting thread parameter (base-exception-handler)
		      ;; intercepts them only on Chez Scheme >= 10.0.0
                      (try
                        (call/cc
                          (lambda (k)
			    (let ((on-success   (lambda args (k (apply ok args))))
                                  (on-failure   (case-lambda
						  (()    (k (failed (void))))
						  ((arg) (k (failed arg))))))
                              ;; intercept calls to (abort) (exit) (reset)
                              (parameterize ((abort-handler  on-failure)
                                             (exit-handler   on-success)
                                             (reset-handler  on-failure))
				(thread-signal-handle)
				;; convert (thunk) return values to status
				(call-with-values thunk ok)))))
			(catch (ex)
			  (exception ex))))

                    ;; race condition: may be executed before set! ret above
                    (thread (or ret (current-thread))))
                (with-tc-mutex
                  ($thread-status-set! thread ($tc) status))))))
    ret))



;; send a signal to specified thread or thread-id.
;; signal-name is optional and defaults to 'sigint.
;;
;; return (void) if successfull, otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define thread-kill
  (case-lambda
    ((thread-or-id signal-name)
      (let ((thread-supported-signals '(sigint sigtstp sigcont)))
        (assert* 'thread-kill (memq signal-name thread-supported-signals)))
      (let* ((thread (datum->thread thread-or-id))
             (ret    (with-tc-mutex* ($thread-kill thread signal-name))))
        (if (xthread? ret)
          (let ()
            (import (only (chezscheme) condition-broadcast))
            (condition-broadcast (xthread-changed ret))
            (void))
          ret)))
    ((thread-or-id)
      (thread-kill thread-or-id 'sigint))))


;; send a signal to specified thread.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated xthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define ($thread-kill thread signal-name)
  (let* ((tc      ($thread-tc thread))
         (xthread ($thread-xthread thread tc)))
    (if (eqv? 0 tc)
      c-errno-esrch
      ($tc-kill tc xthread (eqv? 0 ($tc-id tc))
                (and xthread (xthread-pthread-id xthread))
                signal-name))))


;; send a signal to specified thread-context tc.
;; must be called with locked $tc-mutex.
;;
;; if successful return updated xthread object,
;; otherwise return c_errno() < 0
;;
;; NOTE: the only supported signal names are 'sigint 'sigtstp 'sigcont
(define ($tc-kill tc xthread main-thread? pthread-id signal-name)
  (when (and xthread (not main-thread?))
    ;; set xthread-signal indicating what secondary thread should do
    (xthread-signal-set! xthread signal-name))
  ;; set tc fields indicating a pending keyboard interrupt.
  ;; we cannot emulate any other POSIX signal,
  ;; because Chez Scheme ($event) checks for signals queued by C signal handlers,
  ;; and we have no simple way to enqueue them.
  (if main-thread?
    ($tc-field 'signal-interrupt-pending tc #t)
    ($tc-field 'keyboard-interrupt-pending tc #t))
  ($tc-field 'something-pending tc #t)
  (cond
    (pthread-id
      ;; send actual signal to main thread,
      ;; or SIGCONT to secondary threads, for interrupting any blocking system call
      (let* ((sig (if main-thread? (signal-name->number signal-name) n-sigcont))
             (ret (c-pthread-kill pthread-id sig)))
        (if (eqv? 0 ret)
          xthread ;; success, return xthread
          ret)))
    (xthread
      ;; thread exists, but its pthread-id is not known
      c-errno-eagain)
    (else
      c-errno-esrch)))


;; handle the signal sent to current thread by (thread-kill)
;; should be called by (keyboard-interrupt-handler) in every secondary thread.
(define (thread-signal-handle)
  ;; main thread has its own signal handling mechanism,
  ;; this is for secondary threads only
  (unless (eqv? 0 (get-thread-id))
    (with-tc-mutex
      (let* ((thread  ($current-thread))
             (tc      ($tc))
             (xthread ($thread-xthread thread tc)))
        (when xthread
          ($thread-signal-handle thread tc xthread))))))


;; implementation of (thread-signal-handle)
(define ($thread-signal-handle thread tc xthread)
  (import (only (chezscheme) condition-wait))
  (let ((signal-name (xthread-signal xthread)))
    (case signal-name
      ((sigint)
        ($thread-status-set! thread tc s-running)
        (xthread-signal-set! xthread 'sigcont) ; consume signal
        (raise-thread-interrupted 'thread-signal-handle ($tc-id tc) signal-name))
      ((sigtstp)
        ($thread-status-set! thread tc s-stopped)
        (condition-wait (xthread-changed xthread) $tc-mutex)
        ($thread-signal-handle thread tc xthread))
      (else
        ($thread-status-set! thread tc s-running)))))


(define (raise-thread-interrupted caller thread-id signal-name)
  (meta-cond
    ((threaded?)
      (raise-condition-received-signal caller signal-name "thread ~s interrupted by signal ~s"
        thread-id signal-name))))
