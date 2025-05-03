;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; define all the bindings present in Chez Scheme 10.0.0 with threads
;; also on older versions and also on non-threaded builds,
;;
;; plus improved function (thread-join) that now accepts optional timeout
;; plus some useful functions (get-thread) (thread-count) (thread-find) (thread-id) (threads)

(library (schemesh posix thread (0 9 0))
  (export get-initial-thread get-thread get-thread-id thread thread? threaded?
          thread-count thread-find thread-id thread-join thread-preserve-ownership!
          threads)
  (import
    (rnrs)
    (only (chezscheme)          $primitive add-duration current-time eval foreign-procedure get-thread-id
                                import library-exports make-time meta-cond sleep thread? threaded?
                                time? time<=? time-difference time-type void)
    (only (schemesh bootstrap)  assert* assert-not* catch check-interrupts raise-errorf until try))


;; disable interrupts and acquire $tc-mutex
(define-syntax with-tc-mutex
  (meta-cond
    ((threaded?)
      (syntax-rules ()
        ((_ body1 body2 ...)
          (let ()
            (import (only (chezscheme) enable-interrupts disable-interrupts mutex-acquire mutex-release))
            (dynamic-wind
              (lambda () (disable-interrupts) (mutex-acquire ($primitive $tc-mutex)))
              (lambda () body1 body2 ...)
              (lambda () (mutex-release ($primitive $tc-mutex)) (enable-interrupts)))))))
    (else
      (let ()
        (import (only (chezscheme) with-interrupts-disabled))
        (identifier-syntax with-interrupts-disabled)))))


;; acquire $tc-mutex, but don't disable interrupts
(define-syntax with-tc-mutex*
  (meta-cond
    ((threaded?)
      (syntax-rules ()
        ((_ body1 body2 ...)
          (let ()
            (import (only (chezscheme) mutex-acquire mutex-release))
            (dynamic-wind
              (lambda () (mutex-acquire ($primitive $tc-mutex)))
              (lambda () body1 body2 ...)
              (lambda () (mutex-release ($primitive $tc-mutex))))))))
    (else
      (identifier-syntax begin))))


;; must be called with locked $tc-mutex
(define %locked-thread-tc ($primitive $thread-tc))

;; must be called with locked $tc-mutex
(define %locked-threads (foreign-procedure "c_threads" () ptr))

;; must be called with locked $tc-mutex
(define (%locked-thread-id thread)
  (let ((tc (%locked-thread-tc thread)))
    (if (eqv? tc 0) #f (($primitive $tc-field) 'threadno tc))))


;; return current number of threads.
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define thread-count (foreign-procedure "c_thread_count" () uptr))


;; return a copy of the current threads list.
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define (threads)
  (with-tc-mutex
    ;; copy and reverse the list returned by (%locked-threads)
    (let %threads ((ret '()) (tl (%locked-threads)))
      (if (null? tl)
        ret
        (%threads (cons (car tl) ret) (cdr tl))))))


;; return thread-id of specified thread, or #f if thread is destroyed
(define (thread-id thread)
  (assert* 'thread-id (thread? thread))
  (with-tc-mutex
    (%locked-thread-id thread)))


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


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no such thread is found, raise exception
(define (thread thread-id)
  (let ((t (thread-find thread-id)))
    (unless t
      (raise-errorf 'thread "thread not found: ~s" thread-id))
    t))


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
                (if deadline
                  (let ((max-timeout (time-difference deadline now)))
                    (if (time<=? short-timeout max-timeout) short-timeout max-timeout))
                  short-timeout))
              (%%thread-timed-join (current-time 'time-utc)))))))))


;; wait for specified thread to exit.
;; timeout is optional: it defaults to #f, and must be #f or a time object with type 'time-utc or 'time-duration
;;
;; if timeout is not specified or is #f, or thread exits before timeout, returns (void).
;; if timeout is specified and not #f, and thread is still alive after timeout, returns #f
(define thread-join
  (case-lambda
    ((thread)
      (%thread-join thread))
    ((thread timeout)
      (if timeout
        (%thread-timed-join thread timeout)
        (%thread-join thread)))))


;; return main thread
(define get-initial-thread
  (meta-cond
    ;; Chez Scheme exports get-initial-thread only in versions >= 10.0.0
    ;; and only in threaded builds:
    ;; check if it's actually present, rather than relying on version numbers
    ((memq 'get-initial-thread (library-exports '(chezscheme)))
      (let ()
         (import (prefix (only (chezscheme) get-initial-thread)
                         chez:))
         chez:get-initial-thread))
    (else
      (with-tc-mutex
        (car (%locked-threads))))))



;; return caller's thread
(define (get-thread)
  (thread (get-thread-id)))


(define thread-preserve-ownership!
  (meta-cond
    ;; Chez Scheme exports thread-preserve-ownership! only in versions >= 10.0.0
    ;; and only in threaded builds:
    ;; check if it's actually present, rather than relying on version numbers
    ((memq 'thread-preserve-ownership! (library-exports '(chezscheme)))
      (let ()
         (import (prefix (only (chezscheme) thread-preserve-ownership!)
                         chez:))
         chez:thread-preserve-ownership!))
    (else
      (case-lambda
        (()
          (void))
        ((thread)
          (assert* 'thread-preserve-ownership! (thread? thread)))))))


) ; close library
