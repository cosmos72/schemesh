;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; define all the thread-related bindings present in Chez Scheme 10.0.0 with threads
;; also on older versions and also on non-threaded builds,
;;
;; plus improved Chez Scheme functions:
;;  (fork-thread) also sets the new thread's thread-local parameters to values returned by (thread-initial-bindings)
;;  (thread-join) also accepts an optional timeout and is interruptible
;;
;; plus a subset of SRFI 18 multithreading functions:
;;  (current-thread) (make-thread) (thread-name) (thread-specific) (thread-specific-set!)
;;  (thread-sleep!) (thread-start!) (thread-terminate!) (thread-yield!)
;;
;; plus some useful extra functions
;;  (thread) (thread-alive?) (thread-count) (thread-find) (thread-id) (thread-initial-bindings)
;;  (thread-stop!) (threads) (threads-status) (threads-status-changes)


(library (scheme2k posix thread (0 9 2))
  (export
      current-thread fork-thread get-initial-thread get-thread-id make-thread
      thread thread? threaded? thread-alive? thread-count thread-find thread-id thread-initial-bindings
      thread-join thread-name thread-preserve-ownership! thread-signal-handle
      thread-specific thread-specific-set! thread-status
      thread-sleep! thread-start! thread-stop! thread-terminate! thread-yield!
      threads threads-status threads-status-changes)
  (import
    (rnrs)
    (only (rnrs mutable-pairs)    set-car!)
    (only (chezscheme)            $primitive abort-handler add-duration base-exception-handler current-time eval exit-handler
                                  foreign-procedure get-thread-id import include keyboard-interrupt-handler library-exports
                                  logbit? meta-cond make-ephemeron-eq-hashtable make-parameter make-time
                                  parameterize procedure-arity-mask reset-handler sleep thread? threaded?
                                  time? time<=? time-difference time-type void with-interrupts-disabled)
    (only (scheme2k bootstrap)    assert* assert-not* catch check-interrupts raise-errorf until try)
    (only (scheme2k containers hashtable) for-hash)
    (only (scheme2k containers list)      for-alist for-list)
    (only (scheme2k posix signal) raise-condition-received-signal signal-name->number signal-raise)
    (only (scheme2k posix status) running stopped ok exception failed))


(define c-errno-eagain ((foreign-procedure "c_errno_eagain" () int)))
(define c-errno-einval ((foreign-procedure "c_errno_einval" () int)))
(define c-errno-esrch  ((foreign-procedure "c_errno_esrch"  () int)))


;; return list of existing threads.
;; must be called with locked $tc-mutex
(define $threads (foreign-procedure "c_threads" () ptr))


;; return thread-context i.e. tc of specified thread
;; must be called with locked $tc-mutex
(define $thread-tc ($primitive $thread-tc))


;; return current thread-context i.e. tc of current thread
;; must be called with locked $tc-mutex
(define $tc ($primitive $tc))


;; procedure to access thread-context fields by name
(define $tc-field ($primitive $tc-field))


;; return thread-id of specified thread-context tc, or #f if tc is not set.
;; must be called with locked $tc-mutex
(define ($tc-id tc)
  (if (eqv? tc 0) #f ($tc-field 'threadno tc)))


;; return thread-id of specified thread, or #f if thread is destroyed.
;; must be called with locked $tc-mutex
(define ($thread-id thread)
  ($tc-id ($thread-tc thread)))


(define (thread-id-validate thread-id)
  (unless (fixnum? thread-id)
    (unless (and (integer? thread-id) (exact? thread-id))
      (raise-errorf 'thread-find "~s is not an exact integer thread-id" thread-id)))
  #t)


(meta-cond
  ((threaded?)
    (include "posix/thread-thread.ss"))
  (else
    (include "posix/thread-nothread.ss")))


;; return a copy of the current threads list.
;;
;; Note: threads may be created or destroyed after this call and before
;; the returned value is used.
(define (threads)
  (with-tc-mutex
    ;; copy and reverse the list returned by ($threads)
    (let %threads ((ret '()) (tl ($threads)))
      (if (null? tl)
        ret
        (%threads (cons (car tl) ret) (cdr tl))))))


;; return thread-id of specified thread, or #f if thread is destroyed
(define (thread-id thread)
  (assert* 'thread-id (thread? thread))
  (with-tc-mutex
    ($thread-id thread)))


;; return #t if thread is running, or #f if it's destroyed
(define (thread-alive? thread)
  (if (thread-id thread) #t #f))


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no such thread is found, raise exception
(define (thread thread-id)
  (let ((t (thread-find thread-id)))
    (unless t
      (raise-errorf 'thread "thread not found: ~s" thread-id))
    t))


(define (datum->thread thread-or-id)
  (if (thread? thread-or-id)
    thread-or-id
    (thread thread-or-id)))


;; Create a new thread and establish its initial thread parameters as specified by (thread-initial-bindings).
;; Return the created thread.
;;
;; The new thread starts immediately: it calls (thunk) and will exit when (thunk) returns.
(define (fork-thread thunk)
  (%thread-create 'fork-thread thunk (void) 'sigcont))


;; Create a new thread and establish its initial thread parameters as specified by (thread-initial-bindings).
;; Return the created thread.
;;
;; The new thread is initially in stopped status, and must be unblocked by calling (thread-start! t) on it.
;; Only then it will call (thunk) and will exit when (thunk) returns.
(define make-thread
  (case-lambda
    ((thunk name)
      (%thread-create 'make-thread thunk name 'sigtstp))
    ((thunk)
      (%thread-create 'make-thread thunk (void) 'sigtstp))))


;; wait for specified thread or thread-id to exit.
;; timeout is optional: it defaults to #f, and must be #f or a time object with type 'time-utc or 'time-duration
;;
;; if timeout is not specified or is #f, or thread exits before timeout,
;;   returns thread exit status: a status object
;; if timeout is specified and not #f, and thread is still alive after timeout,
;;   returns thread current status: (running) or (stopped ...)
(define thread-join
  (case-lambda
    ((thread-or-id)
      (%thread-join (datum->thread thread-or-id)))
    ((thread-or-id timeout)
      (let ((thread (datum->thread thread-or-id)))
        (if timeout
          (%thread-timed-join thread timeout)
          (%thread-join thread))))))


;; return main thread
(define get-initial-thread
  (meta-cond
    ;; Chez Scheme exports get-initial-thread only in versions >= 10.0.0
    ;; and only in threaded builds:
    ;; check if it's actually present, rather than relying on version numbers
    ((memq 'get-initial-thread (library-exports '(chezscheme)))
      (let ()
        (import (prefix (only (chezscheme) get-initial-thread) chez:))
        chez:get-initial-thread))
    (else
      (lambda ()
        (with-tc-mutex
          (car ($threads)))))))


(define thread-preserve-ownership!
  (meta-cond
    ;; Chez Scheme exports thread-preserve-ownership! only in versions >= 10.0.0
    ;; and only in threaded builds:
    ;; check if it's actually present, rather than relying on version numbers
    ((memq 'thread-preserve-ownership! (library-exports '(chezscheme)))
      (let ()
         (import (prefix (only (chezscheme) thread-preserve-ownership!) chez:))
         chez:thread-preserve-ownership!))
    (else
      (case-lambda
        (()
          (void))
        ((thread)
          (assert* 'thread-preserve-ownership! (thread? thread)))))))


;; thread parameter containing an alist ((param . thunk) ...) where each param and thunk are procedures.
;;
;; each time (fork-thread thunk) or (make-thread thunk [name]) are invoked,
;; the new thread will iterate on such alist and call (param (thunk)) on each element,
;; allowing for example to establish initial values for other thread parameters.
;;
;; this is similar in spirit to *default-special-bindings* provided by Common Lisp library Bordeaux Threads.
;;
;; NOTE: callers are supposed to prefix pairs into the alist, for example with
;;   (thread-initial-bindings (cons (cons my-param my-thunk) (thread-initial-bindings)))
;; while *removing* or *modifying* alist elements is almost always a bug
(define thread-initial-bindings
  (make-thread-parameter
    '()
    (lambda (alist)
      (do ((tail alist (cdr tail)))
          ((null? tail) alist)
        (assert* 'thread-initial-bindings (pair? tail))
        (let ((a (car tail)))
          (assert* 'thread-initial-bindings (pair? a))
          (assert* 'thread-initial-bindings (procedure? (car a)))
          (assert* 'thread-initial-bindings (procedure? (cdr a)))
          (assert* 'thread-initial-bindings (logbit? 1 (procedure-arity-mask (car a))))
          (assert* 'thread-initial-bindings (logbit? 0 (procedure-arity-mask (cdr a)))))))))


(define (thread-stop! thread)
  (assert* 'thread-stop! (thread? thread))
  (thread-kill thread 'sigtstp))


;; SRFI 18 API
(define thread-sleep! sleep)


(define (thread-start! thread)
  (assert* 'thread-start! (thread? thread))
  (thread-kill thread 'sigcont))


(define (thread-terminate! thread)
  (assert* 'thread-terminate! (thread? thread))
  (thread-kill thread 'sigint))


(define thread-yield! (foreign-procedure "c_sched_yield" () void))

(meta-cond
  ((threaded?)
    (thread-register-self)))

) ; close library
