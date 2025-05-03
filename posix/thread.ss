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
;; plus some useful functions: (get-thread) (thread-count) (thread-find) (thread-id) (threads)

(library (schemesh posix thread (0 9 0))
  (export get-initial-thread get-thread get-thread-id thread thread? threaded?
          thread-count thread-find thread-id thread-join thread-preserve-ownership!
          threads)
  (import
    (rnrs)
    (only (chezscheme)         $primitive library-exports meta-cond foreign-procedure import void)
    (prefix (only (chezscheme) thread? threaded?) chez:)
    (only (schemesh bootstrap) assert* assert-not* raise-errorf))


(define thread? chez:thread?)

(define threaded? chez:threaded?)

(define-syntax with-tc-mutex
  (meta-cond
    ((chez:threaded?)
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


;; must be called with locked $tc-mutex
(define %locked-threads (foreign-procedure "c_threads" () ptr))

;; must be called with locked $tc-mutex
(define (%locked-thread-id thread)
  (let ((tc (($primitive $thread-tc) thread)))
    (and (not (eqv? tc 0)) (($primitive $tc-field) 'threadno tc))))


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


;; wait for specified thread to exit
(define (thread-join thread)
  (assert* 'thread-join (thread? thread))
  ;; a thread cannot wait for itself
  (assert-not* 'thread-join (eq? thread (get-thread)))

  (meta-cond
    ;; Chez Scheme exports thread-join only in versions >= 10.0.0
    ;; and only in threaded builds:
    ;; check if it's actually present, rather than relying on version numbers
    ((memq 'thread-join (library-exports '(chezscheme)))
      (let ()
         (import (prefix (only (chezscheme) thread-join)
                         chez:))
         (chez:thread-join thread)))
    (else
      ;; there's no $terminated-cond we can wait for
      (raise-errorf 'thread-join "unimplemented"))))


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


;; return caller's thread-id
(define get-thread-id
  (let ()
    (import (prefix (only (chezscheme) get-thread-id)
                    chez:))
    chez:get-thread-id))


;; return caller's thread
(define get-thread
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
