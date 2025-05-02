;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh posix thread (0 9 0))
  (export get-initial-thread thread thread-count thread-find thread-id thread-list)
  (import
    (rnrs)
    (only (chezscheme)         $primitive meta-cond foreign-procedure import thread? threaded?)
    (only (schemesh bootstrap) assert* raise-errorf))


(define thread-count
  (meta-cond ((threaded?) (foreign-procedure __collect_safe "c_thread_count" () uptr))
             (else        (lambda () 1))))

(define thread-list (foreign-procedure "c_thread_list" () ptr))

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


(define (%locked-thread-id thread)
  (let ((tc (($primitive $thread-tc) thread)))
    (and (not (eq? tc 0)) (($primitive $tc-field) 'threadno tc))))


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
    (let ((tl (thread-list)))
      (with-tc-mutex
        (let %thread-find ((tl tl))
          (cond
            ((null? tl)
              #f)
            ((eqv? thread-id (%locked-thread-id (car tl)))
              (car tl))
            (else
              (%thread-find (cdr tl)))))))))


;; find and return a thread given its thread-id, which must be #f or an exact integer.
;;
;; if no thread with specified thread-id is found, raise exception
(define (thread thread-id)
  (let ((t (thread-find thread-id)))
    (unless t
      (raise-errorf 'thread "thread not found: ~s" thread-id))
    t))


(define get-initial-thread
  (meta-cond
    ((threaded?)
      (let ()
         (import (prefix (only (chezscheme) get-initial-thread)
                         chez:))
         chez:get-initial-thread))
    (else
      (let ((ret (car (thread-list))))
        (lambda () ret)))))


) ; close library
