;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;;; inter-thread communication library:
;;;
;;; exchanges arbitrary Scheme data through thread-safe in-memory queues
;;;
(library (scheme2k ipc fifo (0 9 3))
  (export make-fifo-pair fifo-default-capacity
          fifo-reader fifo-reader? fifo-reader-close fifo-reader-eof? fifo-reader-get fifo-reader-skip
          fifo-writer fifo-writer? fifo-writer-close fifo-writer-eof? fifo-writer-put

          fifo-reader-timed-get fifo-reader-try-get
          fifo-writer-timed-put fifo-writer-try-put

          in-fifo-reader make-thread-fifo-reader thread==>)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            condition-broadcast condition-signal condition-wait fx1+ fx1- include
                                  list-copy list-head make-condition make-mutex make-time meta-cond record-writer
                                  time<=? time? time-difference! time-type time-second time-nanosecond
                                  void with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k io obj)       obj-reader obj-reader? obj-reader-close obj-reader-eof? obj-reader-get obj-reader-skip
                                  obj-writer obj-writer? obj-writer-close obj-writer-eof? obj-writer-put)
    (only (scheme2k posix thread) fork-thread))


;; this implementation is multi-threaded
(define-syntax threaded?
  (syntax-rules ()
    ((_) #t)))


(include "ipc/fifo-common.ss")


(define (make-fifo-handle capacity)
  (%make-fifo-handle (make-vector capacity) 0 0 0 #f (make-mutex) (make-condition) (make-condition)))


(define (fifo-handle-close h)
  (with-mutex (fifo-handle-mutex h)
    (fifo-handle-eof?-set! h #t))
  (condition-broadcast (fifo-handle-may-get h))
  (condition-broadcast (fifo-handle-may-put h)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get elements


;; block with timeout trying to get one datum from fifo-reader, and return two values:
;;   either (values datum 'ok) if successful
;;   or (values #<unspecified> 'timeout) on timeout
;;   or (values #<unspecified> 'eof) on eof
;;
;; timeout must be one of:
;; * fixnum zero
;; * a time object with type 'time-duration
(define (fifo-handle-timed-get-once-locked h timeout)
  (let ((size (fifo-handle-size h)))
    (cond
      ((fx>? size 0)
        (fifo-handle-pop-left h size))
      ((fifo-handle-eof? h)
        ;; no elements currently available, and fifo-writer is closed => we reached eof
        (values #f 'eof))
      ((eqv? 0 timeout)
        (values #f 'timeout))
      (else
        ;; (condition-wait) is somewhat bugged at least on Linux:
        ;; if CTRL+C is pressed once, it does nothing.
        ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
        (condition-wait (fifo-handle-may-get h) (fifo-handle-mutex h) timeout)
        (let ((size (fifo-handle-size h)))
          (if (fxzero? size)
            (values #f 'timeout)
            (fifo-handle-pop-left h size)))))))


(define (fifo-handle-timed-get-once h timeout)
  (check-interrupts)
  (let-values (((datum flag) (with-mutex (fifo-handle-mutex h)
                               (with-interrupts-disabled
                                 (fifo-handle-timed-get-once-locked h timeout)))))
    (when (eq? 'ok flag)
      ;; unblock one writer
      (condition-signal (fifo-handle-may-put h)))
    (values datum flag)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put datum


(define (fifo-handle-timed-put-once-locked tx datum timeout)
  (let* ((h    (fifo-writer-handle tx))
         (vec  (fifo-handle-vec  h))
         (size (fifo-handle-size h))
         (cap  (vector-length vec)))
    (cond
      ((fifo-handle-eof? h)
        (raise-errorf 'fifo-writer-put "~s is already closed" tx))
      ((fx<? size cap)
        (fifo-handle-push-right h datum vec size cap))
      ((eqv? 0 timeout)
        #f)
      (else
        ;; (condition-wait) is somewhat bugged at least on Linux:
        ;; if CTRL+C is pressed once, it does nothing.
        ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
        (condition-wait (fifo-handle-may-put h) (fifo-handle-mutex h) timeout)
        (let ((size (fifo-handle-size h)))
          (and (fx<? size cap)
               (fifo-handle-push-right h datum vec size cap)))))))


;; block with timeout trying to put datum into fifo-writer, and return one value:
;;   #t if successful, or #f on timeout
;;
;; timeout must be one of:
;; * fixnum zero
;; * a time object with type 'time-duration
(define (fifo-handle-timed-put-once tx datum timeout)
  (check-interrupts)
  (let* ((h   (fifo-writer-handle tx))
         (ok? (with-mutex (fifo-handle-mutex h)
               (with-interrupts-disabled
                 (fifo-handle-timed-put-once-locked tx datum timeout)))))
    (when ok?
      ;; unblock one reader
      (condition-signal (fifo-handle-may-get h)))
    ok?))


(include "ipc/fifo-util.ss")

) ; close library
