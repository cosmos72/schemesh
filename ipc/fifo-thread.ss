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
  (export make-fifo-pair
          fifo-reader fifo-reader? fifo-reader-close fifo-reader-eof? fifo-reader-get fifo-reader-skip
          fifo-writer fifo-writer? fifo-writer-close fifo-writer-eof? fifo-writer-put

          fifo-reader-timed-get fifo-reader-try-get
          fifo-writer-timed-put fifo-writer-try-put

          in-fifo-reader make-thread-fifo-reader thread==>)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)            condition-signal condition-wait fx1+ fx1- include list-copy list-head
                                  make-condition make-mutex make-time meta-cond record-writer
                                  time<=? time? time-difference! time-type time-second time-nanosecond
                                  void with-interrupts-disabled with-mutex)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k io obj)       obj-reader obj-reader? obj-reader-close obj-reader-eof? obj-reader-get obj-reader-skip
                                  obj-writer obj-writer? obj-writer-close obj-writer-eof? obj-writer-put)
    (only (scheme2k posix thread) make-thread thread-start!))


;; this implementation is multi-threaded
(define-syntax threaded?
  (syntax-rules ()
    ((_) #t)))


(include "ipc/fifo-common.ss")


(define (make-fifo-handle capacity)
  (%make-fifo-handle (make-vector capacity) 0 0 0 #f (make-mutex) (make-condition) (make-condition)))


(define (fifo-handle-close h)
  (with-mutex (fifo-handle-mutex h)
    (fifo-handle-eof?-set! h #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get elements


(define (fifo-handle-timed-get-once h timeout)
  (check-interrupts)
  (with-mutex (fifo-handle-mutex h)
    (with-interrupts-disabled
      (let ((size (fifo-handle-size h)))
        (cond
          ((fx>? size 0) ;; consume one element
            (let* ((pos   (fifo-handle-start h))
                   (vec   (fifo-handle-vec   h))
                   (cap   (vector-length vec))
                   (datum (vector-ref    vec pos)))
              ;; help the gc
              (vector-set! vec pos #f)
              (let ((pos+1 (fx1+ pos)))
                (fifo-handle-start-set! h (if (fx>=? pos+1 cap) 0 pos+1)))

              (fifo-handle-size-set! h (fx1- size))
              (when (fx>=? size cap)
                ;; notify any writer that fifo is no longer full
                (condition-signal (fifo-handle-may-put h)))
              (values datum 'ok)))

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

            (if (fxzero? (fifo-handle-size h))
              (values #f 'timeout)
              (fifo-handle-timed-get-once h 0))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put datum


;; block with timeout trying to put datum into fifo-writer, and return one value:
;;   #t if successful, or #f on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
(define (fifo-handle-timed-put-once tx datum timeout)
  (check-interrupts)
  (let ((h (fifo-writer-handle tx)))
    (with-mutex (fifo-handle-mutex h)
      (with-interrupts-disabled
        (let* ((size (fifo-handle-size h))
               (vec  (fifo-handle-vec  h))
               (cap  (vector-length vec)))
          (cond
            ((fifo-handle-eof? h)
              (raise-errorf 'fifo-writer-put "~s is already closed" tx))

            ((fx<? size cap) ;; append one element
              (let ((pos (fifo-handle-end h)))
                (vector-set! vec pos datum)
                (let ((pos+1 (fx1+ pos)))
                  (fifo-handle-end-set! h (if (fx>=? pos+1 cap) 0 pos+1)))

                (fifo-handle-size-set! h (fx1+ size))
                (when (fxzero? size)
                  ;; notify any reader that fifo is no longer empty
                  (condition-signal (fifo-handle-may-get h))))
              #t)

            ((eqv? 0 timeout)
              #f)

            (else
              ;; (condition-wait) is somewhat bugged at least on Linux:
              ;; if CTRL+C is pressed once, it does nothing.
              ;; if CTRL+C is pressed twice before it returns, leaves mutex in inconsistent state.
              (condition-wait (fifo-handle-may-put h) (fifo-handle-mutex h) timeout)

              (if (fx>=? (fifo-handle-size h) cap)
                #f
                (fifo-handle-timed-put-once tx datum 0))))))))) ; try again with zero timeout


(include "ipc/fifo-util.ss")

) ; close library
