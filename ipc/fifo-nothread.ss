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
    (only (chezscheme)            fx1+ fx1- include list-copy list-head make-time meta-cond record-writer
                                  time<=? time? time-difference! time-type time-second time-nanosecond
                                  void with-interrupts-disabled)
    (only (scheme2k bootstrap)    assert* check-interrupts raise-errorf)
    (only (scheme2k posix signal) countdown)
    (only (scheme2k io obj)       obj-reader obj-reader? obj-reader-close obj-reader-eof? obj-reader-get obj-reader-skip
                                  obj-writer obj-writer? obj-writer-close obj-writer-eof? obj-writer-put))


;; this implementation is single-threaded
(define-syntax threaded?
  (syntax-rules ()
    ((_) #f)))


(include "ipc/fifo-common.ss")


(define (make-fifo-handle capacity)
  (%make-fifo-handle (make-vector capacity) 0 0 0 #f #f #f #f))


(define (fifo-handle-close h)
  (fifo-handle-eof?-set! h #t))


(define huge-timeout (* 86400 365))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get elements


(define (fifo-handle-timed-get-once h timeout)
  (check-interrupts)
  (cond
    ((fifo-handle-eof? h)
      ;; this fifo-handle is already closed
      (values #f 'eof))
    ((fxzero? (fifo-handle-size h))
      ;; this fifo-handle is empty, wait
      (if (eqv? 0 timeout)
        (values #f 'timeout)
        (begin
          (countdown timeout)
          (fifo-handle-timed-get-once h 0))))
    (else
      ;; consume one element.
      ;; disable interrupts is useful against user getting/putting elements from (break) handler
      (with-interrupts-disabled
        (fifo-handle-pop-left h (fifo-handle-size h))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put elements


;; block with timeout trying to put datum into fifo-writer, and return one value:
;;   #t if successful, or #f on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
(define (fifo-handle-timed-put-once tx datum timeout)
  (check-interrupts)
  (let* ((h (fifo-writer-handle tx))
         (vec  (fifo-handle-vec  h))
         (size (fifo-handle-size h))
         (cap  (vector-length vec)))
   (cond
     ((fifo-handle-eof? h)
       (raise-errorf 'fifo-writer-put "~s is already closed" tx))

     ((fx<? size cap)
       ;; append one element
       ;; disable interrupts is useful against user getting/putting elements from (break) handler
       (with-interrupts-disabled
         (fifo-handle-push-right h datum vec size cap)))

     ((eqv? 0 timeout)
       #f)

     (else ;; this fifo-handle is full, wait then retry with zero timeout
      (countdown timeout)
      (fifo-handle-timed-put-once tx datum 0)))))


(include "ipc/fifo-util.ss")

) ; close library
