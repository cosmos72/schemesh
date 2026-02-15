;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/fifo-thread.ss or ipc/fifo-nothread.ss


;; create a new thread that calls (obj-reader-get reader) in a loop
;; and writes each generated element to an internal fifo-writer.
;;
;; Optional argument capacity specifies the fifo buffer size.
;; It must be a fixnum > 0 and defaults to (fifo-default-capacity)
;;
;; return a fifo-reader connected to the internal fifo-writer.
(define make-thread-fifo-reader
  (case-lambda
    ((reader capacity)
      (assert* 'make-thread-fifo-reader (obj-reader? reader))
      (meta-cond
        ((threaded?)
          (let-values (((rx tx) (make-fifo-pair capacity)))

            (letrec* ((%thread-fifo-reader-loop
                        (lambda ()
                          (let-values (((datum ok?) (obj-reader-get reader)))
                            (when ok?
                              (obj-writer-put tx datum)
                              (%thread-fifo-reader-loop)))))

                      (%thread-fifo-reader
                        (lambda ()
                          (dynamic-wind
                            void
                            %thread-fifo-reader-loop
                            (lambda () (obj-writer-close tx)))))

                      (thread (make-thread %thread-fifo-reader 'thread-fifo-reader)))

              (thread-start! thread)
              rx)))
        (else
      reader)))

    ((reader)
      (make-thread-fifo-reader (fifo-default-capacity)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get elements


(define short-timeout (make-time 'time-duration 500000000 0))
(define zero-timeout  (make-time 'time-duration 0 0))


(define (fifo-handle-get h)
  (let-values (((datum flag) (fifo-handle-timed-get-once h short-timeout)))
    (if (eq? flag 'timeout)
      (fifo-handle-get h) ;; timeout, retry
      (values datum (eq? flag 'ok)))))


;; block with timeout until a datum is received from fifo-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if fifo-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-reader-get) (fifo-reader-timed-get) (fifo-reader-try-get) (fifo-reader-skip) and (fifo-reader-close)
;; on the same or different fifo-readers.
(define (fifo-reader-timed-get rx timeout)
  (assert* 'fifo-reader-timed-get (fifo-reader? rx))
  (let ((h (fifo-reader-handle rx))
        (timeout (to-duration timeout)))
    (cond
      ((fifo-handle-eof? h)
        (values #f 'eof))
      ((time<=? timeout zero-timeout)
        (fifo-handle-timed-get-once h 0))
      (else
        (let %fifo-handle-timed-get ((h h) (timeout timeout))
          (let ((tiny-timeout? (time<=? timeout short-timeout)))
            (let-values (((datum flag) (fifo-handle-timed-get-once h
                                         (if tiny-timeout? timeout short-timeout))))
              (if (and (eq? flag 'timeout) (not tiny-timeout?))
                (%fifo-handle-timed-get h (time-difference! timeout short-timeout))
                (values datum flag)))))))))


;; non-blockingly try to receive a datum from fifo-writer, and return two values:
;;   received datum and 'ok
;;   or <unspecified> and 'eof if fifo-writer has been closed and all data has been received
;;   or <unspecified> and 'timeout on timeout
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-reader-get) (fifo-reader-timed-get) (fifo-reader-try-get) (fifo-reader-skip) and (fifo-reader-close)
;; on the same or different fifo-readers.
(define (fifo-reader-try-get rx)
  (assert* 'fifo-reader-try-get (fifo-reader? rx))
  (let ((h (fifo-reader-handle rx)))
    (if (fifo-handle-eof? h)
      (values #f 'eof)
      (fifo-handle-timed-get-once h 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put elements



;; called by (fifo-writer-put) and (obj-writer-put)
;;
;; put datum into fifo-writer, blocking if it's full. return unspecified value.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-writer-put) (fifo-writer-timed-put) (fifo-writer-try-put) and (fifo-writer-close)
;; on the same or different fifo-writers.
(define (fifo-handle-put tx datum)
  (let ((ok? (fifo-handle-timed-put-once tx datum short-timeout)))
    (unless ok?
      (fifo-handle-put tx datum)))) ;; timeout, retry


;; block with timeout trying to put datum into fifo-writer, and return one value:
;;   #t if successful, or #f on timeout
;;
;; timeout must be one of:
;; * an exact or inexact real, indicating the number of seconds (non-integer values are supported too)
;; * a pair (seconds . nanoseconds) where both are exact integers
;; * a time object with type 'time-duration
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-writer-put) (fifo-writer-timed-put) (fifo-writer-try-put) and (fifo-writer-close)
;; on the same or different fifo-writers.
(define (fifo-writer-timed-put tx datum timeout)
  (assert* 'fifo-writer-timed-put (fifo-writer? tx))
  (let ((timeout (to-duration timeout)))
    (cond
      ((fifo-handle-eof? (fifo-writer-handle tx))
        (raise-errorf 'fifo-writer-timed-put "~s is already closed" tx))
      ((time<=? timeout zero-timeout)
        (fifo-handle-timed-put-once tx datum 0))
      (else
        (let %fifo-handle-timed-put ((tx tx) (datum datum) (timeout timeout))
          (let* ((tiny-timeout? (time<=? timeout short-timeout))
                 (ok? (fifo-handle-timed-put-once tx datum
                        (if tiny-timeout? timeout short-timeout))))
            (if (or ok? tiny-timeout?)
              ok?
              (%fifo-handle-timed-put tx datum (time-difference! timeout short-timeout)))))))))


;; non-blockingly try to put a datum into fifo-writer, and return one value:
;;   #t if successful, or #f on timeout
;;
;; Raises condition if fifo-writer is closed.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (fifo-writer-put) (fifo-writer-timed-put) (fifo-writer-try-put) and (fifo-writer-close)
;; on the same or different fifo-writers.
(define (fifo-writer-try-put tx datum)
  (assert* 'fifo-writer-try-put (fifo-writer? tx))
  (fifo-handle-timed-put-once tx datum 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thread==>


(define-syntax thread==>
  (lambda (stx)
    (syntax-case stx ()
      ((xname args ...)
        (letrec

          ;; traverse list, find first identifier whose syntax->datum is eq? to sym
          ;; and return its position in the list, or #f if not found
          ((scan=> (lambda (l sym)
            (let %scan=> ((l l) (pos 0))
              (cond
                ((null? l)
                  #f)
                ((and (identifier? (car l)) (eq? (syntax->datum (car l)) sym))
                  pos)
                (else
                  (%scan=> (cdr l) (fx1+ pos)))))))


          ;; scan template for #'_ and replace it with #`(make-thread-fifo-reader #,item).
          ;; if template contains no #'_ then insert item into template as first argument
          ;;
          ;; return template, modified in-place
          (replace_! (lambda (item template)
            (let ((pos (scan=> template '_)))
              (if pos
                (begin
                  (set-car! (list-tail template pos) #`(make-thread-fifo-reader #,item))
                  template)
                (cons (car template) (cons #`(make-thread-fifo-reader #,item) (cdr template)))))))


          ;; expand (=> head rest)
          (compose=> (lambda (k head rest)
            (let ((pos (scan=> rest '=>)))
              (if pos
                (let* ((mid  (list-head rest pos))
                       (mid* (replace_! head mid))
                       (tail (list-tail rest (fx1+ pos))))
                  (compose=> k mid* tail))
                (replace_! head (list-copy rest))))))


          ;; implementation of macro thread==>
          (expand==> (lambda (k l)
            (when (null? l)
              (syntax-violation "" "invalid syntax, need at least one argument after" 'thread==>))
            (let ((pos (scan=> l '=>)))
              (if pos
                (compose=> k (list-head l pos) (list-tail l (fx1+ pos)))
                #`(make-thread-fifo-reader #,l))))))


        ;; finally, the macro thread==> definition
        (expand==> #'xname #'(args ...)))))))



;; customize how "fifo-reader" objects are printed
(record-writer (record-type-descriptor fifo-reader)
  (lambda (rx port writer)
    (put-string port "#<fifo-reader")
    (put-string port (if (obj-reader-eof? rx) " eof" " ok"))
    (put-char port #\>)))


;; customize how "fifo-writer" objects are printed
(record-writer (record-type-descriptor fifo-writer)
  (lambda (tx port writer)
    (put-string port "#<fifo-writer")
    (put-string port (if (obj-writer-eof? tx) " eof" " ok"))
    (put-char port #\>)))
