;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/queue-thread.ss or ipc/queue-nothread.ss


;; Create and return a thread-safe queue-reader and queue-writer, connected to each other
;; and with unlimited queue capacity.
;;
;; Each datum put to the queue-writer can be received by the queue-reader,
;; and if needed more queue-readers can be connected to the same queue-writer
;; by calling (make-queue-reader).
;;
;; Every queue-reader connected to the same queue-writer receives in order
;; each datum put to the queue-writer *after* that queue-reader was created,
;; in publish-and-subscribe style.
;;
;; A single queue-reader can also be used simultaneously from different threads, in dispatch style:
;; each datum put to the queue-writer will be received only by one thread.
(define make-queue-pair
  (case-lambda
    ((name)
      (let* ((tx (make-queue-writer name))
             (rx (make-queue-reader tx)))
        (values rx tx)))
    (()
      (make-queue-pair #f))))


;; block until a datum is received from the connected queue-writer, and return one value:
;;   #t if successful, or #f if queue-writer has been closed and all data has been received.
;;
;; This procedure is thread safe: multiple threads can concurrently call
;; (queue-reader-get) (queue-reader-timed-get) (queue-reader-try-get) (queue-reader-skip) and (queue-reader-close)
;; on the same or different queue-readers.
(define (queue-reader-skip rx)
  (assert* 'queue-reader-skip (queue-reader? rx))
  (obj-reader-skip rx))


;; create a new thread that calls (obj-reader-get reader) in a loop
;; and writes each generated element to an internal queue-writer.
;;
;; return a queue-reader connected to the internal queue-writer.
;;
;; TODO: use finite queues instead
(define (thread-queue-reader reader)
  (assert* 'thread-queue-reader (obj-reader? reader))
  (letrec* ((tx (make-queue-writer))
            (rx (make-queue-reader tx))

            (%thread-queue-reader-loop
              (lambda ()
                (let-values (((datum ok?) (obj-reader-get reader)))
                  (when ok?
                    (obj-writer-put tx datum)
                    (%thread-queue-reader-loop)))))

            (%thread-queue-reader
              (lambda ()
                (dynamic-wind
                  void
                  %thread-queue-reader-loop
                  (lambda () (obj-writer-close tx)))))

            (thread (make-thread %thread-queue-reader 'thread-queue-reader)))

    (thread-start! thread)
    rx))


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


          ;; scan template for #'_ and replace it with #`(thread-queue-reader #,item).
          ;; if template contains no #'_ then insert item into template as first argument
          ;;
          ;; return template, modified in-place
          (replace_! (lambda (item template)
            (let ((pos (scan=> template '_)))
              (if pos
                (begin
                  (set-car! (list-tail template pos) #`(thread-queue-reader #,item))
                  template)
                (cons (car template) (cons #`(thread-queue-reader #,item) (cdr template)))))))


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
                #`(thread-queue-reader #,l))))))


        ;; finally, the macro thread==> definition
        (expand==> #'xname #'(args ...)))))))



;; customize how "queue-reader" objects are printed
(record-writer (record-type-descriptor queue-reader)
  (lambda (rx port writer)
    (put-string port "#<queue-reader")
    (put-string port (if (obj-reader-eof? rx) " eof" " ok"))
    (let ((name (queue-reader-name rx)))
      (when name
        (put-char port #\space)
        (display name port)))
    (put-char port #\>)))


;; customize how "queue-writer" objects are printed
(record-writer (record-type-descriptor queue-writer)
  (lambda (tx port writer)
    (put-string port "#<queue-writer")
    (put-string port (if (obj-writer-eof? tx) " eof" " ok"))
    (let ((name (queue-writer-name tx)))
      (when name
        (put-char port #\space)
        (display name port)))
    (put-char port #\>)))
