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
