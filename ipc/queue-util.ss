;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by files ipc/queue-thread.ss or ipc/queue-nothread.ss


;; customize how "queue-reader" objects are printed
(record-writer (record-type-descriptor queue-reader)
  (lambda (rx port writer)
    (let ((name (queue-reader-name rx)))
      (if name
        (begin
          (display "#<queue-reader " port)
          (display name port)
          (display ">" port))
        (display "#<queue-reader>" port)))))


;; customize how "queue-writer" objects are printed
(record-writer (record-type-descriptor queue-writer)
  (lambda (tx port writer)
    (let ((name (queue-writer-name tx)))
      (if name
        (begin
          (display "#<queue-writer " port)
          (display name port)
          (display ">" port))
        (display "#<queue-writer>" port)))))
