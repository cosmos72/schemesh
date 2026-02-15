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

(meta-cond
  ((threaded?) (include "ipc/queue-thread.ss"))
  (else        (include "ipc/queue-nothread.ss")))
