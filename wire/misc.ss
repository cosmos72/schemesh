;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

;; this file should be included only by file wire/wire.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "time" objects are serialized/deserialized

(define (len/time pos obj)
  (let* ((pos (tag+ pos))
         (pos (len/any pos (time-type obj)))
         (pos (len/any pos (time-nanosecond obj)))
         (pos (len/any pos (time-second obj))))
    pos))

(define (put/time bv pos obj)
  (let* ((pos (put/tag  bv pos tag-time))
         (pos (put/any  bv pos (time-type obj)))
         (pos (put/any  bv pos (time-nanosecond obj)))
         (pos (put/any  bv pos (time-second obj))))
    pos))

(define (get/time bv pos end)
  (let*-values (((type pos)       (get/any bv pos end))
                ((nanosecond pos) (get/any bv pos end))
                ((second pos)     (get/any bv pos end)))
    (if (and pos
             (memq type '(time-collector-cpu time-collector-real time-duration
                          time-monotonic time-process time-thread time-utc))
             (integer? nanosecond) (exact? nanosecond) (<= 0 nanosecond 999999999)
             (integer? second) (exact? second))
      (values
        (make-time type nanosecond second)
        pos)
      (values #f #f))))
