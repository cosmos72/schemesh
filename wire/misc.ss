;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

;; this file should be included only by file wire/wire.ss

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "date" objects are serialized/deserialized

(define (len/date pos obj)
  (let* ((pos (tag+ pos 5))
         (pos (len/any  pos (date-year obj)))
         (pos (len/any  pos (date-nanosecond obj)))
         (pos (len/any  pos (date-zone-offset obj))))
    pos))

(define (put/date bv pos obj)
  (let* ((pos (put/tag  bv pos tag-date))
         (pos (put/any  bv pos (date-year obj)))
         (pos (put/u8   bv pos (date-month obj)))
         (pos (put/u8   bv pos (date-day obj)))
         (pos (put/u8   bv pos (date-hour obj)))
         (pos (put/u8   bv pos (date-minute obj)))
         (pos (put/u8   bv pos (date-second obj)))
         (pos (put/any  bv pos (date-nanosecond obj)))
         (pos (put/any  bv pos (date-zone-offset obj))))
    pos))

(define (get/date bv pos end)
  (let*-values (((year pos)       (get/any bv pos end))
                ((month pos)      (get/u8  bv pos end))
                ((day pos)        (get/u8  bv pos end))
                ((hour pos)       (get/u8  bv pos end))
                ((minute pos)     (get/u8  bv pos end))
                ((second pos)     (get/u8  bv pos end))
                ((nanosecond pos) (get/any bv pos end))
                ((offset pos)     (get/any bv pos end)))
    (if (and pos
             (integer? year) (exact? year) (valid-date? year month day)
             (fx<=? 0 hour 23) (fx<=? 0 minute 59) (fx<=? 0 second 61) ; allow leap seconds
             (integer? nanosecond) (exact? nanosecond) (<= 0 nanosecond 999999999)
             (fixnum? offset) (<= -86400 offset 86400))
      (values
        (date year month day hour minute second nanosecond offset)
        pos)
      (values #f #f))))


(define (valid-date? year month day)
  (and (<= 1901 year) ;; Chez Scheme requires year >= 1901 in date objects
       (fx<=? 1 month 12)
       (fx<=? 1 day 31)
       (cond
         ((fx<=? day 28)
           #t)
         ((fx=? month 2)
           (and (fx=? day 29) (leap-year? year)))
         (else
           (fx<=? day (bytevector-u8-ref #vu8(0 31 28 31 30 31 30 31 31 30 31 30 31) month))))))


(define (leap-year? year)
  (and (fxzero? (fxand year 3))
       (if (fxzero? (fxmod year 100))
         (fxzero? (fxmod year 400))
         #t)))
      
              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customize how "time" objects are serialized/deserialized

(define (len/time pos obj)
  (let* ((pos (tag+ pos))
         (pos (len/any pos (time-type obj)))
         (pos (len/any pos (time-second obj)))
         (pos (len/any pos (time-nanosecond obj))))
    pos))

(define (put/time bv pos obj)
  (let* ((pos (put/tag  bv pos tag-time))
         (pos (put/any  bv pos (time-type obj)))
         (pos (put/any  bv pos (time-second obj)))
         (pos (put/any  bv pos (time-nanosecond obj))))
    pos))

(define (get/time bv pos end)
  (let*-values (((type pos)       (get/any bv pos end))
                ((second pos)     (get/any bv pos end))
                ((nanosecond pos) (get/any bv pos end)))
    (if (and pos
             (memq type '(time-collector-cpu time-collector-real time-duration
                          time-monotonic time-process time-thread time-utc))
             (integer? second) (exact? second)
             (integer? nanosecond) (exact? nanosecond) (<= 0 nanosecond 999999999)
             )
      (values
        (make-time type nanosecond second)
        pos)
      (values #f #f))))



