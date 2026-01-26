;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


#!r6rs

(library (scheme2k containers date (0 9 3))
  (export date date-tz)
  (import
    (rnrs)
    (only (chezscheme) date-year date-month date-day date-hour date-minute date-second date-nanosecond
                       date-zone-name date-zone-offset make-date record-rtd record-writer))


(define date
  (case-lambda
    ((year month day)
      (make-date 0 0 0 0 day month year))
    ((year month day hour minute second)
      (make-date 0 second minute hour day month year))
    ((year month day hour minute second nanosecond)
      (make-date nanosecond second minute hour day month year))))


;; time zone offset is represented in seconds, as (make-date) expects
(define date-tz
  (case-lambda
    ((year month day offset)
      (make-date 0 0 0 0 day month year offset))
    ((year month day hour minute second offset)
      (make-date 0 second minute hour day month year offset))
    ((year month day hour minute second nanosecond offset)
      (make-date nanosecond second minute hour day month year offset))))


;; customize how "date" objects are printed
;; time zone offset is represented in seconds, as (make-date) and (date-tz) expect
(record-writer (record-rtd (make-date 0 0 0 0 1 1 1970 0))
  (lambda (d out writer)
    (let ((default-tz? (date-zone-name d))) ; truish if timezone is the system default
      (put-string out (if default-tz? "(date " "(date-tz "))
      (writer (date-year d) out)
      (put-char out #\space)
      (writer (date-month d) out)
      (put-char out #\space)
      (writer (date-day d) out)
      (let ((hour   (date-hour d))
            (minute (date-minute d))
            (second (date-second d))
            (ns     (date-nanosecond d)))
        (unless (and (fxzero? hour) (fxzero? minute) (fxzero? second) (zero? ns))
          (put-string out "  ")
          (writer hour out)
          (put-char out #\space)
          (writer minute out)
          (put-char out #\space)
          (writer second out)
          (unless (zero? ns)
            (put-string out "  ")
            (writer ns out))))
      (unless default-tz?
        (put-char out #\space)
        (let ((offset (date-zone-offset d)))
          (when (fx>=? offset 0)
            (put-char out #\+))
          (writer offset out)))
      (put-string out ")"))))

) ; close library

;; load the library above: sets the record-writer for date objects
(invoke-library '(scheme2k containers date))
