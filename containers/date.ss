;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


#!r6rs

(library (scheme2k containers date (0 9 3))
  (export date date<? date<=? date=? date>=? date>? date-compare (rename (date=? date-equiv?))
          date-or-false date-systz date->string string->date)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) date-year date-month date-day date-hour date-minute date-second date-nanosecond
                       date-zone-name date-zone-offset date->time-utc    foreign-procedure fx/ fx1+
                       logand make-date meta-cond print-radix record-rtd record-writer string-copy!)
    (only (scheme2k bootstrap)       raise-errorf)
    (only (scheme2k containers time) time-compare))


;; compare two dates.
;;   return -1 if first date is earlier,
;;   return 0 if dates are equal,
;;   return 1 if first date is later
;;
;; Note: dates are fully ordered, never returns #f or other results.
;; should never raise condition
(define (date-compare d1 d2)
  (cond
    ((eq? d1 d2)
      0)
    ((fx=? (date-zone-offset d1) (date-zone-offset d2))
      ;; same time zone => compare each field
      (let ((year1 (date-year d1)) (year2 (date-year d2)))
        (cond
          ((< year1 year2) -1)
          ((> year1 year2) 1)
          (else
            (let ((month1 (date-month  d1)) (month2 (date-month  d2)))
              (cond
                ((< month1  month2) -1)
                ((> month1  month2) 1)
                (else
                  (let ((day1 (date-day d1)) (day2 (date-day d2)))
                    (cond
                      ((< day1 day2) -1)
                      ((> day1 day2) 1)
                      (else
                        (let ((hour1 (date-hour d1)) (hour2 (date-hour d2)))
                          (cond
                            ((< hour1 hour2) -1)
                            ((> hour1 hour2) 1)
                            (else
                              (let ((minute1 (date-minute d1)) (minute2 (date-minute d2)))
                                (cond
                                  ((< minute1 minute2) -1)
                                  ((> minute1 minute2) 1)
                                  (else
                                    (let ((second1 (date-second d1)) (second2 (date-second d2)))
                                      (cond
                                        ((< second1 second2) -1)
                                        ((> second1 second2) 1)
                                        (else
                                          (let ((ns1 (date-nanosecond d1)) (ns2 (date-nanosecond d2)))
                                            (cond
                                              ((< ns1 ns2) -1)
                                              ((> ns1 ns2) 1)
                                              (else 0))))))))))))))))))))))
    (else
      ;; different time zones => convert to time-utc and compare them
      (or (time-compare (date->time-utc d1) (date->time-utc d2)) 0))))


(define (date<? d1 d2)
  (fx<? (date-compare d1 d2) 0))

(define (date<=? d1 d2)
  (fx<=? (date-compare d1 d2) 0))

(define (date=? d1 d2)
  (fxzero? (date-compare d1 d2)))

(define (date>=? d1 d2)
  (fx>=? (date-compare d1 d2) 0))

(define (date>? d1 d2)
  (fx>? (date-compare d1 d2) 0))


;; create and return a date with specified fields.
;; time zone offset is represented in seconds, as (make-date) expects
;; raise condition if fields represent an invalid date.
(define date
  (case-lambda
    ((year month day offset)
      (make-date 0 0 0 0 day month year offset))
    ((year month day hour minute second offset)
      (make-date 0 second minute hour day month year offset))
    ((year month day hour minute second nanosecond offset)
      (make-date nanosecond second minute hour day month year offset))))


;; create and return a date with specified fields.
;; uses system default time zone
;; raise condition if fields represent an invalid date.
(define date-systz
  (case-lambda
    ((year month day)
      (make-date 0 0 0 0 day month year))
    ((year month day hour minute second)
      (make-date 0 second minute hour day month year))
    ((year month day hour minute second nanosecond)
      (make-date nanosecond second minute hour day month year))))


(define (leap-year? year)
  (and (fxzero? (logand year 3))
       (if (fxzero? (mod year 100))
         (fxzero? (mod year 400))
         #t)))


(define (exact-integer? n)
  (or (fixnum? n)
      (and (integer? n) (exact? n))))


(define (valid-date? year month day)
  ;; Chez Scheme requires (fixnum? year) and also 1900 < year < 2^31 in date objects
  (and (fixnum? year)
       (meta-cond
         ((fixnum? #x7fffffff)
           (fx<=? 1901 year #x7fffffff))
         (else
           (fx<=? 1901 year)))
       (fixnum? month) (fx<=? 1 month 12)
       (fixnum? day)   (fx<=? 1 day 31)
       (cond
         ((fx<=? day 28)
           #t)
         ((fx=? month 2)
           (and (fx=? day 29) (leap-year? year)))
         (else
           (fx<=? day (bytevector-u8-ref #vu8(0 31 28 31 30 31 30 31 31 30 31 30 31) month))))))


(define (valid-time? hour minute second)
  (and (fixnum? hour)   (fx<=? 0 hour 23)
       (fixnum? minute) (fx<=? 0 minute 59)
       (fixnum? second) (fx<=? 0 second 59))) ; don't allow leap seconds


(define (valid-nanosecond? ns)
  (meta-cond
    ((fixnum? 999999999)
      (and (fixnum? ns) (fx<=? 0 ns 999999999)))
    (else
      (and (exact? ns) (integer? ns) (<= 0 ns 999999999)))))


;; create and return a date with specified fields.
;; time zone offset is represented in seconds, as (make-date) expects
;; return #f if fields represent an invalid date.
(define (date-or-false year month day hour minute second nanosecond offset)
  (and (valid-date? year month  day)
       (valid-time? hour minute second)
       (valid-nanosecond? nanosecond)
       (fixnum? offset) (fx<=? -86400 offset 86400)

       (make-date nanosecond second minute hour day month year offset)))


;; convert a date to RFC 3339 string, which is stricter than ISO 8601
;; return string
;; raise condition if d is not a date
(define date->string
  (let ((c-date->string (foreign-procedure "c_date_to_string" (integer-32 unsigned-8 unsigned-8
                                                               unsigned-8 unsigned-8 unsigned-8
                                                               unsigned-32 integer-32) ptr)))
    (lambda (d)
      (c-date->string (date-year d) (date-month d)  (date-day d)
                      (date-hour d) (date-minute d) (date-second d)
                      (date-nanosecond d) (date-zone-offset d)))))


;; convert a date from RFC 3339 string, which is stricter than ISO 8601
;; return date, or #f if string could not be parsed
;; raise condition if s is not a string
(define string->date
  (let ((c-string->date (foreign-procedure "c_string_to_date" (ptr ptr) int)))
    (lambda (s)
      (unless (string? s)
        (raise-errorf 'string->date "~s is not a string" s))
      (let ((bv (make-bytevector 20)))
        (and (fxzero? (c-string->date s bv))
             (date-or-false (bytevector-s32-native-ref bv 0)  ; year
                            (bytevector-u8-ref         bv 4)  ; month
                            (bytevector-u8-ref         bv 5)  ; day
                            (bytevector-u8-ref         bv 6)  ; hour
                            (bytevector-u8-ref         bv 7)  ; minute
                            (bytevector-u8-ref         bv 8)  ; second
                            (bytevector-u32-native-ref bv 12) ; nanosecond
                            (bytevector-s32-native-ref bv 16))))))) ; timezone offset, in seconds


(define (write-digits nn out writer base10?)
  (when (and base10? (fx<? nn 10))
    (put-char out #\0))
  (writer nn out))


;; customize how "date" objects are printed
;; time zone offset is represented in seconds, as (make-date) and (date) expect
(record-writer (record-rtd (make-date 0 0 0 0 1 1 1970 0))
  (lambda (d out writer)
    (let ((default-tz? (date-zone-name d)) ; truish if timezone is the system default
          (base10? (fx=? 10 (print-radix))))
      (put-string out (if default-tz? "(date-systz " "(date "))
      (writer (date-year d) out)
      (put-char out #\space)
      (write-digits (date-month d) out writer base10?)
      (put-char out #\space)
      (write-digits (date-day d) out writer base10?)
      (let ((hour   (date-hour d))
            (minute (date-minute d))
            (second (date-second d))
            (ns     (date-nanosecond d)))
        (unless (and (fxzero? hour) (fxzero? minute) (fxzero? second) (zero? ns))
          (put-string out "  ")
          (write-digits hour out writer base10?)
          (put-char out #\space)
          (write-digits minute out writer base10?)
          (put-char out #\space)
          (write-digits second out writer base10?)
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
