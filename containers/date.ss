;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


#!r6rs

(library (scheme2k containers date (0 9 3))
  (export date date-systz date->string string->date)
  (import
    (rnrs)
    (rnrs mutable-strings)
    (only (chezscheme) date-year date-month date-day date-hour date-minute date-second date-nanosecond
                       date-zone-name date-zone-offset fx/ fx1+ make-date record-rtd record-writer string-copy!))


;; create and return a date with specified fields.
;; time zone offset is represented in seconds, as (make-date) expects
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
(define date-systz
  (case-lambda
    ((year month day)
      (make-date 0 0 0 0 day month year))
    ((year month day hour minute second)
      (make-date 0 second minute hour day month year))
    ((year month day hour minute second nanosecond)
      (make-date nanosecond second minute hour day month year))))


(define (date->string-length yneg ylen nanos tzmin)
  (fx+ (fx+ (if yneg 1 0)
            (fxmax 4 ylen))
       (fx+ 15
            (fx+ (if (fxzero? nanos) 0 10)
                 (if (fxzero? tzmin) 1 6)))))


(define (put-year buf yneg ystr ylen)
  (when yneg
    (string-set! buf 0 #\-))
  (let ((pos (fx+ (if yneg 1 0)
                  (fxmax 0 (fx- 4 ylen)))))
   (string-copy! ystr 0 buf pos ylen)
   (fx+ pos ylen)))


(define (put-2digits buf pos ch digits)
  (string-set! buf pos ch)
  (let-values (((tens units) (fxdiv-and-mod digits 10)))
    (string-set! buf (fx1+ pos) (integer->char (fx+ tens 48)))
    (string-set! buf (fx+ 2 pos) (integer->char (fx+ units 48))))
  (fx+ 3 pos))


(define (put-3digits buf pos digits)
  (let*-values (((tens    units) (fxdiv-and-mod digits 10))
                ((hundreds tens) (fxdiv-and-mod tens   10)))
    (string-set! buf      pos  (integer->char (fx+ hundreds 48)))
    (string-set! buf (fx1+ pos) (integer->char (fx+ tens 48)))
    (string-set! buf (fx+ 2 pos) (integer->char (fx+ units 48))))
  (fx+ 3 pos))


(define (put-9digits buf pos digits)
  (let*-values (((millions  units) (fxdiv-and-mod digits 1000000))
                ((thousands units) (fxdiv-and-mod units 1000)))
    (let* ((pos (put-3digits buf pos millions))
           (pos (put-3digits buf pos thousands))
           (pos (put-3digits buf pos units)))
      pos)))
  

(define (put-nanos buf pos ch nanos)
  (if (fxzero? nanos)
    pos
    (begin
      (string-set! buf pos ch)
      (put-9digits buf (fx1+ pos) nanos))))


(define (put-tz buf pos tzmin)
  (cond
    ((fxzero? tzmin)
      (string-set! buf pos #\Z)
      (fx1+ pos))
    (else
      (let* ((tzneg (fx<? tzmin 0))
             (tzabs (if tzneg (- tzmin) tzmin)))
        (let-values (((tzhour tzmin) (div-and-mod tzabs 60)))
          (let* ((pos (put-2digits buf pos (if tzneg #\- #\+) tzhour))
                 (pos (put-2digits buf pos #\: tzmin)))
            pos))))))
  

;; convert a date to RFC 3339 string, which is stricter than ISO 8601
(define (date->string d)
  (let* ((year  (date-year d))
         (yneg  (< year 0))
         (ystr  (number->string (if yneg (- year) year))) ; TODO: slow and allocates a lot
         (ylen  (string-length ystr))
         (nanos (date-nanosecond d))
         (tzmin (fx/ (fx+ (date-zone-offset d) 30) 60))
         (buf   (make-string (date->string-length yneg ylen nanos tzmin) #\0))
         (pos   (put-year    buf yneg ystr ylen))
         (pos   (put-2digits buf pos #\- (date-month d)))
         (pos   (put-2digits buf pos #\- (date-day d)))
         (pos   (put-2digits buf pos #\T (date-hour d)))
         (pos   (put-2digits buf pos #\: (date-minute d)))
         (pos   (put-2digits buf pos #\: (date-second d)))
         (pos   (put-nanos   buf pos #\. nanos)))
    (put-tz buf pos tzmin)
    buf))
  
    
;; convert a date from RFC 3339 string, which is stricter than ISO 8601
(define (string->date str)
  ;; TODO
  str)


;; customize how "date" objects are printed
;; time zone offset is represented in seconds, as (make-date) and (date) expect
(record-writer (record-rtd (make-date 0 0 0 0 1 1 1970 0))
  (lambda (d out writer)
    (let ((default-tz? (date-zone-name d))) ; truish if timezone is the system default
      (put-string out (if default-tz? "(date-systz " "(date "))
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
