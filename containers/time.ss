;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.


#!r6rs

(library (scheme2k containers time (0 9 3))
  (export
      (rename (make-time-duration duration))
      make-time-duration make-time-monotonic make-time-utc make-time-process make-time-thread make-time-collector-cpu make-time-collector-real
      time-compare)
  (import
    (rnrs)
    (only (chezscheme)   make-time record-rtd record-writer time-second time-nanosecond time-type))


(define (make-time-duration s ns)
  (make-time 'time-duration ns s))


(define (make-time-monotonic s ns)
  (make-time 'time-monotonic ns s))


(define (make-time-utc s ns)
  (make-time 'time-utc ns s))


(define (make-time-process s ns)
  (make-time 'time-process ns s))


(define (make-time-thread s ns)
  (make-time 'time-thread ns s))


(define (make-time-collector-cpu s ns)
  (make-time 'time-collector-cpu ns s))


(define (make-time-collector-real s ns)
  (make-time 'time-collector-real ns s))


;; compare two times.
;;   return #f if times have different type,
;;   return -1 if first time is earlier,
;;   return 0 if times are equal,
;;   return 1 if first time is later
(define (time-compare t1 t2)
  (and (eq? (time-type t1) (time-type t2))
       ;; same time-type
       (let ((s1  (time-second t1))     (s2  (time-second t2))
             (ns1 (time-nanosecond t1)) (ns2 (time-nanosecond t2)))
         (cond
           ((< s1  s2)  -1)
           ((> s1  s2)  1)
           ((< ns1 ns2) -1)
           ((> ns1 ns2) 1)
           (else        0)))))


;; customize how "time" objects are printed
(record-writer (record-rtd (make-time 'time-duration 0 0))
  (lambda (t out writer)
    (let* ((type (time-type t))
           (known-type? (memq type '(time-utc time-monotonic time-duration time-process time-thread time-collector-cpu time-collector-real)))
           (s    (time-second t))
           (ns   (time-nanosecond t)))
      (put-string out (if known-type? "(make-" "(make-time "))
      (display type out)
      (put-char out #\space)
      (writer (if known-type? s ns) out)
      (put-char out #\space)
      (writer (if known-type? ns s) out)
      (put-string out ")"))))

) ; close library


;; load the library above: sets the record-writer for time objects
(invoke-library '(scheme2k containers time))
