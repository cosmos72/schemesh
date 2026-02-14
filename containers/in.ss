;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k containers in (0 9 3))
  (export
    constant in-value in-values in-numbers in-range
    in-roundrobin in-list-roundrobin in-iterators number->cflonum
    iterator->list iterator->vector)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         cflonum? cfl+ fl-make-rectangular fx1+ last-pair reverse!)
    (only (scheme2k bootstrap) assert* debugf))


;; A iterator is a closure that, at each call, returns N+1 values:
;;   * N values: the next element in the iterator.
;;               Examples: the next element in a list or vector, or the next key and value in a hashtable.
;;   * a boolean, #f if end-of-iterator is reached, otherwise #t.
;;                if #f, the other N returned values are unspecified.
;;
;; A unary iterator is the case where N = 1: a closure that, at each call, returns two values:
;;   * a datum, representing the next element in the iterator
;;   * a boolean, #f if end-of-iterator is reached, otherwise #t.
;;                if #f, the other datum is unspecified


;; create and return a closure that always returns specified argument(s)
(define constant
  (case-lambda
   (() (let ((%constant0 (lambda () (values)))) ; name shown when displaying the closure
         %constant0))
   ((x) (let ((%constant1 (lambda () x))) ; name shown when displaying the closure
          %constant1))
   ((x y) (let ((%constant2 (lambda () (values x y)))) ; name shown when displaying the closure
            %constant2))
   ((x y z) (let ((%constant3 (lambda () (values x y z)))) ; name shown when displaying the closure
              %constant3))
   ((x y z w) (let ((%constant4 (lambda () (values x y z w)))) ; name shown when displaying the closure
                %constant4))
   (args  (let ((%constants (lambda () (apply values args)))) ; name shown when displaying the closure
            %constants))))


;; create and return a closure that always returns the same two values: args followed by #t
(define (in-value arg)
  (constant arg #t))


;; create and return a closure that always returns the same N+1 values: args followed by #t
(define (in-values . args)
  (apply constant (append args '(#t))))


;; create and return a closure that returns exact or inexact real numbers in the interval [start, end)
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in interval [start, end) and #t,
;; or (values #<unspecified> #f) if end of range is reached.
;;
;; If step is zero or a very small inexact real, the closure may never reach end of range.
;;
;; Implementation:
;;  if all arguments are fixnums, calls (in-fixnum-range)
;;  otherwise, if all arguments are exact, calls (in-exact-range)
;;  otherwise calls (in-flonum-range)
(define in-range
  (case-lambda
    ((start end step)
      (assert* 'in-range (real? start))
      (assert* 'in-range (real? end))
      (assert* 'in-range (real? step))
      (cond
        ((and (fixnum? start) (fixnum? end) (fixnum? step))
          (let ((in-fixnum-range ; name shown when displaying the closure
                  (if (fx>=? step 0)
                    (lambda ()
                      (if (fx<? start end)
                        (let ((ret start))
                          (set! start (fx+ start step))
                          (values ret #t))
                        (values end #f)))
                    (lambda ()
                      (if (fx>? start end)
                        (let ((ret start))
                          (set! start (fx+ start step))
                          (values ret #t))
                        (values end #f))))))
            in-fixnum-range))
        ((and (exact? start) (exact? end) (exact? step))
          (let ((in-exact-range ; name shown when displaying the closure
                  (if (>= step 0)
                    (lambda ()
                      (if (< start end)
                        (let ((ret start))
                          (set! start (+ start step))
                          (values ret #t))
                        (values end #f)))
                    (lambda ()
                      (if (> start end)
                        (let ((ret start))
                          (set! start (+ start step))
                          (values ret #t))
                        (values end #f))))))
            in-exact-range))
        (else
          (let ((start (real->flonum start))
                (end   (real->flonum end))
                (step  (real->flonum step)))
            (let ((in-flonum-range ; name shown when displaying the closure
                    (if (fl>=? step 0.0)
                      (lambda ()
                        (if (fl<? start end)
                          (let ((ret start))
                            (set! start (fl+ start step))
                            (values ret #t))
                          (values end #f)))
                      (lambda ()
                        (if (fl>? start end)
                          (let ((ret start))
                            (set! start (fl+ start step))
                            (values ret #t))
                          (values end #f))))))
              in-flonum-range)))))
    ((start end)
      (in-range start end 1))
    ((end)
      (in-range 0 end 1))))


;; convert an exact or inexact real or complex number to an inexact complex number, i.e. a cflonum.
(define (number->cflonum num)
  (assert* 'number->cflonum (number? num))
  (cond
    ((cflonum? num)
      num)
    ((real? num)
      (fl-make-rectangular (real->flonum num) 0.0))
    (else
      (fl-make-rectangular (real->flonum (real-part num)) (real->flonum (imag-part num))))))


;; create and return a closure that returns an unlimited iterator
;; of exact or inexact real or complex numbers,
;; starting from start and adding step at each iteration, which defaults to 1.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; (values elem #t) i.e. the next element and #t.
(define in-numbers
  (case-lambda
    ((start step)
      (assert* 'in-numbers (number? start))
      (assert* 'in-numbers (number? step))
      (cond
        ((and (exact? start) (exact? step))
          (let ((in-exact-numbers ; name shown when displaying the closure
                  (lambda ()
                    (let ((ret start))
                      (set! start (+ start step))
                        (values ret #t)))))
            in-exact-numbers))
        ((and (real? start) (real? step))
          (let* ((start (real->flonum start))
                 (step  (real->flonum step))
                 (in-flonum-numbers ; name shown when displaying the closure
                   (lambda ()
                     (let ((ret start))
                       (set! start (fl+ start step))
                         (values ret #t)))))
            in-flonum-numbers))
        (else
          (let* ((start (number->cflonum start))
                 (step  (number->cflonum step))
                 (in-cflonum-numbers ; name shown when displaying the closure
                   (lambda ()
                     (let ((ret start))
                       (set! start (cfl+ start step))
                         (values ret #t)))))
            in-cflonum-numbers))))
    ((start)
      (in-numbers start 1))
    (()
      (in-numbers 0 1))))


;; Create and returns a closure that sequentially composes all specified iterators.
;; Each iterator is initiated only after the preceding one is exhausted.
;; If a single iterator is provided, then it is returned;
;; otherwise all iterators should return the same number of multiple values at each iteration
(define in-iterators
  (case-lambda
    ((iter) iter)
    ((iter0 . iters)
      (let* ((iters (cons iter0 iters))
             (%in-iterators ; name shown when displaying the closure
               (lambda ()
                 (let %loop ()
                   (let ((iter (car iters)))
                     ;; (debugf "iterating on ~s, remaining iterators ~s" iter (cdr iters))
                     (if (null? (cdr iters)) ; iter is the last iterator
                       (iter)
                       (let ((vals (call-with-values iter list)))
                         (if (car (last-pair vals))
                           ;; iter not exhausted yet, return its values
                           (apply values vals)
                           ;; iter exhausted, move to next iterator
                           (begin
                             (set! iters (cdr iters))
                             (%loop))))))))))
        %in-iterators))))


;; Create and returns a closure that alternates among input iterators.
;; Each time the closure is called, it forwards the call to the i-th input iterator,
;; then sets i to (fxmod (fx+ i step) (length iterators))
;;
;; If a single iterator is provided, then it is returned;
;; otherwise all iterators should return the same number of multiple values at each iteration
;;
;; If any iterator is exhausted, further calls will always return (... #f)
(define in-roundrobin
  (case-lambda
    ((iter) iter)
    ((iter . iters)
      (let* ((iterv (list->vector (cons iter iters)))
             (n    (vector-length iterv))
             (i    0)
             (eof-vals #f)
             (%in-roundrobin ; name shown when displaying the closure
               (lambda ()
                 (let %loop ()
                   (if eof-vals
                     (apply values eof-vals)
                     (let ((vals (call-with-values (vector-ref iterv i) list))
                           (i+1 (fx1+ i)))
                       (if (car (last-pair vals))
                         (set! i (if (fx<? i+1 n) i+1 0)) ; iter not exhausted, increment i
                         (set! eof-vals vals))            ; iter exhausted, fill eof-vals
                       (apply values vals)))))))
          %in-roundrobin))))


;; Create and returns a closure that alternates among input iterators.
;; Each time the closure is called, it forwards the call to the i-th input iterator,
;; then sets i to (fxmod (fx+ i step) (length iterators))
;;
;; If a single iterator is provided, then it is returned;
;; otherwise all iterators should return the same number of multiple values at each iteration
;;
;; If any iterator is exhausted, further calls will always return (... #f)
(define in-list-roundrobin
  (case-lambda
    ((iters start step)
      (assert* 'in-list-roundrobin (pair? iters))
      (assert* 'in-list-roundrobin (fixnum? start))
      (assert* 'in-list-roundrobin (fixnum? step))
      (if (null? (cdr iters))
        (car iters)
        (let* ((iterv (list->vector iters))
               (n     (vector-length iterv))
               (i     (fxmod start n))
               (eof-vals #f)
               (%in-list-roundrobin ; name shown when displaying the closure
                 (lambda ()
                   (let %loop ()
                     (if eof-vals
                       (apply values eof-vals)
                       (let ((vals (call-with-values (vector-ref iterv i) list)))
                         (if (car (last-pair vals))
                           (set! i (fxmod (fx+ i step) n)) ; iter not exhausted, increment i
                           (set! eof-vals vals))           ; iter exhausted, fill eof-vals
                         (apply values vals)))))))
          %in-list-roundrobin)))
    ((iters start)
      (in-list-roundrobin iters start 1))
    ((iters)
      (in-list-roundrobin iters 0 1))))


;; Read all elements from specified unary iterator, collect them into a list, and return such list.
(define (iterator->list iter)
  (let %iterator->list ((iter iter) (l '()))
    (let-values (((elem ok?) (iter)))
      (if ok?
        (%iterator->list iter (cons elem l))
        (reverse! l)))))


;; Read all elements from specified unary iterator, collect them into a vector, and return such vector.
(define (iterator->vector iter)
  (let %iterator->list ((iter iter) (l '()))
    (let-values (((elem ok?) (iter)))
      (if ok?
        (%iterator->list iter (cons elem l))
        (reverse! l)))))


) ; close library
