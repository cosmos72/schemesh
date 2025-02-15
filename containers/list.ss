;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;; this file should be included only by file containers/misc.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;     some additional list functions    ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; create and return a closure that iterates on elements of list l.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values elem #t) i.e. the next element in list l and #t,
;; or (values #<unspecified> #f) if end of list is reached.
(define (in-list l)
  (lambda ()
    (if (null? l)
      (values #f #f)
      (let ((elem (car l)))
        (set! l (cdr l))
        (values elem #t)))))


;; create and return a closure that iterates on elements of list l.
;;
;; the returned closure accepts no arguments, and each call to it returns two values:
;; either (values tail #t) i.e. the list containing next element and all subsequent ones, and #t,
;; or (values #<unspecified> #f) if end of list is reached.
(define (on-list l)
  (lambda ()
    (if (null? l)
      (values #f #f)
      (let ((tail l))
        (set! l (cdr l))
        (values tail #t)))))


;; (list-iterate l proc) iterates on all elements of given list l,
;; and calls (proc elem) on each element. Stops iterating if (proc ...) returns #f
;;
;; Returns #t if all calls to (proc elem) returned truish,
;; otherwise returns #f.
(define (list-iterate l proc)
  (do ((tail l (cdr tail)))
      ((or (null? tail) (not (proc (car tail))))
       (null? tail))))


;; For each item in items (which must be a list), when found in list l destructively
;; replace it with (list 'quote item).
;; Comparison between items is performed with eq?
(define (list-quoteq! items l)
  (do ((tail l (cdr tail)))
      ((null? tail) l)
    (let ((item (car tail)))
      (when (memq item items)
        (set-car! tail (list 'quote item))))))



;; (list-reverse*! l) destructively reverses list l,
;; creating an improper list - unless (car l) is itself a list.
;;
;; Example: (list-reverse*! (list a b c)) returns '(c b . a)
(define (list-reverse*! l)
  (if (or (null? l) (null? (cdr l)))
    l
    (let* ((tail (if (pair? (cdr l)) (cddr l) '()))
           (head (let ((first  (car l))
                       (second (cadr l)))
                   (set-car! l second)
                   (set-cdr! l first)
                   l)))
      (let %step ((head head)
                  (tail tail))
        (if (null? tail)
          head
          (let ((new-head tail)
                (new-tail (cdr tail)))
            (set-cdr! new-head head)
            (%step new-head new-tail)))))))

;; remove consecutive duplicates from a list, and return it.
;; elements are considered duplicates if (equal-pred elem1 elem2) returns truish.
(define (list-remove-consecutive-duplicates! l equal-pred)
  (let %recurse ((tail l))
    (cond
      ((or (null? tail) (null? (cdr tail)))
        l)
      ((equal-pred (car tail) (cadr tail))
        (set-cdr! tail (cddr tail))
        (%recurse tail))
      (#t
        (%recurse (cdr tail))))))
