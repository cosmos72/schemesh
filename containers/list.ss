;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh containers list (0 7 7))
  (export
    any count every in-list
    list-iterate list-quoteq! list-reverse*! list-remove-consecutive-duplicates!
    on-list)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) fx1+ fx1-))


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



;; apply proc element-wise to the (car) of each list
(define (%apply-proc proc lists)
  (apply proc (map car lists)))


;; (any proc l ...) specialization for a single list
(define (%any1 proc l)
  (if (null? l)
    #f
    (or (proc (car l)) (%any1 proc (cdr l)))))


;; apply proc element-wise to the elements of the lists, stop at the first truish value returned by (proc elem ...) and return it.
;; If all calls to (proc elem ...) return #f, then return #f.
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
(define any
  (case-lambda
    ((proc)
      #f)
    ((proc l)
      (%any1 proc l))
    ((proc l1 l2)
      (let %any ((proc proc) (tail1 l1) (tail2 l2))
        (if (or (null? tail1) (null? tail2))
          #f
          (or      (proc (car tail1) (car tail2))
              (%any proc (cdr tail1) (cdr tail2))))))
    ((proc l1 l2 l3)
      (let %any ((proc proc) (tail1 l1) (tail2 l2) (tail3 l3))
        (if (or (null? tail1) (null? tail2) (null? tail3))
          #f
          (or      (proc (car tail1) (car tail2) (car tail3))
              (%any proc (cdr tail1) (cdr tail2) (cdr tail3))))))
    ((proc . lists)
      (let %any ((proc proc) (tails lists))
        (if (%any1 null? tails)
          #f
          (or (%apply-proc proc tails)
              (%any proc (map cdr tails))))))))


;; apply proc element-wise to the elements of the lists, and count and return how many times (proc elem ...) evaluates to truish.
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
(define count
  (case-lambda
    ((proc)
      0)
    ((proc l)
      (let %count ((proc proc) (tail l) (ret 0))
        (if (null? tail)
          ret
          (%count
            proc
            (cdr tail)
            (if (proc (car tail)) (fx1+ ret) ret)))))
    ((proc l1 l2)
      (let %count ((proc proc) (tail1 l1) (tail2 l2) (ret 0))
        (if (or (null? tail1) (null? tail2))
          ret
          (%count
            proc
            (cdr tail1)
            (cdr tail2)
            (if (proc (car tail1) (car tail2)) (fx1+ ret) ret)))))
    ((proc l1 l2 l3)
      (let %count ((proc proc) (tail1 l1) (tail2 l2) (tail3 l3) (ret 0))
        (if (or (null? tail1) (null? tail2) (null? tail3))
          ret
          (%count
            proc
            (if (proc (car tail1) (car tail2) (car tail3)) (fx1+ ret) ret)
            (cdr tail1)
            (cdr tail2)
            (cdr tail3)))))
    ((proc . lists)
      (let %count ((proc proc) (tails lists) (ret 0))
        (if (%any1 null? tails)
          ret
          (%count
            proc
            (map cdr tails)
            (if (%apply-proc proc lists) (fx1+ ret) ret)))))))


;; apply proc element-wise to the elements of the lists, stop at the first #f returned by (proc elem ...) and return it.
;; If all calls to (proc elem ...) return truish, then return #t.
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
(define every
  (case-lambda
    ((proc)
      #t)
    ((proc l)
      (let %every ((proc proc) (tail l))
        (if (null? tail)
          #t
          (and (proc (car tail))
               (%every proc (cdr tail))))))
    ((proc l1 l2)
      (let %every ((proc proc) (tail1 l1) (tail2 l2))
        (if (or (null? tail1) (null? tail2))
          #t
          (and (proc (car tail1) (car tail2))
               (%every proc (cdr tail1) (cdr tail2))))))
    ((proc l1 l2 l3)
      (let %every ((proc proc) (tail1 l1) (tail2 l2) (tail3 l3))
        (if (or (null? tail1) (null? tail2) (null? tail3))
          #t
          (and (proc (car tail1) (car tail2) (car tail3))
               (%every proc (cdr tail1) (cdr tail2) (cdr tail3))))))
    ((proc . lists)
      (let %every ((proc proc) (tails lists))
        (if (%any1 null? tails)
          #t
          (and (%apply-proc proc tails)
               (%every proc (map cdr tails))))))))


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
      (else
        (%recurse (cdr tail))))))


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

) ; close library
