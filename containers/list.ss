;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh containers list (0 9 2))
  (export
    any count every for-alist for-list for-plist in-alist in-list in-plist on-list

    list-copy* list-index list-quoteq! list-reverse*! list-remove-consecutive-duplicates!

    plist? plist-add plist-ref plist-delete plist-delete/pred)

  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme)         fx1+ fx1- list-copy reverse! void)
    (only (schemesh bootstrap) forever generate-pretty-temporaries with-while-until))


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

(define %dot-args-always-create-new-list?
  (let ((args '(a)))
    (not (eq? args (apply list args)))))

;; return a copy of list l, or list l itself if (lambda (args . l) l) always creates a new list l,
;; even when called with (apply)
(define (list-copy* l)
  (if %dot-args-always-create-new-list? l (list-copy list)))


;; given list l whose elements are lists too,
;; modify l in-place by replacing each inner list with its cdr.
;; return l.
(define (%for-each-skip-car! l)
  (do ((tail l (cdr tail)))
      ((null? tail) l)
    (set-car! tail (cadr tail))))


;; apply proc element-wise to the (car) of each list
(define (%apply-proc proc lists)
  (apply proc (map car lists)))


;; (any proc l ...) specialization for a single list
(define (%any1 proc l)
  (if (null? l)
    #f
    (or (proc (car l)) (%any1 proc (cdr l)))))


;; apply proc element-wise to the elements of the lists,
;; stop at the first truish value returned by (proc elem ...) and return such value.
;;
;; If all calls to (proc elem ...) return #f, then return #f.
;;
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;; Proc must accept as many elements as there are lists, and must return a single value.
(define any
  (case-lambda
    ((proc l)
      (%any1 proc l))
    ((proc l1 l2)
      (let %any ((proc proc) (l1 l1) (tail2 l2))
        (if (or (null? l1) (null? tail2))
          #f
          (or      (proc (car l1) (car tail2))
              (%any proc (cdr l1) (cdr tail2))))))
    ((proc l1 l2 l3)
      (let %any ((proc proc) (l1 l1) (tail2 l2) (tail3 l3))
        (if (or (null? l1) (null? tail2) (null? tail3))
          #f
          (or      (proc (car l1) (car tail2) (car tail3))
              (%any proc (cdr l1) (cdr tail2) (cdr tail3))))))
    ((proc l1 l2 l3 l4)
      (let %any ((proc proc) (l1 l1) (tail2 l2) (tail3 l3))
        (if (or (null? l1) (null? tail2) (null? tail3))
          #f
          (or      (proc (car l1) (car tail2) (car tail3))
              (%any proc (cdr l1) (cdr tail2) (cdr tail3))))))
    ((proc l1 . lists)
      (let %any ((proc proc) (tails (list-copy* (cons l1 lists))))
        (if (%any1 null? tails)
          #f
          (or (%apply-proc proc tails)
              (%any proc (%for-each-skip-car! tails))))))))


;; apply proc element-wise to the elements of the lists,
;; and count and return how many times (proc elem ...) evaluates to truish.
;;
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;;
;; Proc must accept as many elements as there are lists, and must return a single value.
(define count
  (case-lambda
    ((proc)
      0)
    ((proc l)
      (let %count ((ret 0) (proc proc) (l l))
        (if (null? l)
          ret
          (%count (if (proc (car l)) (fx1+ ret) ret)
                  proc (cdr l)))))
    ((proc l1 l2)
      (let %count ((ret 0) (proc proc) (l1 l1) (tail2 l2))
        (if (or (null? l1) (null? tail2))
          ret
          (%count (if (proc (car l1) (car tail2)) (fx1+ ret) ret)
                  proc (cdr l1) (cdr tail2)))))
    ((proc l1 l2 l3)
      (let %count ((ret 0) (proc proc) (l1 l1) (tail2 l2) (tail3 l3))
        (if (or (null? l1) (null? tail2) (null? tail3))
          ret
          (%count (if (proc (car l1) (car tail2) (car tail3)) (fx1+ ret) ret)
                  proc (cdr l1) (cdr tail2) (cdr tail3)))))
    ((proc . lists)
      (let %count ((ret 0) (proc proc) (tails (list-copy* lists)))
        (if (%any1 null? tails)
          ret
          (let ((ret (if (%apply-proc proc tails) (fx1+ ret) ret)))
            (%count ret proc (%for-each-skip-car! tails))))))))


;; apply proc element-wise to the elements of the lists,
;; stop at the first #f returned by (proc elem ...) and return it.
;; If all calls to (proc elem ...) return truish, then return the value returned by the last call to (proc elem).
;;
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;;
;; Proc must accept as many elements as there are lists, and must return a single value.
(define every
  (case-lambda
    ((proc)
      #t)
    ((proc l)
      (let %every ((ret #t) (proc proc) (l l))
        (if (or (not ret) (null? l))
          ret
          (%every (proc (car l))
                  proc (cdr l)))))
    ((proc l1 l2)
      (let %every ((ret #t) (proc proc) (l1 l1) (tail2 l2))
        (if (or (not ret) (null? l1) (null? tail2))
          ret
          (%every (proc (car l1) (car tail2))
                  proc (cdr l1) (cdr tail2)))))
    ((proc l1 l2 l3)
      (let %every ((ret #t) (proc proc) (l1 l1) (tail2 l2) (tail3 l3))
        (if (or (not ret) (null? l1) (null? tail2) (null? tail3))
          ret
          (%every (proc (car l1) (car tail2) (car tail3))
                  proc (cdr l1) (cdr tail2) (cdr tail3)))))
    ((proc . lists)
      (let %every ((ret #t) (proc proc) (tails (list-copy* lists)))
        (if (or (not ret) (%any1 null? tails))
          ret
          (let ((ret (%apply-proc proc tails)))
            (%every ret proc (%for-each-skip-car! tails))))))))


;; Iterate in parallel on elements of given lists l ..., and evaluate body ... on each element.
;; Stop iterating when the shortest list is exhausted, and return unspecified value.
;;
;; If no lists are specified, behave as (forever body ...)
(define-syntax for-list
  (lambda (stx)
    (syntax-case stx ()
      ((_ () body ...)
        #'(forever body ...))
      ((_ ((var l) ...) body ...)
        (with-syntax (((tail ...) (generate-pretty-temporaries #'(l ...))))
          #'(let %for-list ((tail l) ...)
              (if (or (null? tail) ...)
                (void)
                (let ((var (car tail)) ...)
                  (with-while-until
                    body ...
                    (%for-list (cdr tail) ...))))))))))



;; apply proc element-wise to the elements of the lists,
;; stop at the first truish value returned by (proc elem ...) and return the *index* of such value.
;;
;; If all calls to (proc elem ...) return #f, then return #f.
;;
;; If not all lists have the same length, iteration terminates when the end of shortest list is reached.
;;
;; Proc must accept as many elements as there are lists, and must return a single value.
;;
;; Extension: if only one list is specified and proc is not a procedure,
;; search for first element eqv? to proc.
(define list-index
  (case-lambda
    ((proc l)
      (let %list-index ((i 0)
                        (proc (if (procedure? proc) proc (lambda (elem) (eqv? proc elem))))
                        (l l))
         (cond
           ((null? l)      #f)
           ((proc (car l)) i)
           (else
             (%list-index (fx1+ i) proc (cdr l))))))
    ((proc l1 l2)
      (let %list-index ((i 0) (proc proc) (l1 l1) (l2 l2))
         (cond
           ((or (null? l1) (null? l2)) #f)
           ((proc (car l1) (car l2))   i)
           (else
             (%list-index (fx1+ i) proc (cdr l1) (cdr l2))))))
    ((proc l1 l2 l3)
      (let %list-index ((i 0) (proc proc) (l1 l1) (l2 l2) (l3 l3))
         (cond
           ((or (null? l1) (null? l2) (null? l3)) #f)
           ((proc (car l1) (car l2) (car l3))     i)
           (else
             (%list-index (fx1+ i) proc (cdr l1) (cdr l2) (cdr l3))))))
    ((proc . lists)
      (let %list-index ((i 0) (proc proc) (lists (list-copy* lists)))
         (cond
           ((%any1 null? lists)      #f)
           ((%apply-proc proc lists) i)
           (else
             (%list-index (fx1+ i) proc (%for-each-skip-car! lists))))))))


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


;; in-place remove consecutive duplicates from a list, and return it.
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


;; create and return a closure that iterates on elements of association list alist.
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in alist and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of alist is reached.
(define (in-alist alist)
  (lambda ()
    (if (null? alist)
      (values #f #f #f)
      (let ((key (caar alist))
            (val (cdar alist)))
        (set! alist (cdr alist))
        (values key val #t)))))


;; Iterate in parallel on elements of given alists, and evaluate body ... on each element.
;; Stop iterating when the shortest list is exhausted, and return unspecified value.
;;
;; If no lists are specified, behave as (forever body ...)
(define-syntax for-alist
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((key val alist) ...) body ...)
        (if (null? #'(alist ...))
          #'(forever body ...)
          (with-syntax (((tail ...) (generate-pretty-temporaries #'(alist ...))))
            #'(let %for-alist ((tail alist) ...)
                (if (or (null? tail) ...)
                  (void)
                  (let ((key (caar tail)) ...
                        (val (cdar tail)) ...)
                    (with-while-until
                      body ...
                      (%for-alist (cdr tail) ...)))))))))))


;; return #t if plist is a property list, otherwise return #f
;; Note: return #f for all improper lists, including cyclic lists.
(define (plist? plist)
  (and (list? plist)
       (even? (length plist))))


;; given a property list plist, i.e. a list containing alternate keys and values,
;; return a new property list also containing key and value, followed by the old plist (which is not modified)
(define (plist-add plist key value)
  (cons key (cons value plist)))



;; given a property list plist, i.e. a list containing alternate keys and values,
;; find the first key eq? to specified key and return the corresponding value.
;;
;; If no such key is found then return default, which defaults to #f
(define plist-ref
  (case-lambda
    ((plist key default)
      (let %plist-ref ((plist plist) (key key) (default default))
        (cond
          ((null? plist)
            default)
          ((eq? key (car plist))
            (cadr plist))
          (else
            (%plist-ref (cddr plist) key default)))))
    ((plist key)
      (plist-ref plist key #f))))


;; given a property list plist, i.e. a list containing alternate keys and values,
;; return a new new property list where all occurrences of key and the corresponding values have been removed.
;; Does not modify plist.
(define (plist-delete plist key)
  (let %plist-delete ((ret '()) (plist plist) (key key))
    (cond
      ((null? plist)
        (reverse! ret))
      ((eq? key (car plist))
        (%plist-delete ret (cddr plist) key))
      (else
        ;; insert value and key at the beginning of ret
        (%plist-delete (cons (cadr plist) (cons (car plist) ret))
                       (cddr plist)
                       key)))))


;; given a property list plist, i.e. a list containing alternate keys and values,
;; return a new new property list all keys that cause (pred key) to return truish have been removed.
;; Does not modify plist.
(define (plist-delete/pred plist pred)
  (let %plist-delete/pred ((ret '()) (plist plist) (pred pred))
    (cond
      ((null? plist)
        (reverse! ret))
      ((pred (car plist))
        (%plist-delete/pred ret (cddr plist) pred))
      (else
        ;; insert value and key at the beginning of ret
        (%plist-delete/pred (cons (cadr plist) (cons (car plist) ret))
                            (cddr plist)
                            pred)))))



;; create and return a closure that iterates on elements of property list plist.
;;
;; the returned closure accepts no arguments, and each call to it returns three values:
;; either (values key val #t) i.e. the next key and value in plist and #t,
;; or (values #<unspecified> #<unspecified> #f) if end of plist is reached.
(define (in-plist plist)
  (lambda ()
    (if (null? plist)
      (values #f #f)
      (let ((key (car plist))
            (val (cadr plist)))
        (set! plist (cddr plist))
        (values key val #t)))))


;; Iterate in parallel on elements of given property lists plist ..., and evaluate body ... on each element.
;; Stop iterating when the shortest property list is exhausted, and return unspecified value.
;;
;; If no plists are specified, behave as (forever body ...)
(define-syntax for-plist
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((key val plist) ...) body ...)
        (if (null? #'(plist ...))
          #'(forever body ...)
          (with-syntax (((tail ...) (generate-pretty-temporaries #'(plist ...))))
            #'(let %for-plist ((tail plist) ...)
                (if (or (null? tail) ...)
                  (void)
                  (let ((key (car tail)) ...
                        (val (cadr tail)) ...)
                    (with-while-until
                      body ...
                      (%for-plist (cddr tail) ...)))))))))))

) ; close library
