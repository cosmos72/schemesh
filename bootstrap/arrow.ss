;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.

#!r6rs

(library (scheme2k bootstrap arrow (0 9 3))
  (export ==> ~>)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) fx1+ gensym list-copy list-head))



;; Simplify procedure chaining, allows writing (==> proc1 a => proc2 b _ c => proc3 d ...)
;; instead of nested calls: (proc3 (proc2 b (proc1 a) c) d ...)
;;
;; Replaces the placeholder _ with the previous form.
;;
;; If the placeholder _ is not present, the previous form is inserted as first argument.
;; Example:
;;   (==> proc1 a => proc2 b c)
;; expands to
;;   (proc2 (proc1 a) b c)
;;
;; If ?=> is used instead of => then evaluation stops when the expression before ?=> evaluates to #f
(define-syntax ==>
  (lambda (stx)
    (syntax-case stx ()
      ((xname args ...)
        (letrec

          ;; traverse list, find first identifier whose syntax->datum is eq? to sym0 or sym1 and return two values:
          ;;  if sym0 appears first, return its position in the list and 0,
          ;;  if sym1 appears first, return its position in the list and 1,
          ;;  otherwise #f #f
          ((scan=> (lambda (l sym0 sym1)
            (let %scan=> ((l l) (pos 0))
              (cond
                ((null? l)
                  (values #f #f))
                ((and (identifier? (car l)) (eq? (syntax->datum (car l)) sym0))
                  (values pos 0))
                ((and (identifier? (car l)) (eq? (syntax->datum (car l)) sym1))
                  (values pos 1))
                (else
                  (%scan=> (cdr l) (fx1+ pos)))))))


          ;; scan template for #'_ and replace it with item.
          ;; if template contains no #'_ then insert item into template as first argument
          ;;
          ;; return template, modified in-place
          (replace_! (lambda (item template)
            (let-values (((pos dummy) (scan=> template '_ '_)))
              (if pos
                (begin
                  (set-car! (list-tail template pos) item)
                  template)
                (cons (car template) (cons item (cdr template)))))))


          ;; expand (=> head rest)
          (compose=> (lambda (k head rest)
            (let-values (((pos sym) (scan=> rest '=> '?=>)))
              (if pos
                (let* ((mid  (list-head rest pos))
                       (mid* (replace_! head mid))
                       (tail (list-tail rest (fx1+ pos))))
                  (if (fxzero? sym)
                    (compose=>  k mid* tail)
                    (compose?=> k mid* tail)))
                (replace_! head (list-copy rest))))))


          ;; expand (?=> head rest)
          (compose?=> (lambda (k head rest)
            (let-values (((pos sym) (scan=> rest '=> '?=>)))
              (if pos
                (let* ((g    (datum->syntax k (gensym)))
                       (mid  (list-head rest pos))
                       (mid* (replace_! g mid))
                       (tail (list-tail rest (fx1+ pos))))
                  #`(let ((#,g #,head))
                      (and #,g #,(if (fxzero? sym)
                                   (compose=>  k mid* tail)
                                   (compose?=> k mid* tail)))))
                (let* ((g     (datum->syntax k (gensym)))
                       (rest* (replace_! g (list-copy rest))))
                   #`(let ((#,g #,head))
                       (and #,g #,rest*)))))))


          ;; implementation of macro ==>
          (expand==> (lambda (k l)
            (when (null? l)
              (syntax-violation "" "invalid syntax, need at least one argument after" '==>))
            (let-values (((pos sym) (scan=> l '=> '?=>)))
              (case sym
                ((0)
                  (compose=> k (list-head l pos) (list-tail l (fx1+ pos))))
                ((1)
                  (compose?=> k (list-head l pos) (list-tail l (fx1+ pos))))
                (else
                  l))))))

        ;; finally, the macro ==> definition
        (expand==> #'xname #'(args ...)))))))


;; Racket-compatible threading arrow:
;;
;; Simplify procedure chaining, allows writing (~> (proc1 a) (proc2 b _ c) (proc3 d ...))
;; instead of nested calls: (proc3 (proc2 b (proc1 a) c) d ...)
;;
;; Replaces the placeholder _ with the previous form.
;;
;; If the placeholder _ is not present, the previous form is inserted as first argument.
;; Example:
;;   (~> (proc1 a) (proc2 b c))
;; expands to
;;   (proc2 (proc1 a) b c)
(define-syntax ~>
  (lambda (stx)
    (syntax-case stx ()
      ((_ (proc expr ...) ...)
        (letrec
          ((inner-expand~> (lambda (l)
             (if (null? l)
               '()
               (append (list #'=>) (car l) (inner-expand~> (cdr l))))))

           (expand~> (lambda (l)
             (when (null? l)
               (syntax-violation "" "invalid syntax, need at least one argument after" '~>))
             (append (list #'==>) (car l) (inner-expand~> (cdr l))))))

          (expand~> #'((proc expr ...) ...)))))))


) ; close library
