;;; Copyright (C) 2023-2026 by Massimiliano Ghilardi, all rights reserved
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


;;; Macro: ==>
;;;
;;; Purpose:
;;;   DSL for chaining/threading multiple functions or macro invocations.
;;;
;;; Aux keywords:
;;;   =>     : unconditional chaining
;;;   ?=>    : conditional chaining
;;;   _      : placeholder for inserting an expression into a template
;;;
;;; Description:
;;;   ==> must be followed by one or more function or macro invocations.
;;;
;;;   Each functions or macro invocation is **not** enclosed in parentheses:
;;;   it is instead delimited by the keywords => ?=>
;;;
;;;   The call to each function or macro is inserted literally
;;;   into the next one, before the first argument of the next function or macro or,
;;;   if present, at the position of placeholder _
;;;
;;; Examples:
;;;   (==> a foo)                      expands to (a foo)
;;;
;;;   (==> a foo => b)                 expands to (b (a foo))
;;;
;;;   (==> a => b bar)                 expands to (b (a) bar)
;;;
;;;   (==> a => b _ bar)               expands to (b (a) bar) i.e. identical to previous one
;;;
;;;   (==> a => b bar _)               expands to (b bar (a))
;;;
;;;   (==> a foo => b bar)             expands to (b (a foo) bar)
;;;
;;;   (==> a foo => b bar => c baz)    expands to (c (b (a foo) bar) baz)
;;;
;;;   (==> a foo => b bar _ => c baz)  expands to (c (b bar (a foo)) baz)
;;;
;;; Keyword ?=> adds short-circuit logic,
;;; i.e. if the function call at its left evaluates to #f,
;;; does not execute the rest of the chain and evaluates to #f.
;;;
;;; Examples:
;;;   (==> a ?=> b)       expands to (let ((tmp (a)))
;;;                                    (and tmp (b tmp)))
;;;
;;;   (==> a ?=> b ?=> c) expands to (let ((tmp-a (a)))
;;;                                    (and tmp-a
;;;                                         (let ((tmp-b (b tmp-a)))
;;;                                           (and tmp-b (c tmp-b)))))
;;;
;;; The three keywords => ?=> _ can be used simultaneously.
;;; Example:
;;;   (==> a foo ?=> b bar _ => c)
;;; expands to
;;;   (let ((tmp-a (a foo)))
;;;     (and tmp-a
;;;          (c (b bar tmp-a))))
;;;
;;; Note: the keywords => ?=> _ are recognized by symbol eq?, i.e. the identifiers
;;;       in user-provided form are compared with eq? against the symbols => ?=> _
;;;       This means macro ==> **ignores** any definition for the symbols => ?=> _
;;;
;;; The keywords => ?=> _ are recognized **only** if they appear at top level.
;;; They are **not** recognized if they appear inside parentheses.
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


;; Racket-compatible threading:
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
;;
;; Also see the more general macro ==>
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
