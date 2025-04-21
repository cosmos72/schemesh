;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh bootstrap arrow (0 8 3))
  (export expand?=>)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! format
                       fx1+ fx1- fx/ gensym list-copy list-head))


;; scan template for '_ and replace '_ with item.
;; if template contains no '_ then append item to template.
;;
;; return modified of template.
(define (replace_! item template)
  (let ((place (memq '_ template)))
    (if place
      (begin
        (set-car! place item)
        template)
      (append! template (list item)))))


;; helper function used by expand?=>
;;
;; traverse list, find first element eq? to '=> or '?=> and return two values:
;;  its position in the list and the symbol found,
;;  or #f #f if no such element was found
(define (scan=> l)
  (let %scan=> ((l l) (pos 0))
    (cond
      ((null? l)
        (values #f #f))
      ((memq (car l) '(=> ?=>))
        (values pos (car l)))
      (else
        (%scan=> (cdr l) (fx1+ pos))))))


;; set the n-th car of a list
(define (list-set! l n obj)
  (set-car! (list-tail l n) obj))


;; expand (=> head rest)
(define (compose=> head rest)
  (let-values (((pos sym) (scan=> rest)))
    (if pos
      (let* ((mid  (list-head rest pos))
             (tail (list-tail rest (fx1+ pos)))
             (mid* (replace_! head mid)))
        (if (eq? sym '=>)
          (compose=> mid* tail)
          (compose?=> mid* tail)))
      (replace_! head (list-copy rest)))))


;; expand (?=> head rest)
(define (compose?=> head rest)
  (let-values (((pos sym) (scan=> rest)))
    (if pos
      (let* ((mid  (list-head rest pos))
             (tail (list-tail rest (fx1+ pos)))
             (g    (gensym))
             (mid* (replace_! g mid)))
        (if (eq? sym '=>)
          `(let ((,g ,head))
             (and ,g ,(compose=> mid* tail)))
          `(let ((,g ,head))
             (and ,g ,(compose?=> mid* tail)))))
      (let* ((g     (gensym))
             (rest* (replace_! g (list-copy rest))))
         `(let ((,g ,head))
            (and ,g ,rest*))))))




;; implementation of macro ?=>
(define (expand?=> l)
  (when (null? l)
    (syntax-violation "" "invalid syntax, need at least one argument after" '?=>))
  (let-values (((pos sym) (scan=> l)))
    (case sym
      ((=>)
        (compose=> (list-head l pos) (list-tail l (fx1+ pos))))
      ((?=>)
        (compose?=> (list-head l pos) (list-tail l (fx1+ pos))))
      (else
        l))))




) ; close library
