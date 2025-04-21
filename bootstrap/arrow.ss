;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

#!r6rs

(library (schemesh bootstrap arrow (0 8 3))
  (export ->compose ->expand)
  (import
    (rnrs)
    (rnrs mutable-pairs)
    (only (chezscheme) append! format
                       fx1+ fx1- fx/ gensym list-copy list-head))



;; helper function used by ->
;; traverse list, find element eq? to key, and return its position in the list as an exact integer.
;; return #f if no element eq? to key was found
(define (list-index/eq l key)
  (let %list-index ((l l) (key key) (pos 0))
    (cond
      ((null? l)         #f)
      ((eq? key (car l)) pos)
      (else              (%list-index (cdr l) key (fx1+ pos))))))


;; set the n-th car of a list
(define (list-set! l n obj)
  (set-car! (list-tail l n) obj))


;; helper function used by ->expand
(define (->compose head arrow? tail)
  ;; (format #t "->compose head=~s arrow?=~s tail=~s\n" head arrow? tail)
  (let* ((tail (list-copy tail))
         (pos1 (list-index/eq tail '_))
         (pos2 (list-index/eq tail '->))
         (pos3 (list-index/eq tail '?>))
         (ret  tail))
    (cond
      ((and pos1 (or (not pos2) (fx<? pos1 pos2))
                 (or (not pos3) (fx<? pos1 pos3)))
        ;; found _ before next -> or ?>
        (if arrow?
          (list-set! tail pos1 head)
          (let ((g (gensym)))
            (list-set! tail pos1 g)
            (set! ret `(let ((,g ,head))
                         (and ,g ,tail))))))
      ((and pos2 (or (not pos3) (fx<? pos2 pos3)))
        ;; found -> before next _ or ?>
        (let ((splice (list-tail tail pos2)))
          (list-set! tail pos2 (cons head splice))))
      (pos3
        ;; found ?> before next _ or ->
        (let ((splice (list-tail tail pos3)))
          (list-set! tail pos3 (cons head splice))))
      (else
        ;; next -> or ?> not found
        (append! tail (list head))))
    (if (or pos2 pos3)
      (cons '-> ret) ; ret contains further -> that must be processed
      ret)))


;; implementation of macro ->
(define (->expand l)
  (when (null? l)
    (syntax-violation "" "invalid syntax, need at least one argument after" '->))
  (let* ((pos1   (list-index/eq l '->))
         (pos2   (list-index/eq l '?>))
         (arrow? (and pos1 (or (not pos2) (fx<? pos1 pos2))))
         (pos    (if arrow? pos1 pos2)))
    (if pos
      (->compose (list-head l pos) arrow? (list-tail l (fx1+ pos)))
      l)))




) ; close library
