;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-builtins sh-find-builtin sh-false sh-true)
  (import
    (rnrs)
    (only (chezscheme) void))


(define (sh-false . ignored-args)
  '(exited . 1))


(define (sh-true . ignored-args)
  (void))


(define sh-builtins
  (let ((t (make-hashtable string-hash string=?)))
    (hashtable-set! t "false" sh-false)
    (hashtable-set! t "true"  sh-true)
    (lambda () t)))

;; given a command arg-list i.e. a list of strings,
;; extract the first string and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
(define (sh-find-builtin arg-list)
  (if (null? arg-list)
    #f
    (hashtable-ref (sh-builtins) (car arg-list) #f)))

) ; close library
