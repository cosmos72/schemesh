;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-builtin sh-builtins sh-find-builtin sh-false sh-true)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (schemesh bootstrap) raise-errorf))


(define (sh-false . ignored-args)
  '(exited . 1))


(define (sh-true . ignored-args)
  (void))


;; execute a builtin. raises exception if specified builtin is not found.
(define (sh-builtin . args)
  (let ((builtin (sh-find-builtin args)))
    (unless builtin
      (raise-errorf 'sh-builtin "~a: not a shell builtin" (if (null? args) "" (car args))))
    (apply builtin (cdr args))))


;; given a command arg-list i.e. a list of strings,
;; extract the first string and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
(define (sh-find-builtin arg-list)
  (if (null? arg-list)
    #f
    (hashtable-ref (sh-builtins) (car arg-list) #f)))

;; function returning the global hashtable name -> builtin
(define sh-builtins
  (let ((t (make-hashtable string-hash string=?)))
    (hashtable-set! t "builtin" sh-builtin)
    (hashtable-set! t "false"   sh-false)
    (hashtable-set! t "true"    sh-true)
    (lambda () t)))

) ; close library
