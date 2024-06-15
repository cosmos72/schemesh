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
  (let ((t (make-hashtable equal-hash equal?)))
    (hashtable-set! t (string->utf8 "false\x0;") sh-false)
    (hashtable-set! t (string->utf8 "true\x0;")  sh-true)
    (lambda () t)))

;; given a command argv i.e. a vector of bytevector0,
;; extract the first vector element and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
(define (sh-find-builtin argv)
  (if (fxzero? (vector-length argv))
    #f
    (hashtable-ref (sh-builtins) (vector-ref argv 0) #f)))

) ; close library
