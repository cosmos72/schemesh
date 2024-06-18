;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-builtin sh-builtin-false sh-builtin-true
          sh-builtins sh-find-builtin sh-false sh-true)
  (import
    (rnrs)
    (only (chezscheme) void)
    (only (schemesh bootstrap) raise-errorf)
    (only (schemesh containers misc) assert-string-list?)
    (schemesh shell aliases))


(define (sh-false . ignored-args)
  '(exited . 1))


(define (sh-true . ignored-args)
  (void))


;; the "false" builtin
(define (sh-builtin-false job prog-and-args options)
  (assert-string-list? 'sh-builtin-false prog-and-args)
  (sh-false))


;; the "true" builtin
(define (sh-builtin-true job prog-and-args options)
  (assert-string-list? 'sh-builtin-true prog-and-args)
  (sh-true))


;; the "builtin" builtin: execute a builtin. raises exception if specified builtin is not found.
(define (sh-builtin cmd prog-and-args options)
  ; (debugf "sh-builtin ~s~%" prog-and-args)
  (assert-string-list? 'sh-builtin prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void)
    (let* ((args (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (unless builtin
        (raise-errorf 'sh-builtin "~a: not a shell builtin" (car args)))
      (builtin cmd args options))))


;; given a command line prog-and-args i.e. a list of strings,
;; extract the first string and return the corresponding builtin.
;; Return #f if no corresponding builtin is found.
(define (sh-find-builtin prog-and-args)
  (if (null? prog-and-args)
    #f
    (hashtable-ref (sh-builtins) (car prog-and-args) #f)))


;; function returning the global hashtable name -> builtin.
;; Each builtin must be a function accepting as arguments:
;;   a job (actually a cmd)
;;   an prog-and-args i.e. a list of strings containing the builtin name and its arguments
;;   a list of options
;; and returning the job status
(define sh-builtins
  (let ((t (make-hashtable string-hash string=?)))
    (hashtable-set! t "alias"   sh-builtin-alias)
    (hashtable-set! t "builtin" sh-builtin)
    (hashtable-set! t "false"   sh-builtin-false)
    (hashtable-set! t "true"    sh-builtin-true)
    (hashtable-set! t "unalias" sh-builtin-unalias)
    (lambda () t)))

) ; close library
