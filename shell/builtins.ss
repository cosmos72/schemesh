;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-builtin sh-builtin-echo sh-builtin-false sh-builtin-true
          sh-builtins sh-find-builtin sh-echo sh-false sh-true)
  (import
    (rnrs)
    (only (chezscheme)               void)
    (only (schemesh bootstrap)       raise-errorf)
    (only (schemesh containers misc) assert-string-list?)
    (schemesh containers bytespan)
    (schemesh containers utils)
    (only (schemesh posix fd)        fd-write)
    (only (schemesh shell fds)       sh-fd-stdout)
    (schemesh shell aliases))


(define (sh-echo . args)
  (let ((wbuf (make-bytespan 0)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (unless (eq? args tail)
        (bytespan-insert-back/u8! wbuf 32)) ; space
      (bytespan-insert-back/string! wbuf (car tail)))
    (bytespan-insert-back/u8! wbuf 10) ; newline
    ; TODO: loop on short writes
    (fd-write (sh-fd-stdout) (bytespan-peek-data wbuf)
              (bytespan-peek-beg wbuf) (bytespan-peek-end wbuf)))
  (void))


(define (sh-false . ignored-args)
  '(exited . 1))


(define (sh-true . ignored-args)
  (void))


;; the "echo" builtin
(define (sh-builtin-echo job prog-and-args options)
  (assert-string-list? 'sh-builtin-echo prog-and-args)
  (apply sh-echo (cdr prog-and-args)))


;; the "false" builtin
(define (sh-builtin-false job prog-and-args options)
  (assert-string-list? 'sh-builtin-false prog-and-args)
  (sh-false))


;; the "true" builtin
(define (sh-builtin-true job prog-and-args options)
  (assert-string-list? 'sh-builtin-true prog-and-args)
  (sh-true))


;; the "builtin" builtin: find and execute a builtin.
;; raises exception if specified builtin is not found.
(define (sh-builtin job prog-and-args options)
  ; (debugf "sh-builtin ~s~%" prog-and-args)
  (assert-string-list? 'sh-builtin prog-and-args)
  (if (or (null? prog-and-args) (null? (cdr prog-and-args)))
    (void)
    (let* ((args (cdr prog-and-args))
           (builtin (sh-find-builtin args)))
      (unless builtin
        (raise-errorf 'sh-builtin "~a: not a shell builtin" (car args)))
      (builtin job args options))))


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
    (hashtable-set! t "echo"    sh-builtin-echo)
    (hashtable-set! t "false"   sh-builtin-false)
    (hashtable-set! t "true"    sh-builtin-true)
    (hashtable-set! t "unalias" sh-builtin-unalias)
    (lambda () t)))

) ; close library
