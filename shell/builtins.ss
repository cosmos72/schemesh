;;; Copyright (C) 2023-2024 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 1))
  (export sh-alias-delete! sh-alias-set! sh-alias-expand sh-aliases
          sh-builtin sh-builtins sh-find-builtin sh-echo sh-error sh-false sh-true sh-history sh-repl-args)
  (import
    (rnrs)
    (only (chezscheme)                    fx1+ include make-thread-parameter void)
    (only (schemesh bootstrap)            debugf raise-errorf)
    (only (schemesh containers misc)      assert-string-list? list-nth string-contains-only-decimal-digits?)
    (schemesh containers bytespan)
    (only (schemesh containers span)      span-iterate)
    (only (schemesh containers charlines) charlines-iterate)
    (schemesh containers utils)
    (only (schemesh posix fd)             fd-write)
    (schemesh lineedit charhistory)
    (only (schemesh lineedit linectx)     linectx? linectx-history)
    (only (schemesh shell fds)            sh-fd-stdout))

(include "shell/aliases.ss")

(define (flush-bytespan-to-fd-stdout bsp)
  ; TODO: loop on short writes and call sh-consume-signals
  (fd-write (sh-fd-stdout) (bytespan-peek-data bsp)
            (bytespan-peek-beg bsp) (bytespan-peek-end bsp))
  (bytespan-clear! bsp))


;; thread parameter (sh-repl-args) must be empty or a list (parser enabled-parsers eval-func print-func lctx)
;; containing arguments of current call to (repl) or (repl*)
(define sh-repl-args
  (make-thread-parameter
    '()
    (lambda (args)
      (unless (list? args)
        (raise-errorf 'sh-repl-args "invalid value, must be a list: " args))
      args)))


;; implementation of "echo" builtin, writes user-specified arguments to file descriptor 1
(define (sh-echo . args)
  (let ((wbuf (make-bytespan 0)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (unless (eq? args tail)
        (bytespan-insert-back/u8! wbuf 32)) ; space
      (bytespan-insert-back/string! wbuf (car tail)))
    (bytespan-insert-back/u8! wbuf 10) ; newline
    (flush-bytespan-to-fd-stdout wbuf))
  (void))


;; implementation of "error" builtin, exits with user-specified exit status
(define (sh-error . args)
  ; (debugf "sh-error ~s~%" args)
  (if (pair? args)
    (let ((arg (car args)))
      (cond
        ((fixnum? arg)
          (if (fxzero? arg)
            (void) ; '(exited . 0) is always abbreviated to (void)
            (cons 'exited arg)))
        ((and (string? arg) (string-contains-only-decimal-digits? arg))
          (let ((num (string->number arg)))
            (if (zero? num)
              (void) ; '(exited . 0) is always abbreviated to (void)
              (cons 'exited num))))
        ((and (pair? arg) (symbol? (car arg)) (or (fixnum? (cdr arg)) (symbol? (cdr arg))))
          arg)
        (#t
          '(exited . 1))))
    '(exited . 1)))


;; implementation of "false" builtin, always exits with failure exit status '(exited . 1)
(define (sh-false . ignored-args)
  '(exited . 1))


;; implementation of "true" builtin, always exits succesfully i.e. with exit status (void)
(define (sh-true . ignored-args)
  (void))


;; ;; implementation of "history" builtin, lists previous commands saved to history
(define (sh-history lctx)
  ; (debugf "sh-history ~s~%" lctx)
  (when (linectx? lctx)
    (let ((wbuf (make-bytespan 0)))
      (span-iterate (linectx-history lctx)
        (lambda (i lines)
          (bytespan-insert-back/u8! wbuf 32) ; space
          (bytespan-display-back/fixnum! wbuf i)
          (bytespan-insert-back/u8! wbuf 9) ; tab
          (charlines-iterate lines
            (lambda (j line)
              (bytespan-insert-back/cbuffer! wbuf line)))
          (bytespan-insert-back/u8! wbuf 10) ; newline
          (when (fx>=? (bytespan-length wbuf) 4096)
            (flush-bytespan-to-fd-stdout wbuf))))
      (flush-bytespan-to-fd-stdout wbuf))))






;; the "echo" builtin
(define (builtin-echo job prog-and-args options)
  (assert-string-list? 'sh-builtin-echo prog-and-args)
  (apply sh-echo (cdr prog-and-args)))


;; the "error" builtin
(define (builtin-error job prog-and-args options)
  (assert-string-list? 'sh-builtin-error prog-and-args)
  (apply sh-error (cdr prog-and-args)))


;; the "false" builtin
(define (builtin-false job prog-and-args options)
  (assert-string-list? 'sh-builtin-false prog-and-args)
  (sh-false))


;; the "true" builtin
(define (builtin-true job prog-and-args options)
  (assert-string-list? 'sh-builtin-true prog-and-args)
  (sh-true))


;; the "history" builtin
(define (builtin-history job prog-and-args options)
  (assert-string-list? 'sh-builtin-history prog-and-args)
  (sh-history (list-nth 3 (sh-repl-args))))



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
;; Return builtin-true if prog-and-args is the empty list.
(define (sh-find-builtin prog-and-args)
  (if (null? prog-and-args)
    builtin-true ; empty command line, run it with (builtin-true)
    (hashtable-ref (sh-builtins) (car prog-and-args) #f)))


;; function returning the global hashtable name -> builtin.
;; Each builtin must be a function accepting as arguments:
;;   a job (actually a cmd)
;;   a prog-and-args i.e. a list of strings containing the builtin name and its arguments
;;   a list of options
;; and must execute the builtin then return its exit status
(define sh-builtins
  (let ((t (make-hashtable string-hash string=?)))
    (hashtable-set! t "alias"   builtin-alias)
    (hashtable-set! t "builtin" sh-builtin)
    (hashtable-set! t "echo"    builtin-echo)
    (hashtable-set! t "error"   builtin-error)
    (hashtable-set! t "false"   builtin-false)
    (hashtable-set! t "history" builtin-history)
    (hashtable-set! t "true"    builtin-true)
    (hashtable-set! t "unalias" builtin-unalias)
    (lambda () t)))

) ; close library
