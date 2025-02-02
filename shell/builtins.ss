;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

(library (schemesh shell builtins (0 7 1))
  (export sh-builtins sh-find-builtin sh-exception-handler
          sh-echo sh-error sh-false sh-history sh-repl-args sh-true)
  (import
    (rnrs)
    (only (chezscheme)           console-error-port debug debug-condition debug-on-exception
                                 display-condition reset-handler void)
    (only (schemesh bootstrap)      sh-make-thread-parameter raise-errorf)
    (schemesh containers bytespan)
    (only (schemesh containers charlines) charlines-iterate)
    (only (schemesh containers gbuffer)   gbuffer-iterate)
    (only (schemesh containers hashtable) hashtable-iterate)
    (only (schemesh containers misc)      assert-string-list? list-iterate string-contains-only-decimal-digits?)
    (only (schemesh containers sort)      span-sort!)
    (only (schemesh containers span)      span span-insert-back! span-iterate)
    (schemesh containers utils)
    (only (schemesh posix fd)             fd-write)
    (schemesh lineedit charhistory)
    (only (schemesh lineedit linectx)     linectx? linectx-history)
    (only (schemesh shell fds)            sh-fd-stdout sh-fd-stderr))


;; copy-pasted from shell/builtins2.ss
;;
;; write contents of bytespan bsp to file descriptor fd,
;; then clear bytespan bsp
(define (fd-write/bspan! fd bsp)
  ; TODO: loop on short writes and call sh-consume-signals
  (fd-write fd (bytespan-peek-data bsp)
            (bytespan-peek-beg bsp) (bytespan-peek-end bsp))
  (bytespan-clear! bsp))


;; thread parameter (sh-repl-args) must be empty or a list
;;   (parser enabled-parsers eval-func lctx init-file-path quit-file-path)
;; containing arguments of current call to (sh-repl) or (sh-repl*)
(define sh-repl-args
  (sh-make-thread-parameter
    '()
    (lambda (args)
      (unless (list? args)
        (raise-errorf 'sh-repl-args "invalid value, must be a list: " args))
      args)))


;; React to uncaught conditions
(define sh-exception-handler
  (case-lambda
    ((obj proc-after-body)
      (let ((port (console-error-port)))
        (dynamic-wind
          (lambda () ; before body
            (put-string port "; ")
            (display-condition obj port)
            (newline port)
            (flush-output-port port))
          (lambda () ; body
            (when (or (serious-condition? obj) (not (warning? obj)))
              (debug-condition obj) ;; save obj into thread-parameter (debug-condition)
              (if (debug-on-exception)
                (debug)
                (put-string port "; type (debug) to enter the debugger, or (debug-condition) to retrieve the condition.\n"))
              (flush-output-port port)))
          proc-after-body)))
    ((obj)
      (sh-exception-handler obj void))))


;; implementation of "echo" builtin, writes user-specified arguments to file descriptor 1.
;; separator between arguments is #\space
;; terminating character after arguments is #\newline
(define (sh-echo . args)
  (let ((wbuf (make-bytespan 0))
        (fd   (sh-fd-stdout)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (unless (eq? args tail)
        (bytespan-insert-back/u8! wbuf 32)) ; space
      (bytespan-insert-back/string! wbuf (car tail))
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bspan! fd wbuf)))
    (bytespan-insert-back/u8! wbuf 10) ; newline
    (fd-write/bspan! fd wbuf))
  (void))


;; implementation of "echo0" builtin, writes user-specified arguments to file descriptor 1
;; separator between arguments is #\nul
;; terminating character after arguments is #\nul
(define (sh-echo0 . args)
  (let ((wbuf (make-bytespan 0))
        (fd   (sh-fd-stdout)))
    (do ((tail args (cdr tail)))
        ((null? tail))
      (bytespan-insert-back/string! wbuf (car tail))
      (bytespan-insert-back/u8! wbuf 0) ; #\nul
      (when (fx>=? (bytespan-length wbuf) 4096)
        (fd-write/bspan! fd wbuf)))
    (fd-write/bspan! fd wbuf))
  (void))


;; implementation of "error" builtin, exits with user-specified exit status
(define (sh-error . args)
  ; (debugf "sh-error ~s" args)
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
(define (sh-history)
  (let ((lctx (list-ref (sh-repl-args) 2))
        (fd   (sh-fd-stdout)))
    ; (debugf "sh-history ~s" lctx)
    (if (linectx? lctx)
      (let ((wbuf (make-bytespan 0)))
        (gbuffer-iterate (linectx-history lctx)
          (lambda (i lines)
            (bytespan-insert-back/u8! wbuf 32) ; space
            (bytespan-display-back/fixnum! wbuf i)
            (bytespan-insert-back/u8! wbuf 9) ; tab
            (charlines-iterate lines
              (lambda (j line)
                (bytespan-insert-back/cbuffer! wbuf line)))
            (bytespan-insert-back/u8! wbuf 10) ; newline
            (when (fx>=? (bytespan-length wbuf) 4096)
              (fd-write/bspan! fd wbuf))))
        (fd-write/bspan! fd wbuf)
        (void)) ; return (void), means builtin exited succesfully
      '(exited . 1))))



;; the "echo" builtin
(define (builtin-echo job prog-and-args options)
  (assert-string-list? 'builtin-echo prog-and-args)
  (apply sh-echo (cdr prog-and-args)))


;; the "echo0" builtin
(define (builtin-echo0 job prog-and-args options)
  (assert-string-list? 'builtin-echo0 prog-and-args)
  (apply sh-echo0 (cdr prog-and-args)))


;; the "error" builtin
(define (builtin-error job prog-and-args options)
  (assert-string-list? 'builtin-error prog-and-args)
  (apply sh-error (cdr prog-and-args)))


;; the "false" builtin
(define (builtin-false job prog-and-args options)
  (assert-string-list? 'builtin-false prog-and-args)
  (sh-false))


;; the "true" builtin
(define (builtin-true job prog-and-args options)
  (assert-string-list? 'builtin-true prog-and-args)
  (sh-true))


;; the "history" builtin
(define (builtin-history job prog-and-args options)
  (assert-string-list? 'builtin-history prog-and-args)
  (sh-history))


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
    (hashtable-set! t ":"       builtin-true)
    (hashtable-set! t "echo"    builtin-echo)
    (hashtable-set! t "echo0"   builtin-echo0)
    (hashtable-set! t "error"   builtin-error)
    (hashtable-set! t "false"   builtin-false)
    (hashtable-set! t "history" builtin-history)
    (hashtable-set! t "true"    builtin-true)
    (lambda () t)))

) ; close library
