;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh repl (0 1))
  (export repl-lineedit repl-parse repl-eval repl-eval-list repl repl* repl-interrupt-handler)
  (import
    (rnrs)
    (only (chezscheme)
      abort base-exception-handler break-handler console-input-port console-output-port
      eval exit-handler inspect keyboard-interrupt-handler parameterize pretty-print
      read-token reset reset-handler void)
    (schemesh bootstrap)
    (only (schemesh containers) list-iterate)
    (schemesh lineedit io)
    (schemesh lineedit linectx)
    (schemesh lineedit)
    (schemesh parser)
    (schemesh posix signal)
    (schemesh posix tty)
    (only (schemesh shell) sh-consume-sigchld sh-make-linectx))

;
; Read user input.
; If user pressed ENTER, return textual input port containing entered text.
;
; Returns:
; #f if got end-of-file
; #t if waiting for more keypresses
; a textual input port if user pressed ENTER.
(define (repl-lineedit lctx)
  (let ((ret (lineedit-read lctx -1)))
    (if (boolean? ret)
      ret
      (open-charlines-input-port ret))))

;
; Parse user input.
; Arguments:
;   pctx - a parsectx containing textual input port to parse, its position
;          and a hashtable of enabled parsers (can be #f)
;   initial-parser - initial parser to use: a symbol or parser
;
; Automatically switches to other parsers if a directive #!... is found in a (possibly
; nested) list being parsed.
;
; Return two values:
;   list of forms containing Scheme code to evaluate,
;   and updated parser to use.
(define repl-parse parse-forms)


; Eval a single form containing parsed expressions or shell commands,
; and return value or exit status of executed form.
; May return multiple values.
;
; Note: if a form in list is (shell ...), which would create a job but NOT run it,
;       eval instead (sh-run (shell ...)) that also runs the job.
;
; This has two effects:
; 1. when using shell parser, top-level commands will be executed immediately.
; 2. when using scheme parser, top-level (shell ...) will be executed immediately.
(define (repl-eval form)
  ; (format #t "; evaluating: ~s~%" form)
  (try
    (eval
      (if (and (pair? form) (memq (car form) '(shell shell-list)))
        (list 'sh-run form)
        form))
    (catch (cond)
      ; (format #t "repl-eval handling condition ~s~%" cond)
      ((base-exception-handler) cond))))

;
; Execute with (eval-func form) each form in list of forms containing parsed expressions
; or shell commands, and return value or exit status of last form in list.
; May return multiple values.
;
; Implementation note:
;   some procedures we may eval, as (break) (debug) (inspect) ...
;   expect the tty to be in canonical mode, not in raw mode.
;
;   Also, we need tty to be in canonical mode for CTRL+C to generate SIGINT,
;   which causes Chez Scheme to suspend long/infinite evaluations
;   and call (break) - a feature we want to preserve.
;
;   For these reasons, the loop (do ... (eval-func ...))
;   is wrapped inside (dynamic-wind tty-restore! (lambda () ...) tty-setraw!)
(define (repl-eval-list forms eval-func)
  ; (format #t "; evaluating list: ~s~%" forms)
  (unless (null? forms)
    (dynamic-wind
      tty-restore!
      (lambda ()
        (do ((tail forms (cdr tail)))
            ((null? (cdr tail))
              (eval-func (car tail)))
          (eval-func (car tail))))
      tty-setraw!)))


; Print values or exit statuses.
(define (repl-print . values)
  (list-iterate values
    (lambda (value)
      (unless (eq? (void) value)
        (pretty-print value)))))

; Parse and execute user input.
; Calls in sequence (repl-lineedit) (repl-parse) (repl-eval-list) and (repl-print)
;
; Returns updated parser to use, or #f if got end-of-file.
(define (repl-once lctx initial-parser enabled-parsers eval-func)
  (linectx-parser-name-set! lctx (parser-name initial-parser))
  (linectx-parsers-set! lctx enabled-parsers)
  (let ((in (repl-lineedit lctx)))
    (case in
      ((#f) #f)             ; got end-of-file
      ((#t) initial-parser) ; nothing to execute: waiting for more user input
      (else
        (let-values (((form updated-parser)
                        (repl-parse (make-parsectx* in enabled-parsers
                                                    (linectx-width lctx)
                                                    (linectx-prompt-end-x lctx)
                                                    0 0)
                                    initial-parser)))
          (unless (eq? (void) form)
            (call-with-values
              (lambda () (repl-eval-list form eval-func))
              repl-print))
          updated-parser)))))


; main loop of (repl) and (repl*)
(define (repl-loop parser enabled-parsers eval-func lctx)
  (call/cc
    (lambda (k-exit)
      (parameterize ((break-handler (lambda args (repl-interrupt-handler lctx args)))
                     (exit-handler k-exit)
                     (keyboard-interrupt-handler
                       (lambda ()
                         (put-string (console-output-port) "\ninterrupted\n")
                         (repl-interrupt-handler lctx '())))
                     (reset-handler (reset-handler)))
        (let ((k-reset k-exit)
              (updated-parser parser))
          (reset-handler (lambda () (k-reset)))
          (call/cc (lambda (k) (set! k-reset k)))
          ; when the (reset-handler) we installed is called, resume from here
          (while updated-parser
            (set! parser updated-parser)
            (set! updated-parser (repl-once lctx parser enabled-parsers eval-func))
            (sh-consume-sigchld))))))
  lctx)

;
; top-level interactive repl with all arguments mandatory
; Returns linectx, usable for further calls to (repl) or (repl*)
(define (repl* initial-parser enabled-parsers eval-func lctx)
  ; (to-parser) also checks initial-parser's and enabled-parser's validity
  (let ((parser (to-parser enabled-parsers initial-parser 'repl)))
    (assert (procedure? eval-func))
    (assert (linectx? lctx))
    (dynamic-wind
      (lambda ()
        (lineedit-clear! lctx) (signal-init-sigwinch) (tty-setraw!))
      (lambda ()
        (repl-loop parser enabled-parsers eval-func lctx))
      (lambda ()
        (tty-restore!) (signal-restore-sigwinch) (lineedit-finish lctx)))))

;
; top-level interactive repl with optional arguments:
; 'parser initial-parser   - defaults to 'shell
; 'parsers enabled-parsers - defaults to (parsers)
; 'eval eval-func          - defaults to repl-eval
; 'linectx lctx            - defaults to (sh-make-linectx)
;
; Returns linectx, usable for further calls to (repl)
(define (repl . args)
  (let ((initial-parser #f)  (initial-parser? #f)
        (enabled-parsers #f) (enabled-parsers? #f)
        (eval-func #f)       (eval-func? #f)
        (lctx #f)             (lctx? #f))
    (do ((args-left args (cddr args)))
        ((null? args-left))
      (assert (not (null? (cdr args-left))))
      (let ((opt (car args-left))
            (val (cadr args-left)))
        (case opt
          ((parser)  (set! initial-parser val)  (set! initial-parser? #t))
          ((parsers) (set! enabled-parsers val) (set! enabled-parsers? #t))
          ((eval)    (set! eval-func val)       (set! eval-func? #t))
          ((linectx) (set! lctx val)             (set! lctx? #t))
          (else     (syntax-violation 'repl "unexpected argument:" opt)))))
    (repl* (if initial-parser?  initial-parser 'shell)
           (if enabled-parsers? enabled-parsers (parsers))
           (if eval-func? eval-func repl-eval)
           (if lctx? lctx (sh-make-linectx)))))


;; React to keyboard CTRL+C and calls to (break): enter the debugger.
(define (repl-interrupt-handler lctx args)
  (call/cc
    (lambda (k)
      (parameterize ((break-handler void) (keyboard-interrupt-handler void))
        (repl-interrupt-show-who-msg-irritants args)
        (while (repl-interrupt-handler-once lctx k (console-output-port)))))))


;; Print (break ...) arguments
(define (repl-interrupt-show-who-msg-irritants args)
  (when (pair? args)
    (let* ((who  (car args))
           (tail (cdr args))
           (msg  (if (pair? tail) (car tail) ""))
           (irritants (if (pair? tail) (cdr tail) '()))
           (out  (console-output-port)))
     (put-string out "break in " )
     (put-datum  out who)
     (put-string out ": ")
     (put-string out msg)
     (list-iterate irritants
       (lambda (value)
         (put-char   out #\space)
         (put-datum  out value)))
     (put-char   out #\newline))))


;; Single iteration of (repl-interrupt-handler)
(define (repl-interrupt-handler-once lctx k out)
  (put-string out "break> ")
  (flush-output-port out)
  (case (let-values (((type token start end) (read-token (console-input-port))))
          (cond
            ((eq? 'eof type)
              (put-char out #\newline)
              (flush-output-port out)
              'exit)
            (else token)))
    ((a abort)        (abort) #f)
    ((e exit)         #f)
    ((i inspect)      (inspect k) #t)
    ((n new-repl)     (repl) #t)
    ((r q reset quit) (reset) #f)
    ((? help)
      (put-string out "
Type e to exit interrupt handler and continue
     r or q to reset scheme
     a to abort scheme
     n to enter new repl
     i to inspect current continuation\n\n")
      (flush-output-port out)
      #t)
    (else (put-string out "Invalid command.  Type ? for options.\n")
      (flush-output-port out)
      #t)))

) ; close library
