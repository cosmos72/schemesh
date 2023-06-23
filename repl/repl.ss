;;; Copyright (C) 2023 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh repl (0 1))
  (export repl-lineedit repl-parse repl-eval repl-eval-list repl repl* )
  (import
    (rnrs)
    (only (chezscheme)
      base-exception-handler debug eval exit-handler inspect
      parameterize pretty-print reset-handler void)
    (schemesh bootstrap)
    (only (schemesh containers) list-iterate)
    (schemesh io)
    (schemesh lineedit)
    (schemesh parser)
    (schemesh posix signal)
    (schemesh posix tty)
    (only (schemesh shell) sh-consume-sigchld sh-expand-ps1 sh-autocomplete))

;
; Read user input.
; If user pressed ENTER, return textual input port containing entered text.
;
; Returns:
; #f if got end-of-file
; #t if waiting for more keypresses
; a textual input port if user pressed ENTER.
(define (repl-lineedit ctx)
  (let ((ret (lineedit-read ctx -1)))
    (if (boolean? ret)
      ret
      (open-gbuffer-of-chargbuffers-input-port ret))))

;
; Parse user input.
; First argument is a textual input stream containing user input.
; Second argument is initial parser to use, or a symbol containing the parser's name.
; Third argument is #f or a hashtable containing enabled parsers.
;
; Automatically switches to other parsers if a directive #!... is found in a (possibly
; nested) list being parsed.
;
; Returns two values: parsed forms, and update parser to use.
; Arguments:
;   in - textual input stream
;   initial-parser - initial parser to use: a symbol or parser
;   enabled-parsers - #f or hashtable of enabled parsers
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
(define (repl-once ctx initial-parser enabled-parsers eval-func)
  (linectx-parser-name-set! ctx (parser-name initial-parser))
  (let ((in (repl-lineedit ctx)))
    (case in
      ((#f) #f)             ; got end-of-file
      ((#t) initial-parser) ; nothing to execute: waiting for more user input
      (else
        (let-values (((form updated-parser)
                        (repl-parse in initial-parser enabled-parsers)))
          (unless (eq? (void) form)
            (call-with-values
              (lambda () (repl-eval-list form eval-func))
              repl-print))
          updated-parser)))))


; main loop of (repl) and (repl*)
(define (repl-loop parser enabled-parsers eval-func ctx)
  (call/cc
    (lambda (k-exit)
      (parameterize ((exit-handler k-exit) (reset-handler (reset-handler)))
        (let ((k-reset k-exit)
              (updated-parser parser))
          (reset-handler (lambda () (k-reset)))
          (call/cc (lambda (k) (set! k-reset k)))
          ; when the (reset-handler) we installed is called, resume from here
          (while updated-parser
            (set! parser updated-parser)
            (set! updated-parser (repl-once ctx parser enabled-parsers eval-func))
            (sh-consume-sigchld))))))
  ctx)

;
; top-level interactive repl with all arguments mandatory
; Returns linectx, usable for further calls to (repl) or (repl*)
(define (repl* initial-parser enabled-parsers eval-func ctx)
  ; (to-parser) also checks initial-parser's and enabled-parser's validity
  (let ((parser (to-parser initial-parser enabled-parsers 'repl)))
    (assert (procedure? eval-func))
    (assert (linectx? ctx))
    (dynamic-wind
      (lambda ()
        (lineedit-clear! ctx) (signal-init-sigwinch) (tty-setraw!))
      (lambda ()
        (repl-loop parser enabled-parsers eval-func ctx))
      (lambda ()
        (tty-restore!) (signal-restore-sigwinch) (lineedit-finish ctx)))))

;
; top-level interactive repl with optional arguments:
; 'parser initial-parser   - defaults to 'shell
; 'parsers enabled-parsers - defaults to (parsers)
; 'eval eval-func          - defaults to repl-eval
; 'linectx ctx             - defaults to (make-linectx sh-expand-ps1 sh-autocomplete)
;
; Returns linectx, usable for further calls to (repl)
(define (repl . args)
  (let ((initial-parser #f)  (initial-parser? #f)
        (enabled-parsers #f) (enabled-parsers? #f)
        (eval-func #f)       (eval-func? #f)
        (ctx #f)             (ctx? #f))
    (do ((args-left args (cddr args)))
        ((null? args-left))
      (assert (not (null? (cdr args-left))))
      (let ((opt (car args-left))
            (val (cadr args-left)))
        (case opt
          ((parser)  (set! initial-parser val)  (set! initial-parser? #t))
          ((parsers) (set! enabled-parsers val) (set! enabled-parsers? #t))
          ((eval)    (set! eval-func val)       (set! eval-func? #t))
          ((linectx) (set! ctx val)             (set! ctx? #t))
          (else     (syntax-violation 'repl "unexpected argument:" opt)))))
    (repl* (if initial-parser?  initial-parser 'shell)
           (if enabled-parsers? enabled-parsers (parsers))
           (if eval-func? eval-func repl-eval)
           (if ctx? ctx (make-linectx sh-expand-ps1 sh-autocomplete)))))

) ; close library