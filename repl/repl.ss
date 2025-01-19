;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh repl (0 1))
  (export sh-repl sh-repl* sh-repl-eval sh-repl-eval-list sh-repl-lineedit
          sh-repl-parse sh-repl-print sh-repl-exception-handler sh-repl-interrupt-handler)
  (import
    (rnrs)
    (only (rnrs mutable-pairs) set-car!)
    (only (chezscheme)
      abort base-exception-handler break-handler
      console-input-port console-output-port console-error-port
      debug debug-condition debug-on-exception default-exception-handler display-condition
      eval exit-handler inspect keyboard-interrupt-handler parameterize
      pretty-print read-token reset reset-handler void)
    (schemesh bootstrap)
    (only (schemesh containers) list-iterate)
    (only (schemesh lineedit charhistory) charhistory-path-set!)
    (schemesh lineedit io)
    (schemesh lineedit linectx)
    (schemesh lineedit)
    (schemesh parser)
    (schemesh posix signal) ; also for suspend-handler
    (schemesh posix tty)
    (only (schemesh shell) sh-history sh-consume-sigchld sh-make-linectx sh-repl-args sh-xdg-cache-home/))


;; Read user input.
;; If user pressed ENTER, return textual input port containing entered text.
;;
;; Returns:
;; #f if got end-of-file
;; #t if waiting for more keypresses
;; a textual input port if user pressed ENTER.
(define (sh-repl-lineedit lctx)
  (sh-consume-sigchld)
  (let ((ret (lineedit-read lctx -1)))
    (sh-consume-sigchld)
    (if (boolean? ret)
      ret
      (open-charlines-input-port ret))))


;; Parse user input.
;; Arguments:
;;   pctx - a parsectx containing textual input port to parse, its position
;;          and a hashtable of enabled parsers (can be #f)
;;   initial-parser - initial parser to use: a symbol or parser
;;
;; Automatically switches to other parsers if a directive #!... is found in a (possibly
;; nested) list being parsed.
;;
;; Return two values:
;;   list of forms containing Scheme code to evaluate,
;;   and updated parser to use.
(define sh-repl-parse parse-forms)


;; Eval with (sh-eval) a single form containing parsed expressions or shell commands,
;; and return value or exit status of executed form.
;; May return multiple values.
;;
;; Note: if a form in list is (shell ...), which would create a job but NOT run it,
;;       eval instead (sh-run/i (shell ...)) that also interruptibly runs the job.
;;
;; This has two effects:
;; 1. when using shell parser, top-level commands will be executed immediately.
;; 2. when using scheme parser, top-level (shell ...) will be executed immediately.
(define (sh-repl-eval form)
  ; (debugf "sh-repl-eval: ~s" form)
  (try
    (sh-eval
      (if (and (pair? form) (memq (car form) '(shell shell-subshell)))
        (list 'sh-run/i form)
        form))
    (catch (ex)
      (sh-repl-exception-handler ex))))


;; Execute with (sh-repl-eval form) each form in list of forms containing parsed expressions
;; or shell commands, and print each returned value(s) or exit status.
;;
;; Implementation note:
;;   some procedures we may eval, as (break) (debug) (inspect) ...
;;   expect the tty to be in canonical mode, not in raw mode.
;;
;;   Also, we need tty to be in canonical mode for CTRL+C to generate SIGINT,
;;   which causes Chez Scheme to suspend long/infinite evaluations
;;   and call (break) - a feature we want to preserve.
;;
;;   For these reasons, the loop (do ... (sh-repl-eval ...))
;;   is wrapped inside (dynamic-wind tty-restore! (lambda () ...) tty-setraw!)
(define (sh-repl-eval-list forms print-func)
  ; (debugf "evaluating list: ~s" forms)
  (unless (null? forms)
    (dynamic-wind
      tty-restore!
      (lambda ()
        (do ((tail forms (cdr tail)))
            ((null? tail))
          (if print-func
            (call-with-values
              (lambda () (sh-repl-eval (car tail)))
              print-func)
            (begin
              (sh-repl-eval (car tail))
              (void)))))
      tty-setraw!)))


;; Print values or exit statuses.
(define (sh-repl-print . values)
  (do ((tail values (cdr tail)))
      ((null? tail))
    (let ((value (car tail)))
      (unless (eq? (void) value)
        (pretty-print value)))))

;; Parse and execute user input.
;; Calls in sequence (sh-repl-lineedit) (sh-repl-parse) (sh-repl-eval-list)
;;
;; Returns updated parser to use, or #f if got end-of-file.
(define (sh-repl-once initial-parser print-func lctx)
  (linectx-parser-name-set! lctx (parser-name initial-parser))
  (let ((in (sh-repl-lineedit lctx)))
    (case in
      ((#f) #f)             ; got end-of-file
      ((#t) initial-parser) ; nothing to execute: waiting for more user input
      (else
        (let-values (((forms updated-parser)
                        (sh-repl-parse (make-parsectx* in
                                         (linectx-parsers lctx)
                                         (linectx-width lctx)
                                         (linectx-prompt-end-x lctx)
                                         0 0)
                                    initial-parser)))
          (unless (eq? (void) forms)
            (sh-repl-eval-list forms print-func))
          updated-parser)))))


;; main loop of (sh-repl) and (sh-repl*)
;;
;; Returns values passed to (exit), or (void) on linectx eof
(define (sh-repl-loop parser print-func lctx)
  (let ((repl-args (list parser print-func lctx)))
    (call/cc
      (lambda (k-exit)
        (parameterize ((sh-repl-args repl-args)
                       (base-exception-handler sh-repl-exception-handler)
                       (break-handler
                         (lambda break-args
                           (sh-repl-interrupt-handler repl-args break-args)))
                       (exit-handler k-exit)
                       (keyboard-interrupt-handler
                         (lambda ()
                           (put-string (console-error-port) "\n; interrupted\n")
                           (sh-repl-interrupt-handler repl-args '())))
                       (reset-handler (reset-handler))
                       (suspend-handler
                         (lambda ()
                           (put-string (console-error-port) "\n; suspended\n")
                           (sh-repl-interrupt-handler repl-args '()))))
          (let ((k-reset k-exit)
                (updated-parser parser))
            (reset-handler (lambda () (k-reset)))
            (call/cc (lambda (k) (set! k-reset k)))
            ; when the (reset-handler) we installed is called, resume from here
            (while updated-parser
              (set! parser updated-parser)
              (set-car! repl-args updated-parser)
              (set! updated-parser (sh-repl-once parser print-func lctx)))))))))


;; top-level interactive sh-repl with all arguments mandatory
;;
;; Returns values passed to (exit), or (void) on linectx eof
(define (sh-repl* initial-parser print-func lctx)
  ; (to-parser) also checks initial-parser's and enabled-parser's validity
  (let ((parser (to-parser (linectx-parsers lctx) initial-parser 'sh-repl)))
    (assert* 'sh-repl (linectx? lctx))
    (dynamic-wind
      (lambda ()
        (lineedit-clear! lctx)
        (linectx-load-history! lctx)
        (signal-init-sigwinch)
        (tty-setraw!))
      (lambda ()
        (sh-repl-loop parser print-func lctx))
      (lambda ()
        (tty-restore!)
        (signal-restore-sigwinch)
        (lineedit-finish lctx)
        (linectx-save-history lctx)))))


;; top-level interactive repl with optional arguments:
;; 'history history-path    - defaults to (sh-xdg-cache-dir/ "schemesh/history.txt")
;; 'parser initial-parser   - defaults to 'shell
;; 'parsers enabled-parsers - defaults to (parsers)
;; 'print print-func        - defaults to sh-repl-print
;; 'linectx lctx            - defaults to (sh-make-linectx* enabled-parsers history-path)
;;
;; Returns first value passed to (exit), or (void) on linectx eof
(define (sh-repl . options)
  (let ((history-path #f)    (history-path? #f)
        (initial-parser #f)  (initial-parser? #f)
        (enabled-parsers #f) (enabled-parsers? #f)
        (print #f)           (print? #f)
        (lctx #f)            (lctx? #f))
    (do ((tail options (cddr tail)))
        ((null? tail))
      (assert* 'sh-repl (pair? (cdr tail)))
      (let ((key (car tail))
            (val (cadr tail)))
        (case key
          ((linectx) (set! lctx val)            (set! lctx? #t))
          ((history) (set! history-path val)    (set! history-path? #t))
          ((parser)  (set! initial-parser val)  (set! initial-parser? #t))
          ((parsers) (set! enabled-parsers val) (set! enabled-parsers? #t))
          ((print)   (set! print val)           (set! print? #t))
          (else      (syntax-violation 'sh-repl "unexpected argument:" key)))))
    (when (and lctx? enabled-parsers?)
      (linectx-parsers-set! lctx enabled-parsers))
    (when (and lctx? history-path?)
      (charhistory-path-set! (linectx-history lctx) history-path))
    (first-value-or-void
      (sh-repl*
        (if initial-parser?  initial-parser  'shell)
        (if print?   print   sh-repl-print)
        (if lctx?
          lctx
          (sh-make-linectx
            (if enabled-parsers? enabled-parsers (parsers))
            (if history-path?    history-path    (sh-xdg-cache-home/ "schemesh/history.txt"))))))))


;; React to uncaught conditions
(define (sh-repl-exception-handler obj)
  (let ((out (console-error-port)))
    (dynamic-wind
      (lambda () ; before body
        (put-string out "; ")
        (display-condition obj out)
        (put-string out "\n")
        (flush-output-port out))
      (lambda () ; body
        (when (or (serious-condition? obj) (not (warning? obj)))
          (debug-condition obj) ;; save obj into thread-parameter (debug-condition)
          (if (debug-on-exception)
            (debug)
            (put-string out "; type (debug) to enter the debugger.\n"))
          (flush-output-port out)))
      (lambda () ; after body
        ((reset-handler))))))


;; React to calls to (break), to keyboard CTRL+C and to SIGTSTP signal: enter the debugger.
(define (sh-repl-interrupt-handler repl-args break-args)
  (call/cc
    (lambda (k)
      (parameterize ((break-handler void)
                     (keyboard-interrupt-handler void)
                     (suspend-handler void))
        (let ((out (console-output-port)))
          (sh-repl-interrupt-show-who-msg-irritants break-args out)
          (while (sh-repl-interrupt-handler-once repl-args k out)))))))


;; Print (break ...) arguments
(define (sh-repl-interrupt-show-who-msg-irritants args out)
  (when (pair? args)
    (let* ((who  (car args))
           (tail (cdr args))
           (msg  (if (pair? tail) (car tail) ""))
           (irritants (if (pair? tail) (cdr tail) '())))
     (put-string out "break in " )
     (put-datum  out who)
     (put-string out ": ")
     (put-string out msg)
     (list-iterate irritants
       (lambda (value)
         (put-char   out #\space)
         (put-datum  out value)))
     (put-char   out #\newline))))


;; Single iteration of (sh-repl-interrupt-handler)
(define (sh-repl-interrupt-handler-once repl-args k out)
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
    ((c e cont exit)  #f)
    ((i inspect)      (inspect k) #t)
    ((n new)          (apply sh-repl* repl-args) #t)
    ((q r quit reset) (reset) #f)
    ((t throw)        (error #f "user interrupt") #f)
    ((? help)
      (put-string out "
Type ? or help for this help.
     i or inspect to inspect current continuation
     n or new to enter new repl
     c or e to exit interrupt handler and continue
     t or throw to raise an error condition
     q or r to reset scheme
     a or abort to abort scheme
     \n\n")
      (flush-output-port out)
      #t)
    (else (put-string out "Invalid command.  Type ? for help.\n")
      (flush-output-port out)
      #t)))

) ; close library
