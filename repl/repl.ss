;;; Copyright (C) 2023-2025 by Massimiliano Ghilardi
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.


(library (schemesh repl (0 7 6))
  (export repl repl* repl-eval repl-eval-list
          repl-lineedit repl-parse repl-print
          repl-exception-handler repl-interrupt-handler

          sh-eval-file/print sh-eval-file/print* sh-eval-port/print*
          sh-eval-parsectx/print* sh-eval-string/print*)
  (import
    (rnrs)
    (only (rnrs mutable-pairs) set-car!)
    (only (chezscheme)
      abort base-exception-handler break-handler
      console-input-port console-output-port console-error-port
      default-exception-handler display-condition
      eval exit-handler expand inspect keyboard-interrupt-handler
      parameterize pretty-print read-token reset reset-handler void)
    (schemesh bootstrap)
    (only (schemesh containers) list-iterate)
    (only (schemesh lineedit charhistory) charhistory-path-set!)
    (schemesh lineedit charlines io)
    (schemesh lineedit linectx)
    (only (schemesh lineedit lineterm) lineterm-write/u8)
    (schemesh lineedit lineedit)
    (schemesh parser)
    (only (schemesh posix dir) file-type)
    (schemesh posix signal)
    (schemesh posix tty)
    (only (schemesh shell)
       sh-consume-signals sh-current-job-suspend sh-current-job-yield sh-exception-handler
       repl-restart repl-restart? sh-schemesh-reload-count
       sh-eval sh-eval-file sh-eval-file* sh-eval-port* sh-eval-parsectx* sh-eval-string*
       sh-job-control? sh-job-control-available? sh-make-linectx
       repl-args sh-run/i sh-xdg-cache-home/ sh-xdg-config-home/))



;; variant of (sh-eval-file) that pretty-print the result(s) instead of returning them
(define sh-eval-file/print
  (case-lambda
    ((path)
      (call-with-values (lambda () (sh-eval-file path))                                repl-print))
    ((path initial-parser)
      (call-with-values (lambda () (sh-eval-file path initial-parser))                 repl-print))
    ((path initial-parser enabled-parsers)
      (call-with-values (lambda () (sh-eval-file path initial-parser enabled-parsers)) repl-print))))


;; variant of (sh-eval-file*) that pretty-print the result(s) instead of returning them
(define (sh-eval-file/print* path initial-parser enabled-parsers)
  (call-with-values
    (lambda () (sh-eval-file* path initial-parser enabled-parsers))
    repl-print))


;; variant of (sh-eval-port*) that pretty-print the result(s) instead of returning them
(define (sh-eval-port/print* path initial-parser enabled-parsers)
  (call-with-values
    (lambda () (sh-eval-port* path initial-parser enabled-parsers))
    repl-print))


;; variant of (sh-eval-parsectx*) that pretty-print the result(s) instead of returning them
(define (sh-eval-parsectx/print* pctx initial-parser)
  (call-with-values
    (lambda () (sh-eval-parsectx* pctx initial-parser))
    repl-print))


;; variant of (sh-eval-string*) that pretty-print the result(s) instead of returning them
(define (sh-eval-string/print* pctx initial-parser enabled-parsers)
  (call-with-values
    (lambda () (sh-eval-string* pctx initial-parser enabled-parsers))
    repl-print))


;; React to uncaught exceptions
(define (repl-exception-handler obj)
  (sh-exception-handler obj (reset-handler)))


;; Read user input.
;; If user pressed ENTER, return textual input port containing entered text.
;;
;; Returns:
;; #f if got end-of-file
;; #t if waiting for more keypresses
;; a textual input port if user pressed ENTER.
(define (repl-lineedit lctx)
  (sh-consume-signals lctx)
  (let ((ret (lineedit-read lctx -1)))
    (sh-consume-signals lctx)
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
(define (repl-parse pctx initial-parser)
  (parse-forms pctx initial-parser))


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
(define (repl-eval form)
  ; (debugf "repl-eval: ~s" form)
  (try
    (if (and (pair? form) (memq (car form) '(shell shell-subshell shell-expr)))
      (sh-run/i (sh-eval form))
      (sh-eval form)) ; may return multiple values
    (catch (ex)
      (repl-exception-handler ex))))


;; Execute with (repl-eval form) each form in list of forms containing parsed expressions
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
;;   For these reasons, the loop (do ... (repl-eval ...))
;;   is wrapped inside (dynamic-wind tty-restore! (lambda () ...) tty-setraw!)
(define (repl-eval-list forms print-func)
  ; (debugf "evaluating list: ~s" forms)
  (unless (null? forms)
    (dynamic-wind
      tty-restore!
      (lambda ()
        (do ((tail forms (cdr tail)))
            ((null? tail))
          (if print-func
            (call-with-values
              (lambda () (repl-eval (car tail)))
              print-func)
            (begin
              (repl-eval (car tail))
              (void)))))
      tty-setraw!)))


;; Print values or exit statuses.
(define (repl-print . vals)
  (do ((tail vals (cdr tail)))
      ((null? tail))
    (let ((value (car tail)))
      (unless (eq? (void) value)
        (pretty-print value)))))


;; Parse and execute user input.
;; Calls in sequence (repl-lineedit) (repl-parse) (repl-eval-list)
;;
;; Returns updated parser to use, or #f if got end-of-file.
(define (repl-once initial-parser print-func lctx)
  (linectx-parser-name-set! lctx (parser-name initial-parser))
  ;; (debugf "repl-once ready")
  (let ((in (repl-lineedit lctx)))
    ;; (debugf "repl-once read ~s" in)
    (case in
      ((#f) #f)             ; got end-of-file
      ((#t) initial-parser) ; nothing to execute: waiting for more user input
      (else
        (let-values (((forms updated-parser)
                        (repl-parse (make-parsectx* in
                                         (linectx-parsers lctx)
                                         (linectx-width lctx)
                                         (linectx-prompt-end-x lctx)
                                         0 0)
                                    initial-parser)))
          (unless (eq? (void) forms)
            (repl-eval-list forms print-func))
          updated-parser)))))


;; main loop of (repl) and (repl*)
;;
;; Returns values passed to (exit), or (void) on linectx eof
(define (repl-loop parser print-func lctx)
  ;; set to #f the init-file-path and quit-file-path saved in (repl-args):
  ;; if (repl* ...) is called from an interrupt handler, we do NOT want to load them again
  (let ((my-repl-args (list parser print-func lctx #f #f))
        (reload-count (sh-schemesh-reload-count)))
    (repl-restart #f)
    (call/cc
      (lambda (k-exit)
        (parameterize ((repl-args  my-repl-args)
                       (base-exception-handler repl-exception-handler)
                       (break-handler
                         (lambda break-args
                           (repl-interrupt-handler my-repl-args break-args)))
                       (exit-handler k-exit)
                       (keyboard-interrupt-handler
                         (lambda ()
                           (repl-interrupt-handler my-repl-args '())))
                       (reset-handler (reset-handler)))
          (let ((k-reset k-exit))
            (reset-handler (lambda () (k-reset)))
            (call/cc (lambda (k) (set! k-reset k)))
            ; when the (reset-handler) we installed is called, resume from here
            (while parser
              (set! parser (repl-once parser print-func lctx))
              (cond
                (parser
                  (set-car! my-repl-args parser)
                  (if (repl-restart?)
                    (set! parser #f) ; parser if #f, loop will exit
                    (let ((new-reload-count (sh-schemesh-reload-count)))
                      (unless (= reload-count new-reload-count)
                        (set! reload-count new-reload-count)
                        (put-string (console-error-port)
                          "; warning: libschemesh was reloaded. Call (repl-restart) to switch to the new libschemesh.\n")))))
                (else ; EOF
                  (lineterm-write/u8 lctx 10))))
            0)))))) ; EOF, or (repl-restart) was called. return 0


(define (try-eval-file path)
  (and (string? path) (symbol? (file-type path 'catch))
    (try
      (sh-eval-file path)
      #t
      (catch (ex)
        (let ((port (console-error-port)))
          (put-string port "; Warning: failed loading file ")
          (put-datum  port path)
          (put-string port ": ")
          (display-condition ex port)
          (newline port)
          (flush-output-port port))
        #f))))


;; top-level interactive repl with all arguments mandatory
;;
;; Returns values passed to (exit), or (void) on linectx eof
(define (repl* initial-parser print-func lctx init-file-path quit-file-path)
  ; (to-parser) also checks initial-parser's and enabled-parser's validity
  (let ((parser (to-parser (linectx-parsers lctx) initial-parser 'repl))
        (old-job-control (sh-job-control?))
        (new-job-control #f))
    (assert* 'repl (linectx? lctx))
    (dynamic-wind
      (lambda ()
        (lineedit-clear! lctx)
        (linectx-load-history! lctx)
        (signal-init-sigwinch)
        (try-eval-file init-file-path)
        ; enable job control if available
        (set! new-job-control (sh-job-control? (sh-job-control-available?)))
        (when new-job-control
          (tty-setraw!)))
      (lambda ()
        (repl-loop parser print-func lctx))
      (lambda ()
        (when new-job-control
          (tty-restore!))
        ; restore job control to previous value
        (sh-job-control? old-job-control)
        (try-eval-file quit-file-path)
        (signal-restore-sigwinch)
        (lineedit-flush lctx)
        (linectx-save-history lctx)))))


;; top-level interactive repl with optional arguments:
;; 'history history-path    - string,    defaults to (sh-xdg-cache-dir/ "schemesh/history.txt")
;; 'init    init-file-path  - string,    defaults to (sh-xdg-config-dir/ "schemesh/repl_init.sh")
;; 'parser  initial-parser  - symbol,    defaults to 'shell
;; 'parsers enabled-parsers - hashtable, defaults to (parsers)
;; 'print   print-func      - procedure, defaults to repl-print
;; 'quit    quit-file-path  - string,    defaults to (sh-xdg-config-dir/ "schemesh/repl_quit.sh")
;; 'linectx lctx            - linectx,   defaults to (sh-make-linectx* enabled-parsers history-path)
;;
;; Returns first value passed to (exit), or (void) on linectx eof
(define (repl . options)
  ; (debugf "repl options=~s" options)
  (let ((history-path #f)    (history-path? #f)
        (initial-parser #f)  (initial-parser? #f)
        (enabled-parsers #f) (enabled-parsers? #f)
        (print #f)           (print? #f)
        (lctx #f)            (lctx? #f)
        (init-file-path #f)  (init-file-path? #f)
        (quit-file-path #f)  (quit-file-path? #f))
    (do ((tail options (cddr tail)))
        ((null? tail))
      (assert* 'repl (pair? (cdr tail)))
      (let ((key (car tail))
            (val (cadr tail)))
        (case key
          ((linectx) (set! lctx val)            (set! lctx? #t))
          ((history) (set! history-path val)    (set! history-path? #t))
          ((init)    (set! init-file-path val)  (set! init-file-path? #t))
          ((parser)  (set! initial-parser val)  (set! initial-parser? #t))
          ((parsers) (set! enabled-parsers val) (set! enabled-parsers? #t))
          ((print)   (set! print val)           (set! print? #t))
          ((quit)    (set! quit-file-path val)  (set! quit-file-path? #t))
          (else      (syntax-violation 'repl "unexpected argument:" key)))))
    (when (and lctx? enabled-parsers?)
      (linectx-parsers-set! lctx enabled-parsers))
    (when (and lctx? history-path?)
      (charhistory-path-set! (linectx-history lctx) history-path))
    (first-value-or-void
      (repl*
        (if initial-parser?  initial-parser  'shell)
        (if print?   print   repl-print)
        (if lctx?
          lctx
          (sh-make-linectx
            (if enabled-parsers? enabled-parsers (parsers))
            (if history-path?    history-path    (sh-xdg-cache-home/ "schemesh/history.txt"))))
        (if init-file-path? init-file-path (sh-xdg-config-home/ "schemesh/repl_init.ss"))
        (if quit-file-path? quit-file-path (sh-xdg-config-home/ "schemesh/repl_quit.ss"))))))



;; React to calls to (break), to keyboard CTRL+C and to SIGTSTP signal:
;; either enter the debugger, or, if possible, suspend the current job.
(define (repl-interrupt-handler my-repl-args break-args)
  ;; invoked also for SIGTSTP, because signal.h installs
  ;; a SIGTSTP handler that intentionally calls raise(SIGINT)
  ;;
  ;; reason: it's the simplest mechanism to quickly suspend a long-running Scheme procedure
  (let ((suspend? (signal-consume-sigtstp)))
    ;; try to suspend ourselves and our caller
    (unless (and suspend? (sh-current-job-suspend))
      (put-string (console-error-port)
        (if suspend? "\n; suspended\n" "\n; interrupted\n"))
      (call/cc
        (lambda (k)
          (parameterize ((break-handler void)
                         (keyboard-interrupt-handler void))
            (repl-interrupt-show-who-msg-irritants break-args (console-error-port))
            (let ((port (console-output-port)))
              (while (repl-interrupt-handler-once my-repl-args k port)))))))))


;; Print (break ...) arguments
(define (repl-interrupt-show-who-msg-irritants args port)
  (when (pair? args)
    (let* ((who  (car args))
           (tail (cdr args))
           (msg  (if (pair? tail) (car tail) ""))
           (irritants (if (pair? tail) (cdr tail) '())))
     (put-string port "break in " )
     (put-datum  port who)
     (put-string port ": ")
     (put-string port msg)
     (list-iterate irritants
       (lambda (value)
         (put-char   port #\space)
         (put-datum  port value)))
     (put-char   port #\newline))))


;; Single iteration of (repl-interrupt-handler)
(define (repl-interrupt-handler-once my-repl-args k out)
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
    ((n new)          (apply repl* my-repl-args) #t)
    ((q r quit reset) (reset) #f)
    ((t throw)        (error #f "user interrupt") #f)
    ((? help)
      (put-string out "
Type ? or help for this help.
     i or inspect to inspect current continuation
     n or new to enter new repl
     c or e to exit interrupt handler and continue
     t or throw to raise an error condition
     q or r to quit current evaluation. returns to REPL
     a or abort to abort schemesh. terminates the program!
     \n\n")
      (flush-output-port out)
      #t)
    (else (put-string out "Invalid command.  Type ? for help.\n")
      (flush-output-port out)
      #t)))

) ; close library
