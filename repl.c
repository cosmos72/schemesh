/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "container.h"
#include "eval.h"
#include "io.h"
#include "lineedit.h"
#include "parse.h"
#include "shell.h"
#include "signal.h"

#include <string.h>
#include <unistd.h>

#undef SCHEMESH_LIBRARY_REPL_DEBUG

void schemesh_define_library_repl(void) {
#define SCHEMESH_LIBRARY_REPL_EXPORT "repl-lineedit repl-parse repl-eval repl-eval-list repl repl* "

  eval("(library (schemesh repl (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_REPL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme)\n"
       "      base-exception-handler debug eval exit-handler inspect\n"
#ifdef SCHEMESH_LIBRARY_REPL_DEBUG
       "      format\n"
#endif
       "      parameterize reset-handler void)\n"
       "    (schemesh bootstrap)\n"
       "    (only (schemesh containers) list-iterate)\n"
       "    (schemesh io)\n"
       "    (schemesh lineedit)\n"
       "    (schemesh parser)\n"
       "    (schemesh signals)\n"
       "    (schemesh tty)\n"
       "    (only (schemesh shell) sh-consume-sigchld sh-expand-ps1))\n"
       "\n"
       /**
        * Read user input.
        * If user pressed ENTER, return textual input port containing entered text.
        *
        * Returns:
        * #f if got end-of-file
        * #t if waiting for more keypresses
        * a textual input port if user pressed ENTER.
        */
       "(define (repl-lineedit ctx)\n"
       "  (let ((ret (lineedit-read ctx -1)))\n"
       "    (if (boolean? ret)\n"
       "      ret\n"
       "      (open-gbuffer-of-chargbuffers-input-port ret))))\n"
       "\n"
       /**
        * Parse user input.
        * First argument is a textual input stream containing user input.
        * Second argument is initial parser to use, or a symbol containing the parser's name.
        * Third argument is #f or a hashtable containing enabled parsers.
        *
        * Automatically switches to other parsers if a directive #!... is found in a (possibly
        * nested) list being parsed.
        *
        * Returns two values: parsed forms, and update parser to use.
        * Arguments:
        *   in - textual input stream
        *   initial-parser - initial parser to use: a symbol or parser
        *   enabled-parsers - #f or hashtable of enabled parsers
        *
        * Return two values:
        *   list of forms containing Scheme code to evaluate,
        *   and updated parser to use.
        */
       "(define repl-parse parse-forms)\n"
       "\n"
       /**
        * Eval a single form containing parsed expressions or shell commands,
        * and return value or exit status of executed form.
        * May return multiple values.
        *
        * Note: if a form in list is (shell ...), which would create a job but NOT run it,
        *       eval instead (sh-run (shell ...)) that also runs the job.
        *
        * This has two effects:
        * 1. when using shell parser, top-level commands will be executed immediately.
        * 2. when using scheme parser, top-level (shell ...) will be executed immediately.
        */
       "(define (repl-eval form)\n"
#ifdef SCHEMESH_LIBRARY_REPL_DEBUG
       "  (format #t \"; evaluating: ~s~%\" form)\n"
#endif
       "  (with-exception-handler\n"
       "    (lambda (cond)\n"
#ifdef SCHEMESH_LIBRARY_REPL_DEBUG
       "      (format #t \"repl-eval handling condition ~s~%\" cond)\n"
#endif
       "      ((base-exception-handler) cond))\n"
       "    (lambda ()\n"
       "      (eval\n"
       "        (if (and (pair? form) (memq (car form) '(shell shell-list)))\n"
       "          (list 'sh-run form)\n"
       "          form)))))\n"
       "\n"
       /**
        * Execute with (eval-func form) each form in list of forms containing parsed expressions
        * or shell commands, and return value or exit status of last form in list.
        * May return multiple values.
        *
        * Implementation note:
        *   some procedures we may eval, as (break) (debug) (inspect) ...
        *   expect the tty to be in canonical mode, not in raw mode.
        *
        *   Also, we need tty to be in canonical mode for CTRL+C to generate SIGINT,
        *   which causes Chez Scheme to suspend long/infinite evaluations
        *   and call (break) - a feature we want to preserve.
        *
        *   For these reasons, the loop (do ... (eval-func ...))
        *   is wrapped inside (dynamic-wind tty-restore! (lambda () ...) tty-setraw!)
        */
       "(define (repl-eval-list forms eval-func)\n"
#ifdef SCHEMESH_LIBRARY_REPL_DEBUG
       "  (format #t \"; evaluating list: ~s~%\" forms)\n"
#endif
       "  (unless (null? forms)\n"
       "    (dynamic-wind\n"
       "      tty-restore!\n"
       "      (lambda ()\n"
       "        (do ((tail forms (cdr tail)))\n"
       "            ((null? (cdr tail))\n"
       "              (eval-func (car tail)))\n"
       "          (eval-func (car tail))))\n"
       "      tty-setraw!)))\n"
       /**
        * Print values or exit statuses.
        */
       "(define (repl-print . values)\n"
       "  (list-iterate values\n"
       "    (lambda (value)\n"
       "      (unless (eq? (void) value)\n"
       "        (write value)\n"
       "        (write-char #\\newline)))))\n"
       "\n"
       /**
        * Parse and execute user input.
        * Calls in sequence (repl-lineedit) (repl-parse) (repl-eval-list) and (repl-print)
        *
        * Returns updated parser to use, or #f if got end-of-file.
        */
       "(define (repl-once ctx initial-parser enabled-parsers eval-func)\n"
       "  (let ((in (repl-lineedit ctx)))\n"
       "    (case in\n"
       /*     got end-of-file */
       "      ((#f) #f)\n"
       /*     nothing to execute: waiting for more user input */
       "      ((#t) initial-parser)\n"
       "      (else\n"
       "        (let-values (((form updated-parser)\n"
       "                        (repl-parse in initial-parser enabled-parsers)))\n"
       "          (unless (eq? (void) form)\n"
       "            (call-with-values\n"
       "              (lambda () (repl-eval-list form eval-func))\n"
       "              repl-print))\n"
       "          updated-parser)))))\n"
       "\n"
       /** main loop of (repl) and (repl*) */
       "(define (repl-loop parser enabled-parsers eval-func ctx)\n"
       "  (call/cc\n"
       "    (lambda (k-exit)\n"
       "      (parameterize ((exit-handler k-exit) (reset-handler (reset-handler)))\n"
       "        (let ((k-reset k-exit))\n"
       "          (reset-handler (lambda () (k-reset)))\n"
       "          (call/cc (lambda (k) (set! k-reset k)))\n"
       /*         when the (reset-handler) we installed is called, resume from here */
       "          (while parser\n"
       "            (set! parser (repl-once ctx parser\n"
       "                           enabled-parsers eval-func))\n"
       "            (sh-consume-sigchld)))))))\n"
       "\n"
       /** top-level interactive repl with all arguments mandatory */
       "(define (repl* initial-parser enabled-parsers eval-func ctx)\n"
       "  (assert (procedure? eval-func))\n"
       /* (to-parser) also checks initial-parser's validity */
       "  (let ((parser (to-parser initial-parser enabled-parsers 'repl)))\n"
       "    (dynamic-wind\n"
       "      (lambda ()\n"
       "        (lineedit-clear! ctx) (signal-init-sigwinch) (tty-setraw!))\n"
       "      (lambda ()\n"
       "        (repl-loop parser enabled-parsers eval-func ctx))\n"
       "      (lambda ()\n"
       "        (tty-restore!) (signal-restore-sigwinch) (lineedit-finish ctx)))))\n"
       "\n"
       /**
        * top-level interactive repl with optional arguments:
        * initial-parser,  defaults to 'scheme
        * enabled-parsers, defaults to (parsers)
        * eval-func,       defaults to repl-eval
        * ctx,             defaults to (make-linectx sh-expand-ps1)
        */
       "(define repl\n"
       "  (case-lambda\n"
       "    (()\n"
       "      (repl* 'scheme (parsers) repl-eval (make-linectx sh-expand-ps1)))\n"
       "    ((initial-parser)\n"
       "      (repl* initial-parser (parsers) repl-eval (make-linectx sh-expand-ps1)))\n"
       "    ((initial-parser enabled-parsers)\n"
       "      (repl* initial-parser enabled-parsers repl-eval (make-linectx sh-expand-ps1)))\n"
       "    ((initial-parser enabled-parsers eval-func)\n"
       "      (repl* initial-parser enabled-parsers eval-func (make-linectx sh-expand-ps1)))\n"
       "    ((initial-parser enabled-parsers eval-func ctx)\n"
       "      (repl* initial-parser enabled-parsers eval-func ctx))))\n"
       ")\n"); /* close library */
}
