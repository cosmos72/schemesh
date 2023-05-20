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

void schemesh_define_library_repl(void) {
#define SCHEMESH_LIBRARY_REPL_EXPORT "repl-lineedit repl-parse repl-eval repl-eval-list repl "

  eval("(library (schemesh repl (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_REPL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) eval format void)\n"
       "    (schemesh bootstrap)\n"
       "    (only (schemesh containers) list-iterate)\n"
       "    (schemesh io)\n"
       "    (schemesh lineedit)\n"
       "    (schemesh parser)\n"
       "    (schemesh tty))\n"
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
       "  (let ((arg (if (and (pair? form) (eq? 'shell (car form)))\n"
       "               (list 'sh-run form)\n"
       "               form)))\n"
       "    (format #t \"; evaluating: ~s~%\" arg)\n"
       "    (eval arg)))\n"
       "\n"
       /**
        * Execute a list of forms containing parsed expressions or shell commands,
        * and return value or exit status of last form in list.
        * May return multiple values.
        */
       "(define (repl-eval-list forms)\n"
       "  (format #t \"; evaluating list: ~s~%\" forms)\n"
       "  (do ((tail forms (cdr tail)))\n"
       "      ((or (null? tail) (null? (cdr tail)))\n"
       "        (if (null? tail) (values) (repl-eval (car tail))))\n"
       "    (repl-eval (car tail))))\n"
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
       "(define (repl-once ctx initial-parser enabled-parsers)\n"
       /** TODO: catch and show exceptions, support calls to (debug) */
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
       "              (lambda () (repl-eval-list form))\n"
       "              repl-print))\n"
       "          updated-parser)))))\n"
       "\n"
       /** top-level interactive repl with all arguments mandatory */
       "(define (repl* initial-parser enabled-parsers)\n"
       /*        also check initial-parser's validity */
       "  (let ((parser (to-parser initial-parser enabled-parsers 'repl))\n"
       "        (ctx (make-linectx)))\n"
       "    (lineedit-clear! ctx)"
       "    (dynamic-wind\n"
       "      tty-setraw!\n" /** run before body */
       "      (lambda ()\n"  /** body            */
       "        (while parser\n"
       "          (set! parser (repl-once ctx parser enabled-parsers))))\n"
       "      (lambda ()\n" /** run after body  */
       "        (lineedit-flush ctx)\n"
       "        (tty-restore!)))))\n"
       "\n"
       /**
        * top-level interactive repl with optional arguments:
        * initial-parser, defaults to 'scheme
        * enabled-parsers, defaults to (parsers)
        */
       "(define repl\n"
       "  (case-lambda\n"
       "    (()\n"
       "      (repl* 'scheme (parsers)))\n"
       "    ((initial-parser)\n"
       "      (repl* initial-parser (parsers)))\n"
       "    ((initial-parser enabled-parsers)\n"
       "      (repl* initial-parser enabled-parsers))))\n"
       ")\n"); /* close library */
}
