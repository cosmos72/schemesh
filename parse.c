/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

static void define_library_parser_registry(void) {

#define SCHEMESH_LIBRARY_PARSER_REGISTRY_EXPORT                                                    \
  "make-parser parser? parser-parse parser-parse* parser-parse-list "                              \
  "known-parsers default-parser "

  eval("(library (schemesh parser registry (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_REGISTRY_EXPORT ")\n"
       "  (import\n"
       "    (rnrs))\n"
       "\n"
       "(define-record-type\n"
       "  (parser %make-parser parser?)\n"
       "  (fields parse parse* parse-list)\n"
       "  (nongenerative #{parser i32axi1gdq66lhds6b0mmmsqa-24}))\n"
       "\n"
       "(define (make-parser parse parse* parse-list)\n"
       "  (assert (procedure? parse))\n"
       "  (assert (procedure? parse*))\n"
       "  (assert (procedure? parse-list))\n"
       "  (%make-parser parse parse* parse-list))\n"
       "\n"
       /* map symbol -> %parser containing known parsers */
       "(define known-parsers (make-eq-hashtable))\n"
       "\n"
       /* default %parser */
       "(define default-parser (cons #f #f))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_parser_scheme(void) {

#define SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT "parse-scheme parse-scheme* parse-scheme-list "

  eval("(library (schemesh parser scheme (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme)\n"
       "       box bytevector fx1+\n"
       "       fxvector fxvector-set! make-fxvector\n"
       "       read-token reverse!)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh parser registry))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with Chez Scheme function
        * (read-token) and construct a Scheme form.
        * Return two values: parsed Scheme form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-scheme in)\n"
       "  (let-values (((type value start end) (read-token in)))\n"
       "    (let-values (((ret-value ret-type) (parse-impl value type in)))\n"
       "      (values ret-value (not (eq? 'eof ret-type))))))"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with Chez Scheme function
        * (read-token) and construct a Scheme form.
        * Return parsed Scheme form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-scheme* in)\n"
       "  (let-values (((value ok) (parse-scheme in)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-scheme \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with Chez Scheme function
        * (read-token) and construct a single Scheme form.
        * Return two values: the form, and its type.
        */
       "(define (parse-impl value type in)\n"
       "  (values\n"
       "    (case type\n"
       "      ((atomic eof)     value)\n"
       "      ((box)            (box (parse-scheme* in)))\n"
       "      ((lbrack lparen)  (parse-scheme-list type in))\n"
       "      ((quote)          (list 'quote (parse-scheme* in)))\n"
       "      ((vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)\n"
       "        (parse-vector type value in))\n"
       "      (else   (syntax-violation 'parse-scheme \"unimplemented token type\" type)))\n"
       "    type))\n"
       "\n"
       /**
        * Given textual input port 'in', read Scheme forms from it, until a token ) or ] matching
        * the specified begin-type token is found, and return a list containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-scheme-list begin-type in)\n"
       "  (let ((ret '())\n"
       "        (end-type (if (eq? 'lbrack begin-type) 'rbrack 'rparen))\n"
       "        (can-change-parser (eq? 'lparen begin-type))"
       "        (again? #t)\n"
       "        (reverse? #t))\n"
       "    (while again?\n"
       "      (let-values (((type value start end) (read-token in)))\n"
       "        (case type\n"
       "          ((eof)     (syntax-violation 'parse-scheme \"unexpected\" type))\n"
       "          ((rbrack)\n"
       "            (unless (eq? type end-type)\n"
       "              (syntax-violation 'parse-scheme \"unexpected token ], expecting )\" type))\n"
       "            (set! again? #f))\n"
       "          ((rparen)\n"
       "            (unless (eq? type end-type)\n"
       "              (syntax-violation 'parse-scheme \"unexpected token ), expecting ]\" type))\n"
       "            (set! again? #f))\n"
       "          (else\n"
       "            (let ((other-parser (and can-change-parser (symbol? value)\n"
       "                                     (hashtable-ref known-parsers value #f))))\n"
       /*             only the first token can cause a parser change */
       "              (set! can-change-parser #f)\n"
       "              (if (parser? other-parser)\n"
       /*               switch to other-parser */
       "                (let ((other-parse-list (parser-parse-list other-parser)))\n"
       "                  (set! ret (other-parse-list begin-type in))\n"
       "                  (set! reverse? #f)\n"
       "                  (set! again? #f))\n"
       /*               continue with current parser */
       "                (let-values (((value-i type-i) (parse-impl value type in)))\n"
       "                  (when (eq? 'eof type-i)\n"
       "                    (syntax-violation 'parse-scheme \"unexpected\" type-i))\n"
       "                  (set! ret (cons value-i ret)))))))))\n"
       "    (if reverse? (reverse! ret) ret)))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read Scheme forms from it, until a token ) is
        * found, and return a vector, fxvector or bytevector containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-vector vec-type length in)\n"
       "  (let ((values (parse-scheme-list vec-type in)))\n"
       "    (case vec-type\n"
       "      ((vfnxparen) (create-fxvector   length values))\n"
       "      ((vnparen)   (create-vector     length values))\n"
       "      ((vu8nparen) (create-bytevector length values))\n"
       "      ((vfxparen)  (apply fxvector   values))\n"
       "      ((vparen)    (apply vector     values))\n"
       "      ((vu8paren)  (apply bytevector values))\n"
       "      (else  (syntax-violation 'parse-scheme \"unexpected\" vec-type)))))\n"
       "\n"
       "(define (create-fxvector length values)\n"
       "  (let ((vec (make-fxvector length))\n"
       "        (elem (if (null? values) 0 (car values))))\n"
       "    (do ((i 0 (fx1+ i)))\n"
       "        ((fx>=? i length) vec)\n"
       "      (fxvector-set! vec i elem)\n"
       "      (unless (null? values)\n"
       /*       if we run out of values, fill remainder with last element in values */
       "        (set! values (cdr values))\n"
       "        (unless (null? values)\n"
       "          (set! elem (car values)))))))\n"
       "\n"
       "(define (create-vector length values)\n"
       "  (let ((vec (make-vector length))\n"
       "        (elem (if (null? values) 0 (car values))))\n"
       "    (do ((i 0 (fx1+ i)))\n"
       "        ((fx>=? i length) vec)\n"
       "      (vector-set! vec i elem)\n"
       "      (unless (null? values)\n"
       /*       if we run out of values, fill remainder with last element in values */
       "        (set! values (cdr values))\n"
       "        (unless (null? values)\n"
       "          (set! elem (car values)))))))\n"
       "\n"
       "(define (create-bytevector length values)\n"
       "  (let ((vec (make-bytevector length))\n"
       "        (elem (if (null? values) 0 (car values))))\n"
       "    (do ((i 0 (fx1+ i)))\n"
       "        ((fx>=? i length) vec)\n"
       "      (bytevector-u8-set! vec i elem)\n"
       "      (unless (null? values)\n"
       /*       if we run out of values, fill remainder with last element in values */
       "        (set! values (cdr values))\n"
       "        (unless (null? values)\n"
       "          (set! elem (car values)))))))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_parser_shell(void) {

#define SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT "parse-shell parse-shell* parse-shell-list "

  eval("(library (schemesh parser shell (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs))\n"
       "\n"
       /**
        * TODO: implement!
        *
        * Given textual input port 'in', repeatedly read from it using (read-shell-token)
        * and construct corresponding Scheme form.
        * Return two values: parsed Scheme form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-shell in)\n"
       "  (values \"\" #f))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it using (read-shell-token)
        * and construct corresponding form.
        * Return parsed form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-shell* in)\n"
       "  (let-values (((value ok) (parse-shell in)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-shell* \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Given textual input port 'in', read shell forms from it, until a token ) or ] or }
        * matching the specified begin-type token is found, and return a list containing such
        * forms. Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-shell-list begin-type in)\n"
       "  '())\n"
       "\n"
       ")\n"); /* close library */
}

void define_library_parser(void) {
  define_library_parser_registry();
  define_library_parser_scheme();
  define_library_parser_shell();

#define SCHEMESH_LIBRARY_PARSER_EXPORT "parse-form parse-form* parse-form-list "

  eval("(library (schemesh parser (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_REGISTRY_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-pairs)\n"
       "    (schemesh parser registry)\n"
       "    (schemesh parser scheme)\n"
       "    (schemesh parser shell))\n"
       "\n"
       /**
        * Call parse-scheme, parse-shell or whatever is the current default parser.
        * Return two values: parsed form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-form in)\n"
       "  (let ((proc (parser-parse (cdr default-parser))))\n"
       "    (proc in)))\n"
       "\n"
       /**
        * Call parse-scheme*, parse-shell* or whatever is the current default parser.
        * Return parsed form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-form* in)\n"
       "  (let ((proc (parser-parse* (cdr default-parser))))\n"
       "    (proc in)))\n"
       "\n"
       /**
        * Call parse-scheme-list, parse-shell-list or whatever is the current default parser.
        * Return parsed form.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-form-list begin-type in)\n"
       "  (let ((proc (parser-parse-list (cdr default-parser))))\n"
       "    (proc begin-type in)))\n"
       "\n"
       "(let ((parser-scheme (make-parser parse-scheme parse-scheme* parse-scheme-list))\n"
       "      (parser-shell  (make-parser parse-shell  parse-shell*  parse-shell-list)))\n"
       "  (hashtable-set! known-parsers '#%scheme parser-scheme)\n"
       "  (hashtable-set! known-parsers '#%shell  parser-shell)\n"
       "  (set-car! default-parser '#%scheme)\n"
       "  (set-cdr! default-parser parser-scheme))\n"
       "\n"
       ")\n"); /* close library */

  eval("(import (schemesh parser))\n");
}
