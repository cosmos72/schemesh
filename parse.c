/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

static void define_library_parser_scheme(void) {

#define SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT "parse-scheme parse-scheme* "

  eval("(library (schemesh parser scheme (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme)\n"
       "       box bytevector fx1+\n"
       "       fxvector fxvector-set! make-fxvector\n"
       "       read-token reverse!)\n"
       "    (only (schemesh bootstrap) while))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with Chez Scheme (read-token)
        * and construct corresponding Scheme form.
        * Return two values: parsed Scheme form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-scheme in)\n"
       "  (let-values (((type value start end) (read-token in)))\n"
       "    (let-values (((ret-type ret-value) (parse-form type value in)))\n"
       "      (values ret-value (not (eq? 'eof ret-type))))))"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with Chez Scheme (read-token)
        * and construct corresponding Scheme form.
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
        * Given textual input port 'in', read a single Scheme form from it.
        * Return two values: the type of the form, and its value.
        */
       "(define (parse-form type value in)\n"
       "  (values\n"
       "    type\n"
       "    (case type\n"
       "      ((atomic eof)     value)\n"
       "      ((box)            (box (parse-scheme* in)))\n"
       "      ((lbrack lparen)  (parse-list type value in))\n"
       "      ((quote)          (list 'quote (parse-scheme* in)))\n"
       "      ((vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)\n"
       "        (parse-vector type value in))\n"
       "      (else   (syntax-violation 'parse-scheme \"unimplemented token type\" type)))))\n"
       "\n"
       /**
        * Given textual input port 'in', read Scheme forms from it, until a token ) or ] matching
        * the specified begin-type token is found, and return a list containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-list begin-type begin-value in)\n"
       "  (let ((ret '())\n"
       "        (end-type (if (eq? 'lparen begin-type) 'rparen 'rbrack))\n"
       "        (again #t))\n"
       "    (while again\n"
       "      (let-values (((type value start end) (read-token in)))\n"
       "        (case type\n"
       "          ((eof)     (syntax-violation 'parse-scheme \"unexpected\" type))\n"
       "          ((rbrack)\n"
       "            (unless (eq? type end-type)\n"
       "              (syntax-violation 'parse-scheme \"unexpected token ], expecting )\" type))\n"
       "            (set! again #f))\n"
       "          ((rparen)\n"
       "            (unless (eq? type end-type)\n"
       "              (syntax-violation 'parse-scheme \"unexpected token ), expecting ]\" type))\n"
       "            (set! again #f))\n"
       "          (else\n"
       "            (let-values (((type-i value-i) (parse-form type value in)))\n"
       "              (when (eq? 'eof type-i)\n"
       "                (syntax-violation 'parse-scheme \"unexpected\" type-i))\n"
       "              (set! ret (cons value-i ret)))))))\n"
       "    (reverse! ret)))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read Scheme forms from it, until a token ) is
        * found, and return a vector, fxvector or bytevector containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-vector vec-type length in)\n"
       "  (let ((values (parse-list 'lparen #f in)))\n"
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

#define SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT "parse-shell parse-shell* "

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
        * and construct corresponding Scheme form.
        * Return parsed Scheme form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-shell* in)\n"
       "  (let-values (((value ok) (parse-shell in)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-shell \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Given textual input port 'in', read shell forms from it, until a token ) or ] or }
        * matching the specified begin-type token is found, and return a list containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-list begin-type begin-value in)\n"
       "  '())\n"
       "\n"
       ")\n"); /* close library */
}

void define_library_parser(void) {
  define_library_parser_scheme();
  define_library_parser_shell();

#if 0
#define SCHEMESH_LIBRARY_PARSER_EXPORT                                                             \
  "make-parser parser? parser-table-ref parser-table-set! parser-table parser-table-iterate "

  eval("(library (schemesh parser registry (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_REGISTRY_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (schemesh containers hashtable) hashtable-iterate))\n"
       "\n"
       "(define-record-type\n"
       "  (parser %make-parser parser?)\n"
       "  (fields parse-form parse-list))\n"
       "\n"
       "(define (make-parser parse-form parse-list)\n"
       "  (assert (procedure? parse-form))\n"
       "  (assert (procedure? parse-list))\n"
       "  (%make-parser parse-form parse-list))\n"
       "\n"
       "(define parser-table (make-eq-hashtable))\n"
       "\n"
       "(define (parser-table-ref name)\n"
       "  (hashtable-ref parser-table name #f))\n"
       "\n"
       "(define (parser-table-set! name parser-obj)\n"
       "  (assert (symbol? name))\n"
       "  (assert (fxpositive? (string-length (symbol->string name))))\n"
       "  (assert (char=? #\\$ (string-ref (symbol->string name) 0)))\n"
       "  (assert (parser? parser-obj))\n"
#if 1
       "  (display \"parser-table-set! \")"
       "  (display #\\space)"
       "  (display name)"
       "  (display #\\space)"
       "  (display parser-obj)"
       "  (display #\\linefeed)"
#endif
       "  (hashtable-set! parser-table name parser-obj))\n"
       "\n"
       /* call (proc name parser) on each registered parser */
       "(define (parser-table-iterate proc)\n"
       "  (hashtable-iterate parser-table proc))\n"
       "\n"
       ")\n"); /* close library */
#endif

  eval("(library (schemesh parser (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ")\n"
       "  (import\n"
       "    (schemesh parser scheme)\n"
       "    (schemesh parser shell))\n"
       "\n"
       ")\n"); /* close library */

  eval("(import (schemesh parser))\n");
}
