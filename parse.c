/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

static void define_library_parser_base(void) {

#define SCHEMESH_LIBRARY_PARSER_BASE_EXPORT                                                        \
  "make-parser parser? parser-name parser-parse parser-parse* parser-parse-list "                  \
  "get-parser to-parser skip-whitespace "

  eval("(library (schemesh parser base (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_BASE_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) record-writer)\n"
       "    (only (schemesh bootstrap) while))\n"
       "\n"
       /**
        * parser is an object containing two functions:
        *   parser-parse will parse a single form,
        *   parser-parse-list will parse a list of forms.
        */
       "(define-record-type\n"
       "  (parser %make-parser parser?)\n"
       "  (fields name parse parse* parse-list)\n"
       "  (nongenerative #{parser cd39kg38a9c4cnwzwhghs827-24}))\n"
       "\n"
       /** create a new parser */
       "(define (make-parser name parse parse* parse-list)\n"
       "  (assert (symbol?    name))\n"
       "  (assert (procedure? parse))\n"
       "  (assert (procedure? parse*))\n"
       "  (assert (procedure? parse-list))\n"
       "  (%make-parser name parse parse* parse-list))\n"
       "\n"
       /**
        * Find and return the parser corresponding to given parser-name (which must be a symbol)
        * in enabled-parsers.
        * Raise (syntax-violation caller ...) if not found.
        */
       "(define (get-parser parser-name enabled-parsers caller)\n"
       "  (let ((parser (and enabled-parsers\n"
       "                     (hashtable-ref enabled-parsers parser-name #f))))\n"
       "    (unless parser\n"
       "      (syntax-violation caller \"no parser found for #!\" parser-name))\n"
       "    parser))\n"
       "\n"
       /**
        * Convert a parser name to parser:
        * if p is a parser, return p
        * if p is a symbol, return (get-parser p enabled-parsers caller)
        * otherwise raise condition
        */
       "(define (to-parser p enabled-parsers caller)\n"
       "  (if (parser? p)\n"
       "    p\n"
       "    (get-parser p enabled-parsers caller)))\n"
       "\n"
       /**
        * return #t if ch is a character and is <= ' '.
        * otherwise return #f if
        */
       "(define (is-whitespace-char? ch)\n"
       "  (and (char? ch) (char<=? ch #\\space)))\n"
       "\n"
       /**
        * read and discard all initial whitespace in textual input stream 'in'.
        * characters are considered whitespace if they are <= ' '
        */
       "(define (skip-whitespace in)\n"
       "  (while (is-whitespace-char? (peek-char in))\n"
       "    (read-char in)))\n"
       "\n"
       /** customize how "parser" objects are printed */
       "(record-writer (record-type-descriptor parser)\n"
       "  (lambda (p port writer)\n"
       "    (display \"#<parser \" port)\n"
       "    (display (parser-name p) port)\n"
       "    (display \">\" port)))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_parser_scheme(void) {

#define SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT "lex-scheme parse-scheme parse-scheme* parser-scheme "

  eval("(library (schemesh parser scheme (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme)\n"
       "       box bytevector fx1+\n"
       "       fxvector fxvector-set! make-fxvector\n"
       "       read-token reverse! unread-char)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (only (schemesh containers misc) reverse*!)\n"
       "    (schemesh containers charspan)\n"
       "    (schemesh parser base))\n"
       "\n"
       /**
        * return #t if ch1 and ch2 are both chars and they are equal.
        * otherwise return #f if
        */
       "(define (is-char=? ch1 ch2)\n"
       "  (and (char? ch1) (char? ch2) (char=? ch1 ch2)))\n"
       "\n"
       /**
        * return truthy if ch is a character whose value is a number,
        * or an ASCII letter, or '_', or greater than (integer->char 127).
        * Otherwise return #f
        */
       "(define (is-simple-identifier-char? ch)\n"
       "  (and (char? ch)\n"
       "       (or (and (char>=? ch #\\0) (char<=? ch #\\9))\n"
       "           (and (char>=? ch #\\A) (char<=? ch #\\Z))\n"
       "           (and (char>=? ch #\\a) (char<=? ch #\\z))\n"
       "           (char=? ch #\\_)\n"
       "           (char>? ch #\\delete))))\n"
       "\n"
       /**
        * First, skip whitespace in textual input port 'in'.
        * Then, if first token in textual input port 'in' is a parser-change token #!identifier
        * then read it and return it as a symbol, skipping the "#!" prefix.
        *
        * Otherwise do nothing and return #f i.e. do not consume any token or part of it
        * (will still skip whitespace).
        */
       "(define (try-read-parser-change in)\n"
       "  (skip-whitespace in)\n"
       "  (let ((ret #f))\n"
       "    (when (is-char=? #\\# (peek-char in))\n"
       "      (read-char in)\n"
       "      (if (is-char=? #\\! (peek-char in))\n"
       "        (let ((str (charspan)))\n"
       "          (charspan-reserve-back! str 10)\n"
       "          (read-char in)\n"
       "          (while (is-simple-identifier-char? (peek-char in))\n"
       "            (charspan-insert-back! str (read-char in)))\n"
       "          (set! ret (string->symbol (charspan->string str))))\n"
       "        (unread-char #\\# in)))\n"
       "    ret))\n"
       "\n"
       /**
        * Given textual input port 'in', read a single Scheme token from it.
        * Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
        * in pure R6RS.
        *
        * Return two values: token value and its type.
        */
       "(define (lex-scheme in enabled-parsers)\n"
       "  (let ((value (try-read-parser-change in)))\n"
       "    (if (symbol? value)\n"
       /*     cannot switch to other parser here: just return it and let caller switch */
       "      (values (get-parser value enabled-parsers 'parse-scheme) 'parser)\n"
       "      (let-values (((type value start end) (read-token in)))\n"
       "        (values value type)))))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read Scheme tokens from it with
        * (lex-scheme) and construct a Scheme form.
        *
        * Return two values: parsed Scheme form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        *
        * enabled-parsers must be either #f or an hashtable whose keys are symbols, and whose
        * values are be parser objects. When a symbol present in the hashtable is found in textual
        * input port while parsing a list, the rest of the list will be parsed by invoking the
        * parser corresponding to the symbol: this effectively allows changing the parser, and thus
        * the syntax, at any point in a list being parsed from the stream and until the current list
        * is finished.
        */
       "(define (parse-scheme in enabled-parsers)\n"
       "  (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
       "    (let-values (((ret-value ret-type) (parse-impl value type in enabled-parsers)))\n"
       "      (values ret-value (not (eq? 'eof ret-type))))))"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with (lex-scheme) and construct a
        * Scheme form. Return parsed Scheme form. Raise error if end-of-file is reached before
        * completely reading a form.
        *
        * enabled-parsers must be either #f or an hashtable whose keys are symbols, and whose
        * values are be parser objects - see (parse-scheme) for its effects.
        */
       "(define (parse-scheme* in enabled-parsers)\n"
       "  (let-values (((value ok) (parse-scheme in enabled-parsers)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-scheme \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it with (lex-scheme) and construct a
        * single Scheme form. Return two values: the form, and its type.
        */
       "(define (parse-impl value type in enabled-parsers)\n"
       "  (values\n"
       "    (case type\n"
       /*     cannot switch to other parser here: just return it and let caller switch */
       "      ((atomic eof parser) value)\n"
       "      ((box)               (box (parse-scheme* in enabled-parsers)))\n"
       "      ((lbrack lparen)     (parse-scheme-list type in '() enabled-parsers))\n"
       "      ((quote)             (list 'quote (parse-scheme* in enabled-parsers)))\n"
       "      ((vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)\n"
       "        (parse-vector type value in enabled-parsers))\n"
       "      (else   (syntax-violation 'parse-scheme \"unimplemented token type\" type)))\n"
       "    type))\n"
       "\n"
       /**
        * Given textual input port 'in', read Scheme forms from it, until a token ) or ] matching
        * the specified begin-type token is found, and return a list containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        *
        * The argument already-parsed-reverse will be reversed and prefixed to the returned list.
        */
       "(define (parse-scheme-list begin-type in already-parsed-reverse enabled-parsers)\n"
       "  (let ((ret already-parsed-reverse)\n"
       "        (again? #t)\n"
       "        (reverse? #t)\n"
       "        (check-rparen-or-rbrack (lambda (type)\n"
       "          (let ((end-type (if (eq? 'lbrack begin-type) 'rbrack 'rparen)))\n"
       "             (unless (eq? type end-type)\n"
       "               (if (eq? end-type 'rbrack)\n"
       "                 (syntax-violation 'parse-scheme \"unexpected token ], expecting )\"\n"
       "                   type)\n"
       "                 (syntax-violation 'parse-scheme \"unexpected token ), expecting ]\"\n"
       "                   type)))))))\n"
       "    (while again?\n"
       "      (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
       "        (case type\n"
       "          ((eof)     (syntax-violation 'parse-scheme \"unexpected\" type))\n"
       "          ((rparen rbrack)\n"
       "            (check-rparen-or-rbrack type)\n"
       "            (set! again? #f))\n"
       "          ((dot)\n"
       "            (let-values (((value-i type-i) (parse-scheme in enabled-parsers)))\n"
       "              (when (eq? 'parser type-i)\n"
       /*               switch to other parser */
       "                (let ((other-parse* (parser-parse* value-i)))\n"
       "                  (set! value-i (other-parse* in enabled-parsers))))\n"
       "              (set! ret (reverse*! (cons value-i ret)))\n"
       "              (set! reverse? #f)\n"
       "              (set! again? #f))\n"
       /*           then parse ) or ] */
       "            (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
       "              (check-rparen-or-rbrack type)))\n"
       "          ((parser)\n"
       /*           switch to other parser */
       "            (let ((other-parse-list (parser-parse-list value)))\n"
       "              (set! ret (other-parse-list begin-type in ret enabled-parsers))\n"
       "              (set! reverse? #f)\n"
       "              (set! again? #f)))\n"
       "          (else\n"
       "            (let-values (((value-i type-i)\n"
       "                            (parse-impl value type in enabled-parsers)))\n"
       "              (when (eq? 'eof type-i)\n"
       "                (syntax-violation 'parse-scheme \"unexpected\" type-i))\n"
       "              (set! ret (cons value-i ret)))))))\n"
       "    (if reverse? (reverse! ret) ret)))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read Scheme forms from it, until a token ) is
        * found, and return a vector, fxvector or bytevector containing such forms.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-vector vec-type length in enabled-parsers)\n"
       "  (let ((values (parse-scheme-list vec-type in '() enabled-parsers)))\n"
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
       "(define parser-scheme\n"
       "  (let ((ret (make-parser 'scheme parse-scheme parse-scheme* parse-scheme-list)))\n"
       "    (lambda ()\n"
       "      ret)))\n"
       "\n"
       ")\n"); /* close library */
}

static void define_library_parser_shell(void) {

#define SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT "parse-shell parse-shell* parser-shell "

  eval("(library (schemesh parser shell (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) reverse!)\n"
       "    (schemesh parser base))\n"
       "\n"
       /**
        * TODO: implement!
        *
        * Given textual input port 'in', repeatedly read from it using (read-shell-token)
        * and construct corresponding Scheme form.
        * Return two values: parsed Scheme form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-shell in enabled-parsers)\n"
       "  (values \"\" #f))\n"
       "\n"
       /**
        * Given textual input port 'in', repeatedly read from it using (read-shell-token)
        * and construct corresponding form.
        * Return parsed form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-shell* in enabled-parsers)\n"
       "  (let-values (((value ok) (parse-shell in enabled-parsers)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-shell* \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Given textual input port 'in', read shell forms from it, until a token ) or ] or }
        * matching the specified begin-type token is found, and return a list containing such
        * forms. Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-scheme-list begin-type in already-parsed-reverse enabled-parsers)\n"
       "  (reverse! already-parsed-reverse))\n"
       "\n"
       "(define parser-shell\n"
       "  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-scheme-list)))\n"
       "    (lambda ()\n"
       "      ret)))\n"
       "\n"
       ")\n"); /* close library */
}

void define_library_parser(void) {
  define_library_parser_base();
  define_library_parser_scheme();
  define_library_parser_shell();

#define SCHEMESH_LIBRARY_PARSER_EXPORT "parse-form parse-form* parse-form-list parse-forms parsers "

  eval("(library (schemesh parser (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_BASE_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-pairs)\n"
       "    (only (chezscheme) reverse!)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh parser base)\n"
       "    (schemesh parser scheme)\n"
       "    (schemesh parser shell))\n"
       "\n"
       /**
        * Call parse-scheme, parse-shell or whatever is the parser specified as initial-parser.
        *
        * Return two values: parsed form, and #t. If
        * end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-form in initial-parser enabled-parsers)\n"
       "  (let ((proc (parser-parse (to-parser initial-parser enabled-parsers 'parse-form))))\n"
       "    (proc in enabled-parsers)))\n"
       "\n"
       /**
        * Call parse-scheme*, parse-shell* or whatever is the parser specified as initial-parser.
        *
        * Return parsed form.
        * Raise error if end-of-file is reached before completely reading a form.
        */
       "(define (parse-form* in initial-parser enabled-parsers)\n"
       "  (let-values (((value ok) (parse-form in initial-parser enabled-parsers)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-form* \"unexpected end-of-file\" 'eof))\n"
       "    value))\n"
       "\n"
       /**
        * Parse textual input stream until the end of current list, using the parser specified by
        * initial-parser, and temporarily switching to other parsers if the directive #!...
        * is found in a (possibly nested) list being parsed.
        *
        * Return parsed list.
        * Raise error if mismatched end token is found, as for example ']' instead of ')'
        */
       "(define (parse-form-list begin-type in already-parsed-reverse\n"
       "                    initial-parser enabled-parsers)\n"
       "  (let ((proc (parser-parse-list\n"
       "                (to-parser initial-parser enabled-parsers 'parse-form-list))))\n"
       "    (proc begin-type in enabled-parsers)))\n"
       "\n"
       /**
        * Parse textual input stream until eof, using the parser specified by initial-parser,
        * and temporarily switching to other parsers every time the directive #!... is found
        * in a (possibly nested) list being parsed.
        *
        * Return two values: parsed form and final value of current-parser
        */
       "(define (parse-forms in initial-parser enabled-parsers)\n"
       "  (let ((current-parser (to-parser initial-parser enabled-parsers 'parse-forms))\n"
       "        (forms '())\n"
       "        (again #t))\n"
       "    (while again\n"
       "      (let-values (((form ok) (parse-form in current-parser enabled-parsers)))\n"
       "        (if ok\n"
       "          (if (parser? form)\n"
       "            (set! current-parser form)\n"
       "            (set! forms (cons form forms)))\n"
       "          (set! again #f))))\n"
       "    (values\n"
       "      (if (or (null? forms) (null? (cdr forms)))\n"
       "        (car forms)\n"
       "        (cons 'begin (reverse! forms)))\n"
       "      current-parser)))\n"
       /**
        * Return mutable hashtable containing all known parsers.
        */
       "(define parsers\n"
       "  (let ((ret (make-eq-hashtable)))\n"
       "    (hashtable-set! ret 'scheme (parser-scheme))\n"
       "    (hashtable-set! ret 'shell  (parser-shell))\n"
       "    (lambda ()\n"
       "      ret)))\n"
       "\n"
       ")\n"); /* close library */

  eval("(import (schemesh parser))\n");
}
