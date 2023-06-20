/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

#undef SCHEMESH_LIBRARY_PARSE_DEBUG

static void schemesh_define_library_parser_base(void) {

#define SCHEMESH_LIBRARY_PARSER_BASE_EXPORT                                                        \
  "make-parser parser? parser-name parser-parse parser-parse* parser-parse-list "                  \
  "get-parser to-parser skip-whitespace try-unread-char try-read-parser-directive "

  eval("(library (schemesh parser base (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_BASE_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) record-writer unread-char)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh containers charspan))\n"
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
       "      (syntax-violation caller \"no parser found for directive #!\" parser-name))\n"
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
        * otherwise return #f
        */
       "(define (is-whitespace-char? ch newline-is-whitespace?)\n"
       "  (and (char? ch) (char<=? ch #\\space)\n"
       "       (or newline-is-whitespace? (not (char=? ch #\\newline)))))\n"
       "\n"
       /**
        * read and discard all initial whitespace in textual input stream 'in'.
        * characters are considered whitespace if they are <= ' '
        */
       "(define (skip-whitespace in newline-is-whitespace?)\n"
       "  (while (is-whitespace-char? (peek-char in) newline-is-whitespace?)\n"
       "    (read-char in)))\n"
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
        * Try to unread a character from textual input port 'in'.
        *
        * Raise condition if Chez Scheme (unread-char ch in) fails:
        * it will happen ch is different from last character read from input port.
        */
       "(define (try-unread-char ch in)\n"
       "  (unread-char ch in)\n"
       "  (assert (eqv? ch (peek-char in))))\n"
       "\n"
       /**
        * Try to read a parser directive #!... from textual input port 'in'
        * Does NOT skip whitespace in input port.
        *
        * If port's first two characters are a parser directive #!
        * then read the symbol after it, and return such symbol.
        *
        * Otherwise do nothing and return #f i.e. do not consume any character or part of it.
        */
       "(define (try-read-parser-directive in)\n"
       "  (let ((ret #f))\n"
       "    (when (eqv? #\\# (peek-char in))\n"
       "      (read-char in)\n"
       "      (if (eqv? #\\! (peek-char in))\n"
       "        (let ((csp (charspan)))\n"
       "          (charspan-reserve-back! csp 10)\n"
       "          (read-char in)\n"
       "          (while (is-simple-identifier-char? (peek-char in))\n"
       "            (charspan-insert-back! csp (read-char in)))\n"
       "          (set! ret (string->symbol (charspan->string csp))))\n"
       "        (try-unread-char #\\# in)))\n"
       "    ret))\n"
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

static void schemesh_define_library_parser_scheme(void) {

#define SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT "lex-scheme parse-scheme parse-scheme* parser-scheme "

  eval(
      "(library (schemesh parser scheme (0 1))\n"
      "  (export " SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ")\n"
      "  (import\n"
      "    (rnrs)\n"
      "    (only (chezscheme)\n"
      "       box bytevector fx1+\n"
      "       fxvector fxvector-set! make-fxvector\n"
      "       read-token reverse!)\n"
      "    (only (schemesh bootstrap) while)\n"
      "    (only (schemesh containers misc) reverse*!)\n"
      "    (schemesh parser base))\n"
      "\n"
      /**
       * Read a single Scheme token from textual input port 'in.
       * Internally uses Chez Scheme (read-token) for simplicity, but could be reimplemented
       * in pure R6RS.
       *
       * Return two values: token value and its type.
       */
      "(define (lex-scheme in enabled-parsers)\n"
      "  (skip-whitespace in 'also-skip-newlines)\n"
      "  (let ((value (try-read-parser-directive in)))\n"
      "    (if (symbol? value)\n"
      "      (if (eq? 'eof value)\n"
      /*       yes, #!eof is an allowed directive:
       *       it injects (eof-object) in token stream, with type 'eof
       *       thus simulating an actual end-of-file in input port.
       *       Reason: historically used to disable the rest of a file, to help debugging */
      "        (values (eof-object) 'eof)\n"
      /*       cannot switch to other parser here: just return it and let caller switch */
      "        (values (get-parser value enabled-parsers 'parse-scheme) 'parser))\n"
      /*     read a single token with Chez Scheme (read-token),
       *     then replace (values '{ 'atomic) with (values #f 'lbrace)
       *     and replace (values '} 'atomic) with (values #f 'rbrace)
       *     because we use them to switch to shell parser. For example,
       *        {ls -l > log.txt}
       *     is equivalent to
       *        (#!shell ls -l > log.txt)
       */
      "      (let-values (((type value start end) (read-token in)))\n"
      "        (if (eq? 'atomic type)\n"
      "          (case value\n"
      "            ((\\x7B;) (values #f 'lbrace))\n"
      "            ((\\x7D;) (values #f 'rbrace))\n"
      "            (else     (values value type)))\n"
      "          (values value type))))))\n"
      "\n"
      /**
       * Return the symbol, converted to string,
       * of most token types returned by Chez Scheme (read-token),
       *
       * Also recognizes and converts to string the additional types
       * 'lbrace and 'rbrace introduced by (lex-scheme)
       */
      "(define (lex-type->string type)\n"
      "  (case type\n"
      "    ((box) \"#&\")   ((dot) \".\")    ((fasl) \"#@\")  ((insert) \"#N#\")\n"
      "    ((lbrace) \"{\") ((lbrack) \"[\") ((lparen) \"(\") ((mark) \"#N=\") ((quote) \"'\")\n"
      "    ((rbrace) \"}\") ((rbrack) \"]\") ((rparen) \")\") ((record-brack) \"#[\")\n"
      "    ((vfxnparen) \"#Nvfx\") ((vfxparen) \"#vfx\")\n"
      "    ((vnparen)   \"#Nv\")   ((vparen)   \"#v\")\n"
      "    ((vu8nparen) \"#Nvu8\") ((vu8paren) \"#vu8\")\n"
      "    (else \"???\")))\n"
      /**
       * Read Scheme tokens from textual input port 'in'
       * by repeatedly calling (lex-scheme) and construct a Scheme form.
       * Automatically change parser when directive #!... is found.
       *
       * Return two values: parsed form, and #t.
       * If end-of-file is reached, return (eof-object) and #f.
       */
      "(define (parse-scheme in enabled-parsers)\n"
      "  (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
      "    (let-values (((ret-value ret-type) (parse-scheme-impl value type in enabled-parsers)))\n"
      "      (values ret-value (not (eq? 'eof ret-type))))))\n"
      "\n"
      /**
       * Read Scheme tokens from textual input port 'in'
       * by repeatedly calling (lex-scheme) and construct a Scheme form.
       * Automatically change parser when directive #!... is found.
       *
       * Return parsed form.
       * Raises syntax-violation if end of file is reached before reading a complete form.
       */
      "(define (parse-scheme* in enabled-parsers)\n"
      "  (let-values (((value ok) (parse-scheme in enabled-parsers)))\n"
      /*   cannot switch to other parser here, and caller does not expect it => raise */
      "    (unless ok"
      "      (syntax-violation 'parse-scheme \"unexpected end-of-file\" 'eof))\n"
      "    (when (parser? value)\n"
      "      (syntax-violation 'parse-scheme \"parser directive #!... can only appear in lists, "
      "not in single-form contexts: #!\" (parser-name value)))\n"
      "    value))\n"
      "\n"
      /**
       * Common back-end of (parse-scheme) and (parse-scheme*)
       * Read Scheme tokens from textual input port 'in'
       * by repeatedly calling (lex-scheme) and construct a Scheme form.
       * Automatically change parser when directive #!... is found.
       *
       * Return two values: the parsed form, and its type.
       */
      "(define (parse-scheme-impl value type in enabled-parsers)\n"
      "  (values\n"
      "    (case type\n"
      /*     cannot switch to other parser here: just return it and let caller switch */
      "      ((atomic eof parser) value)\n"
      "      ((box)               (list 'box  (parse-scheme* in enabled-parsers)))\n"
      /*     if type = 'quote, value can be one of:
       *        'quote  'quasiquote  'unquote  'unquote-splicing
       *        'synyax 'quasisyntax 'unsyntax 'unsyntax-splicing */
      "      ((quote)             (list value (parse-scheme* in enabled-parsers)))\n"
      "      ((lbrack lparen)     (parse-scheme-list type in '() enabled-parsers))\n"
      /*     lbrace i.e. { switches to shell parser until corresponding rbrace i.e. } */
      "      ((lbrace)\n"
      "        (let ((other-parse-list (parser-parse-list\n"
      "                (get-parser 'shell enabled-parsers 'parse-scheme))))\n"
      "          (other-parse-list type in '() enabled-parsers)))\n"
      /*     parse the various vector types, with or without explicit length */
      "      ((vfxnparen vfxparen vnparen vparen vu8nparen vu8paren)\n"
      "        (parse-vector type value in enabled-parsers))\n"
      /**    TODO: ((record-brack) ... ) */
      "      (else   (syntax-violation 'parse-scheme \"unexpected token type\" type)))\n"
      "    type))\n"
      "\n"
      /**
       * Read Scheme forms from textual input port 'in', until a token ) or ] or } matching
       * the specified begin-type token is found.
       * Automatically change parser when directive #!... is found.
       *
       * Return return a list containing parsed forms.
       * Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
       *
       * The argument already-parsed-reverse will be reversed and prefixed to the returned list.
       */
      "(define (parse-scheme-list begin-type in already-parsed-reverse enabled-parsers)\n"
      "  (let* ((ret already-parsed-reverse)\n"
      "         (again? #t)\n"
      "         (reverse? #t)\n"
      "         (end-type (case begin-type\n"
      "                     ((lbrace) 'rbrace) ((lbrack) 'rbrack) (else 'rparen)))\n"
      "         (check-list-end (lambda (type)\n"
      "           (unless (eq? type end-type)\n"
      "             (syntax-violation\n"
      "               'parse-scheme\n"
      "               (string-append \"unexpected token \" (lex-type->string type)\n"
      "                  \", expecting \" (lex-type->string end-type))\n"
      "               type)))))\n"
      "    (while again?\n"
      "      (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
      "        (case type\n"
      "          ((eof)\n"
      "            (syntax-violation 'parse-scheme \"unexpected\" type))\n"
      "          ((parser)\n"
      /*           switch to other parser until the end of current list */
      "            (let ((other-parse-list (parser-parse-list value)))\n"
      "              (set! ret (other-parse-list begin-type in ret enabled-parsers))\n"
      "              (set! reverse? #f)\n"
      "              (set! again? #f)))\n"
      "          ((rparen rbrack rbrace)\n"
      "            (check-list-end type)\n"
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
      /*           then parse ) or ] or } */
      "            (let-values (((value type) (lex-scheme in enabled-parsers)))\n"
      "              (check-list-end type)))\n"
      "          (else\n"
      /*           parse a single form and append it */
      "            (let-values (((value-i type-i)\n"
      "                            (parse-scheme-impl value type in enabled-parsers)))\n"
      "              (when (eq? 'eof type-i)\n"
      "                (syntax-violation 'parse-scheme \"unexpected\" type-i))\n"
      "              (set! ret (cons value-i ret)))))))\n"
      "    (if reverse? (reverse! ret) ret)))\n"
      "\n"
      /**
       * Read Scheme forms from textual input port 'in' until a token ) or ] or } matching vec-type
       * is found.
       * Automatically change parser when directive #!... is found.
       *
       * Return a vector, fxvector or bytevector containing parsed forms.
       * Raise syntax-violation if mismatched end token is found, as for example ] instead of )
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

static void schemesh_define_library_parser_shell(void) {

#define SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT                                                       \
  "read-shell-char lex-shell parse-shell-word "                                                    \
  "parse-shell parse-shell* parse-shell-list parser-shell "

  eval("(library (schemesh parser shell (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme)\n"
#ifdef SCHEMESH_LIBRARY_PARSE_DEBUG
       "      format\n"
#endif
       "      reverse! unread-char)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh containers charspan)\n"
       "    (schemesh parser base))\n"
       "\n"
       "(define (paren-type->string type)\n"
       "  (case type\n"
       "    ((lparen) \"(\") ((lbrack) \"[\") ((lbrace) \"{\")\n"
       "    ((rparen) \")\") ((rbrack) \"]\") ((rbrace) \"}\")\n"
       "    ((backquote) \"`\") ((dollar+lparen) \"$(\")\n"
       "    (else \"???\")))\n"
       "\n"
       /**
        * Categorize a single character according to shell syntax.
        * Return the character's type.
        */
       "(define (char->type ch)\n"
       "  (if (eof-object? ch)\n"
       "    'eof\n"
       "    (case ch\n"
       "      ((#\\newline #\\;) 'separator)\n"
       /**    TODO: is this list complete?
        *     Note: (lex-shell-impl) will change type of #\& to 'separator
        *           unless it's followed by another #\& */
       "      ((#\\! #\\& #\\# #\\< #\\> #\\|) 'op)\n"
       "      ((#\\\") 'dquote)\n"
       "      ((#\\' ) 'quote)\n"
       "      ((#\\$ ) 'dollar)\n"
       "      ((#\\\\) 'backslash)\n"
       "      ((#\\` ) 'backquote)\n"
       "      ((#\\( ) 'lparen)\n"
       "      ((#\\) ) 'rparen)\n"
       "      ((#\\[ ) 'lbrack)\n"
       "      ((#\\] ) 'rbrack)\n"
       "      ((#\\{ ) 'lbrace)\n"
       "      ((#\\} ) 'rbrace)\n"
       "      (else    (if (char<=? ch #\\space) 'space 'char)))))\n"
       "\n"
       /** Convert a character whose type is 'op or 'separator to corresponding symbol */
       "(define (op->symbol ch)\n"
       "  (case ch\n"
       "    ((#\\newline #\\;) '\\x3c;)\n"
       "    ((#\\!) '!)\n"
       "    ((#\\&) '&)\n"
       "    ((#\\<) '<)\n"
       "    ((#\\>) '>)\n"
       "    ((#\\|) '\\x7c;)\n"
       "    (else (syntax-violation 'lex-shell\n"
       "            \"unexpected operator character, cannot convert to symbol\" ch))))\n"
       "\n"
       /**
        * Peek a single character from textual input port 'in',
        * and categorize it according to shell syntax.
        * Return two values: character value (or eof) and its type.
        */
       "(define (peek-shell-char in)\n"
       "  (let ((ch (peek-char in)))\n"
       "    (values ch (char->type ch))))\n"
       "\n"
       "\n"
       /**
        * Read a single character from textual input port 'in',
        * and categorize it according to shell syntax.
        * Return two values: character value (or eof) and its type.
        */
       "(define (read-shell-char in)\n"
       "  (let ((ch (read-char in)))\n"
       "    (values ch (char->type ch))))\n"
       "\n"
       "\n"
       /** Read a single character, suppressing any special meaning it may have */
       "(define (read-char-after-backslash in csp-already-read)\n"
       "  (let ((ch (read-char in)))\n"
       "    (cond\n"
       "      ((eof-object? ch)\n"
       "        (syntax-violation 'lex-shell\n"
       "          \"unexpected end-of-file after backslash\"\n"
       "          (if csp-already-read (charspan->string csp-already-read) \"\")\n"
       "          'eof))\n"
       "      ((eqv? ch #\\newline)\n"
       /*       backslash followed by newline -> ignore both */
       "        #f)\n"
       "      (#t ch))))\n"
       "\n"
       "\n"
       /** Read a subword starting with ${ */
       "(define (read-subword-dollar-braced in)\n"
       "  (assert (eqv? #\\{ (read-char in)))\n"
       "  (let ((csp (charspan))\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let-values (((ch type) (read-shell-char in)))\n"
       "        (case type\n"
       "          ((eof)\n"
       "            (syntax-violation 'parse-shell\n"
       "              \"unexpected end-of-file after ${\" type))\n"
       "          ((rbrace)\n"
       "            (set! again? #f))\n"
       "          (else\n"
       "            (charspan-insert-back! csp ch)))))\n"
       "    (list 'shell-env-ref (charspan->string csp))))\n"
       "\n"
       "\n"
       /** Read an unquoted subword starting with $ */
       "(define (read-subword-dollar-unquoted in)\n"
       "  (let ((csp (charspan))\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let ((ch (read-char in)))\n"
       "        (cond\n"
       "          ((eof-object? ch)\n"
       "            (set! again? #f))\n"
       "          ((char=? #\\\\ ch)\n"
       /*           read next char, suppressing any special meaning it may have */
       "            (let ((ch-i (read-char-after-backslash in csp)))\n"
       "              (when ch-i (charspan-insert-back! csp ch-i))))\n"
       "          ((or (char<=? #\\0 ch #\\9)\n"
       "               (char<=? #\\A ch #\\Z)\n"
       "               (char<=? #\\a ch #\\z)\n"
       "               (char=?  #\\_ ch))\n"
       "            (charspan-insert-back! csp ch))\n"
       "          (#t\n"
       "            (set! again? #f)\n"
       "            (try-unread-char ch in)))))\n"
       "    (list 'shell-env-ref (charspan->string csp))))\n"
       "\n"
       "\n"
       /** Read a subword starting with $ */
       "(define (read-subword-dollar in enabled-parsers)\n"
       "  (assert (eqv? #\\$ (read-char in)))\n"
       "  (let-values (((ch type) (peek-shell-char in)))\n"
       "    (case type\n"
       "      ((eof)\n"
       "        (syntax-violation 'parse-shell \"unexpected end-of-file after $\"))\n"
       "      ((lparen)\n"
       "        (read-char in)\n" /* consume ( */
       /*       read a shell list surrounded by $(...) */
       "        (parse-shell-list 'dollar+lparen in '() enabled-parsers))\n"
       "      ((lbrace)\n"
       "        (read-subword-dollar-braced in))\n"
       "      (else\n"
       "        (read-subword-dollar-unquoted in)))))\n"
       "\n"
       /**
        * Read a single-quoted subword, stopping after the matching single quote.
        * Example: 'some text'
        */
       "(define (read-subword-quoted in)\n"
       "  (assert (eqv? #\\' (read-char in)))\n"
       "  (let ((csp (charspan))\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let ((ch (read-char in)))\n"
       "        (cond\n"
       "          ((eof-object? ch)\n"
       "            (syntax-violation 'lex-shell \"unexpected end-of-file inside quoted string:\"\n"
       "              (charspan->string csp)))\n"
       "          ((eqv? ch #\\')\n"
       "            (set! again? #f))\n" /* end of string reached  */
       "          (#t\n"
       "            (charspan-insert-back! csp ch)))))\n"
       "    (charspan->string csp)))\n"
       "\n"
       /**
        * Read a subword AFTER double quotes, stopping BEFORE the matching double quote.
        * Example: "some text"
        */
       "(define (read-subword-inside-dquotes in)\n"
       "  (let ((csp (charspan))\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let-values (((ch type) (read-shell-char in)))\n"
       "        (case type\n"
       "          ((eof)\n"
       "            (set! again? #f))\n"
       "          ((dquote dollar backquote)\n"
       "            (try-unread-char ch in)\n"
       "            (set! again? #f))\n"
       "          ((backslash)\n"
       /*           read next char, suppressing any special meaning it may have */
       "            (let ((ch-i (read-char-after-backslash in csp)))\n"
       "              (when ch-i (charspan-insert-back! csp ch-i))))\n"
       "          (else\n"
       /*           single quote, newline, semicolon, operators and parentheses
        *           have no special meaning inside dquotes */
       "            (charspan-insert-back! csp ch)))))\n"
       "    (charspan->string csp)))\n"
       "\n"
       "\n"
       /* Read an unquoted subword: a portion of a word, not inside single or double quotes */
       "(define (read-subword-noquote in)\n"
       "  (let ((csp (charspan))\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let-values (((ch type) (read-shell-char in)))\n"
       "        (case type\n"
       "          ((backslash)\n"
       /*           read next char, suppressing any special meaning it may have */
       "            (let ((ch-i (read-char-after-backslash in csp)))\n"
       "              (when ch-i (charspan-insert-back! csp ch-i))))\n"
       "          ((char)\n"
       "            (charspan-insert-back! csp ch))\n"
       "          (else\n"
       /*
        *           treat anything else as string delimiter. This means in our shell parser the
        *           characters ( ) [ ] { } retain their meaning when found inside an unquoted
        *           string.
        *           Reason: we want to allow writing things like {ls -l | wc} without users having
        *           to worry whether semicolons are needed or not before the }.
        *
        *           That's intentionally different from posix shell,
        *           where characters [ ] { } inside a string have no special meaning,
        *           and where characters ( ) inside a string are a syntax error.
        */
       "            (set! again? #f)\n"
       "            (try-unread-char ch in)))))\n"
       "    (charspan->string csp)))\n"
       "\n"
       /* Read a word, possibly containing single or double quotes and shell variables,
        * as for example: some$foo' text'"other text ${bar} " */
       "(define (parse-shell-word in enabled-parsers)\n"
       "  (let* ((ret '())\n"
       "         (again? #t)\n"
       "         (dquote? #f)\n"
       "         (%append (lambda (subword)\n"
       "           (unless (and (string? subword) (fxzero? (string-length subword)))\n"
       "             (set! ret (cons subword ret))))))\n"
       "    (while again?\n"
       "      (let-values (((ch type) (peek-shell-char in)))\n"
       "        (case type\n"
       "          ((eof)\n"
       "            (when dquote?\n"
       "              (syntax-violation 'parse-shell\n"
       "                \"unexpected end-of-file inside quoted string\" (reverse! ret) type))\n"
       "            (set! again? #f))\n"
       "          ((quote)\n"
       "            (%append (if dquote? (read-subword-inside-dquotes in)\n"
       "                                 (read-subword-quoted in))))\n"
       "          ((dquote)\n"
       "            (set! dquote? (not dquote?))\n"
       "            (read-char in))\n"
       "          ((dollar)\n"
       "            (%append (read-subword-dollar in enabled-parsers)))\n"
       "          (else\n"
       "            (cond\n"
       "              (dquote?\n"
       "                (%append (read-subword-inside-dquotes in)))\n"
       "              ((memq type '(backslash char))\n"
       "                (%append (read-subword-noquote in)))\n"
       /*
        *           treat anything else as string delimiter. This means in our shell parser the
        *           characters ( ) [ ] { } retain their meaning when found inside an unquoted
        *           string.
        *           Reason: we want to allow writing things like {ls -l | wc} without users having
        *           to worry whether semicolons are needed or not before the }.
        *
        *           That's intentionally different from posix shell,
        *           where [ ] { } inside a string are treated as regular characters,
        *           and where ( ) inside a string are a syntax error.
        */
       "              (#t\n"
       "                (set! again? #f)))))))\n"
       "    (cond\n"
       "      ((null? ret)       \"\")\n"
       "      ((null? (cdr ret)) (car ret))\n"
       "      (#t  (cons 'shell-concat (reverse! ret))))))\n"
       "\n"
       /**
        * Read a single shell token from textual input port 'in'.
        * Return two values: token value and its type.
        * Does not skip initial whitespace, and does not recognize parser directives #!...
        * use (lex-shell) for that.
        *
        * The definition of shell token is adapted from
        * https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_03
        */
       "(define (lex-shell-impl in enabled-parsers)\n"
       "  (let-values (((ch type) (read-shell-char in)))\n"
       "    (case type\n"
       "      ((eof separator lparen rparen lbrack rbrack lbrace rbrace)\n"
       "        (values ch type))\n"
       /**    TODO: handle missing multi-character operators #... N< N<> N<& N> N>> N>| N>& */
       "      ((op)\n"
       "        (let ((ch2 (peek-char in)))\n"
       "          (case ch\n"
       "            ((#\\&) (if (eqv? ch2 #\\&)\n"
       "                      (set! ch '&&)\n"
       "                      (set! type 'separator)))\n"
       "            ((#\\|) (cond ((eqv? ch2 #\\&) (set! ch '\\x7c;&))\n"
       "                          ((eqv? ch2 #\\|) (set! ch '\\x7c;\\x7c;))))\n"
       "            ((#\\<) (cond ((eqv? ch2 #\\>) (set! ch '<>))\n"
       "                          ((eqv? ch2 #\\&) (set! ch '<&))))\n"
       "            ((#\\>) (cond ((eqv? ch2 #\\>) (set! ch '>>))\n"
       "                          ((eqv? ch2 #\\&) (set! ch '>&))\n"
       "                          ((eqv? ch2 #\\|) (set! ch '>\\x7c;))))))\n"
       "        (if (symbol? ch)\n"
       "          (read-char in)\n"             /* consume peeked character */
       "          (set! ch (op->symbol ch)))\n" /* convert character to symbol */
       "        (values ch type))\n"
       "      ((dollar)\n"
       "        (if (eqv? #\\( (peek-char in))\n"
       "          (values (read-char in) 'dollar+lparen)\n"
       "          (begin\n"
       "            (try-unread-char ch in)\n"
       "            (values (parse-shell-word in enabled-parsers) 'string))))\n"
       "      ((backquote)\n"
       "        (values ch type))\n"
       "      ((char quote dquote backslash)\n"
       /**      TODO: handle ~ and path-based wildcards */
       "        (try-unread-char ch in)\n"
       "        (values (parse-shell-word in enabled-parsers) 'string))\n"
       "      (else\n"
       "        (syntax-violation 'lex-shell \"unimplemented character type:\" type)))))\n"
       /**
        * Read a single shell token from textual input port 'in'.
        * Return two values: token value and its type.
        * Also recognizes parser directives #!... and returns them with type 'parser.
        */
       "(define (lex-shell in enabled-parsers)\n"
       "  (skip-whitespace in #f)\n" /* don't skip newlines */
       "  (let ((value (try-read-parser-directive in)))\n"
       "    (if (symbol? value)\n"
       "      (if (eq? 'eof value)\n"
       /*       yes, #!eof is an allowed directive:
        *       it injects (eof-object) in token stream, with type 'eof
        *       thus simulating an actual end-of-file in input port.
        *       Reason: historically used to disable the rest of a file, to help debugging */
       "        (values (eof-object) 'eof)\n"
       /*       cannot switch to other parser here: just return it and let caller switch */
       "        (values (get-parser value enabled-parsers 'parse-shell) 'parser))\n"
       /*     read a single shell token */
       "      (lex-shell-impl in enabled-parsers))))\n"
       "\n"
       /**
        * Repeatedly read from textual input port 'in' using (lex-shell)
        * and construct corresponding shell form.
        * Automatically change parser when directive #!... is found in a nested list.
        *
        * Return two values: parsed form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-shell in enabled-parsers)\n"
       "  (let-values (((value type) (lex-shell in enabled-parsers)))\n"
       "    (values\n"
       "      (case type\n"
       /*       cannot switch to other parser here: just return it and let caller switch */
       "        ((eof parser) value)\n"
       "        ((lbrace)\n"
       /*         read a shell list surrounded by {...} */
       "          (parse-shell-list type in '() enabled-parsers))\n"
       "        (else\n"
       "          (parse-shell-impl value type in enabled-parsers #f)))\n"
       "      (not (eq? 'eof type)))))\n"
       "\n"
       /**
        * Repeatedly read shell tokens from textual input port 'in' using (lex-shell)
        * and construct corresponding form.
        * Automatically change parser when directive #!... is found in a nested list.
        *
        * Return parsed form.
        * Raise syntax-violation if end-of-file is reached before completely reading a form.
        */
       "(define (parse-shell* in enabled-parsers)\n"
       "  (let-values (((value ok) (parse-shell in enabled-parsers)))\n"
       "    (unless ok"
       "      (syntax-violation 'parse-shell* \"unexpected end-of-file\" 'eof))\n"
       "    (when (parser? value)\n"
       "      (syntax-violation 'parse-shell \"parser directive #!... can only appear in lists, "
       "not in single-form contexts: #!\" (parser-name value)))\n"
       "    value))\n"
       "\n"
       /** Common backend of (parse-shell) (parse-shell*) and (parse-shell-list) */
       "(define (parse-shell-impl value type in enabled-parsers is-inside-backquote?)\n"
       "  (let ((ret (list 'shell))\n"
       "        (again? #t)\n"
       "        (reverse? #t))\n"
       "    (while again?\n"
#ifdef SCHEMESH_LIBRARY_PARSE_DEBUG
       "      (format #t \"parse-shell-impl: value = ~s, type = ~s~%\" value type)\n"
#endif
       "      (case type\n"
       "        ((eof)\n"
       "          (set! again? #f))\n"
       "        ((parser)\n"
       "          (syntax-violation 'parse-shell \"parser directive #!... can only appear "
       "before or after a shell command, not in the middle of it: #!\" (parser-name value)))\n"
       "        ((separator)\n"
       "          (when (eq? value '&)\n" /* append final & to command */
       "            (set! ret (cons value ret)))\n"
       "          (set! again? #f))\n"
       "        ((op string)\n"
       "          (set! ret (cons value ret)))\n"
       "        ((backquote dollar+lparen)\n"
       "          (if (and is-inside-backquote? (eq? 'backquote type))\n"
       /*           we read one token too much - try to unread it */
       "            (begin\n"
       "              (set! again? #f)\n"
       "              (try-unread-char value in))\n"
       /*           parse nested shell list surrounded by `...` or $(...) */
       "            (set! ret (cons (parse-shell-list type in '() enabled-parsers) ret))))\n"
       "        ((lparen lbrack)\n"
       /*         switch to Scheme parser for a single form.
        *         Convenience: if the first word is #\(, omit the initial (shell ...)
        *         and set again? to #f. This allows entering Scheme forms from shell syntax */
       "          (when (equal? '(shell) ret)\n"
       "            (set! ret '())\n"
       "            (set! again? #f)\n"
       /*           forms returned by (parser-parse-list) are already reversed */
       "            (set! reverse? #f))\n"
       "          (let* ((other-parse-list (parser-parse-list\n"
       "                   (get-parser 'scheme enabled-parsers 'parse-shell)))\n"
       "                 (form (other-parse-list type in '() enabled-parsers)))\n"
       "            (set! ret (if (null? ret) form (cons form ret)))))\n"
       "        ((lbrace)\n"
       "          (if (or (null? (cdr ret)) (memq (car ret) '(! & && \\x7c; \\x7c;\\x7c;)))\n"
       /*           parse nested shell list surrounded by {...} */
       "            (begin\n"
       "              (set! again? #f)\n"
       "              (set! ret (cons (parse-shell-list type in '() enabled-parsers) ret)))\n"
       /*           character { is not allowed in the middle of a shell command */
       "            (syntax-violation 'parse-shell \"misplaced { in the middle of shell command, "
       "can only be at the beginning:\" (reverse! (cons value ret)) type)))\n"
       "        ((rparen rbrack rbrace)\n"
       /*         we read one token too much - try to unread it */
       "          (set! again? #f)\n"
       "          (try-unread-char value in))\n"
       "        (else\n"
       "          (syntax-violation 'parse-shell \"unexpected token type\"\n"
       "            (reverse! ret) type)))\n"
       /*     if needed, read another token and iterate */
       "      (when again?\n"
       "        (let-values (((value-i type-i) (lex-shell in enabled-parsers)))\n"
       "          (set! value value-i)\n"
       "          (set! type type-i))))\n"
       /*   shell form is complete, return it */
       "    (if reverse? (reverse! ret) ret)))\n"
       "\n"
       /**
        * Read shell forms from textual input port 'in' until a token } or ] or )
        * matching the specified begin-type token is found.
        * Automatically change parser when directive #!... is found.
        *
        * Return a list containing 'shell-list followed by such forms.
        * Raise syntax-violation if mismatched end token is found, as for example ')' instead of '}'
        */
       "(define (parse-shell-list begin-type in already-parsed-reverse enabled-parsers)\n"
       "  (let* ((first-token (case begin-type\n"
       "           ((backquote dollar+lparen)\n"
       "             (unless (null? already-parsed-reverse)\n"
       "               (syntax-violation 'parse-shell \"unimplemented backquote in the middle of "
       "non-shell commands, it currently can only be inside shell commands:\" "
       "(reverse! (cons begin-type already-parsed-reverse)) begin-type))\n"
       "             'shell-backquote)\n"
       "           (else 'shell-list)))\n"
       "         (ret (if (null? already-parsed-reverse)\n"
       "                (cons first-token already-parsed-reverse)\n"
       "                already-parsed-reverse))\n"
       "         (again? #t)\n"
       "         (reverse? #t)\n"
       "         (end-type (case begin-type\n"
       "                     ((lbrace) 'rbrace) ((lbrack) 'rbrack)\n"
       "                     ((backquote) 'backquote) (else 'rparen)))\n"
       "         (check-list-end (lambda (type)\n"
       "           (unless (eq? type end-type)\n"
       "             (syntax-violation\n"
       "               'parse-shell\n"
       "               (string-append \"unexpected token \" (paren-type->string type)\n"
       "                  \", expecting \" (paren-type->string end-type))\n"
       "               type)))))\n"
       "    (while again?\n"
       "      (let-values (((value type) (lex-shell in enabled-parsers)))\n"
#ifdef SCHEMESH_LIBRARY_PARSE_DEBUG
       "      (format #t \"parse-shell-list ret=~s value=~s type=~s~%\" (reverse ret) value type)\n"
#endif
       "        (case type\n"
       "          ((eof)\n"
       "            (syntax-violation 'parse-shell-list \"unexpected end-of-file after\"\n"
       "              (if reverse? (reverse! ret) ret)))\n"
       "          ((parser)\n"
       /*           switch to other parser until the end of current list */
       "            (let ((other-parse-list (parser-parse-list value)))\n"
       "              (set! ret (other-parse-list begin-type in ret enabled-parsers)))\n"
       "            (set! reverse? #f)\n"
       "            (set! again? #f))\n"
       "          ((rparen rbrack rbrace)\n"
       "            (check-list-end type)\n"
       "            (set! again? #f))\n"
       "          ((lbrace)\n"
       /*           parse nested shell list */
       "            (let ((nested-list (parse-shell-list type in '() enabled-parsers)))\n"
       "              (set! ret (cons nested-list ret))))\n"
       "          (else\n"
       "            (if (and (eq? 'backquote begin-type) (eq? 'backquote type))\n"
       /*             end of backquote reached */
       "              (begin\n"
       "                (check-list-end type)\n"
       "                (set! again? #f))\n"
       /*             parse a single shell form and accumulate it into ret */
       "              (let ((value (parse-shell-impl value type in enabled-parsers\n"
       "                             (eq? 'backquote begin-type))))\n"
       "                (set! ret (cons value ret))))))))\n"
       "    (if reverse? (reverse! ret) ret)))\n"
       "\n"
       "(define parser-shell\n"
       "  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-shell-list)))\n"
       "    (lambda ()\n"
       "      ret)))\n"
       "\n"
       ")\n"); /* close library */
}

void schemesh_define_library_parser(void) {
  schemesh_define_library_parser_base();
  schemesh_define_library_parser_scheme();
  schemesh_define_library_parser_shell();

#define SCHEMESH_LIBRARY_PARSER_EXPORT "parse-form parse-form* parse-form-list parse-forms parsers "

  eval("(library (schemesh parser (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_BASE_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SCHEME_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ""
       /*        */ SCHEMESH_LIBRARY_PARSER_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-pairs)\n"
       "    (only (chezscheme) reverse! void)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh parser base)\n"
       "    (schemesh parser scheme)\n"
       "    (schemesh parser shell))\n"
       "\n"
       /**
        * Call parse-scheme, parse-shell or whatever is the parser specified as initial-parser.
        * Automatically change parser when directive #!... is found.
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
        * Raise syntax-violation if end-of-file is reached before completely reading a form.
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
        * Raise syntax-violation if mismatched end token is found, as for example ']' instead of ')'
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
        * Return two values.
        * First value is parsed forms: each element in the list is a parsed form.
        * Second value is updated parser to use.
        */
       "(define (parse-forms in initial-parser enabled-parsers)\n"
       "  (let ((current-parser (to-parser initial-parser enabled-parsers 'parse-forms))\n"
       "        (ret '())\n"
       "        (again? #t))\n"
       "    (while again?\n"
       "      (let-values (((form ok) (parse-form in current-parser enabled-parsers)))\n"
       "        (if ok\n"
       "          (if (parser? form)\n"
       "            (set! current-parser form)\n"
       "            (set! ret (cons form ret)))\n"
       "          (set! again? #f))))\n"
       "    (values\n"
       "      (reverse! ret)\n"
       "      current-parser)))\n"
       /**
        * Return mutable hashtable containing all known parsers.
        */
       "(define parsers\n"
       "  (let ((ret (make-eq-hashtable)))\n"
       "    (hashtable-set! ret 'chezscheme (parser-scheme))\n"
       "    (hashtable-set! ret 'scheme   (parser-scheme))\n"
       "    (hashtable-set! ret 'shell  (parser-shell))\n"
       "    (lambda ()\n"
       "      ret)))\n"
       "\n"
       ")\n"); /* close library */
}
