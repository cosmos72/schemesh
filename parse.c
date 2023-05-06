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
  "get-parser to-parser skip-whitespace try-read-parser-directive "

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
       "      (if (eq? 'eof value)"
       /*       yes, #!eof is an allowed directive:
        *       it injects (eof-object) in token stream, with type 'atomic */
       "        (values (eof-object) 'atomic)\n"
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
       "    (let-values (((ret-value ret-type) (parse-impl value type in enabled-parsers)))\n"
       "      (values ret-value (not (eq? 'eof ret-type))))))"
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
       "    (when (parser? value)"
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
       "(define (parse-impl value type in enabled-parsers)\n"
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
       /*     TODO: ((record-brack) ... ) */
       "      (else   (syntax-violation 'parse-scheme \"unimplemented token type\" type)))\n"
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
       "          ((parser)\n"
       /*           switch to other parser until the end of current list */
       "            (let ((other-parse-list (parser-parse-list value)))\n"
       "              (set! ret (other-parse-list begin-type in ret enabled-parsers))\n"
       "              (set! reverse? #f)\n"
       "              (set! again? #f)))\n"
       "          (else\n"
       /*           parse a single form and append it */
       "            (let-values (((value-i type-i)\n"
       "                            (parse-impl value type in enabled-parsers)))\n"
       "              (when (eq? 'eof type-i)\n"
       "                (syntax-violation 'parse-scheme \"unexpected\" type-i))\n"
       "              (set! ret (cons value-i ret)))))))\n"
       "    (if reverse? (reverse! ret) ret)))\n"
       "\n"
       /**
        * Read Scheme forms from textual input port 'in' until a token ) is found.
        * Automatically change parser when directive #!... is found.
        *
        * Return a vector, fxvector or bytevector containing parsed forms.
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

#define SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT                                                       \
  "read-shell-char lex-shell parse-shell parse-shell* parse-shell-list parser-shell "

  eval("(library (schemesh parser shell (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_PARSER_SHELL_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) reverse! unread-char)\n"
       "    (only (schemesh bootstrap) while)\n"
       "    (schemesh containers charspan)\n"
       "    (schemesh parser base))\n"
       "\n"
       /**
        * Read a single character from textual input port 'in',
        * and categorize it according to shell syntax.
        * Return two values: character value (or eof) and its type.
        */
       "(define (read-shell-char in)\n"
       "  (let ((ch (read-char in)))\n"
       "    (values ch\n"
       "      (if (eof-object? ch)\n"
       "        'eof\n"
       "        (case ch\n"
       "          ((#\\newline #\\; #\\&) 'separator)\n"
       /**        TODO: complete this list */
       "          ((#\\! #\\# #\\< #\\> #\\| #\\~) 'op)\n"
       "          ((#\\\") 'dquote)\n"
       "          ((#\\' ) 'quote)\n"
       "          ((#\\\\) 'backslash)\n"
       "          ((#\\` ) 'backquote)\n"
       "          ((#\\( ) 'lparen)\n"
       "          ((#\\) ) 'rparen)\n"
       "          ((#\\[ ) 'lbrack)\n"
       "          ((#\\] ) 'rbrack)\n"
       "          ((#\\{ ) 'lbrace)\n"
       "          ((#\\} ) 'rbrace)\n"
       "          (else    (if (char<=? ch #\\space) 'space 'char)))))))\n"
       "\n"
       /* read a string inside double quotes, as for example "some text" */
       "(define (read-shell-dquote in)\n"
       /** TODO: implement */
       "  (values \"\" 'string))\n"
       "\n"
       /* read a string inside single quotes, as for example 'some text' */
       "(define (read-shell-quote in)\n"
       /** TODO: implement */
       "  (values \"\" 'quoted-string))\n"
       "\n"
       /* read a character after backslash, as for example \$ */
       "(define (read-shell-backslash in)\n"
       /** TODO: implement */
       "  (values \"\" 'quoted-string))\n"
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
       "(define (lex-shell-impl in)\n"
       "  (let-values (((ch type) (read-shell-char in)))\n"
       "    (case type\n"
       /**    TODO: also handle multi-character operators as && || N> N< >> << etc. */
       "      ((eof separator op lparen rparen lbrack rbrack lbrace rbrace)\n"
       "        (values ch type))\n"
       "      ((char)\n"
       "        (let ((str (charspan ch))\n"
       "              (again? #t))\n"
       "          (while again?\n"
       "            (let-values (((ch-i type-i) (read-shell-char in)))\n"
       "              (if (eq? 'char type-i)\n"
       "                (charspan-insert-back! str ch-i)\n"
       "                (begin\n"
       "                  (unread-char ch-i in)\n"
       "                  (set! again? #f)))))\n"
       "          (values (charspan->string str) 'string)))"
       "      (else\n"
       "        (syntax-violation 'lex-shell \"unimplemented character type:\" type)))))\n"
       /**
        * Given textual input port 'in', read a single shell token from it.
        * Return two values: token value and its type.
        * Also recognizes parser directives #!... and returns them with type 'parser.
        */
       "(define (lex-shell in enabled-parsers)\n"
       "  (skip-whitespace in #f)\n" /* don't skip newlines */
       "  (let ((value (try-read-parser-directive in)))\n"
       "    (if (symbol? value)\n"
       "      (if (eq? 'eof value)"
       /*       yes, #!eof is an allowed directive:
        *       it injects (eof-object) in token stream, with type 'atomic */
       "        (values (eof-object) 'atomic)\n"
       /*       cannot switch to other parser here: just return it and let caller switch */
       "        (values (get-parser value enabled-parsers 'parse-shell) 'parser))\n"
       /*     read a single shell token */
       "      (lex-shell-impl in))))\n"
       "\n"
       /**
        * Repeatedly read from textual input port 'in' using (lex-shell)
        * and construct corresponding shell form.
        * Automatically change parser when directive #!... is found.
        *
        * Return two values: parsed form, and #t.
        * If end-of-file is reached, return (eof-object) and #f.
        */
       "(define (parse-shell in enabled-parsers)\n"
       "  (let ((ret '())\n"
       "        (again? #t)"
       "        (reverse? #t)\n"
       "        (eof? #f))\n"
       "    (while again?\n"
       "      (let-values (((value type) (lex-shell in enabled-parsers)))\n"
       /* "        (format #t \"parse-shell: value = ~s, type = ~s~%\" value type)\n" */
       "        (case type\n"
       "          ((eof separator)\n"
       "            (set! eof? (eq? 'eof type))\n"
       "            (when (eq? #\\& value)\n" /* append final & to command */
       "              (set! ret (cons value ret)))\n"
       "            (set! again? #f))\n"
       /**        TODO: handle operators precedence and associativity */
       "          ((op string)     (set! ret (cons value ret)))\n"
       "          ((quoted-string) (set! ret (cons (list 'quote value) ret)))\n"
       "          ((backquote lbrack lbrace)\n"
       /*           read a shell list surrounded by `...` or by [...] or by {...} */
       "            (set! ret (cons (parse-shell-list type in '() enabled-parsers) ret)))\n"
       "          ((lparen)\n"
       /*           switch to Scheme parser for a single form */
       "            (let ((other-parse-list (parser-parse-list\n"
       "                    (get-parser 'scheme enabled-parsers 'parse-shell))))\n"
       "              (set! ret (cons (other-parse-list type in '() enabled-parsers) ret))))\n"
       "          ((rparen rbrack rbrace)\n"
       /*           we read one token too much - try to unread it */
       "            (unread-char value in)\n"
       "            (assert (eq? value (peek-char in)))\n"
       "            (set! again? #f))\n"
       "          ((parser)\n"
       "            (unless (null? ret)\n"
       "              (syntax-violation 'parse-shell \"parser directive #!... can only appear "
       "before or after a shell command, not in the middle of it: #!\" (parser-name value)))\n"
       /*           cannot switch to other parser here: just return it and let caller switch */
       "            (values (get-parser value enabled-parsers 'parse-shell) 'parser))\n"
       "          (else\n"
       "            (syntax-violation 'parse-shell \"unimplemented token type\" type)))))\n"
       "    (when reverse?\n"
       "      (set! ret (reverse! ret)))\n"
       "    (values ret (not (and eof? (null? ret))))))\n"
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
       "(define (parse-shell-list begin-type in already-parsed-reverse enabled-parsers)\n"
       "  (reverse! already-parsed-reverse))\n"
       "\n"
       "(define parser-shell\n"
       "  (let ((ret (make-parser 'shell parse-shell parse-shell* parse-shell-list)))\n"
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
       "      (if (or (null? ret) (null? (cdr ret)))\n"
       "        (car ret)\n"
       "        (cons 'begin (reverse! ret)))\n"
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

  eval("(import (schemesh parser))\n");
}
