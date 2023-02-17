/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "eval.h"
#include "iterator.h"
#include "posix.h"

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

static void define_define_macro(void);
static void define_display_any(void);
static void define_any_to_string(void);
static void define_any_to_bytevector(void);
static void define_eval_to_bytevector(void);
static void define_sh_env(void);
static void c_environ_to_sh_env(char** env);
char**      vector_to_c_argz(ptr vector_of_bytevector0);

void scheme_init(void (*on_scheme_exception)(void)) {
  Sscheme_init(on_scheme_exception);
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  Sbuild_heap(NULL, NULL);

  define_define_macro();
  define_hash_iterator();
  define_display_any();
  define_any_to_string();
  define_any_to_bytevector();
  define_eval_to_bytevector();
  define_sh_env();

  c_environ_to_sh_env(environ);

  define_posix_functions();
}

void scheme_quit(void) {
  Sscheme_deinit();
}

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr call1(const char symbol_name[], ptr arg) {
  return Scall1(Stop_level_value(Sstring_to_symbol(symbol_name)), arg);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr call2(const char symbol_name[], ptr arg1, ptr arg2) {
  return Scall2(Stop_level_value(Sstring_to_symbol(symbol_name)), arg1, arg2);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3) {
  return Scall3(Stop_level_value(Sstring_to_symbol(symbol_name)), arg1, arg2, arg3);
}

/**
 * call Scheme (eval) on a C string and return the resulting Scheme value
 */
ptr eval(const char str[]) {
  return call1("eval", call1("read", call1("open-input-string", Sstring(str))));
}

static void define_define_macro(void) {
  eval("(define-syntax define-macro\n"
       "  (syntax-rules ()\n"
       "    ((k (name . args) body ...)\n"
       "     (define-macro name (lambda args body ...)))\n"
       "    ((k name transformer)\n"
       "     (define-syntax name\n"
       "       (lambda (stx)\n"
       "         (syntax-case stx ()\n"
       "           ((l . sv)\n"
       "            (let* ((v (syntax->datum (syntax sv)))\n"
       "                   (e (apply transformer v)))\n"
       "              (if (eq? (void) e)\n"
       "                  (syntax (void))\n"
       "                  (datum->syntax (syntax l) e))))))))))");
}

static void define_display_any(void) {
  eval("(define (display-condition x port)\n"
       "  (when (condition? x)\n"
       "    (put-string port \"#<condition\")\n"
       "    (do ((clist (simple-conditions x) (cdr clist)))\n"
       "        ((null? clist) (void))\n"
       "      (let ((c (car clist)))\n"
       "        (cond ((assertion-violation? c)       (put-string port \" &assertion\"))\n"
       "              ((non-continuable-violation? c) (put-string port \" &non-continuable\"))"
       "              ((implementation-restriction-violation? c)\n"
       "                         (put-string \" &implementation-restriction \"))\n"
       "              ((lexical-violation? c)   (put-string port \" &lexical\"))\n"
       "              ((syntax-violation? c)    (put-string port \" &syntax \")\n"
       "                                        (put-datum  port (syntax-violation-form c))\n"
       "                                        (put-string port \" \")\n"
       "                                        (put-datum  port (syntax-violation-subform c)))\n"
       "              ((undefined-violation? c) (put-string port \" &undefined\"))\n"
       "              ((violation? c)           (put-string port \" &violation\"))\n"
       "              ((i/o-read-error? c)      (put-string port \" &i/o-read\"))\n"
       "              ((i/o-write-error? c)     (put-string port \" &i/o-write\"))\n"
       "              ((i/o-invalid-position-error? c)\n"
       "                         (put-string port \" &i/o-invalid-position\"))\n"
       /* more i/o errors ... */
       "              ((i/o-error? c)           (put-string port \" &i/o\"))\n"
       "              ((error? c)               (put-string port \" &error\"))\n"
       "              ((warning? c)             (put-string port \" &warning\"))\n"
       "              ((message-condition? c)   (put-string port \" &message \")\n"
       "                                        (put-datum  port (condition-message c)))\n"
       "              ((irritants-condition? c) (put-string port \" &irritants \")\n"
       "                                        (put-datum  port (condition-irritants c)))\n"
       "              ((who-condition? c)       (put-string port \" &who \")\n"
       "                                        (put-datum  port (condition-who c)))\n"
       "              ((serious-condition? c)   (put-string port \" &serious\")))))\n"
       "    (put-string port \">\")))\n");

  eval("(define (display-any x port)\n"
       "  (if (condition? x)\n"
       "    (display-condition x port)\n"
       "    (display x port)))\n");
}

static void define_any_to_string(void) {
  /* convert any value to a string */
  eval("(define (any->string x)\n"
       "  (cond ((string? x) x)\n"
       "        ((bytevector? x) (utf8->string x))\n"
       "        ((eq? (void) x) \"\")\n"
       "        (#t (let-values (((port get-string)\n"
       "                          (open-string-output-port)))\n"
       "              (display-any x port)\n"
       "              (get-string)))))\n");
}

static void define_any_to_bytevector(void) {
  /* convert any value to a bytevector */
  eval("(define any->bytevector\n"
       "  (let ((transcoder (make-transcoder (utf-8-codec) (eol-style lf)\n"
       "                                     (error-handling-mode raise))))\n"
       "    (lambda (x)\n"
       "      (cond ((bytevector? x) x)\n"
       "            ((string? x) (string->utf8 x))\n"
       "            ((eq? (void) x) #vu8())\n"
       "            (#t (let-values (((port get-bytevector)\n"
       "                              (open-bytevector-output-port transcoder)))\n"
       "                  (display-any x port)\n"
       "                  (get-bytevector)))))))\n");

  /* convert any sequence of values to #\nul terminated bytevector */
  eval("(define any->bytevector0\n"
       "  (let ((transcoder (make-transcoder (utf-8-codec) (eol-style lf)\n"
       "                                     (error-handling-mode raise))))\n"
       "    (lambda args\n"
       "      (let-values (((port get-bytevector)\n"
       "                    (open-bytevector-output-port transcoder)))\n"
       "        (for-each (lambda (x) (display-any x port)) args)\n"
       "        (display #\\nul port)\n"
       "        (get-bytevector)))))\n");

  /* convert string to #\nul terminated UTF-8 bytevector */
  eval("(define (string->bytevector0 x)\n"
       "  (assert (or (string? x) (bytevector? x)))\n"
       "  (any->bytevector0 x))\n");
}

static void define_eval_to_bytevector(void) {
  eval("(define (eval->bytevector str)\n"
       "  (any->bytevector (eval (read (open-input-string str)))))\n");
}

static void define_sh_env(void) {
  eval("(define sh-env\n"
       "  (let ((vars (make-hashtable string-hash string=?)))\n"
       "    (lambda ()\n"
       "      vars)))\n");
  eval("(define (sh-env-get vars name)\n"
       "  (let* ((vars (if (null? vars) (sh-env) vars))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (cdr elem)\n"
       "      \"\")))");
  eval("(define (sh-env-set! vars name val)\n"
       "  (let* ((vars (if (null? vars) (sh-env) vars))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (set-cdr! elem val)\n"
       "      (hashtable-set! vars name (cons #f val)))))\n");
  eval("(define (sh-env-unset! vars name)\n"
       "  (let ((vars (if (null? vars) (sh-env) vars)))\n"
       "    (hashtable-delete! vars name)))\n");
  eval("(define (sh-env-exported? vars name)\n"
       "  (let* ((vars (if (null? vars) (sh-env) vars))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (car elem)\n"
       "      #f)))");
  eval("(define (sh-env-export! vars name exported?)\n"
       "  (assert (boolean? exported?))\n"
       "  (let* ((vars (if (null? vars) (sh-env) vars))\n"
       "         (elem (hashtable-ref vars name #f)))\n"
       "    (if (pair? elem)\n"
       "      (set-car! elem exported?)\n"
       "      (hashtable-set! vars name (cons exported? \"\")))))\n");
  /**
   * FIXME: replace with a function that counts env variables from a job and its parents
   */
  eval("(define (sh-env-size vars all?)\n"
       "  (assert (boolean? all?))\n"
       "  (let ((vars (if (null? vars) (sh-env) vars)))\n"
       "    (if all?\n"
       "      (hashtable-size vars)\n"
       "      (let ((n 0))\n"
       "        (hashtable-iterate vars\n"
       "          (lambda (cell)\n"
       "            (when (cadr cell)\n"
       "              (set! n (fx1+ n)))))\n"
       "        n))))\n");
  /**
   * FIXME: replace with a function that extracts env variables from a job and its parents
   */
  eval("(define (sh-env->vector-of-bytevector0 vars all?)\n"
       "  (let* ((vars (if (null? vars) (sh-env) vars))\n"
       "         (i 0)\n"
       "         (n (sh-env-size vars all?))\n"
       "         (out (make-vector n #f)))\n"
       "    (hashtable-iterate vars\n"
       "      (lambda (cell)\n"
       "        (let ((key (car cell))\n"
       "              (val (cdr cell)))\n"
       "          (when (or all? (car val))\n"
       "            (vector-set! out i (any->bytevector0 key \"=\" (cdr val)))\n"
       "            (set! i (fx1+ i))))))\n"
       "    out))\n");
}

static void c_environ_to_sh_env(char** env) {
  const char* entry;
  if (!env) {
    return;
  }
  for (; (entry = *env) != NULL; ++env) {
    const char* separator = strchr(entry, '=');
    size_t      namelen   = separator ? separator - entry : 0;
    iptr        inamelen  = Sfixnum_value(Sfixnum(namelen));
    if (namelen == 0 || inamelen < 0 || namelen != (size_t)inamelen) {
      continue;
    }
    call3("sh-env-set!", Snil, Sstring_of_length(entry, inamelen), Sstring(separator + 1));
    call3("sh-env-export!", Snil, Sstring_of_length(entry, inamelen), Strue);
  }
}

char** vector_to_c_argz(ptr vector_of_bytevector0) {
  ptr    vec    = vector_of_bytevector0;
  char** c_argz = NULL;
  iptr   i, n;
  if (!Svectorp(vec)) {
    return c_argz;
  }
  n      = Svector_length(vec);
  c_argz = malloc((n + 1) * sizeof(char*));
  if (!c_argz) {
    return c_argz;
  }
  for (i = 0; i < n; i++) {
    ptr  bytevec = Svector_ref(vec, i);
    iptr len;
    if (Sbytevectorp(bytevec)                      /*                        */
        && (len = Sbytevector_length(bytevec)) > 0 /*                        */
        && Sbytevector_u8_ref(bytevec, len - 1) == 0) {

      c_argz[i] = (char*)Sbytevector_data(bytevec);
    } else {
      free(c_argz);
      return NULL;
    }
  }
  c_argz[n] = NULL;
  return c_argz;
}

/**
 * call Scheme (eval) on a C string, and convert returned Scheme value to
 * bytevector with (any->bytevector).
 * @return length and pointer to internal array of a Scheme-allocated
 * bytevector.
 *
 * Returned pointer CANNOT be dereferenced anymore after calling Scheme code,
 * because it may be moved or garbage collected.
 */
bytes eval_to_bytevector(const char str[]) {
  ptr   bytevec = call1("eval->bytevector", Sstring(str));
  bytes ret     = {Sbytevector_length(bytevec), Sbytevector_data(bytevec)};
  return ret;
}
