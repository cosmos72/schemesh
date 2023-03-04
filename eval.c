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

#include "eval.h"
#include "container.h"
#include "posix.h"
#include "shell.h"
#include "signal.h"

#include <stddef.h> // NULL

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

static void define_macros(void);
static void define_display_any(void);
static void define_any_to_string(void);
static void define_any_to_bytevector(void);
static void define_eval_to_bytevector(void);

void scheme_init(void (*on_scheme_exception)(void)) {
  Sscheme_init(on_scheme_exception);
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  Sbuild_heap(NULL, NULL);
}

int define_functions(void) {
  int err;

  define_macros();
  define_vector_functions();
  define_array_functions();
  define_hash_functions();
  define_list_functions();
  define_display_any();
  define_any_to_string();
  define_any_to_bytevector();
  define_eval_to_bytevector();
  define_env_functions();
  define_job_functions();

  if ((err = define_fd_functions()) < 0) {
    return err;
  }
  define_signal_functions();
  define_pid_functions();
  define_shell_functions();

  c_environ_to_sh_env(environ);
  return err;
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

static void define_macros(void) {
  eval("(define-syntax while\n"
       "  (syntax-rules ()\n"
       "    ((_ pred)          (do () ((not pred))))\n"
       "    ((_ pred body ...) (do () ((not pred)) body ...))))\n");

  eval("(define-syntax until\n"
       "  (syntax-rules ()\n"
       "    ((_ pred)          (do () (pred)))\n"
       "    ((_ pred body ...) (do () (pred) body ...))))\n");

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
       "                  (datum->syntax (syntax l) e))))))))))\n");
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

  eval("(define (write-bytevector0 x port)\n"
       "  (let ((str (utf8->string x)))\n"
       "    (write (substring str 0 (fx1- (string-length str))) port)))\n");
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
       "        (list-iterate args (lambda (x) (display-any x port)))\n"
       "        (display #\\nul port)\n"
       "        (get-bytevector)))))\n");

  /* convert string to bytevector0 i.e. #\nul terminated UTF-8 bytevector */
  eval("(define (string->bytevector0 x)\n"
       "  (assert (or (string? x) (bytevector? x)))\n"
       "  (any->bytevector0 x))\n");

  /**
   * convert a list of strings or bytevectors to vector-of-bytevector0
   * i.e. to a vector of #\nul terminated UTF-8 bytevectors
   */
  eval("(define (list->cmd-argv l)\n"
       "  (let ((argv (list->vector l)))\n"
       "    (do ([i 0 (+ 1 i)])\n"
       "        ((>= i (vector-length argv)))\n"
       "      (vector-set! argv i (string->bytevector0 (vector-ref argv i))))\n"
       "    argv))\n");

  /**
   * convert a hashtable containing string keys and string values
   * to a vector of bytevector0, where each element is key=value\x0;
   */
  eval("(define (string-hashtable->vector-of-bytevector0 htable)\n"
       "  (let* ((i 0)\n"
       "         (n (hashtable-size htable))\n"
       "         (out (make-vector n)))\n"
       "    (hashtable-iterate htable\n"
       "      (lambda (cell)\n"
       "        (let ((key (car cell))\n"
       "              (val (cdr cell)))\n"
       "          (vector-set! out i (any->bytevector0 key \"=\" val))\n"
       "          (set! i (fx1+ i)))))\n"
       "    out))\n");
}

static void define_eval_to_bytevector(void) {
  eval("(define (eval->bytevector str)\n"
       "  (any->bytevector (eval (read (open-input-string str)))))\n");
}

/**
 * call Scheme (eval) on a C string, and convert returned Scheme value to
 * bytevector with (any->bytevector).
 * @return length and pointer to memory of a Scheme-allocated bytevector.
 *
 * Returned pointer CANNOT be dereferenced anymore after calling Scheme code,
 * because it may be moved or garbage collected.
 */
bytes eval_to_bytevector(const char str[]) {
  ptr   bytevec = call1("eval->bytevector", Sstring(str));
  bytes ret     = {Sbytevector_length(bytevec), Sbytevector_data(bytevec)};
  return ret;
}
