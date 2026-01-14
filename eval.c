/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include "eval.h"
#include "containers/containers.h" /* scheme2k_Sstring_utf8b() */

#include <stddef.h> /* NULL */
#include <string.h> /* strlen() */

static ptr top_level_value(const char symbol_name[]) {
  return Stop_level_value(Sstring_to_symbol(symbol_name));
}

/**
 * call global Scheme procedure with no arguments.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call0(const char symbol_name[]) {
  return Scall0(top_level_value(symbol_name));
}

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call1(const char symbol_name[], ptr arg) {
  return Scall1(top_level_value(symbol_name), arg);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call2(const char symbol_name[], ptr arg1, ptr arg2) {
  return Scall2(top_level_value(symbol_name), arg1, arg2);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3) {
  return Scall3(top_level_value(symbol_name), arg1, arg2, arg3);
}

/**
 * call Scheme (eval (read (open-string-input-port str))) on a C string
 * and return the resulting Scheme value.
 */
ptr scheme2k_eval(const char str[]) {
  /* this must work also without libschemesh -> do not use (sh-eval...) */
  return scheme2k_call1(
      "eval",
      scheme2k_call1("read",
                     scheme2k_call1("open-string-input-port", scheme2k_Sstring_utf8b(str, -1))));
}

/**
 * Load a compiled Scheme library.
 *
 * @return 0 if successful,
 * otherwise print error message to (current-error-port) and return < 0
 */
int scheme2k_load_library(const char* dir, const char* filename) {
  static ptr func_load = Sfalse;
  ptr        ret;
  if (func_load == Sfalse) {
#if 0
    func_load = scheme2k_eval("(lambda (dir filename)\n"
                              "  (load (string-append dir \"/\" filename))\n"
                              "  #t)\n");
#else
    func_load = scheme2k_eval /*                       */
        ("(lambda (dir filename)\n"
         "  (let ((path (if (fxzero? (string-length dir))\n"
         "                filename\n"
         "                (string-append dir \"/\" filename))))\n"
         "    (call/cc\n"
         "      (lambda (k-exit)\n"
         "        (with-exception-handler\n"
         "          (lambda (ex)\n"
         "            (let ((port (current-error-port)))\n"
         "              (put-string port \"schemesh: \")"
         "              (display-condition ex port)\n"
         "              (newline port)\n"
         "              (flush-output-port port))\n"
         "            (k-exit #f))\n" /* exception -> return #f */
         "          (lambda ()\n"
         "            (load path)\n"
         "            #t))))))\n"); /* success -> return #t */
#endif
    Slock_object(func_load);
  }
  ret = Scall2(func_load,
               scheme2k_Sstring_utf8b(dir, -1), /* */
               scheme2k_Sstring_utf8b(filename, -1));
  /* Sunlock_object(func_load); */
  return ret == Strue ? 0 : -1;
}
