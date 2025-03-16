/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"
#include "containers/containers.h" /* schemesh_Sstring_utf8b() */

#include <stddef.h> /* NULL */
#include <string.h> /* strlen() */

static ptr top_level_value(const char symbol_name[]) {
  return Stop_level_value(Sstring_to_symbol(symbol_name));
}

/**
 * call global Scheme procedure with no arguments.
 * Return the resulting Scheme value.
 */
ptr schemesh_call0(const char symbol_name[]) {
  return Scall0(top_level_value(symbol_name));
}

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call1(const char symbol_name[], ptr arg) {
  return Scall1(top_level_value(symbol_name), arg);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call2(const char symbol_name[], ptr arg1, ptr arg2) {
  return Scall2(top_level_value(symbol_name), arg1, arg2);
}

/**
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3) {
  return Scall3(top_level_value(symbol_name), arg1, arg2, arg3);
}

/**
 * call Scheme (eval (read (open-string-input-port str))) on a C string
 * and return the resulting Scheme value.
 * Cannot use (sh-eval) because it may be called before loading libschemesh.
 */
ptr schemesh_eval(const char str[]) {
  /* this must work even if libschemesh is not loaded -> cannot use (sh-eval...) */
  return schemesh_call1(
      "eval",
      schemesh_call1("read",
                     schemesh_call1("open-string-input-port", schemesh_Sstring_utf8b(str, -1))));
}
