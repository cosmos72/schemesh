/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

#include <stddef.h> /* NULL */

/**
 * call global Scheme procedure with no arguments.
 * Return the resulting Scheme value.
 */
ptr call0(const char symbol_name[]) {
  return Scall0(Stop_level_value(Sstring_to_symbol(symbol_name)));
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
 * call Scheme (eval (read (open-string-input-port str))) on a C UTF-8 string
 * and return the resulting Scheme value
 */
ptr eval(const char str[]) {
  return call1("eval", call1("read", call1("open-string-input-port", Sstring_utf8(str, -1))));
}

/**
 * call Scheme (eval) on a C string, and convert returned Scheme value to
 * bytevector with (any->bytevector).
 * @return length and pointer to memory of a Scheme-allocated bytevector.
 *
 * Returned pointer CANNOT be dereferenced anymore after calling further Scheme code,
 * because it may be moved or garbage collected.
 */
bytes eval_to_bytevector(const char str[]) {
  ptr   bytevec = call1("eval->bytevector", Sstring_utf8(str, -1));
  bytes ret     = {Sbytevector_length(bytevec), Sbytevector_data(bytevec)};
  return ret;
}
