/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_EVAL_H
#define SCHEMESH_EVAL_H

#include "chezscheme.h"

/**
 * call global Scheme procedure with no arguments.
 * Return the resulting Scheme value.
 */
ptr schemesh_call0(const char symbol_name[]);

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call1(const char symbol_name[], ptr arg);

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call2(const char symbol_name[], ptr arg1, ptr arg2);

/**
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr schemesh_call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3);

/**
 * call Scheme (eval (read (open-string-input-port str))) on a C string
 * and return the resulting Scheme value.
 * Cannot use (sh-eval) because it may be called before loading libschemesh.
 */
ptr schemesh_eval(const char str[]);

typedef struct {
  iptr                 size;
  const unsigned char* data;
} bytes;

/**
 * call Scheme (sh-eval->bytevector) on a C UTF8 string, which also converts
 * returned Scheme value to bytevector with (any->bytevector).
 * @return length and pointer to internal span of a Scheme-allocated
 * bytevector.
 *
 * Returned pointer CANNOT be dereferenced anymore after calling Scheme code,
 * because it may be moved or garbage collected.
 */
bytes schemesh_eval_to_bytevector(const char str[]);

#endif /* SCHEMESH_EVAL_H */
