/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_EVAL_H
#define SCHEME2K_EVAL_H

#include "chezscheme.h"

/**
 * call global Scheme procedure with no arguments.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call0(const char symbol_name[]);

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call1(const char symbol_name[], ptr arg);

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call2(const char symbol_name[], ptr arg1, ptr arg2);

/**
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr scheme2k_call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3);

/**
 * call Scheme (eval (read (open-string-input-port str))) on a C string
 * and return the resulting Scheme value.
 * Cannot use (sh-eval) because it may be called before loading libschemesh.
 */
ptr scheme2k_eval(const char str[]);

/**
 * Load a compiled Scheme library.
 *
 * @return 0 if successful,
 * otherwise print error message to (current-error-port) and return < 0
 */
int scheme2k_load_library(const char dir[], const char filename[]);

#endif /* SCHEME2K_EVAL_H */
