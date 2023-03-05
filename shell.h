/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_SHELL_H
#define SCHEMESH_SHELL_H

#include "eval.h"

void scheme_init(void (*on_scheme_exception)(void));

/** define all functions. return < 0 if failed */
int define_functions(void);

void scheme_quit(void);

#endif /* SCHEMESH_SHELL_H */
