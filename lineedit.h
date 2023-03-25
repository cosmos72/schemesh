/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_LINEEDIT_H
#define SCHEMESH_LINEEDIT_H

#include <scheme.h>

void define_lineedit_functions(void);

/** return a cons (width . height), or c_errno() on error */
ptr c_tty_size(void);

/** save controlling tty config, then set it to raw mode */
int c_tty_setraw(void);

/** restore controlling tty to saved config */
int c_tty_restore(void);

#endif /* SCHEMESH_LINEEDIT_H */
