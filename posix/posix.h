/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_POSIX_POSIX_H
#define SCHEMESH_POSIX_POSIX_H

/** define posix functions. return < 0 if some C system call failed */
int schemesh_register_c_functions_posix(void);

/** POSIX standard says programs need to declare environ by themselves */
extern char** environ;

#endif /** SCHEMESH_POSIX_POSIX_H */
