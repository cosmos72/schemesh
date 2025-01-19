/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_CONTAINERS_H
#define SCHEMESH_CONTAINERS_H

#include "../chezscheme.h" /* ptr */
#include <stddef.h>        /* size_t */

void schemesh_register_c_functions_containers(void);

/**
 * convert a C char[] to Scheme bytevector and return it.
 * If out of memory, or len > maximum bytevector length, raises condition.
 */
ptr schemesh_Sbytevector(const char chars[], const size_t len);

/**
 * convert a C char[] from UTF-8b to Scheme string and return it.
 * If out of memory, or required string length > maximum string length, raises condition.
 */
ptr schemesh_Sstring_utf8b(const char chars[], const size_t len);

#endif /* SCHEMESH_CONTAINERS_H */
