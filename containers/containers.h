/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_CONTAINERS_H
#define SCHEME2K_CONTAINERS_H

#include "../chezscheme.h" /* ptr */
#include <stddef.h>        /* size_t */

void scheme2k_register_c_functions_containers(void);

/**
 * convert a C byte[] to Scheme bytevector and return it.
 * If len == (size_t)-1, set len = strlen(bytes).
 * If out of memory, or len > maximum bytevector length, raises condition.
 */
ptr scheme2k_Sbytevector(const char bytes[], const size_t len);

/**
 * convert a C byte[] from UTF-8b to Scheme string and return it.
 * If len == (size_t)-1, set len = strlen(bytes).
 * If out of memory, or required string length > maximum string length, raises condition.
 */
ptr scheme2k_Sstring_utf8b(const char bytes[], const size_t len);

#endif /* SCHEME2K_CONTAINERS_H */
