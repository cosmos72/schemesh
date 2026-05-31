/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_OS_H
#define SCHEME2K_OS_OS_H

#include <stddef.h> /* size_t */

/**
 * register all C functions needed by (scheme2k os) library.
 */
void scheme2k_register_c_functions_os(void);

size_t scheme2k_os_pagesize(void);

#endif /* SCHEME2K_OS_OS_H */