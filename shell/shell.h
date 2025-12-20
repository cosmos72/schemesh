/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_SHELL_SHELL_H
#define SCHEMESH_SHELL_SHELL_H

#define LIBSCHEMESH_SO "libschemesh_0.9.2.so"

/** register all C functions needed by schemesh libraries. return != 0 if failed */
int schemesh_register_c_functions(void);

/**
 * if override_library_dir is set, load libschemesh_VERSION.so library from it
 * otherwise load libschemesh_VERSION.so from system-wide installation directory.
 *
 * return 0 if successful, otherwise error code.
 */
int schemesh_load_library(const char* override_library_dir);

/** import all schemesh libraries */
void schemesh_import_all_libraries(void);

#endif /* SCHEMESH_SHELL_SHELL_H */
