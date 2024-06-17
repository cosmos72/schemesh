/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_SHELL_SHELL_H
#define SCHEMESH_SHELL_SHELL_H

/**
 * initialize Chez Scheme. calls in sequence:
 *   Sscheme_init(on_scheme_exception);
 *   Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
 *   Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
 *   Sbuild_heap(NULL, NULL);
 */
void schemesh_init(void (*on_scheme_exception)(void));

/** register all C functions needed by schemesh libraries. return < 0 if failed */
int schemesh_register_c_functions(void);

/** compile and load all schemesh libraries */
void schemesh_compile_and_load_libraries(void);

/** import all schemesh libraries */
void schemesh_import_libraries(void);

/**
 * quit Chez Scheme. calls:
 *   Sscheme_deinit()
 */
void schemesh_quit(void);

#endif /* SCHEMESH_SHELL_SHELL_H */
