/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_POSIX_POSIX_H
#define SCHEME2K_POSIX_POSIX_H

/**
 * initialize Chez Scheme.
 *
 * if override_boot_dir != NULL, calls in sequence:
 *   Sscheme_init(on_scheme_exception);
 *   Sregister_boot_file(string_append(override_boot_dir,  "/petite.boot"));
 *   Sregister_boot_file(string_append(override_boot_dir, "/scheme.boot"));
 *   Sbuild_heap(NULL, NULL);
 *
 * otherwise calls in sequence:
 *   Sscheme_init(on_scheme_exception);
 *   Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
 *   Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
 *   Sbuild_heap(NULL, NULL);
 */
void scheme2k_init(const char* override_boot_dir, void (*on_scheme_exception)(void));

/**
 * quit Chez Scheme. calls:
 *   c_tty_quit()
 *   Sscheme_deinit()
 */
void scheme2k_quit(void);

/**
 * register all C functions needed by scheme2k library.
 * @return < 0 if some C system call failed
 */
int scheme2k_register_c_functions(void);

/** print error message to stderr and return -errno */
int scheme2k_init_failed(const char label[]);

/** POSIX standard says programs need to declare environ by themselves */
extern char** environ;

#endif /** SCHEME2K_POSIX_POSIX_H */
