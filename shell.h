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

/**
 * Define the functions (sh-env...)
 */
void define_env_functions(void);

/**
 * Define the record types "job" "cmd" and functions sh-globals (sh-cmd ...)
 * Requires the function (sh-global-env)
 */
void define_job_functions(void);

/**
 * Define the functions (sh-start) (sh-wait) (sh-run) (sh-redirect...)
 * Requires the "job" and "cmd" record types, the (sh-env...) functions
 * and the fd-related and pid-related functions.
 */
void define_shell_functions(void);

/**
 * copy C environment variables env into Scheme global environment (sh-global-env).
 */
void c_environ_to_sh_env(char** env);

#endif /* SCHEMESH_SHELL_H */
