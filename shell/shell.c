/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "shell.h"
#include "../containers/containers.h" /* scheme2k_Sstring_utf8b() */
#include "../eval.h"
#include "../posix/posix.h"

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

/**
 * return i-th environment variable i.e. environ[i]
 * converted to a cons containing two Scheme strings: (key . value)
 *
 * if environ[i] is NULL, return #f
 */
static ptr c_environ_ref(uptr i) {
  const char* entry = environ[i];
  const char* separator;
  if (entry && (separator = strchr(entry, '=')) != NULL) {
    size_t namelen  = separator - entry;
    iptr   inamelen = Sfixnum_value(Sfixnum(namelen));
    if (namelen > 0 && inamelen > 0 && namelen == (size_t)inamelen) {
      return Scons(scheme2k_Sstring_utf8b(entry, namelen),
                   scheme2k_Sstring_utf8b(separator + 1, -1));
    }
  }
  return Sfalse;
}

int schemesh_register_c_functions(void) {
  int err;

  scheme2k_register_c_functions_containers();

  if ((err = scheme2k_register_c_functions_posix()) != 0) {
    return err;
  }

  Sregister_symbol("c_environ_ref", &c_environ_ref);

  return err;
}

/* return 0 if successful, otherwise error code */
int schemesh_load_library(const char* override_library_dir) {
  const char* filename = LIBSCHEMESH_SO;
  int         err      = -1;

  if (override_library_dir != NULL) {
    err = scheme2k_load_library(override_library_dir, filename);
  } else {
#ifdef SCHEMESH_DIR
    err = scheme2k_load_library(STR(SCHEMESH_DIR), filename);
#endif
    if (err != 0) {
      err = scheme2k_load_library("/usr/local/lib/schemesh", filename);
    }
    if (err != 0) {
      err = scheme2k_load_library("/usr/lib/schemesh", filename);
    }
  }
  return err;
}

void schemesh_import_all_libraries(void) {
  scheme2k_eval("(import (schemesh))\n");
}
