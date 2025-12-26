/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

/**
 * this file contains helper functions to load schemesh library
 * and import its definitions.
 */

#include "eval.h"

#ifndef SCHEMESH_DIR
#error "please #define SCHEMESH_DIR to the desired installation path of schemesh"
#endif

#define LIBSCHEMESH_SO "libschemesh_0.9.3.so"

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

/** @return 0 if successful, otherwise error code */
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
  scheme2k_eval("(import (schemesh))");
}
