/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "shell.h"
#include "../containers/containers.h"
#include "../eval.h"
#include "../posix/posix.h"
#include "../posix/signal.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

#if !defined(__GNUC__) || defined(__OPTIMIZE__)
#define SCHEMESH_OPTIMIZE
#else
#undef SCHEMESH_OPTIMIZE
#endif

/**
 * return i-th environment variable i.e. environ[i]
 * converted to a cons containing two Scheme strings: (key . value)
 *
 * if environ[i] is NULL, return #f
 */
static ptr c_environ_ref(uptr i) {
  const char* entry = environ[i];
  if (entry) {
    const char* separator = strchr(entry, '=');
    size_t      namelen   = separator ? separator - entry : 0;
    iptr        inamelen  = Sfixnum_value(Sfixnum(namelen));
    if (namelen > 0 && inamelen > 0 && namelen == (size_t)inamelen) {
      return Scons(Sstring_utf8(entry, inamelen), Sstring_utf8(separator + 1, -1));
    }
  }
  return Sfalse;
}

/**
 * return current working directory as Scheme string,
 * or empty string if an error happens
 */
static ptr c_get_cwd(void) {
  {
    // call getcwd() with a small stack buffer
    char dir[256];
    if (getcwd(dir, sizeof(dir)) == dir) {
      return Sstring_utf8(dir, -1);
    } else if (c_errno() != -ERANGE) {
      return Sstring_utf8("", 0);
    }
  }
  {
    // call getcwd() with progressively larger heap buffers
    size_t maxlen = 1024;
    char*  dir    = NULL;
    while (maxlen && (dir = malloc(maxlen)) != NULL) {
      if (getcwd(dir, maxlen) == dir) {
        ptr ret = Sstring_utf8(dir, -1);
        free(dir);
        return ret;
      }
      free(dir);
      maxlen *= 2;
    }
  }
  return Sstring_utf8("", 0);
}

/**
 * change current working directory to specified Scheme bytevector0,
 * i.e. a bytevector that must already end with a byte = 0.
 * return 0 on success, or c_errno() < 0 on error.
 */
static int c_chdir(ptr bytevec0) {
  if (Sbytevectorp(bytevec0)) {
    iptr        len = Sbytevector_length(bytevec0);
    const char* dir = (const char*)Sbytevector_data(bytevec0);
    if (len > 0 && dir[len - 1] == 0) {
      if (chdir(dir) == 0) {
        return 0;
      }
      return c_errno();
    }
  }
  return c_errno_set(EINVAL);
}

int schemesh_register_c_functions(void) {
  int err;

  schemesh_register_c_functions_containers();

  if ((err = schemesh_register_c_functions_posix()) < 0) {
    return err;
  }

  Sregister_symbol("c_environ_ref", &c_environ_ref);
  Sregister_symbol("c_get_cwd", &c_get_cwd);
  Sregister_symbol("c_chdir", &c_chdir);

  return err;
}

#ifdef SCHEMESH_OPTIMIZE
#define LIBSCHEMESH_SO "libschemesh.so"
#else /* !SCHEMESH_OPTIMIZE */
#define LIBSCHEMESH_SO "libschemesh_debug.so"
#endif

void schemesh_compile_and_load_libraries(void) {
  eval("(let ((try-load\n"
       "  (lambda (path)\n"
       "    (call/cc\n"
       "      (lambda (k-exit)\n"
       "        (with-exception-handler\n"
       "          (lambda (condition)\n"
       "            (k-exit #f))\n"
       "          (lambda ()\n"
#ifdef SCHEMESH_OPTIMIZE
       "            (parameterize ((optimize-level 2))\n"
#else /* !SCHEMESH_OPTIMIZE */
       "            (parameterize ((optimize-level 0)\n"
       "                           (run-cp0 (lambda (cp0 x) x)))\n"
#endif
       "              (load path))\n"
       "            #t)))))))\n"
       "  (unless (try-load \"/usr/local/lib/schemesh/" LIBSCHEMESH_SO "\")\n"
       "    (unless (try-load \"/usr/lib/schemesh/" LIBSCHEMESH_SO "\")\n"
       "      (unless (try-load \"" LIBSCHEMESH_SO "\")\n"
       "        (compile-file \"libschemesh.ss\" \"libschemesh_debug.so\")\n"
#ifdef SCHEMESH_OPTIMIZE
       "        (strip-fasl-file \"libschemesh_debug.so\" \"libschemesh.so\"\n"
       "          (fasl-strip-options inspector-source source-annotations profile-source))\n"
#endif
       "        (load \"" LIBSCHEMESH_SO "\")))))\n");
}

void schemesh_import_libraries(void) {
  eval("(begin\n"
       "  (import (schemesh bootstrap))\n"
       "  (import (schemesh containers))\n"
       "  (import (schemesh conversions))\n"
       "  (import (schemesh lineedit vscreen))\n"
       "  (import (schemesh lineedit io))\n"
       "  (import (schemesh lineedit parser))\n"
       "  (import (schemesh lineedit charhistory))\n"
       "  (import (schemesh lineedit paren))\n"
       "  (import (schemesh lineedit parenmatcher))\n"
       "  (import (schemesh lineedit))\n"
       "  (import (schemesh parser))\n"
       "  (import (schemesh posix))\n"
       "  (import (schemesh shell))\n"
       "  (import (schemesh repl)))\n");
}

void schemesh_init(void (*on_scheme_exception)(void)) {
  Sscheme_init(on_scheme_exception);
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  Sbuild_heap(NULL, NULL);
}

void schemesh_quit(void) {
  Sscheme_deinit();
}
