/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)
#define INSTALL_LIBDIR_STR STR(INSTALL_LIBDIR)

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
      return Scons(schemesh_Sstring_utf8b(entry, inamelen),
                   schemesh_Sstring_utf8b(separator + 1, strlen(separator + 1)));
    }
  }
  return Sfalse;
}

int schemesh_register_c_functions(void) {
  int err;

  schemesh_register_c_functions_containers();

  if ((err = schemesh_register_c_functions_posix()) != 0) {
    return err;
  }

  Sregister_symbol("c_environ_ref", &c_environ_ref);

  return err;
}

/* return 0 if successful, otherwise error code */
int schemesh_load_libraries(const char* override_library_dir) {
#if 0
  ptr try_load_proc = eval("(lambda (dir)\n"
                           "  (load (string-append dir \"/" LIBSCHEMESH_SO "\"))\n"
                           "  #t)\n");
#else
  ptr try_load_proc = eval("(lambda (dir)\n"
                           "  (let ((path (string-append dir \"/" LIBSCHEMESH_SO "\")))\n"
                           "    (call/cc\n"
                           "      (lambda (k-exit)\n"
                           "        (with-exception-handler\n"
                           "          (lambda (ex)\n"
                           "            (let ((port (current-error-port)))\n"
                           "              (put-string port \"schemesh: \")"
                           "              (display-condition ex port)\n"
                           "              (newline port))\n"
                           "            (k-exit #f))\n" /* exception -> return #f */
                           "          (lambda ()\n"
                           "            (load path)\n"
                           "            #t))))))\n"); /* success -> return #t */
#endif
  ptr ret = Sfalse;
  Slock_object(try_load_proc);

  if (override_library_dir != NULL) {
    ret = Scall1(try_load_proc, Sstring_utf8(override_library_dir, -1));
  } else {
#ifdef INSTALL_LIBDIR
    ret = Scall1(try_load_proc, Sstring_utf8(INSTALL_LIBDIR_STR, -1));
#endif
    if (ret != Strue) {
      ret = Scall1(try_load_proc, Sstring_utf8("/usr/local/lib/schemesh", -1));
    }
    if (ret != Strue) {
      ret = Scall1(try_load_proc, Sstring_utf8("/usr/lib/schemesh", -1));
    }
  }
  Sunlock_object(try_load_proc);
  return ret == Strue ? 0 : EINVAL;
}

void schemesh_import_minimal_libraries(void) {
  eval("(import\n"
       "  (schemesh shell)\n"
       "  (schemesh repl))\n");
}

void schemesh_import_all_libraries(void) {
#if 1
  eval("(import-schemesh/all)");
#else
  eval("(import\n"
       "  (schemesh bootstrap)\n"
       "  (schemesh containers)\n"
       "  (schemesh conversions)\n"
       "  (schemesh lineedit)\n"
       "  (schemesh parser)\n"
       "  (schemesh posix)\n"
       "  (schemesh shell)\n"
       "  (schemesh repl))\n");
#endif
}

void schemesh_init(const char* override_boot_dir, void (*on_scheme_exception)(void)) {
  int loaded = 0;
  Sscheme_init(on_scheme_exception);
  if (override_boot_dir != NULL) {
    size_t dir_len   = strlen(override_boot_dir);
    char*  boot_file = (char*)malloc(dir_len + 13);
    if (boot_file != NULL) {
      memcpy(boot_file, override_boot_dir, dir_len);
      memcpy(boot_file + dir_len, "/petite.boot", 13);
      Sregister_boot_file(boot_file);

      memcpy(boot_file + dir_len, "/scheme.boot", 13);
      Sregister_boot_file(boot_file);
      loaded = 1;
    }
  }
  if (loaded == 0) {
    Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
    Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  }
  Sbuild_heap(NULL, NULL);
}

void schemesh_quit(void) {
  Sscheme_deinit();
}
