/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/**
 * helper file to compile Scheme library libchez_batteries_X.Y.Z.so
 */

#include <errno.h>
#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* strerror() */
#include <unistd.h> /* chdir() */

#if !defined(__GNUC__) || defined(__OPTIMIZE__)
#define SCHEMESH_OPTIMIZE
#else
#undef SCHEMESH_OPTIMIZE
#endif

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)
#ifdef SCHEMESH_DIR
#define SCHEMESH_DIR_STR STR(SCHEMESH_DIR)
#endif

#define LIBCHEZ_BATTERIES_SO "libchez_batteries_0.9.2.so"

static void handle_scheme_exception(void) {
  (void)write(1, "schemesh_test failed: exception evaluating Scheme code!\n", 56);
  exit(1);
}

static void scheme_init(const char* override_boot_dir, void (*on_scheme_exception)(void)) {
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

static void scheme_quit(void) {
  Sscheme_deinit();
}

static ptr call1(const char symbol_name[], ptr arg) {
  return Scall1(Stop_level_value(Sstring_to_symbol(symbol_name)), arg);
}

ptr eval(const char str[]) {
  return call1("eval", call1("read", call1("open-string-input-port", Sstring_utf8(str, -1))));
}

/**
 * compile libschemesh_VERSION.so from sources found in specified directory.
 *
 * return 0 if successful, otherwise error code.
 */
static int compile_libraries(const char* source_dir) {
  ptr ret;
  int err;
  if (source_dir == NULL) {
    fprintf(stderr, "%s", "schemesh: --compile-source-dir argument is null\n");
    return EINVAL;
  }
  if (chdir(source_dir) != 0) {
    err = errno;
    fprintf(stderr,
            "schemesh: C function chdir(\"%s\") failed with error %d: %s\n",
            source_dir,
            err,
            strerror(err));
    return err;
  }
#ifdef SCHEMESH_OPTIMIZE
  ret = eval("(parameterize ((optimize-level 2))\n"
             "  (compile-file \"libchez_batteries.ss\" \"libchez_batteries_temp.so\")\n"
             "  (strip-fasl-file \"libchez_batteries_temp.so\" \"" LIBCHEZ_BATTERIES_SO "\"\n"
             "    (fasl-strip-options inspector-source source-annotations profile-source))\n"
             "    #t\n)");
#else /* !SCHEMESH_OPTIMIZE */
  ret = eval("(parameterize ((optimize-level 0)\n"
             "               (run-cp0 (lambda (cp0 x) x)))\n"
             "  (compile-file \"libchez_batteries.ss\" \"" LIBCHEZ_BATTERIES_SO "\")\n"
             "  #t)");
#endif
  return ret == Strue ? 0 : EINVAL;
}

int main(int argc, const char* argv[]) {
  (void)argc;
  (void)argv;

  scheme_init(NULL, &handle_scheme_exception);

  compile_libraries(".");

  scheme_quit();

  return 0;
}
