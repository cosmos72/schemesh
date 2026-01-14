/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "../containers/containers.h" /* scheme2k_Sstring_utf8b() */
#include "../eval.h"
#include "../load.h"
#include "../posix/posix.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* chdir() */

#if !defined(__GNUC__) || defined(__OPTIMIZE__)
#define SCHEME_OPTIMIZE
#else
#undef SCHEME_OPTIMIZE
#endif

#define LIBSCHEME2K_SO "libscheme2k_0.9.3.so"

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

static void run_scheme_tests(unsigned long* run_n, unsigned long* failed_n, const char* test_file) {

  ptr ret = scheme2k_call1("run-tests", scheme2k_Sstring_utf8b(test_file, -1));

  if (Spairp(ret) && Sfixnump(Scar(ret)) && Sfixnump(Scdr(ret))) {
    *run_n += Sfixnum_value(Scar(ret));
    *failed_n += Sfixnum_value(Scdr(ret));
  } else {
    *run_n += 1;
    *failed_n += 1;
  }
}

static int run_all_tests(void) {
  const char*   test_files[] = {"test/data0.ss", "test/data1.ss", "test/data2.ss", "test/data3.ss"};
  unsigned long run_n        = 0;
  unsigned long failed_n     = 0;

  fprintf(stdout, "%s", "running tests ...\n");
  fflush(stdout);

  scheme2k_call1("load", scheme2k_Sstring_utf8b("test/test.ss", -1));
  scheme2k_eval("(import (schemesh test))");

  for (unsigned i = 0; i < N_OF(test_files); i++) {
    run_scheme_tests(&run_n, &failed_n, test_files[i]);
  }

  if (failed_n == 0) {
    fprintf(stdout, "all %lu tests passed\n", run_n);
  } else {
    fprintf(stdout, "%lu tests failed out of %lu\n", failed_n, run_n);
  }
  fflush(stdout);
  return failed_n == 0 ? 0 : 1;
}

static void handle_scheme_exception(void) {
  (void)write(1, "schemesh_test failed: exception evaluating Scheme code!\n", 56);
  exit(1);
}

/**
 * compile libschemesh_VERSION.so from sources found in specified directory.
 *
 * return 0 if successful, otherwise error code.
 */
static int compile_schemesh_so(const char* source_dir) {
  ptr ret;
  int err;
  if (source_dir == NULL) {
    fprintf(stderr, "%s", "schemesh_test: source_dir is null\n");
    return EINVAL;
  }
  if (chdir(source_dir) != 0) {
    err = errno;
    fprintf(stderr,
            "schemesh_test: C function chdir(\"%s\") failed with error %d: %s\n",
            source_dir,
            err,
            strerror(err));
    return err;
  }
#ifdef SCHEME_OPTIMIZE
  ret =
      scheme2k_eval("(parameterize ((optimize-level 2))\n"
                    "  (compile-file \"libschemesh.ss\" \"libschemesh_temp.so\")\n"
                    "  (strip-fasl-file \"libschemesh_temp.so\" \"" LIBSCHEMESH_SO "\"\n"
                    "    (fasl-strip-options inspector-source source-annotations profile-source))\n"
                    "    #t\n)");
#else /* !SCHEME_OPTIMIZE */
  ret = scheme2k_eval("(parameterize ((optimize-level 0)\n"
                      "               (run-cp0 (lambda (cp0 x) x)))\n"
                      "  (compile-file \"libschemesh.ss\" \"" LIBSCHEMESH_SO "\")\n"
                      "  #t)");
#endif
  return ret == Strue ? 0 : EINVAL;
}

/**
 * compile libscheme2k_VERSION.so from sources found in specified directory.
 *
 * return 0 if successful, otherwise error code.
 */
static int compile_scheme2k_so(const char* source_dir) {
  ptr ret;
  int err;
  if (source_dir == NULL) {
    fprintf(stderr, "%s", "schemesh_test: source_dir is null\n");
    return EINVAL;
  }
  if (chdir(source_dir) != 0) {
    err = errno;
    fprintf(stderr,
            "schemesh_test: C function chdir(\"%s\") failed with error %d: %s\n",
            source_dir,
            err,
            strerror(err));
    return err;
  }
#ifdef SCHEME_OPTIMIZE
  ret =
      scheme2k_eval("(parameterize ((optimize-level 2))\n"
                    "  (compile-file \"libscheme2k.ss\" \"libscheme2k_temp.so\")\n"
                    "  (strip-fasl-file \"libscheme2k_temp.so\" \"" LIBSCHEME2K_SO "\"\n"
                    "    (fasl-strip-options inspector-source source-annotations profile-source))\n"
                    "    #t\n)");
#else /* !SCHEME_OPTIMIZE */
  ret = scheme2k_eval("(parameterize ((optimize-level 0)\n"
                      "               (run-cp0 (lambda (cp0 x) x)))\n"
                      "  (compile-file \"libscheme2k.ss\" \"" LIBSCHEME2K_SO "\")\n"
                      "  #t)");
#endif
  return ret == Strue ? 0 : EINVAL;
}

int main(int argc, const char* argv[]) {
  int err = 0;

  scheme2k_init(NULL, &handle_scheme_exception);

  if (argc == 2 && strcmp(argv[1], "--compile_scheme2k_so") == 0) {
    err = compile_scheme2k_so(".");
  } else {
    if (scheme2k_register_c_functions() == 0 && /*     */
        compile_schemesh_so(".") == 0 &&        /*     */
        scheme2k_load_library(".", LIBSCHEMESH_SO) == 0) {

      schemesh_import_all_libraries();

      (void)run_all_tests();
    }
  }
  scheme2k_quit();

  return err;
}
