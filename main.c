/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#define _POSIX_C_SOURCE 200809L

#include "main.h"
#include "eval.h"
#include "posix/posix.h"
#include "shell/shell.h"

#include <setjmp.h>
#include <stdio.h>
#include <string.h> /* strcmp() */
#include <time.h>

static jmp_buf jmp_env;
static int     on_exception = 0;

enum jmp_arg {
  NOP         = 0,
  INIT_FAILED = 1,
  EVAL_FAILED = 2,
  QUIT_FAILED = 3,
};

struct timespec now(void) {
  struct timespec t;
  (void)clock_gettime(CLOCK_REALTIME, &t);
  return t;
}

static double diff(const struct timespec start, const struct timespec end) {
  return (end.tv_sec - start.tv_sec) + 1e-9 * (end.tv_nsec - start.tv_nsec);
}

static void handle_scheme_exception(void) {
  longjmp(jmp_env, on_exception);
}

static void show(FILE* out, bytes bv) {
  if (bv.size != 0) {
    fwrite(bv.data, 1, bv.size, out);
    fputc('\n', out);
  }
}

static int usage(const char* name) {
  if (name == NULL) {
    name = "schemesh";
  }
  fprintf(stdout, "Usage: %s [-i] [--repl] [--libdir=DIR]\n", name);
  return 0;
}

static int unknown_option(const char* name, const char* arg) {
  if (name == NULL) {
    name = "schemesh";
  }
  fprintf(stderr,
          "%s: unrecognized option '%s'\nTry '%s --help' for more information.\n",
          name,
          arg,
          name);
  return 1;
}

int main(int argc, const char* argv[]) {
  const char* library_dir = NULL;
  const char* arg;
  int         err = 0;
  int         i;
  enum { e_repl_no, e_repl_auto, e_repl_yes } repl_flag = e_repl_auto;

  for (i = 1; i < argc && (arg = argv[i]) != NULL; i++) {
    if (!strcmp(arg, "-h") || !strcmp(arg, "--help")) {
      return usage(argv[0]);
    } else if (!strcmp(arg, "-i") || !strcmp(arg, "--repl")) {
      repl_flag = e_repl_yes;
    } else if (!strncmp(arg, "--libdir=", 9)) {
      library_dir = arg + 9;
    } else {
      return unknown_option(argv[0], arg);
    }
  }

  switch (setjmp(jmp_env)) {
    case NOP: /* first call to setjmp: continue initialization */
      break;
    case INIT_FAILED: /* init() failed */
      err = 1;
      goto finish;
    case EVAL_FAILED: /* exception in eval() */
      err = 0;
      goto again;
    case QUIT_FAILED: /* exception in quit() */
      return 2;
  }

  on_exception = INIT_FAILED;
  schemesh_init(&handle_scheme_exception);
  if ((err = schemesh_register_c_functions()) != 0) {
    return err;
  }
  if (err || repl_flag == e_repl_no) {
    goto finish;
  }
  if ((err = schemesh_load_libraries(library_dir)) != 0) {
    goto finish;
  }

  schemesh_import_libraries();

  on_exception = EVAL_FAILED;
again:
#if 1
  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  c_errno_set(0);
  {
    ptr ret = call0("sh-repl");
    if (Sfixnump(ret)) {
      err = Sfixnum_value(ret);
    }
  }
#elif 0
  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  c_errno_set(0); /* not enough, Scheme (c-errno) still returns -11 */
  Sscheme_start(argc, argv);
#else
  (void)argc;
  (void)argv;
  c_errno_set(0);

  {
    enum { LEN = 1024 };
    char            buf[LEN];
    struct timespec start, end;

    while (fgets(buf, LEN, stdin) != NULL) {
      start = now();

      bytes bv = eval_to_bytevector(buf);
      show(stdout, bv);

      end = now();
      fprintf(stdout, "; elapsed: %.09f\n", diff(start, end));
    }
    fprintf(stdout, "; got EOF. exiting.\n");
  }
#endif /*0*/
finish:
  on_exception = QUIT_FAILED;
  schemesh_quit();

  return err;
}
