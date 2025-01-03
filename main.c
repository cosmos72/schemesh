/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "main.h"
#include "eval.h"
#include "posix/posix.h"
#include "shell/shell.h"

#include <scheme.h>
#include <setjmp.h>
#include <stdio.h>
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

int main(int argc, const char* argv[]) {
  enum { LEN = 1024 };
  int             err;
  char            buf[LEN];
  struct timespec start, end;

  switch (setjmp(jmp_env)) {
    case NOP: /* first call to setjmp: continue initialization */
      break;
    case INIT_FAILED: /* init() failed */
      goto finish;
    case EVAL_FAILED: /* exception in eval() */
      goto again;
    case QUIT_FAILED: /* exception in quit() */
      return 1;
  }
  on_exception = INIT_FAILED;
  schemesh_init(&handle_scheme_exception);
  if ((err = schemesh_register_c_functions()) < 0) {
    return err;
  }
  schemesh_compile_and_load_libraries();
  schemesh_import_libraries();

  on_exception = EVAL_FAILED;
again:
#if 1
  (void)argc;
  (void)argv;
  (void)buf;
  (void)start;
  (void)end;
  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  c_errno_set(0);
  {
    ptr ret = call0("repl");
    if (Sfixnump(ret)) {
      err = Sfixnum_value(ret);
    }
  }
#elif 1
  (void)buf;
  (void)start;
  (void)end;
  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  c_errno_set(0); /* not enough, Scheme (c-errno) still returns -11 */
  Sscheme_start(argc, argv);
#else
  (void)argc;
  (void)argv;
  c_errno_set(0);

  while (fgets(buf, LEN, stdin) != NULL) {
    start = now();

    bytes bv = eval_to_bytevector(buf);
    show(stdout, bv);

    end = now();
    fprintf(stdout, "; elapsed: %.09f\n", diff(start, end));
  }
  fprintf(stdout, "; got EOF. exiting.\n");
#endif /*0*/
finish:
  on_exception = QUIT_FAILED;
  schemesh_quit();

  return err;
}
