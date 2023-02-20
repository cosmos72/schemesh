/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "main.h"
#include "eval.h"

#include <errno.h>
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

static void handle_scheme_exception(void) { //
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
    case NOP: // first call to setjmp: continue initialization
      break;
    case INIT_FAILED: // init() failed
      goto finish;
    case EVAL_FAILED: // exception in eval()
      goto again;
    case QUIT_FAILED: // exception in quit()
      return 1;
  }
  on_exception = INIT_FAILED;
  scheme_init(&handle_scheme_exception);
  if ((err = define_functions()) < 0) {
    return err;
  }

  on_exception = EVAL_FAILED;
again:
#if 1
  (void)buf;
  (void)start;
  (void)end;
  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  errno = 0; // not enough, Scheme (errno) still returns -11
  Sscheme_start(argc, argv);
#else  /*0*/
  (void)argc;
  (void)argv;
  errno = 0;

  while (fgets(buf, LEN, stdin) != NULL) {
    start = now();

    bytes bv = eval_to_bytevector(buf);
    show(stdout, bv);

    end = now();
    fprintf(stdout, "; elapsed: %.09f\n", diff(start, end));
  }
#endif /*0*/
finish:
  on_exception = QUIT_FAILED;
  scheme_quit();

  return 0;
}
