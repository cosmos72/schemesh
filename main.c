/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#define _POSIX_C_SOURCE 200809L

#include "main.h"
#include "containers/containers.h" /* schemesh_String_utf8b() */
#include "eval.h"
#include "posix/posix.h"
#include "shell/shell.h"

#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
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
  fprintf(
      stdout,
      "Usage: %s [options and files]\n"
      "  options:\n"
      "    -c STRING, --cmd STRING   run STRING as shell script\n"
      "    -e STRING, --eval STRING  run STRING as scheme source\n"
      "    -h, --help                display this help and exit immediately\n"
      "    -i, --repl                unconditionally start the interactive repl\n"
      "                              (default: start only if no files or strings are specified)\n"
      "    --boot-dir DIR            load Chez Scheme boot files from DIR\n"
      "    --library-dir DIR         load schemesh libraries from DIR\n"
      "    --                        end of options. always treat further arguments as files\n"
      "\n"
      "  files are read and executed as scheme source if their extension is one of:\n"
      "    *.lisp *.rkt *.ss\n"
      "  otherwise they are read and executed as shell script.\n"
      "\n"
      "  both files and strings can switch to different languages\n"
      "  by using the following language-changing syntax tokens:\n"
      "    (             switch to scheme source until the matching )\n"
      "    {             switch to shell script until the matching }\n"
      "    #!scheme      switch to scheme source until end of current scope\n"
      "    #!shell       switch to shell script until end of current scope\n"
      "\n",
      name);

  exit(0);
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
  exit(1);
}

static int missing_option_argument(const char* name, const char* arg) {
  if (name == NULL) {
    name = "schemesh";
  }
  fprintf(stderr,
          "%s: missing argument for option '%s'\nTry '%s --help' for more information.\n",
          name,
          arg,
          name);
  exit(1);
}

struct cmdline {
  const char* boot_dir;
  const char* library_dir;
  char        have_file_or_string;
  char        force_repl;
};

static void parse_command_line(int argc, const char* argv[], struct cmdline* cmd) {
  const char* arg;
  const char* arg2;
  int         i;

  for (i = 1; i < argc && (arg = argv[i]) != NULL; i++) {
    arg2 = i + 1 < argc ? argv[i + 1] : NULL;
    if (!strcmp(arg, "--")) {
      /* end of options, the rest are files */
      cmd->have_file_or_string = 1;
      break;
    } else if (!strcmp(arg, "--boot-dir")) {
      if (!arg2) {
        missing_option_argument(argv[0], arg);
      }
      cmd->boot_dir = arg2;
      i++;
    } else if (!strcmp(arg, "--library-dir")) {
      if (!arg2) {
        missing_option_argument(argv[0], arg);
      }
      cmd->library_dir = arg2;
      i++;
    } else if (!strcmp(arg, "-c") || !strcmp(arg, "--cmd") || !strcmp(arg, "-e") ||
               !strcmp(arg, "--eval")) {
      if (!arg2) {
        missing_option_argument(argv[0], arg);
      }
      /* will be executed by run_files_and_strings() */
      cmd->have_file_or_string = 1;
      i++;
    } else if (!strcmp(arg, "-h") || !strcmp(arg, "--help")) {
      usage(argv[0]);
    } else if (!strcmp(arg, "-i") || !strcmp(arg, "--repl")) {
      cmd->force_repl = 1;
    } else if (!strncmp(arg, "-", 1)) {
      unknown_option(argv[0], arg);
    } else {
      /* file, will be executed by run_files_and_strings() */
      cmd->have_file_or_string = 1;
    }
  }
}

static void run_files_and_strings(int argc, const char* argv[]) {
  const char* arg;
  const char* arg2;
  int         i;
  int         opts = 1;

  for (i = 1; i < argc && (arg = argv[i]) != NULL; i++) {
    if (opts) {
      arg2 = i + 1 < argc ? argv[i + 1] : NULL;
      if (!strcmp(arg, "--")) {
        opts = 0; /* end of options, the rest are files */
      } else if (arg2 && (!strcmp(arg, "--boot-dir") || !strcmp(arg, "--library-dir"))) {
        i++;
      } else if (arg2 && (!strcmp(arg, "-c") || !strcmp(arg, "--cmd"))) {
        call3("sh-eval-string/print*",
              schemesh_Sstring_utf8b(arg2, strlen(arg2)),
              Sstring_to_symbol("shell"),
              Strue);
        i++;
      } else if (arg2 && (!strcmp(arg, "-e") || !strcmp(arg, "--eval"))) {
        call3("sh-eval-string/print*",
              schemesh_Sstring_utf8b(arg2, strlen(arg2)),
              Sstring_to_symbol("scheme"),
              Strue);
        i++;
      } else if (!strncmp(arg, "-", 1)) {
        /* some other option */
      } else {
        call1("sh-eval-file/print", schemesh_Sstring_utf8b(arg, strlen(arg)));
      }
    } else {
      call1("sh-eval-file/print", schemesh_Sstring_utf8b(arg, strlen(arg)));
    }
  }
}

int main(int argc, const char* argv[]) {
  struct cmdline cmd = {};
  int            err = 0;

  parse_command_line(argc, argv, &cmd);

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
  schemesh_init(cmd.boot_dir, &handle_scheme_exception);
  if ((err = schemesh_register_c_functions()) != 0) {
    goto finish;
  }
  if ((err = schemesh_load_libraries(cmd.library_dir)) != 0) {
    goto finish;
  }

  schemesh_import_minimal_libraries();

  (void)&show;
  (void)&diff;
  Senable_expeditor(NULL);
  c_errno_set(0);

  if (cmd.have_file_or_string) {
    run_files_and_strings(argc, argv);
  }
  if (cmd.force_repl == 0 && cmd.have_file_or_string) {
    goto finish;
  }

again:
#if 1
  on_exception = EVAL_FAILED;
  {
    ptr ret = call0("sh-repl");
    if (Sfixnump(ret)) {
      err = Sfixnum_value(ret);
    }
  }
#elif 0
  Sscheme_start(argc, argv);
#else
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
