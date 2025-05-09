/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#define _POSIX_C_SOURCE 200809L

#include "containers/containers.h" /* schemesh_Sstring_utf8b() */
#include "eval.h"
#include "posix/posix.h"
#include "shell/shell.h"

#include <errno.h>
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

#if 0 /* not used */
struct timespec now(void) {
  struct timespec t;
  (void)clock_gettime(CLOCK_REALTIME, &t);
  return t;
}

static double diff(const struct timespec start, const struct timespec end) {
  return (end.tv_sec - start.tv_sec) + 1e-9 * (end.tv_nsec - start.tv_nsec);
}
#endif

static void handle_scheme_exception(void) {
  longjmp(jmp_env, on_exception);
}

static int usage(const char* name) {
  if (name == NULL) {
    name = "schemesh";
  }
  fprintf(
      stdout,
      "Usage: %s [options and files]\n"
      "  options:\n"
      "    -c STRING, --cmd STRING     run STRING as shell script\n"
      "    -e STRING, --eval STRING    run STRING as scheme source\n"
      "    --cmd-file FILE             read and execute FILE as shell script\n"
      "    --eval-file FILE            read and execute FILE as scheme source\n"
      "    --load-file FILE            load and execute FILE as compiled scheme library\n"
      "    -h, --help                  display this help and exit immediately\n"
      "    -i, --repl                  unconditionally start the interactive repl\n"
      "                                (default: start only if no files, strings or --version\n"
      "                                are specified)\n"
      "    --version                   display version information\n"
      "    -l, --login                 ignored. accepted for compatibility with other shells\n"
      "    --boot-dir DIR              load Chez Scheme boot files from DIR\n"
      "    --library-dir DIR           load schemesh libraries from DIR\n"
      "    --                          end of options. always treat further arguments as files\n"
      "\n"
      "  the type of files, if they are not specified after options '--cmd-file', '--eval-file'\n"
      "  or '--load-file' is determined by their name:\n"
      "    file names ending in '.sh' or not containing '.' are executed as shell script,\n"
      "    file names ending in '.so' are executed as compiled scheme library,\n"
      "    all other files are executed as scheme source\n"
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

static void display_version(void) {
  fputs("0.9.1\n", stdout);
  fflush(stdout);
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
    } else if (!strcmp(arg, "-c") || !strcmp(arg, "--cmd") || !strcmp(arg, "--cmd-file") ||
               !strcmp(arg, "-e") || !strcmp(arg, "--eval") || !strcmp(arg, "--eval-file") ||
               !strcmp(arg, "--load-file")) {
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
    } else if (!strcmp(arg, "-l") || !strcmp(arg, "--login")) {
      /* nop */
    } else if (!strcmp(arg, "--version")) {
      /* disable repl unless cmd->force_repl is set */
      cmd->have_file_or_string = 1;
      display_version();
    } else if (!strncmp(arg, "-", 1)) {
      unknown_option(argv[0], arg);
    } else {
      /* file, will be executed by run_files_and_strings() */
      cmd->have_file_or_string = 1;
    }
  }
}

static void eval_string_type(const char filename[], const size_t len, const char* type) {
  schemesh_call3("sh-eval-string/print*",
                 schemesh_Sstring_utf8b(filename, len),
                 Sstring_to_symbol(type),
                 Strue);
}

static void load_file_type(const char filename[], const size_t len, const char* type) {
  schemesh_call3(
      "sh-eval-file/print*", schemesh_Sstring_utf8b(filename, len), Sstring_to_symbol(type), Strue);
}

static void load_file_type_compiled(const char filename[], const size_t len) {
  schemesh_call1("load", schemesh_Sstring_utf8b(filename, len));
}

static void load_file_type_autodetect(const char filename[], size_t len) {
  if (len == (size_t)-1) {
    len = strlen(filename);
  }
  if (len >= 3 && memcmp(filename + len - 3, ".so", 3) == 0) {
    return load_file_type_compiled(filename, len);
  }
  schemesh_call1("sh-eval-file/print", schemesh_Sstring_utf8b(filename, len));
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
        i++; /* skip subsequent arg */
      } else if (arg2 && (!strcmp(arg, "-c") || !strcmp(arg, "--cmd"))) {
        eval_string_type(arg2, -1, "shell");
        i++;
      } else if (arg2 && (!strcmp(arg, "--cmd-file"))) {
        load_file_type(arg2, -1, "shell");
        i++;
      } else if (arg2 && (!strcmp(arg, "-e") || !strcmp(arg, "--eval"))) {
        eval_string_type(arg2, -1, "scheme");
        i++;
      } else if (arg2 && (!strcmp(arg, "--eval-file"))) {
        load_file_type(arg2, -1, "scheme");
        i++;
      } else if (arg2 && (!strcmp(arg, "--load-file"))) {
        load_file_type_compiled(arg2, -1);
        i++;
      } else if (!strncmp(arg, "-", 1)) {
        /* some other option */
      } else {
        load_file_type_autodetect(arg, -1);
      }
    } else {
      load_file_type_autodetect(arg, -1);
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

  schemesh_import_all_libraries();

  Senable_expeditor(NULL);
  errno = 0;

  if (cmd.have_file_or_string) {
    run_files_and_strings(argc, argv);
  }
  if (cmd.force_repl == 0 && cmd.have_file_or_string) {
    goto finish;
  }

again:
#if 1
  on_exception = EVAL_FAILED;
  do {
    ptr ret = schemesh_call0("repl");

    err = Sfixnump(ret) ? Sfixnum_value(ret) : -1;

  } while (schemesh_call0("repl-restart?") == Strue);
#else
  Sscheme_start(argc, argv);
#endif /*0*/
finish:
  on_exception = QUIT_FAILED;
  schemesh_quit();

  return err;
}
