/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h> /* strcmp() */
#include <time.h>
#include <unistd.h>

#include "containers/containers.h" /* scheme2k_Sstring_utf8b() */
#include "eval.h"
#include "load.h"
#include "posix/posix.h"

static int drop_privileges(void) {
  /* setting gid or uid on Android crashes with SIGSYS */
#ifndef __ANDROID__
  const gid_t gid = getgid();
  const uid_t uid = getuid();

#ifdef __linux__
  if (setresgid(gid, gid, gid) != 0) {
    return scheme2k_init_failed("setresgid()");
  }
  if (setresuid(uid, uid, uid) != 0) {
    return scheme2k_init_failed("setresuid()");
  }
#else
  if (setegid(gid) != 0) {
    return scheme2k_init_failed("setegid()");
  }
  if (seteuid(uid) != 0) {
    return scheme2k_init_failed("seteuid()");
  }
#endif
#endif
  return 0;
}

static jmp_buf jmp_env;
static int     on_exception = 0;

enum jmp_arg {
  NOP         = 0,
  INIT_FAILED = 1,
  EVAL_FAILED = 2,
  QUIT_FAILED = 3,
};

static void handle_scheme_exception(void) {
  longjmp(jmp_env, on_exception);
}

static void usage(const char* name) {
  if (name == NULL) {
    name = "schemesh";
  }
  fprintf(stdout,
          "Usage: %s [options and files]\n"
          "Options:\n"
          "  -c STRING, --cmd STRING   run STRING as shell script\n"
          "  -e STRING, --eval STRING  run STRING as scheme source\n"
          "  --args [ARG...]           store ARGS as the command-line arguments.\n"
          "                            always last option: it consumes all remaining arguments.\n"
          "  --cmd-file FILE           read and execute FILE as shell script\n"
          "  --eval-file FILE          read and execute FILE as scheme source\n"
          "  --load-file FILE          load and execute FILE as compiled scheme library\n"
          "  -h, --help                display this help and exit immediately\n"
          "  -i, --repl                unconditionally start the interactive repl\n"
          "                            (default: start only if no files, strings or --version\n"
          "                            are specified)\n"
          "  --version                 display version information\n"
          "  -l, --login               ignored. accepted for compatibility with other shells\n"
          "  -p                        ignored. accepted for compatibility with other shells\n"
#ifdef SCHEMESH_STATIC
          "  --boot-dir DIR            ignored in this build. set Chez Scheme boot directory\n"
          "  --library-dir DIR         ignored in this build. set schemesh library directory\n"
#else
          "  --boot-dir DIR            load Chez Scheme boot files from DIR\n"
          "  --library-dir DIR         load schemesh libraries from DIR\n"
#endif
          "  --                        end of options. always treat further arguments as files\n"
          "                            even if they start with -"
          "\n"
          "The type of files passed as arguments (i.e. not specified after options\n"
          "'--cmd-file...', '--eval-file...' or '--load-file') is determined by their name:\n"
          "  file names ending in '.sh' or not containing '.' are executed as shell script,\n"
          "  file names ending in '.so' are executed as compiled scheme library,\n"
          "  all other files are executed as scheme source\n"
          "\n"
          "Files and strings can internally switch to different syntax\n"
          "by using the following syntax-changing tokens:\n"
          "  (          switch to scheme source until the matching )\n"
          "  {          switch to shell script until the matching }\n"
          "  #!scheme   switch to scheme source until end of current scope\n"
          "  #!shell    switch to shell script until end of current scope\n"
          "\n",
          name);

  exit(0);
}

static void display_version(void) {
  fputs("1.0.1\n", stdout);
  fflush(stdout);
}

static void unknown_option(const char* name, const char* arg) {
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

static void missing_script_argument(const char* name) {
  if (name == NULL) {
    name = "schemesh-script";
  }
  fprintf(stderr, "%s: missing file path to execute.\n", name);
  exit(1);
}

static void missing_option_argument(const char* name, const char* arg) {
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

static ptr make_string_list(const char* const* args, int n) {
  ptr ret = Snil;
  while (n > 0) {
    ret = Scons(scheme2k_Sstring_utf8b(args[--n], -1), ret);
  }
  return ret;
}

static void set_command_line_args(const char* const* args, int n) {
  ptr l = make_string_list(args, n);
  Slock_object(l);
  scheme2k_call1("command-line", Scons(Smake_string(0, 0), l));
  scheme2k_call1("command-line-arguments", l);
  Sunlock_object(l);
}

static void set_command_name(ptr name) {
  if (Sstringp(name)) {
    ptr l = scheme2k_call0("command-line");
    scheme2k_call1("command-line", Scons(name, Spairp(l) ? Scdr(l) : Snil));
  }
}

typedef struct {
  const char*        boot_dir;
  const char*        library_dir;
  const char* const* runtime_args;
  int                runtime_argn;
  char               have_file_or_string;
  char               is_script;
  char               force_repl;
} cmdline;

typedef struct {
  const char* data;
  size_t      len;
} chars;

static chars chars_from_c(const char* data) {
  chars ret = {data ? data : "", data ? strlen(data) : 0};
  return ret;
}

static chars chars_make(const char* data, size_t len) {
  chars ret = {data ? data : "", len};
  return ret;
}

#define CHARS(str) chars_make((str), (sizeof(str)) - 1)

static int chars_equal(chars left, chars right) {
  if (left.len != right.len) {
    return 0;
  }
  if (left.data == right.data) {
    return 1;
  }
  return memcmp(left.data, right.data, left.len) == 0;
}

static int chars_end_with(chars cs, chars suffix) {
  if (cs.len < suffix.len) {
    return 0;
  }
  cs.data += cs.len - suffix.len;
  cs.len = suffix.len;
  return chars_equal(cs, suffix);
}

static void parse_command_line(int argc, const char* argv[], cmdline* cmd) {
  const char* argi;
  int         i;

  if (chars_end_with(chars_from_c(argv[0]), CHARS("-script"))) {
    cmd->runtime_args        = argv + 2;
    cmd->runtime_argn        = argc - 2;
    cmd->have_file_or_string = 1;
    cmd->is_script           = 1;
    cmd->force_repl          = 0;
    /* consumes all arguments */
    return;
  }

  for (i = 1; (argi = argv[i]) != NULL; i++) {
    chars       arg  = {argi, strlen(argi)};
    const char* arg2 = argv[i + 1]; /* NULL if argi is last argument */

    if (chars_equal(arg, CHARS("--"))) {
      /* end of options, the rest are files */
      cmd->have_file_or_string = 1;
      break;
    } else if (chars_equal(arg, CHARS("--boot-dir"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      cmd->boot_dir = arg2;
      i++;
    } else if (chars_equal(arg, CHARS("--library-dir"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      cmd->library_dir = arg2;
      i++;
    } else if (chars_equal(arg, CHARS("--args"))) {
      /* consumes all remaining arguments */
      cmd->runtime_args = argv + i + 1;
      cmd->runtime_argn = argc - i - 1;
      break;
    } else if (chars_equal(arg, CHARS("-c")) || chars_equal(arg, CHARS("-e")) ||        /**/
               chars_equal(arg, CHARS("--cmd")) || chars_equal(arg, CHARS("--eval")) || /**/
               chars_equal(arg, CHARS("--cmd-file")) || chars_equal(arg, CHARS("--eval-file")) ||
               chars_equal(arg, CHARS("--load-file"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      /* will be executed by run_files_and_strings() */
      cmd->have_file_or_string = 1;
      i++;
    } else if (chars_equal(arg, CHARS("-h")) || chars_equal(arg, CHARS("--help"))) {
      usage(argv[0]);
    } else if (chars_equal(arg, CHARS("-i")) || chars_equal(arg, CHARS("--repl"))) {
      cmd->force_repl = 1;
    } else if (chars_equal(arg, CHARS("-l")) || chars_equal(arg, CHARS("-p")) ||
               chars_equal(arg, CHARS("--login"))) {
      /* nop */
    } else if (chars_equal(arg, CHARS("--version"))) {
      /* disable repl unless cmd->force_repl is set */
      cmd->have_file_or_string = 1;
      display_version();
    } else if (argi[0] == '-') {
      unknown_option(argv[0], argi);
    } else {
      /* file, will be executed by run_files_and_strings() */
      cmd->have_file_or_string = 1;
    }
  }
}

static void eval_string_type(const char filename[], const size_t len, const char* type) {
  scheme2k_call3("sh-eval-string/print",
                 scheme2k_Sstring_utf8b(filename, len),
                 Sstring_to_symbol(type),
                 Strue);
}

static void load_file_type(const char filename[], const size_t len, const char* type) {
  ptr str = scheme2k_Sstring_utf8b(filename, len);
  set_command_name(str);
  scheme2k_call2("sh-eval-file/print", str, Sstring_to_symbol(type));
}

static void load_file_type_compiled(const char filename[], const size_t len) {
  ptr str = scheme2k_Sstring_utf8b(filename, len);
  set_command_name(str);
  scheme2k_call1("load", str);
}

static void load_file_type_autodetect(const char filename[], size_t len) {
  if (len == (size_t)-1) {
    len = strlen(filename);
  }
  if (len >= 3 && memcmp(filename + len - 3, ".so", 3) == 0) {
    return load_file_type_compiled(filename, len);
  }
  ptr str = scheme2k_Sstring_utf8b(filename, len);
  set_command_name(str);
  scheme2k_call1("sh-eval-file/print", str);
}

static void install_exception_handler(void) {
  scheme2k_call1("base-exception-handler",
                 Stop_level_value(Sstring_to_symbol("repl-exception-handler")));
}

static void run_files_and_strings(int argc, const char* argv[], const cmdline* cmd) {
  const char* argi;
  int         i;
  int         opts = 1;

  if (cmd->is_script) {
    if (argc > 1 && argv[1]) {
      load_file_type_autodetect(argv[1], -1);
    } else {
      missing_script_argument(argv[0]);
    }
    /* consumes all arguments */
    return;
  }
  for (i = 1; (argi = argv[i]) != NULL; i++) {
    if (opts) {
      chars       arg  = {argi, strlen(argi)};
      const char* arg2 = argv[i + 1]; /* NULL if argi is last argument */
      if (chars_equal(arg, CHARS("--"))) {
        opts = 0; /* end of options, the rest are files */
      } else if (chars_equal(arg, CHARS("--args"))) {
        break; /* consumes all arguments */
      } else if (arg2 && (chars_equal(arg, CHARS("--boot-dir")) ||
                          chars_equal(arg, CHARS("--library-dir")))) {
        i++; /* skip subsequent argi */
      } else if (arg2 && (chars_equal(arg, CHARS("-c")) || chars_equal(arg, CHARS("--cmd")))) {
        eval_string_type(arg2, -1, "shell");
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("-e")) || chars_equal(arg, CHARS("--eval")))) {
        eval_string_type(arg2, -1, "scheme");
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--cmd-file")))) {
        load_file_type(arg2, -1, "shell");
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--eval-file")))) {
        load_file_type(arg2, -1, "scheme");
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--load-file")))) {
        load_file_type_compiled(arg2, -1);
        i++;
      } else if (argi[0] == '-') {
        /* some other option */
      } else {
        load_file_type_autodetect(argi, -1);
      }
    } else {
      load_file_type_autodetect(argi, -1);
    }
  }
}

int main(int argc, const char* argv[]) {
  cmdline cmd = {};
  int     err = drop_privileges();
  if (err != 0) {
    return err;
  }
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
  if ((err = scheme2k_register_c_functions()) != 0) {
    goto finish;
  }
  if ((err = schemesh_load_library(cmd.library_dir)) != 0) {
    goto finish;
  }

  schemesh_import_all_libraries();

  Senable_expeditor(NULL);
  errno = 0;

  on_exception = EVAL_FAILED;
  /**
   * install the same exception handler use use for REPL,
   * because Chez Scheme default exception handler sometimes causes infinite loops
   * when writing to stderr fails
   */
  install_exception_handler();
  if (cmd.runtime_args) {
    set_command_line_args(cmd.runtime_args, cmd.runtime_argn);
  }
  if (cmd.have_file_or_string) {
    run_files_and_strings(argc, argv, &cmd);
  }

again:
  if (cmd.force_repl == 0 && cmd.have_file_or_string) {
    goto finish;
  }
#if 1
  /* copy only program name, not the arguments we parsed above */
  set_command_name(scheme2k_Sstring_utf8b(argv[0], -1));
  do {
    ptr ret = scheme2k_call0("repl");

    err = Sfixnump(ret) ? Sfixnum_value(ret) : -1;

  } while (scheme2k_call0("repl-restart?") == Strue);
#else
  Sscheme_start(argc, argv);
#endif /*0*/
finish:
  on_exception = QUIT_FAILED;
  scheme2k_quit();

  return err;
}
