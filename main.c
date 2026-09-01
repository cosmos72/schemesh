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

enum jmp_arg {
  NOP         = 0,
  INIT_FAILED = 1,
  EVAL_FAILED = 2,
  QUIT_FAILED = 3,
};

enum syntax_type {
  TYPE_AUTO     = 0,
  TYPE_SCHEME   = 1,
  TYPE_SHELL    = 2,
  TYPE_COMPILED = 3,
};

typedef struct {
  const char* data;
  size_t      len;
} chars;

typedef struct {
  const char* boot_dir;
  const char* library_dir;
  char        have_file;
  char        have_string;
  char        is_script;
  char        force_repl;
} cmdline;

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

static void handle_scheme_exception(void) {
  longjmp(jmp_env, on_exception);
}

static void usage(const char* name, const int is_script) {
  if (name == NULL) {
    name = is_script ? "schemesh-script" : "schemesh";
  }
  fprintf(stdout,
          "Usage: %s %s%s%s%s",
          name,
          is_script ? "[options] FILE [ARG...]" : "[options and files]",
          "\nOptions:\n"
          "  -c STRING, --cmd STRING     run STRING as shell source\n"
          "  -e STRING, --eval STRING    run STRING as scheme source\n"
          "\n"
          "  --cmd-file FILE             read and execute FILE as shell source\n"
          "  --cmd-script FILE [ARG...]  always last option: consume all remaining arguments,\n"
          "                              store them as runtime arguments, then\n"
          "                              read and execute FILE as shell source\n"
          "\n"
          "  --eval-file FILE            read and execute FILE as scheme source\n"
          "  --eval-script FILE [ARG...] always last option: consume all remaining arguments,\n"
          "                              store them as runtime arguments, then\n"
          "                              read and execute FILE as scheme source\n"
          "\n"
          "  --load-file FILE            load and execute FILE as compiled scheme library\n"
          "  --load-script FILE [ARG...] always last option: consume all remaining arguments,\n"
          "                              store them as runtime arguments, then\n"
          "                              load and execute FILE as compiled scheme library\n"
          "\n"
          "  -h, --help                  display this help and exit immediately\n",
          is_script ? /**/
              "  -i, --repl                  IGNORED in schemesh-script,\n"
              "                              would start the interactive repl in schemesh\n" :
              "  -i, --repl                  unconditionally start the interactive repl\n"
              "                              (default: start only if no files, strings\n"
              "                              or --version are specified)\n",

          "  --version                   display version information\n"
          "  -l, --login                 ignored. accepted for compatibility with other shells\n"
          "  -p                          ignored. accepted for compatibility with other shells\n"
#ifdef SCHEMESH_STATIC
          "  --boot-dir DIR              ignored in this build. set Chez Scheme boot directory\n"
          "  --library-dir DIR           ignored in this build. set schemesh library directory\n"
#else
          "  --boot-dir DIR              load Chez Scheme boot files from DIR\n"
          "  --library-dir DIR           load schemesh libraries from DIR\n"
#endif
          "  --                          end of options. always treat further arguments\n"
          "                              as files even if they start with -\n"
          "\n"
          "The type of files passed as arguments (i.e. not specified after options\n"
          "'--cmd-...', '--eval-...' or '--load-...') is determined by their name:\n"
          "  file names ending in '.sh' or not containing '.' are executed as shell source,\n"
          "  file names ending in '.so' are executed as compiled scheme library,\n"
          "  all other files are executed as scheme source\n"
          "\n"
          "Files and strings can internally switch to different syntax\n"
          "by using the following syntax-changing tokens:\n"
          "  (          switch to scheme syntax until the matching )\n"
          "  {          switch to shell syntax until the matching }\n"
          "  #!scheme   switch to scheme syntax until end of current scope\n"
          "  #!shell    switch to shell syntax until end of current scope\n"
          "\n");

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

static ptr make_string_list(const char* const* argv, int n) {
  ptr ret = Snil;
  while (n > 0) {
    ret = Scons(scheme2k_Sstring_utf8b(argv[--n], -1), ret);
  }
  return ret;
}

static void set_command_line(const char* const* argv, int n) {
  ptr l = make_string_list(argv, n);
  Slock_object(l);
  scheme2k_call1("command-line", l);
  scheme2k_call1("command-line-arguments", Spairp(l) ? Scdr(l) : Snil);
  Sunlock_object(l);
}

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

  if (argc > 0 && chars_end_with(chars_from_c(argv[0]), CHARS("-script"))) {
    cmd->is_script = 1;
  }

  for (i = 1; (argi = argv[i]) != NULL; i++) {
    chars       arg  = {argi, strlen(argi)};
    const char* arg2 = argv[i + 1]; /* NULL if argi is last argument */

    if (argi[0] != '-') {
      /* file, will be executed by run_files_and_strings() */
      cmd->have_file = 1;
      if (cmd->is_script) {
        break; /* further arguments are (command-line) */
      }
    } else if (chars_equal(arg, CHARS("--"))) {
      /* end of options, the rest are files and args */
      if (arg2) {
        cmd->have_file = 1;
      }
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
    } else if (chars_equal(arg, CHARS("-c")) || chars_equal(arg, CHARS("-e")) || /**/
               chars_equal(arg, CHARS("--cmd")) || chars_equal(arg, CHARS("--eval"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      /* will be executed by run_files_and_strings() */
      cmd->have_string = 1;
      i++;
    } else if (chars_equal(arg, CHARS("--cmd-file")) || chars_equal(arg, CHARS("--eval-file")) ||
               chars_equal(arg, CHARS("--load-file"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      /* will be executed by run_files_and_strings() */
      cmd->have_file = 1;
      i++;
    } else if (chars_equal(arg, CHARS("--cmd-script")) ||
               chars_equal(arg, CHARS("--eval-script")) ||
               chars_equal(arg, CHARS("--load-script"))) {
      if (!arg2) {
        missing_option_argument(argv[0], argi);
      }
      /* will be executed by run_files_and_strings() */
      cmd->have_file = 1;
      /*consumes all remaining arguments*/
      break;
    } else if (chars_equal(arg, CHARS("-h")) || chars_equal(arg, CHARS("--help"))) {
      usage(argv[0], cmd->is_script);
    } else if (chars_equal(arg, CHARS("-i")) || chars_equal(arg, CHARS("--repl"))) {
      cmd->force_repl = 1;
    } else if (chars_equal(arg, CHARS("-l")) || chars_equal(arg, CHARS("-p")) ||
               chars_equal(arg, CHARS("--login"))) {
      /* nop */
    } else if (chars_equal(arg, CHARS("--version"))) {
      /* disable repl unless cmd->force_repl is set */
      cmd->have_string = 1;
      display_version();
    } else {
      unknown_option(argv[0], argi);
    }
  }
  if (cmd->is_script && !cmd->have_file) {
    missing_script_argument(argv[0]);
  }
}

static ptr type_to_symbol(const enum syntax_type type) {
  return Sstring_to_symbol(type == TYPE_SCHEME ? "scheme" : "shell");
}

static void eval_string_type(const char filename[], const enum syntax_type type) {
  scheme2k_call3(
      "sh-eval-string/print", scheme2k_Sstring_utf8b(filename, -1), type_to_symbol(type), Strue);
}

static void load_script_type(const char* argv[], const int argc, enum syntax_type type) {
  ptr str;
  if (!argv || !argv[0] || argc <= 0) {
    return;
  }
  set_command_line(argv, argc);
  {
    const char* filename = argv[0];
    size_t      len      = strlen(filename);
    str                  = scheme2k_Sstring_utf8b(filename, len);
    if (type == TYPE_AUTO && len > 3 && memcmp(filename + len - 3, ".so", 3) == 0) {
      type = TYPE_COMPILED;
    }
  }
  switch (type) {
    case TYPE_AUTO:
      scheme2k_call1("sh-eval-file/print", str);
      break;
    case TYPE_SCHEME:
    case TYPE_SHELL:
      scheme2k_call2("sh-eval-file/print", str, type_to_symbol(type));
      break;
    case TYPE_COMPILED:
      scheme2k_call1("load", str);
      break;
  }
}

static void load_file_type(const char filename[], const enum syntax_type type) {
  load_script_type(&filename, 1, type);
}

static void install_exception_handler(void) {
  scheme2k_call1("base-exception-handler",
                 Stop_level_value(Sstring_to_symbol("repl-exception-handler")));
}

static void run_files_and_strings(int argc, const char* argv[], const cmdline* cmd) {
  const char* argi;
  int         i;
  int         opts = 1;

  for (i = 1; (argi = argv[i]) != NULL; i++) {
    if (opts && argi[0] == '-') {
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
        eval_string_type(arg2, TYPE_SHELL);
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("-e")) || chars_equal(arg, CHARS("--eval")))) {
        eval_string_type(arg2, TYPE_SCHEME);
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--cmd-file")))) {
        load_file_type(arg2, TYPE_SHELL);
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--eval-file")))) {
        load_file_type(arg2, TYPE_SCHEME);
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--load-file")))) {
        load_file_type(arg2, TYPE_COMPILED);
        i++;
      } else if (arg2 && (chars_equal(arg, CHARS("--cmd-script")))) {
        load_script_type(argv + i + 1, argc - i - 1, TYPE_SHELL);
        break; /* consumes all arguments */
      } else if (arg2 && (chars_equal(arg, CHARS("--eval-script")))) {
        load_script_type(argv + i + 1, argc - i - 1, TYPE_SCHEME);
        break; /* consumes all arguments */
      } else if (arg2 && (chars_equal(arg, CHARS("--load-script")))) {
        load_script_type(argv + i + 1, argc - i - 1, TYPE_COMPILED);
        break; /* consumes all arguments */
      } else {
        /* some other option */
      }
    } else if (cmd->is_script) {
      load_script_type(argv + i, argc - i, TYPE_AUTO);
      break; /* consumes all arguments */
    } else {
      load_file_type(argi, TYPE_AUTO);
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
  if (cmd.have_file || cmd.have_string) {
    run_files_and_strings(argc, argv, &cmd);
  }

again:
  if (cmd.force_repl == 0 && (cmd.have_file || cmd.have_string)) {
    goto finish;
  }
#if 1
  /* copy only program name, not the arguments we parsed above */
  set_command_line(argv, 1);
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
