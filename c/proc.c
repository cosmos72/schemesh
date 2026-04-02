/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/******************************************************************************/
/*                                                                            */
/*      minimal reimplementation of classic 'ps' program with JSON output     */
/*                                                                            */
/******************************************************************************/

#include <stdlib.h> /* exit() */

#include "writer.h"

#include "proc_common.h"

#if defined(__linux__)
#include "proc_linux.h"
#elif defined(__APPLE__)
#include "proc_macos.h"
#elif defined(__FreeBSD__)
#include "proc_freebsd.h"
#else
#include "proc_unsupported.h"
#endif

static void usage(char* argv0) {
  fprintf(stderr,
          "Usage: %s [OPTION]... [ARG]...\n"
          "Write JSON information about running processes.\n"
          "Options:\n"
          "  --      end of options\n"
          "  --help  show this help\n"
          "\n"
          "Arguments:\n"
          "  a       also display processes started by other users.\n"
          "  u       display more details for each process\n"
          "  v       display even more details for each process\n"
          "  x       also display processes running without a terminal\n",
          argv0);
}

static e_proc_flags parse_proc_flags(int argc, char* argv[]) {
  char*        arg;
  int          i;
  int          options = 1;
  e_proc_flags flags   = e_proc_flag_default;
  for (i = 1; i < argc; ++i) {
    if (!(arg = argv[i])) {
      break;
    } else if (options && arg[0] == '-') {
      if (!strcmp(arg, "--")) {
        options = 0;
      } else {
        usage(argv[0]);
        exit(strcmp(arg, "--help") ? 1 : 0);
      }
    } else {
      int  pos;
      char ch;
      for (pos = 0; (ch = arg[pos]) != '\0'; pos++) {
        switch (ch) {
          case 'a':
            flags |= e_proc_flag_other_users;
            break;
          case 'u':
            flags |= e_proc_flag_long;
            break;
          case 'v':
            flags |= e_proc_flag_verbose;
            break;
          case 'x':
            flags |= e_proc_flag_without_tty;
            break;
          default:
            usage(argv[0]);
            exit(1);
        }
      }
    }
  }
  return flags;
}

int main(int argc, char* argv[]) {
  writer       w     = {stdout, 0, {0}};
  e_proc_flags flags = parse_proc_flags(argc, argv);
  print_processes(&w, flags);
  return 0;
}