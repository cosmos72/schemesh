/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_C_PROC_IMPL_H
#define SCHEME2K_C_PROC_IMPL_H

static void print_processes(writer* w, e_proc_flags flags) {
  (void)w;
  (void)flags;

  /** TODO: implement */
  fputs("proc: unsupported operating system\n", stderr);
  exit(1);
}

#endif /* SCHEME2K_C_PROC_IMPL_H */
