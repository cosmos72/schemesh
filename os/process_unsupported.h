/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/**
 * on success, return scheme unsigned number containing C DIR*
 * on error, return c_errno() < 0
 */
static ptr c_process_open(void) {
  return Sinteger(c_errno_set(ENOTSUP)); /* < 0 */
}

static void c_process_close(ptr dir_s) {
  (void)dir_s;
}

/**
 * skip one process pid in /proc.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_process_skip(ptr dir_s) {
  (void)dir_s;
  return 0;
}

/*
 * read one process from /proc.
 *   on success, return list (name_string, status_char, tty_string) and fill bvec.
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static ptr c_process_get(ptr dir_s, ptr bvec) {
  (void)dir_s;
  return Sfixnum(0);
}
