/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_DISK_UNSUPPORTED_H
#define SCHEME2K_OS_DISK_UNSUPPORTED_H

/**
 * on success, return scheme unsigned number containing C DIR*
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  return Sinteger(c_errno_set(ENOTSUP)); /* < 0 */
}

static void c_disk_close(ptr disk_s) {
  (void)disk_s;
}

/**
 * skip one process pid in /proc.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_disk_skip(ptr disk_s) {
  (void)disk_s;
  return 0;
}

/*
 * read one process from /proc.
 *   on success, return pair (device_name . mountpoint) and fill bvec.
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static ptr c_disk_get(ptr disk_s, ptr bvec) {
  (void)disk_s;
  (void)bvec;
  return Sfixnum(0);
}

#endif /* SCHEME2K_OS_DISK_UNSUPPORTED_H */
