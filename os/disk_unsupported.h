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

#ifndef c_make_dev
static uint64_t c_make_dev(unsigned major, unsigned minor) {
  return (uint64_t)major << 32 | (uint32_t)minor;
}
#endif

#ifndef c_dev_major
static unsigned c_dev_major(uint64_t dev) {
  return dev >> 32;
}
#endif

#ifndef c_dev_minor
static unsigned c_dev_minor(uint64_t dev) {
  return dev & 0xffffffff;
}
#endif

/**
 * on success, return some Scheme object for accessing list of mounted file systems
 * on error, return c_errno() < 0
 */
#ifndef c_disk_open
static ptr c_disk_open(void) {
  return Sinteger(c_errno_set(ENOTSUP)); /* < 0 */
}
#endif

#ifndef c_disk_close
static void c_disk_close(ptr disk_s) {
  (void)disk_s;
}
#endif

/**
 * skip one mounted file system.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
#ifndef c_disk_skip
static int c_disk_skip(ptr disk_s) {
  (void)disk_s;
  return 0;
}
#endif

/**
 * read one mounted file system.
 *   on success, return pair (device_name . mountpoint) and fill bvec.
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
#ifndef c_disk_get
static ptr c_disk_get(ptr disk_s, ptr bvec) {
  (void)disk_s;
  (void)bvec;
  return Sfixnum(0);
}
#endif

#endif /* SCHEME2K_OS_DISK_UNSUPPORTED_H */
