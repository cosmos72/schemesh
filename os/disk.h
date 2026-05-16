/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_DISK_H
#define SCHEME2K_OS_DISK_H

enum {
  e_disk_id          = 0, /* uint64 */
  e_disk_size_total  = 1, /* bytes, uint64 */
  e_disk_size_free   = 2, /* bytes, uint64 */
  e_disk_size_avail  = 3, /* bytes, uint64, free for unpriviledged users */
  e_disk_inode_total = 4, /* uint64 */
  e_disk_inode_free  = 5, /* uint64 */
  e_disk_inode_avail = 6, /* uint64, free for unpriviledged user */
  e_disk_blocksize   = 7, /* bytes, uint64 */
  e_disk_dev         = 8, /* dev_t, int64 */
  e_disk_flags       = 9, /* uint64 */
  e_disk_byte_n      = e_disk_flags * 8 + 8,
};

#if defined(__linux__)
#include "disk_linux.h"
#elif defined(__APPLE__)
#include "disk_macos.h"
#elif defined(__FreeBSD__)
#include "disk_freebsd.h"
#else

#include "disk_unsupported.h"
static unsigned c_dev_major(uint64_t dev) {
  return dev >> 32;
}
static unsigned c_dev_minor(uint64_t dev) {
  return dev & 0xffffffff;
}

#endif

#endif /* SCHEME2K_OS_DISK_H */
