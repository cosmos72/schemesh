/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_DISK_MACOS_H
#define SCHEME2K_OS_DISK_MACOS_H

#include <sys/types.h> /* major(), minor() - correct? */

#include "disk_unsupported.h"

static uint64_t c_make_dev(unsigned major, unsigned minor) {
  return (uint64_t)makedev((int)major, (int)minor);
}

static unsigned c_dev_major(uint64_t dev) {
  return (unsigned)major((dev_t)dev);
}

static unsigned c_dev_minor(uint64_t dev) {
  return (unsigned)minor((dev_t)dev);
}

#endif /* SCHEME2K_OS_DISK_MACOS_H */