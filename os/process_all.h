/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <errno.h>
#include <fcntl.h>  /* open() */
#include <stddef.h> /* size_t */
#include <stdint.h> /* int64_t, uint64_t */
#include <stdio.h>
#include <stdlib.h>   /* calloc(), free(), BSD devname() */
#include <string.h>   /* memcpy(), strrchr(), strcmp() */
#include <sys/stat.h> /* fstat(), S_IFCH */
#include <time.h>     /* clock_gettime(), struct timespec */
#include <unistd.h>   /* close(), read(), BSD pagesize(), sysconf(), _SC_PAGESIZE */

#include "../containers/containers.h"

typedef char sizeof_int64_is_8[sizeof(int64_t) == 8 ? 1 : -1];
typedef char sizeof_uint64_is_8[sizeof(uint64_t) == 8 ? 1 : -1];
typedef char sizeof_double_is_8[sizeof(double) == 8 ? 1 : -1];

static void set_int64(void* dst, size_t i, int64_t val) {
  if (dst) {
    memcpy((uint8_t*)dst + i * 8, &val, 8);
  }
}

static void set_uint64(void* dst, size_t i, uint64_t val) {
  if (dst) {
    memcpy((uint8_t*)dst + i * 8, &val, 8);
  }
}

static void set_timespec(void* dst, size_t i, struct timespec t) {
  set_uint64(dst, i, (uint32_t)t.tv_nsec);
  set_int64(dst, i + 1, t.tv_sec);
}

static int c_errno_set(int errno_value) {
  return -(errno = errno_value);
}

static int c_errno(void) {
  return -errno;
}

enum {
  e_pid         = 0,  /* int64 */
  e_uid         = 1,  /* int64 */
  e_gid         = 2,  /* int64 */
  e_ppid        = 3,  /* int64 */
  e_pgid        = 4,  /* int64 */
  e_sid         = 5,  /* int64 */
  e_mem_rss     = 6,  /* bytes, uint64 */
  e_mem_virt    = 7,  /* bytes, uint64 */
  e_start_time  = 8,  /* utc,      timespec, uint64 tv_nsec then int64 tv_sec */
  e_user_time   = 10, /* duration, timespec */
  e_sys_time    = 12, /* duration, timespec */
  e_iowait_time = 14, /* duration, timespec */
  e_priority    = 16, /* int64  */
  e_num_thread  = 17, /* int64  */
  e_min_fault   = 18, /* uint64 */
  e_maj_fault   = 19, /* uint64 */
  e_state       = 20, /* char   */
  e_byte_n      = e_state * 8 + 1,
};

static size_t get_os_pagesize(void) {
  static size_t os_pagesize = 0; /* OS page size, in bytes */

  size_t n = os_pagesize;
  if (n == 0) {
#ifdef _SC_PAGESIZE
    long val = sysconf(_SC_PAGESIZE);
    if (val > 0) {
      n = (size_t)val;
    } else
#endif

    /*else */ {
#ifdef __FreeBSD__
      n = getpagesize();
#elif defined(PAGESIZE) && PAGESIZE > 0
      n = PAGESIZE;
#else
      n = 4096; /* guess */
#endif
    }
    os_pagesize = n; /* cache for future calls */
  }
  return n;
}

#if defined(__linux__)
#include "process_linux.h"
#elif defined(__APPLE__)
#include "process_macos.h"
#elif defined(__FreeBSD__)
#include "process_freebsd.h"
#else
#include "process_unsupported.h"
#endif
