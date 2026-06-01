/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_PROCESS_H
#define SCHEME2K_OS_PROCESS_H

#include "../containers/containers.h"

enum {
  e_proc_pid         = 0,  /* int64 */
  e_proc_uid         = 1,  /* int64 */
  e_proc_gid         = 2,  /* int64 */
  e_proc_ppid        = 3,  /* int64 */
  e_proc_pgid        = 4,  /* int64 */
  e_proc_sid         = 5,  /* int64 */
  e_proc_mem_rss     = 6,  /* bytes, uint64 */
  e_proc_mem_virt    = 7,  /* bytes, uint64 */
  e_proc_start_time  = 8,  /* utc,      timespec, uint64 tv_nsec then int64 tv_sec */
  e_proc_user_time   = 10, /* duration, timespec */
  e_proc_sys_time    = 12, /* duration, timespec */
  e_proc_iowait_time = 14, /* duration, timespec */
  e_proc_priority    = 16, /* int64  */
  e_proc_num_thread  = 17, /* int64  */
  e_proc_min_fault   = 18, /* uint64 */
  e_proc_maj_fault   = 19, /* uint64 */
  e_proc_state       = 20, /* char   */
  e_proc_byte_n      = e_proc_state * 8 + 1,
};

size_t scheme2k_os_pagesize(void) {
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

#endif /* SCHEME2K_OS_PROCESS_H */