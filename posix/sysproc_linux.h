/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <inttypes.h> /* PRId64, PRIu64 */
#include <unistd.h>   /* sysconf() */

static void print_tty_name(int64_t tty_nr) {
  if (tty_nr > 0) {
    uint64_t hi = (uint64_t)tty_nr >> 8;
    unsigned lo = tty_nr & 0xFF;
    if (hi == 136) {
      printf(" tty=/dev/pts%u", lo);
      return;
    } else if (hi == 4) {
      printf(" tty=/dev/tty%u", lo);
      return;
    }
  }
  printf(" tty=#f");
}

static void print_ps_line(const char* pid_or_self) {
  char     path[256];
  char     comm[256];
  FILE*    file;
  double   ticks_per_second;
  uint64_t pid, ppid, pgrp, sid, flags, min_fault, cmin_fault, maj_fault, cmaj_fault;
  uint64_t utime, stime, cutime, cstime, priority, nice, num_threads, start_time;
  uint64_t vsize, rss;
  int64_t  tty_nr, tty_pgrp, rsslim;
  char     state;

  // ---- Read /proc/<pid>/stat ----
  snprintf(path, sizeof(path), "/proc/%s/stat", pid_or_self);
  file = fopen(path, "r");

  file&& read_uint64(file, &pid) && read_string(file, comm, sizeof(comm)) &&
      read_char(file, &state) && read_uint64(file, &ppid) && read_uint64(file, &pgrp) &&
      read_uint64(file, &sid) && read_int64(file, &tty_nr) && read_int64(file, &tty_pgrp) &&
      read_uint64(file, &flags) && read_uint64(file, &min_fault) &&
      read_uint64(file, &cmin_fault) && read_uint64(file, &maj_fault) &&
      read_uint64(file, &cmaj_fault) && read_uint64(file, &utime) && read_uint64(file, &stime) &&
      read_uint64(file, &cutime) && read_uint64(file, &cstime) && read_uint64(file, &priority) &&
      read_uint64(file, &nice) && read_uint64(file, &num_threads) && read_int64(file, NULL) &&
      read_uint64(file, &start_time) && read_uint64(file, &vsize) && read_uint64(file, &rss) &&
      read_int64(file, &rsslim);

  ticks_per_second = (double)sysconf(_SC_CLK_TCK);
  rss *= sysconf(_SC_PAGESIZE); /* convert to bytes */

  printf("pid=%" PRIu64 " comm=%s state=%c ppid=%" PRIu64 " pgrp=%" PRIu64 " sid=%" PRIu64,
         pid,
         comm,
         state,
         ppid,
         pgrp,
         sid);
  print_tty_name(tty_nr);
  printf(" tty_pgrp=%" PRId64 " flags=%" PRIu64 " min_fault=%" PRIu64 " cmin_fault=%" PRIu64
         " maj_fault=%" PRIu64 " cmaj_fault=%" PRIu64
         " utime=%f stime=%f cutime=%f cstime=%f priority=%" PRIu64 " nice=%" PRIu64
         " num_threads=%" PRIu64 " start_time=%f vsize=%" PRIu64 " rss=%" PRIu64 " rsslim=%" PRId64
         "\n",
         tty_pgrp,
         flags,
         min_fault,
         cmin_fault,
         maj_fault,
         cmaj_fault,
         utime / ticks_per_second,
         stime / ticks_per_second,
         cutime / ticks_per_second,
         cstime / ticks_per_second,
         priority,
         nice,
         num_threads,
         start_time / ticks_per_second,
         vsize, /* in bytes */
         rss,   /* in bytes */
         rsslim);

  if (file) {
    fclose(file);
  }
}
