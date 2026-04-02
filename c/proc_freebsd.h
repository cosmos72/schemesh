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

#include <libutil.h> /* kinfo_getallproc() */
#include <stdio.h>   /* printf() */
#include <stdlib.h>  /* devname(), free() */
#include <string.h>  /* strcmp() */

#include <sys/stat.h> /* S_IFCH */
#include <sys/time.h>
#include <sys/types.h>
#include <sys/user.h>

static char get_state(int state) {
  switch (state) {
    case SIDL:
      return 'I'; /* idle */
    case SRUN:
      return 'R'; /* run */
    case SSLEEP:
      return 'S'; /* sleep */
    case SSTOP:
      return 'T'; /* stop*/
    case SZOMB:
      return 'Z'; /* zombie */
    case SWAIT:
      return 'W'; /* wait */
    case SLOCK:
      return 'D'; /* lock */
    default:
      return '?'; /* unknown */
  }
}

static void put_ttyname(writer* w, e_proc_flags flags, dev_t tty) {
  if (!flags) {
    return;
  }
  w_put_literal(w, ",\"tty\":");
  if (tty != NODEV) {
    const char* ttyname = devname(tty, S_IFCHR);
    if (ttyname && strcmp(ttyname, "??")) {
      w_put_quoted_escaped_chars(w, ttyname);
    } else {
      w_put_literal(w, "null"); /* unknown */
    }
  } else {
    w_put_literal(w, "\"\""); /* no controlling tty */
  }
}

static void put_timeval_any(writer* w, const struct timeval* tv, chars prefix, chars prefix2) {
  w_put_chars(w, prefix);
  w_put_chars(w, prefix2);
  w_put_int64(w, tv->tv_sec);
  w_put_char(w, '.');
  w_put_fractional_digits(w, (uint32_t)tv->tv_usec, 6);
  w_put_char(w, '}');
}

static void put_timeval_utc(writer* w, e_proc_flags flags, const struct timeval* tv, chars prefix) {
  if (flags) {
    put_timeval_any(w, tv, prefix, CHARS("{\"<type>\":\"time-utc\",\"value\":"));
  }
}

static void put_timeval(writer* w, e_proc_flags flags, const struct timeval* tv, chars prefix) {
  if (flags) {
    put_timeval_any(w, tv, prefix, CHARS("{\"<type>\":\"time-duration\",\"value\":"));
  }
}

static void print_process(const struct kinfo_proc* kp, writer* w, e_proc_flags flags, uid_t uid) {
  uint64_t mem_rss;
  if ((flags & e_proc_flag_other_users) == 0 && kp->ki_uid != uid) {
    return;
  }
  if ((flags & e_proc_flag_without_tty) == 0 && kp->ki_tdev == NODEV) {
    return;
  }
  w_put_literal(w, "{\"<type>\":\"process-entry\"");
  put_int64(w, flags & e_proc_flag_pid, kp->ki_pid, CHARS(",\"pid\":"));
  put_command(w, flags & e_proc_flag_name, make_chars(kp->ki_comm, (size_t)-1));
  put_ttyname(w, flags & e_proc_flag_tty, kp->ki_tdev);
  put_state(w, flags & e_proc_flag_state, get_state(kp->ki_stat));

  put_username(w, flags & e_proc_flag_user, kp->ki_uid);
  put_groupname(w, flags & e_proc_flag_group, kp->ki_groups[0]);
  put_int64(w, flags & e_proc_flag_uid, kp->ki_uid, CHARS(",\"uid\":"));
  put_int64(w, flags & e_proc_flag_gid, kp->ki_groups[0], CHARS(",\"gid\":"));

  put_int64(w, flags & e_proc_flag_ppid, kp->ki_ppid, CHARS(",\"ppid\":"));
  put_int64(w, flags & e_proc_flag_pgid, kp->ki_pgid, CHARS(",\"pgid\":"));
  put_int64(w, flags & e_proc_flag_sid, kp->ki_sid, CHARS(",\"sid\":"));
  mem_rss = (uint64_t)kp->ki_rssize * get_os_pagesize();
  put_uint64(w, flags & e_proc_flag_mem_rss, mem_rss, CHARS(",\"mem-rss\":"));
  put_uint64(w, flags & e_proc_flag_mem_virt, kp->ki_size, CHARS(",\"mem-virt\":"));

  put_timeval_utc(w, flags & e_proc_flag_start_time, &kp->ki_start, CHARS(",\"start-time\":"));
  put_timeval(w, flags & e_proc_flag_user_time, &kp->ki_rusage.ru_utime, CHARS(",\"user-time\":"));
  put_timeval(w, flags & e_proc_flag_sys_time, &kp->ki_rusage.ru_stime, CHARS(",\"sys-time\":"));
  if (flags & e_proc_flag_iowait_time) {
    w_put_literal(w, ",\"iowait-time\":null");
  }
  put_int64(w, flags & e_proc_flag_priority, 20 - (int)kp->ki_nice, CHARS(",\"priority\":"));
  put_int64(w, flags & e_proc_flag_threads, kp->ki_numthreads, CHARS(",\"threads\":"));
  put_int64(w, flags & e_proc_flag_min_fault, kp->ki_rusage.ru_minflt, CHARS(",\"min-fault\":"));
  put_int64(w, flags & e_proc_flag_maj_fault, kp->ki_rusage.ru_minflt, CHARS(",\"maj-fault\":"));
  w_put_literal(w, "}\n");
  w_flush(w);
}

static void print_processes(writer* w, e_proc_flags flags) {
  int                count = 0;
  struct kinfo_proc* procs = kinfo_getallproc(&count);
  uid_t              uid   = getuid();

  for (int i = 0; i < count; i++) {
    print_process(&procs[i], w, flags, uid);
  }

  free(procs);
}

#endif /* SCHEME2K_C_PROC_IMPL_H */
