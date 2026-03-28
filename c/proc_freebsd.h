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
#include <unistd.h>  /* getuid() */

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

static void print_ttyname(writer* w, dev_t tty) {
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

static void w_put_timeval(writer* w, const struct timeval* tv) {
  w_put_int64(w, tv->tv_sec);
  w_put_literal(w, ".");
  w_put_fractional_digits(w, (uint32_t)tv->tv_usec, 6);
}

static void print_process(const struct kinfo_proc* kp, writer* w, e_proc_flags flags, uid_t uid) {
  if ((flags & e_proc_flag_other_users) == 0 && kp->ki_uid != uid) {
    return;
  }
  if ((flags & e_proc_flag_without_tty) == 0 && kp->ki_tdev == NODEV) {
    return;
  }
  w_put_literal(w, "{\"<type>\":\"process-entry\"");
  if (flags & e_proc_flag_pid) {
    w_put_literal(w, ",\"pid\":");
    w_put_int64(w, kp->ki_pid);
  }
  if (flags & e_proc_flag_name) {
    w_put_literal(w, ",\"name\":");
    w_put_quoted_escaped_chars(w, kp->ki_comm);
  }
  if (flags & e_proc_flag_tty) {
    w_put_literal(w, ",\"tty\":");
    print_ttyname(w, kp->ki_tdev);
  }
  if (flags & e_proc_flag_state) {
    char c = get_state(kp->ki_stat);
    w_put_literal(w, ",\"state\":");
    w_put_quoted_escaped_chars_len(w, &c, 1);
  }
  if (flags & e_proc_flag_user) {
    /** TODO: implement */
  }
  if (flags & e_proc_flag_group) {
    /** TODO: implement */
  }
  if (flags & e_proc_flag_uid) {
    w_put_literal(w, ",\"uid\":");
    w_put_int64(w, kp->ki_uid);
  }
  if (flags & e_proc_flag_gid) {
    w_put_literal(w, ",\"gid\":");
    w_put_int64(w, kp->ki_groups[0]);
  }
  if (flags & e_proc_flag_ppid) {
    w_put_literal(w, ",\"ppid\":");
    w_put_int64(w, kp->ki_ppid);
  }
  if (flags & e_proc_flag_pgid) {
    w_put_literal(w, ",\"pgid\":");
    w_put_int64(w, kp->ki_pgid);
  }
  if (flags & e_proc_flag_sid) {
    w_put_literal(w, ",\"sid\":");
    w_put_int64(w, kp->ki_sid);
  }
  if (flags & e_proc_flag_mem_rss) {
    w_put_literal(w, ",\"mem-rss\":");
    w_put_uint64(w, (uint64_t)kp->ki_rssize * getpagesize());
  }
  if (flags & e_proc_flag_mem_virt) {
    w_put_literal(w, ",\"mem-virt\":");
    w_put_uint64(w, kp->ki_size);
  }
  if (flags & e_proc_flag_start_time) {
    w_put_literal(w, ",\"start-time\":{\"<type>\":\"time-utc\",\"value\":");
    w_put_timeval(w, &kp->ki_start);
    w_put_literal(w, "}");
  }
  if (flags & e_proc_flag_user_time) {
    w_put_literal(w, ",\"user-time\":{\"<type>\":\"time-duration\",\"value\":");
    w_put_timeval(w, &kp->ki_rusage.ru_utime);
    w_put_literal(w, "}");
  }
  if (flags & e_proc_flag_sys_time) {
    w_put_literal(w, ",\"sys-time\":{\"<type>\":\"time-duration\",\"value\":");
    w_put_timeval(w, &kp->ki_rusage.ru_stime);
    w_put_literal(w, "}");
  }
  if (flags & e_proc_flag_iowait_time) {
    w_put_literal(w, ",\"iowait-time\":null");
  }
  if (flags & e_proc_flag_priority) {
    w_put_literal(w, ",\"priority\":");
    w_put_int64(w, 20 - (int)kp->ki_nice);
  }
  if (flags & e_proc_flag_threads) {
    w_put_literal(w, ",\"threads\":");
    w_put_int64(w, kp->ki_numthreads);
  }
  if (flags & e_proc_flag_min_fault) {
    w_put_literal(w, ",\"min-fault\":");
    w_put_int64(w, kp->ki_rusage.ru_minflt);
  }
  if (flags & e_proc_flag_maj_fault) {
    w_put_literal(w, ",\"maj-fault\":");
    w_put_int64(w, kp->ki_rusage.ru_majflt);
  }
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
