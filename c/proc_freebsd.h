/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <libutil.h> /* kinfo_getallproc() */
#include <stdio.h>   /* printf() */
#include <stdlib.h>  /* devname(), free() */
#include <string.h>  /* strcmp() */
#include <unistd.h>

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

static void print_ttyname(dev_t tty) {
  if (tty != NODEV) {
    const char* ttyname = devname(tty, S_IFCHR);
    if (ttyname && strcmp(ttyname, "??")) {
      printf("\"%s\"", ttyname);
    } else {
      printf("null"); /* unknown */
    }
  } else {
    printf("\"\""); /* no controlling tty */
  }
}

static void print_process(const struct kinfo_proc* kp) {
  printf("{\"<type>\":\"process-entry\",\"pid\":%d,\"name\":\"%s\",\"tty\":", /* */
         kp->ki_pid,
         kp->ki_comm);

  print_ttyname(kp->ki_tdev);

  printf(",\"state\":\"%c\",\"uid\":%d,\"gid\":%d,\"ppid\":%d,\"pgid\":%d,\"sid\":%d,"
         "\"mem-rss\":%llu,\"mem-virt\":%llu,\"start-time\":{%s%lld.%06u},"
         "\"user-time\":{%s%lld.%06u},\"sys-time\":{%s%lld.%06u},\"iowait-time\":null,"
         "\"priority\":%d,\"threads\":%d,\"min-fault\":%ld,\"maj-fault\":%ld}\n",
         get_state(kp->ki_stat),
         kp->ki_uid,
         kp->ki_groups[0],
         kp->ki_ppid,
         kp->ki_pgid,
         kp->ki_sid,
         (unsigned long long)kp->ki_rssize * getpagesize(), /* mem-rss */
         (unsigned long long)kp->ki_size,                   /* mem-virt */
         "\"<type>\":\"time-utc\",\"value\":",
         (long long)kp->ki_start.tv_sec,
         (unsigned)kp->ki_start.tv_usec,
         "\"<type>\":\"time-duration\",\"value\":",
         (long long)kp->ki_rusage.ru_utime.tv_sec,
         (unsigned)kp->ki_rusage.ru_utime.tv_usec,
         "\"<type>\":\"time-duration\",\"value\":",
         (long long)kp->ki_rusage.ru_stime.tv_sec,
         (unsigned)kp->ki_rusage.ru_stime.tv_usec,
         20 - (int)kp->ki_nice,
         kp->ki_numthreads,
         kp->ki_rusage.ru_minflt,
         kp->ki_rusage.ru_majflt);
}

int main(void) {
  int                count = 0;
  struct kinfo_proc* procs = kinfo_getallproc(&count);

  for (int i = 0; i < count; i++) {
    print_process(&procs[i]);
  }

  free(procs);
  return 0;
}
