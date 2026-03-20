/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <libutil.h> /* kinfo_getallproc() */

#include <sys/time.h>
#include <sys/types.h>
#include <sys/user.h> /* struct kinfo_proc */

static char state_to_char(int state) {
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

static void set_timeval(void* dst, size_t i, struct timeval t) {
  set_uint64(dst, i, (uint32_t)t.tv_usec * 1000);
  set_int64(dst, i + 1, t.tv_sec);
}

static ptr make_tty_name(dev_t tty) {
  if (tty != NODEV) {
    const char* ttyname = devname(tty, S_IFCHR);
    if (ttyname && strcmp(ttyname, "??")) {
      return Sstring(ttyname);
    } else {
      return Svoid; /* unknown */
    }
  } else {
    return Smake_string(0, 0); /* no controlling tty */
  }
}

typedef struct {
  struct kinfo_proc* kp;
  int                pos;
  int                count;
} kprocs;

/** convert Scheme exact integer to C kprocs* */
static kprocs* to_kprocs(ptr addr) {
  uintptr_t val;
  if ((Sfixnump(addr) || Sbignump(addr)) && (val = Sunsigned_value(addr)) != 0) {
    return (kprocs*)(void*)val;
  }
  return NULL;
}

/**
 * on success, return scheme unsigned number containing C kprocs*
 * on error, return c_errno() < 0
 */
static ptr c_process_open(void) {
  kprocs* procs = (kprocs*)malloc(sizeof(kprocs));
  if (!procs) {
    return Sinteger(c_errno()); /* < 0 */
  }
  procs->kp  = kinfo_getallproc(&procs->count);
  procs->pos = 0;
  if (procs == NULL || procs->count < 0) {
    free(procs);
    return Sinteger(c_errno()); /* < 0 */
  }
  return Sunsigned((uintptr_t)(void*)procs);
}

static void c_process_close(ptr addr) {
  kprocs* procs = to_kprocs(addr);
  if (procs) {
    free(procs->kp);
    free(procs);
  }
  (void)set_timespec;
}

/**
 * skip one process pid.
 *   on success, return 1
 *   on end-of-dir, return 0
 */
static int c_process_skip(ptr addr) {
  kprocs* procs = to_kprocs(addr);
  if (!procs || procs->pos >= procs->count) {
    return 0;
  }
  ++procs->pos;
  return 1; /* ok, skipped one pid */
}

/**
 * read one process from kinfo_getallproc()
 *   on success, return cons (command_string . tty_string) and fill bvec.
 *   on end-of-dir, return 0
 */
static ptr c_process_get(ptr addr, ptr bvec) {
  kprocs*            procs = to_kprocs(addr);
  struct kinfo_proc* kp;
  uint8_t*           vec;
  if (!procs || procs->pos >= procs->count) {
    return Sfixnum(0); /* end of processes */
  }
  kp = &procs->kp[procs->pos++];

  if (!Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  vec = Sbytevector_data(bvec);

  memset(vec, '\0', e_byte_n);
  set_int64(vec, e_pid, kp->ki_pid);
  set_int64(vec, e_uid, kp->ki_uid);
  set_int64(vec, e_gid, kp->ki_groups[0]);
  set_int64(vec, e_ppid, kp->ki_ppid);
  set_int64(vec, e_pgid, kp->ki_pgid);
  set_int64(vec, e_sid, kp->ki_sid);
  set_uint64(vec, e_mem_rss, (uint64_t)kp->ki_rssize * get_os_pagesize());
  set_uint64(vec, e_mem_virt, (uint64_t)kp->ki_size);
  set_timeval(vec, e_start_time, kp->ki_start);
  set_timeval(vec, e_user_time, kp->ki_rusage.ru_utime);
  set_timeval(vec, e_sys_time, kp->ki_rusage.ru_stime);
  set_int64(vec, e_priority, 20 - (int)kp->ki_nice);
  set_int64(vec, e_num_thread, kp->ki_numthreads);
  set_uint64(vec, e_min_fault, kp->ki_rusage.ru_minflt);
  set_uint64(vec, e_maj_fault, kp->ki_rusage.ru_majflt);
  vec[e_state * 8] = (uint8_t)state_to_char(kp->ki_stat);

  return Scons(scheme2k_Sstring_utf8b(kp->ki_comm, (size_t)-1), make_tty_name(kp->ki_tdev));
}
