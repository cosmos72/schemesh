/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_PROCESS_BSD_H
#define SCHEME2K_OS_PROCESS_BSD_H

#if defined(__FreeBSD__)

#include <libutil.h> /* kinfo_getallproc() */
#include <sys/time.h>
#include <sys/types.h>
#include <sys/user.h> /* struct kinfo_proc */

#elif defined(__OpenBSD__)

#include <fcntl.h>
#include <kvm.h>
#include <sys/param.h>
#include <sys/proc.h> /* S* process state */
#include <sys/sysctl.h>

#endif

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
#ifdef SDEAD
    case SDEAD:
      return 'Z'; /* almost gone */
#endif
#ifdef SWAIT
    case SWAIT:
      return 'W'; /* wait */
#endif
#ifdef SLOCK
    case SLOCK:
      return 'D'; /* lock */
#endif
#ifdef SONPROC
    case SONPROC:
      return 'I'; /* idle */
#endif
    default:
      return '?'; /* unknown */
  }
}

static void set_timeval2(void* dst, size_t i, int64_t sec, int32_t usec) {
  set_uint64(dst, i, (uint32_t)usec * 1000);
  set_int64(dst, i + 1, sec);
}

#ifdef __FreeBSD__
static void set_timeval(void* dst, size_t i, struct timeval t) {
  set_timeval2(dst, i, t.tv_sec, t.tv_usec);
}
#endif

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
#ifdef __OpenBSD__
  kvm_t* kvm;
#endif
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
  procs->pos = 0;
#if defined(__FreeBSD__)
  procs->kp = kinfo_getallproc(&procs->count);
  if (procs->kp == NULL || procs->count < 0) {
    free(procs->kp);
#elif defined(__OpenBSD__)
  procs->kvm = kvm_open(NULL, NULL, NULL, KVM_NO_FILES, NULL);
  procs->kp =
      procs->kvm ?
          kvm_getprocs(procs->kvm, KERN_PROC_ALL, 0, sizeof(struct kinfo_proc), &procs->count) :
          NULL;
  if (procs->kp == NULL || procs->count < 0) {
    if (procs->kvm) {
      kvm_close(procs->kvm);
    }
#else
  procs->kp = NULL;
  if (procs->kp == NULL || procs->count < 0) {
#endif
    free(procs);
    return Sinteger(c_errno()); /* < 0 */
  }
  return Sunsigned((uintptr_t)(void*)procs);
}

static void c_process_close(ptr addr) {
  kprocs* procs = to_kprocs(addr);
  if (procs) {
#if defined(__FreeBSD__)
    free(procs->kp);
#elif defined(__OpenBSD__)
    kvm_close(procs->kvm);
#endif
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
  int     pos;
  if (!procs || (pos = procs->pos) >= procs->count) {
    return 0;
  }
  procs->pos = pos + 1;
  return 1; /* ok, skipped one pid */
}

/**
 * read one process from kinfo_getallproc()
 *   on success, return cons (command_string . tty_string) and fill bvec.
 *   on end-of-dir, return 0
 */
static ptr c_process_get(ptr addr, ptr bvec) {
  const uint64_t           pagesz = scheme2k_os_pagesize();
  kprocs*                  procs  = to_kprocs(addr);
  const struct kinfo_proc* kp;
  uint8_t*                 vec;
  int                      pos;
  if (!procs || (pos = procs->pos) >= procs->count) {
    return Sfixnum(0); /* end of processes */
  }
  procs->pos = pos + 1;
  kp         = &procs->kp[pos];

  if (!Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_proc_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  vec = Sbytevector_data(bvec);

#if defined(__FreeBSD__)
#define KP(field) kp->ki_##field
#elif defined(__OpenBSD__)
#define KP(field) kp->p_##field
#endif

  memset(vec, '\0', e_proc_byte_n);
  set_int64(vec, e_proc_pid, KP(pid));
  set_int64(vec, e_proc_uid, KP(uid));
  set_int64(vec, e_proc_gid, KP(groups[0]));
  set_int64(vec, e_proc_ppid, KP(ppid));
  set_int64(vec, e_proc_sid, KP(sid));
  set_int64(vec, e_proc_nice, (int)KP(nice) - 20);

#if defined(__FreeBSD__)
  set_int64(vec, e_proc_pgid, KP(pgid));
  set_uint64(vec, e_proc_mem_rss, (uint64_t)KP(rssize) * pagesz);
  set_uint64(vec, e_proc_mem_virt, (uint64_t)KP(size));
  set_timeval(vec, e_proc_start_time, KP(start));
  set_timeval(vec, e_proc_user_time, KP(rusage.ru_utime));
  set_timeval(vec, e_proc_sys_time, KP(rusage.ru_stime));
  set_int64(vec, e_proc_num_thread, KP(numthreads));
  set_uint64(vec, e_proc_min_fault, KP(rusage.ru_minflt));
  set_uint64(vec, e_proc_maj_fault, KP(rusage.ru_majflt));
#elif defined(__OpenBSD__)
  set_int64(vec, e_proc_pgid, KP(_pgid));
  set_uint64(vec, e_proc_mem_rss, (uint64_t)KP(vm_rssize) * pagesz);
  set_uint64(vec, e_proc_mem_virt, ((uint64_t)KP(vm_tsize) + KP(vm_dsize) + KP(vm_ssize)) * pagesz);
  set_timeval2(vec, e_proc_start_time, KP(ustart_sec), KP(ustart_usec));
  set_timeval2(vec, e_proc_user_time, KP(uutime_sec), KP(uutime_usec));
  set_timeval2(vec, e_proc_sys_time, KP(ustime_sec), KP(ustime_usec));
  // set_int64(vec, e_proc_num_thread, KP(numthreads)); // not available
  set_uint64(vec, e_proc_min_fault, KP(uru_minflt));
  set_uint64(vec, e_proc_maj_fault, KP(uru_majflt));
#endif
  vec[e_proc_state * 8] = (uint8_t)state_to_char(KP(stat));

  return Scons(C_SSTRING_UTF8b(KP(comm)), make_tty_name(KP(tdev)));
#undef KP
}

#endif /* SCHEME2K_OS_PROCESS_BSD_H */
