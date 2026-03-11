/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <libproc.h>
#include <sys/sysctl.h>

#include "process_util_macos.h"

static ptr make_tty_name(dev_t tty_nr) {
  if (tty_nr > 0) {
    char     buf[10];
    unsigned hi     = tty_nr >> 16;
    unsigned lo     = tty_nr & 0xFFFF;
    int      buflen = 0;
    if (hi == 0x1000) {
      buflen = snprintf(buf, sizeof(buf), "ttys%03u", lo);
    } else {
      return Svoid;
    }
    /* Sstring_of_length() works as expected only for ASCII chars */
    /*   i.e. 0 < buf[i] < 128 for every i                        */
    return Sstring_of_length(buf, buflen);
  }
  return Sfalse;
}

static char to_state_char(char kp_stat) {
  switch (kp_stat) {
    case SIDL:
      return 'I';
    case SRUN:
      return 'R';
    case SSLEEP:
      return 'S';
    case SSTOP:
      return 'T';
    case SZOMB:
      return 'Z';
    default:
      return 0;
  }
}

static const char* omit_dirs(const char* path) {
  const char* slash = strrchr(path, '/');
  return slash ? slash + 1 : path;
}

static void c_process_info_fill(const struct kinfo_proc* kp, c_process_info* c) {
  struct proc_taskinfo tinfo;
  char                 name[PROC_PIDPATHINFO_MAXSIZE] = {0};

  const struct extern_proc* p = &kp->kp_proc;

  c->pid  = p->p_pid;
  c->uid  = kp->kp_eproc.e_ucred.cr_uid;
  c->gid  = kp->kp_eproc.e_ucred.cr_gid;
  c->ppid = kp->kp_eproc.e_ppid;
  c->pgrp = kp->kp_eproc.e_pgid;
  c->sid  = -1; /* unimplemented */

  c->priority = p->p_priority;
  c->tty      = kp->kp_eproc.e_tdev;
  c->state    = to_state_char(p->p_stat);

#ifdef p_starttime
  c->start_time = timeval_to_timespec(p->p_starttime);
#else
  c->start_time = timeval_to_timespec(p->p_un.__p_starttime);
#endif

  proc_pidpath(c->pid, name, sizeof(name));
  if (!*name) {
    proc_name(c->pid, name, sizeof(name));
  }
  c->name = c_string_new(*name ? omit_dirs(name) : p->p_comm);

  if (proc_pidinfo(c->pid, PROC_PIDTASKINFO, 0, &tinfo, sizeof(tinfo)) == sizeof(tinfo)) {

    c->mem_rss    = tinfo.pti_resident_size;
    c->mem_virt   = tinfo.pti_virtual_size;
    c->user_time  = ns_to_timespec(tinfo.pti_total_user);
    c->sys_time   = ns_to_timespec(tinfo.pti_total_system);
    c->num_thread = tinfo.pti_threadnum;
    c->maj_fault  = tinfo.pti_faults >= 0 ? tinfo.pti_faults : 0;
    c->min_fault  = tinfo.pti_pageins >= 0 ? tinfo.pti_pageins : 0;
  }
}

/** convert Scheme exact integer to C c_process_infos* */
static c_process_infos* to_c_process_infos(ptr cs_s) {
  uintptr_t cs_u;
  if ((Sfixnump(cs_s) || Sbignump(cs_s)) && (cs_u = Sunsigned_value(cs_s)) != 0) {
    return (c_process_infos*)(void*)cs_u;
  }
  return NULL;
}

/**
 * on success, return scheme unsigned number containing C c_process_infos*
 * on error, return c_errno() < 0
 */
static ptr c_process_open(void) {
  struct kinfo_proc* procs;
  c_process_infos*   cs;
  int                mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_ALL, 0};
  size_t             size, n;

  if (sysctl(mib, 4, NULL, &size, NULL, 0) < 0) {
    return Sinteger(c_errno());
  }
  if ((procs = malloc(size)) == NULL) {
    return Sinteger(c_errno_set(ENOMEM));
  }
  if (sysctl(mib, 4, procs, &size, NULL, 0) < 0) {
    free(procs);
    return Sinteger(c_errno());
  }
  n = size / sizeof(struct kinfo_proc);
  if (n == 0) {
    free(procs);
    return Sfixnum(0);
  }
  if ((cs = c_process_infos_new(n)) == NULL) {
    free(procs);
    return Sinteger(c_errno_set(ENOMEM));
  }
  for (size_t i = 0; i < n; i++) {
    c_process_info_fill(&procs[i], &cs->data[i]);
  }
  free(procs);
  return Sunsigned((uintptr_t)(void*)cs);
}

static void c_process_close(ptr cs_s) {
  c_process_infos_del(to_c_process_infos(cs_s));
}

/**
 * skip one process pid in c_process_infos.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_process_skip(ptr cs_s) {
  c_process_infos* cs = to_c_process_infos(cs_s);
  if (cs && cs->i < cs->n) {
    cs->i++;
    return 1; /* ok, skipped one process */
  }
  return 0; /* end of processes */
}

/*
 * read one process from c_process_infos.
 *   on success, return list (command_string, status_char, tty_string) and fill bvec.
 *   on end-of-dir, return 0
 *   on I/O error, return c_errno() < 0
 *   on parsing errors, return #f
 */
static ptr c_process_get(ptr cs_s, ptr bvec) {
  c_process_infos* cs;
  c_process_info*  c;
  uint8_t*         vec;

  if (!Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  vec = Sbytevector_data(bvec);
  cs  = to_c_process_infos(cs_s);
  if (cs == NULL || cs->i >= cs->n) {
    return Sfixnum(0); /* end of processes */
  }
  memset(vec, '\0', e_byte_n);

  c = &cs->data[cs->i];
  cs->i++;

  set_int64(vec, e_pid, c->pid);
  set_int64(vec, e_uid, c->uid);
  set_int64(vec, e_gid, c->gid);
  set_int64(vec, e_ppid, c->ppid);
  set_int64(vec, e_pgrp, c->pgrp);
  set_int64(vec, e_sid, -1); /* unimplemented */
  set_uint64(vec, e_mem_resident, c->mem_rss);
  set_uint64(vec, e_mem_virtual, c->mem_virt);
  set_timespec(vec, e_start_time, c->start_time);
  set_timespec(vec, e_user_time, c->user_time);
  set_timespec(vec, e_sys_time, c->sys_time);
  /* set_timespec(vec, e_iowait_time, ...); unimplemented */
  set_int64(vec, e_priority, c->priority);
  set_int64(vec, e_num_thread, c->num_thread);
  set_uint64(vec, e_min_fault, c->min_fault);
  set_uint64(vec, e_maj_fault, c->maj_fault);

  vec[e_state * 8] = (uint8_t)c->state;

  return Scons(scheme2k_Sstring_utf8b(c->name.data, c->name.len), make_tty_name(c->tty));
}
