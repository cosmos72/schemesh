/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_C_PROC_COMMON_H
#define SCHEME2K_C_PROC_COMMON_H

#include <grp.h>       /* getgrgid() */
#include <pwd.h>       /* getpwuid() */
#include <stddef.h>    /* size_t     */
#include <sys/types.h> /* getgrgid(), getpwuid() */
#include <unistd.h>    /* getuid(), sysconf(), _SC_* */

typedef enum {
  e_proc_flag_pid         = 1 << 0,
  e_proc_flag_name        = 1 << 1,
  e_proc_flag_tty         = 1 << 2,
  e_proc_flag_state       = 1 << 3,
  e_proc_flag_user        = 1 << 4,
  e_proc_flag_group       = 1 << 5,
  e_proc_flag_uid         = 1 << 6,
  e_proc_flag_gid         = 1 << 7,
  e_proc_flag_ppid        = 1 << 8,
  e_proc_flag_pgid        = 1 << 9,
  e_proc_flag_sid         = 1 << 10,
  e_proc_flag_mem_rss     = 1 << 11,
  e_proc_flag_mem_virt    = 1 << 12,
  e_proc_flag_start_time  = 1 << 13,
  e_proc_flag_user_time   = 1 << 14,
  e_proc_flag_sys_time    = 1 << 15,
  e_proc_flag_iowait_time = 1 << 16,
  e_proc_flag_priority    = 1 << 17,
  e_proc_flag_threads     = 1 << 18,
  e_proc_flag_min_fault   = 1 << 19,
  e_proc_flag_maj_fault   = 1 << 20,

  e_proc_flag_default =
      e_proc_flag_pid | e_proc_flag_name | e_proc_flag_tty | e_proc_flag_start_time,
  e_proc_flag_long = e_proc_flag_pid | e_proc_flag_name | e_proc_flag_tty | e_proc_flag_state |
                     e_proc_flag_user | e_proc_flag_mem_rss | e_proc_flag_start_time |
                     e_proc_flag_user_time,
  e_proc_flag_verbose = (1 << 21) - 1,

  e_proc_flag_other_users = 1 << 21, /* show processes belonging to other users */
  e_proc_flag_without_tty = 1 << 22, /* show processes not attached to a tty */
} e_proc_flags;

typedef struct {
  const unsigned char* data;
  size_t               size;
} chars;

typedef struct {
  unsigned char* data;
  size_t         size;
} charspan;

#define CHARS(str) make_chars((const unsigned char*)(str), sizeof(str) - 1)

static chars make_chars(const unsigned char data[], size_t size) {
  chars cs = {data, size};
  return cs;
}

static void w_put_chars(writer* w, chars cs) {
  w_put_chars_len(w, (const char*)cs.data, cs.size);
}

static void put_command(writer* w, e_proc_flags flags, chars cmd) {
  if (flags) {
    w_put_literal(w, ",\"name\":");
    w_put_quoted_escaped_chars_len(w, (const char*)cmd.data, cmd.size);
  }
}

static void put_state(writer* w, e_proc_flags flags, char state) {
  if (!flags || state == '\0') {
    return;
  }
  w_put_literal(w, ",\"state\":\"");
  w_put_char(w, state);
  w_put_char(w, '"');
}

static void put_username(writer* w, e_proc_flags flags, uid_t uid) {
  static struct passwd* pwd = NULL; /* cache */
  if (!flags) {
    return;
  }
  if (!pwd || !pwd->pw_name || pwd->pw_uid != uid) {
    /* call again only if cached pwd is not applicable */
    pwd = getpwuid(uid);
  }
  if (pwd && pwd->pw_name) {
    w_put_literal(w, ",\"user\":");
    w_put_quoted_escaped_chars(w, pwd->pw_name);
  }
}

static void put_groupname(writer* w, e_proc_flags flags, gid_t gid) {
  static struct group* grp = NULL; /* cache */
  if (!flags) {
    return;
  }
  if (!grp || !grp->gr_name || grp->gr_gid != gid) {
    /* call again only if cached grp is not applicable */
    grp = getgrgid(gid);
  }
  if (grp && grp->gr_name) {
    w_put_literal(w, ",\"group\":");
    w_put_quoted_escaped_chars(w, grp->gr_name);
  }
}

static void put_int64(writer* w, e_proc_flags flags, int64_t num, chars prefix) {
  if (!flags) {
    return;
  }
  w_put_chars(w, prefix);
  w_put_int64(w, num);
}

static void put_uint64(writer* w, e_proc_flags flags, uint64_t num, chars prefix) {
  if (!flags) {
    return;
  }
  w_put_chars(w, prefix);
  w_put_uint64(w, num);
}

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

#endif /* SCHEME2K_C_PROC_COMMON_H */
