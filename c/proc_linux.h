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

#if 0 /* UNFINISHED */

#include <dirent.h>    /* opendir()        */
#include <errno.h>     /* errno            */
#include <string.h>    /* strerror()       */
#include <sys/stat.h>  /* fstat()          */
#include <sys/types.h> /* opendir()        */
#include <time.h>      /* CLK_TCK          */
#include <unistd.h>    /* sysconf(), _SC_* */

static int string_is_decimal_number(const char* str) {
  char ch;
  /* str must start with [1-9] */
  if ((ch = *str) < '1' || ch > '9') {
    return 0;
  }
  /* str can only continue with [0-9]* */
  while ((ch = *str) >= '0' && ch <= '9') {
    str++;
  }
  return *str == '\0';
}

static uint64_t get_os_tick_per_s(void) {
  static uint64_t os_tick_per_s = 0; /* OS clock ticks per second */

  uint64_t n = os_tick_per_s;
  if (n == 0) {
#ifdef _SC_CLK_TCK
    long val = sysconf(_SC_CLK_TCK);
    if (val > 0) {
      n = (uint64_t)val;
    } else
#endif

    /*else */ {
#if defined(CLK_TCK) && CLK_TCK > 0
      n = CLK_TCK;
#else
      n = 100; /* guess */
#endif
    }
    os_tick_per_s = n; /* cache for future calls */
  }
  return n;
}

static struct timespec timespec_sub(struct timespec left, struct timespec right) {
  struct timespec ret;

  int borrow = (ret.tv_nsec = left.tv_nsec - right.tv_nsec) < 0;
  if (borrow) {
    ret.tv_nsec += 1000000000;
  }
  ret.tv_sec = left.tv_sec - right.tv_sec - borrow;
  return ret;
}

static struct timespec timespec_avg(struct timespec left, struct timespec right) {
  struct timespec ret;
  int64_t         lo_s    = min_int64(left.tv_sec, right.tv_sec);
  int64_t         hi_s    = max_int64(left.tv_sec, right.tv_sec);
  int64_t         delta_s = hi_s - lo_s;
  uint32_t        ns      = (left.tv_nsec + right.tv_nsec + 1u) / 2 + (delta_s & 1 ? 500000000 : 0);
  int             carry   = ns >= 1000000000;
  if (carry) {
    ns -= 1000000000;
  }
  ret.tv_nsec = ns;
  ret.tv_sec  = lo_s + delta_s / 2 + carry;
  return ret;
}

static struct timespec boot_time_utc = {0, -1};

static struct timespec get_boot_time_utc(void) {
  struct timespec ret = boot_time_utc;
  if (ret.tv_nsec < 0) {
    struct timespec elapsed_since_boot1;
    struct timespec time_utc;
    struct timespec elapsed_since_boot2;
    if (clock_gettime(CLOCK_BOOTTIME, &elapsed_since_boot1) == 0 && /**/
        clock_gettime(CLOCK_REALTIME, &time_utc) == 0 &&
        clock_gettime(CLOCK_BOOTTIME, &elapsed_since_boot2) == 0) {

      ret = timespec_sub(time_utc, /* */
                         timespec_avg(elapsed_since_boot1, elapsed_since_boot2));
    } else {
      /* failed to get elapsed_since_boot and time_utc */
      ret.tv_nsec = 0;
    }
    boot_time_utc = ret;
  }
  return ret;
}

static void fd_stat_uid_gid(int fd, int64_t* uid, int64_t* gid) {
  if (uid || gid) {
    struct stat statbuf;
    const int   err = fstat(fd, &statbuf);
    if (uid) {
      *uid = err ? -1 : (int64_t)statbuf.st_uid;
    }
    if (gid) {
      *gid = err ? -1 : (int64_t)statbuf.st_gid;
    }
  }
}

static unsigned char* read_file_at(
    int dir_fd, const char path[], unsigned char dst[], size_t dstlen, int64_t* uid, int64_t* gid) {
  ssize_t n;
  size_t  end;
  int     fd = openat(dir_fd, path, O_RDONLY);
  if (fd < 0) {
    return NULL;
  }
  fd_stat_uid_gid(fd, uid, gid);
  n = read(fd, dst, dstlen - 1);
  close(fd);
  end      = n < 0 ? 0 : (size_t)n < dstlen ? (size_t)n : dstlen - 1;
  dst[end] = '\0';
  return dst + end;
}

static const unsigned char* skip_ws(const unsigned char* src) {
  unsigned char ch;
  while ((ch = *src) > 0 && ch <= ' ') {
    ++src;
  }
  return src;
}

/**
 * skip whitespace, copy command name in parentheses (...) to dst.
 * Command name may contain any character, including spaces and ')'
 * Solution: scan src from end and skip all numbers and status, stop at ')'
 * Always add '\0' terminator to dst.
 * @return pointer to chars after command name.
 */
static const unsigned char*
print_command(writer* w, const unsigned char* src, const unsigned char* src_end) {
  const unsigned char* ret = NULL;
  src                      = skip_ws(src);
  /* skip initial '(' */
  if (*src == '(') {
    ++src;
  }
  while (src_end > src) {
    --src_end;
    /* skip final ')' and exit loop */
    if (*src_end == ')') {
      ret = src_end + 1;
      break;
    }
  }
  w_put_quoted_escaped_chars_len(w, src, src_end - src);
  return ret ? ret : src_end;
}

static void print_tty_name(writer* w, int64_t tty_nr) {
  if (tty_nr > 0) {
    char     buf[16];
    uint64_t hi     = (uint64_t)tty_nr >> 8;
    unsigned lo     = tty_nr & 0xFF;
    int      buflen = 0;
    if (hi == 136) {
      buflen = snprintf(buf, sizeof(buf), "pts/%u", lo);
    } else if (hi == 4) {
      if (lo < 64) {
        buflen = snprintf(buf, sizeof(buf), "tty%u", lo);
      } else {
        buflen = snprintf(buf, sizeof(buf), "ttyS%u", lo - 64);
      }
    } else if (hi == 5 && lo == 1) {
      w_put_literal(w, "\"console\"");
    } else {
      w_put_literal(w, "null");
    }
    if (buflen > 0) {
      w_put_quoted_escaped_chars(buf, (size_t)buflen);
    }
  } else {
    w_put_literal(w, "\"\"");
  }
}

/**
 * read one process from /proc and print it as NDJSON
 */
static void print_process_get(
    DIR* dir, const struct dirent* entry, writer* w, e_proc_flags flags, uid_t my_uid) {
  char                 buf[4096];
  const unsigned char* src;
  const unsigned char* src_end;
  DIR*                 dir;
  struct dirent*       entry;
  uint64_t             tick_per_s, start_time_ticks;
  uint64_t             user_time_ticks, sys_time_ticks, iowait_time_ticks;
  int64_t              uid, gid, tty_nr;
  unsigned             i;
  int                  buf_written;
  char                 state;
  uint8_t              ok;

  buf_written = snprintf(buf, sizeof(buf), "%s/stat", entry->d_name);

  src_end = src = (const unsigned char*)buf;

  ok = buf_written > 0 && (unsigned)buf_written < sizeof(buf) &&
       (src_end = read_file_at(dirfd(dir), buf, (unsigned char*)buf, sizeof(buf), &uid, &gid));

  parse_int64(&src, vec, e_pid) && parse_linux_command(&src, src_end, comm, sizeof(comm)) &&
      parse_char(&src, &state) && parse_int64(&src, vec, e_ppid) &&
      parse_int64(&src, vec, e_pgid) && parse_int64(&src, vec, e_sid) &&
      parse_int64(&src, &tty_nr, 0) && parse_int64(&src, NULL, 0 /*tty_pgrp*/) &&
      parse_uint64(&src, NULL, 0 /*flags*/) && parse_uint64(&src, vec, e_min_fault) &&
      parse_uint64(&src, NULL, 0 /*child_min_fault*/) && parse_uint64(&src, vec, e_maj_fault) &&
      parse_uint64(&src, NULL, 0 /*child_maj_fault*/) && parse_uint64(&src, &user_time_ticks, 0) &&
      parse_uint64(&src, &sys_time_ticks, 0) && parse_int64(&src, NULL, 0 /*child_user_time*/) &&
      parse_int64(&src, NULL, 0 /*child_sys_time*/) && parse_int64(&src, vec, e_priority) &&
      parse_int64(&src, NULL, 0 /*nice*/) && parse_int64(&src, vec, e_num_thread) &&
      parse_int64(&src, NULL, 0 /*obsolete*/) && parse_uint64(&src, &start_time_ticks, 0) &&
      parse_uint64(&src, vec, e_mem_virt) && parse_uint64(&src, vec, e_mem_rss);

  if (!ok) {
    fprintf(stderr, "proc: error parsing /proc/%s\n", entry->d_name);
    fflush(stderr);
    return;
  }

  /* skip fields 25...39 */
  for (i = 25; i < 40 && parse_uint64(&src, NULL, 0); i++) {
  }
  iowait_time_ticks = 0;
  if (i == 40) {
    ok = parse_uint64(&src, NULL, 0 /*rt_priority*/) && /**/
         parse_uint64(&src, NULL, 0 /*rt_policy*/) &&   /**/
         parse_uint64(&src, &iowait_time_ticks, 0);
  }

  set_int64(vec, e_uid, uid);
  set_int64(vec, e_gid, gid);

  /* convert mem_resident from pages to bytes */
  uint64_multiply(vec, e_mem_rss, get_os_pagesize());

  tick_per_s = get_os_tick_per_s();

  /* convert ticks -> struct timespec */
  {
    struct timespec start_time_utc =
        timespec_add(get_boot_time_utc(), ticks_to_timespec(start_time_ticks, tick_per_s));
    set_timespec(vec, e_start_time, start_time_utc);
  }
  {
    struct timespec user_time = ticks_to_timespec(user_time_ticks, tick_per_s);
    set_timespec(vec, e_user_time, user_time);
  }
  {
    struct timespec sys_time = ticks_to_timespec(sys_time_ticks, tick_per_s);
    set_timespec(vec, e_sys_time, sys_time);
  }
  {
    struct timespec iowait_time = ticks_to_timespec(iowait_time_ticks, tick_per_s);
    set_timespec(vec, e_iowait_time, iowait_time);
  }

  vec[e_state * 8] = (uint8_t)state;

  return Scons(scheme2k_Sstring_utf8b(comm, (size_t)-1), make_tty_name(tty_nr));
}
#if 0
#endif /* 0 */
return Sfalse;
}

static struct dirent* process_get_next_entry(DIR* dir) {
  struct dirent* entry;

  errno = 0;
  do {
    entry = readdir(dir);
  } while (entry && !string_is_decimal_number(entry->d_name));

  if (!entry) {
    int err = errno; /* 0 if end of dir, otherwise error */
    if (err != 0) {
      fprintf(stderr, "proc: error %d in readdir(\"/proc\"): %s\n", err, strerror(err));
      closedir(dir);
      exit(1);
    }
  }
  return entry;
}

static void print_processes(writer* w, e_proc_flags flags) {
  DIR*           dir = opendir("/proc");
  struct dirent* entry;
  uid_t          uid;
  if (!dir) {
    int err = errno;
    fprintf(stderr, "proc: error %d in opendir(\"/proc\"): %s\n", err, strerror(err));
    exit(1);
  }
  uid = getuid();
  while ((entry = process_get_next_entry(dir)) != NULL) {
    print_process(dir, entry, &w, flags, uid);
    w_flush(&w);
  }
  closedir(dir);
}

#else

static void print_processes(writer* w, e_proc_flags flags) {
  (void)w;
  (void)flags;
  fputs("proc: unsupported operating system\n", stderr);
  exit(1);
}

#endif /* 0 */

#endif /* SCHEME2K_C_PROC_IMPL_H */
