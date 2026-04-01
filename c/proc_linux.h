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

#if 1 /* UNFINISHED */

#include <dirent.h>    /* opendir()        */
#include <errno.h>     /* errno            */
#include <fcntl.h>     /* openat()         */
#include <string.h>    /* strerror()       */
#include <sys/stat.h>  /* fstat()          */
#include <sys/types.h> /* opendir()        */
#include <time.h>      /* CLK_TCK          */
#include <unistd.h>    /* sysconf(), _SC_* */

typedef struct {
  const unsigned char* data;
  size_t               size;
} chars;

typedef struct {
  unsigned char* data;
  size_t         size;
} charspan;

#define make_chars_literal(str) make_chars((const unsigned char*)(str), sizeof(str) - 1)

static chars make_chars(const unsigned char data[], size_t size) {
  chars cs = {data, size};
  return cs;
}

static chars chars_skip1(chars cs) {
  if (cs.size) {
    cs.data++;
    cs.size--;
  }
  return cs;
}

static charspan make_charspan(unsigned char data[], size_t size) {
  charspan cs = {data, size};
  return cs;
}

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

static int64_t max_int64(int64_t left, int64_t right) {
  return left > right ? left : right;
}

static int64_t min_int64(int64_t left, int64_t right) {
  return left < right ? left : right;
}

static struct timespec timespec_add(struct timespec left, struct timespec right) {
  struct timespec ret;

  int carry = (ret.tv_nsec = left.tv_nsec + right.tv_nsec) >= 1000000000;
  if (carry) {
    ret.tv_nsec -= 1000000000;
  }
  ret.tv_sec = left.tv_sec + right.tv_sec + carry;
  return ret;
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

static struct timespec ticks_to_timespec(uint64_t ticks, uint64_t tick_per_s) {
  struct timespec ret;
  uint64_t        remainder = ticks % tick_per_s;

  ret.tv_sec = ticks / tick_per_s;
  /* requires tick_per_s < MAX_UINT64 / 1'000'000'000 */
  ret.tv_nsec = (remainder * 1000000000 + tick_per_s / 2) / tick_per_s;
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

static chars read_file_at(int dir_fd, const char path[], charspan dst, int64_t* uid, int64_t* gid) {
  ssize_t n;
  size_t  end;
  int     fd = openat(dir_fd, path, O_RDONLY);
  if (fd < 0) {
    return make_chars(NULL, 0);
  }
  fd_stat_uid_gid(fd, uid, gid);
  n = read(fd, dst.data, dst.size);
  close(fd);
  end = n < 0 ? 0 : (size_t)n < dst.size ? (size_t)n : dst.size;
  return make_chars(dst.data, end);
}

static chars skip_ws(chars src) {
  size_t i;
  for (i = 0; i < src.size && src.data[i] <= ' '; i++) {
  }
  return make_chars(src.data + i, src.size - i);
}

static chars parse_uint64_noskip_ws(chars src, uint64_t* ret) {
  uint64_t      n = 0;
  unsigned char ch;

  if (src.size == 0 || (ch = src.data[0]) < '0' || ch > '9') {
    return make_chars(NULL, 0);
  }
  do {
    src = chars_skip1(src);
    n   = n * 10 + (unsigned)(ch - '0');
  } while (src.size > 0 && (ch = src.data[0]) >= '0' && ch <= '9');

  if (ret) {
    *ret = n;
  }
  return src;
}

static chars parse_int64_noskip_ws(chars src, int64_t* ret) {
  uint64_t n;
  char     negative;

  if (src.size > 0 && src.data[0] == '-') {
    src      = chars_skip1(src);
    negative = 1;
  } else {
    negative = 0;
  }
  src = parse_uint64_noskip_ws(src, &n);
  if (ret) {
    *ret = negative ? -(int64_t)n : (int64_t)n;
  }
  return src;
}

static chars parse_uint64(chars src, uint64_t* ret) {
  return parse_uint64_noskip_ws(skip_ws(src), ret);
}

static chars parse_int64(chars src, int64_t* ret) {
  return parse_int64_noskip_ws(skip_ws(src), ret);
}

/**
 * skip whitespace, set cmd to point to command name in parentheses (...).
 * Command name may contain any character, including spaces and ')'
 * Solution: scan src from end and skip all numbers and status, stop at ')'
 * @return pointer to chars after command name.
 */
static chars parse_command(chars src, chars* cmd) {
  const unsigned char* end;
  int                  skip_rparen;

  src = skip_ws(src);
  /* skip initial '(' */
  if (src.size != 0 && src.data[0] == '(') {
    src = chars_skip1(src);
  }
  end         = src.data + src.size;
  skip_rparen = 0;
  while (end > src.data) {
    --end;
    /* skip final ')' and exit loop */
    if (*end == ')') {
      skip_rparen = 1;
      break;
    }
  }
  if (cmd) {
    *cmd = make_chars(src.data, end - src.data);
  }
  return make_chars(end + skip_rparen, src.size - (end + skip_rparen - src.data));
}

/**
 * skip whitespace, get next char and store it in *state
 * if char could not be read, store '\0' in *state.
 */
static chars parse_char(chars src, char* state) {
  src = skip_ws(src);
  if (src.size > 0) {
    if (state) {
      *state = src.data[0];
    }
    src = chars_skip1(src);
  } else if (state) {
    *state = '\0';
  }
  return src;
}

static void put_chars(writer* w, chars cs) {
  w_put_chars_len(w, (const char*)cs.data, cs.size);
}

static void put_command(writer* w, e_proc_flags flags, chars cmd) {
  if (flags) {
    w_put_literal(w, ",\"name\":");
    w_put_quoted_escaped_chars_len(w, (const char*)cmd.data, cmd.size);
  }
}

static void put_tty_name(writer* w, e_proc_flags flags, int64_t tty_nr) {
  if (flags == 0) {
    return;
  }
  w_put_literal(w, ",\"tty\":");
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
      w_put_quoted_escaped_chars_len(w, buf, (size_t)buflen);
    }
  } else {
    w_put_literal(w, "\"\"");
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

static void put_int64(writer* w, e_proc_flags flags, int64_t num, chars prefix) {
  if (!flags) {
    return;
  }
  put_chars(w, prefix);
  w_put_int64(w, num);
}

static void put_uint64(writer* w, e_proc_flags flags, uint64_t num, chars prefix) {
  if (!flags) {
    return;
  }
  put_chars(w, prefix);
  w_put_uint64(w, num);
}

static void put_timespec(writer* w, e_proc_flags flags, struct timespec ts, chars prefix) {
  if (!flags) {
    return;
  }
  put_chars(w, prefix);
  w_put_int64(w, ts.tv_sec);
  w_put_char(w, '.');

  {
    char  buf[24];
    char* start = buf + 12;
    int   len   = snprintf(start, sizeof(buf) - 8, "%" PRIu32, (uint32_t)ts.tv_nsec);
    for (; len < 9; len++) {
      *--start = '0'; /* prefix zeros to reach 9 digits */
    }
    /* remove redundant trailing zeroes */
    for (; len > 1 && start[len - 1] == '0'; --len) {
    }
    w_put_chars_len(w, start, len);
  }
}

/**
 * read one process from /proc and print it as NDJSON
 */
static void
print_process(DIR* dir, const struct dirent* entry, writer* w, e_proc_flags flags, uid_t my_uid) {
  char     buf[4096];
  chars    src, command;
  uint64_t mem_virt, mem_rss, min_fault, maj_fault, tick_per_s, start_time_ticks;
  uint64_t user_time_ticks, sys_time_ticks, iowait_time_ticks;
  int64_t  uid = -1, gid, pid, ppid, pgid, sid, tty_nr, priority, threads;
  char     state;
  uint8_t  ok;

  {
    int buf_written = snprintf(buf, sizeof(buf), "%s/stat", entry->d_name);

    ok = buf_written > 0 && (unsigned)buf_written < sizeof(buf) &&
         (src = read_file_at(
              dirfd(dir), buf, make_charspan((unsigned char*)buf, sizeof(buf)), &uid, &gid))
                 .size > 0;
  }

  if ((flags & e_proc_flag_other_users) == 0 && uid != my_uid) {
    return;
  }

  if (ok) {
    src = parse_int64(src, &pid);
    src = parse_command(src, &command);
    src = parse_char(src, &state);
    src = parse_int64(src, &ppid);
    src = parse_int64(src, &pgid);
    src = parse_int64(src, &sid);
    src = parse_int64(src, &tty_nr);

    if ((flags & e_proc_flag_without_tty) == 0 && tty_nr <= 0) {
      return;
    }

    src = parse_int64(src, NULL);              /* tty_pgrp        */
    src = parse_uint64(src, NULL);             /* flags           */
    src = parse_uint64(src, &min_fault);       /*                 */
    src = parse_int64(src, NULL);              /* child_min_fault */
    src = parse_uint64(src, &maj_fault);       /*                 */
    src = parse_uint64(src, NULL);             /* child_maj_fault */
    src = parse_uint64(src, &user_time_ticks); /*                 */
    src = parse_uint64(src, &sys_time_ticks);  /*                 */
    src = parse_int64(src, NULL);              /* child_user_time */
    src = parse_int64(src, NULL);              /* child_sys_time  */
    src = parse_int64(src, &priority);         /*                 */
    src = parse_int64(src, NULL);              /* nice            */
    src = parse_int64(src, &threads);          /*                 */
    src = parse_int64(src, NULL);              /* obsolete        */
    src = parse_uint64(src, &start_time_ticks);
    src = parse_uint64(src, &mem_virt);
    src = parse_uint64(src, &mem_rss);
  }

  if (!ok || src.size == 0) {
    fprintf(stderr, "proc: error parsing /proc/%s\n", entry->d_name);
    fflush(stderr);
    return;
  }

  /* skip fields 25...41 */
  {
    unsigned i;
    for (i = 25; i < 42 && (src = parse_uint64(src, NULL)).size != 0; i++) {
    }
    if (i == 42 && src.size != 0) {
      src = parse_uint64(src, &iowait_time_ticks);
    } else {
      iowait_time_ticks = 0;
    }
  }
  w_put_literal(w, "{\"<type>\":\"process-entry\"");

  put_int64(w, flags & e_proc_flag_pid, pid, make_chars_literal(",\"pid\":"));
  put_command(w, flags & e_proc_flag_name, command);
  put_tty_name(w, flags & e_proc_flag_tty, tty_nr);
  put_state(w, flags & e_proc_flag_state, state);

  put_int64(w, flags & e_proc_flag_ppid, ppid, make_chars_literal(",\"ppid\":"));
  put_int64(w, flags & e_proc_flag_pgid, pgid, make_chars_literal(",\"pgid\":"));
  put_int64(w, flags & e_proc_flag_sid, sid, make_chars_literal(",\"sid\":"));

  put_uint64(w, flags & e_proc_flag_mem_virt, mem_virt, make_chars_literal(",\"mem-virt\":"));
  put_uint64(w,
             flags & e_proc_flag_mem_rss,
             mem_rss * get_os_pagesize(), /* convert mem_rss from pages to bytes */
             make_chars_literal(",\"mem-rss\":"));

  tick_per_s = get_os_tick_per_s();

  put_timespec(w,
               flags & e_proc_flag_start_time, /* convert ticks -> struct timespec */
               timespec_add(get_boot_time_utc(), ticks_to_timespec(start_time_ticks, tick_per_s)),
               make_chars_literal(",\"start-time\":"));
  put_timespec(w,
               flags & e_proc_flag_user_time,
               ticks_to_timespec(user_time_ticks, tick_per_s),
               make_chars_literal(",\"user-time\":"));
  put_timespec(w,
               flags & e_proc_flag_sys_time,
               ticks_to_timespec(sys_time_ticks, tick_per_s),
               make_chars_literal(",\"sys-time\":"));
  put_timespec(w,
               flags & e_proc_flag_iowait_time,
               ticks_to_timespec(iowait_time_ticks, tick_per_s),
               make_chars_literal(",\"iowait-time\":"));

  put_int64(w, flags & e_proc_flag_priority, priority, make_chars_literal(",\"priority\":"));
  put_int64(w, flags & e_proc_flag_threads, threads, make_chars_literal(",\"threads\":"));
  put_int64(w, flags & e_proc_flag_min_fault, min_fault, make_chars_literal(",\"min-fault\":"));
  put_int64(w, flags & e_proc_flag_maj_fault, maj_fault, make_chars_literal(",\"maj-fault\":"));

  w_put_literal(w, "}\n");
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
    print_process(dir, entry, w, flags, uid);
    w_flush(w);
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

#endif /* 1 */

#endif /* SCHEME2K_C_PROC_IMPL_H */
