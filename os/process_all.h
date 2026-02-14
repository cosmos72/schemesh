/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <errno.h>
#include <fcntl.h>  /* open() */
#include <stddef.h> /* size_t */
#include <stdint.h> /* int64_t, uint64_t */
#include <stdio.h>
#include <string.h>   /* memcpy() */
#include <sys/stat.h> /* fstat() */
#include <time.h>     /* clock_gettime(), struct timespec */
#include <unistd.h>   /* close(), read(), sysconf() */

#include "../containers/containers.h"

/* return dst + read_len, or NULL on error */
static unsigned char* read_file_at(
    int dir_fd, const char path[], unsigned char dst[], size_t dstlen, int64_t* uid, int64_t* gid);
static void   skip_ws(const unsigned char** src);
static size_t parse_char(const unsigned char** src, char ret[1]);
static size_t parse_int64(const unsigned char** src, void* dst, size_t i);
static size_t parse_uint64(const unsigned char** src, void* dst, size_t i);

static uint64_t get_os_tick_per_s(void);
static uint64_t get_os_pagesize(void);

typedef char sizeof_int64_is_8[sizeof(int64_t) == 8 ? 1 : -1];
typedef char sizeof_uint64_is_8[sizeof(uint64_t) == 8 ? 1 : -1];
typedef char sizeof_double_is_8[sizeof(double) == 8 ? 1 : -1];

static int64_t max_int64(int64_t left, int64_t right) {
  return left > right ? left : right;
}

static int64_t min_int64(int64_t left, int64_t right) {
  return left < right ? left : right;
}

static void set_int64(void* dst, size_t i, int64_t val) {
  if (dst) {
    memcpy((uint8_t*)dst + i * 8, &val, 8);
  }
}

static void set_uint64(void* dst, size_t i, uint64_t val) {
  if (dst) {
    memcpy((uint8_t*)dst + i * 8, &val, 8);
  }
}

static void set_timespec(void* dst, size_t i, struct timespec t) {
  set_uint64(dst, i, (uint32_t)t.tv_nsec);
  set_int64(dst, i + 1, t.tv_sec);
}

#if 0  /* unused */
static int64_t get_int64(const void* src, size_t i) {
  int64_t val = 0;
  if (src) {
    memcpy(&val, (const uint8_t*)src + i * 8, 8);
  }
  return val;
}
#endif /* 0 */

static uint64_t get_uint64(const void* src, size_t i) {
  uint64_t val = 0;
  if (src) {
    memcpy(&val, (const uint8_t*)src + i * 8, 8);
  }
  return val;
}

static void uint64_multiply(void* src, size_t i, uint64_t scale) {
  set_uint64(src, i, get_uint64(src, i) * scale);
}

static struct timespec ticks_to_timespec(uint64_t ticks, uint64_t tick_per_s) {
  struct timespec ret;
  uint64_t        remainder = ticks % tick_per_s;

  ret.tv_sec = ticks / tick_per_s;
  /* requires tick_per_s < MAX_UINT64 / 1'000'000'000 */
  ret.tv_nsec = (remainder * 1000000000 + tick_per_s / 2) / tick_per_s;
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

static struct timespec timespec_add(struct timespec left, struct timespec right) {
  struct timespec ret;

  int carry = (ret.tv_nsec = left.tv_nsec + right.tv_nsec) >= 1000000000;
  if (carry) {
    ret.tv_nsec -= 1000000000;
  }
  ret.tv_sec = left.tv_sec + right.tv_sec + carry;
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

static int c_errno_set(int errno_value) {
  return -(errno = errno_value);
}

static int c_errno(void) {
  return -errno;
}

enum {
  e_pid          = 0,
  e_uid          = 1,
  e_gid          = 2,
  e_ppid         = 3,
  e_pgrp         = 4,
  e_sid          = 5,
  e_flags        = 6,
  e_mem_resident = 7,  /* bytes, uint64 */
  e_mem_virtual  = 8,  /* bytes, uint64 */
  e_start_time   = 9,  /* utc,      timespec, uint64 tv_nsec then int64 tv_sec */
  e_user_time    = 11, /* duration, timespec */
  e_sys_time     = 13, /* duration, timespec */
  e_iowait_time  = 15, /* duration, timespec */
  e_priority     = 17, /* int64 */
  e_nice         = 18, /* int64 */
  e_rt_priority  = 19,
  e_rt_policy    = 20,
  e_num_threads  = 21,
  e_min_fault    = 22,
  e_maj_fault    = 23,
  e_state        = 24,
  e_byte_n       = e_state * 8 + 1,
};

#ifdef __linux__
#include "process_linux.h"
#else
#include "process_unsupported.h"
#endif

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

static void skip_ws(const unsigned char** src) {
  unsigned char ch;
  while ((ch = **src) > 0 && ch <= ' ') {
    ++*src;
  }
}

/**
 * skip whitespace, get next char and store it in *ret
 * @return 1 if char was read, or 0 on EOF.
 * if char could not be read, store '\0' in *ret.
 */
static size_t parse_char(const unsigned char** src, char ret[1]) {
  unsigned char ch;
  skip_ws(src);
  if ((ch = **src) != 0) {
    ++*src;
  }
  if (ret) {
    *ret = ch;
  }
  return ch != 0;
}

/**
 * skip whitespace, parse decimal digits, convert them to an uint64_t
 * and store it in ((uint64_t*)dst)[i].
 * @return number of parsed decimal digits.
 * if no digits could be parsed, store 0 in ((uint64_t*)dst)[i].
 */
static size_t parse_uint64(const unsigned char** src, void* dst, size_t i) {
  uint64_t      val;
  size_t        digits;
  unsigned char ch;
  skip_ws(src);

  val    = 0;
  digits = 0;
  while ((ch = **src) >= '0' && ch <= '9') {
    ++*src;
    ++digits;
    /* unsigned overflow always wraps, it's never undefined behavior */
    val = val * 10 + (unsigned)(ch - '0');
  }
  set_uint64(dst, i, val);
  return digits;
}

/**
 * skip whitespace, parse optional sign and decimal digits, and convert them to an int64_t,
 * and store it in *ret.
 * @return number of parsed decimal digits.
 * if no digits could be parsed, store 0 in *ret.
 */
static size_t parse_int64(const unsigned char** src, void* dst, size_t i) {
  uint64_t      val      = 0;
  size_t        digits   = 0;
  int           negative = 0;
  unsigned char ch;
  skip_ws(src);
  if ((ch = **src) == '+' || ch == '-') {
    ++*src;
    negative = (ch == '-');
  }
  while ((ch = **src) >= '0' && ch <= '9') {
    ++*src;
    ++digits;
    /* unsigned overflow always wraps, it's never undefined behavior */
    val = val * 10 + (unsigned)(ch - '0');
  }
  set_int64(dst, i, negative ? -(int64_t)val : (int64_t)val);
  return digits;
}

#if 0
/**
 * skip whitespace, copy up to dstlen-1 non-whitespace characters to dst.
 * always add '\0' terminator to dst.
 * @return number of copied characters.
 */
static size_t parse_string(const unsigned char** src, char dst[], size_t dstlen) {
  size_t        len = 0;
  unsigned char ch;
  skip_ws(src);
  while (len + 1 < dstlen && (ch = **src) > ' ') {
    ++*src;
    if (dst) {
      *dst++ = (char)ch;
    }
    len++;
  }
  if (dst) {
    *dst = '\0';
  }
  return len;
}
#endif /* 0 */

static uint64_t os_pagesize   = 0; /* OS page size, in bytes */
static uint64_t os_tick_per_s = 0; /* OS clock ticks per second */

static uint64_t get_os_tick_per_s(void) {
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

static uint64_t get_os_pagesize(void) {
  uint64_t n = os_pagesize;
  if (n == 0) {
#ifdef _SC_PAGESIZE
    long val = sysconf(_SC_PAGESIZE);
    if (val > 0) {
      n = (uint64_t)val;
    } else
#endif

    /*else */ {
#if defined(PAGESIZE) && PAGESIZE > 0
      n = PAGESIZE;
#else
      n = 4096; /* guess */
#endif
    }
    os_pagesize = n; /* cache for future calls */
  }
  return n;
}