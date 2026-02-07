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
#include <unistd.h>   /* close(), read(), sysconf() */

#include "../containers/containers.h"

/* return dst + read_len, or NULL on error */
static unsigned char*
read_file(const char path[], unsigned char dst[], size_t dstlen, int64_t* uid, int64_t* gid);
static void   skip_ws(const unsigned char** src);
static size_t parse_char(const unsigned char** src, char ret[1]);
static size_t parse_int64(const unsigned char** src, void* dst, size_t i);
static size_t parse_uint64(const unsigned char** src, void* dst, size_t i);

typedef char sizeof_int64_is_8[sizeof(int64_t) == 8 ? 1 : -1];
typedef char sizeof_uint64_is_8[sizeof(uint64_t) == 8 ? 1 : -1];
typedef char sizeof_double_is_8[sizeof(double) == 8 ? 1 : -1];

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

static void set_double(void* dst, size_t i, double val) {
  if (dst) {
    memcpy((uint8_t*)dst + i * 8, &val, 8);
  }
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
  e_mem_resident = 7,  /* in bytes */
  e_mem_virtual  = 8,  /* in bytes */
  e_start_time   = 9,  /* in clock ticks since boot */
  e_user_time    = 10, /* in clock ticks */
  e_sys_time     = 11, /* in clock ticks */
  e_priority     = 12, /* signed */
  e_nice         = 13, /* signed */
  e_num_threads  = 14,
  e_min_fault    = 15,
  e_maj_fault    = 16,
  e_count        = 17,
};

#ifdef __linux__
#include "process_linux.h"
#else
#include "process_unsupported.h"
#endif

static void stat_fd(int fd, int64_t* uid, int64_t* gid) {
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

static unsigned char*
read_file(const char path[], unsigned char dst[], size_t dstlen, int64_t* uid, int64_t* gid) {
  ssize_t n;
  size_t  end;
  int     fd = open(path, O_RDONLY);
  if (fd < 0) {
    return NULL;
  }
  stat_fd(fd, uid, gid);
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
 * skip whitespace, parse decimal digits, convert them to an uint64_t and store it in *ret.
 * @return number of parsed decimal digits.
 * if no digits could be parsed, store 0 in *ret.
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
