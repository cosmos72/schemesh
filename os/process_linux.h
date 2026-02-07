/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <dirent.h> /* opendir() */
#include <inttypes.h>
#include <sys/types.h>/* opendir() */

/**
 * skip whitespace, copy command name in parentheses (...) to dst.
 * Command name may contain any character, including spaces and ')'
 * Solution: scan src from end and skip all numbers and status, stop at ')'
 * Always add '\0' terminator to dst.
 * @return number of copied characters.
 */
static size_t parse_linux_command(const unsigned char** src,
                                  const unsigned char*  src_end,
                                  char                  dst[],
                                  size_t                dstlen) {
  size_t len;
  skip_ws(src);
  /* skip initial '(' */
  if (**src == '(') {
    ++*src;
  }
  while (src_end > *src) {
    --src_end;
    /* skip final ')' and exit loop */
    if (*src_end == ')') {
      break;
    }
  }
  len = src_end - *src;
  if (len >= dstlen) {
    len = dstlen - 1;
  }
  if (dst) {
    memcpy(dst, *src, len);
    dst[len] = '\0';
  }
  *src = src_end + (*src_end == ')');
  return len;
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

static ptr make_tty_name(int64_t tty_nr) {
  if (tty_nr > 0) {
    char     buf[16];
    uint64_t hi     = (uint64_t)tty_nr >> 8;
    unsigned lo     = tty_nr & 0xFF;
    int      buflen = 0;
    if (hi == 136) {
      buflen = snprintf(buf, sizeof(buf), "/dev/pts/%u", lo);
    } else if (hi == 4) {
      buflen = snprintf(buf, sizeof(buf), "/dev/tty%u", lo);
    } else {
      return Svoid;
    }
    /* Sstring_of_length() works as expected only for ASCII chars */
    /*   i.e. 0 < buf[i] < 128 for every i                        */
    return Sstring_of_length(buf, buflen);
  }
  return Sfalse;
}

/** convert Scheme exact integer to C DIR* */
static DIR* to_dir(ptr dir_s) {
  uintptr_t dir_u;
  if ((Sfixnump(dir_s) || Sbignump(dir_s)) && (dir_u = Sunsigned_value(dir_s)) != 0) {
    return (DIR*)(void*)dir_u;
  }
  return NULL;
}

/**
 * on success, return scheme unsigned number containing C DIR*
 * on error, return c_errno() < 0
 */
static ptr c_process_open(void) {
  DIR* dir = opendir("/proc");
  if (!dir) {
    return Sinteger(c_errno()); /* < 0 */
  }
  return Sunsigned((uintptr_t)(void*)dir);
}

static void c_process_close(ptr dir_s) {
  DIR* dir = to_dir(dir_s);
  if (dir) {
    closedir(dir);
  }
}

/**
 * read dir until we find a directory whose name is a decimal number
 * and return corresponding dirent*
 *   on end-of-dir, set errno = 0 and return NULL
 *   on error, set errno > 0 and return NULL
 */
static struct dirent* c_process_get_next_pid(DIR* dir) {
  struct dirent* entry;
  if (!dir) {
    c_errno_set(EINVAL);
    return NULL;
  }
  c_errno_set(0);
  do {
    entry = readdir(dir);
  } while (entry && !string_is_decimal_number(entry->d_name));
  return entry;
}

/**
 * skip one process pid in /proc.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_process_skip(ptr dir_s) {
  struct dirent* entry = c_process_get_next_pid(to_dir(dir_s));
  if (!entry) {
    return c_errno(); /* 0 if end of dir, otherwise error */
  }
  return 1; /* ok, skipped one pid */
}

/*
 * read one process from /proc.
 *   on success, return list (command_string, status_char, tty_string) and fill bvec.
 *   on end-of-dir, return 0
 *   on I/O error, return c_errno() < 0
 *   on parsing errors, return #f
 */
static ptr c_process_get(ptr dir_s, ptr bvec) {
  char                 buf[4096];
  char                 comm[256];
  const unsigned char* src;
  const unsigned char* src_end;
  uint8_t*             vec;
  DIR*                 dir;
  struct dirent*       entry;
  int64_t              uid, gid, tty_nr;
  int                  buf_written;
  char                 state;
  uint8_t              ok;

  if (!Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  vec = Sbytevector_data(bvec);

  // ---- Read next /proc/<pid>/stat ----
  dir   = to_dir(dir_s);
  entry = c_process_get_next_pid(dir);
  if (!entry) {
    return Sinteger(c_errno()); /* 0 if end of dir, otherwise error */
  }
  memset(vec, '\0', e_byte_n);

  buf_written = snprintf(buf, sizeof(buf), "%s/stat", entry->d_name);

  src_end = src = (const unsigned char*)buf;

  ok = buf_written > 0 && (unsigned)buf_written < sizeof(buf) &&
       (src_end = read_file_at(dirfd(dir), buf, (unsigned char*)buf, sizeof(buf), &uid, &gid)) &&
       parse_int64(&src, vec, e_pid) && parse_linux_command(&src, src_end, comm, sizeof(comm)) &&
       parse_char(&src, &state) && parse_int64(&src, vec, e_ppid) &&
       parse_int64(&src, vec, e_pgrp) && parse_int64(&src, vec, e_sid) &&
       parse_int64(&src, &tty_nr, 0) && parse_int64(&src, NULL, 0 /*tty_pgrp*/) &&
       parse_uint64(&src, vec, e_flags) && parse_uint64(&src, vec, e_min_fault) &&
       parse_uint64(&src, NULL, 0 /*child_min_fault*/) && parse_uint64(&src, vec, e_maj_fault) &&
       parse_uint64(&src, NULL, 0 /*child_maj_fault*/) && parse_uint64(&src, vec, e_user_time) &&
       parse_uint64(&src, vec, e_sys_time) && parse_int64(&src, NULL, 0 /*child_user_time*/) &&
       parse_int64(&src, NULL, 0 /*child_sys_time*/) && parse_int64(&src, vec, e_priority) &&
       parse_int64(&src, vec, e_nice) && parse_int64(&src, vec, e_num_threads) &&
       parse_int64(&src, NULL, 0 /*obsolete*/) && parse_uint64(&src, vec, e_start_time) &&
       parse_uint64(&src, vec, e_mem_virtual) && parse_uint64(&src, vec, e_mem_resident);

  if (ok) {
    unsigned i;
    /* skip fields 25...39 */
    for (i = 25; i < 40 && parse_uint64(&src, NULL, 0); i++) {
    }
    if (i == 40) {
      ok = parse_uint64(&src, vec, e_rt_priority) && parse_uint64(&src, vec, e_rt_policy) &&
           parse_uint64(&src, vec, e_iowait_time);
    }

    set_int64(vec, e_uid, uid);
    set_int64(vec, e_gid, gid);

    set_uint64(vec, e_tick_per_s, get_os_tick_per_s());

    /* convert mem_resident from pages to bytes */
    uint64_multiply(vec, e_mem_resident, get_os_pagesize());

    vec[e_state * 8] = (uint8_t)state;

    return Scons(scheme2k_Sstring_utf8b(comm, (size_t)-1), make_tty_name(tty_nr));
  }
#if 0
  fprintf(stderr, "error parsing %s\n", entry->d_name);
  fflush(stderr);
#endif /* 0 */
  return Sfalse;
}
