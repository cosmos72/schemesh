/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

typedef struct {
  size_t len;
  char  *data;
} c_string;

typedef struct {
  pid_t pid;
  uid_t uid;
  gid_t gid;
  pid_t ppid;
  pid_t pgrp;
  pid_t sid;        /* -1 if unknown */
  uint64_t mem_rss;
  uint64_t mem_virt;
  struct timespec start_time;
  struct timespec user_time;
  struct timespec sys_time;
  struct timespec iowait_time;
  int32_t  priority;
  int32_t  num_thread; /* -1 if unknown */
  uint64_t min_fault;
  uint64_t maj_fault;
  c_string name;
  dev_t    tty;
  char     state;
} c_process_info;

typedef struct {
  size_t i;
  size_t n;
  c_process_info data[1];
} c_process_infos;

static c_string c_string_new(const char *chars) {
  c_string ret;
  size_t len = strlen(chars);
  if ((ret.data = malloc(len + 1)) != NULL) {
    memcpy(ret.data, chars, len + 1);
    ret.len = len;
  } else {
    ret.len = 0; /* out of memory */
  }
  return ret;
}

static void c_string_del(c_string s) {
  if (s.data) {
    free(s.data);
  }
}

static c_process_infos *c_process_infos_new(size_t n) {
  c_process_infos *ret;
  if (n == 0) {
    return NULL;
  }
  ret = calloc(1, sizeof(c_process_infos) + (n - 1) * sizeof(c_process_info));
  if (ret) {
    ret->n = n;
  }
  return ret;
}

static void c_process_infos_del(c_process_infos *cs) {
  size_t i, n;
  if (cs == NULL) {
    return;
  }
  for (i = 0, n = cs->n; i < n; i++) {
    c_string_del(cs->data[i].name);
  }
  free(cs);
}


static struct timespec ns_to_timespec(uint64_t ns) {
  struct timespec ts;
  ts.tv_sec = ns / 1000000000;
  ts.tv_nsec = ns % 1000000000;
  return ts;
}
    
static struct timespec timeval_to_timespec(const struct timeval tv) {
  struct timespec ts;
  ts.tv_sec = tv.tv_sec;
  ts.tv_nsec = tv.tv_usec * 1000;
  return ts;
}
