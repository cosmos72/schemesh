/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_POSIX_THREAD_H
#define SCHEME2K_POSIX_THREAD_H

#include <errno.h>
#include <pthread.h>

#include "../chezscheme.h"

#ifndef C_THREAD_LOCAL
#if defined(__GNUC__)
#define C_THREAD_LOCAL __thread
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#include <threads.h>
#define C_THREAD_LOCAL thread_local
#endif /* __STDC_VERSION__ */
#endif /* !C_THREAD_LOCAL */

#ifdef EOWNERDEAD
#define C_MUTEX_LOCKED_OK(err) ((err) == 0 || (err) == EOWNERDEAD)
#else
#define C_MUTEX_LOCKED_OK(err) ((err) == 0)
#endif /* EOWNERDEAD */

#ifdef C_THREAD_LOCAL
static C_THREAD_LOCAL size_t c_mutex_locked_n = 0;

static ptr c_mutex_locked_count(void) {
  ptr n = Sfixnum(c_mutex_locked_n);
  if (Sfixnum_value(n) < 0) {
    /* overflow, return some large positive fixnum */
    n = Sfixnum(0x1fffffffu); /* should always be a fixnum */
  }
  return n;
}
#else /* !C_THREAD_LOCAL */

static ptr c_mutex_locked_count(void) {
  return Sfixnum(0);
}

#endif /* C_THREAD_LOCAL */

static uptr c_thread_count(void) {
#ifdef FEATURE_PTHREADS
  extern volatile uptr S_nthreads;
  return S_nthreads;
#else
  return 1;
#endif
}

/** must be called with locked $tc-mutex */
static ptr c_threads(void) {
  extern volatile ptr S_threads;
  return S_threads;
}

#endif /* SCHEME2K_POSIX_THREAD_H */
