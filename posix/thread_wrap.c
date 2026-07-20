/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include "thread.h"

#ifdef C_THREAD_LOCAL
#define C_THREAD_WRAP
#endif

#ifdef C_THREAD_WRAP

static int c_thread_initialized = 0;

int scheme2k_thread_init(void) {
  Sregister_symbol("c_mutex_locked_count", &c_mutex_locked_count);
  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_threads", &c_threads);

  /* initialize thread_local variable c_mutex_locked_n before activating pthread_mutex_*lock() wrappers,
     because thread_local initialization may acquire mutexes */
  c_mutex_locked_n = 0;
  c_thread_initialized = 1;
  
  return 0;
}

extern int __real_pthread_mutex_lock(pthread_mutex_t *m);
extern int __real_pthread_mutex_timedlock(pthread_mutex_t *m, const struct timespec *p);
extern int __real_pthread_mutex_trylock(pthread_mutex_t *m);
extern int __real_pthread_mutex_unlock(pthread_mutex_t *m);

int __wrap_pthread_mutex_lock(pthread_mutex_t *m) {
  int err = __real_pthread_mutex_lock(m);
  /* thread_local initialization may acquire mutexes, */
  /* access c_mutex_locked_n only if scheme2k_thread_init() was called */
  if (C_MUTEX_LOCKED_OK(err) && c_thread_initialized && c_mutex_locked_n != (size_t)-1) {
    ++c_mutex_locked_n;
  }
  return err;
}
    
int __wrap_pthread_mutex_timedlock(pthread_mutex_t *m, const struct timespec *p) {
  int err = __real_pthread_mutex_timedlock(m, p);
  /* thread_local initialization may acquire mutexes, */
  /* access c_mutex_locked_n only if scheme2k_thread_init() was called */
  if (C_MUTEX_LOCKED_OK(err) && c_thread_initialized && c_mutex_locked_n != (size_t)-1) {
    ++c_mutex_locked_n;
  }
  return err;
}

int __wrap_pthread_mutex_trylock(pthread_mutex_t *m) {
  int err = __real_pthread_mutex_trylock(m);
  /* thread_local initialization may acquire mutexes, */
  /* access c_mutex_locked_n only if scheme2k_thread_init() was called */
  if (C_MUTEX_LOCKED_OK(err) && c_thread_initialized && c_mutex_locked_n != (size_t)-1) {
    ++c_mutex_locked_n;
  }
  return err;
}

int __wrap_pthread_mutex_unlock(pthread_mutex_t *m) {
  int err = __real_pthread_mutex_unlock(m);
  /* thread_local initialization may acquire mutexes, */
  /* access c_mutex_locked_n only if scheme2k_thread_init() was called */
  if (err == 0 && c_thread_initialized && c_mutex_locked_n != 0) {
    --c_mutex_locked_n;
  }
  return err;
}

#else /* !C_THREAD_WRAP */

int scheme2k_thread_init(void) {
  Sregister_symbol("c_mutex_locked_count", &c_mutex_locked_count);
  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_threads", &c_threads);
  return 0;
}

#endif /* !C_THREAD_WRAP */
