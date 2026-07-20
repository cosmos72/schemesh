/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include "thread.h"

#if defined(C_THREAD_LOCAL) && defined(__ELF__)
# include <dlfcn.h> /* dlsym(), RTLD_NEXT */
# ifdef RTLD_NEXT
#  define C_THREAD_ELF
# endif
#endif

#ifdef C_THREAD_ELF

/* use ELF symbol interposition for wrapping pthread_mutex_*lock() functions */

typedef int (*c_mutex_func1)(pthread_mutex_t *m);
typedef int (*c_mutex_func2)(pthread_mutex_t *m, const struct timespec *p);

static c_mutex_func1 c_mutex_lock      = NULL;
static c_mutex_func2 c_mutex_timedlock = NULL;
static c_mutex_func1 c_mutex_trylock   = NULL;
static c_mutex_func1 c_mutex_unlock    = NULL;


int scheme2k_thread_init(void) {
  Sregister_symbol("c_mutex_locked_count", &c_mutex_locked_count);
  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_threads", &c_threads);

  /* initialize thread_local variable c_mutex_locked_n before installing pthread_mutex_*lock() wrappers,
     because thread_local initialization may acquire mutexes */
  if ((c_mutex_locked_n != 0) ||
      (c_mutex_lock      = (c_mutex_func1) dlsym(RTLD_NEXT, "pthread_mutex_lock")) == NULL ||
      (c_mutex_timedlock = (c_mutex_func2) dlsym(RTLD_NEXT, "pthread_mutex_timedlock")) == NULL ||
      (c_mutex_trylock   = (c_mutex_func1) dlsym(RTLD_NEXT, "pthread_mutex_trylock")) == NULL ||
      (c_mutex_unlock    = (c_mutex_func1) dlsym(RTLD_NEXT, "pthread_mutex_unlock")) == NULL) {
    
#ifdef EOPNOTSUPPORT
    return -EOPNOTSUPPORT;
#else
    return -EINVAL;
#endif
  }
  return 0;
}
  
int pthread_mutex_lock(pthread_mutex_t *m) {
  if (c_mutex_lock) {
    int err = c_mutex_lock(m);
    if (C_MUTEX_LOCKED_OK(err) && c_mutex_locked_n != (size_t)-1) {
      ++c_mutex_locked_n;
    }
    return err;
  }
  /* thread_local initialization may acquire mutexes, do NOT access c_mutex_locked_n */
  return 0;
}

int pthread_mutex_timedlock(pthread_mutex_t *m, const struct timespec *p) {
  if (c_mutex_timedlock) {
    int err = c_mutex_timedlock(m, p);
    if (C_MUTEX_LOCKED_OK(err) && c_mutex_locked_n != (size_t)-1) {
      ++c_mutex_locked_n;
    }
    return err;
  }
  /* thread_local initialization may acquire mutexes, do NOT access c_mutex_locked_n */
  return 0;
}

int pthread_mutex_trylock(pthread_mutex_t *m) {
  if (c_mutex_trylock) {
    int err = c_mutex_trylock(m);
    if (C_MUTEX_LOCKED_OK(err) && c_mutex_locked_n != (size_t)-1) {
      ++c_mutex_locked_n;
    }
    return err;
  }
  /* thread_local initialization may acquire mutexes, do NOT access c_mutex_locked_n */
  return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *m) {
  if (c_mutex_unlock) {
    int err = c_mutex_unlock(m);
    if (err == 0 && c_mutex_locked_n != 0) {
      --c_mutex_locked_n;
    }
    return err;
  }
  /* thread_local initialization may acquire mutexes, do NOT access c_mutex_locked_n */
  return 0;
}

#else /* !C_THREAD_ELF */

int scheme2k_thread_init(void) {
  Sregister_symbol("c_mutex_locked_count", &c_mutex_locked_count);
  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_threads", &c_threads);
  return 0;
}

#endif /* !C_THREAD_ELF */
