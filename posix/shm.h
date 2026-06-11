/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/******************************************************************************/
/*                                                                            */
/*                     shared-memory related functions                        */
/*                                                                            */
/******************************************************************************/

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/shm.h should only be #included by posix/posix.c"
#endif

/* for exposition only, shm_entry instances are unaligned */
typedef struct {
  /* data: len bytes, followed by: */
  size_t   len;
  uint64_t key; /* usually a pid */
} shm_entry;

typedef struct {
  pthread_mutex_t mutex;
  size_t          entry_n;
  size_t          free_pos;
  size_t          capacity; /* redundant? */
  /* followed by entry_n elements, each a shm_entry */
} shm_head;

typedef struct {
  shm_head* head;
  size_t    mmapped_size;
} shm_ctx;

static int c_shm_mmap_init(shm_ctx* ctx, size_t length) {
  void*  addr;
  size_t pagesz = scheme2k_os_pagesize();
  size_t sz     = (length + pagesz - 1) & ~(pagesz - 1);
  int    err;

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
  if ((addr = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0)) !=
      MAP_FAILED) {
    pthread_mutexattr_t attr;

    shm_head* head = ctx->head = (shm_head*)addr;
    ctx->mmapped_size          = sz;
    if ((err = pthread_mutexattr_init(&attr)) == 0) {
      if ((err = pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED)) == 0
#ifdef PTHREAD_MUTEX_ROBUST
          && (err = pthread_mutexattr_setrobust(&attr, PTHREAD_MUTEX_ROBUST)) == 0
#endif
      ) {
        head->entry_n  = 0;
        head->free_pos = sizeof(shm_head);
        head->capacity = sz;

        err = pthread_mutex_init(&head->mutex, &attr);
      }
      (void)pthread_mutexattr_destroy(&attr);
    }
  } else {
    err = c_errno();
  }
  return -err;
}

static ptr c_shm_open(size_t length) {
  shm_ctx* ctx = NULL;
  int      err;
  if ((ctx = (shm_ctx*)calloc(1, sizeof(shm_ctx))) != NULL) {
    if ((err = c_shm_mmap_init(ctx, length)) == 0) {
      return Sunsigned64((uintptr_t)(void*)ctx);
    }
  } else {
    err = -ENOMEM;
  }
  if (ctx) {
    free(ctx);
  }
  return Sinteger(err);
}

static int c_shm_close(shm_ctx* ctx) {
  if (ctx == NULL) {
    return -EINVAL;
  }
  errno = 0;
  if (ctx->head) {
    munmap(ctx->head, ctx->mmapped_size);
  }
  return c_errno();
}

static int c_shm_lock(shm_ctx* ctx) {
  int err;
  if (ctx == NULL || ctx->head == NULL) {
    return -EINVAL;
  }
  err = pthread_mutex_lock(&ctx->head->mutex);
#ifdef PTHREAD_MUTEX_ROBUST
  if (err == EOWNERDEAD) {
    err = pthread_mutex_consistent(&ctx->head->mutex);
    if (err != 0) {
      (void)pthread_mutex_unlock(&ctx->head->mutex);
    }
  }
#endif
  return -err;
}

static int c_shm_unlock(shm_ctx* ctx) {
  if (ctx == NULL || ctx->head == NULL) {
    return -EINVAL;
  }
  return -pthread_mutex_unlock(&ctx->head->mutex);
}

/*
 * insert key, value into shared memory. value must be a bytevector
 * return 0 on success, or < 0 on error
 */
static int c_shm_insert(shm_ctx* ctx, uint64_t key, ptr value) {
  shm_head* head;
  char*     dst;
  size_t    cap, pos, len, slen;
  iptr      ilen;
  int       err;
  if (!Sbytevectorp(value) || (ilen = Sbytevector_length(value)) < 0) {
    return -EINVAL;
  }
  if ((size_t)ilen > (size_t)-1 - sizeof(uint64_t) + sizeof(size_t)) {
    return -ENOSPC;
  }
  if ((err = c_shm_lock(ctx)) != 0) {
    return -err;
  }
  head = ctx->head;
  len  = (size_t)ilen;
  slen = len + sizeof(uint64_t) + sizeof(size_t);
  pos  = head->free_pos;
  cap  = ctx->mmapped_size;

  if (slen > cap || pos > slen - cap) {
    (void)pthread_mutex_unlock(&head->mutex);
    return -ENOSPC; /* FIXME: implement shm resizing */
  }

  dst = (char*)head + pos;
  memcpy(dst, Sbytevector_data(value), len);
  memcpy(dst + len, &len, sizeof(size_t));
  memcpy(dst + len + sizeof(size_t), &key, sizeof(uint64_t));

  head->entry_n++;
  head->free_pos = pos + slen;

#if 0 /* unneeded */
  {
    const size_t pagesz = scheme2k_os_pagesize();

    /* round dst .. dst+slen to pages */
    char* start = c_shm_align_down(dst, pagesz);
    char* end   = c_shm_align_up(dst + slen, pagesz);

    if (msync(start, (size_t)(end - start), MS_SYNC) != 0) {
      return c_errno();
    }
  }
#endif
  (void)c_shm_unlock(ctx);
  return 0;
}

/*
 * remove last key, value from shared memory and return a pair (key . value)
 * or '() if shared memory is empty, or integer < 0 on error
 */
static ptr c_shm_locked_delete(shm_ctx* ctx) {
  uint64_t  key = 0;
  shm_head* head;
  char*     addr;
  ptr       value;
  size_t    free_pos;
  size_t    len;
  int       err;

  if (ctx == NULL || (head = ctx->head) == NULL) {
    return Sinteger(-EINVAL);
  }
  if ((err = pthread_mutex_trylock(&head->mutex)) == 0) {
    (void)pthread_mutex_unlock(&head->mutex);
    return Sinteger(-err);
  }
  if (head->entry_n == 0) {
    return Snil;
  }
  value    = Sfalse;
  free_pos = head->free_pos;
  if (free_pos <= ctx->mmapped_size &&
      free_pos >= sizeof(shm_head) + sizeof(uint64_t) + sizeof(size_t)) {

    addr = (char*)head + free_pos;

    memcpy(&key, addr -= sizeof(uint64_t), sizeof(uint64_t));
    memcpy(&len, addr -= sizeof(size_t), sizeof(size_t));

    if (len <= (size_t)(addr - (char*)head - sizeof(shm_head))) {
      iptr ilen = (iptr)len;
      if (ilen >= 0 && (size_t)ilen == len) {
        value = Smake_bytevector(ilen, 0);
        memcpy(Sbytevector_data(value), addr -= len, len);
      }
    }
    head->free_pos = addr - (char*)head;
  }
  head->entry_n--;

#if 0  /* unneeded */
  {
    const size_t pagesz = scheme2k_os_pagesize();

    /* round dst .. dst+slen to pages */
    char* start = c_shm_align_down(head, pagesz);
    char* end   = c_shm_align_up(head + 1, pagesz);

    if (msync(start, (size_t)(end - start), MS_SYNC) != 0) {
      return Sinteger(c_errno());
    }
  }
#endif /* 0 */
  return Scons(Sunsigned64(key), value);
}
