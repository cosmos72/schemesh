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
  int       fd;
} shm_ctx;

#if 0  /* not used yet */
/* align addr down to a multiple of power_of_2 */
static char* c_shm_align_down(char* addr, uintptr_t power_of_2) {
  return (char*)((uintptr_t)addr & ~(uintptr_t)(power_of_2 - 1));
}

/* align addr up to a multiple of power_of_2 */
static char* c_shm_align_up(char* addr, uintptr_t power_of_2) {
  return (char*)((uintptr_t)(addr + power_of_2 - 1) & ~(uintptr_t)(power_of_2 - 1));
}
#endif /* 0 */

static int c_shm_mmap_init(shm_ctx* ctx) {
  size_t pagesz = scheme2k_os_pagesize();
  size_t sz     = ((1 << 18) + pagesz - 1) & ~(pagesz - 1);
  void*  addr;
  int    fd = ctx->fd;
  if (ftruncate(fd, sz) >= 0 &&
      (addr = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) != MAP_FAILED) {

    shm_head* head = ctx->head = (shm_head*)addr;

    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    pthread_mutex_init(&head->mutex, &attr); /* documented to always return 0 */
    pthread_mutexattr_destroy(&attr);

    head->entry_n     = 0;
    head->free_pos    = sizeof(shm_head);
    head->capacity    = sz;
    ctx->mmapped_size = sz;

    return 0;
  }
  return c_errno();
}

static int c_shm_fd_open(void) {
  char   path[48];
  time_t t;
  int    err;
  int    fd = -1;
  errno     = 0;
  t         = time(NULL);
  if (t == (time_t)-1 && errno != 0) {
    goto fail;
  }
  err = snprintf(
      path, sizeof(path), "/schemesh_%08" PRIx64 "_08%" PRIx64, (uint64_t)getpid(), (uint64_t)t);
  if (err < 0) {
    goto fail;
  }
  if ((size_t)err >= sizeof(path)) {
    return -ENAMETOOLONG;
  }
#if defined(__linux__)
  /**
   * memfd_create() gives better privacy:
   * we only share fd with child processes that don't exec()
   *
   * memfd_create() declaration requires #define _GNU_SOURCE,
   * which also replaces POSIX strerror_r() with GNU variant, that we do not want.
   */
  int memfd_create(const char* name, unsigned int flags);
  fd = memfd_create(path + 1, 0);
#endif /* __linux__ */

#if !defined(__ANDROID__)
  if (fd < 0) {
    /* shm_open() is not available on Android */
    fd = shm_open(path, O_RDWR | O_CREAT | O_EXCL, (mode_t)0600);
    if (fd >= 0) {
      /*
       * cannot do much if shm_unlink() fails: exiting is worse,
       * as it's a denial of service and users will retry,
       * accumulating stale shm objects.
       */
      (void)shm_unlink(path);
    }
  }
#endif /* !__ANDROID__ */

  if (fd >= 0) {
    return fd;
  }
fail:
  return c_errno();
}

static ptr c_shm_open(int fd_to_use) {
  shm_ctx* ctx = NULL;
  int      fd;
  int      err;
  if (fd_to_use < 0) {
    err = -EBADF;
    goto fail;
  }
  if ((ctx = (shm_ctx*)malloc(sizeof(shm_ctx))) == NULL) {
    err = -ENOMEM;
    goto fail;
  }
  if ((fd = c_shm_fd_open()) < 0) {
    err = fd;
    goto fail;
  }
  if (fd != fd_to_use) {
    if (dup2(fd, fd_to_use) < 0) {
      fd_to_use = fd;
      goto cerrno_close_and_fail;
    }
    (void)close(fd);
  }
  if (fcntl(fd_to_use, F_SETFD, FD_CLOEXEC) < 0) {
    goto cerrno_close_and_fail;
  }
  ctx->head = NULL;
  ctx->fd   = fd_to_use;
  if (c_shm_mmap_init(ctx) == 0) {
#if 1
    /* fd_to_use is only needed to resize shared memory, which we don't do yet */
    (void)close(ctx->fd);
    ctx->fd = -1;
#endif
    return Sunsigned64((uintptr_t)(void*)ctx);
  }
cerrno_close_and_fail:
  err = c_errno();
  (void)close(fd_to_use);
fail:
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
  if (ctx->fd >= 0) {
    close(ctx->fd);
    ctx->fd = -1;
  }
  return c_errno();
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
  if (ctx == NULL || ctx->head == NULL || /* ctx->fd < 0 || */
      !Sbytevectorp(value) || (ilen = Sbytevector_length(value)) < 0) {
    return -EINVAL;
  }
  if ((size_t)ilen > (size_t)-1 - sizeof(uint64_t) + sizeof(size_t)) {
    return -ENOSPC;
  }
  head = ctx->head;
  if ((err = pthread_mutex_lock(&head->mutex)) != 0) {
    return -err;
  }

  len  = (size_t)ilen;
  slen = len + sizeof(uint64_t) + sizeof(size_t);
  pos  = head->free_pos;
  cap  = ctx->mmapped_size;

  if (slen > cap || pos > slen - cap) {
    (void)pthread_mutex_unlock(&head->mutex);
    return -ENOSPC; /* FIXME: implement shm resizing */
  }

  head->entry_n++;
  head->free_pos = pos + slen;

  dst = (char*)head + pos;
  memcpy(dst, Sbytevector_data(value), len);
  memcpy(dst + len, &len, sizeof(size_t));
  memcpy(dst + len + sizeof(size_t), &key, sizeof(uint64_t));

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
  (void)pthread_mutex_unlock(&head->mutex);
  return 0;
}

static int c_shm_lock(shm_ctx* ctx) {
  if (ctx == NULL || ctx->head == NULL) {
    return -EINVAL;
  }
  return -pthread_mutex_lock(&ctx->head->mutex);
}

static int c_shm_unlock(shm_ctx* ctx) {
  if (ctx == NULL || ctx->head == NULL) {
    return -EINVAL;
  }
  return -pthread_mutex_unlock(&ctx->head->mutex);
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
  size_t    entry_n;
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
  if ((entry_n = head->entry_n) == 0) {
    return Snil;
  }
  head->entry_n = entry_n - 1;

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
