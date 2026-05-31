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
  size_t          capacity;
  /* followed by entry_n elements, each a shm_entry */
} shm_head;

static shm_head* c_shm_head = NULL;
static int       c_shm_fd   = -1;

/* align addr down to a multiple of power_of_2 */
static char* c_shm_align_down(char* addr, uintptr_t power_of_2) {
  return (char*)((uintptr_t)addr & ~(uintptr_t)(power_of_2 - 1));
}

/* align addr up to a multiple of power_of_2 */
static char* c_shm_align_up(char* addr, uintptr_t power_of_2) {
  return (char*)((uintptr_t)(addr + power_of_2 - 1) & ~(uintptr_t)(power_of_2 - 1));
}

static int c_shm_mmap_init(void) {
  size_t pagesz = scheme2k_os_pagesize();
  size_t sz     = ((1 << 18) + pagesz - 1) & ~(pagesz - 1);
  void*  addr;
  int    fd = c_shm_fd;
  if (ftruncate(fd, sz) >= 0 &&
      (addr = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) != MAP_FAILED) {

    shm_head* head = c_shm_head = (shm_head*)addr;

    pthread_mutexattr_t attr;
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    pthread_mutex_init(&head->mutex, &attr); /* documented to always return 0 */
    pthread_mutexattr_destroy(&attr);

    head->entry_n  = 0;
    head->free_pos = sizeof(shm_head);
    head->capacity = sz;

    return 0;
  }
  return -1;
}

static int c_shm_init(int fd_to_use) {
  char   path[32]; /* 28 would suffice too */
  time_t t;
  int    err;
  int    fd = -1;
  if (fd_to_use < 0) {
    errno = EBADF;
    goto fail;
  }
  errno = 0;
  t     = time(NULL);
  if (t == (time_t)-1 && errno != 0) {
    goto fail;
  }
  err = snprintf(
      path, sizeof(path), "/schemesh_%" PRIx64 "_%" PRIx64, (uint64_t)getpid(), (uint64_t)t);
  if (err < 0) {
    goto fail;
  }
  if ((size_t)err >= sizeof(path)) {
    errno = ENAMETOOLONG;
    goto fail;
  }
  fd = shm_open(path, O_RDWR | O_CREAT | O_EXCL, (mode_t)0600);
  if (fd < 0) {
    goto fail;
  }
  /*
   * cannot do much if shm_unlink() fails: exiting is worse,
   * as it's a denial of service and users will retry,
   * accumulating shm objects.
   */
  (void)shm_unlink(path);
  if (fd != fd_to_use) {
    err = dup2(fd, fd_to_use);
    (void)close(fd);
    if (err < 0) {
      goto fail;
    }
    err = fcntl(fd_to_use, F_SETFD, FD_CLOEXEC);
    if (err < 0) {
      goto close_and_fail;
    }
  }
  c_shm_fd = fd_to_use;
  if (c_shm_mmap_init() == 0) {
    return 0;
  }
close_and_fail:
  close(fd_to_use);
  c_shm_fd = -1;
fail:
  return c_errno();
}

/*
 * insert key, value into shared memory. value must be a bytevector
 * return 0 on success, or < 0 on error
 */
static int c_shm_insert(uint64_t key, ptr value) {

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
  if ((head = c_shm_head) == NULL) {
    return -EBADF;
  }
  if ((err = pthread_mutex_lock(&head->mutex)) != 0) {
    return -err;
  }

  len  = (size_t)ilen;
  slen = len + sizeof(uint64_t) + sizeof(size_t);
  pos  = head->free_pos;
  cap  = head->capacity;

  if (slen > cap || pos > slen - cap) {
    (void)pthread_mutex_unlock(&head->mutex);
    return -ENOSPC; /* FIXME: implement shm resizing */
  }

  head->entry_n++;
  head->free_pos = pos + slen;

  if ((err = pthread_mutex_unlock(&head->mutex)) != 0) {
    return -err;
  }
  dst = (char*)head + pos;
  memcpy(dst, Sbytevector_data(value), len);
  memcpy(dst + len, &len, sizeof(size_t));
  memcpy(dst + len + sizeof(size_t), &key, sizeof(uint64_t));

  {
    const size_t pagesz = scheme2k_os_pagesize();

    /* round dst .. dst+slen to pages */
    char* start = c_shm_align_down(dst, pagesz);
    char* end   = c_shm_align_up(dst + slen, pagesz);

    if (msync(start, (size_t)(end - start), MS_SYNC) != 0) {
      return c_errno();
    }
  }
  return 0;
}

/*
 * remove last key, value from shared memory and return a pair (key . value)
 * or '() if shared memory is empty, or integer < 0 on error
 */
static ptr c_shm_remove(void) {
  uint64_t  key = 0;
  shm_head* head;
  char*     addr;
  ptr       value;
  size_t    entry_n;
  size_t    len;
  int       err;

  if ((head = c_shm_head) == NULL) {
    return Sinteger(-EBADF);
  }
  if ((err = pthread_mutex_lock(&head->mutex)) != 0) {
    return Sinteger(-err);
  }
  if ((entry_n = head->entry_n) == 0) {
    (void)pthread_mutex_unlock(&head->mutex);
    return Snil;
  }
  head->entry_n = entry_n - 1;

  addr  = (char*)head + head->free_pos;
  value = Sfalse;
  if ((size_t)(addr - (char*)head) >= sizeof(shm_head) + sizeof(uint64_t) + sizeof(size_t)) {
    memcpy(&key, addr -= sizeof(uint64_t), sizeof(uint64_t));
    memcpy(&len, addr -= sizeof(size_t), sizeof(size_t));

    if (len <= (size_t)(addr - (char*)head - sizeof(shm_head))) {
      iptr ilen = (iptr)len;
      if (ilen >= 0 && (size_t)ilen == len) {
        /** FIXME: if Smake_bytevector() throws, mutex remains locked */
        value = Smake_bytevector(ilen, 0);
        memcpy(Sbytevector_data(value), addr -= len, len);
      }
    }
  }
  head->free_pos = addr - (char*)head;
  (void)pthread_mutex_unlock(&head->mutex);
  return Scons(Sunsigned64(key), value);
}
