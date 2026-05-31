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

static int              c_shm_fd    = -1;
static pthread_mutex_t* c_shm_mutex = NULL;

/* for exposition only */
typedef struct {
  uint64_t key; /* usually a pid */
  size_t   len;
  /* followed by data: len bytes */
} s_shm_entry;

typedef struct {
  size_t entry_n;
  size_t used;
  /* followed by entry_n elements, each a s_shm_entry */
} s_shm_data;

s_shm_data*   c_shm_data     = NULL;
static size_t c_shm_capacity = 0; /* sum of len0, data0, len1, data1 ... */

static void* c_shm_align(void* addr) {
  /* align addr at 2 * sizeof(uint64_t) */
  return (void*)(((uintptr_t)addr + 15) & ~(uintptr_t)15);
}

static int c_shm_mmap_init(void) {
  size_t pagesz = scheme2k_os_pagesize();
  size_t sz     = (65536 + pagesz - 1) & ~(pagesz - 1);
  void*  addr;
  int    fd = c_shm_fd;
  if (ftruncate(fd, sz) >= 0 &&
      (addr = mmap(NULL, sz, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0)) != MAP_FAILED) {

    pthread_mutex_t*    mut = c_shm_mutex = (pthread_mutex_t*)addr;
    s_shm_data*         sdata = c_shm_data = (s_shm_data*)c_shm_align(mut + 1);
    pthread_mutexattr_t attr;

    pthread_mutexattr_init(&attr);
    pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    pthread_mutex_init(mut, &attr); /* documented to always return 0 */
    pthread_mutexattr_destroy(&attr);

    sdata->entry_n = 0;
    sdata->used    = sizeof(s_shm_data);
    c_shm_capacity = sz - ((char*)sdata - (char*)addr);

    return 0;
  }
  return -1;
}

static int c_shm_init(int fd_to_use) {
  char   path[80];
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
  }
  c_shm_fd = fd_to_use;
  if (c_shm_mmap_init() == 0) {
    return 0;
  }
  close(fd_to_use);
  c_shm_fd = -1;
fail:
  return c_errno();
}

static int c_shm_write(uint64_t key, ptr bv) {

  pthread_mutex_t* mut;
  s_shm_data*      sdata;
  char*            dst;
  size_t           pos, len, slen;
  iptr             ilen;
  int              err;
  if (!Sbytevectorp(bv) || (ilen = Sbytevector_length(bv)) < 0) {
    return -EINVAL;
  }
  if ((size_t)ilen > (size_t)-1 - sizeof(uint64_t) + sizeof(size_t)) {
    return -ENOSPC;
  }
  mut   = c_shm_mutex;
  sdata = c_shm_data;
  if (!mut || !sdata) {
    return -EBADF;
  }
  if ((err = pthread_mutex_lock(mut)) != 0) {
    return -err;
  }

  len  = (size_t)ilen;
  slen = len + sizeof(uint64_t) + sizeof(size_t);
  pos  = sdata->used;

  if (slen > c_shm_capacity || pos > slen - c_shm_capacity) {
    (void)pthread_mutex_unlock(mut);
    return -ENOSPC; /* FIXME: implement shm resizing */
  }

  sdata->entry_n++;
  sdata->used = pos + slen;

  if ((err = pthread_mutex_unlock(mut)) != 0) {
    return -err;
  }
  dst = (char*)sdata + pos;
  memcpy(dst, &key, sizeof(uint64_t));
  memcpy(dst + sizeof(uint64_t), &len, sizeof(size_t));
  memcpy(dst + sizeof(uint64_t) + sizeof(size_t), Sbytevector_data(bv), len);

  {
    const size_t pagesz = scheme2k_os_pagesize();

    /* round dst .. dst+slen to pages */
    char* start = (char*)((uintptr_t)dst & ~(pagesz - 1));
    char* end   = (char*)((uintptr_t)(dst + slen + (pagesz - 1)) & ~(pagesz - 1));

    if (msync(start, (size_t)(end - start), MS_SYNC) != 0) {
      return c_errno();
    }
  }
  return 0;
}
