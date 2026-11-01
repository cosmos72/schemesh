/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_DISK_BSD_H
#define SCHEME2K_OS_DISK_BSD_H

#include <sys/mount.h> /* getfsstat() */
#include <sys/param.h> /* getfsstat() */
#include <sys/types.h> /* makedev(), major(), minor() */

static uint64_t c_make_dev(unsigned major, unsigned minor) {
  return (uint64_t)makedev((int)major, (int)minor);
}

static unsigned c_dev_major(uint64_t dev) {
  return (unsigned)major((dev_t)dev);
}

static unsigned c_dev_minor(uint64_t dev) {
  return (unsigned)minor((dev_t)dev);
}

typedef struct statfs c_statfs;

typedef struct {
  size_t   pos;
  size_t   len;
  c_statfs vec[1];
} c_disk;

/**
 * on success, return some Scheme object for accessing list of mounted file systems
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  int ret = getfsstat(NULL, 0, MNT_WAIT);
  if (ret >= 0) {
    const size_t head_len = sizeof(c_disk) - sizeof(c_statfs);
    const size_t sz       = (size_t)ret * sizeof(c_statfs);
    c_disk*      disk     = (c_disk*)malloc(sz + head_len);
    if (disk) {
      if ((ret = getfsstat(disk->vec, sz, MNT_WAIT)) >= 0) {
        disk->pos = 0;
        disk->len = ret;
        return Sunsigned64((Suint64_t)disk);
      }
      ret = c_errno();
      free(disk);
    } else {
      ret = c_errno_set(ENOMEM);
    }
  } else {
    ret = c_errno();
  }
  return Sinteger(ret);
}

static void c_disk_close(ptr disk_s) {
  if (Sfixnump(disk_s) || Sbignump(disk_s)) {
    free((void*)Sunsigned64_value(disk_s));
  }
}

/**
 * skip one mounted file system.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_disk_skip(ptr disk_s) {
  if (Sfixnump(disk_s) && Sfixnump(disk_s)) {
    c_disk* disk = (c_disk*)Sunsigned64_value(disk_s);
    size_t  pos;
    if (disk && (pos = disk->pos) < disk->len) {
      disk->pos = pos + 1;
      return 1;
    }
    return 0; /* EOF */
  }
  return c_errno_set(EINVAL);
}

/**
 * copy one mounted file system info to bvec.
 * return Scons(device_name . mountpoint)
 */
static ptr c_disk_fill(const c_statfs* entry, ptr bvec) {
  const uint64_t bsize = entry->f_bsize;
  unsigned char* vec   = Sbytevector_data(bvec);
  memset(vec, '\0', e_disk_byte_n);

  set_uint64(vec, e_disk_id, entry->f_fsid.val[0] | (uint64_t)entry->f_fsid.val[1] << 32);
  set_uint64(vec, e_disk_size_total, bsize * entry->f_blocks);
  set_uint64(vec, e_disk_size_free, bsize * entry->f_bfree);
  set_uint64(vec, e_disk_size_avail, bsize * entry->f_bavail);
  set_uint64(vec, e_disk_inode_total, entry->f_files);
  set_uint64(vec, e_disk_inode_free, entry->f_ffree);
#ifdef __OpenBSD__
  set_uint64(vec, e_disk_inode_avail, entry->f_favail);
#else
  set_uint64(vec, e_disk_inode_avail, entry->f_ffree);
#endif
  set_uint64(vec, e_disk_blocksize, bsize);
  set_uint64(vec, e_disk_dev, makedev(0, 0));
  set_uint64(vec, e_disk_flags, entry->f_flags);
  return Scons(scheme2k_Sstring_utf8b(entry->f_mntfromname, (size_t)-1),
               Scons(scheme2k_Sstring_utf8b(entry->f_mntonname, (size_t)-1),
                     scheme2k_Sstring_utf8b(entry->f_fstypename, (size_t)-1)));
}

/**
 * read one mounted file system.
 *   on success, fill bvec and return pair (device_name . mountpoint)
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static ptr c_disk_get(ptr disk_s, ptr bvec) {
  if ((Sfixnump(disk_s) || Sbignump(disk_s)) && /**/
      Sbytevectorp(bvec) && Sbytevector_length(bvec) == e_disk_byte_n) {
    c_disk* disk = (c_disk*)Sunsigned64_value(disk_s);
    size_t  pos;
    if (disk && (pos = disk->pos) < disk->len) {
      disk->pos = pos + 1;
      return c_disk_fill(disk->vec + pos, bvec);
    }
    return Sfixnum(0); /* EOF */
  }
  return Sinteger(c_errno_set(EINVAL));
}

#endif /* SCHEME2K_OS_DISK_BSD_H */
