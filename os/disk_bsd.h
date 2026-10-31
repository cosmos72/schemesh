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

/**
 * on success, return some Scheme object for accessing list of mounted file systems
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  int err;
  if ((err = getfsstat(NULL, 0, MNT_WAIT)) >= 0) {
    const size_t   sz  = (size_t)err * sizeof(struct statfs);
    struct statfs* buf = (struct statfs*)malloc(sz);
    if (buf) {
      if ((err = getfsstat(buf, sz, MNT_WAIT)) >= 0) {
        return Scons(Sfixnum(0), Scons(Sfixnum(err), Sunsigned64((Suint64_t)buf)));
      } else {
        err = c_errno();
      }
      free(buf);
    } else {
      err = -ENOMEM;
    }
  } else {
    err = c_errno();
  }
  return Sinteger(err);
}

static void c_disk_close(ptr disk_s) {
  ptr obj;
  if (Spairp(disk_s) && Spairp(obj = Scdr(disk_s))) {
    ptr sbuf = Scdr(obj);
    if (Sfixnump(sbuf) || Sbignump(sbuf)) {
      free((void*)Sunsigned64_value(sbuf));
    }
  }
}

/**
 * skip one mounted file system.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_disk_skip(ptr disk_s) {
  ptr obj;
  if (Spairp(disk_s) && Spairp(obj = Scdr(disk_s))) {
    ptr spos = Scar(disk_s);
    ptr sn   = Scar(obj);
    if (Sfixnump(spos) && Sfixnump(sn)) {
      iptr pos = Sfixnum_value(spos);
      iptr n   = Sfixnum_value(sn);
      if (pos < n) {
        Sset_car(disk_s, Sfixnum(pos + 1));
        return 1;
      }
      return 0; /* EOF */
    }
  }
  return -EINVAL;
}

/**
 * copy one mounted file system info to bvec.
 * return Scons(device_name . mountpoint)
 */
static ptr c_disk_fill(const struct statfs* entry, ptr bvec) {
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
               scheme2k_Sstring_utf8b(entry->f_mntonname, (size_t)-1));
}

/**
 * read one mounted file system.
 *   on success, fill bvec and return pair (device_name . mountpoint)
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static ptr c_disk_get(ptr disk_s, ptr bvec) {
  ptr obj;
  if (Spairp(disk_s) && Spairp(obj = Scdr(disk_s)) && /**/
      Sbytevectorp(bvec) && Sbytevector_length(bvec) == e_disk_byte_n) {

    ptr spos = Scar(disk_s);
    ptr sn   = Scar(obj);
    ptr sbuf = Scdr(obj);
    if (Sfixnump(spos) && Sfixnump(sn) && (Sfixnump(sbuf) || Sbignump(sbuf))) {
      iptr pos = Sfixnum_value(spos);
      iptr n   = Sfixnum_value(sn);
      if (pos < n) {
        Sset_car(disk_s, Sfixnum(pos + 1));
        return c_disk_fill((const struct statfs*)Sunsigned64_value(sbuf) + pos, bvec);
      }
      return Sfixnum(0); /* EOF */
    }
  }
  return Sinteger(c_errno_set(EINVAL));
}

#endif /* SCHEME2K_OS_DISK_BSD_H */
