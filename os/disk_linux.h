/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_OS_DISK_LINUX_H
#define SCHEME2K_OS_DISK_LINUX_H

#include <sys/statvfs.h> /* statvfs() */

/**
 * on success, return scheme unsigned number containing C DIR*
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  return Sinteger(c_errno_set(ENOTSUP)); /* < 0 */
}

static void c_disk_close(ptr pair) {
  (void)pair;
}

/**
 * skip one pone mounted filesystem from /proc/self/mountinfo
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_disk_skip(ptr pair) {
  ptr                  bsrc, foffset;
  const unsigned char* src;
  iptr                 offset;
  iptr                 src_len;
  size_t               skipped;

  if (!Spairp(pair) || !Sbytevectorp(bsrc = Scar(pair)) || !Sfixnump(foffset = Scdr(pair)) ||
      (offset = Sfixnum_value(foffset)) < 0 || (src_len = Sbytevector_length(bsrc)) <= 0 ||
      offset >= src_len || (src = Sbytevector_data(bsrc))[src_len - 1] != '\0') {
    return c_errno_set(EINVAL);
  }
  src += offset;
  skipped = skip_line(&src);
  if (skipped == 0) {
    return 0;
  }
  Sset_cdr(pair, Sfixnum(offset + skipped)); /* update offset */
  return 1;
}

/**
 * read one mounted filesystem from /proc/self/mountinfo, represented as a pair
 * (content_bytevector0 . offset)
 *
 * On success, return cons (device_name . mount_point), fill bvec and update offset.
 * On end-of-file, return 0
 * On I/O error, return c_errno() < 0
 * On parsing errors, return #f
 */
static ptr c_disk_get(ptr pair, ptr bvec) {
  char                 mountpoint[4096];
  ptr                  bsrc, foffset;
  const unsigned char* src;
  unsigned char*       vec;
  iptr                 offset;
  iptr                 src_len;
  uint8_t              ok;

  if (!Spairp(pair) || !Sbytevectorp(bsrc = Scar(pair)) || !Sfixnump(foffset = Scdr(pair)) ||
      (offset = Sfixnum_value(foffset)) < 0 || (src_len = Sbytevector_length(bsrc)) <= 0 ||
      offset >= src_len || (src = Sbytevector_data(bsrc))[src_len - 1] != '\0' ||
      !Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_disk_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  src += offset;
  vec = Sbytevector_data(bvec);
  memset(vec, '\0', e_disk_byte_n);

  ok = parse_uint64(&src, NULL, 0 /*id*/) > 0 && parse_uint64(&src, NULL, 0 /*parent_id*/) > 0 &&
       parse_string(&src, NULL, 4096 /*root*/) &&
       parse_string(&src, mountpoint, sizeof(mountpoint));
  skip_line(&src);
  /** TODO: parse dev_t, device_name */

  Sset_cdr(pair, Sfixnum(src - Sbytevector_data(bsrc))); /* update offset */

  if (ok) {
    struct statvfs entry;
    if (statvfs(mountpoint, &entry) == 0) {
      set_uint64(vec, e_disk_id, entry.f_fsid);
      set_uint64(vec, e_disk_size_total, entry.f_bsize * entry.f_blocks);
      set_uint64(vec, e_disk_size_free, entry.f_bsize * entry.f_bfree);
      set_uint64(vec, e_disk_size_avail, entry.f_bsize * entry.f_bavail);
      set_uint64(vec, e_disk_inode_total, entry.f_bsize * entry.f_files);
      set_uint64(vec, e_disk_inode_free, entry.f_bsize * entry.f_ffree);
      set_uint64(vec, e_disk_inode_avail, entry.f_bsize * entry.f_favail);
      set_uint64(vec, e_disk_blocksize, entry.f_bsize);
      set_uint64(vec, e_disk_flags, entry.f_flag);
      /** TODO: fill vec[e_disk_dev], return device_name */
      return Scons(Smake_string(0, 0), scheme2k_Sstring_utf8b(mountpoint, (size_t)-1));
    }
  }
  return Sfalse;
}

#endif /* SCHEME2K_OS_DISK_LINUX_H */
