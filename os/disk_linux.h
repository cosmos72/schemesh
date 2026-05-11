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
 * on success, return a pair (0 . content_bytevector0) containing the bytes read from
 *   /proc/self/mountinfo
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  const size_t src_len = 65536;
  ptr          bsrc    = Smake_bytevector(src_len, 0);
  if (read_file_at(AT_FDCWD, "/proc/self/mountinfo", Sbytevector_data(bsrc), src_len, NULL, NULL) ==
      NULL) {
    return Sinteger(c_errno_set(ENOENT)); /* < 0 */
  }
  return Scons(Sfixnum(0), bsrc);
}

static void c_disk_close(ptr pair) {
  (void)pair;
}

/**
 * skip one mounted filesystem from /proc/self/mountinfo
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

  if (!Spairp(pair) || !Sfixnump(foffset = Scar(pair)) || (offset = Sfixnum_value(foffset)) < 0 ||
      !Sbytevectorp(bsrc = Scdr(pair)) || (src_len = Sbytevector_length(bsrc)) <= 0 ||
      offset >= src_len || (src = Sbytevector_data(bsrc))[src_len - 1] != '\0') {
    return c_errno_set(EINVAL);
  }
  src += offset;
  skipped = skip_until(&src, '\n');
  if (skipped == 0) {
    return 0;
  }
  Sset_car(pair, Sfixnum(offset + skipped)); /* update offset */
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
  char                 fs_type[256];
  char                 device_name[256];
  ptr                  bsrc, foffset;
  const unsigned char* src;
  unsigned char*       vec;
  iptr                 offset;
  iptr                 src_len;
  uint8_t              ok;
  char                 colon;

  if (!Spairp(pair) || !Sfixnump(foffset = Scar(pair)) || (offset = Sfixnum_value(foffset)) < 0 ||
      !Sbytevectorp(bsrc = Scdr(pair)) || (src_len = Sbytevector_length(bsrc)) <= 0 ||
      offset >= src_len || (src = Sbytevector_data(bsrc))[src_len - 1] != '\0' ||
      !Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_disk_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  src += offset;
  vec = Sbytevector_data(bvec);
  memset(vec, '\0', e_disk_byte_n);

  ok = parse_uint64(&src, vec, e_disk_id) > 0 && parse_uint64(&src, NULL, 0 /*parent_id*/) > 0 &&
       parse_uint64(&src, vec, e_disk_major) && parse_char(&src, &colon) && colon == ':' &&
       parse_uint64(&src, vec, e_disk_minor) && parse_string(&src, NULL, 4096 /*root*/) &&
       parse_string(&src, mountpoint, sizeof(mountpoint)) && skip_until(&src, '-') &&
       parse_string(&src, fs_type, sizeof(fs_type)) &&
       parse_string(&src, device_name, sizeof(device_name));
  skip_until(&src, '\n');

  Sset_car(pair, Sfixnum(src - Sbytevector_data(bsrc))); /* update offset */

  if (ok) {
    struct statvfs entry;
    if (statvfs(mountpoint, &entry) == 0) {
      set_uint64(vec, e_disk_size_total, entry.f_bsize * entry.f_blocks);
      set_uint64(vec, e_disk_size_free, entry.f_bsize * entry.f_bfree);
      set_uint64(vec, e_disk_size_avail, entry.f_bsize * entry.f_bavail);
      set_uint64(vec, e_disk_inode_total, entry.f_files);
      set_uint64(vec, e_disk_inode_free, entry.f_ffree);
      set_uint64(vec, e_disk_inode_avail, entry.f_favail);
      set_uint64(vec, e_disk_blocksize, entry.f_bsize);
      set_uint64(vec, e_disk_flags, entry.f_flag);
      return Scons(scheme2k_Sstring_utf8b(device_name, (size_t)-1),
                   scheme2k_Sstring_utf8b(mountpoint, (size_t)-1));
    }
  }
  return *src ? Sfalse : Sfixnum(0);
}

#endif /* SCHEME2K_OS_DISK_LINUX_H */
