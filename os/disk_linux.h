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

#include <sys/statvfs.h>   /* statvfs() */
#include <sys/sysmacros.h> /* makedev(), major(), minor() */

static uint64_t c_make_dev(unsigned major, unsigned minor) {
  return (uint64_t)makedev(major, minor);
}

static unsigned c_dev_major(uint64_t dev) {
  return major((dev_t)dev);
}

static unsigned c_dev_minor(uint64_t dev) {
  return minor((dev_t)dev);
}

/**
 * on success, return a bytevector0 containing the bytes read from
 *   /proc/self/mountinfo
 * on error, return c_errno() < 0
 */
static ptr c_disk_open(void) {
  const size_t   bvec_cap = 65536;
  const iptr     head_len = 4;
  ptr            bsrc     = Smake_bytevector(head_len + bvec_cap, 0);
  unsigned char* start    = Sbytevector_data(bsrc) + head_len;
  unsigned char* end = read_file_at(AT_FDCWD, "/proc/self/mountinfo", start, bvec_cap, NULL, NULL);
  if (end != NULL) {
    const uint16_t len = end - start;
    memcpy(start - 2, &len, 2);
    return bsrc;
  }
  return Sinteger(c_errno_set(ENOENT)); /* < 0 */
}

static void c_disk_close(ptr bsrc) {
  (void)bsrc;
}

/**
 * skip one mounted filesystem from /proc/self/mountinfo
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_disk_skip(ptr bsrc) {
  const unsigned char* src;
  iptr                 bsrc_len;
  const iptr           head_len = 4;
  size_t               skipped;
  uint16_t             pos, len;

  if (!Sbytevectorp(bsrc) || (bsrc_len = Sbytevector_length(bsrc)) < head_len ||
      (src = Sbytevector_data(bsrc))[bsrc_len - 1] != '\0') {
    return c_errno_set(EINVAL);
  }
  memcpy(&pos, src, 2);
  memcpy(&len, src + 2, 2);
  if (pos >= len || (iptr)len >= bsrc_len - head_len) {
    return 0;
  }
  src += head_len + pos;
  skipped = skip_until(&src, '\n');
  if (skipped == 0) {
    return 0;
  }
  pos += skipped;
  memcpy(Sbytevector_data(bsrc), &pos, 2); /* update pos stored in bsrc[0..1] */
  return 1;
}

/**
 * read one mounted filesystem from /proc/self/mountinfo, represented as a bytevector0 bsrc
 *
 * On success, return list (dev_name mount_point . fs_type), fill bvec and update offset.
 * On end-of-file, return 0
 * On I/O error, return c_errno() < 0
 * On parsing errors, return #f
 */
static ptr c_disk_get(ptr bsrc, ptr bvec) {
  char                 mountpoint[4096];
  char                 fs_type[256];
  char                 dev_name[256];
  uint64_t             dev_major, dev_minor;
  const unsigned char* src;
  unsigned char *      bsrc_data, *vec;
  iptr                 bsrc_len;
  const iptr           head_len = 4;
  uint16_t             pos, len;
  uint8_t              ok;
  char                 colon;

  if (!Sbytevectorp(bvec) || (bsrc_len = Sbytevector_length(bsrc)) < head_len ||
      (bsrc_data = Sbytevector_data(bsrc))[bsrc_len - 1] != '\0' || /**/
      !Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_disk_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  memcpy(&pos, bsrc_data, 2);
  memcpy(&len, bsrc_data + 2, 2);
  if (pos >= len || (iptr)len >= bsrc_len - head_len) {
    return 0;
  }
  src = bsrc_data + head_len + pos;

  vec = Sbytevector_data(bvec);
  memset(vec, '\0', e_disk_byte_n);

  ok = parse_uint64(&src, vec, e_disk_id) > 0 && parse_uint64(&src, NULL, 0 /*parent_id*/) > 0 &&
       parse_uint64(&src, &dev_major, 0) && parse_char(&src, &colon) && colon == ':' &&
       parse_uint64(&src, &dev_minor, 0) && parse_string(&src, NULL, 4096 /*root*/) &&
       parse_string(&src, mountpoint, sizeof(mountpoint)) && skip_until(&src, '-') &&
       parse_string(&src, fs_type, sizeof(fs_type)) &&
       parse_string(&src, dev_name, sizeof(dev_name));
  skip_until(&src, '\n');

  pos = src - (bsrc_data + head_len);
  memcpy(bsrc_data, &pos, 2); /* update pos stored in bsrc[0..1] */

  if (ok) {
    struct statvfs entry;
    if (statvfs(mountpoint, &entry) == 0) {
      uint64_t bsize = entry.f_bsize;
      set_uint64(vec, e_disk_size_total, bsize * entry.f_blocks);
      set_uint64(vec, e_disk_size_free, bsize * entry.f_bfree);
      set_uint64(vec, e_disk_size_avail, bsize * entry.f_bavail);
      set_uint64(vec, e_disk_inode_total, entry.f_files);
      set_uint64(vec, e_disk_inode_free, entry.f_ffree);
      set_uint64(vec, e_disk_inode_avail, entry.f_favail);
      set_uint64(vec, e_disk_blocksize, bsize);
      set_uint64(vec, e_disk_dev, makedev(dev_major, dev_minor));
      set_uint64(vec, e_disk_flags, entry.f_flag);
      return Scons(C_SSTRING_UTF8b(dev_name),
                   Scons(C_SSTRING_UTF8b(mountpoint), /**/
                         C_SSTRING_UTF8b(fs_type)));
    }
  }
  return *src ? Sfalse : Sfixnum(0);
}

#endif /* SCHEME2K_OS_DISK_LINUX_H */
