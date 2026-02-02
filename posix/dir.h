/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

typedef enum {
  c_vec_name        = 0,
  c_vec_type        = 1,
  c_vec_target      = 2,
  c_vec_mode        = 3,
  c_vec_num_links   = 4,
  c_vec_inode       = 5,
  c_vec_user        = 6,
  c_vec_group       = 7,
  c_vec_size        = 8,
  c_vec_accessed    = 9,
  c_vec_modified    = 10,
  c_vec_ino_changed = 11,
  c_vec_n           = 12,
} c_vec;

typedef enum {
  c_type_unknown = 0,
  c_type_fifo    = 1,
  c_type_chr     = 2,
  c_type_dir     = 3,
  c_type_blk     = 4,
  c_type_reg     = 5,
  c_type_lnk     = 6,
  c_type_sock    = 7,
} c_type;

#ifdef _DIRENT_HAVE_D_TYPE
static c_type dtypeToFileType(unsigned char d_type) {
  switch (d_type) {
#ifdef DT_FIFO
    case DT_FIFO:
      return c_type_fifo;
#endif
#ifdef DT_CHR
    case DT_CHR:
      return c_type_chr;
#endif
#ifdef DT_DIR
    case DT_DIR:
      return c_type_dir;
#endif
#ifdef DT_BLK
    case DT_BLK:
      return c_type_blk;
#endif
#ifdef DT_REG
    case DT_REG:
      return c_type_reg;
#endif
#ifdef DT_LNK
    case DT_LNK:
      return c_type_lnk;
#endif
#ifdef DT_SOCK
    case DT_SOCK:
      return c_type_sock;
#endif
    default:
      return c_type_unknown;
  }
}
#endif /* _DIRENT_HAVE_D_TYPE */

static c_type modeToFileType(mode_t mode) {
  switch (mode & S_IFMT) {
    case S_IFDIR:
      return c_type_dir;
    case S_IFCHR:
      return c_type_chr;
    case S_IFBLK:
      return c_type_blk;
    case S_IFREG:
      return c_type_reg;
    case S_IFIFO:
      return c_type_fifo;
#ifdef S_IFLNK /* not in POSIX.1-1996 */
    case S_IFLNK:
      return c_type_lnk;
#endif
#ifdef S_IFSOCK /* not in POSIX.1-1996 */
    case S_IFSOCK:
      return c_type_sock;
#endif
    default:
      return c_type_unknown;
  }
}

#ifdef __APPLE__
static void fillTime(ptr vec, unsigned i, const time_t t) {
  Svector_set(vec, i, Scons(Sinteger64(t), Sunsigned32(0)));
}
#else
static void fillTime(ptr vec, unsigned i, const struct timespec* t) {
  Svector_set(vec, i, Scons(Sinteger64(t->tv_sec), Sunsigned32(t->tv_nsec)));
}
#endif /* __APPLE__ */

static void fillUserAndGroup(ptr vec, uid_t uid, gid_t gid) {
#ifdef NSS_BUFLEN_GROUP
  char namebuf[NSS_BUFLEN_GROUP];
#else
  char namebuf[1024];
#endif /* NSS_BUFLEN_GROUP */

  ptr obj;
  {
    struct passwd  pbuf;
    struct passwd* pwd = NULL;

    if (getpwuid_r(uid, &pbuf, namebuf, sizeof(namebuf), &pwd) == 0 && pwd != NULL) {
      obj = scheme2k_Sstring_utf8b(pwd->pw_name, -1);
    } else {
      obj = Sunsigned(uid);
    }
    Svector_set(vec, c_vec_user, obj);
  }
  {
    struct group  gbuf;
    struct group* grp = NULL;

    if (getgrgid_r(gid, &gbuf, namebuf, sizeof(namebuf), &grp) == 0 && grp != NULL) {
      obj = scheme2k_Sstring_utf8b(grp->gr_name, -1);
    } else {
      obj = Sunsigned(gid);
    }
    Svector_set(vec, c_vec_group, obj);
  }
}

static ptr c_dir_get_entry(DIR* dir, ptr vec) {
  struct stat    st;
  struct dirent* entry;
  c_type         type;

  if (!dir || !Svectorp(vec) || Svector_length(vec) < c_vec_n) {
    return Sinteger(c_errno_set(EINVAL));
  }

  entry = readdir(dir);
  if (!entry) {
    return Sunsigned32(0); // end of dir
  }

  /* file name can be arbitrary bytes, not only valid UTF-8 */
  Svector_set(vec, c_vec_name, scheme2k_Sstring_utf8b(entry->d_name, -1));
  Svector_set(vec, c_vec_inode, Sunsigned64(entry->d_ino));

#ifdef _DIRENT_HAVE_D_TYPE
  type = dtypeToFileType(entry->d_type);
#else
  type = c_type_unknown;
#endif

  if (lstat(entry->d_name, &st) < 0) {
    /* only a few fields can be filled */
    Svector_set(vec, c_vec_type, Sunsigned32(type));
    return Sunsigned32(1);
  }

  /* lstat() is successful, fill all fields */
  if (type == c_type_unknown) {
    type = modeToFileType(st.st_mode);
    Svector_set(vec, c_vec_type, Sunsigned32(type));
  }
  Svector_set(vec, c_vec_mode, Sunsigned32(st.st_mode & 07777));

  if (type == c_type_lnk) {
    char    buf[PATH_MAX];
    ssize_t len = readlink(entry->d_name, buf, sizeof(buf));
    if (len > 0) {
      /* link target can be arbitrary bytes, not only valid UTF-8 */
      Svector_set(vec, c_vec_target, scheme2k_Sstring_utf8b(buf, len));
    }
  }

  /* owner / group */
  fillUserAndGroup(vec, st.st_uid, st.st_gid);

#ifdef __APPLE__
  fillTime(vec, c_vec_accessed, st.st_atime);
  fillTime(vec, c_vec_modified, st.st_mtime);
  fillTime(vec, c_vec_ino_changed, st.st_ctime);
#else
  fillTime(vec, c_vec_accessed, &(st.st_atim));
  fillTime(vec, c_vec_modified, &(st.st_mtim));
  fillTime(vec, c_vec_ino_changed, &(st.st_ctim));
#endif
  return Sunsigned32(2);
}
