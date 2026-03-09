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
/*      minimal reimplementation of classic 'ls' program with JSON output     */
/*                                                                            */
/******************************************************************************/

#include <dirent.h>    /* readdir()     */
#include <errno.h>     /* EINVAL, errno */
#include <fcntl.h>     /* AT_SYMLINK_NOFOLLOW */
#include <grp.h>       /* getgrgid_r()  */
#include <pwd.h>       /* getpwuid_r()  */
#include <sys/stat.h>  /* fstatat()     */
#include <sys/types.h> /* opendir(), closedir() */
#include <unistd.h>    /* readlinkat()  */

#include "writer.h"

#ifdef __APPLE__
#undef SCHEME2K_C_DIR_HAVE_TIMESPEC
#else
#define SCHEME2K_C_DIR_HAVE_TIMESPEC
#endif

#define S(str) (",\"" str "\":"), (sizeof(str) + 3)

typedef enum {
  /* e_dir_flag_name     = 1 << 0, */
  e_dir_flag_type        = 1 << 1,
  e_dir_flag_size        = 1 << 2,
  e_dir_flag_target      = 1 << 3,
  e_dir_flag_modified    = 1 << 4,
  e_dir_flag_accessed    = 1 << 5,
  e_dir_flag_ino_changed = 1 << 6,
  e_dir_flag_mode        = 1 << 7,
  e_dir_flag_uid         = 1 << 8,
  e_dir_flag_gid         = 1 << 9,
  e_dir_flag_inode       = 1 << 10,
  e_dir_flag_num_links   = 1 << 11,
  e_dir_flag_hidden      = 1 << 12,
  e_dir_flag_all         = (1 << 13) - 1,
} e_dir_flag;

typedef enum {
  e_type_unknown = 0,
  e_type_fifo    = 1,
  e_type_chr     = 2,
  e_type_dir     = 3,
  e_type_blk     = 4,
  e_type_reg     = 5,
  e_type_lnk     = 6,
  e_type_sock    = 7,
} e_type;

#ifdef SCHEME2K_C_DIR_HAVE_TIMESPEC
static void write_timespec(writer*                w,
                           e_dir_flag             flags,
                           const char*            label,
                           const size_t           label_len,
                           const struct timespec* t) {
  if (flags) {
    w_put_chars3(w, label, label_len);
    w_put_chars(w, "{\"<type>\":\"time-utc\",\"value\":");
    w_put_int64(w, t->tv_sec);
    w_put_char(w, '.');
    w_put_fractional_digits(w, t->tv_nsec, 9);
    w_put_char(w, '}');
  }
}
#else
static void
write_time(writer* w, e_dir_flag flags, const char* label, const size_t label_len, const time_t t) {
  if (flags) {
    w_put_chars3(w, label, label_len);
    w_put_chars(w, "{\"<type>\":\"time-utc\",\"value\":");
    w_put_int64(w, t);
    w_put_char(w, '}');
  }
}
#endif /* SCHEME2K_C_DIR_HAVE_TIMESPEC */

static void write_int64(
    writer* w, e_dir_flag flags, const char* label, const size_t label_len, const int64_t num) {
  if (flags) {
    w_put_chars3(w, label, label_len);
    w_put_int64(w, num);
  }
}

static void write_uint64(
    writer* w, e_dir_flag flags, const char* label, const size_t label_len, const uint64_t num) {
  if (flags) {
    w_put_chars3(w, label, label_len);
    w_put_uint64(w, num);
  }
}

static void write_etype(writer* w, e_dir_flag flags, e_type type) {
  const char* s = NULL;
  if (!flags) {
    return;
  }
  switch (type) {
    case e_type_unknown:
    default:
      return;
    case e_type_fifo:
      s = "\"fifo\"";
      break;
    case e_type_chr:
      s = "\"char-device\"";
      break;
    case e_type_dir:
      s = "\"dir\"";
      break;
    case e_type_blk:
      s = "\"block-device\"";
      break;
    case e_type_reg:
      s = "\"file\"";
      break;
    case e_type_lnk:
      s = "\"symlink\"";
      break;
    case e_type_sock:
      s = "\"socket\"";
      break;
  }
  w_put_chars3(w, S("type"));
  w_put_chars3(w, s, strlen(s));
}

static void write_mode(writer* w, e_dir_flag flags, uint32_t mode) {
  const char rwx[] = "rwxrwxrwxSST";
  unsigned   i;
  if (!flags) {
    return;
  }
  w_put_chars3(w, S("mode"));
  w_put_char(w, '"');
  for (i = 0; i < 9; i++) {
    w_put_char(w, (mode & (0400 >> i)) ? rwx[i] : '-');
  }
  if (mode & 07000) {
    for (i = 0; i < 3; i++) {
      w_put_char(w, (mode & (04000 >> i)) ? rwx[i + 9] : '-');
    }
  }
  w_put_char(w, '"');
}

static void write_pw_username(writer* w, const char* username) {
  w_put_chars3(w, S("user"));
  w_put_string_quoted_json_escape(w, username);
}

static void write_gr_groupname(writer* w, const char* groupname) {
  w_put_chars3(w, S("group"));
  w_put_string_quoted_json_escape(w, groupname);
}

static void write_username(writer* w, e_dir_flag flags, uid_t uid) {
  static struct passwd* pwd = NULL;

  if (!flags) {
    return;
  }
  if (!pwd || !pwd->pw_name || pwd->pw_uid != uid) {
    /* call again only if current pwd is not applicable */
    pwd = getpwuid(uid);
  }
  if (pwd) {
    write_pw_username(w, pwd->pw_name);
  }
}

static void write_groupname(writer* w, e_dir_flag flags, gid_t gid) {
  static struct group* grp = NULL;

  if (!flags) {
    return;
  }
  if (!grp || !grp->gr_name || grp->gr_gid != gid) {
    /* call again only if current grp is not applicable */
    grp = getgrgid((gid_t)gid);
  }
  if (grp) {
    write_gr_groupname(w, grp->gr_name);
  }
}

#ifdef _DIRENT_HAVE_D_TYPE
static e_type dtypeToEtype(unsigned char d_type) {
  switch (d_type) {
#ifdef DT_FIFO
    case DT_FIFO:
      return e_type_fifo;
#endif
#ifdef DT_CHR
    case DT_CHR:
      return e_type_chr;
#endif
#ifdef DT_DIR
    case DT_DIR:
      return e_type_dir;
#endif
#ifdef DT_BLK
    case DT_BLK:
      return e_type_blk;
#endif
#ifdef DT_REG
    case DT_REG:
      return e_type_reg;
#endif
#ifdef DT_LNK
    case DT_LNK:
      return e_type_lnk;
#endif
#ifdef DT_SOCK
    case DT_SOCK:
      return e_type_sock;
#endif
    default:
      return e_type_unknown;
  }
}
#endif /* _DIRENT_HAVE_D_TYPE */

static e_type modeToEtype(mode_t mode) {
  switch (mode & S_IFMT) {
    case S_IFDIR:
      return e_type_dir;
    case S_IFCHR:
      return e_type_chr;
    case S_IFBLK:
      return e_type_blk;
    case S_IFREG:
      return e_type_reg;
    case S_IFIFO:
      return e_type_fifo;
#ifdef S_IFLNK /* not in POSIX.1-1996 */
    case S_IFLNK:
      return e_type_lnk;
#endif
#ifdef S_IFSOCK /* not in POSIX.1-1996 */
    case S_IFSOCK:
      return e_type_sock;
#endif
    default:
      return e_type_unknown;
  }
}

static void write_symlink(writer* w, e_dir_flag flags, e_type type, int dir_fd, const char* path) {
  if (flags && type == e_type_lnk) {
    char    buf[PATH_MAX];
    ssize_t len = readlinkat(dir_fd, path, buf, sizeof(buf));
    if (len > 0) {
      w_put_chars3(w, S("symlink"));
      w_put_string_quoted_json_escape3(w, (const unsigned char*)buf, len);
    }
  }
}

static void write_file_stat(writer* w, e_dir_flag flags, const struct stat* st) {

#ifdef SCHEME2K_C_DIR_HAVE_TIMESPEC
  write_timespec(w, flags & e_dir_flag_modified, S("modified"), &(st->st_mtim));
  write_timespec(w, flags & e_dir_flag_accessed, S("accessed"), &(st->st_atim));
  write_timespec(w, flags & e_dir_flag_ino_changed, S("status-changed"), &(st->st_ctim));
#else
  write_time(w, flags & e_dir_flag_modified, S("modified"), st->st_mtime);
  write_time(w, flags & e_dir_flag_accessed, S("accessed"), st->st_atime);
  write_time(w, flags & e_dir_flag_ino_changed, S("status-changed"), st->st_ctime);
#endif

  write_mode(w, flags & e_dir_flag_mode, st->st_mode & 07777);
  write_username(w, flags & e_dir_flag_uid, st->st_uid);
  write_groupname(w, flags & e_dir_flag_gid, st->st_gid);
  write_int64(w, flags & e_dir_flag_uid, S("uid"), st->st_uid);
  write_int64(w, flags & e_dir_flag_gid, S("gid"), st->st_gid);
  write_uint64(w, flags & e_dir_flag_inode, S("inode"), st->st_ino);
  write_uint64(w, flags & e_dir_flag_num_links, S("nlink"), st->st_nlink);
}

static int write_dir_entry(writer* w, e_dir_flag flags, DIR* dir) {
  struct stat    st;
  struct dirent* entry;
  int            dir_fd;
  int            have_stat;
  e_type         type = e_type_unknown;

  if (!dir) {
    return EINVAL;
  }

  do {
    errno = 0;
    entry = readdir((DIR*)dir);
    if (!entry) {
      return -errno; /* 0 if end if dir, otherwise error */
    }
  } while ((flags & e_dir_flag_hidden) == 0 && entry->d_name[0] == '.');

  w_put_chars(w, "{\"<type>\":\"dir-entry\",\"name\":");
  /* file name can be arbitrary bytes, not only valid UTF-8 */
  w_put_string_quoted_json_escape(w, entry->d_name);

#ifdef _DIRENT_HAVE_D_TYPE
  if (flags & e_dir_flag_type) {
    type = dtypeToEtype(entry->d_type);
  }
#endif

  dir_fd = dirfd((DIR*)dir);

  if (dir_fd >= 0 && fstatat(dir_fd, entry->d_name, &st, AT_SYMLINK_NOFOLLOW) >= 0) {
    /* fstatat() is successful, write all fields */
    type = modeToEtype(st.st_mode);
    write_etype(w, flags & e_dir_flag_type, type);
    write_uint64(w, flags & e_dir_flag_size, S("size"), (uint64_t)(st.st_size));
    have_stat = 1;
  } else {
    /* write available fields: only type, symlink and inode */
    write_etype(w, flags & e_dir_flag_type, type);
    have_stat = 0;
  }
  write_symlink(w, flags & e_dir_flag_target, type, dir_fd, entry->d_name);
  if (have_stat) {
    write_file_stat(w, flags, &st);
  } else {
    write_int64(w, flags & e_dir_flag_inode, S("inode"), entry->d_ino);
  }
  w_put_chars(w, "}\n");
  return 1;
}

static int write_dir(writer* w, e_dir_flag flags, char* dirpath) {
  DIR* dir = opendir(dirpath);
  int  err;
  if (!dir) {
    return -errno;
  }
  while ((err = write_dir_entry(w, flags, dir)) > 0) {
  }
  closedir(dir);
  return err;
}

int main(int argc, char* argv[]) {
  writer     w     = {stdout, 0, {0}};
  e_dir_flag flags = e_dir_flag_all;
  int        err   = 0;

  /* TODO: parse arguments -l -v -a -- */
  if (argc <= 1) {
    err = write_dir(&w, flags, ".");
  } else {
    int i, err_i;
    for (i = 1; argv[i]; ++i) {
      if ((err_i = write_dir(&w, flags, argv[i])) != 0) {
        err = err_i;
      }
    }
  }
  w_flush(&w);
  return err;
}