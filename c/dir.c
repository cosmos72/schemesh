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
#include <limits.h>    /* PATH_MAX on *BSD */
#include <pwd.h>       /* getpwuid_r()  */
#include <stdlib.h>    /* exit()        */
#include <sys/stat.h>  /* fstatat()     */
#include <sys/types.h> /* opendir(), closedir() */
#include <unistd.h>    /* readlinkat()  */

#include "writer.h"

#ifdef __APPLE__
#include <sys/syslimits.h> /* PATH_MAX */
#undef SCHEME2K_C_DIR_HAVE_TIMESPEC
#else
#define SCHEME2K_C_DIR_HAVE_TIMESPEC
#endif

#define KEY(str) (",\"" str "\":"), (sizeof(str) + 3)
#define MKQ(str) make_string("\"" str "\"", sizeof(str) + 1)

typedef enum {
  e_dir_flag_type        = 1 << 0,
  e_dir_flag_size        = 1 << 1,
  e_dir_flag_target      = 1 << 2,
  e_dir_flag_modified    = 1 << 3,
  e_dir_flag_accessed    = 1 << 4,
  e_dir_flag_ino_changed = 1 << 5,
  e_dir_flag_mode        = 1 << 6,
  e_dir_flag_user        = 1 << 7,
  e_dir_flag_group       = 1 << 8,
  e_dir_flag_uid         = 1 << 9,
  e_dir_flag_gid         = 1 << 10,
  e_dir_flag_inode       = 1 << 11,
  e_dir_flag_num_links   = 1 << 12,

  e_dir_flag_default = e_dir_flag_type | e_dir_flag_size | e_dir_flag_modified,
  e_dir_flag_long = e_dir_flag_default | e_dir_flag_accessed | e_dir_flag_mode | e_dir_flag_user |
                    e_dir_flag_group,
  e_dir_flag_verbose = (1 << 13) - 1,

  e_dir_flag_hidden      = 1 << 13, /* omit entries starting with "." */
  e_dir_flag_dir_as_file = 1 << 14, /* show directories themselvers, not their contents */
  e_dir_flag_dirprefix   = 1 << 15, /* print dir path before entry name */
} e_dir_flags;

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

typedef struct {
  const char* data;
  size_t      len;
} string;

static string make_string(const char* data, size_t len) {
  string ret = {data, len};
  return ret;
}

static void w_put_string(writer* w, string s) {
  w_put_chars_len(w, s.data, s.len);
}

#ifdef SCHEME2K_C_DIR_HAVE_TIMESPEC
static void write_timespec(writer*                w,
                           e_dir_flags            flags,
                           const char*            label,
                           const size_t           label_len,
                           const struct timespec* t) {
  if (flags) {
    w_put_chars_len(w, label, label_len);
    w_put_literal(w, "{\"<type>\":\"time-utc\",\"value\":");
    w_put_int64(w, t->tv_sec);
    if (t->tv_nsec != 0) {
      w_put_char(w, '.');
      w_put_fractional_digits(w, t->tv_nsec, 9);
    }
    w_put_char(w, '}');
  }
}
#else
static void write_time(
    writer* w, e_dir_flags flags, const char* label, const size_t label_len, const time_t t) {
  if (flags) {
    w_put_chars_len(w, label, label_len);
    w_put_literal(w, "{\"<type>\":\"time-utc\",\"value\":");
    w_put_int64(w, t);
    w_put_char(w, '}');
  }
}
#endif /* SCHEME2K_C_DIR_HAVE_TIMESPEC */

static void write_int64(
    writer* w, e_dir_flags flags, const char* label, const size_t label_len, const int64_t num) {
  if (flags) {
    w_put_chars_len(w, label, label_len);
    w_put_int64(w, num);
  }
}

static void write_uint64(
    writer* w, e_dir_flags flags, const char* label, const size_t label_len, const uint64_t num) {
  if (flags) {
    w_put_chars_len(w, label, label_len);
    w_put_uint64(w, num);
  }
}

static void write_etype(writer* w, e_dir_flags flags, e_type type) {
  string s;
  if (!flags) {
    return;
  }
  switch (type) {
    case e_type_unknown:
    default:
      return;
    case e_type_fifo:
      s = MKQ("fifo");
      break;
    case e_type_chr:
      s = MKQ("char-device");
      break;
    case e_type_dir:
      s = MKQ("dir");
      break;
    case e_type_blk:
      s = MKQ("block-device");
      break;
    case e_type_reg:
      s = MKQ("file");
      break;
    case e_type_lnk:
      s = MKQ("symlink");
      break;
    case e_type_sock:
      s = MKQ("socket");
      break;
  }
  w_put_chars_len(w, KEY("type"));
  w_put_string(w, s);
}

static void write_mode(writer* w, e_dir_flags flags, uint32_t mode) {
  const char rwx[] = "rwxrwxrwxSST";
  unsigned   i;
  if (!flags) {
    return;
  }
  w_put_chars_len(w, KEY("mode"));
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
  w_put_chars_len(w, KEY("user"));
  w_put_quoted_escaped_chars(w, username);
}

static void write_gr_groupname(writer* w, const char* groupname) {
  w_put_chars_len(w, KEY("group"));
  w_put_quoted_escaped_chars(w, groupname);
}

static void write_username(writer* w, e_dir_flags flags, uid_t uid) {
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

static void write_groupname(writer* w, e_dir_flags flags, gid_t gid) {
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

static void write_symlink(writer* w, e_dir_flags flags, e_type type, int dir_fd, const char* path) {
  if (flags && type == e_type_lnk) {
    char    buf[PATH_MAX];
    ssize_t len = readlinkat(dir_fd, path, buf, sizeof(buf));
    if (len > 0) {
      w_put_chars_len(w, KEY("symlink"));
      w_put_quoted_escaped_chars_len(w, buf, len);
    }
  }
}

static void write_file_stat(writer* w, e_dir_flags flags, const struct stat* st) {

#ifdef SCHEME2K_C_DIR_HAVE_TIMESPEC
  write_timespec(w, flags & e_dir_flag_modified, KEY("modified"), &(st->st_mtim));
  write_timespec(w, flags & e_dir_flag_accessed, KEY("accessed"), &(st->st_atim));
  write_timespec(w, flags & e_dir_flag_ino_changed, KEY("status-changed"), &(st->st_ctim));
#else
  write_time(w, flags & e_dir_flag_modified, KEY("modified"), st->st_mtime);
  write_time(w, flags & e_dir_flag_accessed, KEY("accessed"), st->st_atime);
  write_time(w, flags & e_dir_flag_ino_changed, KEY("status-changed"), st->st_ctime);
#endif

  write_mode(w, flags & e_dir_flag_mode, st->st_mode & 07777);
  write_username(w, flags & e_dir_flag_user, st->st_uid);
  write_groupname(w, flags & e_dir_flag_group, st->st_gid);
  write_int64(w, flags & e_dir_flag_uid, KEY("uid"), st->st_uid);
  write_int64(w, flags & e_dir_flag_gid, KEY("gid"), st->st_gid);
  write_uint64(w, flags & e_dir_flag_inode, KEY("inode"), st->st_ino);
  write_uint64(w, flags & e_dir_flag_num_links, KEY("nlink"), st->st_nlink);
}

static int
write_dir_entry_header(writer* w, e_dir_flags flags, const char* filepath, const string dirpath) {
  w_put_literal(w, "{\"<type>\":\"dir-entry\",\"name\":");

  /* dirpath and entry->d_name can be arbitrary bytes, not only valid UTF-8 */
  w_put_char(w, '"');
  if (flags & e_dir_flag_dirprefix) {
    w_put_escaped_chars_len(w, dirpath.data, dirpath.len);
    if (dirpath.len != 0 && dirpath.data[dirpath.len - 1] != '/') {
      w_put_char(w, '/');
    }
  }
  w_put_escaped_chars(w, filepath);
  w_put_char(w, '"');
  return 0;
}

static int write_dir_entry(writer* w, e_dir_flags flags, DIR* dir, const string dirpath) {
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

  write_dir_entry_header(w, flags, entry->d_name, dirpath);

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
    write_uint64(w, flags & e_dir_flag_size, KEY("size"), (uint64_t)(st.st_size));
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
    write_int64(w, flags & e_dir_flag_inode, KEY("inode"), entry->d_ino);
  }
  w_put_literal(w, "}\n");
  return 1;
}

static int write_dir(writer* w, e_dir_flags flags, const string dirpath) {
  DIR* dir = opendir(dirpath.data);
  int  err;
  if (!dir) {
    return -errno;
  }
  while ((err = write_dir_entry(w, flags, dir, dirpath)) > 0) {
  }
  closedir(dir);
  return err;
}

static int write_file(writer* w, e_dir_flags flags, struct stat* st, const string filepath) {
  const char* path = filepath.data;
  e_type      type = modeToEtype(st->st_mode);
  write_dir_entry_header(w, flags, path, make_string(NULL, 0));
  write_etype(w, flags & e_dir_flag_type, type);
  write_uint64(w, flags & e_dir_flag_size, KEY("size"), (uint64_t)(st->st_size));
  write_symlink(w, flags & e_dir_flag_target, type, AT_FDCWD, path);
  write_file_stat(w, flags, st);
  w_put_literal(w, "}\n");
  return 0;
}

static int write_file_or_dir(writer* w, e_dir_flags flags, const string path) {
  struct stat st;
  if (fstatat(AT_FDCWD, path.data, &st, AT_SYMLINK_NOFOLLOW) < 0) {
    return -errno;
  } else if ((flags & e_dir_flag_dir_as_file) || (st.st_mode & S_IFMT) != S_IFDIR) {
    return write_file(w, flags, &st, path);
  } else {
    return write_dir(w, flags, path);
  }
}

static void usage(char* argv0) {
  fprintf(stderr,
          "Usage: %s [OPTION]... [FILE]...\n"
          "Write JSON information about FILEs (the current directory by default).\n"
          "Options:\n"
          "  -a      also show entries starting with .\n"
          "  -d      list directories themselves, not their contents\n"
          "  -l      use a long listing format\n"
          "  -v      use a very long listing format\n"
          "  --      end of options\n"
          "  --help  show this help\n",
          argv0);
}

static e_dir_flags parse_dir_flags(int argc, char* argv[]) {
  char*       arg;
  int         i;
  e_dir_flags flags = e_dir_flag_default;
  for (i = 1; i < argc; ++i) {
    if (!(arg = argv[i]) || !strcmp(arg, "--")) {
      break;
    } else if (arg[0] != '-') {
      continue;
    } else if (!strcmp(arg, "-a")) {
      flags |= e_dir_flag_hidden;
    } else if (!strcmp(arg, "-d")) {
      flags |= e_dir_flag_dir_as_file;
    } else if (!strcmp(arg, "-l")) {
      flags |= e_dir_flag_long;
    } else if (!strcmp(arg, "-v")) {
      flags |= e_dir_flag_verbose;
    } else {
      usage(argv[0]);
      exit(strcmp(arg, "--help") ? 1 : 0);
    }
  }
  return flags;
}

static int count_args(int argc, char* argv[]) {
  char* arg;
  int   i;
  int   n       = 0;
  int   options = 1;
  for (i = 1; i < argc; ++i) {
    if (!(arg = argv[i])) {
      break;
    } else if (options && arg[0] == '-') {
      if (!strcmp(arg, "--")) {
        options = 0;
      }
    } else {
      n++;
    }
  }
  return n;
}

int main(int argc, char* argv[]) {
  writer      w = {stdout, 0, {0}};
  char*       arg;
  e_dir_flags flags = parse_dir_flags(argc, argv);
  int         err = 0, n = count_args(argc, argv);
  int         options, i;

  if (n == 0) {
    err = write_file_or_dir(&w, flags, make_string(".", 1));
    w_flush(&w);
    return err;
  }

  if (n > 1) {
    flags = flags | e_dir_flag_dirprefix;
  }
  for (i = options = 1; i < argc; i++) {
    if (!(arg = argv[i])) {
      break;
    } else if (options && arg[0] == '-') {
      if (!strcmp(arg, "--")) {
        options = 0;
      }
    } else {
      int err_i = write_file_or_dir(&w, flags, make_string(arg, strlen(arg)));
      if (err == 0 && err_i != 0) {
        err = err_i;
      }
    }
  }
  w_flush(&w);
  return err;
}
