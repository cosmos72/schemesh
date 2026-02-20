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
/*                      file-system-related functions                         */
/*                                                                            */
/******************************************************************************/

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/fs.h should only be #included by posix/posix.c"
#endif

typedef enum {
  e_vec_name,
  e_vec_type,
  e_vec_size,
  e_vec_target,
  e_vec_modified,
  e_vec_accessed,
  e_vec_ino_changed,
  e_vec_mode,
  e_vec_uid,
  e_vec_gid,
  e_vec_inode,
  e_vec_num_links,
  e_vec_n,
} e_vec;

enum {
  e_dir_flag_name        = 1 << e_vec_name,
  e_dir_flag_type        = 1 << e_vec_type,
  e_dir_flag_size        = 1 << e_vec_size,
  e_dir_flag_target      = 1 << e_vec_target,
  e_dir_flag_modified    = 1 << e_vec_modified,
  e_dir_flag_accessed    = 1 << e_vec_accessed,
  e_dir_flag_ino_changed = 1 << e_vec_ino_changed,
  e_dir_flag_mode        = 1 << e_vec_mode,
  e_dir_flag_uid         = 1 << e_vec_uid,
  e_dir_flag_gid         = 1 << e_vec_gid,
  e_dir_flag_inode       = 1 << e_vec_inode,
  e_dir_flag_num_links   = 1 << e_vec_num_links,
  e_dir_flag_hidden      = 1 << e_vec_n,
};

typedef enum {
  e_type_unknown = 0,
  e_type_fifo    = 1,
  e_type_chr     = 2,
  e_type_dir     = 3,
  e_type_blk     = 4,
  e_type_reg     = 5,
  e_type_lnk     = 6,
  e_type_sock    = 7,
  e_type_tty     = 8,
} e_type;

/**
 * change current working directory to specified Scheme bytevector0,
 * i.e. a bytevector that must already end with a byte = 0.
 * return 0 on success, or c_errno() < 0 on error.
 */
static int c_chdir(ptr bytevec0) {
  if (Sbytevectorp(bytevec0)) {
    iptr        len = Sbytevector_length(bytevec0);
    const char* dir = (const char*)Sbytevector_data(bytevec0);
    if (len > 0 && dir[len - 1] == 0) {
      if (chdir(dir) == 0) {
        return 0;
      }
      return c_errno();
    }
  }
  return c_errno_set(EINVAL);
}

/**
 * return current working directory as Scheme string,
 * or empty string if an error happens
 */
static ptr c_get_cwd(void) {
  {
    /* call getcwd() with a small stack buffer */
    char dir[256];
    if (getcwd(dir, sizeof(dir)) == dir) {
      return scheme2k_Sstring_utf8b(dir, -1);
    } else if (c_errno() != -ERANGE) {
      return Smake_string(0, 0);
    }
  }
  {
    /* call getcwd() with progressively larger heap buffers */
    size_t maxlen = 1024;
    char*  dir    = NULL;
    while (maxlen && (dir = malloc(maxlen)) != NULL) {
      if (getcwd(dir, maxlen) == dir) {
        ptr ret = scheme2k_Sstring_utf8b(dir, -1);
        free(dir);
        return ret;
      }
      free(dir);
      maxlen *= 2;
    }
  }
  return Smake_string(0, 0);
}

/**
 * create a directory with specified permissions.
 * Argument must be a Scheme bytevector0,
 * i.e. a bytevector that must already end with a byte = 0.
 * return 0 on success, or c_errno() < 0 on error.
 */
static int c_mkdir(ptr bytevec0, int mode) {
  if (Sbytevectorp(bytevec0)) {
    iptr        len = Sbytevector_length(bytevec0);
    const char* dir = (const char*)Sbytevector_data(bytevec0);
    if (len > 0 && dir[len - 1] == 0) {
      if (mkdir(dir, mode) == 0) {
        return 0;
      }
      return c_errno();
    }
  }
  return c_errno_set(EINVAL);
}

#ifdef _DIRENT_HAVE_D_TYPE
static e_type dtypeToFileType(unsigned char d_type) {
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

static e_type modeToFileType(mode_t mode) {
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

#ifdef __APPLE__
static void fillTime(ptr vec, unsigned i, unsigned flag, const time_t t) {
  if (flag) {
    Svector_set(vec, i, Scons(Sinteger64(t), Sunsigned32(0)));
  }
}
#else
static void fillTime(ptr vec, unsigned i, unsigned flag, const struct timespec* t) {
  if (flag) {
    Svector_set(vec, i, Scons(Sinteger64(t->tv_sec), Sunsigned32(t->tv_nsec)));
  }
}
#endif /* __APPLE__ */

static ptr c_dir_open(ptr path) {
  DIR*        dir;
  const char* path0;
  iptr        len;
  if (!Sbytevectorp(path) ||                   /*              */
      (len = Sbytevector_length(path)) <= 0 || /*              */
      (path0 = (const char*)Sbytevector_data(path))[len - 1] != '\0') {
    return Sinteger(c_errno_set(EINVAL)); /* < 0 */
  }
  if (!(dir = opendir(path0))) {
    return Sinteger(c_errno()); /* < 0 */
  }
  return Sunsigned((uintptr_t)(void*)dir);
}

static void c_dir_close(void* dir) {
  if (dir) {
    closedir((DIR*)dir);
  }
}

static int c_dir_skip(void* dir) {
  if (!dir) {
    return c_errno_set(EINVAL);
  }
  errno = 0;
  if (readdir((DIR*)dir) == NULL) {
    return -errno; /* 0 if end of dir, otherwise error */
  }
  return 1; /* skipped one dir entry */
}

static int c_dir_get(void* dir, ptr vec, unsigned flags) {
  struct stat    st;
  struct dirent* entry;
  iptr           vec_n;
  int            dir_fd;
  e_type         type = e_type_unknown;

  if (!dir || !Svectorp(vec)) {
    return c_errno_set(EINVAL);
  }
  if ((vec_n = Svector_length(vec)) > e_vec_n) {
    vec_n = e_vec_n;
  }
  // unset flags that require access beyond the end of vec
  flags = (flags & e_dir_flag_hidden) | (flags & ((1 << vec_n) - 1));

  do {
    errno = 0;
    entry = readdir((DIR*)dir);
    if (!entry) {
      return -errno; /* 0 if end if dir, otherwise error */
    }
  } while ((flags & e_dir_flag_hidden) == 0 && entry->d_name[0] == '.');

  /* file name can be arbitrary bytes, not only valid UTF-8 */
  if (flags & e_dir_flag_name) {
    Svector_set(vec, e_vec_name, scheme2k_Sstring_utf8b(entry->d_name, -1));
  }
  if (flags & e_dir_flag_inode) {
    Svector_set(vec, e_vec_inode, Sunsigned64(entry->d_ino));
  }
  if (flags & e_dir_flag_type) {
#ifdef _DIRENT_HAVE_D_TYPE
    type = dtypeToFileType(entry->d_type);
#endif
  }

  dir_fd = dirfd((DIR*)dir);

  if (dir_fd < 0 || fstatat(dir_fd, entry->d_name, &st, AT_SYMLINK_NOFOLLOW) < 0) {
    /* only a few fields can be filled */
    iptr i;
    if (flags & e_dir_flag_type) {
      Svector_set(vec, e_vec_type, Sunsigned32(type));
    }
    for (i = e_vec_type + 1; i < vec_n; i++) {
      if (i != e_vec_inode) {
        Svector_set(vec, e_vec_type, Svoid);
      }
    }
    return 1;
  }

  /* fstatat() is successful, fill all fields */

  if (flags & e_dir_flag_type) {
    if (type == e_type_unknown) {
      type = modeToFileType(st.st_mode);
    }
    Svector_set(vec, e_vec_type, Sunsigned32(type));
  }
  if (flags & e_dir_flag_size) {
    Svector_set(vec, e_vec_size, Sunsigned64(st.st_size));
  }
  if ((flags & e_dir_flag_target)) {
    ptr target;
    if (type != e_type_lnk) {
      target = Sfalse; /* not a symlink */
    } else {
      char    buf[PATH_MAX];
      ssize_t len = readlinkat(dir_fd, entry->d_name, buf, sizeof(buf));
      if (len <= 0) {
        target = Svoid; /* failed to read symlink */
      } else {
        /* link target can be arbitrary bytes, not only valid UTF-8 */
        target = scheme2k_Sstring_utf8b(buf, len);
      }
    }
    Svector_set(vec, e_vec_target, target);
  }
#ifdef __APPLE__
  fillTime(vec, e_vec_modified, flags & e_dir_flag_modified, st.st_mtime);
  fillTime(vec, e_vec_accessed, flags & e_dir_flag_accessed, st.st_atime);
  fillTime(vec, e_vec_ino_changed, flags & e_dir_flag_ino_changed, st.st_ctime);
#else
  fillTime(vec, e_vec_modified, flags & e_dir_flag_modified, &(st.st_mtim));
  fillTime(vec, e_vec_accessed, flags & e_dir_flag_accessed, &(st.st_atim));
  fillTime(vec, e_vec_ino_changed, flags & e_dir_flag_ino_changed, &(st.st_ctim));
#endif

  if (flags & e_dir_flag_mode) {
    Svector_set(vec, e_vec_mode, Sunsigned32(st.st_mode & 07777));
  }
  if (flags & e_dir_flag_uid) {
    Svector_set(vec, e_vec_uid, Sinteger(st.st_uid));
  }
  if (flags & e_dir_flag_gid) {
    Svector_set(vec, e_vec_gid, Sinteger(st.st_gid));
  }
  if (flags & e_dir_flag_inode) {
    Svector_set(vec, e_vec_inode, Sunsigned64(st.st_ino));
  }
  if (flags & e_dir_flag_num_links) {
    Svector_set(vec, e_vec_num_links, Sunsigned64(st.st_nlink));
  }
  return 2;
}

/**
 * Convert (struct stat.st_mode & S_IFMT) to Scheme integer:
 *   S_IFIFO    -> e_type_fifo = 1
 *   S_IFCHR    -> e_type_chr  = 2
 *   S_IFDIR    -> e_type_dir  = 3
 *   S_IFBLK    -> e_type_blk  = 4
 *   S_IFREG    -> e_type_reg  = 5
 *   S_IFLNK    -> e_type_lnk  = 6 - can only happen if called with lstat() result
 *   S_IFSOCK   -> e_type_sock = 7
 *   otherwise  -> e_type_unknown  = 0
 */
static int c_stat_type(const mode_t s_type) {
  e_type type;
  switch (s_type & S_IFMT) {
    case S_IFBLK:
      type = e_type_blk;
      break;
    case S_IFCHR:
      type = e_type_chr;
      break;
    case S_IFDIR:
      type = e_type_dir;
      break;
    case S_IFIFO:
      type = e_type_fifo;
      break;
    case S_IFREG:
      type = e_type_reg;
      break;
    case S_IFSOCK:
      type = e_type_sock;
      break;
    case S_IFLNK:
      type = e_type_lnk;
      break;
    default:
      type = e_type_unknown;
      break;
  }
  return type;
}

static ptr c_stat_type_fixnum(const mode_t s_type) {
  return Sfixnum(c_stat_type(s_type));
}

/**
 * Convert struct dirent.d_type to Scheme integer:
 *   DT_UNKNOWN -> e_type_unknown = 0
 *   DT_BLK     -> e_type_blk     = 4
 *   DT_CHR     -> e_type_chr     = 2
 *   DT_DIR     -> e_type_dir     = 3
 *   DT_FIFO    -> e_type_fifo    = 4
 *   DT_REG     -> e_type_reg     = 5
 *   DT_SOCK    -> e_type_sock    = 6
 *   DT_LNK     -> e_type_lnk     = 7
 */
static ptr c_dirent_type(unsigned char d_type) {
  e_type type;
  switch (d_type) {
    case DT_BLK:
      type = e_type_blk;
      break;
    case DT_CHR:
      type = e_type_chr;
      break;
    case DT_DIR:
      type = e_type_dir;
      break;
    case DT_FIFO:
      type = e_type_fifo;
      break;
    case DT_LNK:
      type = e_type_lnk;
      break;
    case DT_REG:
      type = e_type_reg;
      break;
    case DT_SOCK:
      type = e_type_sock;
      break;
    case DT_UNKNOWN:
    default:
      type = e_type_unknown;
      break;
  }
  return Sfixnum(type);
}

/**
 * Return type of a file descriptor.
 *
 * If fstat() is successful, return fd type as a Scheme integer corresponding to enum e_type.
 *
 * On errors, return Scheme integer -errno
 */
static int c_fd_type(int fd) {
  struct stat statbuf;
  const int   err = fstat(fd, &statbuf);
  if (err == 0) {
    int type = c_stat_type(statbuf.st_mode);
    if (type == e_type_chr && isatty(fd)) {
      type = e_type_tty;
    }
    return type;
  }
  return c_errno();
}

/**
 * Delete a file or directory.
 * bytevector0_name must be a 0-terminated bytevector.
 *
 * On success, return 0.
 * On error, return integer -errno
 */
static int c_file_delete(ptr bytevector0_name) {
  if (Sbytevectorp(bytevector0_name)) {
    const char* name = (const char*)Sbytevector_data(bytevector0_name);
    const iptr  len  = Sbytevector_length(bytevector0_name); /* including final '\0' */
    if (len > 0 && name[len - 1] == '\0') {
      if (remove(name) == 0) {
        return 0;
      }
      return c_errno();
    }
  }
  return c_errno_set(EINVAL);
}

/**
 * Move or rename a file or directory.
 * Both bytevector0_old_name and bytevector0_new_name must 0-terminated bytevectors.
 *
 * On success, return 0.
 * On error, return integer -errno
 */
static int c_file_rename(ptr bytevector0_old_name, ptr bytevector0_new_name) {
  const char* old_name;
  const char* new_name;
  iptr        old_len;
  iptr        new_len;
  if (!Sbytevectorp(bytevector0_old_name) || !Sbytevectorp(bytevector0_new_name)) {
    return c_errno_set(EINVAL);
  }
  old_name = (const char*)Sbytevector_data(bytevector0_old_name);
  new_name = (const char*)Sbytevector_data(bytevector0_new_name);
  old_len  = Sbytevector_length(bytevector0_old_name); /* including final '\0' */
  new_len  = Sbytevector_length(bytevector0_new_name); /* including final '\0' */

  if (old_len <= 0 || old_name[old_len - 1] != '\0' || /*                      */
      new_len <= 0 || new_name[new_len - 1] != '\0') {
    return c_errno_set(EINVAL);
  }
  if (rename(old_name, new_name) < 0) {
    return c_errno();
  }
  return 0;
}

/**
 * Convert struct dirent.d_type to Scheme integer:
 *   DT_UNKNOWN -> 0
 *   DT_BLK     -> 1
 *   DT_CHR     -> 2
 *   DT_DIR     -> 3
 *   DT_FIFO    -> 4
 *   DT_REG     -> 5
 *   DT_SOCK    -> 6
 *   DT_LNK     -> 7
 * if keep_symlinks == 0, resolves symlinks i.e. calls fstatat()
 * to resolve DT_LNK and DT_UNKNOWN to the type of the file pointed to.
 */
static ptr c_dirent_type2(DIR*                dir,
                          const char*         filename,
                          const int           keep_symlinks,
                          const unsigned char d_type) {
  if (keep_symlinks == 0 && (d_type == DT_LNK || d_type == DT_UNKNOWN)) {
    struct stat buf;
    if (fstatat(dirfd(dir), filename, &buf, 0) == 0) {
      return c_stat_type_fixnum(buf.st_mode);
    }
  }
  return c_dirent_type(d_type);
}

/**
 * Check existence and type of a filesystem path.
 * bytevector0_path must be a 0-terminated bytevector.
 *
 * If file exists, return its type which is a Scheme integer corresponding to enum e_type.
 * Returns #f if file does not exist.
 *
 * On other errors, return Scheme integer -errno
 */
static ptr c_file_type(ptr bytevector0_path, int keep_symlinks) {
  struct stat buf;
  const char* path;
  iptr        pathlen;
  int         err;
  if (!Sbytevectorp(bytevector0_path)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  path    = (const char*)Sbytevector_data(bytevector0_path);
  pathlen = Sbytevector_length(bytevector0_path); /* including final '\0' */
  if (pathlen <= 0 || path[pathlen - 1] != '\0') {
    return Sinteger(c_errno_set(EINVAL));
  }
  if (keep_symlinks == 0) {
    err = stat(path, &buf);
  } else {
    err = lstat(path, &buf);
  }
  if (err == 0) {
    return c_stat_type_fixnum(buf.st_mode);
  }
  err = errno;
  if (err == ENOENT) {
    errno = 0;
    return Sfalse;
  }
  return Sinteger(-err);
}

typedef enum { o_symlinks = 1, o_append_slash = 2, o_bytes = 4, o_types = 8 } o_dir_options;

typedef struct {
  const char* prefix;
  const char* suffix;
  iptr        prefixlen;
  iptr        suffixlen;
  char        prefix_has_slash;
  char        suffix_has_slash;
  char        keep_symlinks;
  char        ret_append_slash;
  char        ret_bytes;
  char        ret_types;
} s_directory_list_opts;

static ptr
c_directory_list1(DIR* dir, struct dirent* entry, const s_directory_list_opts* opts, ptr ret);

/**
 * Scan directory bytevector0_dirpath and return Scheme list with its contents.
 *
 * If (options & o_types) == 0 each element in returned list is a filename,
 * which is either a Scheme string (if (options & o_string) != 0) or a Scheme bytevector.
 *
 * If (options & o_types) != 0 each element in returned list is a pair (filename . type) where:
 *   filename is either a Scheme string (if (options & o_string) != 0) or a Scheme bytevector.
 *   type is a Scheme integer corresponding to enum e_type.
 *
 * If (options & o_symlinks) == 0, then each type = e_type_lnk will be resolved to indicate
 * the type of the file the symlink points to.
 *
 * If (options & o_append_slash) != 0, then each filename with type = e_type_dir
 * will be modified by appending '/' - useful mostly if (options & o_symlinks) == 0
 *
 * If bytevector_filter_prefix is not empty,
 * only returns filenames that start with bytevector_filter_prefix.
 *
 * If bytevector_filter_suffix is not empty,
 * only returns filenames that end with bytevector_filter_suffix.
 *
 * on error, return Scheme integer -errno
 */
static ptr c_directory_list(ptr bytevector0_dirpath,
                            ptr bytevector_filter_prefix,
                            ptr bytevector_filter_suffix,
                            int options) {
  ptr            ret = Snil;
  const char*    dirpath;
  iptr           dirlen;
  DIR*           dir;
  struct dirent* entry;

  s_directory_list_opts opts;

  if (!Sbytevectorp(bytevector0_dirpath)         /*                 */
      || !Sbytevectorp(bytevector_filter_prefix) /*                 */
      || !Sbytevectorp(bytevector_filter_suffix)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  dirpath        = (const char*)Sbytevector_data(bytevector0_dirpath);
  dirlen         = Sbytevector_length(bytevector0_dirpath); /* including final '\0' */
  opts.prefix    = (const char*)Sbytevector_data(bytevector_filter_prefix);
  opts.prefixlen = Sbytevector_length(bytevector_filter_prefix);
  opts.suffix    = (const char*)Sbytevector_data(bytevector_filter_suffix);
  opts.suffixlen = Sbytevector_length(bytevector_filter_suffix);
  if (opts.prefixlen < 0 || opts.suffixlen < 0 || dirlen <= 0 || dirpath[dirlen - 1] != '\0') {
    return Sinteger(c_errno_set(EINVAL));
  }
  if (opts.prefixlen && opts.prefix[opts.prefixlen - 1] == '/') {
    opts.prefix_has_slash = 1;
    opts.prefixlen--;
  } else {
    opts.prefix_has_slash = 0;
  }
  if (opts.suffixlen && opts.suffix[opts.suffixlen - 1] == '/') {
    opts.suffix_has_slash = 1;
    opts.suffixlen--;
  } else {
    opts.suffix_has_slash = 0;
  }
  opts.keep_symlinks    = (options & o_symlinks) != 0;
  opts.ret_append_slash = (options & o_append_slash) != 0;
  opts.ret_bytes        = (options & o_bytes) != 0;
  opts.ret_types        = (options & o_types) != 0;

  if (!opts.ret_append_slash && (opts.prefix_has_slash || opts.suffix_has_slash)) {
    return ret; /* impossible to satisfy, return nil */
  }
  dir = opendir(dirpath);
  if (!dir) {
    return Sinteger(c_errno());
  }
  while ((entry = readdir(dir)) != NULL) {
    ret = c_directory_list1(dir, entry, &opts, ret);
  }
  (void)closedir(dir);
  return ret;
}

/**
 * called by c_directory_list(): if entry matches opts, add it to ret.
 * returns new ret.
 */
static ptr
c_directory_list1(DIR* dir, struct dirent* entry, const s_directory_list_opts* opts, ptr ret) {

  char*  name    = entry->d_name;
  size_t namelen = strlen(name);
  ptr    type;
  ptr    filename;
  int    name_has_slash;

  if (opts->prefix_has_slash && namelen != (size_t)opts->prefixlen) {
    /*
     * prefix was specified and it ends with '/'
     * => only names containing exactly prefixlen bytes may end with a '/'
     * at the requested position (happens if they are directories)
     */
    return ret;
  }
  if (opts->prefixlen &&
      (namelen < (size_t)opts->prefixlen || memcmp(name, opts->prefix, opts->prefixlen) != 0)) {
    return ret; /* name does not start with prefix, ignore it */
  }
  if (opts->suffixlen &&
      (namelen < (size_t)opts->suffixlen ||
       memcmp(name + namelen - opts->suffixlen, opts->suffix, opts->suffixlen) != 0)) {
    return ret; /* name does not end with suffix, ignore it */
  }
  type           = c_dirent_type2(dir, name, opts->keep_symlinks, entry->d_type);
  name_has_slash = type == Sfixnum(e_type_dir) && opts->ret_append_slash;
  if (name_has_slash) {
    /*
     * relax filter: return name even if suffix does not end with '/'
     * because we want (sh-pattern '*) to list all files
     */
    name[namelen++] = '/'; /* replace final '\0' -> '/' is this portable? */
  } else if (opts->prefix_has_slash || opts->suffix_has_slash) {
    return ret; /* we must only return names that end with '/' */
  }
  filename =
      opts->ret_bytes ? scheme2k_Sbytevector(name, namelen) : scheme2k_Sstring_utf8b(name, namelen);
  ret = Scons(opts->ret_types ? Scons(filename, type) : filename, ret);

  if (name_has_slash) {
    name[--namelen] = '\0'; /* restore final '\0' */
  }
  return ret;
}
