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
/*                            fd-related functions                            */
/*                                                                            */
/******************************************************************************/

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/fd.h should only be #included by posix/posix.c"
#endif

static int c_fd_open_max(void) {
#if defined(OPEN_MAX)
  int fd_n = OPEN_MAX;
#elif defined(_POSIX_OPEN_MAX)
  int fd_n = _POSIX_OPEN_MAX;
#else
  int fd_n = 256; /* reasonable? default */
#endif

#ifdef _SC_OPEN_MAX
  long ret = sysconf(_SC_OPEN_MAX);
  if (ret > 0) {
    if (ret == (long)(int)ret) {
      fd_n = (int)ret;
    } else {
      fd_n = INT_MAX;
    }
  }
#endif

#ifdef __APPLE__
  /* fix issue #33: do not trust high values on macOS, they are not really supported */
  if (fd_n > 32768) {
    fd_n = 32768;
  }
#endif
  return fd_n;
}

/** close specified file descriptor */
static int c_fd_close(int fd) {
  int ret = close(fd);
  return ret >= 0 ? ret : c_errno();
}

/** close a list of file descriptors */
static int c_fd_close_list(ptr fd_list) {
  int ret = 0;
  while (Spairp(fd_list)) {
    ptr elem = Scar(fd_list);
    if (Sfixnump(elem)) {
      if (close(Sfixnum_value(elem)) != 0 && ret == 0) {
        ret = c_errno();
      }
    } else if (ret == 0) {
      ret = -EINVAL;
    }
    fd_list = Scdr(fd_list);
  }
  if (ret == 0 && !Snullp(fd_list)) {
    ret = -EINVAL;
  }
  return ret;
}

/** call dup() */
static int c_fd_dup(int old_fd) {
  int ret = dup(old_fd);
  return ret >= 0 ? ret : c_errno();
}

/** call dup2() */
static int c_fd_dup2(int old_fd, int new_fd) {
  int ret = dup2(old_fd, new_fd);
  return ret >= 0 ? ret : c_errno();
}

/**
 * call fcntl(fd, FD_GETFL)
 * and return > 0 if file descriptor is non-blocking mode,
 * or 0 if if file descriptor is blocking mode.
 * On error return c_errno(), which is < 0
 */
static int c_fd_nonblock_get(int fd) {
  int flags;
  while ((flags = fcntl(fd, F_GETFL)) < 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return flags & O_NONBLOCK;
}

/**
 * call fcntl(fd, FD_SETFL, O_NONBLOCK | fcntl(fd, FD_GETFL))
 * i.e. set file descriptor to non-blocking mode.
 * return 0 on success, or c_errno() on error
 */
static int c_fd_nonblock_set(int fd, int nonblock) {
  int flags;
  while ((flags = fcntl(fd, F_GETFL)) < 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  if (nonblock) {
    flags |= O_NONBLOCK;
  } else {
    flags &= ~O_NONBLOCK;
  }
  while (fcntl(fd, F_SETFL, flags) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

/**
 * call lseek(fd, offset, from) to reposition file offset,
 * and return updated offset in bytes from start of file.
 *
 * returns >= 0 on success, or c_errno() on error
 *
 * allowed 'from' values are:
 *   0 : means SEEK_SET
 *   1 : means SEEK_CUR
 *   2 : means SEEK_END
 */
static int64_t c_fd_seek(int fd, int64_t offset, iptr from) {
  int64_t ret;
  int     whence;
  switch (from) {
    case 0:
      whence = SEEK_SET;
      break;
    case 1:
      whence = SEEK_CUR;
      break;
    case 2:
      whence = SEEK_END;
      break;
    default:
      return c_errno_set(EINVAL);
  }
  while ((ret = lseek(fd, (off_t)offset, whence)) < 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return ret;
}

/**
 * call read().
 * returns number of bytes read, or #t if interrupted, or c_errno() < 0 on error
 */
static ptr c_fd_read(int fd, ptr bytevector_read, iptr start, iptr end) {
  char*   buf;
  iptr    len;
  ssize_t got_n;
  if (start < 0 || end < 0 || start > end || !Sbytevectorp(bytevector_read)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  buf = (char*)Sbytevector_data(bytevector_read);
  len = Sbytevector_length(bytevector_read);
  if (end > len) {
    return Sinteger(c_errno_set(EINVAL));
  }
  buf += start;
  len = end - start;
  C_DEBUG_WRITE(1, "-> c_fd_read\n");
  if ((got_n = read(fd, buf, len)) < 0 && errno == EINTR) {
    C_DEBUG_WRITE(1, "<- c_fd_read ret = EINTR\n");
    return Strue;
  }
  C_DEBUG_WRITE(1, "<- c_fd_read ret >= 0\n");
  return Sinteger(got_n >= 0 ? got_n : c_errno());
}

/**
 * call read(), retrieve a single byte and return it as a fixnum in [0, 255]
 * returns #t if interrupted, or #f on eof, or c_errno() < 0 on error
 */
static ptr c_fd_read_u8(int fd) {
  char    buf[1];
  ssize_t n = read(fd, buf, sizeof(buf));
  int     err;
  switch (n) {
    case 1:
      /* char may be a signed type */
      return Sfixnum((unsigned)(unsigned char)buf[0]);
    case 0:
      return Sfalse; /* EOF */
    default:
      break;
  }
  err = c_errno();
  if (err == -EINTR) {
    return Strue; /* interrupted */
  }
  return Sinteger(err);
}

/**
 * call write().
 * returns number of bytes written, or #t if interrupted, or c_errno() < 0 on error
 */
static ptr c_fd_write(int fd, ptr bytevector_towrite, iptr start, iptr end) {
  const char* buf;
  iptr        len;
  ssize_t     sent_n;
  if (start < 0 || end < 0 || start > end || !Sbytevectorp(bytevector_towrite)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  buf = (const char*)Sbytevector_data(bytevector_towrite);
  len = Sbytevector_length(bytevector_towrite);
  if (end > len) {
    return Sinteger(c_errno_set(EINVAL));
  }
  buf += start;
  len = end - start;
  if ((sent_n = write(fd, buf, len)) < 0 && errno == EINTR) {
    return Strue; /* interrupted */
  }
  return Sinteger(sent_n >= 0 ? sent_n : c_errno());
}

/**
 * call write() and send a single byte.
 * return 0 if success, or #t if interrupted, or c_errno() < 0 on error
 */
static ptr c_fd_write_u8(int fd, int byte) {
  char buf[1];
  int  err;

  buf[0] = (char)byte;
  if (write(fd, buf, sizeof(buf)) == 1) {
    return Sfixnum(0);
  }
  err = c_errno();
  if (err == -EINTR) {
    return Strue; /* interrupted */
  }
  return Sinteger(err);
}

enum read_write_mask {
  mask_READ  = 1,
  mask_WRITE = 2,
  mask_ERR   = 4,
};

/**
 * call select() or poll() on file descriptor.
 * Returns rw_mask of operations available on file descriptor,
 * or c_errno() < 0 on error - which may also be EINTR.
 *
 * argument rw_mask and return value are a bitwise-or of:
 *   1 => fd is readable
 *   2 => fd is writable
 *   4 => fd is in error (only in return value)
 */
static int c_fd_select(int fd, int rw_mask, int timeout_milliseconds) {
  struct pollfd entry;
  entry.fd     = fd;
  entry.events = (rw_mask & mask_READ ? POLLIN : 0) | /*                                         */
                 (rw_mask & mask_WRITE ? POLLOUT : 0);
  entry.revents = 0;
  if (poll(&entry, 1, timeout_milliseconds) < 0) {
    /** do NOT retry on EINTR, return it instead */
    return c_errno();
  }
  return (entry.revents & (POLLIN | POLLHUP) ? mask_READ : 0) | /*                               */
         (entry.revents & POLLOUT ? mask_WRITE : 0) | /*                                         */
         (entry.revents & (POLLERR | POLLNVAL) ? mask_ERR : 0);
}

/**
 * call open() and return fd of newly opened file, or c_errno() on error
 * flag_read_write can be one of:
 *   0 => open readonly
 *   1 => open writeonly
 *   2 => open readwrite
 */
static int c_file_fd(ptr bytevector0_filepath,
                     int flag_read_write,
                     int flag_create,
                     int flag_truncate,
                     int flag_append) {
  const char* filepath;
  iptr        len;
  int         flags, ret;
  if (!Sbytevectorp(bytevector0_filepath)) {
    return c_errno_set(EINVAL);
  }
  filepath = (const char*)Sbytevector_data(bytevector0_filepath);
  len      = Sbytevector_length(bytevector0_filepath);
  if (len <= 0 || filepath[len - 1] != '\0') {
    return c_errno_set(EINVAL);
  }
  flags = (flag_read_write == 0 ? O_RDONLY :
           flag_read_write == 1 ? O_WRONLY :
                                  O_RDWR) |    /*                */
          (flag_create == 0 ? 0 : O_CREAT) |   /*                */
          (flag_truncate == 0 ? 0 : O_TRUNC) | /*                */
          (flag_append == 0 ? 0 : O_APPEND);   /*                */

  ret = open(filepath, flags, 0666);
  return ret >= 0 ? ret : c_errno();
}

/** call pipe() and return a Scheme cons (pipe_read_fd . pipe_write_fd), or c_errno() on error */
static ptr c_pipe_fds(ptr read_fd_close_on_exec, ptr write_fd_close_on_exec) {
  int fds[2];
  int err = pipe(fds);
  if (err < 0) {
    return Sinteger(c_errno());
  }
  if (read_fd_close_on_exec != Sfalse) {
    err = fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0 && write_fd_close_on_exec != Sfalse) {
    err = fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0) {
    return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
  }
  err = c_errno();
  (void)close(fds[0]);
  (void)close(fds[1]);
  return Sinteger(err);
}

/* convert a redirection char < > ≶ (means <>) » (means >>) to open() flags */
static int c_direction_to_open_flags(string_char ch) {
  switch (ch) {
    case '<':
      return O_RDONLY;
    case '>': /* write to file. truncate file if exists, otherwise create it */
      return O_WRONLY | O_CREAT | O_TRUNC;
    case 0x00bb: /* » means >> i.e. append */
      return O_WRONLY | O_CREAT | O_APPEND;
    case 0x2276: /* ≶ means <> i.e. read/write from start of file and do NOT truncate */
      return O_RDWR | O_CREAT;
    default:
      return -1;
  }
}

static int dup2_close_on_exec(int old_fd, int new_fd, ptr close_on_exec) {
  int err = dup2(old_fd, new_fd);
  if (err < 0) {
    err = c_errno();
  } else if (close_on_exec != Sfalse) {
    err = fcntl(new_fd, F_SETFD, FD_CLOEXEC);
    if (err < 0) {
      err = c_errno();
      (void)close(new_fd);
    }
  }
  return err;
}

/** redirect a single fd as specified */
static int
c_fd_redirect(ptr from_fd, ptr direction_ch, ptr to_fd_or_bytevector, ptr close_on_exec) {
  iptr        path_len = 0;
  iptr        ifd;
  const char* path = NULL;
  int         fd;
  int         open_flags;
  int         err = 0;

  if (!Sfixnump(from_fd) || (ifd = Sfixnum_value(from_fd)) < 0 || ifd != (iptr)(fd = (int)ifd)) {
    /* invalid fd */
    return write_invalid_redirection("from_fd", from_fd);
  } else if (!Scharp(direction_ch) ||
             (open_flags = c_direction_to_open_flags(Schar_value(direction_ch))) < 0) {
    /* invalid direction */
    return write_invalid_redirection("direction_ch", direction_ch);
  } else if (Sfixnump(to_fd_or_bytevector)) {
    /* redirect fd to another fd */
    iptr to_fd = Sfixnum_value(to_fd_or_bytevector);
    if (to_fd < -1 || to_fd != (iptr)(int)to_fd) {
      /* invalid to_fd, must be in the range [-1, INT_MAX] */
      return write_invalid_redirection("to_fd_or_bytevector", to_fd_or_bytevector);
      /* redirect fd to another file descriptor, or close it */
    } else if (to_fd == -1) {
      (void)close(fd);
      return 0;
    } else if ((err = dup2_close_on_exec((int)to_fd, fd, close_on_exec) < 0)) {
      return write_c_errno(err);
    }
    return 0;
  } else if (Sbytevectorp(to_fd_or_bytevector) &&
             (path_len = Sbytevector_length(to_fd_or_bytevector)) > 0 &&
             Sbytevector_u8_ref(to_fd_or_bytevector, path_len - 1) == 0) {
    /* redirect fd from/to a file */
    int temp_fd;
    path = (const char*)Sbytevector_data(to_fd_or_bytevector);

#ifdef O_CLOEXEC
    temp_fd = open(path, open_flags | (close_on_exec != Sfalse ? O_CLOEXEC : 0), 0666);
#else
    temp_fd = open(path, open_flags, 0666);
#endif

    if (temp_fd < 0) {
      return write_path_c_errno(path, path_len - 1, c_errno(), "\n");
    } else if (temp_fd == fd) {
#ifndef O_CLOEXEC
      if (close_on_exec != Sfalse) {
        err = fcntl(temp_fd, F_SETFD, FD_CLOEXEC);
        if (err < 0) {
          (void)close(temp_fd);
        }
      }
#endif
    } else {
      err = dup2_close_on_exec(temp_fd, fd, close_on_exec);
      (void)close(temp_fd);
    }
    if (err < 0) {
      return write_path_c_errno(path, path_len - 1, err, "\n");
    }
    return 0;
  } else {
    /* invalid path */
    return write_invalid_redirection("to_fd_or_bytevector", to_fd_or_bytevector);
  }
}

/** redirect a single fd as indicated in vector_fds_redirect[i...i+3]. return < 0 on error */
static int c_fds_redirect_i(ptr vector_fds_redirect, iptr i, ptr close_on_exec) {
  /* element at i + 3 contains to_bytevector0 or Sfalse  */
  ptr to = Svector_ref(vector_fds_redirect, i + 3);
  if (to == Sfalse) {
    /* element at i + 2 contains to_fd, string, or closure  */
    to = Svector_ref(vector_fds_redirect, i + 2);
  }
  return c_fd_redirect(Svector_ref(vector_fds_redirect, i + 0),
                       Svector_ref(vector_fds_redirect, i + 1),
                       to,
                       close_on_exec);
}

/** redirect fds as indicated in vector_fds_redirect. return < 0 on error */
static int c_fds_redirect(ptr vector_fds_redirect, ptr close_on_exec) {
  iptr i, n;
  int  err = 0;
  if (!Svectorp(vector_fds_redirect) || ((n = Svector_length(vector_fds_redirect)) & 3)) {
    write_invalid_redirection("vector_fds_redirect", vector_fds_redirect);
    return -EINVAL;
  }
  for (i = 0; err == 0 && i + 4 <= n; i += 4) {
    err = c_fds_redirect_i(vector_fds_redirect, i, close_on_exec);
  }
  return err;
}
