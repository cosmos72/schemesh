/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#define _POSIX_C_SOURCE 200809L /* fstatat() */
#define _DEFAULT_SOURCE         /* DT_* */
#define _BSD_SOURCE             /* DT_* */

#include "posix.h"
#include "../containers/containers.h" /* schemesh_Sbytevector() */
#include "../eval.h"                  /* eval() */
#include "signal.h"

#include <dirent.h> /* opendir(), readdir(), closedir() */
#include <errno.h>  /* EINVAL, EIO, errno */
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pwd.h>    /* getpwnam_r() */
#include <signal.h> /* kill() ... */
#include <stdio.h>
#include <stdlib.h> /* getenv(), strtoul() */
#include <string.h>
#include <string.h>    /* strlen() */
#include <sys/ioctl.h> /* ioctl(), TIOCGWINSZ */
#include <sys/stat.h>  /* fstatat() */
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h> /* sysconf(), write() */
#ifdef __linux__
#define SCHEMESH_USE_TTY_IOCTL
#else
#undef SCHEMESH_USE_TTY_IOCTL
#endif

#ifdef SCHEMESH_USE_TTY_IOCTL
#include <asm/termbits.h> /* struct termios, incompatible with <termios.h> */
#else
#include <termios.h> /* struct termios, tcgetattr(), tcsetattr() */
#endif

static int c_fd_open_max(void);

/******************************************************************************/
/*                                                                            */
/*                          errno-related functions                           */
/*                                                                            */
/******************************************************************************/

int c_errno(void) {
  return -errno;
}

int c_errno_set(int errno_value) {
  return -(errno = errno_value);
}

static int c_errno_eio(void) {
  return -EIO;
}

static int c_errno_eintr(void) {
  return -EINTR;
}

static int c_errno_einval(void) {
  return -EINVAL;
}

ptr c_strerror(int err) {
  const char* msg = strerror(err < 0 ? -err : err);
  return schemesh_Sstring_utf8b(msg, strlen(msg));
}

int c_init_failed(const char label[]) {
  const int err = errno;
  fprintf(stderr,
          "error initializing POSIX subsystem: %s failed with error %s\n",
          label,
          strerror(err));
  return -err;
}

static int write_c_errno(void) {
  const char* errmsg;
  const int   err = errno;
  /* writev() is less portable */
  (void)write(2, "schemesh: ", 10);
  errmsg = strerror(err);
  (void)write(2, errmsg, strlen(errmsg));
  (void)write(2, "\n", 1);
  return -err;
}

static int write_path_c_errno(const char path[], const size_t path_len) {
  const char* errmsg;
  const int   err = errno;
  (void)write(2, "schemesh: ", 10);
  (void)write(2, path, path_len);
  (void)write(2, ": ", 2);
  errmsg = strerror(err);
  (void)write(2, errmsg, strlen(errmsg));
  return -err;
}

static int write_command_not_found(const char arg0[]) {
  (void)write(2, "schemesh: ", 10);
  (void)write(2, arg0, strlen(arg0));
  (void)write(2, ": command not found\n", 20);
  return -EINVAL;
}

static char to_hex_digit(unsigned number) {
  number &= 0xF;
  return number < 10 ? '0' + number : 'A' + (number - 10);
}

static unsigned ptr_to_hex(ptr p, char outbuf[/* sizeof(ptr) * 2 */]) {
  const unsigned max_digits = sizeof(ptr) * 2;
  char*          out        = outbuf + max_digits;
  uptr           x          = (uptr)p;
  do {
    *--out = to_hex_digit(x);
  } while ((x >>= 4) != 0);
  return out - outbuf;
}

static int write_invalid_redirection(const char label[], ptr value) {
  (void)write(2, "schemesh: invalid redirection ", 30);
  (void)write(2, label, strlen(label));
  (void)write(2, " 0x", 3);

  char     buf[sizeof(ptr) * 2 + 1];
  unsigned skip        = ptr_to_hex(value, buf);
  buf[sizeof(buf) - 1] = '\n';
  (void)write(2, buf + skip, sizeof(buf) - skip);

  return -EINVAL;
}

/******************************************************************************/
/*                                                                            */
/*                    current-directory-related functions                     */
/*                                                                            */
/******************************************************************************/

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
      return schemesh_Sstring_utf8b(dir, strlen(dir));
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
        ptr ret = schemesh_Sstring_utf8b(dir, strlen(dir));
        free(dir);
        return ret;
      }
      free(dir);
      maxlen *= 2;
    }
  }
  return Smake_string(0, 0);
}

/******************************************************************************/
/*                                                                            */
/*                           tty-related functions                            */
/*                                                                            */
/******************************************************************************/

/** close-on-exec file descriptor for our tty */
static int tty_fd = -1;

static int c_tty_init(void) {
  int fd  = c_fd_open_max() - 1;
  int err = 0;
  if (dup2(0, fd) < 0) {
    err = c_init_failed("dup2(0, tty_fd)");
  } else if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
    err = c_init_failed("fcntl(tty_fd, F_SETFD, FD_CLOEXEC)");
  } else {
    tty_fd = fd;
  }
  return err;
}

/** return file descriptor for our controlling tty */
int c_tty_fd(void) {
  return tty_fd;
}

static int c_tty_getattr(int fd, struct termios* conf) {
#ifdef SCHEMESH_USE_TTY_IOCTL
  return ioctl(fd, TCGETS, conf);
#else
  return tcgetattr(fd, conf);
#endif
}

static int c_tty_setattr(int fd, const struct termios* conf) {
#ifdef SCHEMESH_USE_TTY_IOCTL
  return ioctl(fd, TCSETSW, conf);
#else
  return tcsetattr(fd, TCSADRAIN, conf);
#endif
}

static struct termios saved_conf;
static int            have_saved_conf = 0;

/** restore controlling tty to saved config */
static int c_tty_restore(void) {
  /* (void)write(1, "; c_tty_restore\r\n", 17); */
  if (have_saved_conf) {
    while (c_tty_setattr(tty_fd, &saved_conf) != 0) {
      if (errno != EINTR) {
        return c_errno();
      }
    }
  }
  return 0;
}

/** save controlling tty config, then set it to raw mode */
static int c_tty_setraw(void) {
  struct termios conf;
  size_t         i;
  /* (void)write(1, "; c_tty_setraw\r\n", 16); */

  if (!have_saved_conf) {
    while (c_tty_getattr(tty_fd, &saved_conf) != 0) {
      if (errno != EINTR) {
        return c_errno();
      }
    }
    have_saved_conf = 1;
  }
  conf = saved_conf;
  conf.c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR | ISTRIP | IXOFF | IXON | PARMRK);
  conf.c_oflag |= OPOST | ONLCR;
  conf.c_cflag &= ~(CSIZE | PARENB);
  conf.c_cflag |= CS8;
  conf.c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
  /* conf.c_lflag |= TOSTOP; */
  for (i = 0; i < NCCS; i++) {
    conf.c_cc[i] = 0;
  }
  conf.c_cc[VMIN] = 1;
  while (c_tty_setattr(tty_fd, &conf) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

static unsigned long c_parse_unsigned_long(const char* str);

/** return a cons (width . height), or c_errno() on error */
static ptr c_tty_size(void) {
  unsigned long width = 0, height = 0;
  int           err = 0;
#ifdef TIOCGWINSZ
  {
    struct winsize wsize;
    while ((err = ioctl(tty_fd, TIOCGWINSZ, &wsize)) != 0 && errno == EINTR) {
    }
    if (err != 0) {
      /* save ioctl() error */
      err = c_errno();
    } else if (wsize.ws_col > 0 && wsize.ws_row > 0) {
      width  = wsize.ws_col;
      height = wsize.ws_row;
    }
  }
#endif /* TIOCGWINSZ */
  if (width == 0) {
    width = c_parse_unsigned_long(getenv("COLUMNS"));
  }
  if (height == 0) {
    width = c_parse_unsigned_long(getenv("LINES"));
  }
  if (width != 0 && height != 0) {
    return Scons(Sunsigned(width), Sunsigned(height));
  }
  if (err == 0) {
    err = c_errno_set(EINVAL);
  }
  return Sinteger(err);
}

static unsigned long c_parse_unsigned_long(const char* str) {
  if (str != NULL) {
    char*         end = NULL;
    unsigned long n   = strtoul(str, &end, 10);
    if (*end == '\0') {
      return n;
    }
  }
  return 0;
}

/******************************************************************************/
/*                                                                            */
/*                            fd-related functions                            */
/*                                                                            */
/******************************************************************************/

static int c_fd_open_max(void) {
#ifdef _SC_OPEN_MAX
  long ret = sysconf(_SC_OPEN_MAX);
  if (ret > 0) {
    if (ret == (long)(int)ret) {
      return (int)ret;
    }
    return INT_MAX;
  }
#endif
#if defined(OPEN_MAX)
  return OPEN_MAX;
#elif defined(_POSIX_OPEN_MAX)
  return _POSIX_OPEN_MAX;
#else
  return 256; /* reasonable? default */
#endif
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
 * call fcntl(fd, FD_SETFL, O_NONBLOCK | fcntl(fd, FD_GETFL))
 * to set file descriptor to non-blocking mode.
 * returns >= 0 on success, or c_errno() on error
 */
static int c_fd_setnonblock(int fd) {
  int flags;
  while ((flags = fcntl(fd, F_GETFL)) < 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  flags |= O_NONBLOCK;
  while (fcntl(fd, F_SETFL, flags) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

/** call read(). returns number of bytes read, or c_errno() < 0 on error */
static iptr c_fd_read(int fd, ptr bytevector_read, iptr start, iptr end) {
  char*   buf;
  iptr    len;
  ssize_t got_n;
  if (start < 0 || end < 0 || start > end || !Sbytevectorp(bytevector_read)) {
    return c_errno_set(EINVAL);
  }
  buf = (char*)Sbytevector_data(bytevector_read);
  len = Sbytevector_length(bytevector_read);
  if (end > len) {
    return c_errno_set(EINVAL);
  }
  buf += start;
  len = end - start;
  while ((got_n = read(fd, buf, len)) < 0 && errno == EINTR) {
  }
  return got_n >= 0 ? got_n : c_errno();
}

/** call write(). returns number of bytes written, or c_errno() < 0 on error */
static iptr c_fd_write(int fd, ptr bytevector_towrite, iptr start, iptr end) {
  const char* buf;
  iptr        len;
  ssize_t     sent_n;
  if (start < 0 || end < 0 || start > end || !Sbytevectorp(bytevector_towrite)) {
    return c_errno_set(EINVAL);
  }
  buf = (const char*)Sbytevector_data(bytevector_towrite);
  len = Sbytevector_length(bytevector_towrite);
  if (end > len) {
    return c_errno_set(EINVAL);
  }
  buf += start;
  len = end - start;
  while ((sent_n = write(fd, buf, len)) < 0 && errno == EINTR) {
  }
  return sent_n >= 0 ? sent_n : c_errno();
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
static int c_open_file_fd(ptr bytevector0_filepath,
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
static ptr c_open_pipe_fds(ptr read_fd_close_on_exec, ptr write_fd_close_on_exec) {
  int fds[2];
  int ret = pipe(fds);
  if (ret < 0) {
    return Sinteger(c_errno());
  }
  int err = 0;
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
  if (err >= 0 && close_on_exec != Sfalse) {
    err = fcntl(new_fd, F_SETFD, FD_CLOEXEC);
    if (err < 0) {
      (void)close(new_fd);
    }
  }
  return err;
}

/** redirect a single fd as specified */
static int
c_fd_redirect(ptr from_fd, ptr direction_ch, ptr to_fd_or_bytevector, ptr close_on_exec) {
  iptr        fd;
  iptr        path_len = 0;
  const char* path     = NULL;
  int         open_flags;
  int         err = 0;

  if (!Sfixnump(from_fd) || (fd = Sfixnum_value(from_fd)) < 0 || fd != (iptr)(int)fd) {
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
    } else if (dup2_close_on_exec((int)to_fd, (int)fd, close_on_exec) < 0) {
      return write_c_errno();
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
      return write_path_c_errno(path, path_len - 1);
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
      err = dup2_close_on_exec(temp_fd, (int)fd, close_on_exec);
      (void)close(temp_fd);
    }
    if (err < 0) {
      return write_path_c_errno(path, path_len - 1);
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

/******************************************************************************/
/*                                                                            */
/*                           pid-related functions                            */
/*                                                                            */
/******************************************************************************/

/**
 * call kill(pid, sig) i.e. send signal number sig to specified process id.
 * Notes:
 * pid ==  0 means "all processes in the same process group as the caller".
 * pid == -1 means "all processes".
 * pid <  -1 means "all processes in process group -pid"
 *
 * Return 0 on success, otherwise return c_errno()
 */
static int c_pid_kill(int pid, int sig) {
  return kill(pid, sig) >= 0 ? 0 : c_errno();
}

static int c_exit(int status) {
  /* printf("c_exit(%d) invoked\n", status); */
  exit(status);
  return -EINVAL;
}

/** return Scheme string, or Scheme integer on error */
static ptr c_get_hostname(void) {
  char buf[HOST_NAME_MAX + 1];
  if (gethostname(buf, sizeof(buf)) != 0) {
    return Sinteger(c_errno());
  }
  return schemesh_Sstring_utf8b(buf, strlen(buf));
}

/**
 * get home directory of specified username, which must be a 0-terminated bytevector.
 * return Scheme string, or Scheme integer on error
 */
ptr c_get_userhome(ptr username0) {
  struct passwd  pwd;
  struct passwd* result = NULL;
  ptr            ret;
  char*          buf;
  const char*    username_chars;
  iptr           username_len = 0;
  long           bufsize      = -1;
  int            err;

  if (!Sbytevectorp(username0) || (username_len = Sbytevector_length(username0)) == 0) {
    return Sinteger(c_errno_set(EINVAL));
  }
  username_chars = (const char*)Sbytevector_data(username0);
  if (username_chars[username_len - 1] != 0) {
    return Sinteger(c_errno_set(EINVAL));
  }

#ifdef _SC_GETPW_R_SIZE_MAX
  bufsize = sysconf(_SC_GETPW_R_SIZE_MAX);
#endif
  if (bufsize < 0) {
    bufsize = 16384;
  }
  buf = malloc(bufsize);
  if (!buf) {
    return Sinteger(c_errno());
  }
  err = getpwnam_r(username_chars, &pwd, buf, bufsize, &result);
  if (err == 0 && result && result->pw_dir) {
    ret = schemesh_Sstring_utf8b(result->pw_dir, strlen(result->pw_dir));
  } else {
    ret = Sinteger(c_errno_set(err != 0 ? err : ENOENT));
  }
  free(buf);
  return ret;
}

typedef enum {
  e_unknown  = 0,
  e_blockdev = 1,
  e_chardev  = 2,
  e_dir      = 3,
  e_fifo     = 4,
  e_file     = 5,
  e_socket   = 6,
  e_symlink  = 7,
} e_type;

/**
 * Convert (struct stat.st_mode & S_IFMT) to Scheme integer:
 *   S_IFBLK    -> e_blockdev = 1
 *   S_IFCHR    -> e_chardev  = 2
 *   S_IFDIR    -> e_dir      = 3
 *   S_IFIFO    -> e_fifo     = 4
 *   S_IFREG    -> e_file     = 5
 *   S_IFSOCK   -> e_socket   = 6
 *   S_IFLNK    -> e_symlink  = 7 - can only happen if called with lstat() result
 *   otherwise  -> e_unknown  = 0
 */
static ptr c_stat_type(const mode_t s_type) {
  e_type type;
  switch (s_type) {
    case S_IFBLK:
      type = e_blockdev;
      break;
    case S_IFCHR:
      type = e_chardev;
      break;
    case S_IFDIR:
      type = e_dir;
      break;
    case S_IFIFO:
      type = e_fifo;
      break;
    case S_IFREG:
      type = e_file;
      break;
    case S_IFSOCK:
      type = e_socket;
      break;
    case S_IFLNK:
      type = e_symlink;
      break;
    default:
      type = e_unknown;
      break;
  }
  return Sfixnum(type);
}

/**
 * Convert struct dirent.d_type to Scheme integer:
 *   DT_UNKNOWN -> e_unknown  = 0
 *   DT_BLK     -> e_blockdev = 1
 *   DT_CHR     -> e_chardev  = 2
 *   DT_DIR     -> e_dir      = 3
 *   DT_FIFO    -> e_fifo     = 4
 *   DT_REG     -> e_file     = 5
 *   DT_SOCK    -> e_socket   = 6
 *   DT_LNK     -> e_symlink  = 7
 */
static ptr c_dirent_type(unsigned char d_type) {
  e_type type;
  switch (d_type) {
    case DT_BLK:
      type = e_blockdev;
      break;
    case DT_CHR:
      type = e_chardev;
      break;
    case DT_DIR:
      type = e_dir;
      break;
    case DT_FIFO:
      type = e_fifo;
      break;
    case DT_LNK:
      type = e_symlink;
      break;
    case DT_REG:
      type = e_file;
      break;
    case DT_SOCK:
      type = e_socket;
      break;
    case DT_UNKNOWN:
    default:
      type = e_unknown;
      break;
  }
  return Sfixnum(type);
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
      return c_stat_type(buf.st_mode & S_IFMT);
    }
  }
  return c_dirent_type(d_type);
}

/*
 * Check existence and type of a filesystem path.
 * bytevector0_path must be a 0-terminated bytevector.
 *
 * If file exists, return its type which is a Scheme integer corresponding to enum e_type.
 * Returns #f if file does not exist.
 *
 * On other errors, return Scheme integer -errno
 */
static ptr c_file_stat(ptr bytevector0_path, int keep_symlinks) {
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
    return c_stat_type(buf.st_mode & S_IFMT);
  }
  err = errno;
  if (err == ENOENT) {
    errno = 0;
    return Sfalse;
  }
  return Sinteger(-err);
}

typedef enum { o_symlinks = 1, o_append_slash = 2, o_strings = 4 } o_dir_options;

typedef struct {
  const char* prefix;
  const char* suffix;
  iptr        prefixlen;
  iptr        suffixlen;
  int         prefix_has_slash;
  int         suffix_has_slash;
  int         keep_symlinks;
  int         ret_append_slash;
  int         ret_strings;
} s_directory_list_opts;

static ptr
c_directory_list1(DIR* dir, struct dirent* entry, const s_directory_list_opts* opts, ptr ret);

/**
 * Scan directory bytevector0_dirpath and return Scheme list with its contents as pairs
 * (type . filename) where:
 *   filename is either a Scheme string (if (options & o_string) != 0) or a Scheme bytevector.
 *   type is a Scheme integer corresponding to enum e_type.
 *
 * If (options & o_symlinks) == 0, then each type = e_symlink will be resolved to indicate
 * the type of the file the symlink points to.
 *
 * If (options & o_append_slash) != 0, then each filename with type = e_dir
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
  opts.keep_symlinks    = options & o_symlinks;
  opts.ret_append_slash = options & o_append_slash;
  opts.ret_strings      = options & o_strings;

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
  name_has_slash = type == Sfixnum(e_dir) && opts->ret_append_slash;
  if (name_has_slash) {
    /*
     * relax filter: return name even if suffix does not end with '/'
     * because we want (sh-pattern '*) to list all files
     */
    name[namelen++] = '/'; /* replace final '\0' -> '/' is this portable? */
  } else if (opts->prefix_has_slash || opts->suffix_has_slash) {
    return ret; /* we must only return names that end with '/' */
  }
  filename = opts->ret_strings ? schemesh_Sstring_utf8b(name, namelen) :
                                 schemesh_Sbytevector(name, namelen);
  ret      = Scons(Scons(type, filename), ret);

  if (name_has_slash) {
    name[--namelen] = '\0'; /* restore final '\0' */
  }
  return ret;
}

/** return pid of current process, or c_errno() on error */
static int c_pid_get(void) {
  int pid = getpid();
  return pid >= 0 ? pid : c_errno();
}

/** return process group of specified process (0 = current process), or c_errno() on error */
static int c_pgid_get(int pid) {
  int pgid = getpgid((pid_t)pid);
  return pgid >= 0 ? pgid : c_errno();
}

/**
 * set process group id (i.e. pgid) of current process:
 * if existing_pgid > 0:
 *   move current process into process group indicated by existing_pgid.
 *
 * if existing_pgid == 0 or == -1:
 *   create a new process group with pgid == current process pid,
 *   and move current process into it.
 *
 * if existing_pgid < -1:
 *   do nothing.
 */
static int c_pgid_set(int existing_pgid) {
  int err = 0;
  if (existing_pgid >= -1) {
    err = setpgid(0 /*current process*/, existing_pgid > 0 ? (pid_t)existing_pgid : (pid_t)0);
  }
  return err >= 0 ? err : c_errno();
}

/**
 * fork() and redirect file descriptors.
 * parent: return pid, or c_errno() on error
 * child: return 0, or c_errno() on error
 */
static int c_fork_pid(ptr vector_fds_redirect, int existing_pgid) {
  const int pid = fork();
  switch (pid) {
    case -1:
      return c_errno(); /* fork() failed */
    case 0: {
      /* child */
      int err;
      if ((err = c_pgid_set(existing_pgid)) >= 0 && (err = c_signal_setdefault(SIGTSTP)) >= 0) {
        err = c_fds_redirect(vector_fds_redirect, Sfalse);
      }
      return err;
    }
    default:
#ifdef SCHEMESH_DEBUG_POSIX
      fprintf(stdout, "c_fork_pid %d -> %d\n", (int)getpid(), pid);
      fflush(stdout);
#endif
      return pid;
  }
}

/**
 * convert Scheme vector-of-bytevector0 to a C-compatible NULL-terminated span of char*
 * usable for example for environ or argz arguments to execve() execvp() etc.
 * returned span should be deallocated with free()
 * and contains pointers into Scheme bytevectors, thus becomes invalid
 * after any call to Scheme functions.
 */
static char** vector_to_c_argz(ptr vector_of_bytevector0);

/** optionally fork(), then exec() an external program.
 * if forked, return pid in parent process.
 * if existing_pgid > 0, add process to given pgid i.e. process group */
static int c_cmd_spawn_or_exec(ptr vector_of_bytevector0_cmdline,
                               ptr bytevector0_chdir_or_false,
                               ptr vector_fds_redirect,
                               ptr vector_of_bytevector0_environ,
                               int existing_pgid,
                               int is_spawn) {

  char** argv = vector_to_c_argz(vector_of_bytevector0_cmdline);
  char** envp = vector_to_c_argz(vector_of_bytevector0_environ);
  int    pid;
  if (!argv || (!envp && Svectorp(vector_of_bytevector0_environ))) {
    pid = -ENOMEM;
    goto out;
  }
  if (!argv[0]) {
    pid = -EINVAL;
    goto out;
  }
  if (bytevector0_chdir_or_false != Sfalse) {
    const octet* dir;
    iptr         dir_len;
    if (!Sbytevectorp(bytevector0_chdir_or_false)) {
      pid = -EINVAL;
      goto out;
    }
    dir     = Sbytevector_data(bytevector0_chdir_or_false);
    dir_len = Sbytevector_length(bytevector0_chdir_or_false);
    if (dir_len <= 0 || dir[dir_len - 1] != 0) {
      pid = -EINVAL;
      goto out;
    }
  }

#ifdef SCHEMESH_DEBUG_POSIX
  fprintf(stdout, "c_cmd_spawn %s ...\n", argv[0]);
  fflush(stdout);
#endif
  if (is_spawn) {
    pid = fork();
  } else {
    pid = 0; /* pretend we are already in the child */
  }
  switch (pid) {
    case -1:
      /* error */
      pid = c_errno();
      break;
    case 0: {
      /* child */
      if (c_pgid_set(existing_pgid) >= 0 &&
          c_signals_setdefault() >= 0 && /*                                   */
          (bytevector0_chdir_or_false == Sfalse || c_chdir(bytevector0_chdir_or_false) >= 0) &&
          c_fds_redirect(vector_fds_redirect, Sfalse) >= 0) {
        if (envp) {
          environ = envp;
        }
        if (strchr(argv[0], '/')) {
          (void)execv(argv[0], argv);
        } else {
          (void)execvp(argv[0], argv);
        }
        /* in case or execv...() failed and returned */
        (void)write_command_not_found(argv[0]);
        exit(127);
      }
      /* in case c_pgid_set() or c_chdir() or c_fds_redirect() fail */
      exit(1);
    }
    default:
      /* parent */
      break;
  }
out:
  free(argv);
  free(envp);
  if (pid < 0) {
    c_errno_set(-pid);
  }
  return pid;
}

/**
 * exec() an external program. Returns only if failed.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 */
static int c_cmd_exec(ptr vector_of_bytevector0_cmdline,
                      ptr bytevector0_chdir_or_false,
                      ptr vector_fds_redirect,
                      ptr vector_of_bytevector0_environ) {

  return c_cmd_spawn_or_exec(vector_of_bytevector0_cmdline,
                             bytevector0_chdir_or_false,
                             vector_fds_redirect,
                             vector_of_bytevector0_environ,
                             -2, /* do not set pgid */
                             0); /* is_spawn */
}

/**
 * fork() and exec() an external program, return pid.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0 or -1, create new process group
 */
static int c_cmd_spawn(ptr vector_of_bytevector0_cmdline,
                       ptr bytevector0_chdir_or_false,
                       ptr vector_fds_redirect,
                       ptr vector_of_bytevector0_environ,
                       int existing_pgid) {

  return c_cmd_spawn_or_exec(vector_of_bytevector0_cmdline,
                             bytevector0_chdir_or_false,
                             vector_fds_redirect,
                             vector_of_bytevector0_environ,
                             existing_pgid,
                             1); /* is_spawn */
}

/**
 * if expected_pgid i.e. process group id is the foreground process group,
 * then set new_pgid as the foreground process group.
 */
static int c_pgid_foreground(int expected_pgid, int new_pgid) {
  int actual_pgid;
  if (expected_pgid == new_pgid) {
    return 0; /* nothing to do */
  }
  actual_pgid = tcgetpgrp(tty_fd);
  if (actual_pgid < 0) {
    return c_errno();
  } else if (actual_pgid != expected_pgid) {
    return 0; /* fg process group is not the expected one: do nothing */
  }
  return tcsetpgrp(tty_fd, new_pgid) >= 0 ? 0 : c_errno();
}

/**
 * call waitpid(pid, WUNTRACED) i.e. check if process specified by pid exited or stopped.
 * Note: pid == -1 means "any child process".
 * If may_block != 0, wait until pid (or any child process, if pid == -1) exits or stops,
 * otherwise check for such conditions without blocking.
 *
 * If no child process matches pid, or if may_block == 0 and no child exited or
 * stopped, return Scheme empty list '().
 * Otherwise return a Scheme cons (pid . exit_flag), or c_errno() on error.
 * Exit flag is one of: process exit status, or 256 + signal, or 512 + stop signal.
 */
static ptr c_pid_wait(int pid, int may_block) {
  int wstatus = 0;
  int flag    = 0;
  int ret;
  do {
    ret = waitpid((pid_t)pid, &wstatus, may_block ? WUNTRACED : WNOHANG | WUNTRACED);
  } while (ret == -1 && errno == EINTR);

  if (ret <= 0) { /* 0 if children exist but did not change status */
    int err = 0;
    if (ret < 0) {
      err = c_errno();
      if (err == -EAGAIN || err == -ECHILD) {
        err = 0; /* no child changed status */
      }
    }
    return err == 0 ? Snil : Sinteger(err);
  } else if (WIFEXITED(wstatus)) {
    flag = (int)(unsigned char)WEXITSTATUS(wstatus);
  } else if (WIFSIGNALED(wstatus)) {
    flag = 256 + WTERMSIG(wstatus);
  } else if (WIFSTOPPED(wstatus)) {
    flag = 512 + WSTOPSIG(wstatus);
  } else {
    return Sinteger(c_errno_set(EINVAL));
  }
  return Scons(Sinteger(ret), Sinteger(flag));
}

static char** vector_to_c_argz(ptr vector_of_bytevector0) {
  ptr    vec    = vector_of_bytevector0;
  char** c_argz = NULL;
  iptr   i, n;
  if (!Svectorp(vec)) {
    return c_argz;
  }
  n      = Svector_length(vec);
  c_argz = malloc((n + 1) * sizeof(char*));
  if (!c_argz) {
    return c_argz;
  }
  for (i = 0; i < n; i++) {
    ptr  bytevec = Svector_ref(vec, i);
    iptr len;
    if (Sbytevectorp(bytevec)                      /*                        */
        && (len = Sbytevector_length(bytevec)) > 0 /*                        */
        && Sbytevector_u8_ref(bytevec, len - 1) == 0) {

      c_argz[i] = (char*)Sbytevector_data(bytevec);
    } else {
      free(c_argz);
      return NULL;
    }
  }
  c_argz[n] = NULL;
  return c_argz;
}

int schemesh_register_c_functions_posix(void) {
  int err;
  if ((err = c_tty_init()) < 0) {
    return err;
  } else if ((err = c_signals_init()) < 0) {
    return err;
  }

  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_errno_eio", &c_errno_eio);
  Sregister_symbol("c_errno_eintr", &c_errno_eintr);
  Sregister_symbol("c_errno_einval", &c_errno_einval);
  Sregister_symbol("c_strerror", &c_strerror);

  Sregister_symbol("c_chdir", &c_chdir);
  Sregister_symbol("c_get_cwd", &c_get_cwd);

  Sregister_symbol("c_fd_open_max", &c_fd_open_max);
  Sregister_symbol("c_fd_close", &c_fd_close);
  Sregister_symbol("c_fd_close_list", &c_fd_close_list);
  Sregister_symbol("c_fd_dup", &c_fd_dup);
  Sregister_symbol("c_fd_dup2", &c_fd_dup2);
  Sregister_symbol("c_fd_read", &c_fd_read);
  Sregister_symbol("c_fd_write", &c_fd_write);
  Sregister_symbol("c_fd_select", &c_fd_select);
  Sregister_symbol("c_fd_setnonblock", &c_fd_setnonblock);
  Sregister_symbol("c_fd_redirect", &c_fd_redirect);
  Sregister_symbol("c_open_file_fd", &c_open_file_fd);
  Sregister_symbol("c_open_pipe_fds", &c_open_pipe_fds);

  Sregister_symbol("c_tty_restore", &c_tty_restore);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_size", &c_tty_size);

  Sregister_symbol("c_cmd_exec", &c_cmd_exec);
  Sregister_symbol("c_cmd_spawn", &c_cmd_spawn);
  Sregister_symbol("c_pid_get", &c_pid_get);
  Sregister_symbol("c_pgid_get", &c_pgid_get);
  Sregister_symbol("c_fork_pid", &c_fork_pid);
  Sregister_symbol("c_pid_wait", &c_pid_wait);
  Sregister_symbol("c_pgid_foreground", &c_pgid_foreground);
  Sregister_symbol("c_pid_kill", &c_pid_kill);

  Sregister_symbol("c_get_hostname", &c_get_hostname);
  Sregister_symbol("c_get_userhome", &c_get_userhome);
  Sregister_symbol("c_exit", &c_exit);
  Sregister_symbol("c_directory_list", &c_directory_list);
  Sregister_symbol("c_file_stat", &c_file_stat);

  schemesh_register_c_functions_posix_signals();
  return 0;
}
