/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#if 0 /* at least on FreeBSD and MacOSX, these cause more trouble than what they are worth */
#define _POSIX_C_SOURCE 200809L /* fstatat() */
#define _DEFAULT_SOURCE         /* DT_... */
#define _BSD_SOURCE             /* DT_... SIGWINCH */
#endif

#define _FILE_OFFSET_BITS 64

#include "posix.h"
#include "../containers/containers.h" /* schemesh_Sbytevector() */
#include "../eval.h"                  /* eval() */

#include <dirent.h> /* opendir(), readdir(), closedir() */
#include <errno.h>  /* EINVAL, EIO, errno */
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pwd.h>    /* getpwnam_r(), getpwuid_r() */
#include <sched.h>  /* sched_yield() */
#include <signal.h> /* kill(), sigaction(), SIG... */
#include <stdatomic.h>
#include <stddef.h>     /* size_t, NULL */
#include <stdint.h>     /* int64_t */
#include <stdio.h>      /* remove(), rename() ... */
#include <stdlib.h>     /* getenv(), strtoul() */
#include <string.h>     /* strlen(), strerror() */
#include <sys/ioctl.h>  /* ioctl(), TIOCGWINSZ */
#include <sys/socket.h> /* socketpair(), AF_UNIX, SOCK_STREAM */
#include <sys/stat.h>   /* fstatat() */
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>   /* clock_nanosleep(), CLOCK_MONOTONIC, nanosleep() */
#include <unistd.h> /* geteuid(), getpid(), sysconf(), write() */

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

#define SCHEMESH_POSIX_POSIX_C

/** needed by signal.h */
static int c_errno(void);
static int c_errno_set(int errno_value);
static int c_init_failed(const char label[]);

/** signal.h defines a lot of static functions */
#include "signal.h"

static int c_fd_open_max(void);
static int c_job_control_available(void);

/******************************************************************************/
/*                                                                            */
/*                          errno-related functions                           */
/*                                                                            */
/******************************************************************************/

static int c_errno(void) {
  return -errno;
}

static int c_errno_set(int errno_value) {
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

static int c_errno_enoent(void) {
  return -ENOENT;
}

static int c_errno_enotdir(void) {
  return -ENOTDIR;
}

static const char* c_strerror(int err) {
  return strerror(err < 0 ? -err : err);
}

static ptr c_strerror_string(int err) {
  return schemesh_Sstring_utf8b(c_strerror(err), -1);
}

static int c_init_failed(const char label[]) {
  const int err = c_errno();
  fprintf(stderr,
          "error initializing POSIX subsystem: %s failed with error %s\n",
          label,
          c_strerror(err));
  fflush(stderr);
  return err;
}

static int write_c_errno(int err) {
  const char* err_msg = c_strerror(err);
  /* writev() is less portable */
  (void)write(2, "schemesh: ", 10);
  (void)write(2, err_msg, strlen(err_msg));
  (void)write(2, "\n", 1);
  return err;
}

static int write_path_c_errno(const char   path[],
                              const size_t path_len,
                              const int    err,
                              const char   suffix_msg[]) {
  const char* err_msg = c_strerror(err);
  (void)write(2, "schemesh: ", 10);
  (void)write(2, path, path_len);
  (void)write(2, ": ", 2);
  (void)write(2, err_msg, strlen(err_msg));
  (void)write(2, suffix_msg, strlen(suffix_msg));
  return -err;
}

static int write_command_not_found(const char path[]) {
  (void)write(2, "schemesh: ", 10);
  (void)write(2, path, strlen(path));
  (void)write(2, ": command not found. Type 'help' for help.\n", 43);
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
/*                       directory-related functions                          */
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
      return schemesh_Sstring_utf8b(dir, -1);
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
        ptr ret = schemesh_Sstring_utf8b(dir, -1);
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

/******************************************************************************/
/*                                                                            */
/*                           tty-related functions                            */
/*                                                                            */
/******************************************************************************/

/**
 * close-on-exec file descriptor for our tty.
 *
 * Scheme code assumes it is == c_fd_open_max() - 1
 * if c_tty_init() is successful.
 */
static int tty_fd = -1;

/** process group that started this process */
static int tty_pgid = -1;

static int c_tty_init(void) {
  int fd = c_fd_open_max() - 1;
  if (dup2(0, fd) < 0) {
    return c_init_failed("dup2(0, tty_fd)");
  } else if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
    (void)close(fd);
    return c_init_failed("fcntl(tty_fd, F_SETFD, FD_CLOEXEC)");
  } else {
    tty_fd = fd;
  }
  return 0;
}

static void c_tty_quit(void) {
  if (tty_fd >= 0 && tty_pgid >= 0) {
    (void)tcsetpgrp(tty_fd, tty_pgid);
  }
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
static struct termios raw_conf;
static int            have_saved_conf = 0;
static int            have_raw_conf   = 0;

/** copy initial_conf into conf, then change conf to raw mode */
static void c_tty_fill_raw_conf(struct termios* conf, const struct termios* initial_conf) {
  size_t i;
  *conf = *initial_conf;
  conf->c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR | ISTRIP | IXOFF | IXON | PARMRK);
  conf->c_oflag |= OPOST | ONLCR;
  conf->c_cflag &= ~(CSIZE | PARENB);
  conf->c_cflag |= CS8;
  conf->c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
  /* conf->c_lflag |= TOSTOP; */
  for (i = 0; i < NCCS; i++) {
    conf->c_cc[i] = 0;
  }
  conf->c_cc[VMIN] = 1;
}

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

/** save current config of controlling tty, then set it to raw mode */
static int c_tty_setraw(void) {
  struct termios* conf = &raw_conf;

  /* (void)write(1, "; c_tty_setraw\r\n", 16); */
  /*
   * save current config every time, because one of the executed commands
   * may have changed it: we want to preserve such changes for future commands
   */
  while (c_tty_getattr(tty_fd, &saved_conf) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  have_saved_conf = 1;

  if (!have_raw_conf) {
    c_tty_fill_raw_conf(conf, &saved_conf);
    have_raw_conf = 1;
  }
  while (c_tty_setattr(tty_fd, conf) != 0) {
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
/*                            job-control functions                           */
/*                                                                            */
/******************************************************************************/

/** return 1 if job control is available, otherwise return zero */
static int c_job_control_available(void) {
  return tty_fd >= 0 && isatty(tty_fd) ? 1 : 0;
}

/**
 * suspend our process group until someone puts it in the foreground.
 *
 * return process group id of current process.
 */
static int c_suspend_until_foreground(void) {
  for (;;) {
    const pid_t fg_pgid = tcgetpgrp(tty_fd);
    const pid_t pgid    = getpgid(0);
    if (tty_pgid < 0) {
      tty_pgid = fg_pgid;
    }
    if (fg_pgid == pgid) {
      return pgid;
    }
    (void)kill(-pgid, SIGTTIN);
  }
}

/**
 * change our process group, if needed, to match our process id,
 * and put it in the foreground.
 * return our process group, or < 0 on errors.
 */
static int c_create_foreground_pgid(int pgid) {
  if (pgid == getpid()) {
    /**
     * our process already has a process group id == process id
     * and such process group id is already in the foreground
     * => nothing to do
     */

    /* create a new process group id = process id and move our process into it */
  } else if (setpgid(0, 0) < 0 || (pgid = getpgid(0)) < 0 || tcsetpgrp(tty_fd, pgid) < 0) {
    return c_errno();
  }
  return (int)pgid;
}

/**
 * try to enable (if enable > 0) or disable (if enable <= 0) job control.
 *
 * if enable > 0, return process group of current process if successful,
 * otherwise error code < 0
 *
 * if enable <= 0, return 0 if successful,
 * otherwise error code < 0
 */
static int c_job_control_change(int enable) {
  int err;
  if (enable > 0) {
    pid_t pgid;
    if (!c_job_control_available()) {
      return c_errno_set(ENOTTY);
    }
    pgid = c_suspend_until_foreground();

    if ((err = c_signals_init()) < 0) {
      return err;
    }
    err = pgid = c_create_foreground_pgid(pgid);

  } else { /* enable <= 0 */

    err = c_signals_setdefault(); /* keeps SICHLD handler */
  }
  return err;
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

/**
 * call socketpair(AF_UNIX, SOCK_STREAM) and return a Scheme cons (socket1_fd . socket2_fd),
 * or c_errno() on error
 */
static ptr c_open_socketpair_fds(ptr fd1_close_on_exec, ptr fd2_close_on_exec) {
#if defined(AF_UNIX) && defined(SOCK_STREAM)
  int fds[2];
  int err = socketpair(AF_UNIX, SOCK_STREAM, 0, fds);
  if (err < 0) {
    return Sinteger(c_errno());
  }
  if (fd1_close_on_exec != Sfalse) {
    err = fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0 && fd2_close_on_exec != Sfalse) {
    err = fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0) {
    return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
  }
  err = c_errno();
  (void)close(fds[0]);
  (void)close(fds[1]);
  return Sinteger(err);
#elif defined(EAFNOSUPPORT)
  return c_errno_set(EAFNOSUPPORT);
#else
  return c_errno_set(EINVAL);
#endif
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
static int c_pid_kill(int pid, int sig, int pause_if_successful) {
  if (kill(pid, sig) < 0) {
    return c_errno();
  }
  if (pause_if_successful) {
    pause();
  }
  return 0;
}

static int c_exit(int status) {
  /* printf("c_exit(%d) invoked\n", status); */
  exit(status);
  return -EINVAL;
}

static ptr c_get_hostname_buf(char* buf, size_t len) {
  const char* end;
  if (gethostname(buf, len) != 0) {
    return Sinteger(c_errno());
  }
  /*
   * gethostname() does not guarantee to write final '\0'
   * if hostname length is >= buffer length
   */
  if ((end = (const char*)memchr(buf, 0, len)) != NULL) {
    len = end - buf;
  }
  return schemesh_Sstring_utf8b(buf, len);
}

#ifdef HOST_NAME_MAX
#define STACK_HOST_NAME_MAX (HOST_NAME_MAX + 1)
#else
#define STACK_HOST_NAME_MAX 256
#endif

static ptr c_get_hostname_stack(void) {
  char buf[STACK_HOST_NAME_MAX];
  return c_get_hostname_buf(buf, sizeof(buf));
}

/** return Scheme string containing hostname, or Scheme integer on error */
static ptr c_get_hostname(void) {
#ifdef _SC_HOST_NAME_MAX
  long len = sysconf(_SC_HOST_NAME_MAX);
  if (len >= STACK_HOST_NAME_MAX) {
    char* buf = malloc(len + 1);
    if (buf) {
      ptr ret = c_get_hostname_buf(buf, len + 1);
      free(buf);
      return ret;
    }
  }
#endif
  return c_get_hostname_stack();
}

/**
 * get user name of specified uid.
 * return a Scheme string, or Scheme integer on error
 */
static ptr c_get_username(int uid) {
  struct passwd  pwd;
  struct passwd* result = NULL;
  ptr            ret;
  char*          buf;
  long           bufsize = -1;
  int            err;

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
  err = getpwuid_r((uid_t)uid, &pwd, buf, bufsize, &result);
  if (err == 0 && result && result->pw_name) {
    ret = schemesh_Sstring_utf8b(result->pw_name, -1);
  } else {
    ret = Sinteger(c_errno_set(err != 0 ? err : ENOENT));
  }
  free(buf);
  return ret;
}

/**
 * get home directory of specified username, which must be a 0-terminated bytevector.
 * return Scheme string, or Scheme integer on error
 */
static ptr c_get_userhome(ptr username0) {
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
    ret = schemesh_Sstring_utf8b(result->pw_dir, -1);
  } else {
    ret = Sinteger(c_errno_set(err != 0 ? err : ENOENT));
  }
  free(buf);
  return ret;
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
    return c_stat_type(buf.st_mode & S_IFMT);
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
  filename =
      opts->ret_bytes ? schemesh_Sbytevector(name, namelen) : schemesh_Sstring_utf8b(name, namelen);
  ret = Scons(opts->ret_types ? Scons(filename, type) : filename, ret);

  if (name_has_slash) {
    name[--namelen] = '\0'; /* restore final '\0' */
  }
  return ret;
}

/** return effective user id of current process, or c_errno() < 0 on error */
static int c_euid_get(void) {
  int uid = geteuid();
  return uid >= 0 ? uid : c_errno();
}

/** return pid of current process, or c_errno() < 0 on error */
static int c_pid_get(void) {
  int pid = getpid();
  return pid >= 0 ? pid : c_errno();
}

/** return process group of specified process (0 = current process), or c_errno() < 0 on error */
static int c_pgid_get(int pid) {
  int pgid = getpgid((pid_t)pid);
  return pgid >= 0 ? pgid : c_errno();
}

/**
 * set process group id (i.e. pgid) of specified process:
 * if existing_pgid > 0:
 *   move current process into process group indicated by existing_pgid.
 *
 * if existing_pgid == 0:
 *   create a new process group with pgid == current process pid,
 *   and move current process into it.
 *
 * if existing_pgid < 0:
 *   do nothing.
 */
static int c_pgid_set(int pid, int existing_pgid) {
  int err = 0;
  if (existing_pgid >= 0) {
    err = setpgid((pid_t)pid, (pid_t)existing_pgid);
  }
  return err >= 0 ? err : c_errno();
}

/**
 * call fork().
 * parent: return pid, or c_errno() on error
 * child: return 0, or c_errno() on error
 *
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0 create a new process id (numerically equal to the process id)
 *                       and move process into it
 */
static int c_fork_pid(ptr vector_fds_redirect, int existing_pgid) {
  const int pid = fork();
  switch (pid) {
    case -1: /* fork() failed */
      return c_errno();
    case 0: { /* child */
      int err;
      if ((err = c_pgid_set(0, existing_pgid)) >= 0) {
        if ((err = c_signals_setdefault()) >= 0) { /* keeps SICHLD handler */
          err = c_fds_redirect(vector_fds_redirect, Sfalse);
        }
      }

      return err;
    }
    default: /* parent */
      /*
       * fix well-known race condition between parent and child:
       * both need the child to be in desired process group before continuing,
       * thus both must set it before continuing.
       */
      (void)c_pgid_set(pid, existing_pgid);
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

/**
 * optionally fork(), then exec() an external program.
 * if forked, return pid in parent process.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0 create a new process id (numerically equal to the process id)
 *                       and move process into it
 */
static int c_cmd_spawn_or_exec(ptr vector_of_bytevector0_cmdline,
                               ptr bytevector0_chdir_or_false,
                               ptr vector_fds_redirect,
                               ptr vector_of_bytevector0_environ,
                               int existing_pgid,
                               int is_spawn) {

  char** argv = vector_to_c_argz(vector_of_bytevector0_cmdline);
  char** envp = vector_to_c_argz(vector_of_bytevector0_environ);
  int    err  = 0;
  if (!argv || (!envp && Svectorp(vector_of_bytevector0_environ))) {
    err = -ENOMEM;
    goto out;
  }
  if (!argv[0]) {
    err = -EINVAL;
    goto out;
  }
  if (bytevector0_chdir_or_false != Sfalse) {
    const octet* dir;
    iptr         dir_len;
    if (!Sbytevectorp(bytevector0_chdir_or_false)) {
      err = -EINVAL;
      goto out;
    }
    dir     = Sbytevector_data(bytevector0_chdir_or_false);
    dir_len = Sbytevector_length(bytevector0_chdir_or_false);
    if (dir_len <= 0 || dir[dir_len - 1] != 0) {
      err = -EINVAL;
      goto out;
    }
  }

#ifdef SCHEMESH_DEBUG_POSIX
  fprintf(stdout, "c_cmd_spawn %s ...\n", argv[0]);
  fflush(stdout);
#endif
  if (is_spawn) {
    err = fork();
  } else {
    err = 0; /* pretend we are already in the child */
  }
  switch (err) {
    case -1: /* error */
      err = c_errno();
      break;

    default: /* parent */
      /*
       * fix well-known race condition between parent and child:
       * both need the child to be in desired process group before continuing,
       * thus both must set it before continuing.
       */
      (void)c_pgid_set(err, existing_pgid);
      break;

    case 0: { /* child */
      /*
       * only call async-safe functions until execv(), in case parent process is multi-threaded
       * and malloc() or other global resources  were locked or in an inconsistent state
       * (in the middle of a call) when fork() was executed.
       */
      char** saved_environ = environ;

      if (is_spawn) {
        if ((err = c_pgid_set(0, existing_pgid) < 0) ||
            /* keep SICHLD handler, will be resetted by execv...() */
            (err = c_signals_setdefault()) < 0) {
          goto child_out;
        }
      }
      if ((bytevector0_chdir_or_false != Sfalse &&
           (err = c_chdir(bytevector0_chdir_or_false) < 0)) ||
          (err = c_fds_redirect(vector_fds_redirect, Sfalse)) < 0) {
        goto child_out;
      }
      if (envp) {
        environ = envp;
      }
      if (strchr(argv[0], '/')) {
        (void)execv(argv[0], argv);
      } else {
        (void)execvp(argv[0], argv);
      }
      /* in case or execv...() failed and returned */
      err = c_errno();
      if (err == -ENOENT) {
        (void)write_command_not_found(argv[0]);
      } else {
        (void)write_path_c_errno(argv[0], strlen(argv[0]), err, ". Type 'help' for help.\n");
      }
    child_out:
      if (is_spawn) {
        exit(err < 0 ? 1 : 127);
      }
      if (envp) {
        environ = saved_environ;
      }
      break;
    }
  }
out:
  free(argv);
  free(envp);
  return err;
}

/**
 * exec() an external program. Returns only if failed.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0, create a new process group id == process id, and move process into it.
 * if existing_pgid < 0, process inherits the process group id from current process
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
                             0); /* !is_spawn */
}

/**
 * fork() and exec() an external program, return pid.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0, create a new process group id == process id, and move process into it.
 * if existing_pgid < 0, process inherits the process group id from current process
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
 * get the foreground process group id.
 *
 * return pgid >= 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_get() {
  const int pgid = tcgetpgrp(tty_fd);
  return pgid >= 0 ? pgid : c_errno();
}

/**
 * set the foreground process group id.
 *
 * return = 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_set(int new_pgid) {
  return tcsetpgrp(tty_fd, new_pgid) >= 0 ? 0 : c_errno();
}

/**
 * compare-and-swap the foreground process group id.
 *
 * if old_pgid < 0, unconditionally set new_pgid as the foreground process group.
 *
 * if old_pgid >= 0 and the foreground process group == old_pgid,
 * then set new_pgid as the foreground process group.
 *
 * return 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_cas(int old_pgid, int new_pgid) {
  if (old_pgid >= 0) {
    const int current_pgid = tcgetpgrp(tty_fd);
    if (current_pgid < 0) {
      return c_errno();
    } else if (current_pgid != old_pgid) {
      return 0; /* fg process group is not the expected one: do nothing */
    } else if (current_pgid == new_pgid) {
      return 0; /* nothing to do */
    }
  }
  return tcsetpgrp(tty_fd, new_pgid) >= 0 ? 0 : c_errno();
}

/**
 * call waitpid(pid, WUNTRACED|WCONTINUED) i.e. check if process specified by pid
 * finished, stopped or resumed.
 *
 * Special cases:
 *   pid ==  0 means "any child process in the same process group as the caller"
 *   pid == -1 means "any child process"
 *   pid <  -1 means "any child process in process group -pid"
 *
 * If may_block != 0, wait until pid (or any child process, if pid == -1) exits or stops,
 * otherwise check for such conditions without blocking.
 *
 * If no child process matches pid, or if may_block == 0 and no child finished, or
 * stopped, return Scheme empty list '().
 * Otherwise return a Scheme cons (pid . status_flag), or c_errno() on error.
 * status_flag is one of:
 *   process exit status in 0 ... 255
 *   or 256 + signal that killed the process
 *   or 512 + signal that stopped the process
 *   or 768 if job resumed due to SIGCONT
 */
static ptr c_pid_wait(int pid, int may_block) {
  int   wstatus = 0;
  int   result  = 0;
  int   retry_n = 1;
  pid_t ret_pid;
  /*
   * avoid WCONTINUED on macOS:
   * it repeatedly reports the same pid as "continued", causing a busy loop
   */
#if defined(WCONTINUED) && !defined(__APPLE__)
  const int options = WUNTRACED | WCONTINUED;
#else
  const int options = WUNTRACED;
#endif

again:
  ret_pid = waitpid((pid_t)pid, &wstatus, options | (may_block ? 0 : WNOHANG));

  if (ret_pid <= 0) { /* 0 if children exist but did not change status */
    int err = 0;
    if (ret_pid < 0) {
      err = c_errno();
      /*
       * when a child stops on macOS, waitpid() fails with err == -EINTR
       * and we must call waitpid() again to get stopped child's pid.
       *
       * Retrying on EINTR may help on other systems too, thus do it unconditionally
       */
      if (retry_n > 0 && err == -EINTR) {
        retry_n--;
        may_block = 0;
        goto again;
      }
      if (err == -EAGAIN || err == -EINTR || err == -ECHILD) {
        err = 0; /* no child changed status */
      }
    }
#ifdef SCHEMESH_DEBUG_WAIT_PID
    fprintf(stderr,
            "c_pid_wait(pid = %d, may_block = %d) -> pid = %d, errno = %d %s\n",
            pid,
            may_block,
            ret_pid,
            errno,
            strerror(errno));
    fflush(stderr);
#endif /* DEBUG_WAIT_PID */
    return err == 0 ? Snil : Sinteger(err);
  } else if (WIFEXITED(wstatus)) {
    result = (int)(unsigned char)WEXITSTATUS(wstatus);
  } else if (WIFSIGNALED(wstatus)) {
    result = 256 + WTERMSIG(wstatus);
  } else if (WIFSTOPPED(wstatus)) {
    result = 512 + WSTOPSIG(wstatus);
#ifdef WIFCONTINUED
  } else if (WIFCONTINUED(wstatus)) {
    result = 768;
#endif
  } else {
    return Sinteger(c_errno_set(EINVAL));
  }
#ifdef SCHEMESH_DEBUG_WAIT_PID
  fprintf(stderr,
          "c_pid_wait(pid = %d, may_block = %d) -> pid = %d, result = %d\n",
          pid,
          may_block,
          ret_pid,
          result);
  fflush(stderr);
#endif /* SCHEMESH_DEBUG_WAIT_PID */

  return Scons(Sinteger(ret_pid), Sinteger(result));
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

static uptr c_thread_count(void) {
#ifdef FEATURE_PTHREADS
  extern volatile uptr S_nthreads;
  return S_nthreads;
#else
  return 1;
#endif
}

typedef struct s_mutex_t scheme_mutex_t;

extern void S_mutex_acquire(scheme_mutex_t* m);
extern void S_mutex_release(scheme_mutex_t* m);

static ptr c_thread_list(void) {
  extern volatile ptr S_threads;

  ptr ls;
  ptr ret = Snil;

#ifdef FEATURE_PTHREADS
  extern scheme_mutex_t S_tc_mutex;
  extern int            S_tc_mutex_depth;

  S_mutex_acquire(&S_tc_mutex);
  S_tc_mutex_depth += 1;
#endif

  for (ls = S_threads; ls != Snil; ls = Scdr(ls)) {
    ret = Scons(Scar(ls), ret);
  }
#ifdef FEATURE_PTHREADS
  S_tc_mutex_depth -= 1;
  S_mutex_release(&S_tc_mutex);
#endif
  return ret;
}

int schemesh_register_c_functions_posix(void) {
  int err;
  if ((err = c_tty_init()) < 0) {
    return err;
  }

  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_errno_eio", &c_errno_eio);
  Sregister_symbol("c_errno_eintr", &c_errno_eintr);
  Sregister_symbol("c_errno_einval", &c_errno_einval);
  Sregister_symbol("c_errno_enoent", &c_errno_enoent);
  Sregister_symbol("c_errno_enotdir", &c_errno_enotdir);

  Sregister_symbol("c_strerror_string", &c_strerror_string);

  Sregister_symbol("c_chdir", &c_chdir);
  Sregister_symbol("c_get_cwd", &c_get_cwd);
  Sregister_symbol("c_mkdir", &c_mkdir);

  Sregister_symbol("c_fd_open_max", &c_fd_open_max);
  Sregister_symbol("c_fd_close", &c_fd_close);
  Sregister_symbol("c_fd_close_list", &c_fd_close_list);
  Sregister_symbol("c_fd_dup", &c_fd_dup);
  Sregister_symbol("c_fd_dup2", &c_fd_dup2);
  Sregister_symbol("c_fd_seek", &c_fd_seek);
  Sregister_symbol("c_fd_read", &c_fd_read);
  Sregister_symbol("c_fd_read_u8", &c_fd_read_u8);
  Sregister_symbol("c_fd_write", &c_fd_write);
  Sregister_symbol("c_fd_write_u8", &c_fd_write_u8);
  Sregister_symbol("c_fd_select", &c_fd_select);
  Sregister_symbol("c_fd_setnonblock", &c_fd_setnonblock);
  Sregister_symbol("c_fd_redirect", &c_fd_redirect);
  Sregister_symbol("c_open_file_fd", &c_open_file_fd);
  Sregister_symbol("c_open_pipe_fds", &c_open_pipe_fds);
  Sregister_symbol("c_open_socketpair_fds", &c_open_socketpair_fds);

  Sregister_symbol("c_tty_restore", &c_tty_restore);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_size", &c_tty_size);
  Sregister_symbol("c_job_control_available", &c_job_control_available);
  Sregister_symbol("c_job_control_change", &c_job_control_change);

  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_thread_list", &c_thread_list);

  Sregister_symbol("c_cmd_exec", &c_cmd_exec);
  Sregister_symbol("c_cmd_spawn", &c_cmd_spawn);
  Sregister_symbol("c_euid_get", &c_euid_get);
  Sregister_symbol("c_pid_get", &c_pid_get);
  Sregister_symbol("c_pgid_get", &c_pgid_get);
  Sregister_symbol("c_fork_pid", &c_fork_pid);
  Sregister_symbol("c_pid_wait", &c_pid_wait);
  Sregister_symbol("c_pgid_foreground_get", &c_pgid_foreground_get);
  Sregister_symbol("c_pgid_foreground_set", &c_pgid_foreground_set);
  Sregister_symbol("c_pgid_foreground_cas", &c_pgid_foreground_cas);
  Sregister_symbol("c_pid_kill", &c_pid_kill);

  Sregister_symbol("c_get_hostname", &c_get_hostname);
  Sregister_symbol("c_get_username", &c_get_username);
  Sregister_symbol("c_get_userhome", &c_get_userhome);
  Sregister_symbol("c_exit", &c_exit);
  Sregister_symbol("c_directory_list", &c_directory_list);
  Sregister_symbol("c_file_delete", &c_file_delete);
  Sregister_symbol("c_file_rename", &c_file_rename);
  Sregister_symbol("c_file_type", &c_file_type);

  c_register_c_functions_posix_signals();
  return 0;
}

/**
 * quit Chez Scheme. calls:
 *   c_tty_quit()
 *   Sscheme_deinit()
 */
void schemesh_quit(void) {
  c_tty_quit();
  Sscheme_deinit();
}
