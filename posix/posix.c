/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "posix.h"
#include "../eval.h" /* eval() */
#include "signal.h"

#include <dirent.h> /* opendir(), readdir(), closedir() */
#include <errno.h>  /* EINVAL, EIO, errno */
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h> /* getenv(), strtoul() */
#include <string.h>
#include <string.h>    /* strlen() */
#include <sys/ioctl.h> /* ioctl(), TIOCGWINSZ */
#include <sys/wait.h>
#include <termios.h> /* tcgetattr(), tcsetattr() */
#include <unistd.h>  /* write() */

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

static int c_errno_eio() {
  return -EIO;
}

static int c_errno_eintr() {
  return -EINTR;
}

static int c_errno_einval() {
  return -EINVAL;
}

ptr c_strerror(int err) {
  return Sstring_utf8(strerror(err < 0 ? -err : err), -1);
}

int c_init_failed(const char label[]) {
  const int err = errno;
  fprintf(stderr,
          "error initializing POSIX subsystem: %s failed with error %s\n",
          label,
          strerror(err));
  return -err;
}

/******************************************************************************/
/*                                                                            */
/*                           tty-related functions                            */
/*                                                                            */
/******************************************************************************/

/** close-on-exec file descriptor for our tty */
static int tty_fd = -1;

static int c_tty_init(void) {
  int err = 0;
  if (dup2(0, tty_fd = 255) < 0) {
    err = c_init_failed("dup2(0, tty_fd)");
  } else if (fcntl(tty_fd, F_SETFD, FD_CLOEXEC) < 0) {
    err = c_init_failed("fcntl(tty_fd, F_SETFD, FD_CLOEXEC)");
  }
  return err;
}

/** return file descriptor for our controlling tty */
int c_tty_fd(void) {
  return tty_fd;
}

static struct termios saved_conf;
static int            have_saved_conf = 0;

/** restore controlling tty to saved config */
static int c_tty_restore(void) {
  /* (void)write(1, "; c_tty_restore\r\n", 17); */
  if (have_saved_conf) {
    while (tcsetattr(tty_fd, TCSADRAIN, &saved_conf) != 0) {
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
    while (tcgetattr(tty_fd, &saved_conf) != 0) {
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
  while (tcsetattr(tty_fd, TCSADRAIN, &conf) != 0) {
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

/** close specified file descriptor */
static int c_fd_close(int fd) {
  int ret = close(fd);
  return ret >= 0 ? ret : c_errno();
}

/** close all file descriptors >= lowest_fd_to_close */
static void c_fd_close_all(int lowest_fd_to_close) {
  int i       = lowest_fd_to_close >= 0 ? lowest_fd_to_close : 0;
  int last_ok = i - 1;
  for (; i < INT_MAX && i - 32 <= last_ok; i++) {
    if (close(i) >= 0) {
      last_ok = i;
    }
  }
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
static ptr c_open_pipe_fds(void) {
  int fds[2];
  int ret = pipe(fds);
  if (ret < 0) {
    return Sinteger(c_errno());
  }
  return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
}

static int c_check_redirect_fds(ptr vector_redirect_fds) {
  iptr i, n;
  if (!Svectorp(vector_redirect_fds)) {
    return -EINVAL;
  }
  n = Svector_length(vector_redirect_fds);
  for (i = 0; i < n; i++) {
    ptr elem = Svector_ref(vector_redirect_fds, i);
    if (!Sfixnump(elem)) {
      return -EINVAL;
    }
  }
  return 0;
}

/** redirect fds as indicated in vector_redirect_fds. return < 0 on error */
static int c_redirect_fds(ptr vector_redirect_fds) {
  iptr i, n;
  int  lowest_fd_to_close = 0;
  if (!Svectorp(vector_redirect_fds)) {
    return -1;
  }
  n = Svector_length(vector_redirect_fds);
  for (i = 0; i < n; i++) {
    ptr  elem = Svector_ref(vector_redirect_fds, i);
    iptr fd;
    if (!Sfixnump(elem)) {
      return -1;
    }
    fd = Sfixnum_value(elem);
    if (fd >= 0) {
      if (fd != i && dup2(fd, i) < 0) {
        return -1;
      }
      lowest_fd_to_close = i + 1;
    }
  }
  /* close all fds in 0...(lowest_fd_to_close-1) except the redirected ones */
  for (i = 0; i < lowest_fd_to_close; i++) {
    ptr elem = Svector_ref(vector_redirect_fds, i);
    if (!Sfixnump(elem) || Sfixnum_value(elem) < 0) {
      (void)close(i);
    }
  }
  return lowest_fd_to_close;
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
  return Sstring_utf8(buf, -1);
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
 */
static ptr c_readdir_type(unsigned char d_type) {
  unsigned char type;
  switch (d_type) {
    case DT_BLK:
      type = 1;
      break;
    case DT_CHR:
      type = 2;
      break;
    case DT_DIR:
      type = 3;
      break;
    case DT_FIFO:
      type = 4;
      break;
    case DT_LNK:
      type = 7;
      break;
    case DT_REG:
      type = 5;
      break;
    case DT_SOCK:
      type = 6;
      break;
    case DT_UNKNOWN:
    default:
      type = 0;
      break;
  }
  return Sfixnum(type);
}

/* convert a C char[] to Scheme bytevector */
static ptr c_chars_to_bytevector(const char chars[], const size_t len) {
  /* Smake_bytevector() wants iptr length */
  iptr slen = (int)len;
  if (slen < 0 || (size_t)slen != len) {
    /** raises condition in Smake_bytevector() */
    slen = -1;
  }
  ptr bvec = Smake_bytevector(slen, 0);
  memcpy(Sbytevector_data(bvec), chars, len);
  return bvec;
}

/**
 * Scan directory bytevector0_dirpath and return Scheme list with its contents as pairs
 * (type . filename) where filename is a Scheme bytevector,
 * and type is a Scheme integer documented in c_readdir_type()
 *
 * If bytevector_filter_prefix is not empty,
 * only returns filenames that start with bytevector_filter_prefix.
 *
 * on error, return Scheme integer -errno
 */
static ptr c_directory_u8_list(ptr bytevector0_dirpath, ptr bytevector_filter_prefix) {
  ptr            ret = Snil;
  const char*    dirpath;
  const char*    prefix;
  iptr           dirlen;
  iptr           prefixlen;
  DIR*           dir;
  struct dirent* entry;
  if (!Sbytevectorp(bytevector0_dirpath) || !Sbytevectorp(bytevector_filter_prefix)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  dirpath   = (const char*)Sbytevector_data(bytevector0_dirpath);
  dirlen    = Sbytevector_length(bytevector0_dirpath); /* including final '\0' */
  prefix    = (const char*)Sbytevector_data(bytevector_filter_prefix);
  prefixlen = Sbytevector_length(bytevector_filter_prefix);
  if (prefixlen < 0 || dirlen <= 0 || dirpath[dirlen - 1] != '\0') {
    return Sinteger(c_errno_set(EINVAL));
  }
  dir = opendir(dirpath);
  if (!dir) {
    return Sinteger(c_errno());
  }
  while ((entry = readdir(dir)) != NULL) {
    const char*  name = entry->d_name;
    const size_t len  = strlen(name);
    if (!prefixlen || (len >= (size_t)prefixlen && memcmp(name, prefix, prefixlen) == 0)) {
      ptr pair = Scons(c_readdir_type(entry->d_type), c_chars_to_bytevector(name, len));
      ret      = Scons(pair, ret);
    }
  }
  (void)closedir(dir);
  return ret;
}

/** return pid of current process, or c_errno() on error */
static int c_get_pid(void) {
  int pid = getpid();
  return pid >= 0 ? pid : c_errno();
}

/** return process group of specified process (0 = current process), or c_errno() on error */
static int c_get_pgid(int pid) {
  int pgid = getpgid((pid_t)pid);
  return pgid >= 0 ? pgid : c_errno();
}

static int c_set_process_group(pid_t existing_pgid_if_positive) {
  int err = setpgid(0 /*current process*/, /*                                    */
                    existing_pgid_if_positive > 0 ? existing_pgid_if_positive : 0);
  return err >= 0 ? err : c_errno();
}

/** fork() and return pid, or c_errno() on error */
static int c_fork_pid(ptr vector_redirect_fds, int existing_pgid_if_positive) {
  const int pid = fork();
  switch (pid) {
    case -1:
      return c_errno(); /* fork() failed */
    case 0: {
      /* child */
      int err = c_set_process_group((pid_t)existing_pgid_if_positive);
      if (err >= 0) {
        err = c_signal_setdefault(SIGTSTP);
        if (err >= 0) {
          int lowest_fd_to_close = c_redirect_fds(vector_redirect_fds);
          if (lowest_fd_to_close >= 0) {
            return 0;
          }
        }
      }
      /* in case c_set_process_group() or c_redirect_fds() fail */
      exit(255);
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

static void write_command_not_found(const char arg0[]);

/** fork() and exec() an external program, return pid.
 * if existing_pgid_if_positive > 0, add process to given pgid i.e. process group */
static int c_spawn_pid(ptr vector_of_bytevector0_cmdline,
                       ptr vector_redirect_fds,
                       ptr vector_of_bytevector0_environ,
                       int existing_pgid_if_positive) {
  char **argv = NULL, **envp = NULL;
  int    pid;
  if ((pid = c_check_redirect_fds(vector_redirect_fds)) < 0) {
    goto out;
  }
  envp = vector_to_c_argz(vector_of_bytevector0_environ);
  argv = vector_to_c_argz(vector_of_bytevector0_cmdline);
  if (!argv || (!envp && Svectorp(vector_of_bytevector0_environ))) {
    pid = -ENOMEM;
    goto out;
  }
  if (!argv[0]) {
    pid = -EINVAL;
    goto out;
  }
#ifdef SCHEMESH_DEBUG_POSIX
  fprintf(stdout, "c_spawn_pid %s ...\n", argv[0]);
  fflush(stdout);
#endif
  pid = fork();
  switch (pid) {
    case -1:
      /* error */
      pid = c_errno();
      break;
    case 0: {
      /* child */
      int err = c_set_process_group((pid_t)existing_pgid_if_positive);
      if (err >= 0) {
        err = c_signals_setdefault();
        if (err >= 0) {
          int lowest_fd_to_close = c_redirect_fds(vector_redirect_fds);
          if (lowest_fd_to_close >= 0) {
            (void)c_fd_close_all(lowest_fd_to_close);
            if (envp) {
              environ = envp;
            }
            (void)execvp(argv[0], argv);
            write_command_not_found(argv[0]);
          }
        }
      }
      /* in case c_set_process_group() or c_redirect_fds() fail,
       * or execvp() fails and returns */
      exit(255);
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

static void write_command_not_found(const char arg0[]) {
  /* writev() is less portable */
  (void)write(2, "schemesh: ", 10);
  (void)write(2, arg0, strlen(arg0));
  (void)write(2, ": command not found\n", 20);
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
  Sregister_symbol("c_fd_close", &c_fd_close);
  Sregister_symbol("c_fd_dup", &c_fd_dup);
  Sregister_symbol("c_fd_dup2", &c_fd_dup2);
  Sregister_symbol("c_fd_read", &c_fd_read);
  Sregister_symbol("c_fd_write", &c_fd_write);
  Sregister_symbol("c_fd_select", &c_fd_select);
  Sregister_symbol("c_fd_setnonblock", &c_fd_setnonblock);
  Sregister_symbol("c_open_file_fd", &c_open_file_fd);
  Sregister_symbol("c_open_pipe_fds", &c_open_pipe_fds);

  Sregister_symbol("c_tty_restore", &c_tty_restore);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_size", &c_tty_size);

  Sregister_symbol("c_get_pid", &c_get_pid);
  Sregister_symbol("c_get_pgid", &c_get_pgid);
  Sregister_symbol("c_fork_pid", &c_fork_pid);
  Sregister_symbol("c_spawn_pid", &c_spawn_pid);
  Sregister_symbol("c_pid_wait", &c_pid_wait);
  Sregister_symbol("c_pgid_foreground", &c_pgid_foreground);
  Sregister_symbol("c_pid_kill", &c_pid_kill);

  Sregister_symbol("c_get_hostname", &c_get_hostname);
  Sregister_symbol("c_exit", &c_exit);
  Sregister_symbol("c_directory_u8_list", &c_directory_u8_list);

  schemesh_register_c_functions_posix_signals();
  return 0;
}
