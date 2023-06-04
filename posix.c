/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "posix.h"
#include "eval.h" /* eval() */
#include "signal.h"

#include <errno.h> /* EINVAL, EIO, errno */
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

#undef SCHEMESH_LIBRARY_FD_DEBUG

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define STR_EIO STR(EIO)
#define STR_EINTR STR(EINTR)
#define STR_EINVAL STR(EINVAL)

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

int c_errno_print(const char label[]) {
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

/** close-on-exec file descriptor for our controlling tty */
static int tty_fd = -1;

static int c_tty_init(void) {
  int err = 0;

  if ((tty_fd = open("/dev/tty", O_RDWR)) < 0) {
    err = c_errno_print("open(\"/dev/tty\")");
  } else if (dup2(tty_fd, 255) < 0) {
    err = c_errno_print("dup2(tty_fd, 255)");
  } else if (close(tty_fd) < 0) {
    err = c_errno_print("close(tty_fd)");
  } else if (fcntl(tty_fd = 255, F_SETFD, FD_CLOEXEC) < 0) {
    err = c_errno_print("fcntl(tty_fd, F_SETFD, FD_CLOEXEC)");
  }
  return err;
}

/** return file descriptor for our controlling tty */
int c_tty_fd(void) {
  return tty_fd;
}

static struct termios saved_conf;
static int            have_saved_conf = 0;

int c_tty_restore(void) {
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

int c_tty_setraw(void) {
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

ptr c_tty_size(void) {
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

void schemesh_define_library_tty(void) {
  Sregister_symbol("c_tty_restore", &c_tty_restore);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_size", &c_tty_size);

  eval("(library (schemesh tty (0 1))\n"
       "  (export tty-setraw! tty-restore! tty-size)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) foreign-procedure))\n"
       "\n"
       "(define tty-setraw! (foreign-procedure \"c_tty_setraw\" () int))\n"
       "\n"
       "(define tty-restore! (foreign-procedure \"c_tty_restore\" () int))\n"
       /**
        * (tty-size) calls C functions c_tty_size(),
        * which returns controlling tty's (width . height), or c_errno() on error
        */
       "(define tty-size   (foreign-procedure \"c_tty_size\" () scheme-object))\n"
       ")\n"); /* close library */
}

/******************************************************************************/
/*                                                                            */
/*                            fd-related functions                            */
/*                                                                            */
/******************************************************************************/

int c_fd_close(int fd) {
  int ret = close(fd);
  return ret >= 0 ? ret : c_errno();
}

void c_fd_close_all(int lowest_fd_to_close) {
  int i       = lowest_fd_to_close >= 0 ? lowest_fd_to_close : 0;
  int last_ok = i - 1;
  for (; i < INT_MAX && i - 32 <= last_ok; i++) {
    if (close(i) >= 0) {
      last_ok = i;
    }
  }
}

int c_fd_dup(int old_fd) {
  int ret = dup(old_fd);
  return ret >= 0 ? ret : c_errno();
}

int c_fd_dup2(int old_fd, int new_fd) {
  int ret = dup2(old_fd, new_fd);
  return ret >= 0 ? ret : c_errno();
}

int c_fd_setnonblock(int fd) {
  int flags;
  while ((flags = fcntl(fd, F_GETFL)) < 0 && errno == EINTR) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  flags |= O_NONBLOCK;
  while (fcntl(fd, F_SETFL, flags) < 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

/** call read(). returns number of bytes received, or c_errno() < 0 on error */
iptr c_fd_read(int fd, ptr bytevector_read, iptr start, iptr end) {
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
iptr c_fd_write(int fd, ptr bytevector_towrite, iptr start, iptr end) {
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

int c_fd_select(int fd, int rw_mask, int timeout_milliseconds) {
  struct pollfd entry;
  entry.fd     = fd;
  entry.events = (rw_mask & mask_READ ? POLLIN : 0) | /*                                         */
                 (rw_mask & mask_WRITE ? POLLOUT : 0);
  entry.revents = 0;
  if (poll(&entry, 1, timeout_milliseconds) < 0) {
    /** do NOT retry on EINTR, return it instead */
    return c_errno();
  }
  return (entry.revents & POLLIN ? mask_READ : 0) |   /*                                         */
         (entry.revents & POLLOUT ? mask_WRITE : 0) | /*                                         */
         (entry.revents & POLLERR ? mask_ERR : 0);
}

int c_open_file_fd(ptr bytevector0_filepath,
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
  if (len == 0 || filepath[len - 1] != '\0') {
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

ptr c_open_pipe_fds(void) {
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

int c_pid_kill(int pid, int sig) {
  return kill(pid, sig) >= 0 ? 0 : c_errno();
}

int schemesh_define_library_fd(void) {
  int err;
  if ((err = c_tty_init()) < 0) {
    return err;
  } else if ((err = c_signals_init()) < 0) {
    return err;
  }
  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_fd_close", &c_fd_close);
  Sregister_symbol("c_fd_dup", &c_fd_dup);
  Sregister_symbol("c_fd_dup2", &c_fd_dup2);
  Sregister_symbol("c_fd_read", &c_fd_read);
  Sregister_symbol("c_fd_write", &c_fd_write);
  Sregister_symbol("c_fd_select", &c_fd_select);
  Sregister_symbol("c_fd_setnonblock", &c_fd_setnonblock);
  Sregister_symbol("c_open_file_fd", &c_open_file_fd);
  Sregister_symbol("c_open_pipe_fds", &c_open_pipe_fds);

  eval("(library (schemesh fd (0 1))\n"
       "  (export\n"
       "    errno make-errno-condition raise-errno-condition\n"
       "    fd-close fd-close-list fd-dup fd-dup2 fd-read fd-write fd-select fd-setnonblock\n"
       "    open-file-fd open-pipe-fds)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) foreign-procedure void\n"
#ifdef SCHEMESH_LIBRARY_FD_DEBUG
       "      format\n"
#endif
       "      )\n"
       "    (only (schemesh containers misc) list-iterate)\n"
       "    (only (schemesh conversions)     string->bytevector0))\n"
       "\n"
       "(define errno\n"
       "  (foreign-procedure \"c_errno\" () int))\n"
       "\n"
       "(define (make-errno-condition who c-errno)\n"
       "  (condition\n"
       "    (make-error)\n"
       "    (make-who-condition who)\n"
       "    (make-message-condition \"error in C function\")\n"
       "    (make-irritants-condition c-errno)))\n"
       "\n"
       "(define (raise-errno-condition who c-errno)\n"
#ifdef SCHEMESH_LIBRARY_FD_DEBUG
       "  (format #t \"raise-errno-condition ~s ~s~%\" who c-errno)\n"
#endif
       "  (raise (make-errno-condition who c-errno)))\n"
       "\n"
       "(define fd-close\n"
       "  (let ((c-fd-close (foreign-procedure \"c_fd_close\" (int) int)))\n"
       "    (lambda (x)\n"
       "      (let ((ret (c-fd-close x)))\n"
       "        (if (>= ret 0)\n"
       "          (void)\n"
       "          (make-errno-condition 'fd-close ret))))))\n"
       "\n"
       "(define (fd-close-list fd-list)\n"
       "  (list-iterate fd-list fd-close))\n"
       "\n"
       "(define fd-dup\n"
       "  (let ((c-fd-dup (foreign-procedure \"c_fd_dup\" (int) int)))\n"
       "    (lambda (old-fd)\n"
       "      (let ((ret (c-fd-dup old-fd)))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'fd-dup ret))))))\n"
       "\n"
       "(define fd-dup2\n"
       "  (let ((c-fd-dup2 (foreign-procedure \"c_fd_dup2\" (int int) int)))\n"
       "    (lambda (old-fd new-fd)\n"
       "      (let ((ret (c-fd-dup2 old-fd new-fd)))\n"
       "        (if (>= ret 0)\n"
       "          (void)\n"
       "          (raise-errno-condition 'fd-dup2 ret))))))\n"
       "\n"
       "(define fd-read\n"
       "  (let ((c-fd-read (foreign-procedure \"c_fd_read\" (int ptr iptr iptr) iptr)))\n"
       "    (lambda (fd bytevector-result start end)\n"
       "      (let ((ret (c-fd-read fd bytevector-result start end)))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'fd-read ret))))))\n"
       "\n"
       "(define fd-write\n"
       "  (let ((c-fd-write (foreign-procedure \"c_fd_write\" (int ptr iptr iptr) iptr)))\n"
       "    (lambda (fd bytevector-towrite start end)\n"
       "      (let ((ret (c-fd-write fd bytevector-towrite start end)))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'fd-write ret))))))\n"
       "\n"
       /**
        * (fd-select fd direction timeout-milliseconds) waits up to timeout-milliseconds
        * for file descriptor fd to become ready for input, output or both.
        *
        * direction must be one of: 'read 'write 'rw
        * timeout-milliseconds < 0 means infinite timeout
        *
        * On success, returns one of: 'timeout 'read 'write 'rw
        * On error, raises condition.
        */
       "(define fd-select\n"
       "  (let ((c-fd-select (foreign-procedure \"c_fd_select\" (int int int) int)))\n"
       "    (lambda (fd direction timeout-milliseconds)\n"
       "      (assert (memq direction '(read write rw)))\n"
       "      (let* ((rw-mask (cond ((eq? 'rw    direction) 3)\n"
       "                            ((eq? 'write direction) 2)\n"
       "                            ((eq? 'read  direction) 1)\n"
       "                            (#t (error 'fd-select\n"
       "                                \"direction must be one of 'read 'write 'rw\"))))\n"
       "              (ret (c-fd-select fd rw-mask timeout-milliseconds)))\n"
       "        (cond\n"
       /*         if c_fd_select() returns EINTR, consider it a timeout */
       "          ((eqv? ret -" STR_EINTR ") 'timeout)\n"
       "          ((< ret 0) (raise-errno-condition 'fd-select ret))\n"
       "          ((< ret 4) (vector-ref '#(timeout read write rw) ret))\n"
       /*                     c_fd_select() called poll() which set (revents & POLLERR)
        */
       "          (#t        (raise-errno-condition 'fd-select -" STR_EIO ")))))))\n"
       "\n"
       "(define fd-setnonblock\n"
       "  (let ((c-fd-setnonblock (foreign-procedure \"c_fd_setnonblock\" (int) int)))\n"
       "    (lambda (fd)\n"
       "      (let ((ret (c-fd-setnonblock fd)))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'fd-setnonblock ret))))))\n"
       "\n"
       "(define open-file-fd\n"
       "  (let ((c-open-file-fd (foreign-procedure \"c_open_file_fd\""
       "                          (scheme-object int int int int) int)))\n"
       "    (lambda (filepath . flags)\n"
       "      (let* ([filepath0 (string->bytevector0 filepath)]\n"
       "             [flag-rw (cond ((memq 'rw    flags) 2)\n"
       "                            ((memq 'write flags) 1)\n"
       "                            ((memq 'read  flags) 0)\n"
       "                            (#t (error 'open-file-fd\n"
       "                                 \"flags must contain one of 'read 'write 'rw\" "
       "flags)))]\n"
       "             [flag-create   (if (memq 'create   flags) 1 0)]\n"
       "             [flag-truncate (if (memq 'truncate flags) 1 0)]\n"
       "             [flag-append   (if (memq 'append   flags) 1 0)]\n"
       "             [ret (c-open-file-fd filepath0 flag-rw flag-create "
       "                    flag-truncate flag-append)])\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'open-file-fd ret))))))\n"
       "\n"
       "(define open-pipe-fds\n"
       "  (let ((c-open-pipe-fds (foreign-procedure \"c_open_pipe_fds\" () "
       "scheme-object)))\n"
       "    (lambda ()\n"
       "      (let ((ret (c-open-pipe-fds)))\n"
       "        (if (pair? ret)\n"
       "          ret\n"
       "          (raise-errno-condition 'open-pipe-fds ret))))))\n"
       ")\n"); /* close library */

  return 0;
}

static int c_exit(int status) {
  /* printf("c_exit(%d) invoked\n", status); */
  exit(status);
  return -EINVAL;
}

static ptr c_get_hostname(void) {
  char buf[HOST_NAME_MAX + 1];
  if (gethostname(buf, sizeof(buf)) != 0) {
    return Sinteger(c_errno());
  }
  return Sstring_utf8(buf, -1);
}

void schemesh_define_library_posix(void) {
  Sregister_symbol("c_get_hostname", &c_get_hostname);
  Sregister_symbol("c_exit", &c_exit);

  eval("(library (schemesh posix (0 1))\n"
       "  (export c-hostname c-exit)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) foreign-procedure))\n"
       "\n"
       "(define c-exit (foreign-procedure \"c_exit\" (int) int))\n"
       "\n"
       "(define c-hostname\n"
       "  (let* ((hostname-or-error ((foreign-procedure \"c_get_hostname\" () scheme-object)))\n"
       "         (hostname (if (string? hostname-or-error) hostname-or-error \"???\")))\n"
       "    (lambda ()\n"
       "      hostname)))\n"
       "\n"
       ")\n"); /* close library */
}

void schemesh_define_library_pid(void) {
  Sregister_symbol("c_get_pid", &c_get_pid);
  Sregister_symbol("c_get_pgid", &c_get_pgid);
  Sregister_symbol("c_fork_pid", &c_fork_pid);
  Sregister_symbol("c_spawn_pid", &c_spawn_pid);
  Sregister_symbol("c_pid_wait", &c_pid_wait);
  Sregister_symbol("c_pgid_foreground", &c_pgid_foreground);
  Sregister_symbol("c_pid_kill", &c_pid_kill);

  eval("(library (schemesh pid (0 1))\n"
       "  (export get-pid get-pgid spawn-pid pid-kill pid-wait exit-with-job-status)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) foreign-procedure void\n"
#ifdef SCHEMESH_LIBRARY_FD_DEBUG
       "      format\n"
#endif
       "      )"
       "    (schemesh fd)\n"
       "    (only (schemesh conversions) list->cmd-argv)\n"
       "    (only (schemesh signals) signal-name->number signal-raise)\n"
       "    (only (schemesh posix) c-exit))\n"
       "\n"
       /** (get-pid) returns pid of current process */
       "(define get-pid"
       "  (let ((c-get-pid (foreign-procedure \"c_get_pid\" () int)))\n"
       "    (lambda ()\n"
       "      (let ((ret (c-get-pid)))\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'get-pid ret))\n"
       "        ret))))\n"
       "\n"
       /** (get-pgid) returns process group of specified process (0 = current process) */
       "(define get-pgid"
       "  (let ((c-get-pgid (foreign-procedure \"c_get_pgid\" (int) int)))\n"
       "    (lambda (pid)\n"
       "      (let ((ret (c-get-pgid pid)))\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'get-pgid ret))\n"
       "        ret))))\n"
       "\n"
       /**
        * Spawn an external program in a new background process group (pgid) and return its pid.
        *
        * Parameter program is the program path to spawn;
        * Parameter args is the list of arguments to pass to the program;
        * The parameter program and each element in args must be either a string or a bytevector.
        */
       "(define spawn-pid\n"
       "  (let ((c-spawn-pid (foreign-procedure \"c_spawn_pid\""
       "                        (scheme-object scheme-object scheme-object int) int)))\n"
       "    (lambda (program . args)\n"
       "      (let ((ret (c-spawn-pid\n"
       "                   (list->cmd-argv (cons program args))\n"
       "                   (vector 0 1 2)\n"
       "                   #f\n" /* no environment override */
       "                   0)))\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'spawn-pid ret))\n"
       "        ret))))\n"
       "\n"
       /**
        * (pid-kill pid signal-name) calls C function kill(pid, sig) i.e. sends specified signal
        * to the process(es) identified by pid.
        * Notes:
        *   pid ==  0 means "all processes in the same process group as the caller".
        *   pid == -1 means "all processes".
        *   pid <  -1 means "all processes in process group -pid"
        *
        * Returns < 0 if signal-name is unknown, or if C function kill() fails with C errno != 0.
        */
       "(define pid-kill"
       "  (let ((c-pid-kill (foreign-procedure \"c_pid_kill\" (int int) int)))\n"
       "    (lambda (pid signal-name)\n"
       "      (let ((signal-number (signal-name->number signal-name)))\n"
       "        (if (fixnum? signal-number)\n"
       "          (c-pid-kill pid signal-number)\n"
       "          -" STR_EINVAL ")))))\n"
       "\n"
       /**
        * (pid-wait pid may-block) calls waitpid(pid, WUNTRACED) i.e. checks if process specified by
        * pid exited or stopped. Notes: pid ==  0 means "any process in the same process group as
        * the caller". pid == -1 means "any child process". pid <  -1 means "any process in process
        * group -pid".
        *
        * Argument may-block must be either 'blocking or 'nonblocking.
        * If may-block is 'blocking, wait until pid (or any child process, if pid == -1) exits or
        * stops, otherwise check for such conditions without blocking.
        *
        * If waitpid() fails with C errno != 0, return < 0.
        * If no child process matches pid, or if may_block is 'nonblocking and no child exited or
        * stopped, return '().
        * Otherwise return a Scheme cons (pid . exit_flag), where exit_flag is one of:
        * process_exit_status, or 256 + signal, or 512 + stop_signal.
        */
       "(define pid-wait"
       "  (let ((c-pid-wait (foreign-procedure \"c_pid_wait\" (int int) scheme-object)))\n"
       "    (lambda (pid may-block)\n"
       "      (assert (memq may-block '(blocking nonblocking)))\n"
       "      (c-pid-wait pid (if (eq? may-block 'blocking) 1 0)))))\n"
       /**
        * Call kill() or exit() to terminate current process with job-status, which can be one of:
        *   (cons 'exited  exit-status)  ; will call C function exit(exit_status)
        *   (cons 'killed  signal-name)  ; will call C function kill(getpid(), signal_number)
        *               ; unless signal-name is one of: 'sigstop 'sigtstp 'sigcont 'sigttin 'sigttou
        *               ; if kill() returns, will call C function exit(128 + signal_number)
        *   ... any other value ... ;  will call C function exit(255)
        */
       "(define (exit-with-job-status status)\n"
#if 0 && defined(SCHEMESH_LIBRARY_FD_DEBUG)
       "  (format #t \"exit-with-job-status ~s~%\" status)\n"
#endif
       "  (let ((exit-status\n"
       "         (if (and (pair? status) (eq? 'exited (car status))\n"
       "                  (fixnum? (cdr status)) (fx=? (cdr status)\n"
       "                                               (fxand 255 (cdr status))))\n"
       "           (cdr status)\n"
       "           255)))\n"
       "    (dynamic-wind\n"
       "      void\n"       /* before body */
       "      (lambda ()\n" /* body */
       "        (when (and (pair? status) (eq? 'killed (car status)))\n"
       "          (let ((signal-name (cdr status)))\n"
       "            (unless (memq signal-name '(sigstop sigtstp sigcont\n"
       "                                          sigttin sigttou))\n"
       "              (signal-raise signal-name))\n"
       /*               process did not die with (signal-raise) */
       "            (let ((signal-number (signal-name->number signal-name)))\n"
       "              (when (fixnum? signal-number)\n"
       "                (set! exit-status (fx+ 128 signal-number)))))))\n"
       "      (lambda ()\n" /* after body */
       "        (c-exit exit-status)))))\n"
       ")\n"); /* close library */
}

int c_get_pid(void) {
  int pid = getpid();
  return pid >= 0 ? pid : c_errno();
}

int c_get_pgid(int pid) {
  int pgid = getpgid((pid_t)pid);
  return pgid >= 0 ? pgid : c_errno();
}

static int c_set_process_group(pid_t existing_pgid_if_positive) {
  int err = setpgid(0 /*current process*/, /*                                    */
                    existing_pgid_if_positive > 0 ? existing_pgid_if_positive : 0);
  return err >= 0 ? err : c_errno();
}

int c_fork_pid(ptr vector_redirect_fds, int existing_pgid_if_positive) {
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

int c_spawn_pid(ptr vector_of_bytevector0_cmdline,
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

int c_pgid_foreground(int expected_pgid, int new_pgid) {
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

ptr c_pid_wait(int pid, int may_block) {
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
