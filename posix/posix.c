/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#if 0 /* at least on FreeBSD and MacOSX, these cause more trouble than what they are worth */
#define _POSIX_C_SOURCE 200809L /* fstatat() */
#define _DEFAULT_SOURCE         /* DT_... */
#define _BSD_SOURCE             /* DT_... SIGWINCH */
#endif

#define _FILE_OFFSET_BITS 64

#include "posix.h"

#include "../containers/containers.h" /* scheme2k_Sbytevector(), scheme2k_Sstring_utf8b() */
#include "../eval.h"                  /* eval() */

#include <arpa/inet.h> /* inet_pton(), ntohs() */
#include <dirent.h>    /* opendir(), readdir(), closedir() */
#include <dirent.h>
#include <errno.h> /* EINVAL, EIO, ESRCH, errno */
#include <fcntl.h>
#include <grp.h>
#include <limits.h> /* INT_MAX, INT_MIN */
#include <netdb.h>
#include <netinet/in.h> /* struct sockaddr_in ,,, */
#include <poll.h>
#include <pthread.h> /* pthread_kill(), pthread_self() */
#include <pwd.h>
#include <pwd.h>          /* getpwnam_r(), getpwuid_r() */
#include <sched.h>        /* sched_yield() */
#include <signal.h>       /* kill(), sigaction(), SIG... */
#include <stdatomic.h>    /* _Atomic, atomic_store() */
#include <stddef.h>       /* size_t, NULL */
#include <stdint.h>       /* int64_t, uint64_t */
#include <stdio.h>        /* remove(), rename() ... */
#include <stdlib.h>       /* getenv(), strtoul() */
#include <string.h>       /* strlen(), strerror() */
#include <sys/ioctl.h>    /* ioctl(), TIOCGWINSZ */
#include <sys/resource.h> /* getrlimit(), setrlimit() */
#include <sys/socket.h>   /* getaddrinfo(), socket(), socketpair(), AF_*, SOCK_* */
#include <sys/stat.h>
#include <sys/stat.h>  /* fstatat() */
#include <sys/types.h> /* ... */
#include <sys/un.h>    /* struct sockaddr_un */
#include <sys/wait.h>  /* waitpid(), W... */
#include <time.h>
#include <time.h> /* clock_nanosleep(), CLOCK_MONOTONIC, nanosleep() */
#include <unistd.h>
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

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#undef SCHEME2K_C_DEBUG
#ifdef SCHEME2K_C_DEBUG
#define C_DEBUG_WRITE(fd, str) ((void)write(fd, str, sizeof(str) - 1))
#else
#define C_DEBUG_WRITE(fd, str) ((void)0)
#endif

#ifdef ATOMIC_INT_LOCK_FREE /* macro value does not matter */
#define ATOMIC _Atomic
#else
#define ATOMIC volatile
#endif

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

#define SCHEME2K_POSIX_POSIX_C /* tell who we are to posix/signal.h  */

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

typedef struct {
  int        val;
  const char name[12];
} namepair;

/** needed by signal.h */
static int c_errno(void);
static int c_errno_set(int errno_value);
static ptr c_namepair_list(const namepair pairs[], size_t n);

static int c_fd_open_max(void);
static int c_job_control_available(void);
static int c_signals_setdefault(void);

static ptr c_namepair_list(const namepair pairs[], size_t n) {
  ptr    ret = Snil;
  size_t i;
  for (i = 0; i < n; i++) {
    ptr name = Sstring_to_symbol(pairs[i].name);
    ptr val  = Sfixnum(pairs[i].val);
    ret      = Scons(Scons(val, name), ret);
  }
  return ret;
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
 * close-on-exec file descriptor for our tty.
 *
 * Scheme code assumes it is == c_fd_open_max() - 1
 * if c_tty_init() is successful.
 */
static int tty_fd = -1;

/** process group that started this process */
static int tty_pgid = -1;

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

static int c_errno_eagain(void) {
  return -EAGAIN;
}

static int c_errno_einprogress(void) {
  return -EINPROGRESS;
}

static int c_errno_eintr(void) {
  return -EINTR;
}

static int c_errno_einval(void) {
  return -EINVAL;
}

static int c_errno_eio(void) {
  return -EIO;
}

static int c_errno_enoent(void) {
  return -ENOENT;
}

static int c_errno_enotdir(void) {
  return -ENOTDIR;
}

static int c_errno_esrch(void) {
  return -ESRCH;
}

static const char* c_strerror(int err) {
  return strerror(err < 0 ? -err : err);
}

static ptr c_errno_to_string(int err) {
  return scheme2k_Sstring_utf8b(c_strerror(err), -1);
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

/******************************************************************************/
/*                                                                            */
/*                                include files                               */
/*                                                                            */
/******************************************************************************/

/* the files below require lots of #includes and define static functions */
#include "endpoint.h"
#include "fd.h"
#include "fs.h"
#include "pid.h"
#include "signal.h"
#include "socket.h"
#include "tty.h"

int scheme2k_init_failed(const char label[]) {
  const int err = c_errno();
  fprintf(stderr,
          "error initializing POSIX subsystem: %s failed with error %s\n",
          label,
          c_strerror(err));
  fflush(stderr);
  return err;
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
  return scheme2k_Sstring_utf8b(buf, len);
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
    ret = scheme2k_Sstring_utf8b(result->pw_name, -1);
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
    ret = scheme2k_Sstring_utf8b(result->pw_dir, -1);
  } else {
    ret = Sinteger(c_errno_set(err != 0 ? err : ENOENT));
  }
  free(buf);
  return ret;
}

static void c_sched_yield(void) {
  (void)sched_yield();
}

static uptr c_thread_count(void) {
#ifdef FEATURE_PTHREADS
  extern volatile uptr S_nthreads;
  return S_nthreads;
#else
  return 1;
#endif
}

/** must be called with locked $tc-mutex */
static ptr c_threads(void) {
  extern volatile ptr S_threads;
  return S_threads;
}

#define NOKEY INT_MIN

static const int rlimit_keys[] = {
/* order must match (rlimit-keys) */

#ifdef RLIMIT_CORE
    RLIMIT_CORE, /* coredump-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_DATA
    RLIMIT_DATA, /* data-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_NICE
    RLIMIT_NICE, /* nice */
#else
    NOKEY,
#endif

#ifdef RLIMIT_FSIZE
    RLIMIT_FSIZE, /* file-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_SIGPENDING
    RLIMIT_SIGPENDING, /* pending-signals */
#else
    NOKEY,
#endif

#ifdef RLIMIT_MEMLOCK
    RLIMIT_MEMLOCK, /* locked-memory-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_RSS
    RLIMIT_RSS, /* memory-size */
#else
    NOKEY,
#endif

#if defined(RLIMIT_NOFILE)
    RLIMIT_NOFILE, /* open-files */
#elif defined(RLIMIT_OFILE)
    RLIMIT_OFILE
#else
    NOKEY,
#endif

    NOKEY, /* pipe-size. Retrieved from PIPE_BUF, not getrlimit() */

#ifdef RLIMIT_MSGQUEUE
    RLIMIT_MSGQUEUE, /* msgqueue-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_RTPRIO
    RLIMIT_RTPRIO, /* realtime-priority */
#else
    NOKEY,
#endif

#ifdef RLIMIT_STACK
    RLIMIT_STACK, /* stack-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_CPU
    RLIMIT_CPU, /* cpu-time */
#else
    NOKEY,
#endif

#ifdef RLIMIT_NPROC
    RLIMIT_NPROC, /* user-processes */
#else
    NOKEY,
#endif

#ifdef RLIMIT_AS
    RLIMIT_AS, /* virtual-memory-size */
#else
    NOKEY,
#endif

#ifdef RLIMIT_LOCKS
    RLIMIT_LOCKS, /* file-locks */
#else
    NOKEY,
#endif

#ifdef RLIMIT_RTTIME
    RLIMIT_RTTIME, /* realtime-nonblocking-time */
#else
    NOKEY,
#endif

};

static ptr c_rlimit_keys(void) {
  ptr l = Snil;
  /* iteratively create list from tail */
  for (size_t i = N_OF(rlimit_keys); i > 0; i--) {
    const int key = rlimit_keys[i - 1];
    l             = Scons(key == NOKEY ? Sfalse : Sinteger(key), l);
  }
  return l;
}

#undef NOKEY

static ptr c_rlimit_get(int is_hard, int resource) {
  struct rlimit lim;
  int           err = getrlimit(resource, &lim);
  if (err < 0) {
    return Sinteger(c_errno());
  }
  uint64_t c_value = is_hard ? lim.rlim_max : lim.rlim_cur;
  return c_value == RLIM_INFINITY ? Strue : Sunsigned64(c_value);
}

static int c_rlimit_set(int is_hard, int resource, ptr value) {
  struct rlimit lim;
  uint64_t      c_value = (value == Strue) ? RLIM_INFINITY : Sunsigned64_value(value);
  int           err     = getrlimit(resource, &lim);
  if (err < 0) {
    return c_errno();
  }
  if (is_hard) {
    lim.rlim_max = c_value;
  } else {
    lim.rlim_cur = c_value;
  }
  err = setrlimit(resource, &lim);
  return err < 0 ? c_errno() : 0;
}

/**
 * return i-th environment variable i.e. environ[i]
 * converted to a cons containing two Scheme strings: (key . value)
 *
 * if environ[i] is NULL, return #f
 */
static ptr c_environ_ref(uptr i) {
  const char* entry = environ[i];
  const char* separator;
  if (entry && (separator = strchr(entry, '=')) != NULL) {
    size_t namelen  = separator - entry;
    iptr   inamelen = Sfixnum_value(Sfixnum(namelen));
    if (namelen > 0 && inamelen > 0 && namelen == (size_t)inamelen) {
      return Scons(scheme2k_Sstring_utf8b(entry, namelen),
                   scheme2k_Sstring_utf8b(separator + 1, -1));
    }
  }
  return Sfalse;
}

int scheme2k_register_c_functions(void) {
  int err;
  if ((err = c_tty_init()) < 0) {
    return err;
  }
  if ((err = c_register_c_functions_posix_signals()) < 0) {
    return err;
  }

  scheme2k_register_c_functions_containers();

  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_errno_eagain", &c_errno_eagain);
  Sregister_symbol("c_errno_einprogress", &c_errno_einprogress);
  Sregister_symbol("c_errno_eintr", &c_errno_eintr);
  Sregister_symbol("c_errno_einval", &c_errno_einval);
  Sregister_symbol("c_errno_eio", &c_errno_eio);
  Sregister_symbol("c_errno_enoent", &c_errno_enoent);
  Sregister_symbol("c_errno_enotdir", &c_errno_enotdir);
  Sregister_symbol("c_errno_esrch", &c_errno_esrch);
  Sregister_symbol("c_errno_to_string", &c_errno_to_string);

  Sregister_symbol("c_chdir", &c_chdir);
  Sregister_symbol("c_dir_get_entry", &c_dir_get_entry);
  /* Sregister_symbol("c_dir_open", &c_dir_open); */
  /* Sregister_symbol("c_dir_close", &c_dir_close); */
  Sregister_symbol("c_environ_ref", &c_environ_ref);
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
  Sregister_symbol("c_fd_nonblock_get", &c_fd_nonblock_get);
  Sregister_symbol("c_fd_nonblock_set", &c_fd_nonblock_set);
  Sregister_symbol("c_fd_redirect", &c_fd_redirect);
  Sregister_symbol("c_file_fd", &c_file_fd);
  Sregister_symbol("c_pipe_fds", &c_pipe_fds);

  Sregister_symbol("c_endpoint_inet", &c_endpoint_inet);
  Sregister_symbol("c_endpoint_inet6", &c_endpoint_inet6);
  Sregister_symbol("c_endpoint_unix", &c_endpoint_unix);
  Sregister_symbol("c_endpoint_unix_path_max", &c_endpoint_unix_path_max);
  Sregister_symbol("c_hostname_to_endpoint", &c_hostname_to_endpoint);
  Sregister_symbol("c_hostname_to_endpoint_list", &c_hostname_to_endpoint_list);
  Sregister_symbol("c_hostname_error_to_string", &c_hostname_error_to_string);
  Sregister_symbol("c_socket_accept", &c_socket_accept);
  Sregister_symbol("c_socket_bind", &c_socket_bind);
  Sregister_symbol("c_socket_connect", &c_socket_connect);
  Sregister_symbol("c_socket_listen", &c_socket_listen);
  Sregister_symbol("c_socket_fd", &c_socket_fd);
  Sregister_symbol("c_socket_endpoint2", &c_socket_endpoint2);
  Sregister_symbol("c_socket_family_list", &c_socket_family_list);
  Sregister_symbol("c_socket_type_list", &c_socket_type_list);
  Sregister_symbol("c_socketpair_fds", &c_socketpair_fds);

  Sregister_symbol("c_tty_restore", &c_tty_restore);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_size", &c_tty_size);
  Sregister_symbol("c_job_control_available", &c_job_control_available);
  Sregister_symbol("c_job_control_change", &c_job_control_change);

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

  Sregister_symbol("c_thread_count", &c_thread_count);
  Sregister_symbol("c_threads", &c_threads);
  Sregister_symbol("c_sched_yield", &c_sched_yield);
  Sregister_symbol("c_rlimit_keys", &c_rlimit_keys);
  Sregister_symbol("c_rlimit_get", &c_rlimit_get);
  Sregister_symbol("c_rlimit_set", &c_rlimit_set);

  return 0;
}

void scheme2k_init(const char* override_boot_dir, void (*on_scheme_exception)(void)) {
  int loaded = 0;

  c_signals_unblock();

  Sscheme_init(on_scheme_exception);
  if (override_boot_dir != NULL) {
    size_t dir_len   = strlen(override_boot_dir);
    char*  boot_file = (char*)malloc(dir_len + 13);
    if (boot_file != NULL) {
      memcpy(boot_file, override_boot_dir, dir_len);
      memcpy(boot_file + dir_len, "/petite.boot", 13);
      Sregister_boot_file(boot_file);

      memcpy(boot_file + dir_len, "/scheme.boot", 13);
      Sregister_boot_file(boot_file);
      loaded = 1;
    }
  }
  if (loaded == 0) {
    Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
    Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  }
  Sbuild_heap(NULL, NULL);
}

/**
 * quit Chez Scheme. calls:
 *   c_tty_quit()
 *   Sscheme_deinit()
 */
void scheme2k_quit(void) {
  c_tty_quit();
  Sscheme_deinit();
}
