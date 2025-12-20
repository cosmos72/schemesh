/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <errno.h>
#include <sched.h> /* sched_yield() */
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h> /* int64_t */
#include <stdio.h>
#include <string.h> /* strerror() */
#include <time.h>   /* clock_gettime() */
#include <unistd.h>

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

#if 1 /* defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__) \
       */
#define ATOMIC _Atomic
#else
#define ATOMIC volatile
#endif

static int c_errno(void) {
  return -errno;
}

static const char* c_strerror(int err) {
  return strerror(err < 0 ? -err : err);
}

static int scheme2k_init_failed(const char label[]) {
  const int err = c_errno();
  fprintf(stderr,
          "error initializing POSIX subsystem: %s failed with error %s\n",
          label,
          c_strerror(err));
  return err;
}

static ATOMIC int c_sigusr1_received = 0;

static void c_sigusr1_handler(int sig_num) {
  atomic_store(&c_sigusr1_received, 1);
}

static int c_sigusr1_consume(void) {
  return atomic_exchange(&c_sigusr1_received, 0);
}

static int c_signals_init(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigusr1_handler;
  if (sigaction(SIGCHLD, &action, NULL) < 0) {
    return scheme2k_init_failed("sigaction(SIGCHLD)");
  }
  return 0;
}

static int c_signals_setdefault(void) {
  struct sigaction action = {};
  size_t           i      = 0;
  action.sa_handler       = SIG_DFL;

  if (sigaction(SIGCHLD, &action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

static int c_pid_send_signal(pid_t pid, int sig) {
  if (kill(pid, sig) < 0) {
    return c_errno();
  }
  return 0;
}

static int c_pid_fork(void) {
  const pid_t pid = fork();
  switch (pid) {
    case -1: /* fork() failed */
      return c_errno();
    case 0: /* child */
      return 0;
    default: /* parent */
      return pid;
  }
}

static int64_t c_steady_time_ns(void) {
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) < 0) {
    return (int64_t)-1;
  }
  return ts.tv_nsec + ts.tv_sec * 1000000000;
}

static ssize_t c_read(int fd, char buf[], size_t n) {
  const ssize_t got = read(fd, buf, n);
  return got >= 0 ? got : c_errno();
}

static int c_run_child(void) {
  ssize_t err;
  char    buf[1];
  for (;;) {
    err = c_read(0, buf, sizeof(buf));
    if (err > 0) {
      continue;
    } else if (err == 0 || (err < 0 && err != -EINTR)) {
      return err;
    }
    if (c_sigusr1_consume()) {
      fprintf(stdout, "signal handler executed in 0 ns\n");
    } else {
      const int64_t t1 = c_steady_time_ns();
      while (!c_sigusr1_consume()) {
        (void)sched_yield();
      }
      const int64_t t2 = c_steady_time_ns();
      fprintf(stdout, "signal handler executed in %lld ns\n", (long long)(t2 - t1));
    }
    const int64_t t2 = c_steady_time_ns();
    fflush(stdout);
  }
}

static int c_sleep(const int64_t s, const uint32_t ns) {
  struct timespec ts;
  ts.tv_sec  = s;
  ts.tv_nsec = ns;
  return nanosleep(&ts, NULL) < 0 ? c_errno() : 0;
}

static int c_run_parent(pid_t child_pid) {
  for (int i = 0; i < 10; i++) {
    const int64_t t1 = c_steady_time_ns();
    const int64_t t2 = c_steady_time_ns();
    fprintf(stdout, "clock_gettime executed in %lld ns\n", (long long)(t2 - t1));
  }
  for (;;) {
    (void)c_sleep(1, 0);
    c_pid_send_signal(child_pid, SIGCHLD);
  }
  return 0;
}

int main(int argc, char** argv) {
  int err;
  if ((err = c_signals_init()) < 0) {
    return err;
  }
#if 1
  if ((err = c_pid_fork()) < 0) {
    return err;
  }
#endif
  if (err == 0) {
    return c_run_child();
  } else {
    return c_run_parent((pid_t)err);
  }
}
