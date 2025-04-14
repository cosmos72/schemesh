/**
 * Written in 2025 by Massimiliano Ghilardi <massimiliano.ghilardi@gmail.com>
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication along with
 * this software. If not, see <https://creativecommons.org/publicdomain/zero/1.0/>.
 */

/**
 * Pause for user-specified number of seconds.
 *
 * The number of seconds to pause include only the interval this program is running:
 * if suspended with CTRL+Z or SIGTSTP, the suspended duration is not counted.
 *
 * This effectively works as a countdown from NUMBER seconds to zero,
 * that can be suspended with CTRL+Z or SIGTSTP and resumed by continuing this program.
 */

#include <errno.h>     /* errno */
#include <signal.h>    /* sigaction() */
#include <stdatomic.h> /* _Atomic, atomic_store(), atomic_exchange() */
#include <stdio.h>     /* fprintf(), stderr() */
#include <stdlib.h>    /* strtod() */
#include <string.h>    /* strerror() */
#include <time.h>      /* clock_gettime(), clock_nanosleep() */

typedef struct timespec timespec;

static int c_fail(const char label[], int err) {
  fprintf(stderr, "%s failed with error %d: %s\n", label, err, strerror(err));
  return err;
}

static _Atomic int c_sigtstp_received = 0;

static void c_sigtstp_handler(int sig) {
  (void)sig;
  atomic_store(&c_sigtstp_received, 1);
}

static void c_sigtstp_sethandler(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigtstp_handler;
  (void)sigaction(SIGTSTP, &action, NULL);
}

static void c_sigtstp_setdefault(void) {
  struct sigaction action = {};
  action.sa_handler       = SIG_DFL;
  (void)sigaction(SIGTSTP, &action, NULL);
}

static int c_countdown(timespec interval) {
  timespec left = {};
  int      err;
  while ((interval.tv_sec > 0 || (interval.tv_sec == 0 && interval.tv_nsec > 0))) {
    c_sigtstp_sethandler();

    /* temporary workaround. How to test for clock_nanosleep() availability? */
#if defined(CLOCK_MONOTONIC) && !defined(__APPLE__)
    err = clock_nanosleep(CLOCK_MONOTONIC, 0, &interval, &left);
#else
    if (nanosleep(&interval, &left) != 0) {
      err = errno;
    }
#endif
    if (err == 0) {
      break;
    } else if (err != EINTR) {
#if defined(CLOCK_MONOTONIC) && !defined(__APPLE__)
      return c_fail("clock_nanosleep(CLOCK_MONOTONIC)", err);
#else
      return c_fail("nanosleep()", err);
#endif
    }
    interval = left;
    if (atomic_exchange(&c_sigtstp_received, 0)) {
      c_sigtstp_setdefault();
      raise(SIGTSTP);
    }
  }
  return 0;
}

static int help(const char* name) {
  if (!name || !name[0]) {
    name = "countdown";
  }
  fprintf(stdout, "%s: missing argument.\nType '%s --help' for more information.\n", name, name);
  return 0;
}

static int usage(const char* name) {
  if (!name || !name[0]) {
    name = "countdown";
  }
  fprintf(stdout,
          "Usage: %s NUMBER\n%s",
          name,
          "Pause for NUMBER seconds.\n"
          "\n"
          "The number of seconds to pause include only the interval this program is running:\n"
          "if suspended with CTRL+Z or SIGTSTP, the suspended duration is not counted.\n"
          "\n"
          "This effectively works as a countdown from NUMBER seconds to zero,\n"
          "that can be suspended with CTRL+Z or SIGTSTP and resumed by continuing this program.\n");
  return 0;
}

int main(int argc, char** argv) {
  double seconds = 0.0;
  if (argc <= 1) {
    return help(argv[0]);
  } else if (!strcmp(argv[1], "--help")) {
    return usage(argv[0]);
  } else {
    char* end = NULL;
    seconds   = strtod(argv[1], &end);
    if (*end) {
      seconds = 0.0;
    }
  }
  if (seconds > 0.0) {
    timespec interval;
    interval.tv_sec  = (time_t)seconds;
    interval.tv_nsec = (long)(0.5 + 1e9 * (seconds - (double)interval.tv_sec));
    return c_countdown(interval);
  }
  return 0;
}
