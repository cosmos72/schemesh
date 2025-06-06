/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

/** this file should be included only by posix/posix.c */
#ifndef SCHEMESH_POSIX_POSIX_C
#error "posix/signal.h should only be #included by posix/posix.c"
#endif

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

#ifdef ATOMIC_INT_LOCK_FREE /* macro value does not matter */
#define ATOMIC _Atomic
#else
#define ATOMIC volatile
#endif

#undef SCHEMESH_C_DEBUG
#ifdef SCHEMESH_C_DEBUG
#define C_DEBUG_WRITE(fd, str) ((void)write(fd, str, sizeof(str) - 1))
#else
#define C_DEBUG_WRITE(fd, str) ((void)0)
#endif

/* ------------------------------------ signals handling ---------------------------------------- */

static int c_thread_signal_setblocked(int sig, int block);

static const int signals_tohandle[] = {
    SIGCHLD, /* c_signals_setdefault() assumes SIGCHLD is the first */
    SIGALRM,
    SIGPIPE,
    SIGQUIT,
    SIGTERM,
    SIGTSTP,
    SIGTTIN,
    SIGTTOU,
    SIGUSR1,
    SIGUSR2,
#ifdef SIGPWR
    SIGPWR,
#endif
#ifdef SIGURG
    SIGURG,
#endif
#ifdef SIGVTALRM
    SIGVTALRM,
#endif
};

static int c_signals_init(void) {
  struct sigaction         action   = {};
  static const char* const labels[] = {
      "sigaction(SIGCHLD, SIG_DFL)",
      "sigaction(SIGALRM, SIG_IGN)",
      "sigaction(SIGPIPE, SIG_IGN)",
      "sigaction(SIGQUIT, SIG_IGN)",
      "sigaction(SIGTERM, SIG_IGN)",
      "sigaction(SIGTSTP, SIG_IGN)",
      "sigaction(SIGTTIN, SIG_IGN)",
      "sigaction(SIGTTOU, SIG_IGN)",
      "sigaction(SIGUSR1, SIG_IGN)",
      "sigaction(SIGUSR2, SIG_IGN)",
#ifdef SIGPWR
      "sigaction(SIGPWR,  SIG_IGN)",
#endif
#ifdef SIGURG
      "sigaction(SIGURG,  SIG_IGN)",
#endif
#ifdef SIGVTALRM
      "sigaction(SIGVTALRM, SIG_IGN)",
#endif
  };
  size_t i = 0;

  for (i = 0; i < N_OF(signals_tohandle); i++) {
    /* cannot ignore SIGCHLD, it would break waitpid() */
    action.sa_handler = i == 0 ? SIG_DFL : SIG_IGN;
    if (sigaction(signals_tohandle[i], &action, NULL) < 0) {
      return c_init_failed(labels[i]);
    }
  }
  return 0;
}

static int c_signals_setdefault(void) {
  struct sigaction action = {};
  size_t           i      = 0;
  action.sa_handler       = SIG_DFL;

  /* keep current SIGCHLD handler, subshells need it */
  for (i = 1; i < N_OF(signals_tohandle); i++) {
    if (sigaction(signals_tohandle[i], &action, NULL) < 0) {
      return c_errno();
    }
  }
  return 0;
}

static int c_signal_setdefault(int sig) {
  struct sigaction action = {};
  action.sa_handler       = SIG_DFL;

  if (sigaction(sig, &action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

static const struct {
  int         sig;
  const char* name;
} signals_known[] = {
    {SIGABRT, "sigabrt"},     {SIGALRM, "sigalrm"}, {SIGBUS, "sigbus"},   {SIGCHLD, "sigchld"},
    {SIGCONT, "sigcont"},     {SIGFPE, "sigfpe"},   {SIGHUP, "sighup"},   {SIGILL, "sigill"},
    {SIGINT, "sigint"},       {SIGKILL, "sigkill"}, {SIGPIPE, "sigpipe"}, {SIGQUIT, "sigquit"},
    {SIGSEGV, "sigsegv"},     {SIGSTOP, "sigstop"}, {SIGTERM, "sigterm"}, {SIGTSTP, "sigtstp"},
    {SIGTTIN, "sigttin"},     {SIGTTOU, "sigttou"}, {SIGUSR1, "sigusr1"}, {SIGUSR2, "sigusr2"},

    {SIGWINCH, "sigwinch"}, /* not fully standard, but we need it */

#ifdef SIGINFO /* synonym for SIGPWR */
    {SIGINFO, "siginfo"},
#endif
#ifdef SIGIO
    {SIGIO, "sigio"},
#endif
#ifdef SIGPOLL
    {SIGPOLL, "sigpoll"},
#endif
#ifdef SIGPROF
    {SIGPROF, "sigprof"},
#endif
#ifdef SIGPWR
    {SIGPWR, "sigpwr"},
#endif
#ifdef SIGSTKFLT
    {SIGSTKFLT, "sigstkflt"},
#endif
#ifdef SIGSYS
    {SIGSYS, "sigsys"},
#endif
#ifdef SIGTRAP
    {SIGTRAP, "sigtrap"},
#endif
#ifdef SIGURG
    {SIGURG, "sigurg"},
#endif
#ifdef SIGVTALRM
    {SIGVTALRM, "sigvtalrm"},
#endif
#ifdef SIGXCPU
    {SIGXCPU, "sigxcpu"},
#endif
#ifdef SIGXFSZ
    {SIGXFSZ, "sigxfsz"},
#endif
};

/**
 * return a Scheme list containing pairs (sig . name)
 * where sig is a fixnum and name is a symbol
 */
static ptr c_signals_list(void) {

  ptr    ret = Snil;
  size_t i;
  for (i = 0; i < N_OF(signals_known); i++) {
    ptr name = Sstring_to_symbol(signals_known[i].name);
    ptr num  = Sfixnum(signals_known[i].sig);
    ret      = Scons(Scons(num, name), ret);
  }
  return ret;
}

/**
 * if unset_sighandler != 0, set handler for signal sig to SIG_DFL then unblock the signal.
 *
 * raise signal sig.
 */
static int c_thread_signal_raise(int sig, int unset_sighandler) {
  int err;
  if (unset_sighandler) {
    (void)c_signal_setdefault(sig);
  }
  (void)c_thread_signal_setblocked(sig, 0);

  if ((err = pthread_kill(pthread_self(), sig)) <
      0) { /* better than kill(getpid(), sig) in multi-threaded-programs */
    return c_errno();
  }
  return 0;
}

/**
 * if block != 0, add signal sig to the set of blocked signals for caller's thread.
 * otherwise remove sig from the set of blocked signals for caller's thread
 */
static int c_thread_signal_setblocked(int sig, int block) {
  sigset_t sigset;
  sigemptyset(&sigset);
  sigaddset(&sigset, sig);
  if (pthread_sigmask(block ? SIG_BLOCK : SIG_UNBLOCK, &sigset, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

/**
 * unblock most signals: we need to receive them in main thread
 * Exception: if SIGHUP is blocked, keep it blocked.
 */
static int c_signals_unblock_most(void) {
  sigset_t sigset;
  size_t   i = 0;

  sigemptyset(&sigset);
  for (i = 0; i < N_OF(signals_known); i++) {
    const int sig = signals_known[i].sig;
    if (sig != SIGHUP) {
      sigaddset(&sigset, sig);
    }
  }
  if (sigprocmask(SIG_UNBLOCK, &sigset, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

/**
 * block most signals in caller's thread: we need to receive them in main thread
 * Exception: allow receiving SIGCONT in caller's thread
 */
static int c_thread_signals_block_most(void) {
  static const int block_signals[] = {SIGHUP, SIGINT, SIGQUIT, SIGPIPE, SIGCHLD, SIGWINCH};
  sigset_t         sigset;
  size_t           i;

  sigemptyset(&sigset);
  for (i = 0; i < N_OF(block_signals); i++) {
    sigaddset(&sigset, block_signals[i]);
  }
  if (pthread_sigmask(SIG_BLOCK, &sigset, NULL) < 0) {
    return c_errno();
  }

  sigemptyset(&sigset);
  sigaddset(&sigset, SIGCONT);
  if (pthread_sigmask(SIG_UNBLOCK, &sigset, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

/* ------------------------------------ SIGCONT handling ---------------------------------------- */

/**
 * do-nothing handler for SIGCONT
 * used to react to SIGCONT in secondary threads and interrupt blocking system calls
 */
static void c_sigcont_handler(int sig) {
  (void)sig;
}

/**
 * install a do-nothing signal handler for SIGCONT.
 * used to react to SIGCONT in secondary threads and interrupt blocking system calls
 */
static int c_signal_init_sigcont(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigcont_handler;
  if (sigaction(SIGCONT, &action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

/* ------------------------------------ SIGWINCH handling --------------------------------------- */

static ATOMIC int c_sigwinch_received = 0;

static void c_sigwinch_handler(int sig) {
  (void)sig;
  atomic_store(&c_sigwinch_received, 1);
}

static ptr c_signal_consume_sigwinch(void) {
  return atomic_exchange(&c_sigwinch_received, 0) ? Strue : Sfalse;
}

static struct sigaction c_sigwinch_saved_action;

static int c_signal_init_sigwinch(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigwinch_handler;
  if (sigaction(SIGWINCH, &action, &c_sigwinch_saved_action) < 0) {
    c_sigwinch_saved_action.sa_handler = SIG_DFL;
    return c_init_failed("sigaction(SIGWINCH)");
  }
  return 0;
}

static int c_signal_restore_sigwinch(void) {
  if (sigaction(SIGWINCH, &c_sigwinch_saved_action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

/* ------------------------------------- sleep -------------------------------------------------- */

/**
 * Pause for user-specified duration.
 *
 * The duration to pause include only the interval this program is running:
 * if suspended with CTRL+Z or SIGTSTP, the suspended interval is not counted.
 *
 * This effectively works as a countdown from NUMBER seconds to zero,
 * that can be suspended with CTRL+Z or SIGTSTP and resumed by continuing this program.
 *
 * duration_inout must be a pair (seconds_int64 . nanoseconds_int32)
 * and if the returned value is > 0, duration it will be updated
 * with the remaining duration to sleep.
 */
static int c_countdown(ptr duration_inout) {
  struct timespec duration, left = {};
  int             err;
  if (!Spairp(duration_inout)) {
    return c_errno_set(EINVAL);
  }
  duration.tv_sec  = Sinteger64_value(Scar(duration_inout));
  duration.tv_nsec = Sinteger32_value(Scdr(duration_inout));
  if (duration.tv_sec < 0 || (duration.tv_sec == 0 && duration.tv_nsec <= 0)) {
    return 0;
  }
  /* macOS lacks clock_nanosleep(). How to test for clock_nanosleep() availability? */
#if defined(CLOCK_MONOTONIC) && !defined(__APPLE__)
  err = clock_nanosleep(CLOCK_MONOTONIC, 0, &duration, &left);
#else
  if ((err = nanosleep(&duration, &left)) != 0) {
    err = errno;
  }
#endif
  if (err == 0) {
    return 0;
  } else if (err != EINTR) {
    return c_errno_set(err);
  }
  Sset_car(duration_inout, Sinteger64(left.tv_sec));
  Sset_cdr(duration_inout, Sinteger32(left.tv_nsec));
  return 1;
}

static int c_register_c_functions_posix_signals(void) {
  if (c_signal_init_sigcont() < 0) {
    return c_init_failed("sigaction(SIGCONT)");
  }
  if (c_signals_unblock_most() < 0) {
    return c_init_failed("sigprocmask(SIG_UNBLOCK)");
  }
  Sregister_symbol("c_countdown", &c_countdown);
  Sregister_symbol("c_signals_list", &c_signals_list);
  Sregister_symbol("c_thread_signal_raise", &c_thread_signal_raise);
  Sregister_symbol("c_signal_setdefault", &c_signal_setdefault);
  Sregister_symbol("c_signal_consume_sigwinch", &c_signal_consume_sigwinch);
  Sregister_symbol("c_signal_init_sigwinch", &c_signal_init_sigwinch);
  Sregister_symbol("c_signal_restore_sigwinch", &c_signal_restore_sigwinch);
  Sregister_symbol("c_thread_signals_block_most", &c_thread_signals_block_most);
  return 0;
}

#undef N_OF
