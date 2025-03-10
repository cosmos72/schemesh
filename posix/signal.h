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
#error "posix/signal.h should only be #included by posix/posix.h"
#endif

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

#if 1 /* defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_ATOMICS__) \
       */
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

static struct sigaction c_sigint_oldaction;

static ATOMIC int c_sigint_received   = 0;
static ATOMIC int c_sigchld_received  = 0;
static ATOMIC int c_sigtstp_received  = 0;
static ATOMIC int c_sigwinch_received = 0;

static ATOMIC int c_sigint_consumed_while_pending = 0;

static void c_sched_yield(void) {
  /* this is a workaround */
  (void)sched_yield();
}

static void c_sigchld_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigchld_received, 1);
}

static void c_sigint_handler(int sig_num) {
  /**
   * Chez Scheme sometimes blocks SIGINT.
   *
   * c_sigint_consume() may be called while SIGINT is blocked and pending,
   * i.e. before this handler is called, and for correctness it must return true.
   *
   * But that "consumes" SIGINT from schemesh point of view,
   * thus when c_sigint_handler() is eventually called,
   * it must NOT set c_sigint_received = 1.
   *
   * Use a flag c_sigint_consumed_while_pending to recognize this case
   * and handle it correctly.
   */
  C_DEBUG_WRITE(1, "-> c_sigint_handler\n");
  if (atomic_exchange(&c_sigint_consumed_while_pending, 0) == 0) {
    atomic_store(&c_sigint_received, 1);
    C_DEBUG_WRITE(1, "...c_sigint_handler: set c_sigint_received = 1\n");
  } else {
    C_DEBUG_WRITE(1, "...c_sigint_handler: set consumed_while_pending = 0\n");
  }
  C_DEBUG_WRITE(1, "<- c_sigint_handler\n");
  if (c_sigint_oldaction.sa_handler) {
    /* daisy-chain Chez Scheme SIGINT signal handler, as in the old DOS times */
    (*c_sigint_oldaction.sa_handler)(sig_num);
  }
}

static void c_sigtstp_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigtstp_received, 1);
  /*
   * do NOT directly call Chez Scheme SIGINT signal handler:
   * it may have blocked SIGINT and not expect the handler to be called.
   */
  (void)raise(SIGINT);
}

static void c_sigwinch_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigwinch_received, 1);
}

/**
 * Chez Scheme sometimes blocks SIGINT.
 *
 * If SIGINT is pending, mark it as "consumed" by setting
 * c_sigint_consumed_while_pending = 1, then return Strue.
 * Otherwise return Sfalse.
 */
static ptr c_sigint_consume_while_pending() {
  sigset_t set;
  int      err = sigpending(&set);
  int      ret = sigismember(&set, SIGINT);
#ifdef SCHEMESH_C_DEBUG
  fprintf(stdout, "<- c_sigint_consume.while_pending: err = %d, ret = %d\n", err, ret);
  fflush(stdout);
#endif
  if (err == 0 && ret > 0) {
    atomic_store(&c_sigint_consumed_while_pending, 1);
    return Strue;
  }
  return Sfalse;
}

static ptr c_sigint_consume(void) {
  C_DEBUG_WRITE(1, "-> c_sigint_consume\n");
  if (atomic_exchange(&c_sigint_received, 0)) {
    C_DEBUG_WRITE(1, "<- c_sigint_consume: ret = Strue\n");
    return Strue;
  }
  return c_sigint_consume_while_pending();
}

static ptr c_sigchld_consume(void) {
  return atomic_exchange(&c_sigchld_received, 0) ? Strue : Sfalse;
}

static ptr c_sigtstp_consume(void) {
  if (atomic_exchange(&c_sigtstp_received, 0)) {
    /**
     * c_sigtstp_handler above handler intentionally raises SIGINT
     * to reuse Chez Scheme detection of signals: consume it,
     * otherwise we will think that a SIGINT was received,
     * and kill some job or interrupt some procedure.
     */
    atomic_store(&c_sigint_received, 0);
    return Strue;
  }
  return Sfalse;
}

static ptr c_sigwinch_consume(void) {
  return atomic_exchange(&c_sigwinch_received, 0) ? Strue : Sfalse;
}

static const int signals_tohandle[] = {SIGINT, SIGCHLD, SIGTSTP, SIGTTIN, SIGTTOU};

static int c_signals_init(void) {
  struct sigaction   action   = {};
  static const char* labels[] = {
      "sigaction(SIGINT)",
      "sigaction(SIGCHLD)",
      "sigaction(SIGTSTP)",
      "sigaction(SIGTTIN, SIG_IGN)",
      "sigaction(SIGTTOU, SIG_IGN)",
  };
  typedef void (*signal_handler_func)(int);
  static const signal_handler_func handlers[] = {
      &c_sigint_handler,
      &c_sigchld_handler,
      &c_sigtstp_handler,
      SIG_IGN,
      SIG_IGN,
  };
  size_t i = 0;

#define SCHEMESH_HANDLE_SIGINT
#ifdef SCHEMESH_HANDLE_SIGINT
  action.sa_handler = handlers[i];
  if (sigaction(signals_tohandle[i], &action, &c_sigint_oldaction) < 0) {
    return c_init_failed(labels[i]);
  }
#endif

  for (i = 1; i < N_OF(signals_tohandle); i++) {
    action.sa_handler = handlers[i];
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

#ifdef C_HANDLE_SIGINT
  if (sigaction(signals_tohandle[i],
                c_sigint_oldaction.sa_handler ? &c_sigint_oldaction : &action,
                NULL) < 0) {
    return c_errno();
  }
#endif
  /* keep SIGCHLD handler */
  for (i = 2; i < N_OF(signals_tohandle); i++) {
    if (sigaction(signals_tohandle[i], &action, NULL) < 0) {
      return c_errno();
    }
  }
  return 0;
}

static struct sigaction c_sigwinch_saved_action;

static int c_sigwinch_init(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigwinch_handler;
  if (sigaction(SIGWINCH, &action, &c_sigwinch_saved_action) < 0) {
    c_sigwinch_saved_action.sa_handler = SIG_DFL;
    return c_init_failed("sigaction(SIGWINCH)");
  }
  return 0;
}

static int c_sigwinch_restore(void) {
  if (sigaction(SIGWINCH, &c_sigwinch_saved_action, NULL) < 0) {
    return c_errno();
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

static int c_signal_raise(int sig) {
  if (sig == SIGTTIN || sig == SIGTTOU) {
    (void)c_signal_setdefault(sig);
  }
  if (raise(sig) < 0) { /* better than kill(getpid(), sig) in multi-threaded-programs */
    return c_errno();
  }
  return 0;
}

/**
 * return a Scheme list containing pairs (sig_num . sig_name)
 * where sig_num is a fixnum and sig_name is a symbol
 */
static ptr c_signals_list(void) {
  static const struct {
    int         sig_num;
    const char* sig_name;
  } sigtable[] = {
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
#ifdef SIGPWR
      {SIGPWR, "sigpwr"},
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

  ptr    ret = Snil;
  size_t i;
  for (i = 0; i < sizeof(sigtable) / sizeof(sigtable[0]); i++) {
    ptr name = Sstring_to_symbol(sigtable[i].sig_name);
    ptr num  = Sfixnum(sigtable[i].sig_num);
    ret      = Scons(Scons(num, name), ret);
  }
  return ret;
}

static void c_register_c_functions_posix_signals(void) {
  Sregister_symbol("c_sched_yield", &c_sched_yield);
  Sregister_symbol("c_signals_list", &c_signals_list);
  Sregister_symbol("c_signal_raise", &c_signal_raise);
  Sregister_symbol("c_sigint_consume", &c_sigint_consume);
  Sregister_symbol("c_sigchld_consume", &c_sigchld_consume);
  Sregister_symbol("c_sigtstp_consume", &c_sigtstp_consume);
  Sregister_symbol("c_sigwinch_consume", &c_sigwinch_consume);
  Sregister_symbol("c_sigwinch_init", &c_sigwinch_init);
  Sregister_symbol("c_sigwinch_restore", &c_sigwinch_restore);
}

#undef N_OF
