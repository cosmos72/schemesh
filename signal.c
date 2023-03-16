/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <signal.h>
#include <stdatomic.h>
#include <stddef.h> // size_t, NULL

#include "eval.h"
#include "posix.h"
#include "signal.h"

#define N_OF(array) (sizeof(array) / sizeof((array)[0]))

static volatile int c_sigchld_received = 0;

static void c_sigchld_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigchld_received, 1);
}

int c_sigchld_consume(void) {
  return atomic_exchange(&c_sigchld_received, 0);
}

static const int signals_tohandle[] = {SIGCHLD, SIGTSTP, SIGTTOU};

int c_signals_init(void) {
  struct sigaction   action   = {};
  static const char* labels[] = {
      "sigaction(SIGCHLD)",
      "sigaction(SIGTSTP, SIG_IGN)",
      "sigaction(SIGTTOU, SIG_IGN)",
  };
  typedef void (*signal_handler_func)(int);
  static const signal_handler_func handlers[] = {
      &c_sigchld_handler,
      SIG_IGN,
      SIG_IGN,
  };
  size_t i;

  for (i = 0; i < N_OF(signals_tohandle); i++) {
    action.sa_handler = handlers[i];
    if (sigaction(signals_tohandle[i], &action, NULL) < 0) {
      return c_print_errno(labels[i]);
    }
  }
  return 0;
}

int c_signals_restore(void) {
  struct sigaction action = {};
  size_t           i;
  action.sa_handler = SIG_DFL;

  for (i = 0; i < N_OF(signals_tohandle); i++) {
    if (sigaction(signals_tohandle[i], &action, NULL) < 0) {
      return c_errno();
    }
  }
  return 0;
}

int c_signal_restore(int sig) {
  struct sigaction action = {};
  action.sa_handler       = SIG_DFL;

  if (sigaction(sig, &action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

#define STR_(arg) #arg
#define x(arg) STR_(arg)

void define_signal_functions(void) {
  /* clang-format off */
  eval("(define signal-table-number->name\n"
       "  (vector->hashtable '#("
       " (" x(SIGHUP)  " . sighup)"
       " (" x(SIGINT)  " . sigint)"
       " (" x(SIGQUIT) " . sigquit)"
       " (" x(SIGILL)  " . sigill)"
       " (" x(SIGTRAP) " . sigtrap)"
       " (" x(SIGABRT) " . sigabrt)"
       " (" x(SIGBUS)  " . sigbus)"
       " (" x(SIGFPE)  " . sigfpe)"
       " (" x(SIGKILL) " . sigkill)"
       " (" x(SIGUSR1) " . sigusr1)"
       " (" x(SIGSEGV) " . sigsegv)"
       " (" x(SIGUSR2) " . sigusr2)"
       " (" x(SIGPIPE) " . sigpipe)"
       " (" x(SIGALRM) " . sigalrm)"
       " (" x(SIGTERM) " . sigterm)"
       " (" x(SIGSTKFLT) " . sigstkflt)"
       " (" x(SIGCHLD) " . sigchld)"
       " (" x(SIGCONT) " . sigcont)"
       " (" x(SIGSTOP) " . sigstop)"
       " (" x(SIGTSTP) " . sigtstp)"
       " (" x(SIGTTIN) " . sigttin)"
       " (" x(SIGTTOU) " . sigttou)"
       " (" x(SIGURG)  " . sigurg)"
       " (" x(SIGXCPU) " . sigxcpu)"
       " (" x(SIGXFSZ) " . sigxfsz)"
       " (" x(SIGVTALRM) " . sigvtalrm)"
       " (" x(SIGPROF) " . sigprof)"
       " (" x(SIGWINCH) " . sigwinch)"
       " (" x(SIGIO)   " . sigio)"
       " (" x(SIGPWR)  " . sigpwr)"
       " (" x(SIGSYS)  " . sigsys))\n"
       "    (make-eq-hashtable)))\n");
  /* clang-format on */

  eval("(define signal-table-name->number\n"
       "  (hashtable-transpose\n"
       "    signal-table-number->name\n"
       "    (make-eq-hashtable)))\n");

  eval("(define (signal-number->name number)\n"
       "  (hashtable-ref signal-table-number->name number #f))\n");

  eval("(define (signal-name->number name)\n"
       "  (hashtable-ref signal-table-name->number name #f)))\n");
}
