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

int c_signal_raise(int sig) {
  (void)c_signal_restore(sig);
  int pid = c_get_pid();
  if (pid <= 0 || kill(pid, sig) < 0) {
    return c_errno();
  }
  return 0;
}

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

void define_signal_functions(void) {
  Sregister_symbol("c_signal_raise", &c_signal_raise);

  /* clang-format off */
  eval("(define signal-table-number->name\n"
       "  (vector->hashtable '#("
       " (" STR(SIGHUP)  " . sighup)"
       " (" STR(SIGINT)  " . sigint)"
       " (" STR(SIGQUIT) " . sigquit)"
       " (" STR(SIGILL)  " . sigill)"
       " (" STR(SIGTRAP) " . sigtrap)"
       " (" STR(SIGABRT) " . sigabrt)"
       " (" STR(SIGBUS)  " . sigbus)"
       " (" STR(SIGFPE)  " . sigfpe)"
       " (" STR(SIGKILL) " . sigkill)"
       " (" STR(SIGUSR1) " . sigusr1)"
       " (" STR(SIGSEGV) " . sigsegv)"
       " (" STR(SIGUSR2) " . sigusr2)"
       " (" STR(SIGPIPE) " . sigpipe)"
       " (" STR(SIGALRM) " . sigalrm)"
       " (" STR(SIGTERM) " . sigterm)"
       " (" STR(SIGSTKFLT) " . sigstkflt)"
       " (" STR(SIGCHLD) " . sigchld)"
       " (" STR(SIGCONT) " . sigcont)"
       " (" STR(SIGSTOP) " . sigstop)"
       " (" STR(SIGTSTP) " . sigtstp)"
       " (" STR(SIGTTIN) " . sigttin)"
       " (" STR(SIGTTOU) " . sigttou)"
       " (" STR(SIGURG)  " . sigurg)"
       " (" STR(SIGXCPU) " . sigxcpu)"
       " (" STR(SIGXFSZ) " . sigxfsz)"
       " (" STR(SIGVTALRM) " . sigvtalrm)"
       " (" STR(SIGPROF) " . sigprof)"
       " (" STR(SIGWINCH) " . sigwinch)"
       " (" STR(SIGIO)   " . sigio)"
       " (" STR(SIGPWR)  " . sigpwr)"
       " (" STR(SIGSYS)  " . sigsys))\n"
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

  /**
   * (signal-raise signal-name) calls C functions sigaction(sig, SIG_DFL),
   * then calls C function kill(getpid(), sig)
   * i.e. sends specified signal to the process itself.
   *
   * Returns < 0 if signal-name is unknown, or if C functions getpid() or kill()
   * fail with C errno != 0.
   */
  eval("(define signal-raise"
       "  (let ((c-signal-raise (foreign-procedure \"c_signal_raise\" (int) int)))\n"
       "    (lambda (signal-name)\n"
       "      (let ((signal-number (signal-name->number signal-name)))\n"
       "        (if (fixnum? signal-number)\n"
       "          (c-signal-raise signal-number)\n"
       "          -" STR(EINVAL) ")))))\n");
}
