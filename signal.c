/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include <errno.h> /* EINVAL */
#include <signal.h>
#include <stdatomic.h>
#include <stddef.h> /* size_t, NULL */

#include "eval.h"
#include "posix.h"
#include "signal.h"

#define N_OF(span) (sizeof(span) / sizeof((span)[0]))

static volatile int c_sigchld_received  = 0;
static volatile int c_sigwinch_received = 0;

static void c_sigchld_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigchld_received, 1);
}

static void c_sigwinch_handler(int sig_num) {
  (void)sig_num;
  atomic_store(&c_sigwinch_received, 1);
}

ptr c_sigchld_consume(void) {
  return atomic_exchange(&c_sigchld_received, 0) ? Strue : Sfalse;
}

ptr c_sigwinch_consume(void) {
  return atomic_exchange(&c_sigwinch_received, 0) ? Strue : Sfalse;
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
      return c_errno_print(labels[i]);
    }
  }
  return 0;
}

int c_signals_setdefault(void) {
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

static struct sigaction c_sigwinch_saved_action;

int c_sigwinch_init(void) {
  struct sigaction action = {};
  action.sa_handler       = &c_sigwinch_handler;
  if (sigaction(SIGWINCH, &action, &c_sigwinch_saved_action) < 0) {
    c_sigwinch_saved_action.sa_handler = SIG_DFL;
    return c_errno_print("sigaction(SIGWINCH)");
  }
  return 0;
}

int c_sigwinch_restore(void) {
  if (sigaction(SIGWINCH, &c_sigwinch_saved_action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

int c_signal_setdefault(int sig) {
  struct sigaction action = {};
  action.sa_handler       = SIG_DFL;

  if (sigaction(sig, &action, NULL) < 0) {
    return c_errno();
  }
  return 0;
}

int c_signal_raise(int sig) {
  (void)c_signal_setdefault(sig);
  if (raise(sig) < 0) { /* better than kill(getpid(), sig) in multi-threaded-programs */
    return c_errno();
  }
  return 0;
}

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

void schemesh_define_library_signals(void) {
  Sregister_symbol("c_signal_raise", &c_signal_raise);
  Sregister_symbol("c_sigchld_consume", &c_sigchld_consume);
  Sregister_symbol("c_sigwinch_consume", &c_sigwinch_consume);
  Sregister_symbol("c_sigwinch_init", &c_sigwinch_init);
  Sregister_symbol("c_sigwinch_restore", &c_sigwinch_restore);

  eval("(library (schemesh signals (0 1))\n"
       "  (export signal-raise signal-number->name signal-name->number\n"
       "          signal-consume-sigchld signal-consume-sigwinch signal-init-sigwinch "
       "signal-restore-sigwinch)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) foreign-procedure)\n"
       "    (only (schemesh containers hashtable) eq-hashtable hashtable-transpose))\n"
       "\n"
       "(define signal-table-number->name\n"
       /* clang-format off */
       "  (eq-hashtable"
       "'(" STR(SIGHUP)  " . sighup)"
       "'(" STR(SIGINT)  " . sigint)"
       "'(" STR(SIGQUIT) " . sigquit)"
       "'(" STR(SIGILL)  " . sigill)"
       "'(" STR(SIGTRAP) " . sigtrap)"
       "'(" STR(SIGABRT) " . sigabrt)"
       "'(" STR(SIGBUS)  " . sigbus)"
       "'(" STR(SIGFPE)  " . sigfpe)"
       "'(" STR(SIGKILL) " . sigkill)"
       "'(" STR(SIGUSR1) " . sigusr1)"
       "'(" STR(SIGSEGV) " . sigsegv)"
       "'(" STR(SIGUSR2) " . sigusr2)"
       "'(" STR(SIGPIPE) " . sigpipe)"
       "'(" STR(SIGALRM) " . sigalrm)"
       "'(" STR(SIGTERM) " . sigterm)"
       "'(" STR(SIGSTKFLT) " . sigstkflt)"
       "'(" STR(SIGCHLD) " . sigchld)"
       "'(" STR(SIGCONT) " . sigcont)"
       "'(" STR(SIGSTOP) " . sigstop)"
       "'(" STR(SIGTSTP) " . sigtstp)"
       "'(" STR(SIGTTIN) " . sigttin)"
       "'(" STR(SIGTTOU) " . sigttou)"
       "'(" STR(SIGURG)  " . sigurg)"
       "'(" STR(SIGXCPU) " . sigxcpu)"
       "'(" STR(SIGXFSZ) " . sigxfsz)"
       "'(" STR(SIGVTALRM) " . sigvtalrm)"
       "'(" STR(SIGPROF) " . sigprof)"
       "'(" STR(SIGWINCH) " . sigwinch)"
       "'(" STR(SIGIO)   " . sigio)"
       "'(" STR(SIGPWR)  " . sigpwr)"
       "'(" STR(SIGSYS)  " . sigsys)))\n"
       "\n"
       "(define signal-table-name->number\n"
       "  (hashtable-transpose\n"
       "    signal-table-number->name\n"
       "    (make-eq-hashtable)))\n"
       "\n"
       "(define (signal-number->name number)\n"
       "  (hashtable-ref signal-table-number->name number #f))\n"
       "\n"
       "(define (signal-name->number name)\n"
       "  (hashtable-ref signal-table-name->number name #f))\n"
       "\n"
       /**
        * (signal-raise signal-name) calls C functions sigaction(sig, SIG_DFL),
        * then calls C function kill(getpid(), sig)
        * i.e. sends specified signal to the process itself.
        *
        * Returns < 0 if signal-name is unknown, or if C function raise() fails with C errno != 0.
        */
       "(define signal-raise"
       "  (let ((c-signal-raise (foreign-procedure \"c_signal_raise\" (int) int)))\n"
       "    (lambda (signal-name)\n"
       "      (let ((signal-number (signal-name->number signal-name)))\n"
       "        (if (fixnum? signal-number)\n"
       "          (c-signal-raise signal-number)\n"
       "          -" STR(EINVAL) ")))))\n"
       "\n"
       "(define signal-consume-sigchld  (foreign-procedure \"c_sigchld_consume\" () scheme-object))\n"
       "(define signal-consume-sigwinch (foreign-procedure \"c_sigwinch_consume\" () scheme-object))\n"
       "\n"
       "(define signal-init-sigwinch\n"
       "  (let ((c-signal-init-sigwinch (foreign-procedure \"c_sigwinch_init\" () int)))\n"
       "    (lambda ()\n"
       "      (assert (fxzero? (c-signal-init-sigwinch))))))\n"
       "\n"
       "(define signal-restore-sigwinch (foreign-procedure \"c_sigwinch_restore\" () int))\n"
       "\n"
       ")\n"); /* close library */
  /* clang-format on */
}
