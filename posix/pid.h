/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/******************************************************************************/
/*                                                                            */
/*                           pid-related functions                            */
/*                                                                            */
/******************************************************************************/

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/pid.h should only be #included by posix/posix.c"
#endif

/** return effective user id of current process, or c_errno() < 0 on error */
static int c_euid_get(void) {
  int uid = geteuid();
  return uid >= 0 ? uid : c_errno();
}

/** return pid of current process, or c_errno() < 0 on error */
static int c_pid_get(void) {
  int pid = getpid();
  return pid >= 0 ? pid : c_errno();
}

/** return process group of specified process (0 = current process), or c_errno() < 0 on error */
static int c_pgid_get(int pid) {
  int pgid = getpgid((pid_t)pid);
  return pgid >= 0 ? pgid : c_errno();
}

/**
 * set process group id (i.e. pgid) of specified process:
 * if existing_pgid > 0:
 *   move current process into process group indicated by existing_pgid.
 *
 * if existing_pgid == 0:
 *   create a new process group with pgid == current process pid,
 *   and move current process into it.
 *
 * if existing_pgid < 0:
 *   do nothing.
 */
static int c_pgid_set(int pid, int existing_pgid) {
  int err = 0;
  if (existing_pgid >= 0) {
    err = setpgid((pid_t)pid, (pid_t)existing_pgid);
  }
  return err >= 0 ? err : c_errno();
}

/**
 * call kill(pid, sig) i.e. send signal number sig to specified process id.
 * Notes:
 * pid ==  0 means "all processes in the same process group as the caller".
 * pid == -1 means "all processes".
 * pid <  -1 means "all processes in process group -pid"
 *
 * Return 0 on success, otherwise return c_errno()
 */
static int c_pid_kill(int pid, int sig, int pause_if_successful) {
  if (kill(pid, sig) < 0) {
    return c_errno();
  }
  if (pause_if_successful) {
    pause();
  }
  return 0;
}

/**
 * call fork().
 * parent: return pid, or c_errno() on error
 * child: return 0, or c_errno() on error
 *
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0 create a new process id (numerically equal to the process id)
 *                       and move process into it
 */
static int c_fork_pid(ptr vector_fds_redirect, int existing_pgid) {
  /**
   * fix issue #26: all open FILE* streams, such as stdout and stderr,
   * must be flushed before fork(), because any buffered data will survive the fork()
   * and will be flushed when the forked child exits:
   *
   * buffered data may be written to the wrong destination
   * (the child's file descriptors may be redirected)
   * and possibly multiple times: one per forked child.
   */
  (void)fflush(NULL); /* flushes ALL open output streams */

  const int pid = fork();
  switch (pid) {
    case -1: /* fork() failed */
      return c_errno();
    case 0: { /* child */
      int err;
      if ((err = c_pgid_set(0, existing_pgid)) >= 0) {
        if ((err = c_signals_setdefault()) >= 0) { /* keeps SICHLD handler */
          err = c_fds_redirect(vector_fds_redirect, Sfalse);
        }
      }

      return err;
    }
    default: /* parent */
      /*
       * fix well-known race condition between parent and child:
       * both need the child to be in desired process group before continuing,
       * thus both must set it before continuing.
       */
      (void)c_pgid_set(pid, existing_pgid);
#ifdef SCHEMESH_DEBUG_POSIX
      fprintf(stdout, "c_fork_pid %d -> %d\n", (int)getpid(), pid);
      fflush(stdout);
#endif
      return pid;
  }
}

/**
 * optionally fork(), then exec() an external program.
 * if forked, return pid in parent process.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0 create a new process id (numerically equal to the process id)
 *                       and move process into it
 */
static int c_cmd_spawn_or_exec(ptr vector_of_bytevector0_cmdline,
                               ptr bytevector0_chdir_or_false,
                               ptr vector_fds_redirect,
                               ptr vector_of_bytevector0_environ,
                               int existing_pgid,
                               int is_spawn) {

  char** argv = vector_to_c_argz(vector_of_bytevector0_cmdline);
  char** envp = vector_to_c_argz(vector_of_bytevector0_environ);
  int    err  = 0;
  if (!argv || (!envp && Svectorp(vector_of_bytevector0_environ))) {
    err = -ENOMEM;
    goto out;
  }
  if (!argv[0]) {
    err = -EINVAL;
    goto out;
  }
  if (bytevector0_chdir_or_false != Sfalse) {
    const octet* dir;
    iptr         dir_len;
    if (!Sbytevectorp(bytevector0_chdir_or_false)) {
      err = -EINVAL;
      goto out;
    }
    dir     = Sbytevector_data(bytevector0_chdir_or_false);
    dir_len = Sbytevector_length(bytevector0_chdir_or_false);
    if (dir_len <= 0 || dir[dir_len - 1] != 0) {
      err = -EINVAL;
      goto out;
    }
  }

#ifdef SCHEMESH_DEBUG_POSIX
  fprintf(stdout, "c_cmd_spawn %s ...\n", argv[0]);
  fflush(stdout);
#endif

  if (is_spawn) {
    /**
     * issue #26: no need to call fflush(NULL) here,
     * the child will immediately execv() or _exit(),
     * and neither of them flushes open FILE* streams
     */
    err = fork();
  } else {
    err = 0; /* pretend we are already in the child */
  }
  switch (err) {
    case -1: /* error */
      err = c_errno();
      break;

    default: /* parent */
      /*
       * fix well-known race condition between parent and child:
       * both need the child to be in desired process group before continuing,
       * thus both must set it before continuing.
       */
      (void)c_pgid_set(err, existing_pgid);
      break;

    case 0: { /* child */
      /*
       * only call async-safe functions until execv(), in case parent process is multi-threaded
       * and malloc() or other global resources were locked or in an inconsistent state
       * (in the middle of a call) when fork() was executed.
       */
      char** saved_environ = environ;

      if (is_spawn) {
        if ((err = c_pgid_set(0, existing_pgid) < 0) ||
            /* keep SICHLD handler, will be resetted by execv...() */
            (err = c_signals_setdefault()) < 0) {
          goto child_out;
        }
      }
      if ((bytevector0_chdir_or_false != Sfalse &&
           (err = c_chdir(bytevector0_chdir_or_false) < 0)) ||
          (err = c_fds_redirect(vector_fds_redirect, Sfalse)) < 0) {
        goto child_out;
      }
      if (envp) {
        environ = envp;
      }
      if (strchr(argv[0], '/')) {
        (void)execv(argv[0], argv);
      } else {
        (void)execvp(argv[0], argv);
      }
      /* in case or execv...() failed and returned */
      err = c_errno();
      if (err == -ENOENT) {
        (void)write_command_not_found(argv[0]);
      } else {
        (void)write_path_c_errno(argv[0], strlen(argv[0]), err, ". Type 'help' for help.\n");
      }
    child_out:
      if (is_spawn) {
        /** not exit(), because it would flush a fork()ed copy of open FILE* streams */
        _exit(err < 0 ? 1 : 127);
      }
      if (envp) {
        environ = saved_environ;
      }
      break;
    }
  }
out:
  free(argv);
  free(envp);
  return err;
}

/**
 * exec() an external program. Returns only if failed.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0, create a new process group id == process id, and move process into it.
 * if existing_pgid < 0, process inherits the process group id from current process
 */
static int c_cmd_exec(ptr vector_of_bytevector0_cmdline,
                      ptr bytevector0_chdir_or_false,
                      ptr vector_fds_redirect,
                      ptr vector_of_bytevector0_environ) {

  return c_cmd_spawn_or_exec(vector_of_bytevector0_cmdline,
                             bytevector0_chdir_or_false,
                             vector_fds_redirect,
                             vector_of_bytevector0_environ,
                             -2, /* do not set pgid */
                             0); /* !is_spawn */
}

/**
 * fork() and exec() an external program, return pid.
 * if existing_pgid > 0, add process to given pgid i.e. process group
 * if existing_pgid == 0, create a new process group id == process id, and move process into it.
 * if existing_pgid < 0, process inherits the process group id from current process
 */
static int c_cmd_spawn(ptr vector_of_bytevector0_cmdline,
                       ptr bytevector0_chdir_or_false,
                       ptr vector_fds_redirect,
                       ptr vector_of_bytevector0_environ,
                       int existing_pgid) {

  return c_cmd_spawn_or_exec(vector_of_bytevector0_cmdline,
                             bytevector0_chdir_or_false,
                             vector_fds_redirect,
                             vector_of_bytevector0_environ,
                             existing_pgid,
                             1); /* is_spawn */
}

/**
 * get the foreground process group id.
 *
 * return pgid >= 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_get() {
  const int pgid = tcgetpgrp(tty_fd);
  return pgid >= 0 ? pgid : c_errno();
}

/**
 * set the foreground process group id.
 *
 * return = 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_set(int new_pgid) {
  return tcsetpgrp(tty_fd, new_pgid) >= 0 ? 0 : c_errno();
}

/**
 * compare-and-swap the foreground process group id.
 *
 * if old_pgid < 0, unconditionally set new_pgid as the foreground process group.
 *
 * if old_pgid >= 0 and the foreground process group == old_pgid,
 * then set new_pgid as the foreground process group.
 *
 * return 0 on success, otherwise error code < 0.
 */
static int c_pgid_foreground_cas(int old_pgid, int new_pgid) {
  if (old_pgid >= 0) {
    const int current_pgid = tcgetpgrp(tty_fd);
    if (current_pgid < 0) {
      return c_errno();
    } else if (current_pgid != old_pgid) {
      return 0; /* fg process group is not the expected one: do nothing */
    } else if (current_pgid == new_pgid) {
      return 0; /* nothing to do */
    }
  }
  return tcsetpgrp(tty_fd, new_pgid) >= 0 ? 0 : c_errno();
}

/**
 * call waitpid(pid, WUNTRACED|WCONTINUED) i.e. check if process specified by pid
 * finished, stopped or resumed.
 *
 * Special cases:
 *   pid ==  0 means "any child process in the same process group as the caller"
 *   pid == -1 means "any child process"
 *   pid <  -1 means "any child process in process group -pid"
 *
 * If may_block != 0, wait until pid (or any child process, if pid == -1) exits or stops,
 * otherwise check for such conditions without blocking.
 *
 * If no child process matches pid, or if may_block == 0 and no child finished, or
 * stopped, return Scheme empty list '().
 * Otherwise return a Scheme cons (pid . status_flag), or c_errno() on error.
 * status_flag is one of:
 *   process exit status in 0 ... 255
 *   or 256 + signal that killed the process
 *   or 512 + signal that stopped the process
 *   or 768 if job resumed due to SIGCONT
 */
static ptr c_pid_wait(int pid, int may_block) {
  int   wstatus = 0;
  int   result  = 0;
  int   retry_n = 1;
  pid_t ret_pid;
  /*
   * avoid WCONTINUED on macOS:
   * it repeatedly reports the same pid as "continued", causing a busy loop
   */
#if defined(WCONTINUED) && !defined(__APPLE__)
  const int options = WUNTRACED | WCONTINUED;
#else
  const int options = WUNTRACED;
#endif

again:
  ret_pid = waitpid((pid_t)pid, &wstatus, options | (may_block ? 0 : WNOHANG));

  if (ret_pid <= 0) { /* 0 if children exist but did not change status */
    int err = 0;
    if (ret_pid < 0) {
      err = c_errno();
      /*
       * when a child stops on macOS, waitpid() fails with err == -EINTR
       * and we must call waitpid() again to get stopped child's pid.
       *
       * Retrying on EINTR may help on other systems too, thus do it unconditionally
       */
      if (retry_n > 0 && err == -EINTR) {
        retry_n--;
        may_block = 0;
        goto again;
      }
      if (err == -EAGAIN || err == -EINTR || err == -ECHILD) {
        err = 0; /* no child changed status */
      }
    }
#ifdef SCHEMESH_DEBUG_WAIT_PID
    fprintf(stderr,
            "c_pid_wait(pid = %d, may_block = %d) -> pid = %d, errno = %d %s\n",
            pid,
            may_block,
            ret_pid,
            errno,
            strerror(errno));
    fflush(stderr);
#endif /* DEBUG_WAIT_PID */
    return err == 0 ? Snil : Sinteger(err);
  } else if (WIFEXITED(wstatus)) {
    result = (int)(unsigned char)WEXITSTATUS(wstatus);
  } else if (WIFSIGNALED(wstatus)) {
    result = 256 + WTERMSIG(wstatus);
  } else if (WIFSTOPPED(wstatus)) {
    result = 512 + WSTOPSIG(wstatus);
#ifdef WIFCONTINUED
  } else if (WIFCONTINUED(wstatus)) {
    result = 768;
#endif
  } else {
    return Sinteger(c_errno_set(EINVAL));
  }
#ifdef SCHEMESH_DEBUG_WAIT_PID
  fprintf(stderr,
          "c_pid_wait(pid = %d, may_block = %d) -> pid = %d, result = %d\n",
          pid,
          may_block,
          ret_pid,
          result);
  fflush(stderr);
#endif /* SCHEMESH_DEBUG_WAIT_PID */

  return Scons(Sinteger(ret_pid), Sinteger(result));
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
