/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_POSIX_H
#define SCHEMESH_POSIX_H

#include <scheme.h>

/** define fd-related functions. return < 0 if some C system call failed */
int define_fd_functions(void);

/**
 * define functions (fork-pid) (spawn-pid) (pid-wait)
 * requires functions (sh-env...)
 */
void define_pid_functions(void);

/* return current (-errno) value */
int c_errno(void);

/* close specified file descriptor */
int c_fd_close(int fd);

/* close all file descriptors >= lowest_fd_to_close */
void c_fd_close_all(int lowest_fd_to_close);

/* call dup() */
int c_fd_dup(int old_fd);

/* call dup2() */
int c_fd_dup2(int old_fd, int new_fd);

/* call open() and return fd of newly opened file, or c_errno() on error */
int c_open_file_fd(ptr bytevector0_filepath,
                   int flag_read_write,
                   int flag_create,
                   int flag_truncate,
                   int flag_append);

/* call pipe() and return a Scheme cons (pipe_read_fd . pipe_write_fd), or c_errno() on error */
ptr c_open_pipe_fds(void);

/* fork() and return pid, or c_errno() on error */
int c_fork_pid(void);

typedef enum c_spawn_options_e {
  c_spawn_create_new_pgid   = 0, // insert new process into its own pgid
  c_spawn_use_existing_pgid = 1, // insert new process into existing_pgid
  c_spawn_foreground        = 2, // call tcgetpgrp(pid) to mark new process as foreground
} c_spawn_options;

/** fork() and exec() an external program, return pid */
int c_spawn_pid(ptr vector_of_bytevector0_cmdline,
                ptr vector_redirect_fds,
                ptr vector_of_bytevector0_environ,
                int existing_pgid,
                int spawn_options); // c_spawn_options

/**
 * call waitpid() i.e. wait for process specified by pid to exit.
 * return exit status, or 256 + signal, or c_errno() on error
 */
int c_pid_wait(int pid);

/**
 * call waitpid(-1, WNOHANG|WUNTRACED) i.e. non-blocking check if some child process
 * exited or stopped.
 * return a Scheme cons (pid . exit_flag), or 0 if no child exited, or c_errno() on error.
 * Exit flag is one of: exit status, or 256 + signal, or 512 + stop signal
 */
ptr c_try_wait(void);

/** print label and current errno value to stderr. return -errno */
int c_print_errno(const char label[]);

/** POSIX standard says programs need to declare environ by themselves */
extern char** environ;

#endif /* SCHEMESH_POSIX_H */
