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
int c_fork_pid(ptr vector_redirect_fds, int existing_pgid_if_positive);

/** fork() and exec() an external program, return pid */
int c_spawn_pid(ptr vector_of_bytevector0_cmdline,
                ptr vector_redirect_fds,
                ptr vector_of_bytevector0_environ,
                int existing_pgid_if_positive); // if > 0, add process to given process group

/**
 * set the specified pgid i.e. process group id as the foreground process group.
 */
int c_pgid_foreground(int pgid);

/**
 * call kill(pid, sig) i.e. send signal number sig to specified process id.
 * Notes:
 * pid ==  0 means "all processes in the same process group as the caller".
 * pid == -1 means "all processes".
 * pid <  -1 means "all processes in process group -pid"
 *
 * Return 0 on success, otherwise return c_errno()
 */
int c_pid_kill(int pid, int sig);

/**
 * call waitpid(pid, WUNTRACED) i.e. check if process specified by pid exited or stopped.
 * Note: pid == -1 means "any child process".
 * If may_block != 0, wait until pid (or any child process, if pid == -1) exits or stops,
 * otherwise check for such conditions without blocking.
 *
 * If no child process matches pid, or if may_block == 0 and no child exited or
 * stopped, return Scheme empty list '().
 * Otherwise return a Scheme cons (pid . exit_flag), or c_errno() on error.
 * Exit flag is one of: process exit status, or 256 + signal, or 512 + stop signal.
 */
ptr c_pid_wait(int pid, int may_block);

/** print label and current errno value to stderr. return -errno */
int c_print_errno(const char label[]);

/** POSIX standard says programs need to declare environ by themselves */
extern char** environ;

#endif /* SCHEMESH_POSIX_H */
