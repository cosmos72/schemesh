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
int schemesh_register_c_functions_fd(void);

/** define tty-related functions. */
void schemesh_register_c_functions_tty(void);

/** define miscellaneous posix functions */
void schemesh_register_c_functions_posix(void);

/**
 * define process-related functions (fork-pid) (spawn-pid) (pid-wait)
 * requires functions (sh-env...)
 */
void schemesh_register_c_functions_pid(void);

/** return current (-errno) value */
int c_errno(void);

/** set errno = errno_value, then return (-errno) */
int c_errno_set(int errno_value);

/** return file descriptor for our controlling tty */
int c_tty_fd(void);

/** restore controlling tty to saved config */
int c_tty_restore(void);

/** save controlling tty config, then set it to raw mode */
int c_tty_setraw(void);

/** return a cons (width . height), or c_errno() on error */
ptr c_tty_size(void);

/** close specified file descriptor */
int c_fd_close(int fd);

/** close all file descriptors >= lowest_fd_to_close */
void c_fd_close_all(int lowest_fd_to_close);

/** call dup() */
int c_fd_dup(int old_fd);

/** call dup2() */
int c_fd_dup2(int old_fd, int new_fd);

/** call read(). returns number of bytes read, or c_errno() < 0 on error */
iptr c_fd_read(int fd, ptr bytevector_read, iptr start, iptr end);

/** call write(). returns number of bytes written, or c_errno() < 0 on error */
iptr c_fd_write(int fd, ptr bytevector_towrite, iptr start, iptr end);

/**
 * call select() or poll() on file descriptor.
 * Returns rw_mask of operations available on file descriptor,
 * or c_errno() < 0 on error - which may also be EINTR.
 *
 * argument rw_mask and return value are a bitwise-or of:
 *   1 => fd is readable
 *   2 => fd is writable
 *   4 => fd is in error (only in return value)
 */
int c_fd_select(int fd, int rw_mask, int timeout_milliseconds);

/**
 * call fcntl(fd, FD_SETFL, O_NONBLOCK | fcntl(fd, FD_GETFL))
 * to set file descriptor to non-blocking mode.
 * returns >= 0 on success, or c_errno() on error
 */
int c_fd_setnonblock(int fd);

/**
 * call open() and return fd of newly opened file, or c_errno() on error
 * flag_read_write can be one of:
 *   0 => open readonly
 *   1 => open writeonly
 *   2 => open readwrite
 */
int c_open_file_fd(ptr bytevector0_filepath,
                   int flag_read_write,
                   int flag_create,
                   int flag_truncate,
                   int flag_append);

/** call pipe() and return a Scheme cons (pipe_read_fd . pipe_write_fd), or c_errno() on error */
ptr c_open_pipe_fds(void);

/** print label and current errno value to stderr. return -errno */
int c_errno_print(const char label[]);

/** POSIX standard says programs need to declare environ by themselves */
extern char** environ;

#endif /** SCHEMESH_POSIX_H */
