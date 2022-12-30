#ifndef SCHEMESH_POSIX_H
#define SCHEMESH_POSIX_H

#include <scheme.h>

void register_posix_functions_into_scheme(void);

/* close specified file descriptor */
int c_close_fd(int fd);

/* call dup() */
int c_dup_fd(int old_fd);

/* call dup2() */
int c_dup2_fd(int old_fd, int new_fd);

/* return current C errno value */
int c_errno(void);

/* call open() and return fd of newly opened file, or c_errno() on error */
int c_open_file_fd(ptr bytevector0_filepath,
                   int flag_read_write,
                   int flag_create,
                   int flag_truncate,
                   int flag_append);

/* call pipe() and return a Scheme cons (pipe_read_fd . pipe_write_fd), or c_errno() on error */
ptr c_open_pipe_fds(void);

/* fork() and exec() an external program, return pid */
int c_spawnv(ptr vector_of_bytevector_cmdline, ptr vector_redirect_fds);

/* call waitpid(). return exit status, or 256 + signal, or c_errno() on error */
int c_wait_pid(int pid);

#endif /* SCHEMESH_POSIX_H */
