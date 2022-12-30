#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

#include "main.h"
#include "posix.h"

int c_errno(void) {
  return -errno;
}

int c_close_fd(int fd) {
  int ret = close(fd);
  return ret >= 0 ? ret : -errno;
}

void c_close_all_fds(int lowest_fd_to_close) {
  int i       = lowest_fd_to_close >= 0 ? lowest_fd_to_close : 0;
  int last_ok = i - 1;
  for (; i < INT_MAX && i - 32 <= last_ok; i++) {
    if (close(i) >= 0) {
      last_ok = i;
    }
  }
}

int c_dup_fd(int old_fd) {
  int ret = dup(old_fd);
  return ret >= 0 ? ret : -errno;
}

int c_dup2_fd(int old_fd, int new_fd) {
  int ret = dup2(old_fd, new_fd);
  return ret >= 0 ? ret : -errno;
}

int c_open_file_fd(ptr bytevector0_filepath,
                   int flag_read_write,
                   int flag_create,
                   int flag_truncate,
                   int flag_append) {
  const char* filepath;
  iptr        len;
  int         ret;
  if (!Sbytevectorp(bytevector0_filepath)) {
    return -(errno = EINVAL);
  }
  filepath = (const char*)Sbytevector_data(bytevector0_filepath);
  len      = Sbytevector_length(bytevector0_filepath);
  if (len == 0 || filepath[len - 1] != '\0') {
    return -(errno = EINVAL);
  }
  ret = open(filepath,
             (flag_read_write == 0 ? O_RDONLY :
              flag_read_write == 1 ? O_WRONLY :
                                     O_RDWR) |        /*                */
                 (flag_create == 0 ? 0 : O_CREAT) |   /*                */
                 (flag_truncate == 0 ? 0 : O_TRUNC) | /*                */
                 (flag_append == 0 ? 0 : O_APPEND),   /*                */
             0666);
  return ret >= 0 ? ret : -errno;
}

ptr c_open_pipe_fds(void) {
  int fds[2];
  int ret = pipe(fds);
  if (ret < 0) {
    return Sinteger((iptr)-errno);
  }
  return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
}

static void register_process_functions_into_scheme(void);

void register_posix_functions_into_scheme(void) {
  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_close_fd", &c_close_fd);
  Sregister_symbol("c_dup_fd", &c_dup_fd);
  Sregister_symbol("c_dup2_fd", &c_dup2_fd);
  Sregister_symbol("c_open_file_fd", &c_open_file_fd);
  Sregister_symbol("c_open_pipe_fds", &c_open_pipe_fds);

  eval("(define (make-errno-condition who c-errno)\n"
       "  (condition\n"
       "    (make-error)\n"
       "    (make-who-condition who)\n"
       "    (make-message-condition \"error in C function\")\n"
       "    (make-irritants-condition c-errno)))\n");
  eval("(define (raise-errno-condition who c-errno)\n"
       "  (raise (make-errno-condition who c-errno)))\n");

  eval("(define close-fd\n"
       "  (let ((c-close-fd (foreign-procedure \"c_close_fd\" (int) int)))\n"
       "    (lambda (x)\n"
       "      (let ((ret (c-close-fd x)))\n"
       "        (if (>= ret 0)\n"
       "          (void)\n"
       "          (make-errno-condition 'close-fd ret))))))\n");
  eval("(define dup-fd\n"
       "  (let ((c-dup-fd (foreign-procedure \"c_dup_fd\" (int) int)))\n"
       "    (lambda (old-fd)\n"
       "      (let ((ret (c-dup-fd old-fd)))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'dup-fd ret))))))\n");
  eval("(define dup2-fd\n"
       "  (let ((c-dup2-fd (foreign-procedure \"c_dup2_fd\" (int int) int)))\n"
       "    (lambda (old-fd new-fd)\n"
       "      (let ((ret (c-dup2-fd old-fd new-fd)))\n"
       "        (if (>= ret 0)\n"
       "          (void)\n"
       "          (raise-errno-condition 'dup2-fd ret))))))\n");
  eval("(define errno\n"
       "  (foreign-procedure \"c_errno\" () int))\n");
  eval("(define open-pipe-fds\n"
       "  (let ((c-open-pipe-fds (foreign-procedure \"c_open_pipe_fds\" () scheme-object)))\n"
       "    (lambda ()\n"
       "      (let ((ret (c-open-pipe-fds)))\n"
       "        (if (pair? ret)\n"
       "          ret\n"
       "          (raise-errno-condition 'open-pipe-fds ret))))))\n");
  eval("(define open-file-fd\n"
       "  (let ((c-open-file-fd (foreign-procedure \"c_open_file_fd\""
       "                          (scheme-object int int int int) int)))\n"
       "    (lambda (filepath . flags)\n"
       "      (let* ([filepath0 (string->bytevector0 filepath)]\n"
       "             [flag-rw (cond ((member 'rw flags) 2)\n"
       "                            ((member 'write flags) 1)\n"
       "                            ((member 'read flags) 0)\n"
       "                            (#t (error 'open-file-fd\n"
       "                                 \"flags must contain one of 'read 'write 'rw\" flags)))]\n"
       "             [flag-create   (if (member 'create flags) 1 0)]\n"
       "             [flag-truncate (if (member 'truncate flags) 1 0)]\n"
       "             [flag-append   (if (member 'append flags) 1 0)]\n"
       "             [ret (c-open-file-fd filepath0 flag-rw flag-create "
       "                    flag-truncate flag-append)])\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'open-file-fd ret))))))\n");

  register_process_functions_into_scheme();
}

static void register_process_functions_into_scheme(void) {
  Sregister_symbol("c_spawnv", &c_spawnv);
  Sregister_symbol("c_wait_pid", &c_wait_pid);

  eval("(define (list->cmd-argv l)\n"
       "  (let ((argv (list->vector l)))\n"
       "    (do ([i 0 (+ 1 i)])\n"
       "        ((>= i (vector-length argv)))\n"
       "      (vector-set! argv i (string->bytevector0 (vector-ref argv i))))\n"
       "    argv))\n");

  /**
   * Spawn an external program and return its pid.
   *
   * Parameter program-and-args is a list containing program name and its arguments,
   * and each element must be either a string or a bytevector.
   */
  eval("(define spawn-pid\n"
       "  (let ((c-spawn-pid (foreign-procedure \"c_spawnv\""
       "                        (scheme-object scheme-object) int)))\n"
       "    (lambda program-and-args\n"
       "      (let ((ret (c-spawn-pid (list->cmd-argv program-and-args) (vector 0 1 2))))\n"
       "        (if (>= ret 0)\n"
       "          ret\n"
       "          (raise-errno-condition 'spawn-pid ret))))))\n");

  /**
   * Wait for the program identified by pid to exit.
   *
   * Return the program's exit status, or 256 + signal, or c_errno() on error.
   */
  eval("(define wait-pid (foreign-procedure \"c_wait_pid\" (int) int))\n");

  /**
   * Define the record type "cmd"
   */
  eval("(define-record-type\n"
       "  (cmd %make-cmd cmd?)\n"
       "  (fields\n"
       "    (mutable pid)\n"              /* fixnum, -1 if unknown */
       "    (mutable exit-status)\n"      /* fixnum, -1 if unknown */
       "    argv\n"                       /* vector of bytevectors, each #\nul terminated */
       "    (mutable to-redirect-fds)\n"  /* vector of fds to redirect at spawn */
       "    (mutable to-close-fds)))\n"); /* vector of fds to close after exit */

  /** Create a cmd to later spawn it. */
  eval("(define (make-cmd argv-list)\n"
       "  (%make-cmd -1 -1 (list->cmd-argv argv-list) (vector 0 1 2) (vector)))\n");

  /** Spawn a cmd */
  eval("(define cmd-spawn\n"
       "  (let ((c-spawn-pid (foreign-procedure \"c_spawnv\""
       "                        (scheme-object scheme-object) int)))\n"
       "    (lambda (c)\n"
       "      (when (>= (cmd-pid c) 0)\n"
       "        (error 'cmd-spawn \"command already started\" (cmd-pid c)))\n"
       "      (let ([ret (c-spawn-pid (cmd-argv c) (cmd-to-redirect-fds c))])\n"
       "        (if (>= ret 0)\n"
       "          (cmd-pid-set! c ret)\n"
       "          (raise-errno-condition 'cmd-spawn ret))))))\n");

  /** Wait for a cmd to exit and return its exit status, or 256 + signal */
  eval("(define (cmd-wait c)\n"
       "  (if (>= (cmd-exit-status c) 0)\n"
       "    (cmd-exit-status c)\n" /* already waited for */
       "    (begin\n"
       "      (when (< (cmd-pid c) 0)\n"
       "        (error 'cmd-wait \"command not started yet\" c))\n"
       "      (let ([ret (wait-pid (cmd-pid c))])\n"
       "        (when (< ret 0)\n"
       "          (raise-errno-condition 'cmd-wait ret))\n"
       "        (cmd-pid-set! c -1)\n" /* cmd can now be spawned again */
       "        (cmd-exit-status-set! c ret)\n"
       "        ret))))\n");

  /** Create or remove a redirection for cmd */
  eval("(define (cmd-redirect! c child-fd existing-fd-or-minus-1)\n"
       "  (when (or (not (fixnum? child-fd)) (< child-fd 0))\n"
       "    (error 'cmd-redirect! \"invalid redirect fd\" child-fd))\n"
       "  (let* ([old-fds (cmd-to-redirect-fds c)]\n"
       "         [old-n (vector-length old-fds)])\n"
       "    (when (<= old-n child-fd)\n"
       "      (let* ([new-n (max (+ 1 child-fd) (* 2 old-n))]\n"
       "             [new-fds (make-vector new-n -1)])\n" /* fill with -1 i.e. no redirection */
       "        (do ([i 0 (+ 1 i)])\n"
       "            ((>= i old-n))\n"
       "          (vector-set! new-fds i (vector-ref old-fds i)))\n"
       "        (cmd-to-redirect-fds-set! c new-fds))))\n"
       "  (vector-set! (cmd-to-redirect-fds c) child-fd existing-fd-or-minus-1))\n");
}

static int c_check_redirect_fds(ptr vector_redirect_fds) {
  iptr i, n;
  if (!Svectorp(vector_redirect_fds)) {
    return -EINVAL;
  }
  n = Svector_length(vector_redirect_fds);
  for (i = 0; i < n; i++) {
    ptr elem = Svector_ref(vector_redirect_fds, i);
    if (!Sfixnump(elem)) {
      return -EINVAL;
    }
  }
  return 0;
}

static int c_redirect_fds(ptr vector_redirect_fds) {
  iptr i, n;
  int  lowest_fd_to_close = 0;
  if (!Svectorp(vector_redirect_fds)) {
    return -1;
  }
  n = Svector_length(vector_redirect_fds);
  for (i = 0; i < n; i++) {
    ptr  elem = Svector_ref(vector_redirect_fds, i);
    iptr fd;
    if (!Sfixnump(elem)) {
      return -1;
    }
    fd = Sfixnum_value(elem);
    if (fd >= 0) {
      if (fd != i && dup2(fd, i) < 0) {
        return -1;
      }
      lowest_fd_to_close = i + 1;
    }
  }
  return lowest_fd_to_close;
}

int c_spawnv(ptr vector_of_bytevector_cmdline, ptr vector_redirect_fds) {
  char** argv = NULL;
  iptr   argn, i;
  int    pid;
  if (!Svectorp(vector_of_bytevector_cmdline) || c_check_redirect_fds(vector_redirect_fds) != 0) {
    return -(errno = EINVAL);
  }
  argn = Svector_length(vector_of_bytevector_cmdline);
  if (argn == 0) {
    return -(errno = EINVAL);
  }
  argv = malloc((1 + argn) * sizeof(char*));
  if (!argv) {
    return -(errno = ENOMEM);
  }
  for (i = 0; i < argn; i++) {
    ptr  argi = Svector_ref(vector_of_bytevector_cmdline, i);
    iptr len;
    if (!Sbytevectorp(argi)) {
      goto bad_arg;
    }
    argv[i] = (char*)Sbytevector_data(argi);
    len     = Sbytevector_length(argi);
    if (len == 0 || argv[i][len - 1] != '\0') {
      goto bad_arg;
    }
  }
  argv[argn] = NULL;
  pid        = fork();
  switch (pid) {
    case -1:
      /* error */
      pid = -errno;
      break;
    case 0: {
      /* child */
      int lowest_fd_to_close = c_redirect_fds(vector_redirect_fds);
      if (lowest_fd_to_close >= 0) {
        (void)c_close_all_fds(lowest_fd_to_close);
        (void)execvp(argv[0], argv);
      }
      exit(1); // in case c_redirect_fds() fails or execvp() fails and returns
    }
    default:
      /* parent */
      break;
  }
  free(argv);
  return pid;
bad_arg:
  free(argv);
  return -(errno = EINVAL);
}

int c_wait_pid(int pid) {
  int wstatus = 0;
  int ret     = waitpid((pid_t)pid, &wstatus, 0);
  if (ret < 0) {
    return -errno;
  } else if (WIFEXITED(wstatus)) {
    return (int)(unsigned char)WEXITSTATUS(wstatus);
  } else if (WIFSIGNALED(wstatus)) {
    return 256 + WTERMSIG(wstatus);
  } else {
    return -(errno = -ENOENT);
  }
}
