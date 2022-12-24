#include <errno.h>
#include <unistd.h>

#include "main.h"
#include "posix.h"

ptr c_errno(void) {
  return Sinteger((iptr)-errno);
}

int c_close_fd(int fd) {
  int ret = close(fd);
  return ret >= 0 ? ret : -errno;
}

int c_dup_fd(int old_fd) {
  int ret = dup(old_fd);
  return ret >= 0 ? ret : -errno;
}

int c_dup2_fd(int old_fd, int new_fd) {
  int ret = dup2(old_fd, new_fd);
  return ret >= 0 ? ret : -errno;
}

ptr c_make_pipe_fds(void) {
  int fds[2];
  int ret = pipe(fds);
  if (ret < 0) {
    return c_errno();
  }
  return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
}

void register_posix_functions_into_scheme(void) {
  Sregister_symbol("c_errno", &c_errno);
  Sregister_symbol("c_close_fd", &c_close_fd);
  Sregister_symbol("c_dup_fd", &c_dup_fd);
  Sregister_symbol("c_dup2_fd", &c_dup2_fd);
  Sregister_symbol("c_make_pipe_fds", &c_make_pipe_fds);

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
       "  (foreign-procedure \"c_errno\" () scheme-object))\n");
  eval("(define make-pipe-fds\n"
       "  (let ((c-make-pipe-fds (foreign-procedure \"c_make_pipe_fds\" () scheme-object)))\n"
       "    (lambda ()\n"
       "      (let ((ret (c-make-pipe-fds)))\n"
       "        (if (pair? ret)\n"
       "          ret\n"
       "          (raise-errno-condition 'make-pipe-fds ret))))))\n");
}
