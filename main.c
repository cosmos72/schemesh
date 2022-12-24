#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#include <scheme.h>
#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <time.h>

#include "posix.h"

#define STR_(arg) #arg
#define STR(arg) STR_(arg)
#define CHEZ_SCHEME_DIR_STR STR(CHEZ_SCHEME_DIR)

static jmp_buf jmp_env;
static int     on_exception = 0;

enum jmp_arg {
  NOP         = 0,
  INIT_FAILED = 1,
  EVAL_FAILED = 2,
  QUIT_FAILED = 3,
};

typedef struct bytes_s {
  iptr                 size;
  const unsigned char* data;
} bytes;

struct timespec now(void) {
  struct timespec t;
  (void)clock_gettime(CLOCK_REALTIME, &t);
  return t;
}

static double diff(const struct timespec start, const struct timespec end) {
  return (end.tv_sec - start.tv_sec) + 1e-9 * (end.tv_nsec - start.tv_nsec);
}

static void handle_scheme_exception(void) { //
  longjmp(jmp_env, on_exception);
}

static void define_display_condition(void);
static void define_any_to_bytevector(void);
static void define_eval_to_bytevector(void);

static void init(void) {
  Sscheme_init(&handle_scheme_exception);
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/petite.boot");
  Sregister_boot_file(CHEZ_SCHEME_DIR_STR "/scheme.boot");
  Sbuild_heap(NULL, NULL);

  define_display_condition();
  define_any_to_bytevector();
  define_eval_to_bytevector();

  register_posix_functions_into_scheme();
}

static void quit(void) { //
  Sscheme_deinit();
}

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr call1(const char symbol_name[], ptr arg) {
  return Scall1(Stop_level_value(Sstring_to_symbol(symbol_name)), arg);
}

/**
 * call Scheme (eval) on a C string and return the resulting Scheme value
 */
ptr eval(const char str[]) {
  return call1("eval", call1("read", call1("open-input-string", Sstring(str))));
}

static void define_display_condition(void) {
  eval("(define (display-condition x port)\n"
       "  (when (condition? x)\n"
       "    (put-string port \"#<condition\")\n"
       "    (do ((clist (simple-conditions x) (cdr clist)))\n"
       "        ((null? clist) (void))\n"
       "      (let ((c (car clist)))\n"
       "        (cond ((assertion-violation? c)       (put-string port \" &assertion\"))\n"
       "              ((non-continuable-violation? c) (put-string port \" &non-continuable\"))"
       "              ((implementation-restriction-violation? c)\n"
       "                         (put-string \" &implementation-restriction \"))\n"
       "              ((lexical-violation? c)   (put-string port \" &lexical\"))\n"
       "              ((syntax-violation? c)    (put-string port \" &syntax \")\n"
       "                                        (put-datum  port (syntax-violation-form c))\n"
       "                                        (put-string port \" \")\n"
       "                                        (put-datum  port (syntax-violation-subform c)))\n"
       "              ((undefined-violation? c) (put-string port \" &undefined\"))\n"
       "              ((violation? c)           (put-string port \" &violation\"))\n"
       "              ((i/o-read-error? c)      (put-string port \" &i/o-read\"))\n"
       "              ((i/o-write-error? c)     (put-string port \" &i/o-write\"))\n"
       "              ((i/o-invalid-position-error? c)\n"
       "                         (put-string port \" &i/o-invalid-position\"))\n"
       /* more i/o errors ... */
       "              ((i/o-error? c)           (put-string port \" &i/o\"))\n"
       "              ((error? c)               (put-string port \" &error\"))\n"
       "              ((warning? c)             (put-string port \" &warning\"))\n"
       "              ((message-condition? c)   (put-string port \" &message \")\n"
       "                                        (put-datum  port (condition-message c)))\n"
       "              ((irritants-condition? c) (put-string port \" &irritants \")\n"
       "                                        (put-datum  port (condition-irritants c)))\n"
       "              ((who-condition? c)       (put-string port \" &who \")\n"
       "                                        (put-datum  port (condition-who c)))\n"
       "              ((serious-condition? c)   (put-string port \" &serious\")))))\n"
       "    (put-string port \">\")))\n");
}

static void define_any_to_bytevector(void) {
  eval("(define any->bytevector\n"
       "  (let ((transcoder (make-transcoder (utf-8-codec) (eol-style lf)\n"
       "                                     (error-handling-mode raise))))\n"
       "    (lambda (x)\n"
       "      (cond ((bytevector? x) x)\n"
       "            ((string? x) (string->utf8 x))\n"
       "            ((eq? (void) x) #vu8())\n"
       "            (#t (let-values (([port get-bytevector]\n"
       "                              (open-bytevector-output-port transcoder)))\n"
       "                  (if (condition? x)\n"
       "                    (display-condition x port)\n"
       "                    (display x port))\n"
       "                  (get-bytevector)))))))\n");
}

static void define_eval_to_bytevector(void) {
  eval("(define (eval->bytevector str)\n"
       "  (any->bytevector (eval (read (open-input-string str)))))\n");
}

/**
 * call Scheme (eval) on a C string, and convert returned Scheme value to
 * bytevector with (any->bytevector).
 * @return length and pointer to internal array of a Scheme-allocated
 * bytevector.
 *
 * Returned pointer CANNOT be dereferenced anymore after calling Scheme code,
 * because it may be moved or garbage collected.
 */
static bytes eval_to_bytevector(const char str[]) {
  ptr    bytevec = call1("eval->bytevector", Sstring(str));
  iptr   size    = Sbytevector_length(bytevec);
  octet* data    = Sbytevector_data(bytevec);
  bytes  ret     = {size, data};
  return ret;
}

static void show(FILE* out, bytes bv) {
  if (bv.size != 0) {
    fwrite(bv.data, 1, bv.size, out);
    fputc('\n', out);
  }
}

int main(int argc, char* argv[]) {
  enum { LEN = 1024 };
  char            buf[LEN];
  struct timespec start, end;
  (void)argc;
  (void)argv;

  switch (setjmp(jmp_env)) {
    case NOP: // first call to setjmp: continue initialization
      break;
    case INIT_FAILED: // init() failed
      goto finish;
    case EVAL_FAILED: // exception in eval()
      goto again;
    case QUIT_FAILED: // exception in quit()
      return 1;
  }
  on_exception = INIT_FAILED;
  init();

  on_exception = EVAL_FAILED;
again:
  while (fgets(buf, LEN, stdin) != NULL) {
    start = now();

    bytes bv = eval_to_bytevector(buf);
    show(stdout, bv);

    end = now();
    fprintf(stdout, "; elapsed: %.09f\n", diff(start, end));
  }
finish:
  on_exception = QUIT_FAILED;
  quit();

  return 0;
}
