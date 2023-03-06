/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "shell.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned run_test(const char string_to_eval[], const char expected_result[]);
static int      run_tests(void);

static const struct {
  const char* string_to_eval;
  const char* expected_result;
} tests[] = {
    {"(+ 1 2 3)", "6"},
    {"(* 4 5 6)", "120"},
    {"(array 1 2 3)", "(array 1 2 3)"},
    {"(array-length (array 1 2 3))", "3"},
    {"(array-capacity (array 1 2 3))", "3"},
    {"(array-ref (array 'a 'b 'c) 1)", "b"},
    {"(let ((arr (array 'foo)))\n"
     "  (array-append! arr 'bar 'qux)\n"
     "  arr)",
     "(array foo bar qux)"},
    {"(let ((arr (array 'a 'b 'c 'd)))\n"
     "  (array-find arr 0 999 (lambda (elem) (eq? 'c elem))))\n",
     "2"},
    {"(errno)", "0"},
    {"(let ((ret '()))\n"
     "  (list-iterate '(a b c)\n"
     "    (lambda (elem)\n"
     "      (set! ret (cons elem ret))\n"
     /*     stop iterating if (eq? 'b elem) */
     "      (not (eq? 'b elem))))\n"
     "  ret)\n",
     "(b a)"},
    {"(let ((h (make-eqv-hashtable)))\n"
     "  (hashtable-set! h 1.0 'A)\n"
     "  (hashtable-set! h 2.1 'B)\n"
     "  (hashtable-set! h 3   'C)\n"
     "  (hashtable-cells h))\n",
     "#((3 . C) (1.0 . A) (2.1 . B))"},
    {"(let ((h (make-hashtable string-hash string=?)))\n"
     "  (hashtable-set! h \"A\" \"X\")\n"
     "  (hashtable-set! h \"B\" \"Y\")\n"
     "  (hashtable-set! h \"C\" \"Z\")\n"
     "    (string-hashtable->vector-of-bytevector0 h))\n",
     "#(#vu8(67 61 90 0) #vu8(66 61 89 0) #vu8(65 61 88 0))"},
    {"(let ((h (make-eqv-hashtable))\n"
     "       (ret '()))\n"
     "  (hashtable-set! h 1.0 'A)\n"
     "  (hashtable-set! h 2.1 'B)\n"
     "  (hashtable-set! h 3   'C)\n"
     "  (hashtable-iterate h\n"
     "    (lambda (cell)\n"
     "      (set! ret (cons cell ret))))\n"
     "  ret)\n",
     "((2.1 . B) (1.0 . A) (3 . C))"},
    {"(begin\n"
     "  (sh-env-set! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env-get       #t \"foo\")\n"
     "    (sh-env-exported? #t \"foo\")))\n",
     "(bar . #f)"},
    {"(sh-run (sh-cmd \"true\"))", "(exited . 0)"},
    {"(sh-run (sh-cmd \"false\"))", "(exited . 1)"},
};

static int run_tests(void) {
  const unsigned long n = sizeof(tests) / sizeof(tests[0]);
  unsigned long       i;
  unsigned long       failed_n = 0;

  for (i = 0; i < n; i++) {
    failed_n += run_test(tests[i].string_to_eval, tests[i].expected_result);
  }
  if (failed_n == 0) {
    fprintf(stdout, "all %lu tests passed\n", n);
    return 0;
  } else {
    fprintf(stdout, "%lu tests failed out of %lu\n", failed_n, n);
    return 1;
  }
}

static unsigned run_test(const char string_to_eval[], const char expected_result[]) {
  bytes actual   = eval_to_bytevector(string_to_eval);
  bytes expected = {strlen(expected_result), (const unsigned char*)expected_result};
  if (actual.size == expected.size && memcmp(actual.data, expected.data, actual.size) == 0) {
    return 0;
  }
  fprintf(stdout,
          "test failed:\n"
          "    Scheme code  %s\n"
          "    evaluated to %.*s\n"
          "    expecting    %s\n",
          string_to_eval,
          (int)actual.size,
          (const char*)actual.data,
          expected_result);
  return 1;
}

void handle_scheme_exception(void) { //
  fputs("schemesh_test failed: exception evaluating Scheme code!\n", stdout);
  exit(1);
}

int main(int argc, const char* argv[]) {
  int err;
  (void)argc;
  (void)argv;

  scheme_init(&handle_scheme_exception);
  if ((err = define_functions()) < 0) {
    return err;
  }

  errno = 0;
  err   = run_tests();

  scheme_quit();

  return err;
}
