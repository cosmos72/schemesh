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
    {"(let ((x 0))\n"
     "  (repeat 5 (set! x (fx1+ x)))\n"
     "  x)",
     "5"},
    {"(subvector '#(aa bb cc dd) 1 3)", "#(bb cc)"},
    {"(subbytevector '#vu8(44 55 66 77) 2 3)", "B"},
    /* ------------------------- span ----------------------------- */
    {"(span 1 2 3)", "(span 1 2 3)"},
    {"(list->span '(foo bar baz))", "(span foo bar baz)"},
    {"(span-length (span 1 2 3))", "3"},
    {"(span-capacity-front (span 1 2 3))", "3"},
    {"(span-capacity-back (span 1 2 3))", "3"},
    {"(span-empty? (span))", "#t"},
    {"(span-empty? (span 'x))", "#f"},
    {"(span-back (span 'x 'y))", "y"},
    {"(span-ref (span 'a 'b 'c) 1)", "b"},
    {"(let* ((v (vector 1 2 3))\n"
     "       (sp (vector->span v)))\n"
     "  (vector-set! v 1 7)\n" /* set! does NOT propagate to the span */
     "  sp)",
     "(span 1 2 3)"},
    {"(let* ((v (vector 1 2 3))\n"
     "       (sp (vector->span* v)))\n"
     "  (vector-set! v 1 7)\n" /* set! propagates to the span */
     "  sp)",
     "(span 1 7 3)"},
    {"(let ((sp (span 'p 'q 'r)))\n"
     "  (span-insert-front! sp 'i 'j)\n"
     "  sp)",
     "(span i j p q r)"},
    {"(let ((sp (span 'foo)))\n"
     "  (span-insert-back! sp 'bar 'qux)\n"
     "  sp)",
     "(span foo bar qux)"},
    {"(let ((sp (span 1 2 3))"
     "      (sp2 (span -1 0)))\n"
     "  (span-sp-insert-front! sp sp2 0 2)\n"
     "  sp)",
     "(span -1 0 1 2 3)"},
    {"(let ((sp (span 1 2 3))"
     "      (sp2 (span -1 0)))\n"
     "  (span-sp-insert-back! sp sp2 0 2)\n"
     "  sp)",
     "(span 1 2 3 -1 0)"},
    {"(let ((sp (span 'a 'b 'c 'd)))\n"
     "  (span-erase-front! sp 3)\n"
     "  sp)",
     "(span d)"},
    {"(let ((sp (span 'a 'b 'c 'd)))\n"
     "  (span-erase-back! sp 1)\n"
     "  sp)",
     "(span a b c)"},
    {"(let ((sp (span 'a 'b 'c 'd)))\n"
     "  (span-find sp 0 999 (lambda (elem) (eq? 'c elem))))\n",
     "2"},
    /* ----------------------- bytespan --------------------------- */
    {"(bytespan 1 2 3)", "(bytespan 1 2 3)"},
    {"(list->bytespan '(56 12 0 46))", "(bytespan 56 12 0 46)"},
    {"(bytevector->bytespan #vu8(7 19 88 255))", "(bytespan 7 19 88 255)"},
    {"(bytespan->bytevector (bytespan 65 66 67))", "ABC"},
    {"(bytespan-length (bytespan 1 2 3))", "3"},
    {"(bytespan-capacity-back (bytespan 1 2 3))", "3"},
    {"(bytespan-empty? (bytespan))", "#t"},
    {"(bytespan-empty? (bytespan 250))", "#f"},
    {"(bytespan-u8-back (bytespan 251 252))", "252"},
    {"(bytespan-u8-ref (bytespan 252 253 254 255) 2)", "254"},
    {"(let* ((v (bytevector 1 2 3))\n"
     "       (sp (bytevector->bytespan v)))\n"
     "  (bytevector-u8-set! v 1 7)\n" /* set! does NOT propagate to the bytespan */
     "  sp)",
     "(bytespan 1 2 3)"},
    {"(let* ((v (bytevector 1 2 3))\n"
     "       (sp (bytevector->bytespan* v)))\n"
     "  (bytevector-u8-set! v 1 7)\n" /* set! propagates to the bytespan */
     "  sp)",
     "(bytespan 1 7 3)"},
    {"(let ((sp (bytespan 4 5 6)))\n"
     "  (bytespan-u8-insert-back! sp 7 8)\n"
     "  sp)",
     "(bytespan 4 5 6 7 8)"},
    {"(let ((sp (bytespan 9 10 11 12)))\n"
     "  (bytespan-u8-find sp 0 999 (lambda (elem) (eq? 11 elem))))\n",
     "2"},
    /* ----------------------- charspan --------------------------- */
    {"(charspan #\\1 #\\2 #\\3)", "(string->charspan* \"123\")"},
    {"(list->charspan '(#\\i #\\j #\\k #\\l))", "(string->charspan* \"ijkl\")"},
    {"(string->charspan \"pqrst\")", "(string->charspan* \"pqrst\")"},
    {"(string->charspan* \"ouh[()&*U\")", "(string->charspan* \"ouh[()&*U\")"},
    {"(charspan->string (string->charspan \"pqrst\"))", "pqrst"},
    {"(charspan-length (charspan #\\a #\\b #\\c))", "3"},
    {"(charspan-capacity-back (charspan #\\a #\\b #\\c))", "3"},
    {"(charspan-empty? (charspan))", "#t"},
    {"(charspan-empty? (charspan #\\~))", "#f"},
    {"(charspan-back (charspan #\\{ #\\\\))", "\\"},
    {"(charspan-ref (charspan #\\x #\\y #\\z) 2)", "z"},
    {"(let* ((s \"abc\")\n"
     "       (sp (string->charspan s)))\n"
     "  (string-set! s 1 #\\^)\n" /* set! does NOT propagate to the charspan */
     "  sp)",
     "(string->charspan* \"abc\")"},
    {"(let* ((s \"abc\")\n"
     "       (sp (string->charspan* s)))\n"
     "  (string-set! s 1 #\\^)\n" /* set! propagates to the charspan */
     "  sp)",
     "(string->charspan* \"a^c\")"},
    {"(let ((sp (charspan #\\A #\\B)))\n"
     "  (charspan-insert-front! sp #\\{ #\\~)\n"
     "  sp)",
     "(string->charspan* \"{~AB\")"},
    {"(let ((sp (charspan #\\4 #\\5 #\\6)))\n"
     "  (charspan-insert-back! sp #\\7 #\\8)\n"
     "  sp)",
     "(string->charspan* \"45678\")"},
    {"(let ((sp (string->charspan \"qwerty\")))\n"
     "  (charspan-erase-front! sp 1)\n"
     "  sp)",
     "(string->charspan* \"werty\")"},
    {"(let ((sp (string->charspan \"asdfuiop\")))\n"
     "  (charspan-erase-back! sp 3)\n"
     "  sp)",
     "(string->charspan* \"asdfu\")"},
    {"(let ((sp (charspan #\\@ #\\a #\\b #\\c)))\n"
     "  (charspan-find sp 0 999 (lambda (elem) (eq? #\\b elem))))\n",
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
    /* ----------------------- hashtable --------------------------- */
    {"(hashtable-cells\n"
     "  (eq-hashtable '(3 . C) '(2 . B) '(1 . A)))\n",
     "#((1 . A) (2 . B) (3 . C))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C)))\n",
     "#((3 . C) (1.0 . A) (2.1 . B))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(3.1 . C) '(2 . B) '(1 . A)))\n",
     "#((1 . A) (2 . B) (3.1 . C))"},
    {"(hashtable-cells\n"
     "  (hashtable string-hash string=? '(\"a\" . 1) '(\"B\" . 2) '(\"+\" . 3)))\n",
     "#((+ . 3) (B . 2) (a . 1))"},
    {"(string-hashtable->vector-of-bytevector0\n"
     "  (hashtable string-hash string=?\n"
     "             '(\"A\" . \"X\") '(\"B\" . \"Y\") '(\"C\" . \"Z\")))\n",
     "#(#vu8(67 61 90 0) #vu8(66 61 89 0) #vu8(65 61 88 0))"},
    {"(let ((ret '()))\n"
     "  (hashtable-iterate (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C))\n"
     "    (lambda (cell)\n"
     "      (set! ret (cons cell ret))))\n"
     "  ret)\n",
     "((2.1 . B) (1.0 . A) (3 . C))"},
    /* -------------------------- tty ------------------------------ */
    {"(let ((sz (tty-size)))\n"
     "  (and (pair? sz)\n"
     "       (integer? (car sz))\n"
     "       (integer? (cdr sz))\n"
     "       (positive? (car sz))\n"
     "       (positive? (cdr sz))))\n",
     "#t"},
    /* ------------------------- shell ----------------------------- */
    {"(begin\n"
     "  (sh-env-set! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env-get       #t \"foo\")\n"
     "    (sh-env-exported? #t \"foo\")))\n",
     "(bar . #f)"},
    {"(sh-cmd \"echo\" \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
    {"(sh-run (sh-cmd \"true\"))", "(exited . 0)"},
    {"(sh-run (sh-cmd \"false\"))", "(exited . 1)"},
    {"(make-multijob 'hello (lambda (j) '(exited . 42)))", "(sh-hello)"},
    {"(sh-run (make-multijob 'hello (lambda (j) '(exited . 42))))", "(exited . 42)"},
    {"(sh-run (make-multijob 'hello (lambda (j) '(killed . sigsegv))))", "(killed . sigsegv)"},
    {"(let ((j (sh-vec (sh-cmd \"false\") (sh-cmd \"true\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))\n",
     "(exited . 0)"},
    {"(let ((j (sh-vec (sh-cmd \"true\") (sh-cmd \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))\n",
     "(exited . 1)"},
    {"(sh-run (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 0)"},
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

void handle_scheme_exception(void) {
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
