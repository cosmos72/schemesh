/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"
#include "shell/shell.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h> /* chdir() */

#if !defined(__GNUC__) || defined(__OPTIMIZE__)
#define SCHEMESH_OPTIMIZE
#else
#undef SCHEMESH_OPTIMIZE
#endif

#define GRAY(str) "\033[30;1m" str "\033[m"

typedef struct {
  const char* string_to_eval;
  const char* expected_result;
} testcase;

static int      run_tests(void);
static unsigned run_test(const testcase* test);
static unsigned run_tests_utf8b(void);
static unsigned run_test_utf8b(ptr string, unsigned first_codepoint);
static unsigned adjust_codepoint(unsigned codepoint);

static const testcase tests[] = {
    {"", "#!eof"},
    {"(+ 1 2 3)", "6"},
    {"(* 4 5 6)", "120"},
    /* ----------------- bootstrap ------------------------------------------ */
    {"(let ((x 0))\n"
     "  (repeat 5 (set! x (fx1+ x)))\n"
     "  x)",
     "5"},
    {"(try (assert* 'who #t) (catch (ex) 1))", "#t"},
    {"(try (assert* 'who #f) (catch (ex) 2))", "2"},
    {"(values->list (values 1 2 3))", "(1 2 3)"},
    {"(let-macro ((plus . args) `(+ ,@args))\n"
     "  (plus 3 4 5))",
     "12"},
    {"(let-macro ((plus arg0 . args) `(+ ,arg0 ,@args))\n"
     "  (plus 3 4 5))",
     "12"},
    {"(expand '(-> a b (c ^ d) (e f ^)))", "(e f (c (b a) d))"},
    /* {"(begin (debugf \"warmup\") (debugf \"a\") (debugf \"b\") (debugf \"c\"))", ""}, */
    /* ----------------- containers/misc ------------------------------------ */
    {"(subvector '#(aa bb cc dd) 1 3)", "#(bb cc)"},
    {"(subbytevector #vu8(44 55 66 77) 2 3)", "B"},
    {"(bytevector-compare #vu8(44 55) #vu8(44 55))", "0"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 77 0))", "-1"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 78))", "-1"},
    {"(bytevector-compare #vu8(79) #vu8(78 0))", "1"},
    {"(string-range-count= \"qwertyuiop\" 2 \"_ertyuio7\" 1 8)", "7"},
    {"(format #f \"~s\" (string-split \"\" #\\:))", "(\"\")"},
    {"(format #f \"~s\" (string-split \":\" #\\:))", "(\"\" \"\")"},
    {"(format #f \"~s\" (string-split \"x:\" #\\:))", "(\"x\" \"\")"},
    {"(format #f \"~s\" (string-split \":y\" #\\:))", "(\"\" \"y\")"},
    {"(format #f \"~s\" (string-split \"ab:cdef::g\" 1 10 #\\:))", /*        */
     "(\"b\" \"cdef\" \"\" \"g\")"},
    {"(string-trim-split-at-blanks \"\"))", "()"},
    {"(format #f \"~s\" (string-trim-split-at-blanks"
     "  \"\\n\\x0;ab c\\x1f;\"))",
     "(\"ab\" \"c\")"},
    {"(list-remove-consecutive-duplicates!\n"
     "  (list \"foo\" \"foo\" \"foo\" \"bar\" \"bar\")\n"
     "  string=?)",
     "(foo bar)"},
    {"(do ((i #x-10000 (fx1+ i)))\n"
     "    ((fx>=? i #x120000))\n"
     "  (if (or (fx<=? #x0000 i #xD7FF)\n"
     "          (fx<=? #xDC80 i #xDCFF)\n"
     "          (fx<=? #xE000 i #x10FFFF))\n"
     "    (assert* 'test (fx=? i (char->integer (integer->char* i))))\n"
     "    (unless (throws? (integer->char* i))\n"
     "      (error 'integer->char* \"should throw\" i))))",
     ""},
    {"(string-range<? \"abcdef\" 1 5 \"_abxyef\" 2 4))\n", "#t"},
    /* ----------------- containers/sort ------------------------------------ */
    {"(let ((v (vector 9 8 7 6 5 4 3 2 1 0)))\n"
     "  (vector-range-sort! fx<? v 1 9)\n"
     "  v)",
     "#(9 1 2 3 4 5 6 7 8 0)"},
    /* ----------------- bytevector/utf8 ------------------------------------ */
    {"(values->list (bytevector-ref/utf8b #vu8() 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytevector-ref/utf8b #vu8(1) 0 1))", "(\x01 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(33) 0 1))", "(! 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(#x7E) 0 1))", "(~ 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(#x7F) 0 1))", "(\x7F 1)"},
    /* UTF-8b roundtrip #x80 -> U+DC80 -> #x80 */
    {"(first-value  (bytevector-ref/utf8b #vu8(#x80) 0 1))", "\x80"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xc0 #x80) 0 1))", "(#t 1)"},
    /* overlong UTF-8 sequences, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xc0 #x80) 0 2))", "\xc0"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xc1 #xbf) 0 2))", "\xc1"},
    /* bad UTF-8 continuation byte, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xc2 #x7F) 0 2))", "\xc2"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytevector-ref/utf8b #vu8(#xc2 #xa3) 0 2))", "(\xc2\xa3 2)"}, /* pound sign */
    {"(values->list (bytevector-ref/utf8b #vu8(#xc2 #xbf) 0 2))", "(\xc2\xbf 2)"},
    /* bad UTF-8 continuation byte, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xc2 #xc0) 0 2))", "\xc2"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytevector-ref/utf8b #vu8(#xe0 #x80 #x80) 0 2))", "(#t 2)"},  /* incomplete */
    /* overlong UTF-8 sequences, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xe0 #x80 #x80) 0 3))", "\xe0"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xe0 #x9f #xbf) 0 3))", "\xe0"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytevector-ref/utf8b #vu8(#xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytevector-ref/utf8b #vu8(#xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    /* invalid UTF-8 sequences, they represent forbidden codepoints in surrogate range
     * U+D800 ... U+DFFF -> only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xa0 #x80) 0 3))", "\xed"}, /* U+D800 */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xb2 #x80) 0 3))", "\xed"}, /* U+DC80 */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xb3 #xbf) 0 3))", "\xed"}, /* U+DCFF */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xbf #xbf) 0 3))", "\xed"}, /* U+DFFF */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xb2 #x80) 0 3))", "\xed"}, /* U+DC80 */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xb3 #xbf) 0 3))", "\xed"}, /* U+DCFF */
    {"(first-value (bytevector-ref/utf8b #vu8(#xed #xbf #xbf) 0 3))", "\xed"}, /* U+DFFF */
    {"(string->utf8b (string (integer->char* #xdc80)))", "\x80"},              /* U+DC80 */
    {"(string->utf8b (string (integer->char* #xdcff)))", "\xff"},              /* U+DCFF */
    {"(values->list (bytevector-ref/utf8b #vu8(#xee #x80 #x80) 0 3))",
     "(\xee\x80\x80 3)"}, /* U+E000 */
    {"(values->list (bytevector-ref/utf8b #vu8(#xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytevector-ref/utf8b #vu8(#xf0 #x80 #x80 #x80) 0 3))",
     "(#t 3)"}, /* incomplete */
    /* overlong UTF-8 sequences, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xf0 #x80 #x80 #x80) 0 4))", "\xf0"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xf0 #x8f #xbf #xbf) 0 4))", "\xf0"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytevector-ref/utf8b #vu8(#xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    /* invalid UTF-8 sequences, exceed U+10FFFF -> only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xf4 #x90 #x80 #x80) 0 4))", "\xf4"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xf4 #xbf #xbf #xbf) 0 4))", "\xf4"},
    /* invalid UTF-8 first byte >= #xf5 -> only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xf5 #x80 #x80 #x80) 0 4))", "\xf5"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xf6) 0 1))", "\xf6"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xfe) 0 1))", "\xfe"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xff) 0 1))", "\xff"},
    {"(let ((bv (make-bytevector 1)))\n"
     "  (bytevector-set/utf8b! bv 0 #\\~)\n"
     "  bv)",
     "~"},
    {"(list\n"
     "  (char->utf8b-length (integer->char 0))\n"
     "  (char->utf8b-length (integer->char #x7F))\n"
     "  (char->utf8b-length (integer->char #x80))\n"
     "  (char->utf8b-length (integer->char #x7ff))\n"
     "  (char->utf8b-length (integer->char #x800))\n"
     "  (char->utf8b-length (integer->char #xffff))\n"
     "  (char->utf8b-length (integer->char #x10000))\n"
     "  (char->utf8b-length (integer->char #x10ffff)))",
     "(1 1 2 2 3 3 4 4)"},
    {"(let ((bv (make-bytevector 1)))\n"
     "  (bytevector-set/utf8b! bv 0 #\\~)\n"
     "  bv)",
     "~"},
    {"(let ((bv (make-bytevector 2)))\n"
     "  (bytevector-set/utf8b! bv 0 (integer->char #xa3))\n" /* pound sign */
     "  bv)",
     "\xc2\xa3"},
    {"(let ((bv (make-bytevector 3)))\n"
     "  (bytevector-set/utf8b! bv 0 (integer->char #x20ac))\n" /* euro sign */
     "  bv)",
     "\xe2\x82\xac"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-set/utf8b! bv 0 (integer->char #x10348))\n"
     "  bv)",
     "\xf0\x90\x8d\x88"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-set/utf8b! bv 0 (integer->char #x10ffff))\n"
     "  bv)",
     "\xf4\x8f\xbf\xbf"},
    /* ----------------- bytespan-utf8b -------------------------------------- */
    {"(values->list (bytespan-ref/char (bytespan) 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytespan-ref/char (bytespan 1) 0 1))", "(\x01 1)"},
    {"(values->list (bytespan-ref/char (bytespan #x7F) 0 1))", "(\x7F 1)"},
    {"(first-value  (bytespan-ref/char (bytespan #x80) 0 1))", "\x80"},
    {"(values->list (bytespan-ref/char (bytespan #xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytespan-ref/char (bytespan #xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytespan-ref/char (bytespan #xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytespan-ref/char (bytespan #xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytespan-ref/char (bytespan #xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    /* invalid UTF-8 sequences, codepoints U+D800 ... U+DFFF are surrogate half
     * -> only first byte undergoes UTF-8b roundtrip */
    {"(first-value  (bytespan-ref/char (bytespan #xed #xa0 #x80) 0 3))", "\xed"},
    {"(first-value  (bytespan-ref/char (bytespan #xed #xbf #xbf) 0 3))", "\xed"},
    {"(values->list (bytespan-ref/char (bytespan #xee #x80 #x80) 0 3))",
     "(\xee\x80\x80 3)"}, /* U+E000 */
    {"(values->list (bytespan-ref/char (bytespan #xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytespan-ref/char (bytespan #xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytespan-ref/char (bytespan #xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/char! sp #\\~)\n"
     "  sp)",
     "(bytespan 126)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/char! sp (integer->char #xa3))\n" /* pound sign */
     "  sp)",
     "(bytespan 194 163)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/char! sp (integer->char #x20ac))\n" /* euro sign */
     "  sp)",
     "(bytespan 226 130 172)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-front/char! sp (integer->char #x10348))\n"
     "  sp)",
     "(bytespan 240 144 141 136)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-front/char! sp (integer->char #x10ffff))\n"
     "  sp)",
     "(bytespan 244 143 191 191)"},
    /* ----------------- bytespan-fixnum-display ---------------------------- */
    {"(let ((sp (bytespan)))\n"
     "  (list-iterate '(0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 "
     "                  9999998 10000000 12345678 -1 -9 -10 -87654321)\n"
     "    (lambda (n)\n"
     "      (bytespan-display-back/fixnum! sp n)\n"
     "      (bytespan-insert-back/u8! sp 32)))\n"
     "  (bytespan->bytevector sp))",
     "0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 "
     "9999998 10000000 12345678 -1 -9 -10 -87654321 "},
    /* ------------------------- span --------------------------------------- */
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
     "  (span-insert-front/span! sp sp2 0 2)\n"
     "  sp)",
     "(span -1 0 1 2 3)"},
    {"(let ((sp (span 1 2 3))"
     "      (sp2 (span -1 0)))\n"
     "  (span-insert-back/span! sp sp2 0 2)\n"
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
     "  (span-find sp 0 999 (lambda (elem) (eq? 'c elem))))",
     "2"},
    /* ----------------------- bytespan ------------------------------------- */
    {"(bytespan 1 2 3)", "(bytespan 1 2 3)"},
    {"(list->bytespan '(56 12 0 46))", "(bytespan 56 12 0 46)"},
    {"(bytevector->bytespan #vu8(7 19 88 255))", "(bytespan 7 19 88 255)"},
    {"(bytespan->bytevector (bytespan 65 66 67))", "ABC"},
    {"(bytespan-length (bytespan 1 2 3))", "3"},
    {"(bytespan-capacity-back (bytespan 1 2 3))", "3"},
    {"(bytespan-empty? (bytespan))", "#t"},
    {"(bytespan-empty? (bytespan 250))", "#f"},
    {"(bytespan-back/u8 (bytespan 251 252))", "252"},
    {"(bytespan-ref/u8 (bytespan 252 253 254 255) 2)", "254"},
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
     "  (bytespan-insert-back/u8! sp 7 8)\n"
     "  sp)",
     "(bytespan 4 5 6 7 8)"},
    {"(let ((bsp (bytespan 9 10 11 12)))\n"
     "  (bytespan-find/u8 bsp 0 (bytespan-length bsp) (lambda (elem) (fx=? 11 elem))))",
     "2"},
    /* ----------------------- charspan ------------------------------------- */
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
    {"(charspan-range-count= (string->charspan* \"abcdef\") 2 (string->charspan* \"1cde34\") 1 4)",
     "3"},
    {"(charspan-range=? (string->charspan* \"abcdef\") 2 (string->charspan* \"1cde34\") 1 3)",
     "#t"},
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
     "  (charspan-find sp (lambda (elem) (eq? #\\b elem))))",
     "2"},
    {"(charspan->utf8b (string->charspan* \"\x7c \xce\x98 \xe0\xa4\xb9 \xf0\x90\x8d\x88\"))",
     "(bytespan 124 32 206 152 32 224 164 185 32 240 144 141 136)"},
    {"(bytevector->bytespan (text->bytevector0 (string->charspan* \"123\\x0;\")))",
     "(bytespan 49 50 51 0)"},
    /* ----------------------- gbuffer --------------------------- */
    {"(gbuffer 'a 2 3.7)", "(gbuffer a 2 3.7)"},
    {"(vector->gbuffer* (vector 0 1 2))", "(gbuffer 0 1 2)"},
    {"(span->gbuffer* (span 0 1 2))", "(gbuffer 0 1 2)"},
    {"(let ((gb (make-gbuffer 5 #f)))\n"
     "  (gbuffer-iterate gb\n"
     "    (lambda (i elem)\n"
     "      (gbuffer-set! gb i (fx- i))))\n"
     "  gb)",
     "(gbuffer 0 -1 -2 -3 -4)"},
    {"(let ((gb (gbuffer 'a 'b 'c 'd 'e)))\n"
     "  (gbuffer-erase-range! gb 2 4)\n"
     "  (gbuffer-insert-at! gb 1 'x)\n"
     "  gb)",
     "(gbuffer a x b e)"},
    /* --------------------- chargbuffer ------------------------------------ */
    {"(chargbuffer #\\X #\\Y #\\Z)", "(string->chargbuffer* \"XYZ\")"},
    {"(string->chargbuffer* \"qwerty\")", "(string->chargbuffer* \"qwerty\")"},
    {"(charspan->chargbuffer* (string->charspan* \"abcdef\"))",
     "(string->chargbuffer* \"abcdef\")"},
    {"(let ((gb (make-chargbuffer 5 #\\@)))\n"
     "  (chargbuffer-iterate gb\n"
     "    (lambda (i elem)\n"
     "      (chargbuffer-set! gb i (integer->char (fx+ i 64)))))\n"
     "  gb)",
     "(string->chargbuffer* \"@ABCD\")"},
    {"(let ((gb (chargbuffer #\\a #\\b #\\c #\\d #\\e)))\n"
     "  (chargbuffer-erase-range! gb 2 4)\n"
     "  (chargbuffer-insert-at! gb 1 #\\x)\n"
     "  gb)",
     "(string->chargbuffer* \"axbe\")"},
    /* ------------------------ charline ------------------------------------ */
    {"(string->charline \"abc 123\")", "(string->charline* \"abc 123\")"},
    {"(string->charline* \"echo \\n\")", "(string->charline* \"echo \\n\")"},
    {"(charline-nl? (string->charline \"echo \\n\"))", "#t"},
    {"(charline-length (string->charline \"echo \\n\"))", "6"},
    {"(charline-find/left (string->charline* \"qwerty=<>\") 9\n"
     "  (lambda (ch) (char=? ch #\\=)))",
     "6"},
    {"(let ((line (string->charline* \"foo/bar\"))\n"
     "      (sp   (span))\n"
     "      (pred (lambda (ch) (char=? ch #\\b))))\n"
     "  (do ((i 0 (fx1+ i)))\n"
     "      ((fx>=? i 10) sp)\n"
     "    (span-insert-back! sp (charline-find/right line i pred))))\n",
     "(span 4 4 4 4 4 #f #f #f #f #f)"},
    {"(let ((line (string->charline* \"qwerty===\"))\n"
     "      (sp   (span))\n"
     "      (pred (lambda (ch) (char=? ch #\\=))))\n"
     "  (do ((i 0 (fx1+ i)))\n"
     "      ((fx>=? i 12) sp)\n"
     "    (span-insert-back! sp (charline-count/left line i pred))))\n",
     "(span 0 0 0 0 0 0 0 1 2 3 3 3)"},
    {"(let ((line (string->charline* \"qwerty===\"))\n"
     "      (sp   (span))\n"
     "      (pred (lambda (ch) (char=? ch #\\=))))\n"
     "  (do ((i 0 (fx1+ i)))\n"
     "      ((fx>=? i 12) sp)\n"
     "    (span-insert-back! sp (charline-count/right line i pred))))\n",
     "(span 0 0 0 0 0 0 3 2 1 0 0 0)"},
    {"(let* ((l1 (string->charline* \"foo/bar\"))\n"
     "       (l2 (charline-copy-on-write l1)))\n"
     "  (charline-erase-range! l1 3 4)\n"
     "  (charline-insert-at! l2 3 #\\~)\n"
     "  (list l1 l2))",
     "((string->charline* \"foobar\") (string->charline* \"foo~/bar\"))"},
    {"(let* ((l1 (string->charline* \"abcdefgh\"))\n"
     "       (l2 (charline-copy-on-write l1)))\n"
     "  (charline-insert-at/cbuf! l1 5 (string->charline* \"012345\") 2 5)\n"
     "  (list l1 l2))",
     "((string->charline* \"abcde234fgh\") (string->charline* \"abcdefgh\"))"},
    /* ------------------------ charlines ----------------------------------- */
    {"(charlines (string->charline* \"foo/bar\") (string->charline \"\\n\"))",
     "(strings->charlines* \"foo/bar\" \"\\n\")"},
    {"(charlines-find/left (strings->charlines* \"qwerty@$%\" \"asdf\")\n"
     "  4 1\n"
     "  (lambda (ch) (char=? ch #\\@)))",
     "7"},
    {"(charlines-find/right (charlines (string->charline* \"IOHPR$\n\")\n"
     "                                  (string->charline* \"ORJZX\"))\n"
     "  0 0\n"
     "  (lambda (ch) (char=? ch #\\Z)))",
     "10"},
    {"(charlines-count/left (strings->charlines* \"abc\n\" \"ccc\")\n"
     "  4 1\n"
     "  (lambda (ch) (char=? ch #\\c)))",
     "3"},
    {"(charlines-count/right (strings->charlines* \"abc\n\" \"ccc\")\n"
     "  3 0\n"
     "  (lambda (ch) (not (char=? ch #\\c))))",
     "1"},
    /* ------------------------ vscreen ------------------------------------- */
    {"(let ((screen (vscreen* 8 30 \"qwerty\\n\" \"asdfgh\")))\n"
     "  (vscreen-cursor-vxy-set! screen 3 1)\n"
     "  (vscreen-cursor-move/left! screen 6)\n"
     "  (values->list\n"
     "    (vscreen-cursor-vxy screen)))\n",
     "(4 0)"},
    {"(let ((screen (vscreen* 8 30 \"qwertyuiop\\n\" \"asdfgh\")))\n"
     "  (vscreen-cursor-vxy-set! screen 5 0)\n"
     "  (vscreen-cursor-move/right! screen 13)\n"
     "  (values->list\n"
     "    (vscreen-cursor-vxy screen)))\n",
     "(6 1)"},
    {"(let ((screen (vscreen* 8 30 \"qwerty\\n\" \"asdfghjkl\")))\n"
     "  (vscreen-cursor-vxy-set! screen 9 1)\n"
     "  (vscreen-cursor-move/up! screen 1)\n"
     "  (values->list\n"
     "    (vscreen-cursor-vxy screen)))\n",
     "(6 0)"},
    {"(let ((screen (vscreen* 8 30 \"abcdef\\n\" \"0123456\")))\n"
     "  (vscreen-erase-at-xy! screen 5 0 3)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcde123\" \"456\")"},
    {"(let ((screen (vscreen* 8 30 \"{[()]}\\n\" \"abcdef\" \"0123456\")))\n"
     "  (vscreen-cursor-vxy-set! screen 3 2)\n"
     "  (vscreen-erase-left/line! screen)\n"
     "  screen)",
     "(vscreen* 8 30 \"{[()]}\\n\" \"3456\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdef\" \"012\\n\" \"{[()]}\\n\")))\n"
     "  (vscreen-cursor-vxy-set! screen 4 0)\n"
     "  (vscreen-erase-right/line! screen)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcd\\n\" \"{[()]}\\n\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdef\" \"012\\n\")))\n"
     "  (vscreen-insert-at-xy/char! screen 4 1 #\\space)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcdef\" \"012\\n\" \" \")"},
    {"(let ((screen (vscreen* 8 30 \"abcdef\" \"012\\n\")))\n"
     "  (vscreen-insert-at-xy/newline! screen 4 0)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcd\\n\" \"ef012\\n\" \"\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (vscreen-insert-at-xy/cspan! screen 4 0 (string->charspan* \"uwxyz\"))\n"
     "  screen)",
     "(vscreen* 8 30 \"abcduwxy\" \"zefgh012\" \"\\n\" \"\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (values->list (vscreen-count-before-xy/left screen 4 1"
     "                  (lambda (ch) (not (char=? ch #\\d))))))",
     "(4 0 8)"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (values->list (vscreen-count-at-xy/right screen 4 0"
     "                  (lambda (ch) (not (char=? ch #\\newline))))))",
     "(3 1 7)"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\" \"qwert\")))\n"
     "  (vscreen-cursor-ixy-set! screen 3 1)\n" /* move the cursor to the char '\n' */
     "  (vscreen-resize! screen 5 30)\n"
     "  (list (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) screen))",
     "(1 2 (vscreen* 5 30 \"abcde\" \"fgh01\" \"2\\n\" \"qwert\" \"\"))"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\" \"qwerty\")))\n"
     "  (vscreen-cursor-ixy-set! screen 3 1)\n" /* move the cursor to the char '\n' */
     "  (vscreen-resize! screen 9 30)\n"
     "  (list (vscreen-cursor-ix screen) (vscreen-cursor-iy screen) screen))",
     "(2 1 (vscreen* 9 30 \"abcdefgh0\" \"12\\n\" \"qwerty\"))"},
    /* --------------------- list ------------------------------------------- */
    {"(let ((ret '()))\n"
     "  (list-iterate '(a b c)\n"
     "    (lambda (elem)\n"
     "      (set! ret (cons elem ret))\n"
     /*     stop iterating if (eq? 'b elem) */
     "      (not (eq? 'b elem))))\n"
     "  ret)",
     "(b a)"},
    {"(list-reverse*! (list))", "()"},
    {"(list-reverse*! (list 1))", "(1)"},
    {"(list-reverse*! (list 1 2))", "(2 . 1)"},
    {"(list-reverse*! (list 1 2 3 4 5 6))", "(6 5 4 3 2 . 1)"},
    /* ------------------------ hashtable ----------------------------------- */
    {"(hashtable-cells\n"
     "  (eq-hashtable '(3 . C) '(2 . B) '(1 . A)))",
     "#((1 . A) (2 . B) (3 . C))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C)))",
     "#((3 . C) (2.1 . B) (1.0 . A))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(3.1 . C) '(2 . B) '(1 . A)))",
     "#((1 . A) (2 . B) (3.1 . C))"},
    {"(hashtable-cells\n"
     "  (hashtable string-hash string=? '(\"a\" . 1) '(\"B\" . 2) '(\"+\" . 3)))",
     "#((a . 1) (+ . 3) (B . 2))"},
    {"(string-hashtable->argv\n"
     "  (hashtable string-hash string=?\n"
     "             '(\"A\" . \"X\") '(\"B\" . \"Y\") '(\"C\" . \"Z\")))",
     "#(#vu8(67 61 90 0) #vu8(65 61 88 0) #vu8(66 61 89 0))"},
    {"(let ((ret '()))\n"
     "  (hashtable-iterate (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C))\n"
     "    (lambda (cell)\n"
     "      (set! ret (cons cell ret))))\n"
     "  ret)",
     "((1.0 . A) (2.1 . B) (3 . C))"},
    /* ------------------------ lineedit io --------------------------------- */
    {"(get-string-all\n"
     "  (open-charline-input-port\n"
     "    (string->charline* \"58gu405gu*(&)\\n\"))))",
     "58gu405gu*(&)\n"},
    {"(get-string-all\n"
     "  (open-charlines-input-port\n"
     "    (charlines\n"
     "      (string->charline* \"085ug&^%\\n\")))))",
     "085ug&^%\n"},
    {"(read\n"
     "  (open-charline-input-port\n"
     "    (string->charline* \"(re8u (+ -) [* /] 'foo bar . baz)\"))))",
     "(re8u (+ -) (* /) 'foo bar . baz)"},
    {"(read\n"
     "  (open-charlines-input-port\n"
     "    (charlines\n"
     "      (string->charline* \"(urehg* (a . 'b) 12\")\n"
     "      (charline)\n"
     "      (string->charline* \"3.45e3 . #\\\\m\\n)\"))))",
     "(urehg* (a quote b) 123450.0 . m)"},
    /* ------------------------ parser scheme ------------------------------- */
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"foo bar\"))",
     "(foo bar)"},
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"(foo bar) '(a b)\"))",
     "((foo bar) '(a b))"},
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"(a (b c . d) . e)\"))",
     "((a (b c . d) . e))"},
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"(list #| '\\\" . #| ,`@# |# |#" /* nested block comments */
     "      '#(a 1.0 2/3) #2(d) #vu8(1 2 3) #4vu8(9) #vfx(-1 0 2) #3vfx(4))\"))",
     "((list '#(a 1.0 2/3) #(d d) #vu8(1 2 3) #vu8(9 9 9 9) #vfx(-1 0 2) #vfx(4 4 4)))"},

    /* invariant: {#!scheme ...} is always equivalent to (...) */
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"(1 2 3)\" (parsers)))",
     "((1 2 3))"},
    {"(parse-scheme-forms1 (string->parsectx"
     "  \"{#!scheme 1 2 3}\" (parsers)))",
     "((1 2 3))"},
    /* ------------------------ parser shell1 ------------------------------- */
    {"(parse-shell-form1 (string->parsectx \"\")))", ""},
    {"(parse-shell-form1 (string->parsectx \"{}\")))", "(shell)"},
    {"(parse-shell-form1 (string->parsectx \"{{}}\")))", "(shell (shell))"},
    {"(parse-shell-form1 (string->parsectx \"ls -l>/dev/null&\"))", "(shell ls -l > /dev/null &)"},
    {"(parse-shell-form1 (string->parsectx \"{;foo} <log 2>&1 && bar<>baz|wc -l;;\"))",
     "(shell (shell ; foo) < log 2 >& 1 && bar <> baz | wc -l ; ;)"},
    {"(parse-shell-form1 (string->parsectx \"echo|{cat;{true}\n}&\"))",
     "(shell echo | (shell cat ; (shell true) ;) &)"},
    {"(parse-shell-form1 (string->parsectx \"ls; [foo || bar &] & echo\"))",
     "(shell ls ; (shell-subshell foo || bar &) & echo)"},
    {"(parse-shell-form1 (string->parsectx \"ls && [A=1 foo || bar &] || [B=2 echo]\"))",
     "(shell ls && (shell-subshell A = 1 foo || bar &) || (shell-subshell B = 2 echo))"},
    {"(parse-shell-form1 (string->parsectx \"ls[A-Z]?[!ax-z] .\"))",
     "(shell (shell-wildcard ls % A-Z ? %! ax-z) .)"},
    {"(parse-shell-form1 (string->parsectx \"{{{{echo|cat}}}}\"))",
     "(shell (shell (shell (shell echo | cat))))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"a<>/dev/null||b>/dev/zero&&!c>&2\"))",
     "(shell a <> /dev/null || b > /dev/zero && ! c >& 2)"},
    /* test fd number [N] before redirection */
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"foo 0</dev/zero 1<>/dev/urandom 2<&- 3>>logfile 4>otherfile 5>&/dev/null\")))",
     "(shell \"foo\" 0 < \"/dev/zero\" 1 <> \"/dev/urandom\" 2 <& \"-\" 3 >> \"logfile\""
     " 4 > \"otherfile\" 5 >& \"/dev/null\")"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls \\\"-l\\\" '.'\")))",
     "(shell \"ls\" \"-l\" \".\")"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls \\\"some\\\"'file'path\")))",
     "(shell \"ls\" (shell-wildcard \"some\" \"file\" \"path\"))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls `cmd1 && cmd2 || cmd3 -arg3`\"))))",
     "(shell \"ls\" (shell-backquote \"cmd1\" && \"cmd2\" \\x7C;\\x7C; \"cmd3\" \"-arg3\"))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls $var1 \\\"$var2\\\" '$var3'\")))",
     "(shell \"ls\" (shell-env \"var1\") (shell-env \"var2\") \"$var3\")"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls ${v 1} \\\"${ v 2 }\\\" '${ v 3 }'\")))",
     "(shell \"ls\" (shell-env \"v 1\") (shell-env \" v 2 \") \"${ v 3 }\")"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls \\\"$var1\\\"'$var2'$var3\")))",
     "(shell \"ls\" (shell-wildcard (shell-env \"var1\") \"$var2\" (shell-env \"var3\")))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls $(cmd arg $var)\")))",
     "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls \\\"$(cmd arg $var)\\\"\")))",
     "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"ls '$(cmd arg $var)'\")))",
     "(shell \"ls\" \"$(cmd arg $var)\")"},
    /* test () inside shell syntax */
    {"(parse-shell-form1 (string->parsectx \"echo a || (cons 1 2)\" (parsers)))",
     "(shell echo a || (cons 1 2))"},
    /* ------------------------ parse-forms --------------------------------- */
    {"(parse-forms1\n"
     "  (string->parsectx \"\" (parsers))\n"
     "  'scheme)",
     "()"},
    {"(parse-forms1\n"
     "  (string->parsectx \"+\" (parsers))\n"
     "  'scheme)",
     "(+)"},
    {"(parse-forms1\n"
     /* #!eof is equivalent to end-of-file in the input port */
     "  (string->parsectx \"'(a . b) c #!eof . ) syntax error\" (parsers))\n"
     "  'scheme)",
     "('(a . b) c)"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"uiop asdf #!scheme xyz %%a\" (parsers))\n"
     "  'scheme))",
     "((uiop asdf xyz %%a) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"uiop asdf #!scheme (xyz %%a)\" (parsers))\n"
     "  'scheme))",
     "((uiop asdf (xyz %%a)) #<parser scheme>)"},
    /* #! not followed by [0-9A-Za-z] skips the rest of line */
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"qwerty #!/some/path '()\" (parsers))\n"
     "  'scheme))",
     "((qwerty) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"qwerty #!/some/path\\n '()\" (parsers))\n"
     "  'scheme))",
     "((qwerty '()) #<parser scheme>)"},
    /* ; skips the rest of line too */
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"uiop ; this is a comment\" (parsers))\n"
     "  'scheme))",
     "((uiop) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"uiop ;\\n this is not a comment\" (parsers))\n"
     "  'scheme))",
     "((uiop this is not a comment) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)\" (parsers))\n"
     "  'scheme))",
     "((`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)) #<parser scheme>)"},
    {"(parse-forms1\n"
     "  (string->parsectx \"foo && bar || baz &\" (parsers))\n"
     "  'shell)",
     "((shell foo && bar || baz &))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"ls -l >& log.txt\" (parsers))\n"
     "  'shell))",
     "((shell ls -l >& log.txt))"},
    /* character { switches to shell parser */
    {"(parse-forms1\n"
     "  (string->parsectx \"7 {ls -l >& log.txt}\" (parsers))\n"
     "  'scheme))",
     "(7 (shell ls -l >& log.txt))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"(values {ls -al >> log.txt})\" (parsers))\n"
     "  'scheme))",
     "((values (shell ls -al >> log.txt)))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"9 #!shell ls -al >> log.txt\" (parsers))\n"
     "  'scheme))",
     "(9 (shell ls -al >> log.txt))"},
    /* directive #!shell switches to shell parser also inside (...) */
    {"(parse-forms1\n"
     "  (string->parsectx \"(#!shell ls -al >> log.txt)\" (parsers))\n"
     "  'scheme))",
     "((shell ls -al >> log.txt))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"(values foo bar #!shell baz >> log.txt; wc -l log.txt)\""
     "    (parsers))\n"
     "  'scheme))",
     "((values foo bar (shell baz >> log.txt ; wc -l log.txt)))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"ls ; #!shell echo\" (parsers))\n"
     "  'shell))",
     "((shell ls ; echo))"},
    /* ( inside shell syntax switches to Scheme parser for a single Scheme form,
     * then continues parsing shell syntax.
     * An eof, newline, semicolon or ( is required after (...) if ( was initial */
    {"(parse-forms1\n"
     "  (string->parsectx \"(+ 1 2)\" (parsers))\n"
     "  'shell)",
     "((+ 1 2))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"(+ 2 3) ; echo\" (parsers))\n"
     "  'shell)",
     "((+ 2 3) (shell echo))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"(+ 4 5) (* 6 7) ; ls\" (parsers))\n"
     "  'shell)",
     "((+ 4 5) (* 6 7) (shell ls))"},
    /* test inserting Scheme forms inside shell syntax */
    {"(parse-forms1\n"
     "  (string->parsectx \"echo (+ 8 9)\" (parsers))\n"
     "  'shell)",
     "((shell echo (+ 8 9)))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"ls (apply + a `(,@b)) &\" (parsers))\n"
     "  'shell))",
     "((shell ls (apply + a `(,@b)) &))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"ls (my-dir) >> log.txt\" (parsers))\n"
     "  'shell))",
     "((shell ls (my-dir) >> log.txt))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"{foo; bar}\" (parsers))\n"
     "  'shell)",
     "((shell foo ; bar))"},
    {"(parse-forms1\n"
     "  (string->parsectx \"[foo; bar]\" (parsers))\n"
     "  'shell)",
     "((shell-subshell foo ; bar))"},
    /* open bracket [ at the beginning of a command starts a subshell, not a wildcard */
    {"(parse-forms1\n"
     "  (string->parsectx \"[foo] [bar]\" (parsers))\n"
     "  'shell)",
     "((shell (shell-subshell foo) (shell-subshell bar)))"},
    /* open bracket [ not at the beginning of a command starts a wildcard, not a subshell */
    {"(parse-forms1\n"
     "  (string->parsectx \"''[foo] [bar]\" (parsers))\n"
     "  'shell)",
     "((shell (shell-wildcard % foo) (shell-wildcard % bar)))"},
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"ls ~; #!scheme (f a b)\" (parsers))\n"
     "  'shell))",
     "(((shell ls (shell-wildcard ~) ;) (f a b)) #<parser scheme>)"},
    /* test multiple #!... directives */
    {"(values->list (parse-forms\n"
     "  (string->parsectx \"(+ a b) #!shell ls -al >> log.txt; #!scheme foo bar\""
     "    (parsers))\n"
     "  'scheme))",
     "(((+ a b) (shell ls -al >> log.txt ;) foo bar) #<parser scheme>)"},
    /* ------------------------ parse-paren -------------------------------- */
    {"(values->list (paren->values (string->paren \"{\")))", "(scheme #t 0 0 #t 1 0)"},
    {"(values->list (paren->values (string->paren \"{[(\")))", "(scheme #t 0 0 #t 3 0)"},
    {"(values->list (paren->values (paren-inner-ref (string->paren \"{\") 0)))",
     "(shell { 0 0 #f 1 0)"},
    {"(values->list (paren->values (string->paren \"{\n\")))", "(scheme #t 0 0 #t 0 1)"},
    {"(values->list (paren->values (paren-inner-ref (string->paren \"{\n\") 0)))",
     "(shell { 0 0 #f 0 1)"},
    {"(values->list (paren->values (paren-inner-ref* (string->paren \"{[(\") 0 0 0)))",
     "(scheme ( 2 0 #f 3 0)"},
    {"(string->paren \"(foo \\\"a()\\\" \\\"b[]\\\" \\\"c{}\\\" [* |2| 3])\")",
     "#<paren _(\"\" \"\" \"\" [||])_>"},
    {"(string->paren \"#\\newline #\\\\( #\\\\) #\\\\[ #\\\\] #\\\\{ #\\\\} #\\\\#\")",
     "#<paren __>"},
    {"(string->paren \"#| comment . , \\\\ |#\")", "#<paren _##_>"},
    /* [] are grouping tokens in shell syntax, and `` are not special in lisp syntax */
    {"(string->paren \"{[(``)]}\")", "#<paren _{[()]}_>"},
    /* [] are grouping tokens in lisp syntax and `` are grouping tokens in shell syntax */
    {"(string->paren \"([{``}])\")", "#<paren _([{``}])_>"},
    /* test $( shell syntax )*/
    {"(string->paren \"{$(`{}()`)}\")", "#<paren _{(`{} ()`)}_>"},
    /* test single-quoted strings in shell syntax */
    {"(string->paren \"{'foo\\\"bar{}[]()``baz'}\")", "#<paren _{''}_>"},
    /* test double-quoted strings in shell syntax */
    {"(string->paren \"{\\\"foobar{}[]``${baz}\\\"}\")", "#<paren _{\"`` {}\"}_>"},
    /* paren are not special in shell syntax inside double quoted string */
    {"(string->paren \"{\\\"()\\\"}\")", "#<paren _{\"\"}_>"},
    /* parse mismatched paren */
    {"(string->paren \"'\" 'shell)", "#<paren _'" GRAY("'") "_>"},
    {"(string->paren \"([{)]}\")",
     "#<paren _([{" GRAY("(") ") " GRAY("[") "]}" GRAY("]") GRAY(")") "_>"},
    {"(string->paren \"(\\\" a\\\"\")", "#<paren _(\"\"" GRAY(")") "_>"},
    /* the code after #!scheme is inside a nested paren with name = 'scheme */
    {"(string->paren \"ls #!scheme 1 2 3\" 'shell)", "#<paren ____>"},
    {"(string->paren \"{ls ; #!scheme 1 2 3}\")", "#<paren _{{}}_>"},
    {"(string->paren \"(values '{ls; #!scheme 1 2 3})\")", "#<paren _({{}})_>"},
    {"(let ((p (string->paren \"{[a] && b]\")))\n"
     "  (list\n"
     "    (paren-find/surrounds p 0 0)\n"
     "    (paren-find/surrounds p 1 0)\n"
     "    (paren-find/surrounds p 2 0)\n"
     "    (paren-find/surrounds p 3 0)\n"
     "    (paren-find/surrounds p 4 0)\n"
     "    (paren-find/surrounds p 5 0)\n"
     "    (paren-find/surrounds p 6 0)\n"
     "    (paren-find/surrounds p 7 0)\n"
     "    (paren-find/surrounds p 8 0)\n"
     "    (paren-find/surrounds p 9 0)\n"
     "    (paren-find/surrounds p 10 0)))",
     ""
#define P0_ "#<paren _{[] " GRAY("[") "]" GRAY("}") "_> "
#define P1 "#<paren {[] " GRAY("[") "]" GRAY("}") ">"
#define P1_ P1 " "
#define P2_ "#<paren []> "
     "(" P0_ P1_ P2_ P2_ P1_ P1_ P1_ P1_ P1_ P1_ P1 ")"},
#undef P0_
#undef P1
#undef P1_
#undef P2_
    /* -------------------------- parenmatcher -------------------------------*/
    {"(values->list (paren->values\n"
     "  (parenmatcher-find/at\n"
     "    (make-parenmatcher)\n"
     "    (string->parsectx \"([{``}] #| |# )\" (parsers))\n"
     "    'scheme\n"
     "    6 0)))",
     "(scheme [ 1 0 ] 6 0)"},
    {"(values->list (paren->values\n"
     "  (parenmatcher-find/surrounds\n"
     "    (make-parenmatcher)\n"
     "    (string->parsectx \"([{``)))\" (parsers))\n"
     "    'scheme\n"
     "    6 0)))",
     "(shell { 2 0 #f 8 0)"},
    /* -------------------------- tty --------------------------------------- */
    /* (tty-size) returs a cons (width . height), or c_errno() < 0 on error */
    {"(let ((sz (tty-size)))\n"
     "  (if (pair? sz)\n"
     "    (and (integer? (car sz)) (positive? (car sz))\n"
     "         (integer? (cdr sz)) (positive? (cdr sz)))\n"
     "    (and (integer? sz) (negative? sz))))\n",
     "#t"},
    /* ------------------------- posix -------------------------------------- */
    {"(c-errno)", "0"},
    {"(file-type \".\" 'catch)", "dir"},
    {"(file-type \"parser/parser.ss\" 'catch)", "file"},
    {"(directory-sort! (directory-list \"parser\"))",
     "((dir . .) (dir . ..) (file . lisp.ss) (file . parser.ss)"
     " (file . r6rs.ss) (file . scheme.ss) (file . shell.ss))"},
    /* ------------------------- posix patterns ----------------------------- */
    {"(sh-pattern \"foo\" '* \".bar\" '? '% \"[a-z]\" '%! \"A-Z\")",
     "(sh-pattern foo '* .bar '? '% [a-z] '%! A-Z)"},
    {"(sh-pattern '* '% \"ch\")", "(sh-pattern '* '% ch)"},
    {"(try (sh-pattern \"foo\" \".bar\") #f (catch (ex) #t))", "#t"},
    {"(try (sh-pattern '%) #f (catch (ex) #t))", "#t"},
    {"(try (sh-pattern '%!) #f (catch (ex) #t))", "#t"},
    {"(try (sh-pattern '+) #f (catch (ex) #t))", "#t"},
    {"(sh-pattern-match?"
     "  (sh-pattern \"foo\" '? \"bar\")"
     "  \"foo.bar\")",
     "#t"},
    {"(sh-pattern-match?"
     "  (sh-pattern \"asdf\" '% \"abc.\" '%! \"a-pr-z\" \"werty\" '?)"
     "  \"asdf.qwerty.\")",
     "#t"},
    {"(try (sh-pattern-match? (sh-pattern '* '% \"ch\") \"shell.c\") (catch (ex) ex))", "#t"},
    /* initial wildcards never match an initial dot */
    {"(sh-pattern-match? (sh-pattern '? \"foo\")        \".foo\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '% \" ~\" \"foo\") \".foo\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '% \".\" \"foo\")  \".foo\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '%! \"f\" \"foo\") \".foo\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '*)                \".foo\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '* \"foo\")        \".my.foo\")", "#f"},
    /* match empty pattern */
    {"(sh-pattern-match? (sh-pattern) \"\")", "#t"},
    {"(sh-pattern-match? (sh-pattern) \"o\")", "#f"},
    /* match empty string */
    {"(sh-pattern-match? (sh-pattern '*) \"\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '?) \"\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '% \" -~\") \"\")", "#f"},
    {"(sh-pattern-match? (sh-pattern '% \"!~\") \"\")", "#f"},
    /* match string against '* */
    {"(sh-pattern-match? (sh-pattern '*) \"uiop.def..\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '*) \"\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* '*) \"\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* '* '*) \"\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* \"bar\") \"foo.bar\")", "#t"},
    {"(sh-pattern-match? (sh-pattern \"abc\" '* \"def\") \"abc...def\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* \"zzz\" '? '*) \"abc.zzz.def\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* \"xyz\" '%! \"x-z\" \"xyz\" '*) \"xyzxyz.xyz\")", "#t"},
    {"(sh-pattern-match? (sh-pattern '* '* \"abc\" '%! \".\" '* '* \"abc\" '* '*)"
     " \"abc.zzz.abc^abc\")",
     "#t"},
    /* ------------------------- shell paths -------------------------------- */
    {"(sh-path-absolute? (string->charspan* \"/foo\"))", "#t"},
    {"(sh-path-absolute? (string->charspan* \"bar/\"))", "#f"},
    {"(sh-path \"//foo///bar////\")", "(string->charspan* \"//foo///bar////\")"},
    {"(sh-subpath \"//foo///bar////\")", "(string->charspan* \"/foo/bar\")"},
    {"(sh-subpath \"/foo/bar/\" \"/aaa/\" \"../baz/bbbb/\")",
     "(string->charspan* \"/foo/bar/baz/bbbb\")"},
    {"(sh-path? (string->charspan* \"../a//b/\"))", "#t"},
    {"(sh-path? (string->charspan* \"\\x0;\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"../a//b/\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"a//b\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"a/b/\"))", "#t"},
    /* ------------------------- shell aliases ------------------------------ */
    {"(begin\n"
     "  (sh-alias-set! \"test-alias-foo\" '(\"bar\" \"baz\"))\n"
     "  (sh-aliases-expand '(\"test-alias-foo\" \"123\" \"456\")))\n",
     "(bar baz 123 456)"},
    /* ------------------------- shell job --------------------------------- */
    {"(begin\n"
     "  (sh-env-set! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env-ref   #t \"foo\")\n"
     "    (values->list (sh-env-visibility-ref #t \"foo\"))))",
     "(bar bar private)"},
    {"(let ((j (sh-subshell (sh-cmd \"sleep\" \"1\") '\\x3B; (sh-cmd \"echo\" \"done\"))))\n"
     "  (let-values (((port get-string) (open-string-output-port)))\n"
     "    (sh-job-display j port)\n"
     "    (newline          port)\n"
     "    (sh-job-write   j port)\n"
     "    (get-string)))\n",
     "[sleep 1 ; echo done]\n"
     "(sh-subshell (sh-cmd \"sleep\" \"1\") '\\x3B; (sh-cmd \"echo\" \"done\"))"},
    {"(let ((j (sh-and (sh-or (sh-subshell (sh-cmd \"sleep\" \"1\")) (sh-cmd \"ls\"))\n"
     "                 (sh-cmd \"cd\" \"..\"))))\n"
     "  (let-values (((port get-string) (open-string-output-port)))\n"
     "    (sh-job-display j port)\n"
     "    (newline          port)\n"
     "    (sh-job-write   j port)\n"
     "    (get-string)))\n",
     "{{[sleep 1] || ls} && cd ..}\n"
     "(sh-and (sh-or (sh-subshell (sh-cmd \"sleep\" \"1\")) (sh-cmd \"ls\"))"
     " (sh-cmd \"cd\" \"..\"))"},
    {"(sh-cmd  \"echo\"  \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
    {"(sh-cmd* \"ls\" (lambda (j) \".\"))", "(sh-cmd* \"ls\" #<procedure>)"},
    {"(sh-cmd* \"A\" '= \"B\" \"echo\")", "(sh-cmd* \"A\" '= \"B\" \"echo\")"},
    {"(sh-find-job 0)", "#f"},
    {"(sh-find-job 1)", "#f"},
    {"(sh-find-job #t)", "(#<global> #t)"},
    {"(sh-run/i (sh-cmd \"true\"))", ""}, /* (void) is displayed as empty string */
    {"(sh-run   (sh-cmd \"false\"))", "(exited . 1)"},
    {"(sh-run   (sh-cmd \"error\" \"0\"))", ""},
    {"(sh-run   (sh-cmd \"error\" \"257\"))", "(exited . 257)"},
    {"(sh-run/i (sh-list (sh-cmd \"false\") (sh-cmd \"true\")))\n", ""},
    {"(sh-run   (sh-list (sh-cmd \"true\") (sh-cmd \"false\")))\n", "(exited . 1)"},
    {"(sh-run/i (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run   (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run/i (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
    {"(sh-run   (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
    {"(sh-run   (sh-or  (sh-cmd \"false\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run/i (sh-not (sh-cmd \"true\")))", "(exited . 1)"},
    {"(sh-run   (sh-not (sh-cmd \"true\")))", "(exited . 1)"},
    {"(sh-run/i (sh-not (sh-cmd \"false\")))", ""},
    {"(sh-run   (sh-not (sh-cmd \"false\")))", ""},
    {"(let ((j (sh-and (sh-cmd \"true\") (sh-cmd \"command\" \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-bg j)\n"
     "  (sh-wait j))\n",
     "(exited . 1)"},
    {"(let ((j (sh-pipe* (sh-cmd \"true\") '\\x7C;& (sh-cmd \"command\" \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-bg j)\n"
     "  (sh-wait j))\n",
     "(exited . 1)"},
    /* (sh-start) of a builtin, or a multijob containing (recursively) only builtins,
     * directly returns their exit status, as (sh-run) would do.
     * Reason: there is no external process started asynchronously in the background */
    {"(sh-start (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))\n", /* */
     "(exited . 1)"},
    {"(let ((j (sh-cmd \"sleep\" \"1\")))\n"
     "  (sh-start j)\n"
     "  (sh-bg j))\n",
     "(running . 1)"},
    {"(sh-run (sh-subshell (sh-cmd \"true\") '\\x3B; (sh-cmd \"false\")))\n", /* */
     "(exited . 1)"},
    /* ------------------------- shell syntax ------------------------------- */
    {"(sh-parse-datum '(shell \"wc\" \"-l\" \"myfile\" > \"mylog\" \\x3B; \"echo\" \"done\"))",
     "(sh-list (sh-cmd* wc -l myfile 1 '> mylog) '; (sh-cmd echo done))"},
    {"(sh-parse-datum '(shell \"find\" \"-type\" \"f\" \\x7C;& \"wc\" &))",
     "(sh-list (sh-pipe* (sh-cmd find -type f) '|& (sh-cmd wc)) '&)"},
    /* (sh-parse) does not alter nested (shell "foo") and returns it verbatim */
    {"(sh-parse-datum '(shell (shell \"foo\") \\x3B; \"bar\"))",
     "(sh-list (shell foo) '; (sh-cmd bar))"},
    {"(sh-parse-datum '(shell ! \"foo\" && \"bar\"))",
     "(sh-and (sh-not (sh-cmd foo)) (sh-cmd bar))"},
    /* double negation is optimized away */
    {"(sh-parse-datum '(shell ! ! \"true\"))", "(sh-cmd true)"},
    {"(sh-parse-datum '(shell ! ! ! \"false\"))", "(sh-not (sh-cmd false))"},
    {"(sh-parse-datum '(shell-subshell \"abc\" && \"def\"))",
     "(sh-subshell (sh-and (sh-cmd abc) (sh-cmd def)))"},

#define INVOKELIB_SHELL_JOBS                                                                       \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell job) '(0 7 2) 'job)"

    /* ------------------------- shell macros ------------------------------- */
    {"(expand '(shell))", /* */
     INVOKELIB_SHELL_JOBS " (sh-cmd))"},
    {"(expand '(shell 2 >& 1))", /* */
     INVOKELIB_SHELL_JOBS " (sh-cmd* 2 '>& 1))"},
    {"(expand '(shell \"ls\" \"-l\" && \"wc\" \"-b\" \\x7C;\\x7C; \"echo\" \"error\" &))",
     INVOKELIB_SHELL_JOBS
     " (sh-list (sh-or (sh-and (sh-cmd ls -l) (sh-cmd wc -b)) (sh-cmd echo error)) '&))"},
    {"(expand '(shell \"true\" \\x7C;\\x7C; ! \"false\"))",
     INVOKELIB_SHELL_JOBS " (sh-or (sh-cmd true) (sh-not (sh-cmd false))))"},
    {"(expand '(shell-list (shell \"ls\" \"-al\" >> \"log.txt\")))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* ls -al 1 '>> log.txt))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"{{{{echo|cat}}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-cmd cat)))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"{echo|{cat;{true}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-list (sh-cmd cat) '; (sh-cmd true))))"},
    {"(expand '(shell (shell \"ls\" & \"echo\")))",
     INVOKELIB_SHELL_JOBS " (sh-list (sh-cmd ls) '& (sh-cmd echo)))"},
    {"(expand '(shell (shell \"foo\") \\x3B; \"bar\"))",
     INVOKELIB_SHELL_JOBS " (sh-list (sh-cmd foo) '; (sh-cmd bar)))"},
    {"(expand '(shell (shell \"ls\" & \"echo\") 2 >& 1))",
     INVOKELIB_SHELL_JOBS " (sh-redirect! (sh-list (sh-cmd ls) '& (sh-cmd echo)) 2 '>& 1))"},
    {"(shell \\x3B; (shell \"foo\") \\x3B; \"bar\")",
     "(sh-list '\\x3B; (sh-cmd \"foo\") '\\x3B; (sh-cmd \"bar\"))"},
    {"(shell (shell \"ls\" & \"echo\") 2 >& 1)",
     "(sh-redirect! (sh-list (sh-cmd \"ls\") '& (sh-cmd \"echo\")) 2 '>& 1)"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{{foo};bar}\"))",
     "(shell (shell foo) ; bar)"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"A=B ls\")))",
     "(shell A = B ls)"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{C=D echo}\")))",
     "(shell C = D echo)"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"{A=B ls}\")))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* A '= B ls))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{FOO=$BAR/subdir echo}\")))",
     "(shell FOO = (shell-wildcard (shell-env BAR) /subdir) echo)"},
    {"(expand '(shell-wildcard *))", INVOKELIB_SHELL_JOBS " (lambda (job) (sh-wildcard job '*)))"},
    {"(expand '(shell-wildcard ?))", INVOKELIB_SHELL_JOBS " (lambda (job) (sh-wildcard job '?)))"},
    {"(expand '(shell-wildcard ~))", INVOKELIB_SHELL_JOBS " (lambda (job) (sh-wildcard job '~)))"},
    {"(expand '(shell-wildcard \"a\" (shell-wildcard ~ \"b/\" *)"
     " ? % \"def\" %! \"ghi\"))", /* */
     INVOKELIB_SHELL_JOBS " (lambda (job) (sh-wildcard job a '~ b/ '* '? '% def '%! ghi)))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"{FOO=$BAR/subdir echo}\"))))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* FOO '= (lambda (job) (sh-wildcard job"
                          " (lambda (job) (sh-env-ref job BAR)) /subdir)) echo))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"A=$(echo abc; echo def)\"))",
     "(shell A = (shell-backquote echo abc ; echo def))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"A=`echo abc; echo def`\"))",
     "(shell A = (shell-backquote echo abc ; echo def))"},
    /* should rather expand to (sh-env-set/lazy! ...) ? */
    {"(expand '(shell \"A\" = (shell-backquote \"echo\" \"abc\" \\x3B; \"echo\" \"def\")))",
     INVOKELIB_SHELL_JOBS
     " (sh-cmd* A '= (lambda ()"
     " (sh-run/string-rtrim-newlines (sh-list (sh-cmd echo abc) '; (sh-cmd echo def))))))"},
    {"(expand '(shell (shell-wildcard \"l\" \"s\")))", INVOKELIB_SHELL_JOBS " (sh-cmd* ls))"},
    {"(expand '(shell (shell-wildcard \"l\" \"s\") \".\"))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* ls .))"},
    {"(expand '(shell (shell-backquote \"echo\" \"ls\")))",
     INVOKELIB_SHELL_JOBS
     " (sh-cmd* (lambda () (sh-run/string-rtrim-newlines (sh-cmd echo ls)))))"},
    /* test wildcards and patterns [...] */
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{echo *}\")))",
     "(shell echo (shell-wildcard *))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{echo .*[a-z]?.so}\")))",
     "(shell echo (shell-wildcard . * % a-z ? .so))"},
    {"(format #f \"~s\" (parse-shell-form1 (string->parsectx\n"
     "  \"A=* B=~ ls ~bar\"))))",
     "(shell \"A\" = \"*\" \"B\" = (shell-wildcard ~) \"ls\" (shell-wildcard ~ \"bar\"))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"echo ab'c'\\\"d\\\"*?[a-z]\"))",
     "(shell echo (shell-wildcard ab c d (shell-wildcard * ? % a-z)))"},
    /* in shell syntax, = is an operator only before command name */
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"ls A=B\")))",
     "(shell ls A=B)"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"{ls A=B}\")))",
     "(shell ls A=B)"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"ls A=B\"))))",
     INVOKELIB_SHELL_JOBS " (sh-cmd ls A=B))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"echo [ab]* ? [!z]\"))))",
     "(shell echo (shell-wildcard % ab *) (shell-wildcard ?) (shell-wildcard %! z))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"ls [ab]*\"))))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* ls (lambda (job) (sh-wildcard job '% ab '*))))"},
    {"(parse-shell-form1 (string->parsectx\n"
     "  \"echo $(foo&&bar)\"))",
     "(shell echo (shell-backquote foo && bar))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"echo $(foo&&bar)\")))",
     INVOKELIB_SHELL_JOBS " (sh-cmd* echo (lambda () (sh-run/string-rtrim-newlines"
                          " (sh-and (sh-cmd foo) (sh-cmd bar))))))"},
    {"(expand (parse-shell-form1 (string->parsectx\n"
     "  \"{ls} > log.txt &\")))",
     INVOKELIB_SHELL_JOBS " (sh-list* (sh-cmd ls) 1 '> log.txt '&))"},
    {"(sh-eval (parse-shell-form1 (string->parsectx\n"
     "  \"{ls} > log.txt &\")))",
     "(sh-list (sh-cmd* \"ls\" 1 '> \"log.txt\") '&)"},
    {"(expand '(shell \"echo\" \"abc\" > \"DEL_ME\" &&"
     " \"cat\" \"DEL_ME\" && \"rm\" \"DEL_ME\"))",
     INVOKELIB_SHELL_JOBS
     " (sh-and (sh-cmd* echo abc 1 '> DEL_ME) (sh-cmd cat DEL_ME) (sh-cmd rm DEL_ME)))"},
    {"(shell \"echo\" \"abc\" > \"DEL_ME\" && \"cat\" \"DEL_ME\" && \"rm\" \"DEL_ME\")",
     "(sh-and (sh-cmd* \"echo\" \"abc\" 1 '> \"DEL_ME\")"
     " (sh-cmd \"cat\" \"DEL_ME\") (sh-cmd \"rm\" \"DEL_ME\"))"},
    /* ------------------------- wildcard expansion ------------------------- */
    {"(sh-wildcard #t \"a\" \"bcd\" \"\" \"ef\")", "abcdef"},
    {"(sh-wildcard->sh-patterns '(*))", "(span (sh-pattern '*))"},
    {"(sh-wildcard->sh-patterns '(\"/\" * \".so\"))", "(span / (sh-pattern '* .so))"},
    {"(sh-wildcard->sh-patterns '(\"//abc//\" \"//def//\"))", "(span / abc/ def/)"},
    {"(sh-wildcard->sh-patterns '(\"/foo/\" * \"/\" \"/bar\"))",
     "(span / foo/ (sh-pattern '* /) bar)"},
    {"(sh-wildcard #t '* \"/\" '* \".c\")",
     "(containers/containers.c posix/posix.c shell/shell.c)"},
    {"(sh-wildcard #t \"Makefile\")", "(Makefile)"}, /* file exists => returned ad list */
    {"(sh-wildcard #t \"_does_not_exist_\")", /* file does not exists => returned as string */
     "_does_not_exist_"},
    /* ------------------------- job execution ------------------------------ */
    {"(sh-run (shell \"true\" \\x7C; \"command\" \"true\" \\x7C; \"false\"))", "(exited . 1)"},
    {"(sh-run/string (shell \"echo\" \"a\"  \"b\" \"c\"))", "a b c\n"},
    {"(sh-run/string-rtrim-newlines (shell \"echo\" \" abc \"))", " abc "},
    {"(sh-run/string (shell \"FOO\" = \"abc\" \\x3B; \"echo\" (shell-env \"FOO\")))", "abc\n"},
    /* also test that overwriting existing environment variables works */
    {"(sh-run/string (shell\n"
     "    \"FOO\" = (shell-backquote \"echo\" \"abc\") \\x3B;\n"
     "    \"echo\" (shell-env \"FOO\")))\n",
     "abc\n"},
    {"(sh-run (shell \"echo\" \"abc\" > \"DEL_ME\""
     " && \"cat\" \"DEL_ME\" > \"/dev/null\""
     " && \"rm\" \"DEL_ME\"))",
     ""},
    {"(sh-run/string (shell"
     "    \"echo\" \"a\" \"b\" \"c\" > \"DEL_ME\""
     " && \"cat\" \"DEL_ME\""
     " && \"rm\" \"DEL_ME\""
     " && \"echo\" \"ok\""
     " \\x7C;\\x7C; \"echo\" \"error\"))",
     "a b c\nok\n"},
    {"(sh-run/string (shell \"echo\" \"foo  bar\\n asdf\" \\x7C; \"grep\" \"asd\" \\x3B; \"echo\" "
     "\"ok\"))",
     " asdf\nok\n"},
    {"(sh-run (shell \"echo\" \"xyz\" \\x7C;"
     " (shell \"command\" \"true\" && \"grep\" \"abc\" > \"/dev/null\")))",
     "(exited . 1)"},
    {"(format #f \"~s\" (sh-run/string (shell \"echo0\" \"def\" \"gh\" \"i\" \"\")))",
     "\"def\\x0;gh\\x0;i\\x0;\\x0;\""},
    {"(format #f \"~s\" (sh-run/string (shell \"split-at-0\" \"echo\" (shell-backquote \"echo0\" "
     "\"jkl\" \"mn\" \"o\" "
     "\"\"))))",
     "\"jkl mn o \\n\""},
    /* ------------------------- sh-read ------------------------------------ */
    {"(sh-read-string* \"#!/some/path some-arg\\n(display (+ 1 2)) {ls}\""
     "  'scheme #t)",
     "(begin (display (+ 1 2)) (sh-run (shell ls)))"},
    {"(sh-read-string* \"#!/some/other/path\\n(display (* 3 4)); ls\""
     "  'shell #t)",
     "(begin (display (* 3 4)) (sh-run (shell ls)))"},
    {"(sh-read-file \"utils/test_file.ss\")",
     "(begin (define (fib n)"
     " (let %fib ((i n))"
     " (if (fx>? i 2) (fx+ (%fib (fx1- i)) (%fib (fx- i 2))) 1))) "
     "(sh-run (shell ; FOO = bar ;)))"},
    {"(sh-read-file \"utils/test_file.sh\")",
     "(begin (sh-run (shell ; ;"
     " BAR =  ; foo a b c | bar (shell-env BAR)"
     " && (shell echo (shell-backquote baz --quiet) < /dev/null 2 >& 1 || fail --verbose) ; ;)) "
     "(set! a 42))"},
    /* ------------------------- repl --------------------------------------- */
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"(+ 2 3) (values 7 (cons 'a 'b))\" (parsers))\n"
     "  'scheme))\n",
     "(((+ 2 3) (values 7 (cons 'a 'b))) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"ls -l | wc -b && echo ok || echo error &\" (parsers))\n"
     "  'shell))\n",
     "(((shell ls -l | wc -b && echo ok || echo error &)) #<parser shell>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"(values '{})\" (parsers))\n"
     "  'scheme))\n",
     "(((values '(shell))) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"(values '{ls; #!scheme 1 2 3})\" (parsers))\n"
     "  'scheme))\n",
     /* ugly result, and not very useful */
     "(((values '(shell ls ; 1 2 3))) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"(1 2 3)\" (parsers))\n"
     "  'scheme))\n",
     "(((1 2 3)) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"#!scheme 1 2 3\" (parsers))\n"
     "  'shell))\n",
     "((1 2 3) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"1 2 3\" (parsers))\n"
     "  'shell))\n",
     "(((shell 1 2 3)) #<parser shell>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"{#!scheme 1 2 3}\" (parsers))\n"
     "  'scheme))\n",
     /* must return the same as previous test */
     "(((1 2 3)) #<parser scheme>)"},
    {"(values->list (sh-repl-parse\n"
     "  (string->parsectx \"{#!scheme 1 2 3}\" (parsers))\n"
     "  'shell))\n",
     /* ideally would return the same as previous test, but deciding to omit the (shell ...) wrapper
        is tricky */
     "(((shell (1 2 3))) #<parser shell>)"},
};

static int run_tests(void) {
  unsigned long run_n = sizeof(tests) / sizeof(tests[0]);
  unsigned long i;
  unsigned long failed_n = 0;

  for (i = 0; i < run_n; i++) {
    errno = 0;
    failed_n += run_test(&tests[i]);
  }
  {
    run_n++;
    failed_n += run_tests_utf8b();
  }
  if (failed_n == 0) {
    fprintf(stdout, "all %lu tests passed\n", run_n);
    return 0;
  } else {
    fprintf(stdout, "%lu tests failed out of %lu\n", failed_n, run_n);
    return 1;
  }
}

static unsigned run_test(const testcase* test) {
  /* fprintf(stdout, "test: %s\n", test->string_to_eval); */

  bytes actual   = schemesh_eval_to_bytevector(test->string_to_eval);
  bytes expected = {strlen(test->expected_result), (const unsigned char*)test->expected_result};
  if (actual.size == expected.size && memcmp(actual.data, expected.data, actual.size) == 0) {
    return 0;
  }
  fprintf(stdout,
          "test failed:\n"
          "    Scheme code  %s\n"
          "    evaluated to %.*s\n"
          "    expecting    %s\n",
          test->string_to_eval,
          (int)actual.size,
          (const char*)actual.data,
          test->expected_result);
  exit(1);
  return 1;
}

#define MAX_CODEPOINT 0x10FFFF

static unsigned run_tests_utf8b(void) {
  const unsigned maxlen = 1024;
  unsigned       first_codepoint;
  ptr            string = Smake_string(maxlen, 0);
  for (first_codepoint = 0; first_codepoint <= MAX_CODEPOINT; first_codepoint += maxlen) {
    if (run_test_utf8b(string, first_codepoint) != 0) {
      return 1;
    }
  }
  return 0;
}

static unsigned run_test_utf8b(ptr string, unsigned first_codepoint) {
  const unsigned maxlen = Sstring_length(string);
  unsigned       pos;
  unsigned       codepoint = first_codepoint;
  for (pos = 0; pos < maxlen; ++pos) {
    codepoint = adjust_codepoint(codepoint);
    Sstring_set(string, pos, codepoint++);
  }
  ptr bvec    = schemesh_call1("string->utf8b", string);
  ptr string2 = schemesh_call1("utf8b->string", bvec);

  codepoint = first_codepoint;
  for (pos = 0; pos < maxlen; ++pos) {
    const unsigned codepoint2 = Sstring_ref(string2, pos);
    codepoint                 = adjust_codepoint(codepoint);
    if (codepoint != codepoint2) {
      fprintf(stdout,
              "test failed:\n"
              "    (utf8b->string (string->utf8b ...)) \n"
              "    evaluated to U+%04X\n"
              "    expecting    U+%04X\n",
              codepoint2,
              codepoint);
      return 1;
    }
    codepoint++;
  }
  return 0;
}

static unsigned adjust_codepoint(unsigned codepoint) {
  if (codepoint >= 0xD800 && codepoint < 0xDC80) {
    codepoint = 0xDC80;
  } else if (codepoint >= 0xDD00 && codepoint < 0xE000) {
    codepoint = 0xE000;
  } else if (codepoint >= 0x110000) {
    codepoint = 0;
  }
  return codepoint;
}

static void handle_scheme_exception(void) {
  fputs("schemesh_test failed: exception evaluating Scheme code!\n", stdout);
  exit(1);
}

/**
 * compile libschemesh_VERSION.so from sources found in specified directory.
 *
 * return 0 if successful, otherwise error code.
 */
static int compile_libraries(const char* source_dir) {
  ptr ret;
  int err;
  if (source_dir == NULL) {
    fprintf(stderr, "%s", "schemesh: --compile-source-dir argument is null\n");
    return EINVAL;
  }
  if (chdir(source_dir) != 0) {
    err = errno;
    fprintf(stderr,
            "schemesh: C function chdir(\"%s\") failed with error %d: %s\n",
            source_dir,
            err,
            strerror(err));
    return err;
  }
#ifdef SCHEMESH_OPTIMIZE
  ret =
      schemesh_eval("(parameterize ((optimize-level 2))\n"
                    "  (compile-file \"libschemesh.ss\" \"libschemesh_temp.so\")\n"
                    "  (strip-fasl-file \"libschemesh_temp.so\" \"" LIBSCHEMESH_SO "\"\n"
                    "    (fasl-strip-options inspector-source source-annotations profile-source))\n"
                    "    #t\n)");
#else /* !SCHEMESH_OPTIMIZE */
  ret = schemesh_eval("(parameterize ((optimize-level 0)\n"
                      "               (run-cp0 (lambda (cp0 x) x)))\n"
                      "  (compile-file \"libschemesh.ss\" \"" LIBSCHEMESH_SO "\")\n"
                      "  #t)");
#endif
  return ret == Strue ? 0 : EINVAL;
}

int main(int argc, const char* argv[]) {
  int err;
  (void)argc;
  (void)argv;

  schemesh_init(NULL, &handle_scheme_exception);
  if ((err = schemesh_register_c_functions()) != 0) {
    goto finish;
  }
  if ((err = compile_libraries(".")) != 0) {
    goto finish;
  }
  if ((err = schemesh_load_libraries(".")) != 0) {
    goto finish;
  }

  schemesh_import_all_libraries();

  err = run_tests();

finish:
  schemesh_quit();

  return err;
}
