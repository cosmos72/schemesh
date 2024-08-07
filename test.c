/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"
#include "shell/shell.h"

#include <errno.h>
#include <scheme.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    /* ----------------- containers/misc ------------------------------------ */
    {"(subvector '#(aa bb cc dd) 1 3)", "#(bb cc)"},
    {"(subbytevector #vu8(44 55 66 77) 2 3)", "B"},
    {"(bytevector-compare #vu8(44 55) #vu8(44 55))", "0"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 77 0))", "-1"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 78))", "-1"},
    {"(bytevector-compare #vu8(79) #vu8(78 0))", "1"},
    {"(do ((i #x-10000 (fx1+ i)))\n"
     "    ((fx>=? i #x120000))\n"
     "  (if (or (fx<=? #x0000 i #xD7FF)\n"
     "          (fx<=? #xDC80 i #xDCFF)\n"
     "          (fx<=? #xE000 i #x10FFFF))\n"
     "    (assert* 'test (fx=? i (char->integer (integer->char* i))))\n"
     "    (unless (throws? (integer->char* i))\n"
     "      (error 'integer->char* \"should throw\" i))))",
     ""},
    /* ----------------- bytevector/utf8 ------------------------------------ */
    {"(values->list (bytevector-ref/utf8b #vu8() 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytevector-ref/utf8b #vu8(1) 0 1))", "(\x01 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(33) 0 1))", "(! 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(#x7e) 0 1))", "(~ 1)"},
    {"(values->list (bytevector-ref/utf8b #vu8(#x7f) 0 1))", "(\x7f 1)"},
    /* UTF-8b roundtrip #x80 -> U+DC80 -> #x80 */
    {"(first-value  (bytevector-ref/utf8b #vu8(#x80) 0 1))", "\x80"},
    {"(values->list (bytevector-ref/utf8b #vu8(#xc0 #x80) 0 1))", "(#t 1)"},
    /* overlong UTF-8 sequences, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xc0 #x80) 0 2))", "\xc0"},
    {"(first-value (bytevector-ref/utf8b #vu8(#xc1 #xbf) 0 2))", "\xc1"},
    /* bad UTF-8 continuation byte, only first byte undergoes UTF-8b roundtrip */
    {"(first-value (bytevector-ref/utf8b #vu8(#xc2 #x7f) 0 2))", "\xc2"},
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
     "  (char->utf8b-length (integer->char #x7f))\n"
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
    /* ----------------- bytespan-utf8 -------------------------------------- */
    {"(values->list (bytespan-ref/char (bytespan) 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytespan-ref/char (bytespan 1) 0 1))", "(\x01 1)"},
    {"(values->list (bytespan-ref/char (bytespan #x7f) 0 1))", "(\x7f 1)"},
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
    {"(charspan->utf8 (string->charspan* \"\x7c \xce\x98 \xe0\xa4\xb9 \xf0\x90\x8d\x88\"))",
     "(bytespan 124 32 206 152 32 224 164 185 32 240 144 141 136)"},
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
    {"(let ((sp (bytespan 9 10 11 12)))\n"
     "  (bytespan-find/u8 sp 0 999 (lambda (elem) (eq? 11 elem))))",
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
     "  (charspan-find sp 0 999 (lambda (elem) (eq? #\\b elem))))",
     "2"},
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
     "  (gbuffer-erase-at! gb 2 2)\n"
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
     "  (chargbuffer-erase-at! gb 2 2)\n"
     "  (chargbuffer-insert-at! gb 1 #\\x)\n"
     "  gb)",
     "(string->chargbuffer* \"axbe\")"},
    /* ------------------------ charline ------------------------------------ */
    {"(string->charline \"abc 123\")", "(string->charline* \"abc 123\")"},
    {"(string->charline* \"echo \\n\")", "(string->charline* \"echo \\n\")"},
    {"(charline-nl? (string->charline \"echo \\n\"))", "#t"},
    {"(charline-length (string->charline \"echo \\n\"))", "6"},
    {"(charline-find-left (string->charline* \"qwerty=<>\") 999\n"
     "  (lambda (ch) (char=? ch #\\=)))",
     "6"},
    {"(charline-find-right (string->charline* \"foo/bar\") -10\n"
     "  (lambda (ch) (char=? ch #\\b)))",
     "4"},
    {"(let* ((l1 (string->charline* \"foo/bar\"))\n"
     "       (l2 (charline-copy-on-write l1)))\n"
     "  (charline-erase-at! l1 3 1)\n"
     "  (charline-insert-at! l2 3 #\\~)\n"
     "  (list l1 l2))",
     "((string->charline* \"foobar\") (string->charline* \"foo~/bar\"))"},
    {"(let* ((l1 (string->charline* \"abcdefgh\"))\n"
     "       (l2 (charline-copy-on-write l1)))\n"
     "  (charline-insert-at/cbuf! l1 5 (string->charline* \"012345\") 2 3)\n"
     "  (list l1 l2))",
     "((string->charline* \"abcde234fgh\") (string->charline* \"abcdefgh\"))"},
    /* ------------------------ charlines ----------------------------------- */
    {"(charlines (string->charline* \"foo/bar\") (string->charline \"\\n\"))",
     "(strings->charlines* \"foo/bar\" \"\\n\")"},
    {"(charlines-count-left (charlines (string->charline* \"qwerty@$%\")\n"
     "                                (string->charline* \"asdf\"))\n"
     "  999 1\n"
     "  (lambda (ch) (char=? ch #\\@)))",
     "7"},
    {"(charlines-count-right (charlines (string->charline* \"IOHPR$\n\")\n"
     "                                  (string->charline* \"ORJZX\"))\n"
     "  -999 0\n"
     "  (lambda (ch) (char=? ch #\\Z)))",
     "10"},
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
     "  (vscreen-insert-at-xy/ch! screen 4 1 #\\space)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcdef\" \"012\\n\" \" \")"},
    {"(let ((screen (vscreen* 8 30 \"abcdef\" \"012\\n\")))\n"
     "  (vscreen-insert-at-xy/newline! screen 4 0)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcd\\n\" \"ef012\\n\" \"\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (vscreen-insert-at-xy/cspan! screen 4 0 (string->charspan* \"uwxyz\") 0 5)\n"
     "  screen)",
     "(vscreen* 8 30 \"abcduwxy\" \"zefgh012\" \"\\n\" \"\")"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (values->list (vscreen-count-at-xy/left screen 4 1"
     "                  (lambda (ch) (not (char=? ch #\\d))))))",
     "(4 0 8)"},
    {"(let ((screen (vscreen* 8 30 \"abcdefgh\" \"012\\n\")))\n"
     "  (values->list (vscreen-count-at-xy/right screen 4 0"
     "                  (lambda (ch) (not (char=? ch #\\newline))))))",
     "(2 1 6)"},
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
    {"(reverse*! (list))", "()"},
    {"(reverse*! (list 1))", "(1)"},
    {"(reverse*! (list 1 2))", "(2 . 1)"},
    {"(reverse*! (list 1 2 3 4 5 6))", "(6 5 4 3 2 . 1)"},
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
    {"(parse-scheme* (make-parsectx-from-string"
     "  \"(foo bar) '(a b)\"))",
     "(foo bar)"},
    {"(parse-scheme* (make-parsectx-from-string"
     "  \"(a (b c . d) . e)\"))",
     "(a (b c . d) . e)"},
    {"(parse-scheme* (make-parsectx-from-string"
     "  \"(list #| '\\\" . #| ,`@# |# |#" /* nested block comments */
     "      '#(a 1.0 2/3) #2(d) #vu8(1 2 3) #4vu8(9) #vfx(-1 0 2) #3vfx(4))\"))",
     "(list '#(a 1.0 2/3) #(d d) #vu8(1 2 3) #vu8(9 9 9 9) #vfx(-1 0 2) #vfx(4 4 4))"},
    /* ------------------------ parser shell -------------------------------- */
    {"(parse-shell (make-parsectx-from-string \"\")))", "#!eof"},
    {"(parse-shell* (make-parsectx-from-string \"{}\"))", "(shell)"},
    {"(parse-shell* (make-parsectx-from-string \"ls -l>/dev/null&\"))\n",
     "(shell ls -l > /dev/null &)"},
    /* (parse-shell*) stops after {} */
    {"(parse-shell* (make-parsectx-from-string \"{} </dev/null 2>&1\"))", "(shell)"},
    {"(parse-shell* (make-parsectx-from-string \"echo  foo  bar|wc -l\"))",
     "(shell echo foo bar | wc -l)"},
    {"(parse-shell* (make-parsectx-from-string \"echo  foo  bar|wc -l ; \"))",
     "(shell echo foo bar | wc -l ;)"},
    {"(parse-shell* (make-parsectx-from-string \"{;echo  foo  bar|wc -l; ; }\"))",
     "(shell ; echo foo bar | wc -l ; ;)"},
    {"(parse-shell* (make-parsectx-from-string \"{echo|{cat;{true}\n}&}\"))",
     "(shell echo | (shell cat ; (shell true) ;) &)"},
    {"(parse-shell* (make-parsectx-from-string \"{{foo} </dev/null 2>&1 }\"))",
     "(shell (shell foo) < /dev/null 2 >& 1)"},
    {"(parse-shell* (make-parsectx-from-string \"{{foo} ; bar}\"))", "(shell (shell foo) ; bar)"},
    {"(parse-shell* (make-parsectx-from-string \"{ls; {foo ; bar} & echo}\"))",
     "(shell ls ; (shell foo ; bar) & echo)"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{{{{echo|cat}}}}\"))",
     "(shell (shell (shell (shell echo | cat))))"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"a<>/dev/null||b>/dev/zero&&!c>&2\"))",
     "(shell a <> /dev/null || b > /dev/zero && ! c >& 2)"},
    /** test fd number [N] before redirection */
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"foo 0</dev/zero 1<>/dev/urandom 2<&- 3>>logfile 4>otherfile 5>&/dev/null\")))",
     "(shell \"foo\" 0 < \"/dev/zero\" 1 <> \"/dev/urandom\" 2 <& \"-\" 3 >> \"logfile\""
     " 4 > \"otherfile\" 5 >& \"/dev/null\")"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls \\\"-l\\\" '.'\")))",
     "(shell \"ls\" \"-l\" \".\")"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls \\\"some\\\"'file'path\")))",
     "(shell \"ls\" (shell-concat \"some\" \"file\" \"path\"))"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"{ls `cmd1 && cmd2 || cmd3 -arg3`}\"))))",
     "(shell \"ls\" (shell-backquote \"cmd1\" && \"cmd2\" \\x7C;\\x7C; \"cmd3\" \"-arg3\"))"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls $var1 \\\"$var2\\\" '$var3'\")))",
     "(shell \"ls\" (shell-env \"var1\") (shell-env \"var2\") \"$var3\")"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls ${v 1} \\\"${ v 2 }\\\" '${ v 3 }'\")))",
     "(shell \"ls\" (shell-env \"v 1\") (shell-env \" v 2 \") \"${ v 3 }\")"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls \\\"$var1\\\"'$var2'$var3\")))",
     "(shell \"ls\" (shell-concat (shell-env \"var1\") \"$var2\" (shell-env \"var3\")))"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls $(cmd arg $var)\")))",
     "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls \\\"$(cmd arg $var)\\\"\")))",
     "(shell \"ls\" (shell-backquote \"cmd\" \"arg\" (shell-env \"var\")))"},
    {"(format #f \"~s\" (parse-shell* (make-parsectx-from-string\n"
     "  \"ls '$(cmd arg $var)'\")))",
     "(shell \"ls\" \"$(cmd arg $var)\")"},
    /* ------------------------ parse-forms --------------------------------- */
    {"(values->list (parse-forms\n"
     "  (make-parsectx-from-string \"\" (parsers))\n"
     "  'scheme))",
     "(() #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     /* #!eof is equivalent to end-of-file in the input port */
     "  (make-parsectx-from-string \"'(a . b) c #!eof . ) syntax error\" (parsers))\n"
     "  'scheme))",
     "(('(a . b) c) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (make-parsectx-from-string \"uiop asdf #!scheme (xyz %%a)\" (parsers))\n"
     "  'scheme))",
     "((uiop asdf (xyz %%a)) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (make-parsectx-from-string \"`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)\" (parsers))\n"
     "  'scheme))",
     "((`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (make-parsectx-from-string \"foo && bar || baz &\" (parsers))\n"
     "  'shell))",
     "(((shell foo && bar || baz &)) #<parser shell>)"},
    /* character { switches to shell parser */
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"{ls -l >& log.txt}\" (parsers))\n"
     "  'scheme))",
     "(shell ls -l >& log.txt)"},
    /* directive #!shell switches to shell parser also inside (...) */
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"(#!shell ls -al >> log.txt)\" (parsers))\n"
     "  'scheme))",
     "(shell ls -al >> log.txt)"},
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"(foo << bar #!shell baz >> log.txt; wc -l log.txt)\""
     " (parsers))\n"
     "  'scheme))",
     "(foo << bar (shell baz >> log.txt ;) (shell wc -l log.txt))"},
    /* ( inside shell syntax switches to Scheme parser for a single Scheme form,
     * then continues parsing shell syntax */
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"(+ 1 2)\" (parsers))\n"
     "  'shell)",
     "(+ 1 2)"},
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"{foo; bar}\" (parsers))\n"
     "  'shell)",
     "(shell foo ; bar)"},
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"[foo; bar]\" (parsers))\n"
     "  'shell)",
     "(shell-subshell foo ; bar)"},
    /* ( inside shell syntax switches to Scheme parser for a single Scheme form,
     * then continues parsing shell syntax */
    {"(values->list (parse-forms\n"
     "  (make-parsectx-from-string \"ls (apply + a `(,@b)) &\" (parsers))\n"
     "  'shell)))",
     "(((shell ls (apply + a `(,@b)) &)) #<parser shell>)"},
    /* ( at the beginning of a shell command switches to Scheme parser,
     * parses a single Scheme form, and omits the initial (shell ...) */
    {"(parse-shell\n"
     "  (make-parsectx-from-string \"(+ 1 2) not_parsed_yet\" (parsers)))",
     "(+ 1 2)"},
    /* idem */
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"(+ 1 2) not_parsed_yet\" (parsers))\n"
     "  'shell))",
     "(+ 1 2)"},
    {"(parse-form*\n"
     "  (make-parsectx-from-string \"ls (my-dir) >> log.txt\" (parsers))\n"
     "  'shell))",
     "(shell ls (my-dir) >> log.txt)"},
    {"(values->list (parse-forms\n" /* directive #!scheme switches to Scheme parser too */
     "  (make-parsectx-from-string \"ls ~; #!scheme (f a b)\" (parsers))\n"
     "  'shell))",
     "(((shell ls ~ ;) (f a b)) #<parser scheme>)"},
    {"(values->list (parse-forms\n" /* directive #!shell switches to shell parser */
     "  (make-parsectx-from-string \"(+ a b) #!shell ls -al >> log.txt; #!scheme foo bar\""
     "    (parsers))\n"
     "  'scheme))",
     "(((+ a b) (shell ls -al >> log.txt ;) foo bar) #<parser scheme>)"},
    /* ------------------------ parse-paren -------------------------------- */
    {"(parse-paren-from-string \"(foo \\\"a()\\\" \\\"b[]\\\" \\\"c{}\\\" [* |2| 3])\")",
     "#<paren _(\"\" \"\" \"\" [||])_>"},
    {"(parse-paren-from-string \"#\\newline #\\\\( #\\\\) #\\\\[ #\\\\] #\\\\{ #\\\\} #\\\\#\")",
     "#<paren __>"},
    {"(parse-paren-from-string \"#| comment . , \\\\ |#\")", "#<paren _##_>"},
    /* [] are grouping tokens in shell syntax, and `` are not special in lisp syntax */
    {"(parse-paren-from-string \"{[(``)]}\")", "#<paren _{[()]}_>"},
    /* [] are grouping tokens in lisp syntax and `` are grouping tokens in shell syntax */
    {"(parse-paren-from-string \"([{``}])\")", "#<paren _([{``}])_>"},
    /* test $( shell syntax )*/
    {"(parse-paren-from-string \"{$(`{}()`)}\")", "#<paren _{(`{} ()`)}_>"},
    /* test single-quoted strings in shell syntax */
    {"(parse-paren-from-string \"{'foo\\\"bar{}[]()``baz'}\")", "#<paren _{''}_>"},
    /* test double-quoted strings in shell syntax */
    {"(parse-paren-from-string \"{\\\"foobar{}[]``${baz}\\\"}\")", "#<paren _{\"`` {}\"}_>"},
    /** paren are not special in shell syntax inside double quoted string */
    {"(parse-paren-from-string \"{\\\"()\\\"}\")", "#<paren _{\"\"}_>"},
    /** parse mismatched paren */
    {"(parse-paren-from-string \"([{)]}\")", "#<paren _([{}])_>"},
    {"(parse-paren-from-string \"(\\\" a\\\"\")", "#<paren _(\"\")_>"},
    /* -------------------------- parenmatcher -------------------------------*/
    {"(values->list (paren->values\n"
     "  (parenmatcher-find-match\n"
     "    (make-parenmatcher)\n"
     "    (make-parsectx-from-string \"([{``}] #| |# )\" (parsers))\n"
     "    'scheme\n"
     "    6 0)))",
     "(scheme [ 1 0 6 0)"},
    /* -------------------------- tty --------------------------------------- */
    {"(let ((sz (tty-size)))\n"
     "  (and (pair? sz)\n"
     "       (integer? (car sz))\n"
     "       (integer? (cdr sz))\n"
     "       (positive? (car sz))\n"
     "       (positive? (cdr sz))))",
     "#t"},
    /* ------------------------- posix -------------------------------------- */
    {"(c-errno)", "0"},
    /* ------------------------- shell paths -------------------------------- */
    {"(sh-path-absolute? (string->charspan* \"/foo\"))", "#t"},
    {"(sh-path-absolute? (string->charspan* \"bar/\"))", "#f"},
    {"(sh-path \"//usr///local////\")", "(string->charspan* \"//usr///local////\")"},
    {"(sh-subpath \"//usr///local////\")", "(string->charspan* \"/usr/local\")"},
    {"(sh-subpath \"/usr/local/\" \"/bin/\" \"../lib/scheme/\")",
     "(string->charspan* \"/usr/local/lib/scheme\")"},
    {"(sh-path? (string->charspan* \"../a//b/\"))", "#t"},
    {"(sh-path? (string->charspan* \"\\x0;\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"../a//b/\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"a//b\"))", "#f"},
    {"(sh-subpath? (string->charspan* \"a/b/\"))", "#t"},
    /* ------------------------- shell aliases ------------------------------ */
    {"(begin\n"
     "  (sh-alias-set! \"test-alias-foo\" '(\"bar\" \"baz\"))\n"
     "  (sh-alias-expand '(\"test-alias-foo\" \"123\" \"456\")))\n",
     "(bar baz 123 456)"},
    /* ------------------------- shell jobs --------------------------------- */
    {"(begin\n"
     "  (sh-env! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env       #t \"foo\")\n"
     "    (sh-env-exported? #t \"foo\")))",
     "(bar . #f)"},
    {"(let ((j (sh-and (sh-or (sh-cmd \"sleep\" \"1\") (sh-cmd \"ls\"))\n"
     "                 (sh-cmd \"cd\" \"..\"))))\n"
     "  (let-values (((port get-string) (open-string-output-port)))\n"
     "    (sh-job-display j port)\n"
     "    (newline          port)\n"
     "    (sh-job-write   j port)\n"
     "    (get-string)))\n",
     "{{sleep 1 || ls} && cd ..}\n"
     "(sh-and (sh-or (sh-cmd \"sleep\" \"1\") (sh-cmd \"ls\")) (sh-cmd \"cd\" \"..\"))"},
    {"(sh-cmd  \"echo\"  \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
    {"(sh-run/i (sh-cmd \"true\"))", ""}, /* (void) is displayed as empty string */
    {"(sh-run   (sh-cmd \"false\"))", "(exited . 1)"},
    {"(sh-run/i (sh-list (sh-cmd \"false\") (sh-cmd \"true\")))\n", ""},
    {"(sh-run   (sh-list (sh-cmd \"true\") (sh-cmd \"false\")))\n", "(exited . 1)"},
    {"(sh-run/i (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run   (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run/i (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
    {"(sh-run   (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
    {"(sh-run/i (sh-not (sh-cmd \"true\")))", "(exited . 1)"},
    {"(sh-run   (sh-not (sh-cmd \"true\")))", "(exited . 1)"},
    {"(sh-run/i (sh-not (sh-cmd \"false\")))", ""},
    {"(sh-run   (sh-not (sh-cmd \"false\")))", ""},
    {"(let ((j (sh-and (sh-cmd \"true\") (sh-cmd \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-bg j)\n"
     "  (sh-wait j))\n",
     "(exited . 1)"},
    /* ------------------------- shell syntax ------------------------------- */
    {"(sh-parse '(shell \"wc\" \"-l\" \"myfile\" > \"mylog\" \\x3b; \"echo\" \"done\"))",
     "(sh-list (sh-cmd* wc -l myfile 1 '> mylog) '; (sh-cmd echo done))"},
    {"(sh-parse '(shell \"find\" \"-type\" \"f\" \\x7c; \"wc\" &))",
     "(sh-list (sh-pipe* (sh-cmd find -type f) '| (sh-cmd wc)) '&)"},
    /* (sh-parse) does not alter nested (shell "foo") and returns it verbatim */
    {"(sh-parse '(shell (shell \"foo\") \\x3b; \"bar\"))", "(sh-list (shell foo) '; (sh-cmd bar))"},
    {"(sh-parse '(shell ! \"foo\" && \"bar\"))", "(sh-and (sh-not (sh-cmd foo)) (sh-cmd bar))"},
    /* double negation is optimized away */
    {"(sh-parse '(shell ! ! \"true\"))", "(sh-cmd true)"},
    {"(sh-parse '(shell ! ! ! \"false\"))", "(sh-not (sh-cmd false))"},
    {"(sh-parse '(shell-subshell \"abc\" && \"def\"))",
     "(sh-subshell (sh-and (sh-cmd abc) (sh-cmd def)))"},

#define INVOKELIB_SHELL_BUILTINS                                                                   \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell builtins) '(0 1) 'builtins)"
#define INVOKELIB_SHELL_JOBS                                                                       \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell jobs) '(0 1) 'jobs)"
#define INVOKELIB_SHELL_PARSE                                                                      \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell parse) '(0 1) 'parse)"
#define INVOKELIB_SHELL_BUILTINS_JOBS                                                              \
  "(begin"                                                                                         \
  " (($primitive 3 $invoke-library) '(schemesh shell builtins) '(0 1) 'builtins)"                  \
  " (($primitive 3 $invoke-library) '(schemesh shell jobs) '(0 1) 'jobs)"
#define INVOKELIB_SHELL_JOBS_PARSE                                                                 \
  "(begin"                                                                                         \
  " (($primitive 3 $invoke-library) '(schemesh shell jobs) '(0 1) 'jobs)"                          \
  " (($primitive 3 $invoke-library) '(schemesh shell parse) '(0 1) 'parse)"

    /* ------------------------- shell macros ------------------------------- */
    {"(expand '(shell))", INVOKELIB_SHELL_JOBS " (sh-cmd true))"},
    {"(expand '(shell \"ls\" \"-l\" && \"wc\" \"-b\" \\x7c;\\x7c; \"echo\" \"error\" &))",
     INVOKELIB_SHELL_JOBS
     " (sh-list (sh-or (sh-and (sh-cmd ls -l) (sh-cmd wc -b)) (sh-cmd echo error)) '&))"},
    {"(expand '(shell \"true\" \\x7c;\\x7c; ! \"false\"))",
     INVOKELIB_SHELL_JOBS " (sh-or (sh-cmd true) (sh-not (sh-cmd false))))"},
    {"(expand '(shell-list (shell \"ls\" \"-al\" >> \"log.txt\")))",
     INVOKELIB_SHELL_PARSE " (sh-cmd* ls -al 1 '>> log.txt))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{{{{echo|cat}}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-cmd cat)))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{echo|{cat;{true}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-list (sh-cmd cat) '; (sh-cmd true))))"},
    {"(expand '(shell (shell \"ls\" & \"echo\")))",
     INVOKELIB_SHELL_JOBS " (sh-list (sh-cmd ls) '& (sh-cmd echo)))"},
    {"(expand '(shell (shell \"foo\") \\x3b; \"bar\"))",
     INVOKELIB_SHELL_JOBS " (sh-list (sh-cmd foo) '; (sh-cmd bar)))"},
    {"(expand '(shell (shell \"ls\" & \"echo\") 2 >& 1))",
     INVOKELIB_SHELL_JOBS " (sh-redirect! (sh-list (sh-cmd ls) '& (sh-cmd echo)) 2 '>& 1))"},
    {"(shell \\x3b; (shell \"foo\") \\x3b; \"bar\")",
     "(sh-list '\\x3B; (sh-cmd \"foo\") '\\x3B; (sh-cmd \"bar\"))"},
    {"(shell (shell \"ls\" & \"echo\") 2 >& 1)",
     "(sh-redirect! (sh-list (sh-cmd \"ls\") '& (sh-cmd \"echo\")) 2 '>& 1)"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{{foo};bar}\"))",
     "(shell (shell foo) ; bar)"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{A=B ls}\")))",
     "(shell A = B ls)"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{A=B ls}\")))",
     INVOKELIB_SHELL_PARSE " (sh-cmd* A '= B ls))"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{FOO=$BAR/subdir echo}\")))",
     "(shell FOO = (shell-concat (shell-env BAR) /subdir) echo)"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{FOO=$BAR/subdir echo}\"))))",
     INVOKELIB_SHELL_JOBS_PARSE " (sh-cmd* FOO '= (lambda (job) (sh-concat job"
                                " (lambda (job) (sh-env job BAR)) /subdir)) echo))"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{ls A=B}\")))",
     "(shell ls A=B)"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{ls A=B}\"))))",
     INVOKELIB_SHELL_JOBS " (sh-cmd ls A=B))"},
    {"(parse-shell* (make-parsectx-from-string\n"
     "  \"{echo $(foo&&bar)}\"))",
     "(shell echo (shell-backquote foo && bar))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{echo $(foo&&bar)}\")))",
     INVOKELIB_SHELL_JOBS_PARSE " (sh-cmd* echo (lambda (job) (sh-run/string"
                                " (sh-and (sh-cmd foo) (sh-cmd bar))))))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{{ls} > log.txt &}\")))",
     INVOKELIB_SHELL_JOBS_PARSE " (sh-list* (sh-cmd ls) 1 '> log.txt '&))"},
    {"(eval (parse-shell* (make-parsectx-from-string\n"
     "  \"{{ls} > log.txt &}\")))",
     "(sh-list (sh-cmd* \"ls\" 1 '> \"log.txt\") '&)"},
    /* ------------------------- repl --------------------------------------- */
    {"(values->list (repl-parse\n"
     "  (make-parsectx-from-string \"(+ 2 3) (values 7 (cons 'a 'b))\" (parsers))\n"
     "  'scheme))\n",
     "(((+ 2 3) (values 7 (cons 'a 'b))) #<parser scheme>)"},
    {"(values->list (repl-parse\n"
     "  (make-parsectx-from-string \"ls -l | wc -b && echo ok || echo error &\" (parsers))\n"
     "  'shell))\n",
     "(((shell ls -l | wc -b && echo ok || echo error &)) #<parser shell>)"},
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
  bytes actual   = eval_to_bytevector(test->string_to_eval);
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
  ptr bvec    = call1("string->utf8b", string);
  ptr string2 = call1("utf8b->string", bvec);

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

int main(int argc, const char* argv[]) {
  int err;
  (void)argc;
  (void)argv;

  schemesh_init(&handle_scheme_exception);
  if ((err = schemesh_register_c_functions()) < 0) {
    return err;
  }
  schemesh_compile_and_load_libraries();
  schemesh_import_libraries();

  err = run_tests();

  schemesh_quit();

  return err;
}
