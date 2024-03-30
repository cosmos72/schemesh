/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
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

static unsigned run_test(const char string_to_eval[], const char expected_result[]);
static int      run_tests(void);

static const struct {
  const char* string_to_eval;
  const char* expected_result;
} tests[] = {
    {"(+ 1 2 3)", "6"},
    {"(* 4 5 6)", "120"},
    /* ----------------- bootstrap ------------------------------------------ */
    {"(let ((x 0))\n"
     "  (repeat 5 (set! x (fx1+ x)))\n"
     "  x)",
     "5"},
    {"(try (assert* #t) (catch (condition) 1))", "#t"},
    {"(try (assert* #f) (catch (condition) 2))", "2"},
    {"(values->list (values 1 2 3))", "(1 2 3)"},
    {"(let-macro ((plus . args) `(+ ,@args))\n"
     "  (plus 3 4 5))",
     "12"},
    {"(let-macro ((plus arg0 . args) `(+ ,arg0 ,@args))\n"
     "  (plus 3 4 5))",
     "12"},
    /* ----------------- containers/misc ------------------------------------ */
    {"(subvector '#(aa bb cc dd) 1 3)", "#(bb cc)"},
    {"(subbytevector #vu8(44 55 66 77) 2 3)", "B"},
    {"(bytevector-compare #vu8(44 55) #vu8(44 55))", "0"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 77 0))", "-1"},
    {"(bytevector-compare #vu8(66 77) #vu8(66 78))", "-1"},
    {"(bytevector-compare #vu8(79) #vu8(78 0))", "1"},
    /* ----------------- bytevector/utf8 ------------------------------------ */
    {"(values->list (bytevector-ref/utf8 #vu8() 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytevector-ref/utf8 #vu8(1) 0 1))", "(\x01 1)"},
    {"(values->list (bytevector-ref/utf8 #vu8(33) 0 1))", "(! 1)"},
    {"(values->list (bytevector-ref/utf8 #vu8(#x7e) 0 1))", "(~ 1)"},
    {"(values->list (bytevector-ref/utf8 #vu8(#x7f) 0 1))", "(\x7f 1)"},
    {"(values->list (bytevector-ref/utf8 #vu8(#x80) 0 1))", "(#f 1)"},
    {"(values->list (bytevector-ref/utf8 #vu8(#xc0 #x80) 0 1))", "(#t 1)"}, /* incomplete */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc0 #x80) 0 2))", "(#f 2)"}, /* overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc1 #xbf) 0 2))", "(#f 2)"}, /* overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc2 #x7f) 0 2))", "(#f 2)"}, /* bad continuation */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc2 #xa3) 0 2))", "(\xc2\xa3 2)"}, /* pound sign */
    {"(values->list (bytevector-ref/utf8 #vu8(#xc2 #xbf) 0 2))", "(\xc2\xbf 2)"},
    {"(values->list (bytevector-ref/utf8 #vu8(#xc2 #xc0) 0 2))", "(#f 2)"}, /* bad continuation */
    {"(values->list (bytevector-ref/utf8 #vu8(#xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xe0 #x80 #x80) 0 2))", "(#t 2)"},  /* incomplete */
    {"(values->list (bytevector-ref/utf8 #vu8(#xe0 #x80 #x80) 0 3))", "(#f 3)"},  /* overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xe0 #x9f #xbf) 0 3))", "(#f 3)"},  /* overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xed #xa0 #x80) 0 3))",
     "(#f 3)"}, /* invalid, U+D800 is surrogate half */
    {"(values->list (bytevector-ref/utf8 #vu8(#xed #xbf #xbf) 0 3))",
     "(#f 3)"}, /* invalid, U+DFFF is surrogate half */
    {"(values->list (bytevector-ref/utf8 #vu8(#xee #x80 #x80) 0 3))",
     "(\xee\x80\x80 3)"}, /* U+E000 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf0 #x80 #x80 #x80) 0 3))",
     "(#t 3)"}, /* incomplete */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf0 #x80 #x80 #x80) 0 4))",
     "(#f 4)"}, /*   overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf0 #x8f #xbf #xbf) 0 4))",
     "(#f 4)"}, /*   overlong */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf4 #x90 #x80 #x80) 0 4))",
     "(#f 4)"}, /* invalid U+110000, exceeds U+10FFFF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf4 #xbf #xbf #xbf) 0 4))",
     "(#f 4)"}, /* invalid, exceeds U+10FFFF */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf5 #x80 #x80 #x80) 0 4))",
     "(#f 1)"}, /* invalid, UTF-8 sequences cannot contain #xf5 .. #xff */
    {"(values->list (bytevector-ref/utf8 #vu8(#xf6) 0 1))", "(#f 1)"}, /* invalid #xf6 */
    {"(values->list (bytevector-ref/utf8 #vu8(#xfe) 0 1))", "(#f 1)"}, /* invalid #xfe */
    {"(values->list (bytevector-ref/utf8 #vu8(#xff) 0 1))", "(#f 1)"}, /* invalid #xff */
    {"(let ((bv (make-bytevector 1)))\n"
     "  (bytevector-set/utf8! bv 0 #\\~)\n"
     "  bv)",
     "~"},
    {"(list\n"
     "  (char->utf8-length (integer->char 0))\n"
     "  (char->utf8-length (integer->char #x7f))\n"
     "  (char->utf8-length (integer->char #x80))\n"
     "  (char->utf8-length (integer->char #x7ff))\n"
     "  (char->utf8-length (integer->char #x800))\n"
     "  (char->utf8-length (integer->char #xffff))\n"
     "  (char->utf8-length (integer->char #x10000))\n"
     "  (char->utf8-length (integer->char #x10ffff)))",
     "(1 1 2 2 3 3 4 4)"},
    {"(let ((bv (make-bytevector 1)))\n"
     "  (bytevector-set/utf8! bv 0 #\\~)\n"
     "  bv)",
     "~"},
    {"(let ((bv (make-bytevector 2)))\n"
     "  (bytevector-set/utf8! bv 0 (integer->char #xa3))\n" /* pound sign */
     "  bv)",
     "\xc2\xa3"},
    {"(let ((bv (make-bytevector 3)))\n"
     "  (bytevector-set/utf8! bv 0 (integer->char #x20ac))\n" /* euro sign */
     "  bv)",
     "\xe2\x82\xac"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-set/utf8! bv 0 (integer->char #x10348))\n"
     "  bv)",
     "\xf0\x90\x8d\x88"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-set/utf8! bv 0 (integer->char #x10ffff))\n"
     "  bv)",
     "\xf4\x8f\xbf\xbf"},
    /* ----------------- bytespan-utf8 -------------------------------------- */
    {"(values->list (bytespan-ref/utf8 (bytespan) 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytespan-ref/utf8 (bytespan 1) 0 1))", "(\x01 1)"},
    {"(values->list (bytespan-ref/utf8 (bytespan #x7f) 0 1))", "(\x7f 1)"},
    {"(values->list (bytespan-ref/utf8 (bytespan #x80) 0 1))", "(#f 1)"},
    {"(values->list (bytespan-ref/utf8 (bytespan #xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytespan-ref/utf8 (bytespan #xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytespan-ref/utf8 (bytespan #xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytespan-ref/utf8 (bytespan #xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytespan-ref/utf8 (bytespan #xed #xa0 #x80) 0 3))",
     "(#f 3)"}, /* invalid, U+D800 is surrogate half */
    {"(values->list (bytespan-ref/utf8 (bytespan #xed #xbf #xbf) 0 3))",
     "(#f 3)"}, /* invalid, U+DFFF is surrogate half */
    {"(values->list (bytespan-ref/utf8 (bytespan #xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    {"(values->list (bytespan-ref/utf8 (bytespan #xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytespan-ref/utf8 (bytespan #xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytespan-ref/utf8 (bytespan #xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/utf8! sp #\\~)\n"
     "  sp)",
     "(bytespan 126)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/utf8! sp (integer->char #xa3))\n" /* pound sign */
     "  sp)",
     "(bytespan 194 163)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-back/utf8! sp (integer->char #x20ac))\n" /* euro sign */
     "  sp)",
     "(bytespan 226 130 172)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-front/utf8! sp (integer->char #x10348))\n"
     "  sp)",
     "(bytespan 240 144 141 136)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-insert-front/utf8! sp (integer->char #x10ffff))\n"
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
    {"(string-hashtable->vector-of-bytevector0\n"
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
    /* ------------------------ parser shell -------------------------------- */
    {"(parse-shell (make-parsectx-from-string \"\")))", "#!eof"},
    {"(parse-shell* (make-parsectx-from-string \"{}\"))", "(shell)"},
    {"(parse-shell* (make-parsectx-from-string \"ls -l>/dev/null&\"))\n",
     "(shell ls -l > /dev/null &)"},
    {"(parse-shell* (make-parsectx-from-string \"echo  foo  bar|wc -l\"))",
     "(shell echo foo bar | wc -l)"},
    {"(parse-shell* (make-parsectx-from-string \"echo  foo  bar|wc -l ; \"))",
     "(shell echo foo bar | wc -l ;)"},
    {"(parse-shell* (make-parsectx-from-string \"{echo  foo  bar|wc -l; ; }\"))",
     "(shell echo foo bar | wc -l ; ;)"},
    {"(parse-shell* (make-parsectx-from-string \"{echo|{cat;{true}\n}&}\"))",
     "(shell echo | (shell cat ; (shell true) ;) &)"},
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
    /* ------------------------ parse-parens -------------------------------- */
    {"(parse-parens-from-string \"(foo \\\"a()\\\" \\\"b[]\\\" \\\"c{}\\\" [* |2| 3])\")",
     "#<parens _(\"\" \"\" \"\" [||])_>"},
    {"(parse-parens-from-string \"#\\newline #\\\\( #\\\\) #\\\\[ #\\\\] #\\\\{ #\\\\} #\\\\#\")",
     "#<parens __>"},
    {"(parse-parens-from-string \"#| comment . , \\\\ |#\")", "#<parens _##_>"},
    /* [] are grouping tokens in shell syntax, and `` are not special in lisp syntax */
    {"(parse-parens-from-string \"{[(``)]}\")", "#<parens _{[()]}_>"},
    /* [] are grouping tokens in lisp syntax and `` are grouping tokens in shell syntax */
    {"(parse-parens-from-string \"([{``}])\")", "#<parens _([{``}])_>"},
    /* test $( shell syntax )*/
    {"(parse-parens-from-string \"{$(`{}()`)}\")", "#<parens _{(`{} ()`)}_>"},
    /* test single-quoted strings in shell syntax */
    {"(parse-parens-from-string \"{'foo\\\"bar{}[]()``baz'}\")", "#<parens _{''}_>"},
    /* test double-quoted strings in shell syntax */
    {"(parse-parens-from-string \"{\\\"foobar{}[]``${baz}\\\"}\")", "#<parens _{\"`` {}\"}_>"},
    /** parens are not special in shell syntax inside double quoted string */
    {"(parse-parens-from-string \"{\\\"()\\\"}\")", "#<parens _{\"\"}_>"},
    /** parse mismatched parens */
    {"(parse-parens-from-string \"([{)]}\")", "#<parens _([{}])_>"},
    {"(parse-parens-from-string \"(\\\" a\\\"\")", "#<parens _(\"\")_>"},
    /* -------------------------- parenmatcher -------------------------------*/
    {"(values->list (parens->values\n"
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
    {"(sh-path \"//usr///local////\")", "(string->charspan* \"/usr/local\")"},
    {"(sh-path \"/usr/local/\" \"/bin/\" \"../lib/scheme/\")",
     "(string->charspan* \"/usr/local/lib/scheme\")"},
    /* ------------------------- shell jobs --------------------------------- */
    {"(begin\n"
     "  (sh-env! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env       #t \"foo\")\n"
     "    (sh-env-exported? #t \"foo\")))",
     "(bar . #f)"},
    {"(sh-cmd \"echo\" \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
    {"(sh-run/i (sh-cmd \"true\"))", ""}, /* (void) is displayed as empty string */
    {"(sh-run/i (sh-cmd \"false\"))", "(exited . 1)"},
    {"(sh-multijob 'hello (lambda (j) '(exited . 42)))", "(sh-hello)"},
    {"(sh-run/i (sh-multijob 'hello (lambda (j) '(exited . 42))))", "(exited . 42)"},
    {"(sh-run/i (sh-multijob 'hello (lambda (j) '(killed . sigsegv))))", "(killed . sigsegv)"},
    {"(let ((j (sh-list (sh-cmd \"false\") (sh-cmd \"true\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))",
     ""},
    {"(let ((j (sh-list (sh-cmd \"true\") (sh-cmd \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))",
     "(exited . 1)"},
    {"(sh-run/i (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run/i (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", ""},
    /* ------------------------- shell syntax ------------------------------- */
    {"(sh-parse '(\"wc\" \"-l\" \"myfile\" > \"mylog\" \\x3b; \"echo\" \"done\"))",
     "(sh-list (sh-cmd<> wc -l myfile '> mylog) '; (sh-cmd echo done))"},
    {"(sh-parse '(\"find\" \"-type\" \"f\" \\x7c; \"wc\" &))",
     "(sh-list (sh-pipe* (sh-cmd find -type f) '| (sh-cmd wc)) '&)"},

#define INVOKELIB_SHELL_BUILTINS                                                                   \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell builtins) '(0 1) 'builtins)"
#define INVOKELIB_SHELL_JOBS                                                                       \
  "(begin (($primitive 3 $invoke-library) '(schemesh shell jobs) '(0 1) 'jobs)"
#define INVOKELIB_SHELL_BUILTINS_JOBS                                                              \
  "(begin"                                                                                         \
  " (($primitive 3 $invoke-library) '(schemesh shell builtins) '(0 1) 'builtins)"                  \
  " (($primitive 3 $invoke-library) '(schemesh shell jobs) '(0 1) 'jobs)"

    /* ------------------------- shell macros ------------------------------- */
    {"(expand '(shell))", INVOKELIB_SHELL_BUILTINS " (sh-true))"},
    {"(expand '(shell \"ls\" \"-l\" && \"wc\" \"-b\" \\x7c;\\x7c; \"echo\" \"error\" &))",
     INVOKELIB_SHELL_JOBS
     " (sh-list (sh-or (sh-and (sh-cmd ls -l) (sh-cmd wc -b)) (sh-cmd echo error)) '&))"},
    {"(expand '(shell-list (shell \"ls\" \"-al\" >> \"log.txt\")))",
     INVOKELIB_SHELL_JOBS " (sh-cmd<> ls -al '>> log.txt))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{{{{echo|cat}}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-cmd cat)))"},
    {"(expand (parse-shell* (make-parsectx-from-string\n"
     "  \"{echo|{cat;{true}}}\")))",
     INVOKELIB_SHELL_JOBS " (sh-pipe* (sh-cmd echo) '| (sh-list (sh-cmd cat) '; (sh-cmd true))))"},
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
  const unsigned long n = sizeof(tests) / sizeof(tests[0]);
  unsigned long       i;
  unsigned long       failed_n = 0;

  for (i = 0; i < n; i++) {
    errno = 0;
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
