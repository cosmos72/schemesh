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
    {"(values->list (values 1 2 3))", "(1 2 3)"},
    {"(subvector '#(aa bb cc dd) 1 3)", "#(bb cc)"},
    {"(subbytevector '#vu8(44 55 66 77) 2 3)", "B"},
    /* ----------------- bytevector-utf8 ----------------------------- */
    {"(values->list (bytevector-utf8-ref #vu8() 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytevector-utf8-ref #vu8(1) 0 1))", "(\x01 1)"},
    {"(values->list (bytevector-utf8-ref #vu8(33) 0 1))", "(! 1)"},
    {"(values->list (bytevector-utf8-ref #vu8(#x7e) 0 1))", "(~ 1)"},
    {"(values->list (bytevector-utf8-ref #vu8(#x7f) 0 1))", "(\x7f 1)"},
    {"(values->list (bytevector-utf8-ref #vu8(#x80) 0 1))", "(#f 1)"},
    {"(values->list (bytevector-utf8-ref #vu8(#xc0 #x80) 0 1))", "(#t 1)"}, /* incomplete */
    {"(values->list (bytevector-utf8-ref #vu8(#xc0 #x80) 0 2))", "(#f 2)"}, /* overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xc1 #xbf) 0 2))", "(#f 2)"}, /* overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xc2 #x7f) 0 2))", "(#f 2)"}, /* bad continuation */
    {"(values->list (bytevector-utf8-ref #vu8(#xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytevector-utf8-ref #vu8(#xc2 #xa3) 0 2))", "(\xc2\xa3 2)"}, /* pound sign */
    {"(values->list (bytevector-utf8-ref #vu8(#xc2 #xbf) 0 2))", "(\xc2\xbf 2)"},
    {"(values->list (bytevector-utf8-ref #vu8(#xc2 #xc0) 0 2))", "(#f 2)"}, /* bad continuation */
    {"(values->list (bytevector-utf8-ref #vu8(#xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytevector-utf8-ref #vu8(#xe0 #x80 #x80) 0 2))", "(#t 2)"},  /* incomplete */
    {"(values->list (bytevector-utf8-ref #vu8(#xe0 #x80 #x80) 0 3))", "(#f 3)"},  /* overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xe0 #x9f #xbf) 0 3))", "(#f 3)"},  /* overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytevector-utf8-ref #vu8(#xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytevector-utf8-ref #vu8(#xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    {"(values->list (bytevector-utf8-ref #vu8(#xed #xa0 #x80) 0 3))",
     "(#f 3)"}, /* invalid, U+D800 is surrogate half */
    {"(values->list (bytevector-utf8-ref #vu8(#xed #xbf #xbf) 0 3))",
     "(#f 3)"}, /* invalid, U+DFFF is surrogate half */
    {"(values->list (bytevector-utf8-ref #vu8(#xee #x80 #x80) 0 3))",
     "(\xee\x80\x80 3)"}, /* U+E000 */
    {"(values->list (bytevector-utf8-ref #vu8(#xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytevector-utf8-ref #vu8(#xf0 #x80 #x80 #x80) 0 3))",
     "(#t 3)"}, /* incomplete */
    {"(values->list (bytevector-utf8-ref #vu8(#xf0 #x80 #x80 #x80) 0 4))",
     "(#f 4)"}, /*   overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xf0 #x8f #xbf #xbf) 0 4))",
     "(#f 4)"}, /*   overlong */
    {"(values->list (bytevector-utf8-ref #vu8(#xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytevector-utf8-ref #vu8(#xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    {"(values->list (bytevector-utf8-ref #vu8(#xf4 #x90 #x80 #x80) 0 4))",
     "(#f 4)"}, /* invalid U+110000, exceeds U+10FFFF */
    {"(values->list (bytevector-utf8-ref #vu8(#xf4 #xbf #xbf #xbf) 0 4))",
     "(#f 4)"}, /* invalid, exceeds U+10FFFF */
    {"(values->list (bytevector-utf8-ref #vu8(#xf5 #x80 #x80 #x80) 0 4))",
     "(#f 1)"}, /* invalid, UTF-8 sequences cannot contain #xf5 .. #xff */
    {"(values->list (bytevector-utf8-ref #vu8(#xf6) 0 1))", "(#f 1)"}, /* invalid #xf6 */
    {"(values->list (bytevector-utf8-ref #vu8(#xfe) 0 1))", "(#f 1)"}, /* invalid #xfe */
    {"(values->list (bytevector-utf8-ref #vu8(#xff) 0 1))", "(#f 1)"}, /* invalid #xff */
    {"(let ((bv (make-bytevector 1)))\n"
     "  (bytevector-utf8-set! bv 0 #\\~)\n"
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
     "  (bytevector-utf8-set! bv 0 #\\~)\n"
     "  bv)",
     "~"},
    {"(let ((bv (make-bytevector 2)))\n"
     "  (bytevector-utf8-set! bv 0 (integer->char #xa3))\n" /* pound sign */
     "  bv)",
     "\xc2\xa3"},
    {"(let ((bv (make-bytevector 3)))\n"
     "  (bytevector-utf8-set! bv 0 (integer->char #x20ac))\n" /* euro sign */
     "  bv)",
     "\xe2\x82\xac"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-utf8-set! bv 0 (integer->char #x10348))\n"
     "  bv)",
     "\xf0\x90\x8d\x88"},
    {"(let ((bv (make-bytevector 4)))\n"
     "  (bytevector-utf8-set! bv 0 (integer->char #x10ffff))\n"
     "  bv)",
     "\xf4\x8f\xbf\xbf"},
    /* ----------------- bytespan-utf8 ----------------------------- */
    {"(values->list (bytespan-utf8-ref (bytespan) 0 1))", "(#t 0)"}, /* incomplete */
    {"(values->list (bytespan-utf8-ref (bytespan 1) 0 1))", "(\x01 1)"},
    {"(values->list (bytespan-utf8-ref (bytespan #x7f) 0 1))", "(\x7f 1)"},
    {"(values->list (bytespan-utf8-ref (bytespan #x80) 0 1))", "(#f 1)"},
    {"(values->list (bytespan-utf8-ref (bytespan #xc2 #x80) 0 2))", "(\xc2\x80 2)"}, /* U+0080 */
    {"(values->list (bytespan-utf8-ref (bytespan #xdf #xbf) 0 2))", "(\xdf\xbf 2)"}, /* U+07FF */
    {"(values->list (bytespan-utf8-ref (bytespan #xe0 #xa0 #x80) 0 3))",
     "(\xe0\xa0\x80 3)"}, /* U+0800 */
    {"(values->list (bytespan-utf8-ref (bytespan #xed #x80 #x80) 0 3))",
     "(\xed\x80\x80 3)"}, /* U+D000 */
    {"(values->list (bytespan-utf8-ref (bytespan #xed #xa0 #x80) 0 3))",
     "(#f 3)"}, /* invalid, U+D800 is surrogate half */
    {"(values->list (bytespan-utf8-ref (bytespan #xed #xbf #xbf) 0 3))",
     "(#f 3)"}, /* invalid, U+DFFF is surrogate half */
    {"(values->list (bytespan-utf8-ref (bytespan #xed #x9f #xbf) 0 3))",
     "(\xed\x9f\xbf 3)"}, /* U+D7FF */
    {"(values->list (bytespan-utf8-ref (bytespan #xef #xbf #xbf) 0 3))",
     "(\xef\xbf\xbf 3)"}, /* U+FFFF */
    {"(values->list (bytespan-utf8-ref (bytespan #xf0 #x90 #x80 #x80) 0 4))",
     "(\xf0\x90\x80\x80 4)"}, /* U+10000 */
    {"(values->list (bytespan-utf8-ref (bytespan #xf4 #x8f #xbf #xbf) 0 4))",
     "(\xf4\x8f\xbf\xbf 4)"}, /* U+10FFFF */
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-utf8-insert-back! sp #\\~)\n"
     "  sp)",
     "(bytespan 126)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-utf8-insert-back! sp (integer->char #xa3))\n" /* pound sign */
     "  sp)",
     "(bytespan 194 163)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-utf8-insert-back! sp (integer->char #x20ac))\n" /* euro sign */
     "  sp)",
     "(bytespan 226 130 172)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-utf8-insert-front! sp (integer->char #x10348))\n"
     "  sp)",
     "(bytespan 240 144 141 136)"},
    {"(let ((sp (bytespan)))\n"
     "  (bytespan-utf8-insert-front! sp (integer->char #x10ffff))\n"
     "  sp)",
     "(bytespan 244 143 191 191)"},
    /* ----------------- bytespan-fixnum-display ------------------ */
    {"(let ((sp (bytespan)))\n"
     "  (list-iterate '(0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 "
     "                  9999998 10000000 12345678 -1 -9 -10 -87654321)\n"
     "    (lambda (n)\n"
     "      (bytespan-fixnum-display-back! sp n)\n"
     "      (bytespan-u8-insert-back! sp 32)))\n"
     "  (bytespan->bytevector sp))",
     "0 1 9 10 99 100 999 1000 9999 10000 99999 100000 999999 1000000 "
     "9999998 10000000 12345678 -1 -9 -10 -87654321 "},
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
     "  (span-find sp 0 999 (lambda (elem) (eq? 'c elem))))",
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
     "  (bytespan-u8-find sp 0 999 (lambda (elem) (eq? 11 elem))))",
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
    /* --------------------- chargbuffer-input-port-------------------------- */
    {"(read\n"
     "  (open-chargbuffer-input-port\n"
     "    (string->chargbuffer* \"(re8u (+ -) [* /] 'foo bar . baz)\"))))",
     "(re8u (+ -) (* /) 'foo bar . baz)"},
    /* --------------------- gbuffer-input-port-------------------------- */
    {"(read\n"
     "  (open-gbuffer-of-chargbuffers-input-port\n"
     "    (gbuffer\n"
     "      (string->chargbuffer* \"(urehg* (a . 'b) 12\")\n"
     "      (chargbuffer)\n"
     "      (string->chargbuffer* \"3.45e3 . #\\\\m)\"))))",
     "(urehg* (a quote b) 123450.0 . m)"},
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
    /* --------------------- hashtable -------------------------------------- */
    {"(hashtable-cells\n"
     "  (eq-hashtable '(3 . C) '(2 . B) '(1 . A)))",
     "#((1 . A) (2 . B) (3 . C))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C)))",
     "#((3 . C) (1.0 . A) (2.1 . B))"},
    {"(hashtable-cells\n"
     "  (eqv-hashtable '(3.1 . C) '(2 . B) '(1 . A)))",
     "#((1 . A) (2 . B) (3.1 . C))"},
    {"(hashtable-cells\n"
     "  (hashtable string-hash string=? '(\"a\" . 1) '(\"B\" . 2) '(\"+\" . 3)))",
     "#((+ . 3) (B . 2) (a . 1))"},
    {"(string-hashtable->vector-of-bytevector0\n"
     "  (hashtable string-hash string=?\n"
     "             '(\"A\" . \"X\") '(\"B\" . \"Y\") '(\"C\" . \"Z\")))",
     "#(#vu8(67 61 90 0) #vu8(66 61 89 0) #vu8(65 61 88 0))"},
    {"(let ((ret '()))\n"
     "  (hashtable-iterate (eqv-hashtable '(1.0 . A) '(2.1 . B) '(3 . C))\n"
     "    (lambda (cell)\n"
     "      (set! ret (cons cell ret))))\n"
     "  ret)",
     "((2.1 . B) (1.0 . A) (3 . C))"},
    /* ------------------------ parser scheme ------------------------------- */
    {"(parse-scheme* (open-string-input-port \"(foo bar) '(a b)\") #f)", "(foo bar)"},
    {"(parse-scheme* (open-string-input-port \"(a (b c . d) . e)\") #f)", "(a (b c . d) . e)"},
    /* ------------------------ parser shell -------------------------------- */
    {"(values->list (parse-shell (open-string-input-port \"\") #f))", "(#!eof #f)"},
    {"(parse-shell* (open-string-input-port \"{}\") #f)", "(sh-list)"},
    {"(parse-shell* (open-string-input-port \"ls   -l>/dev/null&\") #f)",
     "(sh-macro ls -l > /dev/null &)"},
    {"(parse-shell* (open-string-input-port\n"
     "  \"{echo  foo  bar|wc -l;  }\") #f)",
     "(sh-list (sh-macro echo foo bar | wc -l))"},
    {"(parse-shell* (open-string-input-port\n"
     "  \"{echo|{cat\n}}\") #f)",
     "(sh-list (sh-macro echo | (sh-list (sh-macro cat))))"},
    {"(parse-shell* (open-string-input-port\n"
     "  \"a>>/dev/null||b>|/dev/zero&&!c>&log\") #f)",
     "(sh-macro a >> /dev/null || b >| /dev/zero && ! c >& log)"},
    /* ------------------------ parser -------------------------------------- */
    {"(values->list (parse-forms\n"
     "  (open-string-input-port \"\")\n"
     "  'scheme (parsers)))",
     "(() #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     /* #!eof is equivalent to end-of-file in the input port */
     "  (open-string-input-port \"'(a . b) c #!eof . ) syntax error\")\n"
     "  'scheme (parsers)))",
     "((begin '(a . b) c) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (open-string-input-port \"uiop asdf #!scheme (xyz %%a)\")\n"
     "  'scheme (parsers)))",
     "((begin uiop asdf (xyz %%a)) #<parser scheme>)"},
    {"(values->list (parse-forms\n"
     "  (open-string-input-port \"`('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)\")\n"
     "  'scheme (parsers)))",
     "((begin `('foo ,bar ,@baz) #`(#'sfoo #,sbar #,@sbaz)) #<parser scheme>)"},
    {"(parse-form*\n" /* { switches to shell parser */
     "  (open-string-input-port \"{ls -l >& log.txt}\")\n"
     "  'scheme (parsers)))",
     "(sh-list (sh-macro ls -l >& log.txt))"},
    {"(parse-form*\n" /* directive #!shell switches to shell parser too */
     "  (open-string-input-port \"(#!shell ls -al >> log.txt)\")\n"
     "  'scheme (parsers)))",
     "(sh-list (sh-macro ls -al >> log.txt))"},
    {"(parse-form*\n" /* ( switches to Scheme parser */
     "  (open-string-input-port \"(apply + a `(,@b))\")\n"
     "  'shell (parsers)))",
     "(sh-macro (apply + a `(,@b)))"},
    {"(parse-form*\n"
     "  (open-string-input-port \"ls (my-dir) >> log.txt\")\n"
     "  'shell (parsers)))",
     "(sh-macro ls (my-dir) >> log.txt)"},
    {"(values->list (parse-forms\n" /* directive #!scheme switches to Scheme parser too */
     "  (open-string-input-port \"ls ~; #!scheme (my-cmd)\")\n"
     "  'shell (parsers)))",
     "((begin (sh-macro ls ~) (my-cmd)) #<parser scheme>)"},
    /* -------------------------- tty --------------------------------------- */
    {"(let ((sz (tty-size)))\n"
     "  (and (pair? sz)\n"
     "       (integer? (car sz))\n"
     "       (integer? (cdr sz))\n"
     "       (positive? (car sz))\n"
     "       (positive? (cdr sz))))",
     "#t"},
    /* ------------------------- posix -------------------------------------- */
    {"(errno)", "0"},
    /* ------------------------- shell jobs --------------------------------- */
    {"(begin\n"
     "  (sh-env-set! #t \"foo\" \"bar\")\n"
     "  (cons\n"
     "    (sh-env-get       #t \"foo\")\n"
     "    (sh-env-exported? #t \"foo\")))",
     "(bar . #f)"},
    {"(sh-cmd \"echo\" \"foo\" \" bar \")", "(sh-cmd \"echo\" \"foo\" \" bar \")"},
    {"(sh-run (sh-cmd \"true\"))", "(exited . 0)"},
    {"(sh-run (sh-cmd \"false\"))", "(exited . 1)"},
    {"(sh-multijob 'hello (lambda (j) '(exited . 42)))", "(sh-hello)"},
    {"(sh-run (sh-multijob 'hello (lambda (j) '(exited . 42))))", "(exited . 42)"},
    {"(sh-run (sh-multijob 'hello (lambda (j) '(killed . sigsegv))))", "(killed . sigsegv)"},
    {"(let ((j (sh-list (sh-cmd \"false\") (sh-cmd \"true\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))",
     "(exited . 0)"},
    {"(let ((j (sh-list (sh-cmd \"true\") (sh-cmd \"false\"))))\n"
     "  (sh-start j)\n"
     "  (sh-wait j))",
     "(exited . 1)"},
    {"(sh-run (sh-and (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 1)"},
    {"(sh-run (sh-or  (sh-cmd \"true\") (sh-cmd \"false\")))", "(exited . 0)"},
    /* ------------------------- shell repl --------------------------------- */
    {"(sh-parse-scheme\n"
     "  (open-string-input-port \"(+ 2 3) (values 7 (cons 'a 'b))\")\n"
     "  (parsers))",
     "(begin (+ 2 3) (values 7 (cons 'a 'b)))"},
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
  if ((err = define_libraries()) < 0) {
    return err;
  }

  errno = 0;
  err   = run_tests();

  scheme_quit();

  return err;
}
