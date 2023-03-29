/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "lineedit.h"
#include "eval.h"

void define_lineedit_functions(void) {
  eval("(define-record-type\n"
       "  (linectx %make-linectx linectx?)\n"
       "  (fields\n"
       "    (mutable rbuf)\n"       /* bytearray, buffer for (fd-read) */
       "    (mutable wbuf)\n"       /* bytearray, buffer for (fw-write) */
       "    (mutable lines)\n"      /* array of bytearrays, input being edited */
       "    (mutable state)\n"      /* bytearray, stack of nested ( [ { and " */
       "    (mutable x)\n"          /* fixnum, cursor x position */
       "    (mutable y)\n"          /* fixnum, cursor y position */
       "    (mutable save-x)\n"     /* fixnum, saved cursor x position */
       "    (mutable save-y)\n"     /* fixnum, saved cursor y position */
       "    (mutable rows)\n"       /* fixnum, multiline: max number of rows being edited */
       "    (mutable width)\n"      /* fixnum, terminal width */
       "    (mutable height)))\n"); /* fixnum, terminal height */

  eval("(define (make-linectx)\n"
       "  (let ((sz (tty-size))\n"
       "        (rbuf  (make-bytearray 2048))\n"
       "        (wbuf  (make-bytearray 2048)))\n"
       "        (lines (make-array     10)))\n"
       "        (state (make-bytearray 32)))\n"
       "    (bytearray-length-set! rbuf 0)\n"
       "    (bytearray-length-set! wbuf 0)\n"
       "    (array-length-set!     lines 0)\n"
       "    (bytearray-length-set! state 0)\n"
       "    (%make-linectx\n"
       "      rbuf wbuf lines state\n"
       "      -1 -1 -1 -1 +1\n"                   /* x y save-x save-y rows */
       "      (if (pair? sz) (car sz) 80)\n"      /* width */
       "      (if (pair? sz) (cdr sz) 24))))\n"); /* height */

  eval("(begin\n"
       "\n"
       "(define (lineedit-key-left ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-right ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-up ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-down ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-word-left ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-word-right ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-bol ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-eol ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-break ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-ctrl-d ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-transpose-char ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-char-left ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-char-right ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-word-left ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-word-right ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-line ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-bol ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-eol ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-tab ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-enter ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-redraw ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-history-next ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-history-prev ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-keyseq . bytes)\n"
       "  (do ((ret 0 (fxior (car tail) (fxarithmetic-shift-left ret 8)))\n"
       "       (tail bytes (cdr tail)))\n"
       "      ((null? tail) ret)))\n"
       ")\n");

  eval("(define lineedit-keytable\n"
       "  (let ((seq lineedit-keyseq))\n"
       "    (eq-hashtable\n"
       "(cons 1 lineedit-key-bol)\n"             /* CTRL+A */
       "(cons 2 lineedit-key-left)\n"            /* CTRL+B */
       "(cons 3 lineedit-key-break)\n"           /* CTRL+C */
       "(cons 4 lineedit-key-ctrl-d)\n"          /* CTRL+D */
       "(cons 5 lineedit-key-eol)\n"             /* CTRL+E */
       "(cons 6 lineedit-key-right)\n"           /* CTRL+F */
       "(cons 8 lineedit-key-del-char-left)\n"   /* CTRL+H */
       "(cons 9 lineedit-key-tab)\n"             /* CTRL+I or TAB  */
       "(cons 10 lineedit-key-enter)\n"          /* CTRL+J or LF   */
       "(cons 11 lineedit-key-del-eol)\n"        /* CTRL+K         */
       "(cons 12 lineedit-key-redraw)\n"         /* CTRL+L         */
       "(cons 13 lineedit-key-enter)\n"          /* CTRL+M or CR   */
       "(cons 14 lineedit-key-history-next)\n"   /* CTRL+N         */
       "(cons 16 lineedit-key-history-prev)\n"   /* CTRL+P         */
       "(cons 20 lineedit-key-transpose-char)\n" /* CTRL+T         */
       "(cons 21 lineedit-key-del-line)\n"       /* CTRL+U         */
       "(cons 23 lineedit-key-del-word-left)\n"  /* CTRL+W         */
#if 0
        "(cons 31 lineedit-key-del-word-left)\n"  /* CTRL+BACKSPACE */
#endif
       "(cons 127 lineedit-key-del-char-left)\n"           /* BACKSPACE     */
       "(cons (seq 27 66)  lineedit-key-word-left)\n"      /* ALT+B         */
       "(cons (seq 27 98)  lineedit-key-word-left)\n"      /* ALT+b         */
       "(cons (seq 27 68)  lineedit-key-del-word-right)\n" /* ALT+D         */
       "(cons (seq 27 100) lineedit-key-del-word-right)\n" /* ALT+d         */
       "(cons (seq 27 70)  lineedit-key-word-right)\n"     /* ALT+F         */
       "(cons (seq 27 102) lineedit-key-word-right)\n"     /* ALT+f         */
       "(cons (seq 27 127) lineedit-key-del-word-left)\n"  /* ALT+BACKSPACE */

       "(cons (seq 27 79 70) lineedit-key-eol)\n"   /* END    \eOF  */
       "(cons (seq 27 79 72) lineedit-key-bol)\n"   /* HOME   \eOH  */
       "(cons (seq 27 91 65) lineedit-key-up)\n"    /* UP     \e[A  */
       "(cons (seq 27 91 66) lineedit-key-down)\n"  /* DOWN   \e[B  */
       "(cons (seq 27 91 67) lineedit-key-right)\n" /* RIGHT  \e[C  */
       "(cons (seq 27 91 68) lineedit-key-left)\n"  /* LEFT   \e[D  */
       "(cons (seq 27 91 70) lineedit-key-eol)\n"   /* END    \e[F  */
       "(cons (seq 27 91 72) lineedit-key-bol)\n"   /* HOME   \e[H  */

       "(cons (seq 27 91 49 126) lineedit-key-bol)\n" /* HOME \e[1~    */
#if 0
       "(cons (seq 27 91 50 126) lineedit-key-toggle-insert)\n" /* INSERT \e[2~   */
#endif
       "(cons (seq 27 91 51 126) lineedit-key-del-char-right)\n" /* DELETE \e[3~ */
       "(cons (seq 27 91 52 126) lineedit-key-eol)\n"            /* END    \e[4~ */

       "    )))\n");

  eval("(define (lineedit-keytable-apply ctx)\n"
       "  (assert (lineedit? ctx))\n"
       "  (void))\n");

  eval("(define (lineedit-readsome ctx timeout-milliseconds)\n"
       "  (assert (linectx? ctx))\n"
       "  (assert (fixnum? timeout-milliseconds))\n"
       "  (let* ((rbuf (linectx-rbuf ctx))\n"
       "         (rlen (bytearray-length rbuf))\n"
       "         (delta 1024))\n"
       /*   ensure bytearray-capacity is large enough */
       "    (bytearray-length-set! rbuf (fx+ rlen delta))\n"
       "    (bytearray-length-set! rbuf delta)\n"
       "    (when (eq? 'read (fd-select 0 'read timeout-milliseconds))\n"
       "      (let ((got (fd-read 0 (bytearray-underlying rbuf) rlen)))\n"
       "        (assert (fixnum? got))\n"
       "        (assert (fx>=? got 0))\n"
       "        (bytearray-length-set! rbuf (fx+ rlen got)))\n"
       "      (lineedit-keytable-apply ctx))))\n");

  eval("(define (sh-lineedit ctx)\n"
       "  (assert (linectx? ctx))\n"
       "  (void))\n");
}
