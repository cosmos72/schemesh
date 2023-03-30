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
       "    (mutable rbuf)\n"       /* bytespan, buffer for (fd-read) */
       "    (mutable wbuf)\n"       /* bytespan, buffer for (fw-write) */
       "    (mutable lines)\n"      /* span of bytespans, input being edited */
       "    (mutable state)\n"      /* bytespan, stack of nested ( [ { and " */
       "    (mutable x)\n"          /* fixnum, cursor x position */
       "    (mutable y)\n"          /* fixnum, cursor y position */
       "    (mutable save-x)\n"     /* fixnum, saved cursor x position */
       "    (mutable save-y)\n"     /* fixnum, saved cursor y position */
       "    (mutable rows)\n"       /* fixnum, multiline: max number of rows being edited */
       "    (mutable width)\n"      /* fixnum, terminal width */
       "    (mutable height)))\n"); /* fixnum, terminal height */

  eval("(define (make-linectx)\n"
       "  (let ((sz (tty-size))\n"
       "        (rbuf  (make-bytespan 2048))\n"
       "        (wbuf  (make-bytespan 2048))\n"
       "        (lines (make-span     10))\n"
       "        (state (make-bytespan 32)))\n"
       "    (bytespan-length-set! rbuf 0)\n"
       "    (bytespan-length-set! wbuf 0)\n"
       "    (span-length-set!     lines 0)\n"
       "    (bytespan-length-set! state 0)\n"
       "    (%make-linectx\n"
       "      rbuf wbuf lines state\n"
       "      -1 -1 -1 -1 +1\n"                   /* x y save-x save-y rows */
       "      (if (pair? sz) (car sz) 80)\n"      /* width */
       "      (if (pair? sz) (cdr sz) 24))))\n"); /* height */

  eval("(define (lineedit-insert-rbuf ctx start end)\n"
       /** TODO: update linectx-lines */
       "  (fd-write 1 (bytespan-underlying (linectx-rbuf ctx)) start end))\n");

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
       ")\n");

  eval("(define lineedit-keytable\n"
       "  (eq-hashtable\n"
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
       "(cons 31 lineedit-key-del-word-left)\n"  /* CTRL+BACKSPACE */
       "(cons 127 lineedit-key-del-char-left)\n" /* BACKSPACE     */
       /**/
       "(cons 27\n" /* all sequences starting with ESC */
       "  (eq-hashtable\n"
       "(cons 66  lineedit-key-word-left)\n"      /* ALT+B         */
       "(cons 98  lineedit-key-word-left)\n"      /* ALT+b         */
       "(cons 68  lineedit-key-del-word-right)\n" /* ALT+D         */
       "(cons 100 lineedit-key-del-word-right)\n" /* ALT+d         */
       "(cons 70  lineedit-key-word-right)\n"     /* ALT+F         */
       "(cons 102 lineedit-key-word-right)\n"     /* ALT+f         */
       "(cons 127 lineedit-key-del-word-left)\n"  /* ALT+BACKSPACE */
       /**/
       "(cons 79\n" /* all sequences starting with ESC O */
       "  (eq-hashtable\n"
       "    (cons 70 lineedit-key-eol)\n"   /* END    \eOF  */
       "    (cons 72 lineedit-key-bol)))\n" /* HOME   \eOH  */
       /**/
       "(cons 91\n" /* all sequences starting with ESC [ */
       "  (eq-hashtable\n"
       "(cons 65 lineedit-key-up)\n"    /* UP     \e[A  */
       "(cons 66 lineedit-key-down)\n"  /* DOWN   \e[B  */
       "(cons 67 lineedit-key-right)\n" /* RIGHT  \e[C  */
       "(cons 68 lineedit-key-left)\n"  /* LEFT   \e[D  */
       "(cons 70 lineedit-key-eol)\n"   /* END    \e[F  */
       "(cons 72 lineedit-key-bol)\n"   /* HOME   \e[H  */
       /**/
       "(cons 49\n" /* all sequences starting with ESC [ 1 */
       "  (eq-hashtable\n"
       "    (cons 126 lineedit-key-bol)))\n" /* HOME \e[1~ */
#if 0
       /**/
       "(cons 50\n" /* all sequences starting with ESC [ 2 */
       "  (eq-hashtable\n"
       "    (cons 126 lineedit-key-toggle-insert)))\n" /* INSERT \e[2~ */
#endif
       /**/
       "(cons 51\n" /* all sequences starting with ESC [ 3 */
       "  (eq-hashtable\n"
       "    (cons 126 lineedit-key-del-char-right)))\n" /* DELETE \e[3~ */
       /**/
       "(cons 52\n" /* all sequences starting with ESC [ 4 */
       "  (eq-hashtable\n"
       "    (cons 126 lineedit-key-eol)))\n" /* END \e[4~ */
       /**/
       "    ))))))\n");

  eval("(define (lineedit-keytable-find rbuf rpos)\n"
       "  (assert (bytespan? rbuf))\n"
       "  (let find ((htable lineedit-keytable)\n"
       "             (rpos rpos))\n"
       "    (let* ((ch (bytespan-u8-ref rbuf rpos))\n"
       "           (entry (hashtable-ref htable ch #f))\n"
       "           (rpos+1 (fx1+ rpos)))\n"
       "      (cond\n"
       "        ((procedure? entry) (values entry rpos+1))\n"
       "        ((hashtable? entry) (find   entry rpos+1))\n"
       "        (#t                 (values #f    rpos+1))))))\n");

  eval("(define (lineedit-keytable-apply ctx rstart)\n"
       "  (assert (lineedit? ctx))\n"
       "  (let-values (((proc rpos) (lineedit-keytable-find (linectx-rbuf ctx) rstart)))\n"
       "    (if (procedure? proc)\n"
       "      (begin\n"
       "        (proc ctx)\n"
       "        rpos)\n"
       "      (lineedit-insert-rbuf ctx rstart rpos))))\n");

  eval("(define (lineedit-readsome ctx timeout-milliseconds)\n"
       "  (assert (linectx? ctx))\n"
       "  (assert (fixnum? timeout-milliseconds))\n"
       "  (let* ((rbuf (linectx-rbuf ctx))\n"
       "         (rlen (bytespan-length rbuf))\n"
       "         (delta 1024))\n"
       /*   ensure bytespan-capacity is large enough */
       "    (bytespan-length-set! rbuf (fx+ rlen delta))\n"
       "    (bytespan-length-set! rbuf delta)\n"
       "    (when (eq? 'read (fd-select 0 'read timeout-milliseconds))\n"
       "      (let ((got (fd-read 0 (bytespan-underlying rbuf) 0 rlen)))\n"
       "        (assert (fixnum? got))\n"
       "        (assert (fx>=? got 0))\n"
       "        (bytespan-length-set! rbuf (fx+ rlen got)))\n"
       "      (lineedit-keytable-apply ctx))))\n");

  eval("(define (sh-lineedit ctx)\n"
       "  (assert (linectx? ctx))\n"
       "  (void))\n");
}
