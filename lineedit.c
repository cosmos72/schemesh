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

void define_library_lineedit(void) {
  eval("(library (schemesh lineedit (0 1))\n"
       "  (export\n"
       "    make-linectx linectx? lineedit-default-keytable\n"
       "    lineedit-insert!\n"
       "    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down"
       "    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol\n"
       "    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char\n"
       "    lineedit-key-del-char-left lineedit-key-del-char-right\n"
       "    lineedit-key-del-word-left lineedit-key-del-word-right\n"
       "    lineedit-key-del-line lineedit-key-del-bol lineedit-key-del-eol\n"
       "    lineedit-key-enter lineedit-key-redraw lineedit-key-tab\n"
       "    lineedit-key-history-next lineedit-key-history-prev\n"
       "    lineedit-keytable-set! lineedit-keytable-find lineedit-readsome\n"
       "    sh-lineedit sh-repl)\n"
       "  (import\n"
       "    (chezscheme)\n"
       "    (schemesh bootstrap)\n"
       "    (schemesh containers span)\n"
       "    (schemesh containers bytespan)\n"
       "    (schemesh containers hashtable)\n"
       "    (schemesh fd)\n"
       "    (schemesh tty))\n"
       "\n"
       "(define-record-type\n"
       "  (linectx %make-linectx linectx?)\n"
       "  (fields\n"
       "    (mutable rbuf)\n"       /* bytespan, buffer for (fd-read) */
       "    (mutable wbuf)\n"       /* bytespan, buffer for (fd-write) */
       "    (mutable lines)\n"      /* span of bytespans, input being edited */
       "    (mutable state)\n"      /* bytespan, stack of nested ( [ { and " */
       "    (mutable x)\n"          /* fixnum, cursor x position */
       "    (mutable y)\n"          /* fixnum, cursor y position */
       "    (mutable save-x)\n"     /* fixnum, saved cursor x position */
       "    (mutable save-y)\n"     /* fixnum, saved cursor y position */
       "    (mutable rows)\n"       /* fixnum, max number of rows being edited */
       "    (mutable width)\n"      /* fixnum, terminal width */
       "    (mutable height)\n"     /* fixnum, terminal height */
       "    (mutable eof)\n"        /* bool */
       "    (mutable keytable)))\n" /* hastable, contains keybindings */
       "\n"
       "(define lineedit-default-keytable (eq-hashtable))\n"
       "\n"
       "(define (make-linectx)\n"
       "  (let ((sz (tty-size))\n"
       "        (rbuf  (make-bytespan 2048))\n"
       "        (wbuf  (make-bytespan 2048))\n"
       "        (lines (make-span     10))\n"
       "        (state (make-bytespan 32)))\n"
       "    (bytespan-resize-back! rbuf 0)\n"
       "    (bytespan-resize-back! wbuf 0)\n"
       "    (span-resize-back!     lines 0)\n"
       "    (bytespan-resize-back! state 0)\n"
       "    (%make-linectx\n"
       "      rbuf wbuf lines state\n"
       "      -1 -1 -1 -1 +1\n"                  /* x y save-x save-y rows */
       "      (if (pair? sz) (car sz) 80)\n"     /* width        */
       "      (if (pair? sz) (cdr sz) 24)\n"     /* height       */
       "      #f lineedit-default-keytable)))\n" /* eof keytable */
       "\n"
       "(define (lineedit-insert! ctx n)\n"
       /** TODO: update linectx-lines */
       "  (let* ((rbuf  (linectx-rbuf ctx))\n"
       "         (start (bytespan-peek-beg rbuf))\n"
       "         (end   (fx+ start n)))\n"
       "    (fd-write 1 (bytespan-peek-data rbuf) start end)))\n"
       "\n"
       "(define (lineedit-key-nop ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-left ctx)\n"
       "  (display \"<-\"))\n"
       "\n"
       "(define (lineedit-key-right ctx)\n"
       "  (display \"->\"))\n"
       "\n"
       "(define (lineedit-key-up ctx)\n"
       "  (display \"^^\"))\n"
       "\n"
       "(define (lineedit-key-down ctx)\n"
       "  (display \"vv\"))\n"
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
       "  (linectx-eof-set! ctx #t))\n" /** FIXME: temporary */
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
       "(define (lineedit-keytable-set! keytable proc . keysequences)\n"
       "  (letrec\n"
       "    ((%add-bytelist (lambda (htable bytelist)\n"
       "      (let ((byte (car bytelist)))\n"
       "        (if (null? (cdr bytelist))\n"
       "          (hashtable-set! htable byte proc)\n"
       "          (let ((inner-htable (hashtable-ref htable byte #f)))\n"
       "            (unless (hashtable? inner-htable)\n"
       "              (set! inner-htable (eq-hashtable))\n"
       "              (hashtable-set! htable byte inner-htable))\n"
       "            (%add-bytelist inner-htable (cdr bytelist)))))))\n"
       "     (%any->bytelist (lambda (keyseq)\n"
       "       (cond\n"
       "         ((fixnum?     keyseq) (list keyseq))\n"
       "         ((pair?       keyseq) keyseq)\n"
       "         ((bytevector? keyseq) (bytevector->u8-list keyseq))\n"
       "         ((string?     keyseq) (bytevector->u8-list (string->utf8 keyseq)))\n"
       "         (#t (assert\n"
       "               (or (fixnum? keyseq) (pair? keyseq)\n"
       "                   (bytevector? keyseq) (string? keyseq))))))))\n"
       "    (do ((l keysequences (cdr l)))\n"
       "        ((null? l))\n"
       "      (%add-bytelist keytable (%any->bytelist (car l))))))\n"
       "\n"
       "(define (lineedit-keytable-find keytable rbuf)\n"
       "  (assert (bytespan? rbuf))\n"
       "  (let %find ((htable keytable)\n"
       "              (rpos 0))\n"
       "    (if (fx>= rpos (bytespan-length rbuf))\n"
       "      (values htable rpos)\n"
       "      (let* ((ch (bytespan-u8-ref rbuf rpos))\n"
       "             (entry (hashtable-ref htable ch #f))\n"
       "             (rpos+1 (fx1+ rpos)))\n"
       "        (cond\n"
       "          ((procedure? entry) (values entry rpos+1))\n"
       "          ((hashtable? entry) (%find  entry rpos+1))\n"
       "          (#t                 (values #f    rpos+1)))))))\n"
       "\n"
       /** find one key sequence in lineedit-keytable matching rbuf and execute it */
       "(define (lineedit-keytable-call ctx)\n"
       "  (assert (linectx? ctx))\n"
       "  (let-values (((proc n) (lineedit-keytable-find\n"
       "                           (linectx-keytable ctx) (linectx-rbuf ctx))))\n"
       "    (cond\n"
       "      ((procedure? proc) (proc ctx))\n"
       "      ((hashtable? proc) (set! n 0))\n" /* incomplete sequence, wait for more keystrokes */
       "      (#t                (lineedit-insert! ctx n)))\n"
       "    (bytespan-erase-front! (linectx-rbuf ctx) n)\n"
       "    n))\n"
       "\n"
       /** repeatedly call (lineedit-keytable-call) until no more matches are found */
       "(define (lineedit-keytable-iterate ctx)\n"
       "  (do ()\n"
       "      ((or (linectx-eof ctx) (fx=? 0 (lineedit-keytable-call ctx))))))\n"
       "\n"
       "(define (lineedit-readsome ctx timeout-milliseconds)\n"
       "  (assert (linectx? ctx))\n"
       "  (assert (fixnum? timeout-milliseconds))\n"
       "  (let* ((rbuf (linectx-rbuf ctx))\n"
       "         (rlen (bytespan-length rbuf))\n"
       "         (delta 1024))\n"
       /*   ensure bytespan-capacity-back is large enough */
       "    (bytespan-reserve-back! rbuf (fx+ rlen delta))\n"
       "    (flush-output-port)\n"
       "    (when (eq? 'read (fd-select 0 'read timeout-milliseconds))\n"
       "      (let ((got (fd-read 0 (bytespan-peek-data rbuf) (bytespan-peek-end rbuf) delta)))\n"
       "        (assert (fixnum? got))\n"
       "        (assert (fx<=? 0 got delta))\n"
       "        (when (fx>? got 0)\n"
       "          (bytespan-resize-back! rbuf (fx+ rlen got))\n"
       "          (lineedit-keytable-iterate ctx)\n"
       "          (not (linectx-eof ctx)))))))\n"
       "\n"
       "(define (sh-lineedit ctx)\n"
       "  (lineedit-readsome ctx -1))\n"
       "\n"
       "(define (sh-repl)\n"
       "  (let ((ctx (make-linectx)))\n"
       "    (dynamic-wind\n"
       "      tty-setraw!\n"                  /* run before body */
       "      (lambda ()\n"                   /* body            */
       "        (while (sh-lineedit ctx)))\n" /*                 */
       "      (lambda ()\n"                   /* run after body  */
       "        (flush-output-port)\n"
       "        (tty-restore!)))))\n"
       "\n"
       "(let ((t lineedit-default-keytable)\n"
       "      (%add lineedit-keytable-set!))\n"
       "(%add t lineedit-key-bol 1)\n"               /* CTRL+A           */
       "(%add t lineedit-key-left 2)\n"              /* CTRL+B           */
       "(%add t lineedit-key-break 3)\n"             /* CTRL+C           */
       "(%add t lineedit-key-ctrl-d 4)\n"            /* CTRL+D           */
       "(%add t lineedit-key-eol 5)\n"               /* CTRL+E           */
       "(%add t lineedit-key-right 6)\n"             /* CTRL+F           */
       "(%add t lineedit-key-del-char-left 8 127)\n" /* CTRL+H BACKSPACE */
       "(%add t lineedit-key-tab 9)\n"               /* CTRL+I or TAB    */
       "(%add t lineedit-key-enter 10 13)\n"         /* CTRL+J or LF, CTRL+M or CR */
       "(%add t lineedit-key-del-eol 11)\n"          /* CTRL+K           */
       "(%add t lineedit-key-redraw 12)\n"           /* CTRL+L           */
       "(%add t lineedit-key-history-next 14)\n"     /* CTRL+N           */
       "(%add t lineedit-key-history-prev 16)\n"     /* CTRL+P           */
       "(%add t lineedit-key-transpose-char 20)\n"   /* CTRL+T           */
       "(%add t lineedit-key-del-line 21)\n"         /* CTRL+U           */
                                                     /* CTRL+W, CTRL+BACKSPACE, ALT+BACKSPACE */
       "(%add t lineedit-key-del-word-left 23 31 '(27 127))\n"
       /* sequences starting with ESC */
       "(%add t lineedit-key-word-left '(27 66) '(27 98))\n"       /* ALT+B, ALT+b */
       "(%add t lineedit-key-del-word-right '(27 68) '(27 100))\n" /* ALT+D, ALT+d */
       "(%add t lineedit-key-word-right '(27 70) '(27 102))\n"     /* ALT+F, ALT+f */
       /* sequences starting with ESC O */
       "(%add t lineedit-key-eol   '(27 79 70))\n"     /* END   \eOF  */
       "(%add t lineedit-key-bol   '(27 79 72))\n"     /* HOME  \eOH  */
       /* sequences starting with ESC [ */             /*             */
       "(%add t lineedit-key-up    '(27 91 65))\n"     /* UP    \e[A  */
       "(%add t lineedit-key-down  '(27 91 66))\n"     /* DOWN  \e[B  */
       "(%add t lineedit-key-right '(27 91 67))\n"     /* RIGHT \e[C  */
       "(%add t lineedit-key-left  '(27 91 68))\n"     /* LEFT  \e[D  */
       "(%add t lineedit-key-eol   '(27 91 70))\n"     /* END   \e[F  */
       "(%add t lineedit-key-bol   '(27 91 72))\n"     /* HOME  \e[H  */
       "(%add t lineedit-key-bol   '(27 91 49 126))\n" /* HOME  \e[1~ */
#if 0
       "(%add t lineedit-key-toggle-insert '(27 91 50 126))\n" /* INSERT \e[2~ */
#endif
       "(%add t lineedit-key-del-char-right '(27 91 51 126))\n" /* DELETE \e[3~ */
       "(%add t lineedit-key-eol   '(27 91 52 126))\n"          /* END    \e[4~ */
       /**/
       ")\n"   /* close let */
       ")\n"); /* close library */

  eval("(import (schemesh lineedit))\n");
}
