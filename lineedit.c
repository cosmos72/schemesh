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
       "    lineedit-clear! linectx-stdin-set! linectx-stdout-set! linectx-rbuf-insert!\n"
       "    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down"
       "    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol\n"
       "    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char\n"
       "    lineedit-key-del-char-left lineedit-key-del-char-right\n"
       "    lineedit-key-del-word-left lineedit-key-del-word-right\n"
       "    lineedit-key-del-line lineedit-key-del-bol lineedit-key-del-eol\n"
       "    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right\n"
       "    lineedit-key-redraw lineedit-key-tab\n"
       "    lineedit-key-history-next lineedit-key-history-prev\n"
       "    lineedit-keytable-set! lineedit-keytable-find lineedit-read\n"
       "    sh-lineedit sh-repl)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) format fx1+ fx1- inspect void)\n"
       "    (schemesh bootstrap)\n"
       "    (schemesh containers)\n"
       "    (schemesh fd)\n"
       "    (schemesh tty))\n"
       "\n"
       "(define-record-type\n"
       "  (linectx %make-linectx linectx?)\n"
       "  (fields\n"
       "    (mutable rbuf)\n"   /* bytespan, buffer for (fd-read) */
       "    (mutable wbuf)\n"   /* bytespan, buffer for (fd-write) */
       "    (mutable line)\n"   /* chargbuffer, input's current line being edited */
       "    (mutable lines)\n"  /* gbuffer of chargbuffers, input being edited */
       "    (mutable state)\n"  /* bytespan, stack of nested ( [ { and " */
       "    (mutable x)\n"      /* fixnum, cursor x position in line */
       "    (mutable y)\n"      /* fixnum, cursor y position in lines*/
       "    (mutable save-x)\n" /* fixnum, saved cursor x position */
       "    (mutable save-y)\n" /* fixnum, saved cursor y position */
       "    (mutable rows)\n"   /* fixnum, max number of rows being edited */
       "    (mutable width)\n"  /* fixnum, terminal width */
       "    (mutable height)\n" /* fixnum, terminal height */
       "    (mutable stdin)\n"  /* input file descriptor, or binary input port */
       "    (mutable stdout)\n" /* output file descriptor, or binary output port */
       "    (mutable read-timeout-milliseconds)\n"                  /* -1 means unlimited timeout */
       "    (mutable return linectx-return? linectx-return-set!)\n" /* bool */
       "    (mutable eof linectx-eof? linectx-eof-set!)\n"          /* bool */
       "    (mutable keytable)))\n" /* hastable, contains keybindings */
       "\n"
       "(define lineedit-default-keytable (eq-hashtable))\n"
       "\n"
       "(define (make-linectx)\n"
       "  (let* ((sz    (tty-size))\n"
       "         (rbuf  (bytespan))\n"
       "         (wbuf  (bytespan))\n"
       "         (line  (chargbuffer))\n"
       "         (lines (gbuffer line))\n"
       "         (state (bytespan)))\n"
       "    (bytespan-reserve-back! rbuf 1024)\n"
       "    (bytespan-reserve-back! wbuf 1024)\n"
       "    (bytespan-reserve-back! state 32)\n"
       "    (%make-linectx\n"
       "      rbuf wbuf line lines state\n"
       "      0 0 -1 -1 1\n"                  /* x y save-x save-y rows    */
       "      (if (pair? sz) (car sz) 80)\n"  /* width                     */
       "      (if (pair? sz) (cdr sz) 24)\n"  /* height                    */
       "      0 1 -1 #f #f \n"                /* stdin stdout timeout return eof  */
       "      lineedit-default-keytable)))\n" /* keytable */
       "\n"
       /* clear lines and line: they have been parsed and executed */
       "(define (linectx-clear! ctx)\n"
       "  (let ((line (linectx-line ctx))\n"
       "        (lines (linectx-lines ctx)))\n"
       "    (linectx-x-set! ctx 0)\n"
       "    (linectx-y-set! ctx 0)\n"
       "    (linectx-save-x-set! ctx -1)\n"
       "    (linectx-save-y-set! ctx -1)\n"
       "    (chargbuffer-clear! line)\n"
       "    (gbuffer-clear! lines)\n"
       "    (gbuffer-insert-at! lines 0 line)\n"
       "    (linectx-return-set! ctx #f)))\n"
       "\n"
       /* write a byte to wbuf */
       "(define (linectx-u8-write ctx val)\n"
       "  (bytespan-u8-insert-back! (linectx-wbuf ctx) val))\n"
       "\n"
       /* write a portion of given bytevector to wbuf */
       "(define (linectx-bv-write ctx bv start end)\n"
       "  (bytespan-bv-insert-back! (linectx-wbuf ctx) bv start end))\n"
       "\n"
       /* write a portion of given chargbuffer to wbuf */
       "(define (linectx-cgb-write ctx cgb start end)\n"
       "  (do ((wbuf (linectx-wbuf ctx))\n"
       "       (pos start (fx1+ pos)))\n"
       "      ((fx>=? pos end))\n"
       "    (bytespan-utf8-insert-back! wbuf (chargbuffer-ref cgb pos))))\n"
       "\n"
       /* send escape sequence "move cursor left by n", without checking or updating linectx-x */
       "(define (linectx-tty-move-left-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 68) 0 3))\n" /* ESC [ D */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n" /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n"
       "        (bytespan-u8-insert-back! wbuf 68)))))" /* D */
       "\n"
       /* send escape sequence "move cursor right by n", without checking or updating linectx-x */
       "(define (linectx-tty-move-right-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 67) 0 3))\n" /* ESC [ C */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n" /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n"
       "        (bytespan-u8-insert-back! wbuf 67)))))" /* C */
       "\n"
       /* send escape sequence "move to begin-of-line" */
       "(define (linectx-tty-move-to-bol ctx)\n"
       "  (linectx-bv-write ctx #vu8(13) 0 1))\n" /* CTRL+M */
       "\n"
       /* send escape sequence "clear from cursor to end-of-line" */
       "(define (linectx-tty-clear-to-eol ctx)\n"
       "  (linectx-bv-write ctx #vu8(27 91 75) 0 3))\n" /* ESC [ K */
       "\n"
       "(define (linectx-redraw-to-eol ctx clear-line-right)\n"
       "  (let* ((line (linectx-line ctx))\n"
       "         (beg  (linectx-x ctx))\n"
       "         (end  (chargbuffer-length line)))\n"
       "    (when (fx<? beg end)\n"
       "      (linectx-cgb-write ctx line beg end))\n"
       "    (when (eq? clear-line-right 'clear-line-right)\n"
       "      (linectx-tty-clear-to-eol ctx))\n"
       "    (when (fx<? beg end)\n"
       "      (linectx-tty-move-left-n ctx (fx- end beg)))))\n"
       "\n"
       "(define (linectx-flush ctx)\n"
       "  (let* ((wbuf (linectx-wbuf ctx))\n"
       "         (beg  (bytespan-peek-beg wbuf))\n"
       "         (end  (bytespan-peek-end wbuf)))\n"
       "    (when (fx<? beg end)\n"
       "      (let ((bv (bytespan-peek-data wbuf))\n"
       "            (stdout (linectx-stdout ctx))\n"
       "            (n (fx- end beg)))\n"
       "        (if (fixnum? stdout)\n"
       /**        TODO: loop on short writes */
       "          (set! n (fd-write stdout bv beg end))\n"
       "          (put-bytevector stdout bv beg n))\n"
       "        (bytespan-clear! wbuf)))))\n"
       "\n"
       "(define (lineedit-clear! ctx)\n"
       "  (linectx-clear! ctx)\n"
       /* \r ESC [ K */
       "  (linectx-bv-write ctx #vu8(13 27 91 75) 0 4)\n"
       "  (linectx-flush ctx))\n"
       "\n"
       /* consume up to n bytes from rbuf and insert them into current line.
        * return number of bytes actually consumed */
       "(define (linectx-rbuf-insert! ctx n)\n"
       "  (let* ((rbuf  (linectx-rbuf ctx))\n"
       "         (beg   (bytespan-peek-beg rbuf))\n"
       "         (pos   beg)\n"
       "         (end   (fx+ pos n))\n"
       "         (line  (linectx-line ctx))\n"
       "         (x     (linectx-x ctx))\n"
       "         (incomplete #f)\n"
       "         (wbuf  (linectx-wbuf ctx)))\n"
       /**  TODO: handle lines longer than tty width */
       "    (do ()\n"
       "        ((or incomplete\n"
       "             (fx>=? pos end)\n"
       /*            stop at any byte < 32, unless it's the first byte (which we skip) */
       "             (and (fx>? pos beg) (fx<? (bytespan-u8-ref rbuf pos) 32))))\n"
       "      (let-values (((ch len) (bytespan-utf8-ref rbuf pos (fx- end pos))))\n"
       "        (when (eq? #t ch)\n"
       "          (set! incomplete #t))\n"
       "        (set! pos (fxmin end (fx+ pos len)))"
       "        (when (and (char? ch) (char>=? ch #\\space))\n"
       "          (chargbuffer-insert-at! line x ch)\n"
       "          (bytespan-utf8-insert-back! wbuf ch)\n"
       "          (set! x (fx1+ x)))))\n"
       "    (linectx-x-set! ctx x)\n"
       "    (linectx-redraw-to-eol ctx 'dont-clear-line-right)\n"
       "    (fx- pos beg)))\n" /* return number of bytes actually consumed */
       "\n"
       "(define (lineedit-key-nop ctx)\n"
       "  (void))\n"
       "\n"
       /* move cursor left by 1 */
       "(define (lineedit-key-left ctx)\n"
       "  (let ((x (linectx-x ctx)))\n"
       "    (when (fx>? x 0)\n"
       "      (linectx-x-set! ctx (fx1- x))\n"
       "      (linectx-bv-write ctx #vu8(27 91 68) 0 3))))\n" /* ESC [ D */
       "\n"
       "(define (lineedit-key-right ctx)\n"
       "  (let ((x (linectx-x ctx)))\n"
       "    (unless (fx>=? x (chargbuffer-length (linectx-line ctx)))\n"
       "      (linectx-x-set! ctx (fx1+ x))\n"
       "      (linectx-bv-write ctx #vu8(27 91 67) 0 3))))\n" /* ESC [ C */
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
       "  (when (fx>? (linectx-x ctx) 0)\n"
       "    (linectx-tty-move-to-bol ctx)\n"
       "    (linectx-x-set! ctx 0)))\n"
       "\n"
       "(define (lineedit-key-eol ctx)\n"
       "  (let ((x    (linectx-x ctx))\n"
       "        (len  (chargbuffer-length (linectx-line ctx))))\n"
       "    (when (fx<? x len)\n"
       "      (linectx-tty-move-right-n ctx (fx- len x))\n"
       "      (linectx-x-set! ctx len))))\n"
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
       "  (when (fx>? (linectx-x ctx) 0)\n"
       "    (lineedit-key-left ctx)\n"
       "    (lineedit-key-del-char-right ctx)))\n"
       "\n"
       "(define (lineedit-key-del-char-right ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (line (linectx-line ctx))\n"
       "         (len  (chargbuffer-length line)))\n"
       "    (when (fx<? x len)\n"
       "      (chargbuffer-erase-at! line x 1)\n"
#if 1
       "      (linectx-bv-write ctx #vu8(27 91 80) 0 3))))\n" /* VT102 sequence: ESC [ P */
#else
       "      (linectx-redraw-to-eol ctx 'clear-line-right))))\n"
#endif
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
       "(define (lineedit-key-newline-left ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-newline-right ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-enter ctx)\n"
       "  (linectx-return-set! ctx #t)\n"
       "  (linectx-u8-write ctx 10))\n"
       "\n"
       "(define (lineedit-key-history-next ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-history-prev ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-redraw ctx)\n"
       "  (let* ((line (linectx-line ctx))\n"
       "         (len (chargbuffer-length line))\n"
       "         (x (linectx-x ctx)))\n"
       "    (linectx-tty-move-to-bol ctx)\n"
       "    (linectx-cgb-write ctx line 0 len)\n"
       "    (linectx-tty-clear-to-eol ctx)\n"
       "    (linectx-tty-move-left-n ctx (fx- len x))))\n"
       "\n"
       "(define (lineedit-key-tab ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-toggle-insert ctx)\n"
       "  (dynamic-wind\n"
       "    tty-restore!\n"              /* run before body */
       "    (lambda () (inspect ctx))\n" /* body */
       "    tty-setraw!))"               /* run after body */
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
       "    (if (fx>=? rpos (bytespan-length rbuf))\n"
       "      (values htable rpos)\n"
       "      (let* ((ch (bytespan-u8-ref rbuf rpos))\n"
       "             (entry (hashtable-ref htable ch #f))\n"
       "             (rpos+1 (fx1+ rpos)))\n"
       "        (cond\n"
       "          ((procedure? entry) (values entry rpos+1))\n"
       "          ((hashtable? entry) (%find  entry rpos+1))\n"
       "          (#t                 (values #f    (bytespan-length rbuf))))))))\n"
       "\n"
       /** find one key sequence in lineedit-keytable matching rbuf and execute it */
       "(define (linectx-keytable-call ctx)\n"
       "  (assert (linectx? ctx))\n"
       "  (let-values (((proc n) (lineedit-keytable-find\n"
       "                           (linectx-keytable ctx) (linectx-rbuf ctx))))\n"
       "    (cond\n"
       "      ((procedure? proc) (proc ctx))\n"
       "      ((hashtable? proc) (set! n 0))\n" /* incomplete sequence, wait for more keystrokes */
       "      (#t                (set! n (linectx-rbuf-insert! ctx n))))\n"
       "    (let ((rbuf (linectx-rbuf ctx)))\n"
       "      (bytespan-erase-front! rbuf n)\n"
       "      (when (bytespan-empty? rbuf)\n"
       "        (bytespan-clear! rbuf)))\n" /* set begin, end to 0 */
       "    n))\n"
       "\n"
       /**
        * repeatedly call (linectx-keytable-call) until ENTER is found and processed,
        * or until no more keytable matches are found.
        * if user pressed ENTER, return a reference to internal chargbuffer.
        * if waiting for more keypresses, return #t
        * if got end-of-file, return #f
        */
       "(define (linectx-keytable-iterate ctx)\n"
       "  (when (linectx-return? ctx)\n"
       "    (linectx-clear! ctx))\n"
       "  (do ()\n"
       "      ((or (linectx-return? ctx)\n"
       "           (linectx-eof? ctx)\n"
       "           (bytespan-empty? (linectx-rbuf ctx))\n"
       "           (fxzero? (linectx-keytable-call ctx)))))\n"
       "  (linectx-flush ctx)\n"
       "  (cond\n"
       "    ((linectx-return? ctx) (linectx-lines ctx))\n"
       "    ((linectx-eof?    ctx) #f)\n"
       "    (#t #t)))\n"
       "\n"
       /* read some bytes from (linectx-stdin ctx) and append them to (linectx-rbuf ctx).
        * return number of read bytes */
       "(define (linectx-read ctx)\n"
       "  (let* ((rbuf (linectx-rbuf ctx))\n"
       "         (rlen (bytespan-length rbuf))\n"
       "         (max-n 1024)\n"
       "         (stdin (linectx-stdin ctx))\n"
       "         (got 0))\n"
       /*   ensure bytespan-capacity-back is large enough */
       "    (bytespan-reserve-back! rbuf (fx+ rlen max-n))\n"
       "    (if (fixnum? stdin)\n"
       "      (when (eq? 'read (fd-select stdin 'read (linectx-read-timeout-milliseconds ctx)))\n"
       "        (set! got (fd-read stdin (bytespan-peek-data rbuf)\n"
       "                           (bytespan-peek-end rbuf) max-n)))\n"
       "      (let ((n (get-bytevector-n! stdin (bytespan-peek-data rbuf)\n"
       "                                  (bytespan-peek-end rbuf) max-n)))\n"
       "        (when (fixnum? n)\n"
       "          (set! got n))))\n"
       "    (bytespan-resize-back! rbuf (fx+ rlen got))\n"
       "    (assert (fixnum? got))\n"
       "    (assert (fx<=? 0 got max-n))\n"
       "    got))\n"
       "\n"
       /**
        * if user pressed ENTER, return a reference to internal chargbuffer.
        * if waiting for more keypresses, return #t
        * if got end-of-file, return #f
        */
       "(define (lineedit-read ctx)\n"
       "  (assert (linectx? ctx))\n"
       "  (flush-output-port (current-output-port))\n"
       "  (let ((ret (if (bytespan-empty? (linectx-rbuf ctx))\n"
       "               #t\n"
       /*              some bytes already in rbuf, try to consume them */
       "               (linectx-keytable-iterate))))\n"
       "    (if (eq? #t ret)\n"
       "      (if (fx>? (linectx-read ctx) 0)\n"
       /*       got some bytes, call again (linectx-keytable-iterate) and return its value */
       "        (linectx-keytable-iterate ctx)\n"
       /*       got end-of-file, return #f */
       "        #f)\n"
       /*     propagate return value of first (linectx-keytable-iterate) */
       "      ret)))\n"
       "\n"
       /** parse gbuffer of chargbuffers, return corresponding shell commands */
       "(define (sh-parse gb)\n"
#if 1
       "  (display gb)\n"
       "  (display #\\newline)\n"
#else
       "  (assert (gbuffer? gb))\n"
       "  (write (chargbuffer->string gb))\n"
       "  (display #\\newline)\n"
#endif
       /** TODO: implement */
       "  '())\n"
       "\n"
       /** execute parsed shell commands and return their exit status */
       "(define (sh-exec commands)\n"
       "  (assert (or (pair? commands) (null? commands)))\n"
       /** TODO: implement */
       "  0)\n"
       /**
        * if user pressed ENTER, execute entered commands and return their exit status.
        * if waiting for more keypresses, return #t
        * if got end-of-file, return #f
        */
       "(define (sh-lineedit ctx)\n"
       "  (let ((gb (lineedit-read ctx)))\n"
       "    (if (boolean? gb)\n"
       "      gb\n"
       "      (sh-exec (sh-parse gb)))))\n"
       "\n"
       "(define (sh-repl)\n"
       "  (let ((ctx (make-linectx)))\n"
       "    (lineedit-clear! ctx)"
       "    (dynamic-wind\n"
       "      tty-setraw!\n"                /* run before body */
       "      (lambda ()\n"                 /*                 */
       "        (while (sh-lineedit ctx)\n" /* body            */
       "          (void)))\n"               /*                 */
       "      (lambda ()\n"                 /* run after body  */
       "        (linectx-flush ctx)\n"
       "        (tty-restore!)))))\n"
       "\n"
       "(let ((t lineedit-default-keytable)\n"
       "      (%add lineedit-keytable-set!))\n"
       "(%add t lineedit-key-bol 1)\n"               /* CTRL+A        */
       "(%add t lineedit-key-left 2)\n"              /* CTRL+B        */
       "(%add t lineedit-key-break 3)\n"             /* CTRL+C        */
       "(%add t lineedit-key-ctrl-d 4)\n"            /* CTRL+D        */
       "(%add t lineedit-key-eol 5)\n"               /* CTRL+E        */
       "(%add t lineedit-key-right 6)\n"             /* CTRL+F        */
       "(%add t lineedit-key-del-char-left 8 127)\n" /* CTRL+H or BACKSPACE */
       "(%add t lineedit-key-tab 9)\n"               /* CTRL+I or TAB */
       "(%add t lineedit-key-enter 10 13)\n"         /* CTRL+J or ENTER, CTRL+M */
       "(%add t lineedit-key-del-eol 11)\n"          /* CTRL+K        */
       "(%add t lineedit-key-redraw 12)\n"           /* CTRL+L        */
       "(%add t lineedit-key-history-next 14)\n"     /* CTRL+N        */
       "(%add t lineedit-key-newline-right 15)\n"    /* CTRL+O        */
       "(%add t lineedit-key-history-prev 16)\n"     /* CTRL+P        */
       "(%add t lineedit-key-transpose-char 20)\n"   /* CTRL+T        */
       "(%add t lineedit-key-del-line 21)\n"         /* CTRL+U        */
       /* CTRL+W, CTRL+BACKSPACE, ALT+BACKSPACE */
       "(%add t lineedit-key-del-word-left 23 31 '(27 127))\n"
       /* sequences starting with ESC */
       "(%add t lineedit-key-word-left '(27 66) '(27 98))\n"       /* ALT+B, ALT+b */
       "(%add t lineedit-key-del-word-right '(27 68) '(27 100))\n" /* ALT+D, ALT+d */
       "(%add t lineedit-key-word-right '(27 70) '(27 102))\n"     /* ALT+F, ALT+f */
       /* sequences starting with ESC O */
       "(%add t lineedit-key-up    '(27 79 65))\n"            /* UP    \eOA  */
       "(%add t lineedit-key-down  '(27 79 66))\n"            /* DOWN  \eOB  */
       "(%add t lineedit-key-right '(27 79 67))\n"            /* RIGHT \eOC  */
       "(%add t lineedit-key-left  '(27 79 68))\n"            /* LEFT  \eOD  */
       "(%add t lineedit-key-eol   '(27 79 70))\n"            /* END   \eOF  */
       "(%add t lineedit-key-bol   '(27 79 72))\n"            /* HOME  \eOH  */
       "(%add t lineedit-key-newline-left '(27 79 77))\n"     /* KPRET \eOM  */
       "(%add t lineedit-key-nop   '(27 79 80) \n"            /* NUM-LOCK    */
       "   '(27 79 81) '(27 79 82) '(27 79 83))\n"            /* KP/ KP* KP- */
       /* sequences starting with ESC [ */                    /*             */
       "(%add t lineedit-key-up    '(27 91 65))\n"            /* UP    \e[A  */
       "(%add t lineedit-key-down  '(27 91 66))\n"            /* DOWN  \e[B  */
       "(%add t lineedit-key-right '(27 91 67))\n"            /* RIGHT \e[C  */
       "(%add t lineedit-key-left  '(27 91 68))\n"            /* LEFT  \e[D  */
       "(%add t lineedit-key-eol   '(27 91 70))\n"            /* END   \e[F  */
       "(%add t lineedit-key-bol   '(27 91 72))\n"            /* HOME  \e[H  */
       "(%add t lineedit-key-bol   '(27 91 49 126))\n"        /* HOME  \e[1~ */
       "(%add t lineedit-key-history-prev '(27 91 53 126))\n" /* PGUP  \e[5~ */
       "(%add t lineedit-key-history-next '(27 91 54 126))\n" /* PGDWN \e[6~ */
       /* */
       "(%add t lineedit-key-nop   '(27 79 80) '(27 79 81) '(27 79 82)\n" /* F1..F3 */
       "    '(27 79 82) '(27 91 49 53 126))\n"                            /* F4..F5 */
       /* */
       "(%add t lineedit-key-nop   '(27 91 91 65) '(27 91 91 66) '(27 91 91 67)\n" /* F1..F3 */
       "    '(27 91 91 68) '(27 91 91 69) '(27 91 49 55 126) '(27 91 49 56 126)\n" /* F4..F7 */
       "    '(27 91 49 57 126) '(27 91 50 48 126) '(27 91 50 49 126)\n"            /* F8..F10 */
       "    '(27 91 50 50 126) '(27 91 50 51 126) '(27 91 50 52 126))\n"           /* F?..F12 */
#if 1
       "(%add t lineedit-key-toggle-insert '(27 91 50 126))\n" /* INSERT \e[2~ */
#endif
       "(%add t lineedit-key-del-char-right '(27 91 51 126))\n" /* DELETE \e[3~ */
       "(%add t lineedit-key-eol   '(27 91 52 126))\n"          /* END    \e[4~ */
       /**/
       ")\n"   /* close let */
       ")\n"); /* close library */

  eval("(import (schemesh lineedit))\n");
}
