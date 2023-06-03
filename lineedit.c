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

static void schemesh_define_library_lineedit_base(void) {
  eval("(library (schemesh lineedit base (0 1))\n"
       "  (export\n"
       "    charline charline? string->charline string->charline* charline->string\n"
       "    charline-nl? charline-nl?-set! charline-copy-on-write\n"
       "    charline-empty? charline-length charline-ref charline-set!\n"
       "    charline-clear! charline-erase-at! charline-insert-at! assert-charline?\n"
       "\n"
       "    charlines charlines? charlines-iterate charlines-empty? charlines-length\n"
       "    charlines-clear! charlines-copy-on-write charlines-erase-at! charlines-insert-at!\n"
       "    charlines-ref charlines-set! assert-charlines?\n"
       "\n"
       "    charhistory charhistory? make-charhistory\n"
       "    charhistory-empty? charhistory-length charhistory-cow-ref charhistory-set!)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (rnrs mutable-pairs)   set-car!)\n"
       "    (only (rnrs mutable-strings) string-set!)\n"
       "    (only (chezscheme) fx1+ fx1- record-writer string-copy!)\n"
       "    (schemesh containers))\n"
       "\n"
       /* copy-pasted from container.c */
       "(define-record-type\n"
       "  (%chargbuffer %make-chargbuffer %chargbuffer?)\n"
       "  (fields\n"
       "     (mutable left  chargbuffer-left  chargbuffer-left-set!)\n"
       "     (mutable right chargbuffer-right chargbuffer-right-set!))\n"
       "  (nongenerative #{%chargbuffer itah4n3k0nl66ucaakkpqk55m-16}))\n"
       "\n"
       /**
        * type charline is a char gap-buffer with two additional fields:
        * - charline-newline? true if the gap buffer logically ends with a #\newline
        * - charline-share a cons. its car will be > 0 if the gab buffer is shared copy-on-write
        *   between two or more charlines: its content will be automatically cloned at the first
        *   attempt to modify it.
        */
       "(define-record-type\n"
       "  (%charline %make-charline charline?)\n"
       "  (parent %chargbuffer)\n"
       "  (fields\n"
       "    (mutable newline? charline-nl? charline-nl?-set!)\n"
       "    (mutable share))\n"
       "  (nongenerative #{%charline i81qf0lqcmlgj68ai4ihn68w3-28}))\n"
       "\n"
       "(define (assert-charline? who line)\n"
       "  (unless (charline? line)\n"
       "    (assertion-violation who \"not a charline\" line)))\n"
       "\n"
       "(define (make-charline left-span right-span nl?)\n"
       "  (assert (charspan? left-span))\n"
       "  (assert (charspan? right-span))\n"
       "  (assert (boolean? nl?))\n"
       "  (%make-charline left-span right-span nl? (cons 0 #f)))\n"
       "\n"
       /** increment charline share count by 1.
        * return pair containing share count */
       "(define (charline-share-inc! line)\n"
       "  (let ((pair (%charline-share line)))\n"
       "    (set-car! pair (fx1+ (car pair)))\n"
       "    pair))\n"
       "\n"
       /** decrement charline share count by 1.
        * return #t if charline was shared, otherwise return #f */
       "(define (charline-share-dec! line)\n"
       "  (let* ((pair (%charline-share line))\n"
       "         (count (car pair))\n"
       "         (shared? (fx>? count 0)))\n"
       "    (when shared?\n"
       "      (set-car! pair (fx1- count)))\n"
       "    shared?))\n"
       "\n"
       "(define (charline)\n"
       "  (make-charline (charspan) (charspan) #f))\n"
       "\n"
       /**
        * Return a copy-on-write clone of specified charline.
        */
       "(define (charline-copy-on-write line)\n"
       "  (%make-charline (chargbuffer-left line) (chargbuffer-right line)\n"
       "                  (charline-nl? line) (charline-share-inc! line)))\n"
       "\n"
       /** if charline was a copy-on-write clone, actually clone it. */
       "(define (charline-unshare! line)\n"
       "  (when (charline-share-dec! line)\n"
       "    (chargbuffer-left-set!  line (charspan-copy (chargbuffer-left line)))\n"
       "    (chargbuffer-right-set! line (charspan-copy (chargbuffer-right line)))\n"
       "    (%charline-share-set! line (cons 0 #f))))\n"
       "\n"
       "(define charline-empty?     chargbuffer-empty?)\n"
       "(define charline-length     chargbuffer-length)\n"
       "(define charline-ref        chargbuffer-ref)\n"
       "\n"
       "(define (charline-set! line idx ch)\n"
       "  (charline-unshare! line)\n"
       "  (chargbuffer-set! line idx ch))\n"
       "\n"
       "(define (charline-insert-at! line idx ch)\n"
       "  (charline-unshare! line)\n"
       "  (chargbuffer-insert-at! line idx ch))\n"
       "\n"
       "(define (charline-erase-at! line start n)\n"
       "  (charline-unshare! line)\n"
       "  (chargbuffer-erase-at! line start n))\n"
       "\n"
       "(define (charline-clear! line)\n"
       "  (charline-unshare! line)\n"
       "  (chargbuffer-clear! line))\n"
       "\n"
       "(define (string->charline str)\n"
       "  (let ((line (make-charline (charspan) (string->charspan str) #f))\n"
       "        (last (fx1- (string-length str))))\n"
       "    (when (and (fx>=? last 0) (char=? #\\newline (string-ref str last)))\n"
       "      (charline-erase-at! line last 1)\n"
       "      (charline-nl?-set! line #t))\n"
       "    line))\n"
       "\n"
       "(define (string->charline* str)\n"
       "  (let ((line (make-charline (charspan) (string->charspan* str) #f))\n"
       "        (last (fx1- (string-length str))))\n"
       "    (when (and (fx>=? last 0) (char=? #\\newline (string-ref str last)))\n"
       "      (charline-erase-at! line last 1)\n"
       "      (charline-nl?-set! line #t))\n"
       "    line))\n"
       "\n"
       "(define (charline->string line)\n"
       "  (if (charline-nl? line)\n"
       "    (let* ((left    (chargbuffer-left  line))\n"
       "           (right   (chargbuffer-right line))\n"
       "           (left-n  (charspan-length left))\n"
       "           (right-n (charspan-length right))\n"
       "           (n       (fx+ left-n right-n))\n"
       "           (dst     (make-string (fx1+ n))))\n"
       "      (string-copy! (charspan-peek-data left)  (charspan-peek-beg left)\n"
       "                    dst 0 left-n)\n"
       "      (string-copy! (charspan-peek-data right) (charspan-peek-beg right)\n"
       "                    dst left-n right-n)\n"
       "      (string-set! dst n #\\newline)\n"
       "      dst)\n"
       "    (chargbuffer->string line)))\n"
       "\n"
       /* copy-pasted from container.c */
       "(define-record-type\n"
       "  (%gbuffer %make-gbuffer %gbuffer?)\n"
       "  (fields\n"
       "     (mutable left  gbuffer-left  gbuffer-left-set!)\n"
       "     (mutable right gbuffer-right gbuffer-right-set!))\n"
       "  (nongenerative #{%gbuffer ejch98ka4vi1n9dn4ybq4gzwe-0}))\n"
       "\n"
       /**
        * type charlines is a gap-buffer, containing charline elements
        */
       "(define-record-type\n"
       "  (%charlines %make-charlines charlines?)\n"
       "  (parent %gbuffer)\n"
       "  (nongenerative #{%charlines g2x4legjl16y9nnoua5c9y1u9-28}))\n"
       "\n"
       "(define (assert-charlines? who lines)\n"
       "  (unless (charlines? lines)\n"
       "    (assertion-violation who \"not a charlines\" lines)))\n"
       "\n"
       "(define (charlines . vals)\n"
       "  (list-iterate vals (lambda (val) (assert-charline? 'charlines val)))\n"
       "  (%make-charlines (span) (list->span vals)))\n"
       "\n"
       "(define charlines-iterate    gbuffer-iterate)\n"
       "(define charlines-empty?     gbuffer-empty?)\n"
       "(define charlines-length     gbuffer-length)\n"
       "(define charlines-clear!     gbuffer-clear!)\n"
       "(define charlines-erase-at!  gbuffer-erase-at!)\n"
       "(define charlines-insert-at! gbuffer-insert-at!)\n"
       "(define charlines-ref        gbuffer-ref)\n"
       "(define (charlines-set! lines idx line)\n"
       "  (assert-charlines? 'charlines-set! lines)\n"
       "  (assert-charline?  'charlines-set! line)\n"
       "  (gbuffer-set! lines idx line))\n"
       "\n"
       /**
        * Return a copy-on-write clone of charlines.
        * Also calls (charline-copy-on-write) on each line.
        */
       "(define (charlines-copy-on-write lines)\n"
       "  (let ((dst (make-span (charlines-length lines))))\n"
       "    (charlines-iterate lines\n"
       "      (lambda (i line)\n"
       "        (span-set! dst i (charline-copy-on-write line))))\n"
       "    (%make-charlines (span) dst)))\n"
       "\n"
       /* copy-pasted from container.c */
       "(define-record-type\n"
       "  (%span %make-span %span?)\n"
       "  (fields\n"
       "     (mutable beg span-beg span-beg-set!)\n"
       "     (mutable end span-end span-end-set!)\n"
       "     (mutable vec span-vec span-vec-set!))\n"
       "  (nongenerative #{%span ng1h8vurkk5k61p0jsryrbk99-0}))\n"
       "\n"
       /**
        * type charhistory is a span containing charlines elements (the history itself)
        */
       "(define-record-type\n"
       "  (%charhistory %make-charhistory charhistory?)\n"
       "  (parent %span)\n"
       "  (nongenerative #{%charhistory db0fmss41lc1voqx7uww5xnhu-28}))\n"
       "\n"
       "(define (charhistory . vals)\n"
       "  (list-iterate vals (lambda (val) (assert-charlines? 'charhistory val)))\n"
       "  (let* ((vec (list->vector vals))\n"
       "         (n (vector-length vec)))\n"
       "    (%make-charhistory 0 n vec)))\n"
       "\n"
       "(define (make-charhistory n)\n"
       /* optimization: (charhistory-cow-ref) returns a copy-on-write clone of i-th
        * charline, thus we can reuse the same empty (charlines) for all elements */
       "  (%make-charhistory 0 n (make-vector n (charlines))))\n"
       "\n"
       "(define charhistory-empty? span-empty?)\n"
       "(define charhistory-length span-length)\n"
       "\n"
       /** return a copy-on-write clone of i-th charlines in history */
       "(define (charhistory-cow-ref hist idx)\n"
       "  (charlines-copy-on-write (span-ref hist idx)))\n"
       "\n"
       /** set i-th charlines in history. Resizes history if needed */
       "(define (charhistory-set! hist idx lines)\n"
       "  (assert-charlines? 'charhistory-set! lines)\n"
       "  (let ((len (span-length hist)))\n"
       "    (when (fx>=? idx len)\n"
       "      (span-resize-back! hist (fx1+ idx))\n"
       /*     optimization: (charhistory-cow-ref) returns a copy-on-write clone of i-th
        *     charline, thus we can reuse the same empty (charlines) for all elements we add */
       "      (let ((lines (charlines)))"
       "        (do ((i len (fx1+ i)))\n"
       "            ((fx>=? i idx))\n"
       "          (span-set! hist i lines)))))\n"
       "  (span-set! hist idx lines))\n"
       "\n"
       /** customize how "charline" objects are printed */
       "(record-writer (record-type-descriptor %charline)\n"
       "  (lambda (line port writer)\n"
       "    (display \"(string->charline* \" port)\n"
       "    (write (charline->string line) port)\n"
       "    (display #\\) port)))\n"
       "\n"
       /** customize how "charlines" objects are printed */
       "(record-writer (record-type-descriptor %charlines)\n"
       "  (lambda (sp port writer)\n"
       "    (display \"(charlines\" port)\n"
       "    (charlines-iterate sp"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       "\n"
       /** customize how "charhistory" objects are printed */
       "(record-writer (record-type-descriptor %charhistory)\n"
       "  (lambda (hist port writer)\n"
       "    (display \"(charhistory\" port)\n"
       "    (span-iterate hist"
       "      (lambda (i elem)"
       "        (display #\\space port)\n"
       "        (writer elem port)))\n"
       "    (display #\\) port)))\n"
       "\n"
       ")\n"); /* close library */
}

void schemesh_define_library_lineedit(void) {
  schemesh_define_library_lineedit_base();

  eval("(library (schemesh lineedit (0 1))\n"
       "  (export\n"
       "    make-linectx make-linectx* linectx? lineedit-default-keytable lineedit-clear!\n"
       "    lineedit-lines-set! linectx-stdin-set! linectx-stdout-set! linectx-rbuf-insert!\n"
       "    lineedit-key-nop lineedit-key-left lineedit-key-right lineedit-key-up lineedit-key-down"
       "    lineedit-key-word-left lineedit-key-word-right lineedit-key-bol lineedit-key-eol\n"
       "    lineedit-key-break lineedit-key-ctrl-d lineedit-key-transpose-char\n"
       "    lineedit-key-del-left lineedit-key-del-right\n"
       "    lineedit-key-del-word-left lineedit-key-del-word-right\n"
       "    lineedit-key-del-line lineedit-key-del-line-left lineedit-key-del-line-right\n"
       "    lineedit-key-enter lineedit-key-newline-left lineedit-key-newline-right\n"
       "    lineedit-key-redraw lineedit-key-tab\n"
       "    lineedit-key-history-next lineedit-key-history-prev\n"
       "    lineedit-keytable-set! lineedit-keytable-find lineedit-read lineedit-flush)\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (only (chezscheme) " /*format*/ " fx1+ fx1- inspect void)\n"
       "    (schemesh bootstrap)\n"
       "    (schemesh containers)\n"
       "    (schemesh fd)\n"
       "    (schemesh lineedit base)\n"
       "    (schemesh tty))\n"
       "\n"
       "\n"
       /* linectx is the top-level object used by most lineedit functions */
       "(define-record-type\n"
       "  (linectx %make-linectx linectx?)\n"
       "  (fields\n"
       "    (mutable rbuf)\n"        /* bytespan, buffer for (fd-read)              */
       "    (mutable wbuf)\n"        /* bytespan, buffer for (fd-write)             */
       "    (mutable line)\n"        /* charline, input's current line being edited */
       "    (mutable lines)\n"       /* charlines, input being edited               */
       "    (mutable x)\n"           /* fixnum, cursor x position in line           */
       "    (mutable y)\n"           /* fixnum, cursor y position in lines          */
       "    (mutable match-x)\n"     /* fixnum, x position of matching parenthesis  */
       "    (mutable match-y)\n"     /* fixnum, y position of matching parenthesis  */
       "    (mutable rows)\n"        /* fixnum, max number of rows being edited     */
       "    (mutable prompt)\n"      /* charspan, prompt                            */
       "    (mutable prompt-func)\n" /* procedure, returns new prompt as charspan   */
       "    (mutable width)\n"       /* fixnum, terminal width                      */
       "    (mutable height)\n"      /* fixnum, terminal height                     */
       "    (mutable stdin)\n"       /* input file descriptor, or binary input port */
       "    (mutable stdout)\n"      /* output file descriptor, or binary output port */
       "    (mutable read-timeout-milliseconds)\n" /* -1 means unlimited timeout */
       "    (mutable return? linectx-return? linectx-return-set!)\n" /* bool */
       "    (mutable eof? linectx-eof? linectx-eof-set!)\n"          /* bool */
       "    (mutable keytable)\n"      /* hashtable, contains keybindings */
       "    (mutable history-index)\n" /* index of last used item in history */
       "    history))\n"               /* charhistory, history of entered commands */
       "\n"
       "(define lineedit-default-keytable (eq-hashtable))\n"
       "\n"
       /** prompt-func must be a procedure accepting a linectx and returning a charspan,
        * which will be used as prompt */
       "(define (make-linectx* prompt-func)\n"
       "  (let* ((sz    (tty-size))\n"
       "         (rbuf  (bytespan))\n"
       "         (wbuf  (bytespan))\n"
       "         (line  (charline))\n"
       "         (lines (charlines line)))\n"
       "    (bytespan-reserve-back! rbuf 1024)\n"
       "    (bytespan-reserve-back! wbuf 1024)\n"
       "    (%make-linectx\n"
       "      rbuf wbuf line lines\n"
       "      0 0 -1 -1 1\n"                 /* x y match-x match-y rows */
       "      #f prompt-func\n"              /* prompt prompt-func       */
       "      (if (pair? sz) (car sz) 80)\n" /* width                    */
       "      (if (pair? sz) (cdr sz) 24)\n" /* height                   */
       "      0 1 -1 #f #f \n"               /* stdin stdout read-timeout return? eof? */
       "      lineedit-default-keytable\n"   /* keytable */
       "      0 (charhistory))))\n"          /* history  */
       "\n"
       "(define make-linectx\n"
       "  (case-lambda\n"
       "    (()\n"
       "      (let ((prompt (string->charspan* \"$ \")))\n"
       "        (make-linectx* (lambda (ctx) prompt))))\n"
       "    ((prompt-func)\n"
       "      (make-linectx* prompt-func))))\n"
       "\n"
       /* also recreate line and lines: they have been saved to history, which retains them
        * also update prompt */
       "(define (linectx-clear! ctx)\n"
       "  (linectx-x-set! ctx 0)\n"
       "  (linectx-y-set! ctx 0)\n"
       "  (linectx-match-x-set! ctx -1)\n"
       "  (linectx-match-y-set! ctx -1)\n"
       "  (linectx-return-set!  ctx #f)\n"
       "  (let ((line (charline)))\n"
       "    (linectx-line-set!  ctx line)\n"
       "    (linectx-lines-set! ctx (charlines line))))\n"
       "\n"
       /* write a byte to wbuf */
       "(define (linectx-u8-write ctx u8)\n"
       "  (bytespan-u8-insert-back! (linectx-wbuf ctx) u8))\n"
       "\n"
       /* write a portion of given bytevector to wbuf */
       "(define (linectx-bv-write ctx bv start end)\n"
       "  (bytespan-bv-insert-back! (linectx-wbuf ctx) bv start end))\n"
       "\n"
       /* write a portion of given chargspan to wbuf */
       "(define (linectx-csp-write ctx csp start end)\n"
       "  (do ((wbuf (linectx-wbuf ctx))\n"
       "       (pos start (fx1+ pos)))\n"
       "      ((fx>=? pos end))\n"
       "    (bytespan-utf8-insert-back! wbuf (charspan-ref csp pos))))\n"
       "\n"
       /* write a portion of given chargbuffer to wbuf */
       "(define (linectx-cgb-write ctx cgb start end)\n"
       "  (do ((wbuf (linectx-wbuf ctx))\n"
       "       (pos start (fx1+ pos)))\n"
       "      ((fx>=? pos end))\n"
       "    (bytespan-utf8-insert-back! wbuf (chargbuffer-ref cgb pos))))\n"
       "\n"
       /** return a copy-on-write clone of current lines being edited */
       "(define (linectx-lines-copy ctx)\n"
       /* "  (format #t \"linectx-lines-copy~%\")" */
       /* "  (dynamic-wind tty-restore! break tty-setraw!)\n" */
       "  (charlines-copy-on-write (linectx-lines ctx)))\n"
       "\n"
       /** save current linectx-lines to history, and return them */
       "(define (linectx-to-history ctx)\n"
       /**  TODO: do not insert duplicates in history */
       "  (let ((lines (linectx-lines ctx)))\n"
       "    (charhistory-set! (linectx-history ctx) (linectx-history-index ctx) lines)\n"
       "    lines))\n"
       "\n"
       /* send escape sequence "move cursor up by n", without checking or updating linectx */
       "(define (term-move-up-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 65) 0 3))\n" /* ESC [ A */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n"  /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n" /* n     */
       "        (bytespan-u8-insert-back! wbuf 65)))))"   /* A     */
       "\n"
       /* send escape sequence "move cursor up by n", without checking or updating linectx */
       "(define (term-move-down-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 66) 0 3))\n" /* ESC [ B */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n"  /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n" /* n     */
       "        (bytespan-u8-insert-back! wbuf 66)))))"   /* B     */
       "\n"
       /* send escape sequence "move cursor left by n", without checking or updating linectx-x */
       "(define (term-move-left-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 68) 0 3))\n" /* ESC [ D */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n"  /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n" /* n     */
       "        (bytespan-u8-insert-back! wbuf 68)))))"   /* D     */
       "\n"
       /* send escape sequence "move cursor right by n", without checking or updating linectx */
       "(define (term-move-right-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 67) 0 3))\n" /* ESC [ C */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n"  /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n" /* n     */
       "        (bytespan-u8-insert-back! wbuf 67)))))"   /* C     */
       "\n"
       /* send escape sequence "delete n chars at right", without checking or updating linectx */
       "(define (term-del-right-n ctx n)\n"
       "  (cond\n"
       "    ((fx<=? n 0) (void))\n" /* nop */
       "    ((fx=? n 1)\n"
       "      (linectx-bv-write ctx #vu8(27 91 80) 0 3))\n" /* VT102 sequence: ESC [ P */
       "    (#t\n"
       "      (let ((wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-u8-insert-back! wbuf 27 91)\n"  /* ESC [ */
       "        (bytespan-fixnum-display-back! wbuf n)\n" /* n     */
       "        (bytespan-u8-insert-back! wbuf 80)))))"   /* P     */
       "\n"
       /* send escape sequence "move to begin-of-line". Moves at beginning of prompt! */
       "(define (term-move-to-bol ctx)\n"
       "  (linectx-u8-write ctx 13))\n" /* CTRL+M i.e. '\r' */
       "\n"
       /* send escape sequence "clear from cursor to end-of-line" */
       "(define (term-clear-to-eol ctx)\n"
       "  (linectx-bv-write ctx #vu8(27 91 75) 0 3))\n" /* ESC [ K */
       "\n"
       /* clear-line-right must be either 'clear-line-right or 'dont-clear-line-right*/
       "(define (term-redraw-to-eol ctx clear-line-right)\n"
       "  (let* ((line (linectx-line ctx))\n"
       "         (beg  (linectx-x ctx))\n"
       "         (end  (charline-length line)))\n"
       "    (when (fx<? beg end)\n"
       "      (linectx-cgb-write ctx line beg end))\n"
       "    (when (eq? clear-line-right 'clear-line-right)\n"
       "      (term-clear-to-eol ctx))\n"
       "    (when (fx<? beg end)\n"
       "      (term-move-left-n ctx (fx- end beg)))))\n"
       "\n"
       /* return index of first character before position = end in line that satisfies (pred ch).
        * return -1 if no character before position = end in line satisfies (pred ch) */
       "(define (char-find-left line end pred)\n"
       "  (assert (fx<=? end (charline-length line)))\n"
       "  (do ((x (fx1- end) (fx1- x)))\n"
       "      ((or (fx<? x 0) (pred (charline-ref line x)))\n"
       "        x)))\n"
       "\n"
       /* return index of first character at position = start or later in line that satisfies
        * (pred ch). return (charline-length line) if no character at position = start or later in
        * line satisfies (pred ch) */
       "(define (char-find-right line start pred)\n"
       "  (assert (fx>=? start 0))\n"
       "  (do ((x start (fx1+ x))\n"
       "       (len (charline-length line)))\n"
       "      ((or (fx>=? x len) (pred (charline-ref line x)))\n"
       "        x)))\n"
       "\n"
       /* return index of beginning of word before position = end in line */
       "(define (word-find-begin-left line end)\n"
       "  (let* ((pos1 (fx1+ (char-find-left line end  (lambda (ch) (char>? ch #\\space)))))\n"
       "         (pos2 (fx1+ (char-find-left line pos1 (lambda (ch) (char<=? ch #\\space))))))\n"
       "    pos2))\n"
       "\n"
       /* return index of end of word at position = start or later in line */
       "(define (word-find-end-right line start)\n"
       "  (let* ((pos1 (char-find-right line start  (lambda (ch) (char>? ch #\\space))))\n"
       "         (pos2 (char-find-right line pos1 (lambda (ch) (char<=? ch #\\space)))))\n"
       "    pos2))\n"
       "\n"
       "(define (lineedit-flush ctx)\n"
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
       "  (let ((x (linectx-x ctx)))\n"
       "    (linectx-clear! ctx)\n"
       /*   do not use (term-move-to-bol), there will be a prompt at bol */
       "    (term-move-left-n ctx x))\n"
       "  (term-clear-to-eol ctx)\n"
       "  (lineedit-flush ctx))\n"
       "\n"
       /**
        * save current linectx-lines to history, then replace current lines with specified
        * charlines - which are retained, do NOT modify them after calling this function
        */
       "(define (lineedit-lines-set! ctx lines)\n"
       "  (assert-charlines? 'lineedit-lines-set! lines)\n"
       "  (linectx-to-history ctx)\n"
       "  (lineedit-clear! ctx)\n" /* leaves a single, empty line in lines */
       "  (when (charlines-empty? lines)\n"
       "    (set! lines (linectx-lines ctx)))\n"
       "  (charlines-iterate lines\n"
       "    (lambda (i line)\n"
       "      (linectx-cgb-write ctx line 0 (charline-length line))))\n"
       "  (let* ((lines-n (charlines-length lines))\n"
       "         (line    (charlines-ref lines (fx1- lines-n))))\n"
       "    (linectx-line-set!  ctx line)\n"
       "    (linectx-lines-set! ctx lines)\n"
       "    (linectx-x-set! ctx (charline-length line))\n"
       "    (linectx-y-set! ctx lines-n)))\n"
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
       "          (charline-insert-at! line x ch)\n"
       "          (bytespan-utf8-insert-back! wbuf ch)\n"
       "          (set! x (fx1+ x)))))\n"
       "    (linectx-x-set! ctx x)\n"
       "    (term-redraw-to-eol ctx 'dont-clear-line-right)\n"
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
       "      (term-move-left-n ctx 1))))\n"
       "\n"
       "(define (lineedit-key-right ctx)\n"
       "  (let ((x (linectx-x ctx)))\n"
       "    (when (fx<? x (charline-length (linectx-line ctx)))\n"
       "      (linectx-x-set! ctx (fx1+ x))\n"
       "      (term-move-right-n ctx 1))))\n"
       "\n"
       "(define (lineedit-key-up ctx)\n"
       /** TODO: multiline editing */
       "  (lineedit-navigate-history ctx -1))\n"
       "\n"
       "(define (lineedit-key-down ctx)\n"
       /** TODO: multiline editing */
       "  (lineedit-navigate-history ctx +1))\n"
       "\n"
       "(define (lineedit-key-word-left ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (pos   (word-find-begin-left (linectx-line ctx) x))\n"
       "         (move-n (fx- x pos)))\n"
       "    (when (fx>? move-n 0)\n"
       "      (linectx-x-set! ctx pos)\n"
       "      (term-move-left-n ctx move-n))))\n"
       "\n"
       "(define (lineedit-key-word-right ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (pos   (word-find-end-right (linectx-line ctx) x))\n"
       "         (move-n (fx- pos x)))\n"
       "    (when (fx>? move-n 0)\n"
       "      (linectx-x-set! ctx pos)\n"
       "      (term-move-right-n ctx move-n))))\n"
       "\n"
       "(define (lineedit-key-bol ctx)\n"
       "  (let ((x (linectx-x ctx)))\n"
       "    (when (fx>? x 0)\n"
       /*     do not use (term-move-to-bol), there will be a prompt at bol */
       "      (linectx-x-set! ctx 0)\n"
       "      (term-move-left-n ctx x))))\n"
       "\n"
       "(define (lineedit-key-eol ctx)\n"
       "  (let ((x    (linectx-x ctx))\n"
       "        (len  (charline-length (linectx-line ctx))))\n"
       "    (when (fx<? x len)\n"
       "      (linectx-x-set! ctx len)\n"
       "      (term-move-right-n ctx (fx- len x)))))\n"
       "\n"
       "(define (lineedit-key-break ctx)\n"
       "  (lineedit-clear! ctx))\n"
       "\n"
       "(define (lineedit-key-ctrl-d ctx)\n"
       "  (if (and (fx=? 0 (charline-length (linectx-line ctx)))\n"
       "           (fx=? 1 (charlines-length (linectx-lines ctx))))\n"
       "    (linectx-eof-set! ctx #t)\n"
       "    (lineedit-key-del-right ctx)))\n"
       "\n"
       "(define (lineedit-key-transpose-char ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (line (linectx-line ctx))\n"
       "         (len  (charline-length line)))\n"
       "    (when (and (fx>? x 0) (fx>? len 1))\n"
       "      (let ((eol (fx=? x len)))\n"
       "        (term-move-left-n ctx (if eol 2 1))\n"
       "        (when eol\n"
       "          (set! x (fx1- x))))\n"
       "      (let ((ch1  (charline-ref line (fx1- x)))\n"
       "            (ch2  (charline-ref line x))\n"
       "            (wbuf (linectx-wbuf ctx)))\n"
       "        (bytespan-utf8-insert-back! wbuf ch2)\n"
       "        (bytespan-utf8-insert-back! wbuf ch1)\n"
       "        (charline-set! line (fx1- x) ch2)\n"
       "        (charline-set! line x ch1)\n"
       "        (linectx-x-set! ctx (fx1+ x))))))\n"
       "\n"
       "(define (lineedit-key-del-left ctx)\n"
       "  (when (fx>? (linectx-x ctx) 0)\n"
       "    (lineedit-key-left ctx)\n"
       "    (lineedit-key-del-right ctx)))\n"
       "\n"
       "(define (lineedit-key-del-right ctx)\n"
       "  (let ((x    (linectx-x ctx))\n"
       "        (line (linectx-line ctx)))\n"
       "    (when (fx<? x (charline-length line))\n"
       "      (charline-erase-at! line x 1)\n"
       "      (term-del-right-n ctx 1))))\n"
       "\n"
       "(define (lineedit-key-del-word-left ctx)\n"
       "  (let* ((x     (linectx-x ctx))\n"
       "         (line  (linectx-line ctx))\n"
       "         (pos   (word-find-begin-left line x))\n"
       "         (del-n (fx- x pos)))\n"
       "    (when (fx>? del-n 0)\n"
       "      (charline-erase-at! line pos del-n)\n"
       "      (linectx-x-set! ctx pos)\n"
       "      (term-move-left-n ctx del-n)\n"
       "      (term-del-right-n ctx del-n))))\n"
       "\n"
       "(define (lineedit-key-del-word-right ctx)\n"
       "  (let* ((x     (linectx-x ctx))\n"
       "         (line  (linectx-line ctx))\n"
       "         (pos   (word-find-end-right line x))\n"
       "         (del-n (fx- pos x)))\n"
       "    (when (fx>? del-n 0)\n"
       "      (charline-erase-at! line x del-n)\n"
       "      (term-del-right-n ctx del-n))))\n"
       "\n"
       "(define (lineedit-key-del-line ctx)\n"
       "  (void))\n"
       "\n"
       "(define (lineedit-key-del-line-left ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (line (linectx-line ctx))\n"
       "         (len  (charline-length line)))\n"
       "    (when (and (fx>? x 0) (fx>? len 0))\n"
       "      (charline-erase-at! line 0 x)\n"
       "      (linectx-x-set! ctx 0)\n"
       "      (term-move-left-n ctx x)\n"
       "      (term-redraw-to-eol ctx 'clear-line-right))))\n"
       "\n"
       "(define (lineedit-key-del-line-right ctx)\n"
       "  (let* ((x    (linectx-x ctx))\n"
       "         (line (linectx-line ctx))\n"
       "         (len  (charline-length line)))\n"
       "    (when (fx<? x len)\n"
       "      (charline-erase-at! line x (fx- len x))\n"
       "      (term-clear-to-eol ctx))))\n"
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
       "  (lineedit-navigate-history ctx +1))\n"
       "\n"
       "(define (lineedit-key-history-prev ctx)\n"
       "  (lineedit-navigate-history ctx -1))\n"
       "\n"
       "(define (lineedit-key-redraw ctx)\n"
       "  (let* ((lines (linectx-lines ctx))\n"
       "         (lines-n-1 (fx1- (charlines-length lines)))\n"
       "         (prompt (linectx-prompt ctx))\n"
       "         (x (linectx-x ctx))\n"
       "         (y (linectx-y ctx))\n"
       "         (nl? #f))\n"
       /* " (format #t \"lineedit-key-redraw: prompt = ~s~%\" prompt)\n" */
       "    (term-move-to-bol ctx)\n"
       "    (term-move-up-n ctx y)\n"
       "    (linectx-csp-write ctx prompt 0 (charspan-length prompt))\n"
       "    (charlines-iterate lines\n"
       "      (lambda (i line)\n"
       "        (when nl?\n"
       "          (linectx-u8-write ctx 10))\n"
       "        (linectx-cgb-write ctx line 0 (charline-length line))\n"
       "        (term-clear-to-eol ctx)\n"
       "        (set! nl? #t)))\n"
       "    (term-move-up-n ctx (fx- lines-n-1 y))\n"
       "    (let* ((last-line (charlines-ref lines lines-n-1))\n"
       "           (delta-x   (fx- x (charline-length last-line))))\n"
       "      (when (and (fxzero? y) (not (fxzero? lines-n-1)))\n"
       "        (set! delta-x (fx+ delta-x (charspan-length prompt))))\n"
       "      (if (fx<? delta-x 0)\n"
       "        (term-move-left-n ctx (fx- delta-x))\n"
       "        (term-move-right-n ctx delta-x)))))\n"
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
       "(define (lineedit-navigate-history ctx delta-y)\n"
       "  (let ((y    (fx+ delta-y (linectx-history-index ctx)))\n"
       "        (hist (linectx-history ctx)))\n"
       /**  TODO: when delta-y < 0, move cursor to end of first line */
       "    (when (fx<? -1 y (charhistory-length hist))\n"
       /*     also saves a copy of current linectx-lines to history */
       "      (lineedit-lines-set! ctx (charhistory-cow-ref hist y))\n"
       "      (linectx-history-index-set! ctx y))))\n"
       "\n"
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
        * append linectx-lines to history, and return them.
        * the returned charlines MUST NOT be modified, not even temporarily,
        * because linectx-history still references it.
        */
       "(define (linectx-return-lines ctx)\n"
       "  (linectx-return-set! ctx #f)\n" /* clear flag "user pressed ENTER" */
       "  (linectx-prompt-set! ctx #f)\n" /* set flag "update prompt" */
       "  (let* ((y (linectx-history-index ctx))\n"
       "         (hist (linectx-history ctx))\n"
       "         (hist-len (charhistory-length hist)))\n"
       /*   always overwrite last history slot */
       "    (linectx-history-index-set! ctx (fxmax 0 y (fx1- hist-len)))\n"
       "    (let* ((lines (linectx-to-history ctx))\n"
       "           (empty-line (charline)))\n"
       "      (linectx-history-index-set! ctx (charhistory-length hist))\n"
       /*     lines are referenced by history - allocate new ones */
       "      (linectx-line-set! ctx empty-line)\n"
       "      (linectx-lines-set! ctx (charlines empty-line))\n"
       "      (linectx-x-set! ctx 0)\n"
       "      (linectx-y-set! ctx 0)\n"
       "      lines)))\n"
       "\n"
       /**
        * repeatedly call (linectx-keytable-call) until ENTER is found and processed,
        * or until no more keytable matches are found.
        * if user pressed ENTER, return a reference to internal charlines (linectx-lines)
        * if waiting for more keypresses, return #t
        * if got end-of-file, return #f
        */
       "(define (linectx-keytable-iterate ctx)\n"
       "  (do ()\n"
       "      ((or (linectx-return? ctx)\n"
       "           (linectx-eof? ctx)\n"
       "           (bytespan-empty? (linectx-rbuf ctx))\n"
       "           (fxzero? (linectx-keytable-call ctx)))))\n"
       "  (lineedit-flush ctx)\n"
       "  (cond\n"
       "    ((linectx-return? ctx) (linectx-return-lines ctx))\n"
       "    ((linectx-eof?    ctx) #f)\n"
       "    (#t                    #t)))\n"
       "\n"
       /**
        * read some bytes, blocking at most for read-timeout-milliseconds
        *   (0 = non-blocking, -1 = unlimited timeout)
        * from (linectx-stdin ctx) and append them to (linectx-rbuf ctx).
        * return number of read bytes.
        * return 0 on timeout
        * return -1 on eof
        */
       "(define (linectx-read ctx read-timeout-milliseconds)\n"
       "  (let* ((rbuf (linectx-rbuf ctx))\n"
       "         (rlen (bytespan-length rbuf))\n"
       "         (max-n 1024)\n"
       "         (stdin (linectx-stdin ctx))\n"
       "         (got 0)\n"
       "         (eof? #f))\n"
       /*   ensure bytespan-capacity-back is large enough */
       "    (bytespan-reserve-back! rbuf (fx+ rlen max-n))\n"
       "    (if (fixnum? stdin)\n"
       /*     stdin is a file descriptor -> call (fd-select) then (fd-read) */
       "      (when (eq? 'read (fd-select stdin 'read read-timeout-milliseconds))\n"
       "        (set! got (fd-read stdin (bytespan-peek-data rbuf)\n"
       "                     (bytespan-peek-end rbuf) max-n))\n"
       /*       (fxzero? got) means end of file */
       "        (set! eof? (fxzero? got)))\n"
       /*     stdin is a binary input port -> call (get-bytevector-n!) */
       "      (let ((n (get-bytevector-n! stdin (bytespan-peek-data rbuf)\n"
       "                                  (bytespan-peek-end rbuf) max-n)))\n"
       "        (when (fixnum? n)\n"
       "          (set! got n)\n"
       /*         (fxzero? n) means end of file */
       "          (set! eof? (fxzero? n)))))\n"
       "    (assert (fx>=? got 0))\n"
       "    (bytespan-resize-back! rbuf (fx+ rlen got))\n"
       "    (assert (fixnum? got))\n"
       "    (assert (fx<=? 0 got max-n))\n"
       "    (if eof? -1 got)))\n"
       "\n"
       /**
        * if user pressed ENTER, return a reference to internal linectx-lines.
        * if waiting for more keypresses, return #t
        * if got end-of-file, return #f
        */
       "(define (lineedit-read ctx timeout-milliseconds)\n"
       "  (assert (linectx? ctx))\n"
       "  (flush-output-port (current-output-port))\n"
       /* update prompt if needed */
       "  (unless (charspan? (linectx-prompt ctx))\n"
       "    (linectx-prompt-set! ctx ((linectx-prompt-func ctx) ctx))\n"
       "    (lineedit-key-redraw ctx)\n"
       "    (lineedit-flush ctx))\n"
       "  (let ((ret (if (bytespan-empty? (linectx-rbuf ctx))\n"
       "               #t\n" /* need more input */
       /*              some bytes already in rbuf, try to consume them */
       "               (linectx-keytable-iterate ctx))))\n"
       "    (if (eq? #t ret)\n"
       /*     need more input */
       "      (let ((n (linectx-read ctx timeout-milliseconds)))\n"
       "        (cond\n"
       /*         got some bytes, call again (linectx-keytable-iterate) and return its value */
       "          ((fx>? n 0)  (linectx-keytable-iterate ctx))\n"
       "          ((fxzero? n) #t)\n"   /* read timed out, return #t */
       "          (#t          #f)))\n" /* end-of-file, return #f  */
       /*     propagate return value of first (linectx-keytable-iterate) */
       "      ret)))\n"
       "\n"
       "(let ((t lineedit-default-keytable)\n"
       "      (%add lineedit-keytable-set!))\n"
       "(%add t lineedit-key-bol 1)\n"             /* CTRL+A        */
       "(%add t lineedit-key-left 2)\n"            /* CTRL+B        */
       "(%add t lineedit-key-break 3)\n"           /* CTRL+C        */
       "(%add t lineedit-key-ctrl-d 4)\n"          /* CTRL+D        */
       "(%add t lineedit-key-eol 5)\n"             /* CTRL+E        */
       "(%add t lineedit-key-right 6)\n"           /* CTRL+F        */
       "(%add t lineedit-key-del-left 8 127)\n"    /* CTRL+H or BACKSPACE */
       "(%add t lineedit-key-tab 9)\n"             /* CTRL+I or TAB */
       "(%add t lineedit-key-enter 10 13)\n"       /* CTRL+J or ENTER, CTRL+M */
       "(%add t lineedit-key-del-line-right 11)\n" /* CTRL+K        */
       "(%add t lineedit-key-redraw 12)\n"         /* CTRL+L        */
       "(%add t lineedit-key-history-next 14)\n"   /* CTRL+N        */
       "(%add t lineedit-key-newline-right 15)\n"  /* CTRL+O        */
       "(%add t lineedit-key-history-prev 16)\n"   /* CTRL+P        */
       "(%add t lineedit-key-transpose-char 20)\n" /* CTRL+T        */
       "(%add t lineedit-key-del-line-left 21)\n"  /* CTRL+U        */
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
       "(%add t lineedit-key-bol   '(27 91 72)\n"             /* HOME  \e[H  */
       /*                    */ "  '(27 91 49 126))\n"        /* HOME  \e[1~ */
       "(%add t lineedit-key-history-prev '(27 91 53 126))\n" /* PGUP  \e[5~ */
       "(%add t lineedit-key-history-next '(27 91 54 126))\n" /* PGDWN \e[6~ */
       /* */
       "(%add t lineedit-key-nop   '(27 91 91 65) '(27 91 91 66)\n"      /* F1..F2  */
       "    '(27 91 91 67) '(27 91 91 68) '(27 91 91 69)\n"              /* F3..F4  */
       "    '(27 91 49 53 126) '(27 91 49 55 126) '(27 91 49 56 126)\n"  /* F4..F7  */
       "    '(27 91 49 57 126) '(27 91 50 48 126) '(27 91 50 49 126)\n"  /* F8..F10 */
       "    '(27 91 50 50 126) '(27 91 50 51 126) '(27 91 50 52 126))\n" /* F?..F12 */
#if 1
       "(%add t lineedit-key-toggle-insert '(27 91 50 126))\n" /* INSERT \e[2~ */
#endif
       "(%add t lineedit-key-del-right '(27 91 51 126))\n" /* DELETE \e[3~ */
       "(%add t lineedit-key-eol   '(27 91 52 126))\n"     /* END    \e[4~ */
       /**/
       ")\n"   /* close let */
       ")\n"); /* close library */
}
