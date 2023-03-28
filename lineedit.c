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
       "  (lineedit %make-lineedit lineedit?)\n"
       "  (fields\n"
       "    (mutable rbuf)\n"       /* bytearray */
       "    (mutable wbuf)\n"       /* bytearray */
       "    (mutable x)\n"          /* fixnum, cursor x position */
       "    (mutable y)\n"          /* fixnum, cursor y position */
       "    (mutable save-x)\n"     /* fixnum, saved cursor x position */
       "    (mutable save-y)\n"     /* fixnum, saved cursor y position */
       "    (mutable rows)\n"       /* fixnum, multiline: max number of rows being edited */
       "    (mutable width)\n"      /* fixnum, terminal width */
       "    (mutable height)))\n"); /* fixnum, terminal height */

  eval("(define (make-lineedit)\n"
       "  (let ((sz (tty-size)))\n"
       "    (%make-lineedit\n"
       "      (make-bytearray)\n"                 /* rbuf */
       "      (make-bytearray)\n"                 /* wbuf */
       "      -1 -1 -1 -1 +1\n"                   /* x y save-x save-y rows */
       "      (if (pair? sz) (car sz) 80)\n"      /* width */
       "      (if (pair? sz) (cdr sz) 24))))\n"); /* height */

  eval("(define (sh-lineedit ctx)\n"
       "  (assert (lineedit? ctx))\n"
       "  )\n");
}
