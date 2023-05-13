/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "eval.h"

void define_library_io(void) {

#define SCHEMESH_LIBRARY_IO_EXPORT                                                                 \
  "open-chargbuffer-input-port open-gbuffer-of-chargbuffers-input-port "

  eval("(library (schemesh io (0 1))\n"
       "  (export " SCHEMESH_LIBRARY_IO_EXPORT ")\n"
       "  (import\n"
       "    (rnrs)\n"
       "    (rnrs mutable-strings)\n"
       "    (only (chezscheme) fx1+)\n"
       "    (schemesh bootstrap)\n" /* until */
       "    (schemesh containers))\n"
       "\n"
       /* helper for input port wrapping a chargbuffer */
       "(define-record-type icport\n"
       "  (fields\n"
       "    (immutable source)\n" /* chargbuffer to read from */
       "    (mutable   pos))\n"   /* position in chargbuffer  */
       "  (nongenerative #{icport do8t0druatc9fhaize8s4a1wd-20}))\n"
       "\n"
       "(define (icport-read-string p str start n)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (string-length str)))\n"
       "  (let* ((source     (icport-source p))\n"
       "         (source-pos (icport-pos p))\n"
       "         (source-len (chargbuffer-length source))\n"
       "         (ret-n  (fxmin n (fx- source-len source-pos))))\n"
       "    (if (fx<=? ret-n 0)\n"
       "      0\n"
       "      (do ((i 0 (fx1+ i)))\n"
       "           ((fx>=? i ret-n)\n"
       "             (icport-pos-set! p (fx+ ret-n source-pos))\n"
       "             ret-n)\n"
       "        (string-set! str (fx+ i start) (chargbuffer-ref source (fx+ i source-pos)))))))\n"
       "\n"
       "(define icport-position icport-pos)\n"
       "\n"
       "(define (icport-position-set! p pos)\n"
       "  (unless (and (fixnum? pos)\n"
       "               (fx>=? pos 0)\n"
       "               (fx<=? pos (chargbuffer-length (icport-source p))))\n"
       "    (raise (make-i/o-invalid-position-error pos)))\n"
       "  (icport-pos-set! p pos))\n"
       "\n"
       /* create an input port wrapping a chargbuffer */
       "(define (open-chargbuffer-input-port cgb)\n"
       "  (assert (chargbuffer? cgb))\n"
       "  (let ((p (make-icport cgb 0)))\n"
       "    (make-custom-textual-input-port\n"
       "      \"chargbuffer-input-port\"\n"
       "      (lambda (str start n) (icport-read-string p str start n))\n"
       "      (lambda ()            (icport-position     p))\n"
       "      (lambda (pos)         (icport-position-set! p pos))\n"
       "      #f)))\n" /* nothing to do on (close-input-port) */
       "\n"
       "\n"
       /* helper for input port wrapping a gbuffer of chargbuffers */
       "(define-record-type iport\n"
       "  (fields\n"
       "    (mutable   x)\n"        /* position in y-th chargbuffer */
       "    (mutable   y)\n"        /* position in gbuffer */
       "    (immutable sources))\n" /* gbuffer of chargbuffers to read from */
       "  (nongenerative #{iport cy8auoivds3jpsu99eergcoco-20}))\n"
       "\n"
       "(define (iport-read-char p)\n"
       "  (let* ((x (iport-x p))\n"
       "         (y (iport-y p))\n"
       "         (sources (iport-sources p))\n"
       "         (source  (gbuffer-ref sources y)))\n"
       "    (cond\n"
       "      ((fx<? x (chargbuffer-length source))\n"
       "        (iport-x-set! p (fx1+ x))\n"
       "        (chargbuffer-ref source x))\n"
       "      ((fx<? (fx1+ y) (gbuffer-length sources))\n"
       /*       end-of-line reached, go to next line */
       "        (iport-x-set! p 0)\n"
       "        (iport-y-set! p (fx1+ y))\n"
       "        (iport-read-char p))\n"
       "      (#t #f))))" /* end-of-file reached */
       "\n"
       "(define (iport-read-string p str start n)\n"
       "  (assert (fx>=? start 0))\n"
       "  (assert (fx>=? n 0))\n"
       "  (assert (fx<=? (fx+ start n) (string-length str)))\n"
       "  (let ((i 0)\n"
       "        (eof #f))\n"
       "    (until (or (fx>=? i n) eof)\n"
       "      (let ((ch (iport-read-char p)))\n"
       "        (if (char? ch)\n"
       "          (begin\n"
       "            (string-set! str (fx+ i start) ch)\n"
       "            (set! i (fx1+ i)))\n"
       "          (set! eof #t))))\n"
       "    i))\n"
       "\n"
       "(define (iport-position p)\n"
       "  (cons (iport-x p) (iport-y p)))\n"
       "\n"
       "(define (iport-position-set! p pos)\n"
       "  (unless (and (pair? pos) (fixnum? (car pos)) (fixnum? (cdr pos)))\n"
       "    (raise (make-i/o-invalid-position-error pos)))\n"
       "  (let ((y (cdr pos))\n"
       "        (sources (iport-sources p)))\n"
       "    (assert (fx>=? y 0))\n"
       "    (assert (fx<? y (gbuffer-length sources)))\n"
       "    (let ((x (car pos))\n"
       "          (source (gbuffer-ref sources y)))\n"
       "      (assert (fx>=? x 0))\n"
       "      (assert (fx<=? x (chargbuffer-length source)))\n"
       "      (iport-x-set! p x)\n"
       "      (iport-y-set! p y))))\n"
       "\n"
       /* create an input port wrapping a gbuffer containing chargbuffers */
       "(define open-gbuffer-of-chargbuffers-input-port\n"
       "  (let ((empty-gb (gbuffer (chargbuffer))))\n"
       "    (lambda (gb)\n"
       "      (gbuffer-iterate gb\n"
       "        (lambda (i elem)\n"
       "          (assert (chargbuffer? elem))))\n"
       "      (let ((p (make-iport 0 0 (if (gbuffer-empty? gb) empty-gb gb))))\n"
       "        (make-custom-textual-input-port\n"
       "          \"gbuffer-input-port\"\n"
       "          (lambda (str start n) (iport-read-string p str start n))\n"
       "          (lambda ()            (iport-position     p))\n"
       "          (lambda (pos)         (iport-position-set! p pos))\n"
       "          #f)))))\n" /* nothing to do on (close-input-port) */
       "\n"
       ")\n"); /* close library */
}
