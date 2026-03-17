/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

/**
 * this file contains helper functions to load schemesh library
 * and import its definitions.
 */

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

#define CAT_(a,b) a##b
#define CAT(a,b) CAT_(a,b)

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

#define LIBSCHEMESH_SO libschemesh_1.0.0.so

#ifdef __APPLE__
#define SYM_(a,b) _##a##b
#define SYM(a,b)  SYM_(a,b)
.section __TEXT,__const
#else
#define SYM_(a,b) a##b
#define SYM(a,b)  SYM_(a,b)
.section .rodata
#endif

/* clang-format off */
#define EMBED_FILE(sym, filepath) \
  .balign 4096                 ;  \
  .global SYM(sym, _start)     ;  \
  SYM(sym, _start):            ;  \
  .incbin filepath             ;  \
  .global SYM(sym, _end)       ;  \
  SYM(sym, _end):              ;  \
  .balign 4096		       ;
/* clang-format on */


EMBED_FILE(petite_boot, STR(CAT(CHEZ_SCHEME_DIR,/petite.boot)))

EMBED_FILE(scheme_boot, STR(CAT(CHEZ_SCHEME_DIR,/scheme.boot)))

EMBED_FILE(libschemesh_so, STR(CAT(./,LIBSCHEMESH_SO)))
