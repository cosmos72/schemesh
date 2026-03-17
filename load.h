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

#include "eval.h"

#define LIBSCHEMESH_SO "libschemesh_1.0.0.so"

#define STR_(arg) #arg
#define STR(arg) STR_(arg)

#ifdef SCHEMESH_STATIC

#include <signal.h>   /* sigprocmask() */
#include <stddef.h>   /* size_t        */
#include <stdint.h>   /* uintptr_t     */
#include <string.h>   /* memcpy()      */
#include <sys/mman.h> /* munmap()      */
#include <unistd.h>   /* sysconf()     */

/* clang-format off */
#define EMBED_FILE(sym, filepath)         \
  __asm__(".section .rodata\n"            \
          ".balign 4096\n"                \
          ".global " STR(sym) "_start\n"  \
          STR(sym) "_start:\n"            \
          ".incbin \"" filepath "\"\n"    \
          ".global " STR(sym) "_end\n"    \
          STR(sym) "_end:\n"              \
          ".balign 4096\n"                \
          ".section \".text\"\n")
/* clang-format on */

#ifndef CHEZ_SCHEME_DIR
#error "please #define CHEZ_SCHEME_DIR to the installation path of Chez Scheme"
#endif

EMBED_FILE(petite_boot, STR(CHEZ_SCHEME_DIR) "/petite.boot");
EMBED_FILE(scheme_boot, STR(CHEZ_SCHEME_DIR) "/scheme.boot");

EMBED_FILE(libschemesh_so, "./" LIBSCHEMESH_SO);

extern const char petite_boot_start[], petite_boot_end[];
extern const char scheme_boot_start[], scheme_boot_end[];
extern const char libschemesh_so_start[], libschemesh_so_end[];

static void schemesh_signals_unblock(void) {
  sigset_t set;
  if (sigemptyset(&set) == 0) {
    (void)sigprocmask(SIG_SETMASK, &set, NULL);
  }
}

static size_t schemesh_pagesize(void) {
  static size_t os_pagesize = 0; /* OS page size, in bytes */

  size_t n = os_pagesize;
  if (n == 0) {
#ifdef _SC_PAGESIZE
    long val = sysconf(_SC_PAGESIZE);
    if (val > 0) {
      n = (size_t)val;
    } else
#endif

    /*else */ {
#if defined(PAGESIZE) && PAGESIZE > 0
      n = PAGESIZE;
#else
      n = 4096; /* guess */
#endif
    }
    os_pagesize = n; /* cache for future calls */
  }
  return n;
}

static void schemesh_unmap(const char* start, const char* end) {
  const size_t pagesize = schemesh_pagesize();

  start = (const char*)(((uintptr_t)start + pagesize - 1) / pagesize * pagesize);
  end   = (const char*)(((uintptr_t)end + 4095) & ~(uintptr_t)4095);
  end   = (const char*)(((uintptr_t)end / pagesize * pagesize));
  munmap((void*)start, end - start);
}

/** initialize Chez Scheme using embedded boot files */
static void schemesh_init(const char* override_boot_dir, void (*on_scheme_exception)(void)) {
  (void)override_boot_dir;
  schemesh_signals_unblock();

  Sscheme_init(on_scheme_exception);
  Sregister_boot_file_bytes(
      "petite.boot", (void*)petite_boot_start, (iptr)(petite_boot_end - petite_boot_start));
  Sregister_boot_file_bytes(
      "scheme.boot", (void*)scheme_boot_start, (iptr)(scheme_boot_end - scheme_boot_start));
  Sbuild_heap(NULL, NULL);
}

/**
 * Load embedded libschemesh library.
 * @return 0 if successful,
 * otherwise print error message to (current-error-port) and return < 0
 */
static int schemesh_load_library(const char* override_library_dir) {
  ptr func_load = scheme2k_eval /*                       */
      ("(lambda (bv)\n"
       "  (call/cc\n"
       "    (lambda (k-exit)\n"
       "      (with-exception-handler\n"
       "        (lambda (ex)\n"
       "          (let ((port (current-error-port)))\n"
       "            (put-string port \"schemesh: \")"
       "            (display-condition ex port)\n"
       "            (newline port)\n"
       "            (flush-output-port port))\n"
       "          (k-exit #f))\n" /* exception -> return #f */
       "        (lambda ()\n"
       "          (load-compiled-from-port\n"
       "            (open-bytevector-input-port bv))\n"
       "          (collect (collect-maximum-generation))\n"
       "          #t)))))\n"); /* success -> return #t */
  iptr size = libschemesh_so_end - libschemesh_so_start;
  ptr  bv   = Smake_bytevector(size, 0);
  int  ret;

  (void)override_library_dir;

  memcpy(Sbytevector_data(bv), libschemesh_so_start, size);
  ret = Scall1(func_load, bv) ? 0 : -1;

  schemesh_unmap(petite_boot_start, petite_boot_end);
  schemesh_unmap(scheme_boot_start, scheme_boot_end);
  schemesh_unmap(libschemesh_so_start, libschemesh_so_end);
  return ret;
}

#else

#ifndef SCHEMESH_DIR
#error "please #define SCHEMESH_DIR to the desired installation path of schemesh"
#endif

/** initialize Chez Scheme. */
static void schemesh_init(const char* override_boot_dir, void (*on_scheme_exception)(void)) {
  extern void scheme2k_init(const char* override_boot_dir, void (*on_scheme_exception)(void));

  scheme2k_init(override_boot_dir, on_scheme_exception);
}

/**
 * load libschemesh_VERSION.so
 * @return 0 if successful, otherwise error code
 */
static int schemesh_load_library(const char* override_library_dir) {
  const char* filename = LIBSCHEMESH_SO;
  int         err      = -1;

  if (override_library_dir != NULL) {
    err = scheme2k_load_library(override_library_dir, filename);
  } else {
    err = scheme2k_load_library(STR(SCHEMESH_DIR), filename);
    if (err != 0) {
      err = scheme2k_load_library("/usr/local/lib/schemesh", filename);
    }
    if (err != 0) {
      err = scheme2k_load_library("/usr/lib/schemesh", filename);
    }
  }
  return err;
}

#endif

static void schemesh_import_all_libraries(void) {
  scheme2k_eval("(import (schemesh))");
}
