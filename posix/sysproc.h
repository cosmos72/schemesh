/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <stddef.h> /* size_t */
#include <stdint.h> /* in64_t, uint64_t */
#include <stdio.h>

static void   skip_ws(FILE* file);
static size_t read_char(FILE* file, char* ret);
static size_t read_uint64(FILE* file, uint64_t* ret);
static size_t read_int64(FILE* file, int64_t* ret);
static size_t read_string(FILE* file, char* str, size_t maxlen);

#include "sysproc_linux.h"

static void skip_ws(FILE* file) {
  int ch;
  while ((ch = fgetc(file)) != EOF && ch <= ' ') {
  }
  if (ch != EOF) {
    ungetc(ch, file);
  }
}

/**
 * skip whitespace, get next char and store it in *ret
 * @return 1 if char was read, or 0 on EOF.
 * if char could not be read, store '\0' in *ret.
 */
static size_t read_char(FILE* file, char* ret) {
  int ch;
  skip_ws(file);
  if ((ch = fgetc(file)) == EOF) {
    if (ret) {
      *ret = '\0';
    }
    return 0;
  }
  if (ret) {
    *ret = (char)ch;
  }
  return 1;
}

/**
 * skip whitespace, parse decimal digits, convert them to an uint64_t and store it in *ret.
 * @return number of parsed decimal digits.
 * if no digits could be parsed, store 0 in *ret.
 */
static size_t read_uint64(FILE* file, uint64_t* ret) {
  uint64_t val;
  size_t   digits;
  int      ch;
  skip_ws(file);

  val    = 0;
  digits = 0;
  while ((ch = fgetc(file)) != EOF && ch >= '0' && ch <= '9') {
    val = val * 10 + (unsigned)(ch - '0');
    digits++;
  }
  if (ret) {
    *ret = val;
  }
  return digits;
}

/**
 * skip whitespace, parse optional sign and decimal digits, and convert them to an int64_t,
 * and store it in *ret.
 * @return number of parsed decimal digits.
 * if no digits could be parsed, store 0 in *ret.
 */
static size_t read_int64(FILE* file, int64_t* ret) {
  uint64_t val;
  size_t   digits;
  int      negative;
  char     ch;
  if (!read_char(file, &ch)) {
    return 0;
  }
  negative = (ch == '-');
  if (ch != '+' && ch != '-') {
    ungetc(ch, file);
  }
  val = 0;
  while ((ch = fgetc(file)) != EOF && ch >= '0' && ch <= '9') {
    val = val * 10 + (unsigned)(ch - '0');
    digits++;
  }
  if (ret) {
    *ret = negative ? -(int64_t)val : (int64_t)val;
  }
  return digits;
}

/**
 * skip whitespace, copy up to maxlen-1 non-whitespace characters to str.
 * always add '\0' terminator to str.
 * @return number of copied characters.
 */
static size_t read_string(FILE* file, char* str, size_t maxlen) {
  size_t len = 0;
  int    ch;
  skip_ws(file);
  while (len + 1 < maxlen && (ch = fgetc(file)) != EOF && ch > ' ') {
    if (str) {
      *str++ = (char)ch;
    }
    len++;
  }
  if (str) {
    *str = '\0';
  }
  return len;
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <pid|self>\n", argv[0]);
    return 1;
  }
  print_ps_line(argv[1]);
  return 0;
}
