/**
 * Copyright (C) 2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_C_WRITER_H
#define SCHEME2K_C_WRITER_H

#include <inttypes.h> /* PRIi64, PRIu32 */
#include <stddef.h>   /* size_t */
#include <stdint.h>   /* int64_t, uint32_t */
#include <stdio.h>    /* FILE* */
#include <string.h>   /* memcpy() */

#define WBUF_MAX 65536

/* buffered writer */

typedef struct {
  FILE*  f;
  size_t pos;
  char   buf[WBUF_MAX];
} writer;

static void w_flush(writer* w) {
  fwrite(w->buf, 1, w->pos, w->f);
  w->pos = 0;
}

static void w_put_char(writer* w, char c) {
  if (w->pos >= WBUF_MAX) {
    w_flush(w);
  }
  w->buf[w->pos++] = c;
}

#define w_put_chars(w, s) w_put_chars3(w, s, sizeof(s) - 1)

static void w_put_chars3(writer* w, const char* s, const size_t n) {
  size_t i = 0;
  while (i < n) {
    size_t avail, copy_n;
    if (w->pos >= WBUF_MAX) {
      w_flush(w);
    }
    avail  = WBUF_MAX - w->pos;
    copy_n = n < avail ? n : avail;
    memcpy(&w->buf[w->pos], &s[i], copy_n);
    w->pos += copy_n;
    i += copy_n;
  }
}

static void w_put_int64(writer* w, int64_t num) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIi64, num);
  w_put_chars3(w, buf, len);
}

static void w_put_uint64(writer* w, uint64_t num) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIu64, num);
  w_put_chars3(w, buf, len);
}

static void w_put_fractional_digits(writer* w, uint64_t fraction, int digit_n) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIu64, fraction);
  int  i;
  for (i = len; i < digit_n; i++) {
    w_put_char(w, '0');
  }
  w_put_chars3(w, buf, len);
}

static void w_put_double(writer* w, double num) {
  char buf[32];
  int  len = snprintf(buf, sizeof(buf), "%g", num);
  w_put_chars3(w, buf, len);
}

/* JSON escape */

#define w_put_string_quoted_json_escape(w, s)                                                      \
  w_put_string_quoted_json_escape3(w, (const unsigned char*)(s), (size_t)-1)

static void w_put_string_quoted_json_escape3(writer* w, const unsigned char* s, size_t len) {
  size_t        i;
  unsigned char ch;
  w_put_char(w, '"');
  for (i = 0; i < len && (ch = s[i]); i++) {
    switch (ch) {
      case '"':
        w_put_chars(w, "\\\"");
        break;
      case '\\':
        w_put_chars(w, "\\\\");
        break;
      case '\b':
        w_put_chars(w, "\\b");
        break;
      case '\f':
        w_put_chars(w, "\\f");
        break;
      case '\n':
        w_put_chars(w, "\\n");
        break;
      case '\r':
        w_put_chars(w, "\\r");
        break;
      case '\t':
        w_put_chars(w, "\\t");
        break;
      default:
        if (ch < 32) {
          char tmp[8];
          snprintf(tmp, sizeof(tmp), "\\u%04x", *s);
          w_put_chars3(w, tmp, 6);
        } else {
          w_put_char(w, ch);
        }
    }
  }
  w_put_char(w, '"');
}

/* base64 */

static char base64_table(unsigned index) {
  index &= 63;
  if (index < 27) {
    return 'A' + index;
  } else if (index < 54) {
    return 'a' - 27 + index;
  } else if (index == 62) {
    return '+';
  } else {
    return '/';
  }
}

static void w_put_base64(writer* w, const unsigned char* data, size_t len) {

  w_put_chars(w, "{\"<type>\":\"base64\",\"value\":\"");

  for (size_t i = 0; i < len; i += 3) {

    size_t remain = len - i;

    unsigned a = data[i];
    unsigned b = (remain > 1) ? data[i + 1] : 0;
    unsigned c = (remain > 2) ? data[i + 2] : 0;

    unsigned triple = (a << 16) | (b << 8) | c;

    w_put_char(w, base64_table(triple >> 18));
    w_put_char(w, base64_table(triple >> 12));
    w_put_char(w, remain > 1 ? base64_table(triple >> 6) : '=');
    w_put_char(w, remain > 2 ? base64_table(triple) : '=');
  }

  w_put_chars(w, "\"}");
}

#endif /* SCHEME2K_C_WRITER_H */
