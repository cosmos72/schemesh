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

#define w_put_literal(w, s) w_put_chars_len(w, s, sizeof(s) - 1)

static void w_put_chars_len(writer* w, const char* s, const size_t n) {
  if (WBUF_MAX - w->pos >= n) {
    /* chars fit in buffer */
    memcpy(&w->buf[w->pos], s, n);
    w->pos += n;
    return;
  }
  /* chars do not fit in buffer, flush it and retry */
  w_flush(w);
  if (n < WBUF_MAX) {
    memcpy(&w->buf[w->pos], s, n);
    w->pos += n;
  } else {
    fwrite(s, 1, n, w->f);
  }
}

static void w_put_chars_len2(writer* w, const char s[2]) {
  if (w->pos > WBUF_MAX - 2) {
    w_flush(w);
  }
  memcpy(&w->buf[w->pos], s, 2);
  w->pos += 2;
}

static void w_put_chars_len4(writer* w, const char s[4]) {
  if (w->pos > WBUF_MAX - 4) {
    w_flush(w);
  }
  memcpy(&w->buf[w->pos], s, 4);
  w->pos += 4;
}

static void w_put_chars_len6(writer* w, const char s[6]) {
  if (w->pos > WBUF_MAX - 6) {
    w_flush(w);
  }
  memcpy(&w->buf[w->pos], s, 6);
  w->pos += 6;
}

/*static*/ void w_put_int64(writer* w, int64_t num) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIi64, num);
  w_put_chars_len(w, buf, len);
}

/*static*/ void w_put_uint64(writer* w, uint64_t num) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIu64, num);
  w_put_chars_len(w, buf, len);
}

/*static*/ void w_put_fractional_digits(writer* w, uint64_t fraction, int digit_n) {
  char buf[24];
  int  len = snprintf(buf, sizeof(buf), "%" PRIu64, fraction);
  int  i;
  for (i = len; i < digit_n; i++) {
    w_put_char(w, '0');
  }
  w_put_chars_len(w, buf, len);
}

/*static*/ void w_put_double(writer* w, double num) {
  char buf[32];
  int  len = snprintf(buf, sizeof(buf), "%g", num);
  w_put_chars_len(w, buf, len);
}

/* JSON escape */

static void w_put_escaped_chars_len(writer* w, const char* s, size_t len) {
  size_t i;
  for (i = 0; i < len; i++) {
    const unsigned char ch = s[i];
    if (len == (size_t)-1 && ch == '\0') {
      /* end of nul-terminated string */
      break;
    }
    if (ch == '"' || ch == '\\') {
      w_put_char(w, '\\');
    }
    if (ch >= ' ') {
      w_put_char(w, ch);
      continue;
    }
    switch (ch) {
      case '\b':
        w_put_chars_len2(w, "\\b");
        break;
      case '\f':
        w_put_chars_len2(w, "\\f");
        break;
      case '\n':
        w_put_chars_len2(w, "\\n");
        break;
      case '\r':
        w_put_chars_len2(w, "\\r");
        break;
      case '\t':
        w_put_chars_len2(w, "\\t");
        break;
      default: {
        char tmp[8];
        snprintf(tmp, sizeof(tmp), "\\u%04x", *s);
        w_put_chars_len6(w, tmp);
        break;
      }
    }
  }
}

static inline void w_put_escaped_chars(writer* w, const char* s) {
  w_put_escaped_chars_len(w, s, (size_t)-1);
}

static inline void w_put_quoted_escaped_chars_len(writer* w, const char* s, size_t len) {
  w_put_char(w, '"');
  w_put_escaped_chars_len(w, s, len);
  w_put_char(w, '"');
}

static inline void w_put_quoted_escaped_chars(writer* w, const char* s) {
  w_put_quoted_escaped_chars_len(w, s, (size_t)-1);
}

/* base64 */

static char base64_table(unsigned index) {
  index &= 63;
  if (index < 26) {
    return index + 'A';
  } else if (index < 52) {
    return index + ('a' - 26);
  } else if (index < 62) {
    return index - (52 - '0');
  } else if (index == 62) {
    return '+';
  } else {
    return '/';
  }
}

/*static*/ void w_put_base64(writer* w, const unsigned char* data, size_t len) {
  size_t i;
  w_put_literal(w, "{\"<type>\":\"base64\",\"value\":\"");

  for (i = 0; i + 3 <= len; i += 3) {
    unsigned triple = ((unsigned)data[i] << 16) | ((unsigned)data[i + 1] << 8) | data[i + 2];

    const char buf[4] = {base64_table(triple >> 18),
                         base64_table(triple >> 12),
                         base64_table(triple >> 6),
                         base64_table(triple)};
    w_put_chars_len4(w, buf);
  }
  if (i < len) {
    size_t   remain = len - i;
    unsigned a      = data[i];
    unsigned b      = (remain > 1) ? data[i + 1] : 0;
    unsigned c      = (remain > 2) ? data[i + 2] : 0;
    unsigned triple = (a << 16) | (b << 8) | c;

    const char buf[4] = {base64_table(triple >> 18),
                         base64_table(triple >> 12),
                         remain > 1 ? base64_table(triple >> 6) : '=',
                         remain > 2 ? base64_table(triple) : '='};
    w_put_chars_len4(w, buf);
  }

  w_put_literal(w, "\"}");
}

#endif /* SCHEME2K_C_WRITER_H */
