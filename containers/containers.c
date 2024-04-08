/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "../eval.h"

#include <stdint.h> // uint32_t
#include <string.h> /* memcmp(), memmove() */

#if defined(__GNUC__) && __GNUC__ >= 4
#define LIKELY(pred) __builtin_expect(pred, 1)
#define UNLIKELY(pred) __builtin_expect(pred, 0)
#else
#define LIKELY(pred) (pred)
#define UNLIKELY(pred) (pred)
#endif

static signed char c_bytevector_compare(ptr left, ptr right) {
#if 0 /* redundant, already checked by Scheme function (bytevector-compare) */
  if (Sbytevectorp(left) && Sbytevectorp(right))
#endif
  {
    const iptr left_n  = Sbytevector_length(left);
    const iptr right_n = Sbytevector_length(right);
    const iptr n       = left_n < right_n ? left_n : right_n;
    if (n > 0) {
      const int cmp = memcmp(Sbytevector_data(left), Sbytevector_data(right), (size_t)n);
      if (cmp != 0) {
        return cmp < 0 ? -1 : 1;
      }
    }
    /* common prefix matches -> shorter bytevector is smaller */
    return left_n < right_n ? -1 : left_n > right_n ? 1 : 0;
  }
}

/**
 * similar to (integer->char) but integer Unicode codepoint is not checked for validity:
 * it INTENTIONALLY allows invalid codepoints in the ranges #xD800..#xDFFF and #x10FFFF..#xFFFFFF
 */
static ptr c_integer_to_char(uint32_t codepoint) {
  return Schar(codepoint);
}

/**
 * convert UTF-32 codepoint to UTF-8b sequence, and return length of such sequence.
 * Does not actually create an UTF-8b sequence - only pretends to.
 *
 * For a definition of UTF-8b, see
 *   https://peps.python.org/pep-0383
 *   https://web.archive.org/web/20090830064219/http://mail.nl.linux.org/linux-utf8/2000-07/msg00040.html
 */
static iptr c_codepoint_to_utf8b_length(string_char codepoint) {
  if (LIKELY(codepoint < 0x80)) {
    return 1;
  } else if (LIKELY(codepoint < 0x800)) {
    return 2;
  } else if (LIKELY(codepoint < 0x10000)) {
    if (UNLIKELY(codepoint >= 0xDC80 && codepoint < 0xDD00)) {
      /*
       * unpaired low-half surrogate, it is used to represent a single byte
       * in the range 0x80 - 0xFF that is NOT part of a valid UTF-8 sequence
       */
      return 1;
    } else {
      return 2;
    }
  } else {
    return 4;
  }
}

typedef struct {
  uint32_t u4;
} byte4;

/**
 * convert UTF-32 codepoint to UTF-8b sequence, and return such sequence.
 */
byte4 c_codepoint_to_utf8b(string_char codepoint) {
  byte4 ret = {0};
  if (LIKELY(codepoint < 0x80)) {
    ret.u4 = codepoint;
  } else if (codepoint < 0x800) {
    ret.u4 = 0x80C0 |                    /*                    */
             ((codepoint >> 6) & 0x1F) | /*                    */
             ((codepoint & 0x3F)) << 8;
  } else if (LIKELY(codepoint < 0x10000)) {
    if (UNLIKELY(codepoint >= 0xDC80 && codepoint < 0xDD00)) {
      /*
       * unpaired low-half surrogate, it is used to represent a single byte
       * in the range 0x80 - 0xFF that is NOT part of a valid UTF-8 sequence
       */
      ret.u4 = codepoint & 0xFF;
    } else {
      ret.u4 = 0x8080E0 |                    /*                    */
               ((codepoint >> 12) & 0x0F) |  /*                    */
               ((codepoint << 2) & 0x3F00) | /*                    */
               ((codepoint << 16) & 0x3F0000);
    }
  } else {
    ret.u4 = 0x808080F0 |                     /*                    */
             ((codepoint >> 18) & 0x07) |     /*                    */
             ((codepoint >> 4) & 0x3F00) |    /*                    */
             ((codepoint << 10) & 0x3F0000) | /*                    */
             (codepoint & 0x3F) << 24;
  }
  return ret;
}

/**
 * convert a portion of Scheme string to UTF-8b bytevector, and return length of such bytevector.
 * Does not actually create a bytevector - only pretends to.
 */
static iptr c_string_range_to_utf8b_length(ptr string, iptr start, iptr n) {
  iptr result = 0;
  if (Sstringp(string) && start >= 0 && n > 0) {
    const iptr len = Sstring_length(string);
    iptr       pos = start < len ? start : len;
    const iptr end = (n < len - pos) ? pos + n : len;
    for (; pos < end; ++pos) {
      result += c_codepoint_to_utf8b_length(Sstring_ref(string, pos));
    }
  }
  return result;
}

/**
 * convert a portion of Scheme string to UTF-8b bytevector.
 * Caller must provide a large enough bytevector.
 *
 * Return 1 + position of last byte written in bytevector if successful,
 * or Sfalse if provided bytevector is too small.
 */
static ptr c_string_range_to_utf8b(ptr string, iptr start, iptr n, ptr bvec, iptr ostart) {

  if (Sstringp(string) && start >= 0 && n >= 0 && Sbytevectorp(bvec) && ostart >= 0) {
    const iptr ilen = Sstring_length(string);
    iptr       ipos = start < ilen ? start : ilen;
    const iptr iend = (n < ilen - ipos) ? ipos + n : ilen;
    const iptr oend = Sbytevector_length(bvec);
    iptr       opos = ostart;
    for (; ipos < iend; ++ipos) {
      byte4    bytes = c_codepoint_to_utf8b(Sstring_ref(string, ipos));
      unsigned i;
      for (i = 0; i < 4; i++) {
        if (UNLIKELY(opos >= oend)) {
          return Sfalse;
        }
        Sbytevector_u8_set(bvec, opos++, bytes.u4 & 0xFF);
        bytes.u4 >>= 8;
        if (LIKELY(bytes.u4 == 0)) {
          break;
        }
      }
    }
    return Sfixnum(opos);
  }
  return Sfalse;
}

void schemesh_register_c_functions_containers(void) {
  Sregister_symbol("c_bytevector_compare", &c_bytevector_compare);
  Sregister_symbol("c_integer_to_char", &c_integer_to_char);
  Sregister_symbol("c_string_range_to_utf8b_length", &c_string_range_to_utf8b_length);
  Sregister_symbol("c_string_range_to_utf8b", &c_string_range_to_utf8b);
}
