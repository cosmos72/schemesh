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

typedef struct {
  uint32_t codepoint;
  uint32_t length;
} u32pair;

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
static ptr c_integer_to_char(const uint32_t codepoint) {
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
static iptr c_codepoint_to_utf8b_length(const uint32_t codepoint) {
  if (LIKELY(codepoint < 0x800)) {
    /*
     * 0xDC80...0xDCFF is inside the surrogate range.
     * UTF-8b uses it to represent a single byte in the range 0x80 - 0xFF
     * that is NOT part of a valid UTF-8 sequence
     */
    return (LIKELY(codepoint < 0x80 || (codepoint >= 0xDC80 && codepoint < 0xDD00))) ? 1 : 2;
  }
  return LIKELY(codepoint < 0x10000) ? 3 : 4;
}

/**
 * convert Unicode codepoint to UTF-8b sequence, and write such sequence into out.
 * @return number of written bytes
 */
static uptr c_codepoint_to_utf8b(string_char codepoint, octet* out, uptr out_len) {
  if (LIKELY(codepoint < 0x80 || (codepoint >= 0xDC80 && codepoint < 0xDD00))) {
    /*
     * 0xDC80...0xDCFF is inside the surrogate range.
     * UTF-8b uses it to represent a single byte in the range 0x80 - 0xFF
     * that is NOT part of a valid UTF-8 sequence
     */
    if (LIKELY(out_len > 0)) {
      out[0] = codepoint;
      return 1;
    }
    return 0;
  }
  if (LIKELY(codepoint < 0x800)) {
    if (LIKELY(out_len >= 2)) {
      out[0] = 0xC0 | ((codepoint >> 6) & 0x1F);
      out[1] = 0x80 | (codepoint & 0x3F);
      return 2;
    }
    return 0;
  }
  if (LIKELY(codepoint < 0x10000)) {
    if (LIKELY(codepoint < 0xD800 || codepoint >= 0xE000)) {
      if (LIKELY(out_len >= 3)) {
        out[0] = 0xE0 | ((codepoint >> 12) & 0x0F);
        out[1] = 0x80 | ((codepoint >> 6) & 0x3F);
        out[2] = 0x80 | (codepoint & 0x3F);
        return 3;
      }
      return 0;
    }
    /* not enough space, or codepoint is in surrogate range: not allowed */
    return 0;
  }
  if (LIKELY(codepoint < 0x110000)) {
    if (LIKELY(out_len >= 4)) {
      out[0] = 0xF0 | ((codepoint >> 18) & 0x07);
      out[1] = 0x80 | ((codepoint >> 12) & 0x3F);
      out[2] = 0x80 | ((codepoint >> 6) & 0x3F);
      out[3] = 0x80 | (codepoint & 0x3F);
      return 4;
    }
  }
  return 0;
}

/**
 * convert a portion of Scheme string to UTF-8b bytevector, and return length of such bytevector.
 * Does not actually create a bytevector - only pretends to.
 */
static iptr c_string_to_utf8b_length(ptr string, iptr start, iptr n) {
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
 * convert a portion of Scheme string to UTF-8b,
 * and write conversion into a caller-provided bytevector,
 * which must be large enough.
 *
 * Return 1 + position of last byte written in bytevector if successful,
 * or Sfalse if arguments are invalid or provided bytevector is too small.
 */
static ptr c_string_to_utf8b_append(ptr string, iptr start, iptr n, ptr bvec, iptr ostart) {
  if (Sstringp(string) && start >= 0 && n >= 0 && Sbytevectorp(bvec) && ostart >= 0) {
    const iptr ilen = Sstring_length(string);
    iptr       ipos = start < ilen ? start : ilen;
    const iptr iend = (n < ilen - ipos) ? ipos + n : ilen;

    octet*     out  = Sbytevector_data(bvec);
    const iptr oend = Sbytevector_length(bvec);
    iptr       opos = ostart;
    for (; ipos < iend; ++ipos) {
      if (UNLIKELY(opos >= oend)) {
        return Sfalse;
      }
      const uptr written = c_codepoint_to_utf8b(Sstring_ref(string, ipos), out + opos, oend - opos);
      if (LIKELY(written != 0)) {
        opos += written;
      } else {
        return Sfalse;
      }
    }
    return Sfixnum(opos);
  }
  return Sfalse;
}

#if 0  /* slower */
/**
 * convert a portion of Scheme string to UTF-8b,
 * and write conversion into a newly created bytevector.
 *
 * Return created bytevector,
 * or Sfalse if arguments are invalid or allocation fails.
 */
static ptr c_string_to_utf8b(ptr string, iptr start, iptr n, iptr zeropad_n) {
  if (start >= 0 && n >= 0 && zeropad_n >= 0) {
    iptr byte_n = c_string_to_utf8b_length(string, start, n);
    ptr  bvec   = Smake_bytevector(byte_n + zeropad_n, 0);
    if (c_string_to_utf8b_append(string, start, n, bvec, 0) != Sfalse) {
      return bvec;
    }
  }
  return Sfalse;
}
#endif /* 0 */

/**
 * convert a single UTF-8b sequence to Unicode codepoint,
 * and return ONLY the number of converted bytes.
 */
static uint32_t c_utf8b_to_codepoint_length(const octet* in, uptr in_len) {
  uint32_t in0, in1, in2, in3;
  if (UNLIKELY(in_len == 0)) {
    return 0;
  }
  in0 = in[0];
  if (LIKELY(in0 < 0x80)) {
    return 1;
  }
  if (in0 < 0xC2 || in0 > 0xF4 || in_len == 1) {
    return 1; // invalid, overlong or truncated UTF-8 sequence.
  }
  in1 = in[1];
  if ((in1 & 0xC0) != 0x80) {
    return 1; // invalid continuation byte in UTF-8 sequence.
  }
  if (in0 < 0xE0) {
    return 2;
  }
  if (in_len == 2) {
    return 1; // truncated UTF-8 sequence.
  }
  in2 = in[2];
  if ((in2 & 0xC0) != 0x80) {
    return 1; // invalid continuation byte in UTF-8 sequence.
  }
  if (in0 < 0xF0) {
    const uint32_t val = (in0 & 0x0F) << 16 | (in1 & 0x3F) << 8 | (in2 & 0x3F);
    if (val >= 0x800 && (val < 0xD800 || val >= 0xE000)) {
      return 3;
    } else {
      // overlong UTF-8 sequence, or invalid codepoint in surrogate range 0xD000...0xDFFF
    }
    return 1;
  }
  if (in_len == 3) {
    return 1; // truncated UTF-8 sequence.
  }
  in3 = in[3];
  if (in0 <= 0xF4 && (in3 & 0xC0) == 0x80) {
    const uint32_t val =
        (in0 & 0x07) << 24 | (in1 & 0x3F) << 16 | (in2 & 0x3F) << 8 || (in3 & 0x3F);
    if (val >= 0x10000 && val < 0x110000) {
      return 4;
    } else {
      // overlong UTF-8 sequence, or invalid codepoint > 0x10FFFF
    }
  } else {
    // invalid byte in UTF-8 sequence.
  }
  return 1;
}

/**
 * convert a single UTF-8b sequence to Unicode codepoint, and return it.
 * Also return number of converted bytes.
 */
static u32pair c_utf8b_to_codepoint(const octet* in, uptr in_len) {
  u32pair  ret;
  uint32_t in0, in1, in2, in3;
  if (UNLIKELY(in_len == 0)) {
    ret.codepoint = 0;
    ret.length    = 0;
    return ret;
  }
  in0 = in[0];
  if (LIKELY(in0 < 0x80)) {
    ret.codepoint = in0;
    ret.length    = 1;
    return ret;
  }
  // default: invalid, overlong or truncated UTF-8 sequence.
  // Encoded by UTF-8b as 0xDC00 | in0 to allow lossless roundtrip of non UTF-8 data.
  ret.codepoint = 0xDC00 | in0;
  ret.length    = 1;

  if (in0 < 0xC2 || in0 > 0xF4 || in_len == 1) {
    return ret; // invalid, overlong or truncated UTF-8 sequence.
  }
  in1 = in[1];
  if ((in1 & 0xC0) != 0x80) {
    return ret; // invalid continuation byte in UTF-8 sequence.
  }
  if (in0 < 0xE0) {
    ret.codepoint = (in0 & 0x1F) << 8 | (in1 & 0x3F);
    ret.length    = 2;
    return ret;
  }
  if (in_len == 2) {
    return ret; // truncated UTF-8 sequence.
  }
  in2 = in[2];
  if ((in2 & 0xC0) != 0x80) {
    return ret; // invalid continuation byte in UTF-8 sequence.
  }
  if (in0 < 0xF0) {
    const uint32_t val = (in0 & 0x0F) << 16 | (in1 & 0x3F) << 8 | (in2 & 0x3F);
    if (val >= 0x800 && (val < 0xD800 || val >= 0xE000)) {
      ret.codepoint = val;
      ret.length    = 3;
    } else {
      // overlong UTF-8 sequence, or invalid codepoint in surrogate range 0xD000...0xDFFF
    }
    return ret;
  }
  if (in_len == 3) {
    return ret; // truncated UTF-8 sequence.
  }
  in3 = in[3];
  if (in0 <= 0xF4 && (in3 & 0xC0) == 0x80) {
    const uint32_t val =
        (in0 & 0x07) << 24 | (in1 & 0x3F) << 16 | (in2 & 0x3F) << 8 || (in3 & 0x3F);
    if (val >= 0x10000 && val < 0x110000) {
      ret.codepoint = val;
      ret.length    = 4;
    } else {
      // overlong UTF-8 sequence, or invalid codepoint > 0x10FFFF
    }
  } else {
    // invalid byte in UTF-8 sequence.
  }
  return ret;
}

/**
 * convert up to n bytes of UTF-8b bytevector to UTF-32 string.
 * and return ONLY the length of converted string i.e. the number of Unicode codepoints.
 */
static iptr c_utf8b_to_string_length(ptr bvec, iptr bvec_start, iptr n) {
  iptr ret = 0;
  if (Sbytevectorp(bvec) && bvec_start >= 0 && n > 0) {
    iptr bvec_len = Sbytevector_length(bvec);
    if (bvec_start < bvec_len) {
      const octet* bvec_data = &Sbytevector_u8_ref(bvec, bvec_start);
      if (bvec_len - bvec_start > n) {
        bvec_len = n;
      } else {
        bvec_len -= bvec_start;
      }
      while (bvec_len > 0) {
        const uint32_t consumed = c_utf8b_to_codepoint_length(bvec_data, bvec_len);
        if (consumed == 0 || consumed > (uptr)bvec_len) {
          break; // should not happen
        }
        bvec_data += consumed;
        bvec_len -= consumed;
        ret++;
      }
    }
  }
  return ret;
}

/**
 * convert up to n bytes of UTF-8b bytevector to UTF-32 string.
 * return the length of converted string, i.e. the number of Unicode codepoints written into it,
 * or Sfalse if caller-provided string is too small.
 */
static ptr c_utf8b_to_string_append(ptr bvec, iptr bvec_start, iptr n, ptr str, iptr str_start) {
  if (Sbytevectorp(bvec) && bvec_start >= 0 && n >= 0 && Sstringp(str) && str_start >= 0) {
    iptr bvec_len = Sbytevector_length(bvec);
    iptr str_len  = Sstring_length(str);
    if (bvec_start <= bvec_len && str_start <= str_len) {
      iptr   str_pos   = str_start;
      octet* bvec_data = &Sbytevector_u8_ref(bvec, bvec_start);
      if (bvec_len - bvec_start > n) {
        bvec_len = n;
      } else {
        bvec_len -= bvec_start;
      }
      while (bvec_len > 0) {
        const u32pair pair = c_utf8b_to_codepoint(bvec_data, bvec_len);
        if (pair.length == 0 || pair.length > (uptr)bvec_len || str_pos >= str_len) {
          return Sfalse;
        }
        bvec_data += pair.length;
        bvec_len -= pair.length;
        Sstring_set(str, str_pos, pair.codepoint);
        str_pos++;
      }
      return Sfixnum(str_pos - str_start);
    }
  }
  return Sfalse;
}

void schemesh_register_c_functions_containers(void) {
  Sregister_symbol("c_bytevector_compare", &c_bytevector_compare);
  Sregister_symbol("c_integer_to_char", &c_integer_to_char);
  Sregister_symbol("c_string_to_utf8b_length", &c_string_to_utf8b_length);
  Sregister_symbol("c_string_to_utf8b_append", &c_string_to_utf8b_append);
#if 0
  Sregister_symbol("c_string_to_utf8b", &c_string_to_utf8b);
#endif /* 0 */
  Sregister_symbol("c_utf8b_to_string_length", &c_utf8b_to_string_length);
  Sregister_symbol("c_utf8b_to_string_append", &c_utf8b_to_string_append);
}
