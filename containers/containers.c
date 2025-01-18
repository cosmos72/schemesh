/**
 * Copyright (C) 2023-2024 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "containers.h"
#include "../eval.h"

#include <errno.h>  /* errno */
#include <stdint.h> /* uint32_t */
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
    return LIKELY(codepoint < 0x80) ? 1 : 2;
  } else if (codepoint >= 0xDC80 && codepoint < 0xDD00) {
    /*
     * 0xDC80...0xDCFF is inside the surrogate range.
     * UTF-8b uses it to represent a single byte in the range 0x80 - 0xFF
     * that is NOT part of a valid UTF-8 sequence
     */
    return 1;
  } else {
    return LIKELY(codepoint < 0x10000) ? 3 : 4;
  }
}

/**
 * convert Unicode codepoint to UTF-8b sequence, and write such sequence into out.
 * @return number of written bytes on success,
 * otherwise 0 if out_len is too small or invalid codepoint is found.
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
static iptr c_string_to_utf8b_length(ptr string, iptr start, iptr end) {
  iptr result = 0;
  if (Sstringp(string) && start >= 0 && end > start) {
    const iptr len  = Sstring_length(string);
    iptr       i    = start < len ? start : len;
    const iptr iend = (end < len) ? end : len;
    for (; i < iend; ++i) {
      result += c_codepoint_to_utf8b_length(Sstring_ref(string, i));
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
 * otherwise Sfalse if arguments are invalid or provided bytevector is too small.
 * If an invalid codepoint is found, return it.
 */
static ptr c_string_to_utf8b_append(ptr string, iptr start, iptr end, ptr bvec, iptr ostart) {
  if (Sstringp(string) && start >= 0 && end >= start && Sbytevectorp(bvec) && ostart >= 0) {
    const iptr ilen = Sstring_length(string);
    iptr       ipos = start < ilen ? start : ilen;
    const iptr iend = end < ilen ? end >= start ? end : start : ilen;

    octet*     out  = Sbytevector_data(bvec);
    const iptr oend = Sbytevector_length(bvec);
    iptr       opos = ostart;
    for (; ipos < iend; ++ipos) {
      if (UNLIKELY(opos >= oend)) {
        return Sfalse;
      }
      const unsigned codepoint = Sstring_ref(string, ipos);
      const uptr     written   = c_codepoint_to_utf8b(codepoint, out + opos, oend - opos);
      if (LIKELY(written != 0)) {
        opos += written;
      } else {
        return Schar(codepoint); /* codepoint is invalid */
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
static ptr c_string_to_utf8b(ptr string, iptr start, iptr end, iptr zeropad_n) {
  if (start >= 0 && end >= 0 && zeropad_n >= 0) {
    iptr byte_n = c_string_to_utf8b_length(string, start, end);
    ptr  bvec   = Smake_bytevector(byte_n + zeropad_n, 0);
    if (c_string_to_utf8b_append(string, start, end, bvec, 0) != Sfalse) {
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
static uint32_t c_utf8b_to_codepoint_length(const octet* in, size_t in_len) {
  uint32_t in0, in1, in2, in3;
  if (UNLIKELY(in_len == 0)) {
    return 0;
  }
  in0 = in[0];
  if (LIKELY(in0 < 0x80)) {
    return 1;
  }
  if (in0 < 0xC2 || in0 > 0xF4 || in_len == 1) {
    return 1; /* invalid, overlong or truncated UTF-8 sequence. */
  }
  in1 = in[1];
  if ((in1 & 0xC0) != 0x80) {
    return 1; /* invalid continuation byte in UTF-8 sequence. */
  }
  if (in0 < 0xE0) {
    return 2;
  }
  if (in_len == 2) {
    return 1; /* truncated UTF-8 sequence. */
  }
  in2 = in[2];
  if ((in2 & 0xC0) != 0x80) {
    return 1; /* invalid continuation byte in UTF-8 sequence. */
  }
  if (in0 < 0xF0) {
    const uint32_t val = (in0 & 0x0F) << 12 | (in1 & 0x3F) << 6 | (in2 & 0x3F);
    if (val >= 0x800 && (val < 0xD800 || val >= 0xE000)) {
      return 3;
    } else {
      /* overlong UTF-8 sequence, or invalid codepoint in surrogate range 0xD000...0xDFFF */
    }
    return 1;
  }
  if (in_len == 3) {
    return 1; /* truncated UTF-8 sequence. */
  }
  in3 = in[3];
  if (in0 <= 0xF4 && (in3 & 0xC0) == 0x80) {
    const uint32_t val = (in0 & 0x07) << 18 | (in1 & 0x3F) << 12 | (in2 & 0x3F) << 6 | (in3 & 0x3F);
    if (val >= 0x10000 && val < 0x110000) {
      return 4;
    } else {
      /* overlong UTF-8 sequence, or invalid codepoint > 0x10FFFF */
    }
  } else {
    /* invalid byte in UTF-8 sequence. */
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
  /* default: invalid, overlong or truncated UTF-8 sequence. */
  /* Encoded by UTF-8b as 0xDC00 | in0 to allow lossless roundtrip of non UTF-8 data. */
  ret.codepoint = 0xDC00 | in0;
  ret.length    = 1;

  if (in0 < 0xC2 || in0 > 0xF4 || in_len == 1) {
    return ret; /* invalid, overlong or truncated UTF-8 sequence. */
  }
  in1 = in[1];
  if ((in1 & 0xC0) != 0x80) {
    return ret; /* invalid continuation byte in UTF-8 sequence. */
  }
  if (in0 < 0xE0) {
    ret.codepoint = (in0 & 0x1F) << 6 | (in1 & 0x3F);
    ret.length    = 2;
    return ret;
  }
  if (in_len == 2) {
    return ret; /* truncated UTF-8 sequence. */
  }
  in2 = in[2];
  if ((in2 & 0xC0) != 0x80) {
    return ret; /* invalid continuation byte in UTF-8 sequence. */
  }
  if (in0 < 0xF0) {
    const uint32_t val = (in0 & 0x0F) << 12 | (in1 & 0x3F) << 6 | (in2 & 0x3F);
    if (val >= 0x800 && (val < 0xD800 || val >= 0xE000)) {
      ret.codepoint = val;
      ret.length    = 3;
    } else {
      /* overlong UTF-8 sequence, or invalid codepoint in surrogate range 0xD000...0xDFFF */
    }
    return ret;
  }
  if (in_len == 3) {
    return ret; /* truncated UTF-8 sequence. */
  }
  in3 = in[3];
  if (in0 <= 0xF4 && (in3 & 0xC0) == 0x80) {
    const uint32_t val = (in0 & 0x07) << 18 | (in1 & 0x3F) << 12 | (in2 & 0x3F) << 6 | (in3 & 0x3F);
    if (val >= 0x10000 && val < 0x110000) {
      ret.codepoint = val;
      ret.length    = 4;
    } else {
      /* overlong UTF-8 sequence, or invalid codepoint > 0x10FFFF */
    }
  } else {
    /* invalid byte in UTF-8 sequence. */
  }
  return ret;
}

/**
 * convert up to n bytes from UTF-8b to UTF-32 string.
 * and return ONLY the length of converted string i.e. the number of Unicode codepoints.
 */
static size_t c_bytes_utf8b_to_string_length(const octet* bytes, size_t len) {
  size_t ret = 0;
  while (len > 0) {
    const uint32_t consumed = c_utf8b_to_codepoint_length(bytes, len);
    if (consumed == 0 || consumed > len) {
      break; /* should not happen */
    }
    bytes += consumed;
    len -= consumed;
    ret++;
  }
  return ret;
}

/**
 * convert the range [start, end) of UTF-8b bytevector to UTF-32 string.
 * and return ONLY the length of converted string i.e. the number of Unicode codepoints.
 * return 0 if arguments are invalid or out of range.
 */
static iptr c_bytevector_utf8b_to_string_length(ptr bvec, iptr start, iptr end) {
  if (Sbytevectorp(bvec) && start >= 0 && end > start) {
    iptr len = Sbytevector_length(bvec);
    if (start < len) {
      const octet* data = &Sbytevector_u8_ref(bvec, start);
      if (end > len) {
        end = len;
      }
      {
        size_t ret  = c_bytes_utf8b_to_string_length(data, (size_t)(end - start));
        iptr   iret = (iptr)ret;
        if (iret >= 0 && (size_t)iret == ret) {
          return iret;
        }
      }
    }
  }
  return 0;
}

/**
 * convert up to in_len bytes from an UTF-8b C char[] to a Scheme string,
 * starting at position str_start.
 * return the the number of Unicode codepoints written into the string,
 * or Sfalse if caller-provided string is too small.
 */
static ptr c_bytes_utf8b_to_string_append(const octet* in, size_t in_len, ptr str, iptr str_start) {
  iptr str_len = Sstring_length(str);
  if (str_start <= str_len) {
    iptr str_pos = str_start;
    if (in_len == 0) {
      return Sfixnum(0);
    }
    while (in_len > 0) {
      const u32pair pair = c_utf8b_to_codepoint(in, in_len);
      if (pair.length == 0 || pair.length > in_len || str_pos >= str_len) {
        return Sfalse;
      }
      in += pair.length;
      in_len -= pair.length;
      Sstring_set(str, str_pos, pair.codepoint);
      str_pos++;
    }
    return Sfixnum(str_pos - str_start);
  }
  return Sfalse;
}

/**
 * convert up to bvec_end - bvec_start bytes from an UTF-8b bytevector to a Scheme string.
 * return the length of converted string, i.e. the number of Unicode codepoints written into it.
 * return Sfalse if caller-provided string is too small, bytevector range [start, end)
 *   is out-of-range, or any other error.
 */
static ptr c_bytevector_utf8b_to_string_append(
    ptr bvec, iptr bvec_start, iptr bvec_end, ptr str, iptr str_start) {
  if (Sbytevectorp(bvec) && bvec_start >= 0 && bvec_end >= bvec_start && Sstringp(str) &&
      str_start >= 0) {
    octet* bvec_data = Sbytevector_data(bvec);
    iptr   bvec_len  = Sbytevector_length(bvec);
    if (bvec_start <= bvec_len && bvec_end <= bvec_len) {
      return c_bytes_utf8b_to_string_append(
          bvec_data + bvec_start, (size_t)(bvec_end - bvec_start), str, str_start);
    }
  }
  return Sfalse;
}

/**
 * convert a C char[] from UTF-8b to Scheme string and return it.
 * If out of memory, or required string length > maximum string length, raises condition.
 */
ptr schemesh_Sstring_utf8b(const char chars[], const size_t len) {
  size_t slen = c_bytes_utf8b_to_string_length((const octet*)chars, len);
  /* Smake_string() wants iptr length */
  iptr str_len = (iptr)slen;
  if (str_len < 0 || (size_t)str_len != slen) {
    /* Smake_string() will raise condition */
    str_len = -1;
  }
  ptr str     = Smake_string(str_len, 0);
  ptr written = c_bytes_utf8b_to_string_append((const octet*)chars, len, str, 0);
  if (Sfixnump(written) && Sfixnum_value(written) == str_len) {
    return str;
  }
  /* raise condition */
  return Smake_string(-1, 0);
}

/**
 * convert a C char[] to Scheme bytevector and return it.
 * If out of memory, or len > maximum bytevector length, raises condition.
 */
ptr schemesh_Sbytevector(const char chars[], const size_t len) {
  /* Smake_bytevector() wants iptr length */
  iptr bvec_len = (iptr)len;
  if (bvec_len < 0 || (size_t)bvec_len != len) {
    /* Smake_bytevector() will raise condition */
    bvec_len = -1;
  }
  ptr bvec = Smake_bytevector(bvec_len, 0);
  memcpy(Sbytevector_data(bvec), chars, len);
  return bvec;
}

void schemesh_register_c_functions_containers(void) {
  Sregister_symbol("c_bytevector_compare", &c_bytevector_compare);
  Sregister_symbol("c_integer_to_char", &c_integer_to_char);
  Sregister_symbol("c_string_to_utf8b_length", &c_string_to_utf8b_length);
  Sregister_symbol("c_string_to_utf8b_append", &c_string_to_utf8b_append);
#if 0
  Sregister_symbol("c_string_to_utf8b", &c_string_to_utf8b);
#endif /* 0 */
  Sregister_symbol("c_bytevector_utf8b_to_string_length", &c_bytevector_utf8b_to_string_length);
  Sregister_symbol("c_bytevector_utf8b_to_string_append", &c_bytevector_utf8b_to_string_append);
}
