/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "../eval.h"

#include <string.h> /* memcmp(), memmove() */

/** C backend implementation of (vectory-copy!) */
static void c_vector_copy(ptr src, iptr src_start, ptr dst, iptr dst_start, iptr n) {
#if 0 /* redundant, already checked by Scheme function (vector-copy!) */
  if (Svectorp(src) && Svectorp(dst) && src_start >= 0 && dst_start >= 0 && n > 0 &&
      src_start <= Svector_length(src) && dst_start <= Svector_length(dst) &&
      n <= Svector_length(src) - src_start && n <= Svector_length(dst) - dst_start)
#endif
  {

    ptr* src_ptr = &Svector_ref(src, src_start);
    ptr* dst_ptr = &Svector_ref(dst, dst_start);
    if (src_ptr != dst_ptr) {
      memmove(dst_ptr, src_ptr, n * sizeof(ptr));
    }
  }
}

static signed char c_bytevector_compare(ptr left, ptr right) {
#if 0 /* redundant, already checked by Scheme function (bytevector-compare) */
  if (Sbytevectorp(left) && Sbytevectorp(right))
#endif
  {
    const iptr left_n  = Sbytevector_length(left);
    const iptr right_n = Sbytevector_length(right);
    const iptr n       = left_n < right_n ? left_n : right_n;
    const int cmp = n <= 0 ? 0 : memcmp(Sbytevector_data(left), Sbytevector_data(right), (size_t)n);
    if (cmp != 0) {
      return cmp < 0 ? -1 : 1;
    }
    /* common prefix matches -> shorter bytevector is smaller */
    return left_n < right_n ? -1 : left_n > right_n ? 1 : 0;
  }
}

void schemesh_register_c_functions_containers(void) {
  Sregister_symbol("c_vector_copy", &c_vector_copy);
  Sregister_symbol("c_bytevector_compare", &c_bytevector_compare);
}
