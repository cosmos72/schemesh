/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include "process_all.h"

void scheme2k_register_c_functions_os(void) {
  Sregister_symbol("c_process_open", &c_process_open);
  Sregister_symbol("c_process_get", &c_process_get);
  Sregister_symbol("c_process_skip", &c_process_skip);
  Sregister_symbol("c_process_close", &c_process_close);
}

#if 0
int main(int argc, char* argv[]) {
  ptr     ret;
  uint8_t vec[e_count * 8];
  if (argc != 2) {
    fprintf(stderr, "Usage: %s <pid|self>\n", argv[0]);
    return 1;
  }
  ret = c_process_get(argv[1], vec);
  return ret == Sfalse ? 1 : 0;
}
#endif /* 0 */
