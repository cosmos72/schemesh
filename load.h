/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
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

void schemesh_import_all_libraries(void) {
  scheme2k_eval("(import (schemesh))");
}
