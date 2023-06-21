/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#ifndef SCHEMESH_SIGNAL_H
#define SCHEMESH_SIGNAL_H

/** install signal handlers */
int c_signals_init(void);

/** restore all installed signal handlers to SIG_DFL */
int c_signals_setdefault(void);

/** restore handler for specified signal to SIG_DFL */
int c_signal_setdefault(int sig);

/** define functions (signal-number->name) (signal-name->number) */
void schemesh_register_c_functions_signals(void);

#endif /* SCHEMESH_SIGNAL_H */
