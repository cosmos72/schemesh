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

/** restore all signal handlers to default values */
int c_signals_restore(void);

/** restore handler for specified signal to default value */
int c_signal_restore(int sig);

/** return != 0 if SIGCHLD was received after last call to this function */
int c_sigchld_consume(void);

/** define functions (signal-number->name) (signal-name->number) */
void define_library_signal(void);

#endif /* SCHEMESH_SIGNAL_H */
