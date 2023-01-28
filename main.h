#ifndef SCHEMESH_MAIN_H
#define SCHEMESH_MAIN_H

#include <scheme.h>

/**
 * call global Scheme procedure having specified symbol name
 * passing a single Scheme argument to it.
 * Return the resulting Scheme value.
 */
ptr call1(const char symbol_name[], ptr arg);

/**
 * call global Scheme procedure having specified symbol name
 * passing two Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr call2(const char symbol_name[], ptr arg1, ptr arg2);

/**
 * call Scheme (eval) on a C string and return the resulting Scheme value
 */
ptr eval(const char str[]);

#endif /* SCHEMESH_MAIN_H */
