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
 * call global Scheme procedure having specified symbol name
 * passing three Scheme arguments to it.
 * Return the resulting Scheme value.
 */
ptr call3(const char symbol_name[], ptr arg1, ptr arg2, ptr arg3);

/**
 * call Scheme (eval) on a C string and return the resulting Scheme value
 */
ptr eval(const char str[]);

/**
 * convert Scheme vector-of-bytevector0 to a C-compatible NULL-terminated array of char*
 * usable for example for environ or argz arguments to execve() execvp() etc.
 * returned array should be deallocated with free()
 * and contains pointers into Scheme bytevectors, thus becomes invalid
 * after any call to Scheme functions.
 */
char** vector_to_c_argz(ptr vector_of_bytevector0);

#endif /* SCHEMESH_MAIN_H */
