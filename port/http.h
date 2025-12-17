/**
 * Copyright (C) 2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef CHEZ_BATTERIES_PORT_HTTP_H
#define CHEZ_BATTERIES_PORT_HTTP_H

#include <stddef.h> /* size_t */
#include <stdio.h>  /* FILE* */

enum { HTTP_EOF = -1 };

typedef struct http_struct http;

int         http_global_init(void);
void        http_global_cleanup(void);
const char* http_global_strerror(int err);

http* http_new(void);
void  http_del(http* ctx);

int  http_open(http* ctx, const char* url);
void http_close(http* ctx);

/**
 * Receive up to dst_end - dst_start bytes and write them into dst[dst_start ...].
 *
 * @return number of bytes actually read, which may also be == 0 on success,
 * and may also be != 0 on error. On end-of-file, returns (size_t)-1.
 *
 * To check for errors, call http_errcode() after this function returns.
 */
size_t http_read(http* ctx, void* dst, size_t dststart, size_t dstend);

/**
 * Non-blocking try to receive up to dst_end - dst_start bytes and write them
 * into dst[dst_start ...].
 *
 * @return number of bytes actually read, which may also be == 0 on success,
 * and may also be != 0 on error. On end-of-file, returns (size_t)-1.
 *
 * To check for errors, call http_errcode() after this function returns.
 */
size_t http_try_read(http* ctx, void* dst, size_t dststart, size_t dstend);

/**
 * Wait up to timeout_ms milliseconds for I/O to become available on http connection.
 */
int http_select(http* ctx, int timeout_ms);

/** @return current error, or 0 if success */
int http_errcode(http* ctx);
/** Print current error to file, if any */
void http_fprint_error(http* ctx, FILE* out);
/** Print current error to string buffer, if any */
size_t http_sprint_error(http* ctx, char* out, size_t outlen);

#endif /* CHEZ_BATTERIES_PORT_HTTP_H */
