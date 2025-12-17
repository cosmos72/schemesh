/**
 * Copyright (C) 2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include "http.h"

#include <curl/curl.h>
#include <stdlib.h> /* malloc(), free*( )*/
#include <string.h> /* strerror() */

#if LIBCURL_VERSION_MAJOR > 8 || (LIBCURL_VERSION_MAJOR == 8 && LIBCURL_VERSION_MINOR >= 17)
#define HAVE_LIBCURL_NOTIFY_ENABLE
#else
#undef HAVE_LIBCURL_NOTIFY_ENABLE
#endif

int http_global_init(void) {
  return (int)curl_global_init(CURL_GLOBAL_ALL);
}

void http_global_cleanup(void) {
  (void)curl_global_cleanup();
}

const char* http_global_strerror(int err) {
  return curl_easy_strerror((CURLcode)err);
}

struct recv_node_struct;
typedef struct recv_node_struct recv_node;

struct recv_node_struct {
  recv_node* prev;
  recv_node* next;
  size_t     len;
  /* void * bytes[0] */
};

typedef struct {
  recv_node* head;
  recv_node* tail;
  size_t     offset; /* number of bytes already consumed in ->head */
} recv_list;

static CURLcode recv_list_push_back(recv_list* list, const void* bytes, const size_t len) {
  recv_node* node;
  if (len == 0) {
    return CURLE_OK;
  }
  node = (recv_node*)malloc(sizeof(recv_node) + len);
  if (!node) {
    return CURLE_OUT_OF_MEMORY;
  }
  node->next = NULL;
  node->len  = len;
  if ((node->prev = list->tail) != NULL) {
    node->prev->next = node;
  } else {
    list->head = node;
  }
  list->tail = node;
  memcpy(node + 1, bytes, len);
  return CURLE_OK;
}

static void recv_list_pop_front(recv_list* list) {
  recv_node* node = list->head;
  if (node == NULL) {
    return;
  }
  recv_node* next = node->next;
  free(node);
  if (next) {
    next->prev = NULL;
  } else {
    list->tail = NULL;
  }
  list->head = next;
}

typedef struct http_struct {
  CURLM* multi;
  CURL*  easy;
  struct {
    void*  bytes;
    size_t len;
  } torecv;
  recv_list   received;
  const char* errfunc;
  long        errdetail;
  CURLMcode   mc;
  CURLcode    err;
  char        errbuf[CURL_ERROR_SIZE];
} http;

http* http_new(void) {
  http* ctx = (http*)malloc(sizeof(http));
  if (ctx) {
    ctx->multi           = NULL;
    ctx->easy            = NULL;
    ctx->torecv.bytes    = NULL;
    ctx->torecv.len      = 0;
    ctx->received.head   = NULL;
    ctx->received.tail   = NULL;
    ctx->received.offset = 0;
    ctx->errfunc         = NULL;
    ctx->errdetail       = 0;
    ctx->mc              = CURLM_OK;
    ctx->err             = CURLE_OK;
    ctx->errbuf[0]       = '\0';
  }
  return ctx;
}

void http_del(http* ctx) {
  if (ctx) {
    http_close(ctx);
    free(ctx);
  }
}

static void fprint_mc(http* ctx, FILE* out) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  const char* errmsg = ctx->errbuf[0] ? ctx->errbuf : curl_multi_strerror(ctx->mc);
  fprintf(out, "curlm error %d%s%s: %s\n", (int)ctx->mc, prefix, caller, errmsg);
}

static size_t sprint_mc(http* ctx, char* out, size_t outlen) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  const char* errmsg = ctx->errbuf[0] ? ctx->errbuf : curl_multi_strerror(ctx->mc);
  return snprintf(out, outlen, "curlm error %d%s%s: %s\n", (int)ctx->mc, prefix, caller, errmsg);
}

static void fprint_err(http* ctx, FILE* out) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  const char* errmsg = ctx->errbuf[0] ? ctx->errbuf : curl_easy_strerror(ctx->err);
  fprintf(out, "curl error %d%s%s: %s\n", (int)ctx->err, prefix, caller, errmsg);
}

static size_t sprint_err(http* ctx, char* out, size_t outlen) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  const char* errmsg = ctx->errbuf[0] ? ctx->errbuf : curl_easy_strerror(ctx->err);
  return snprintf(out, outlen, "curl error %d%s%s: %s\n", (int)ctx->err, prefix, caller, errmsg);
}

static void fprint_errdetail(http* ctx, FILE* out) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  long        detail = ctx->errdetail;
  switch (ctx->err) {
    case CURLE_READ_ERROR:
      fprintf(out, "I/O error %ld%s%s: %s\n", detail, prefix, caller, strerror((int)detail));
      break;
    case CURLE_SSH:
      fprintf(out, "SSL error %ld%s%s\n", detail, prefix, caller);
      break;
    case CURLE_PROXY:
      fprintf(out, "PROXY error %ld%s%s\n", detail, prefix, caller);
      break;
    case CURLE_HTTP_RETURNED_ERROR:
      fprintf(out, "HTTP response code %ld%s%s\n", detail, prefix, caller);
      break;
    default:
      break;
  }
}

static size_t sprint_errdetail(http* ctx, char* out, size_t outlen) {
  const char* caller = ctx->errfunc ? ctx->errfunc : "";
  const char* prefix = caller[0] ? " in " : "";
  long        detail = ctx->errdetail;
  switch (ctx->err) {
    case CURLE_READ_ERROR:
      return snprintf(
          out, outlen, "I/O error %ld%s%s: %s\n", detail, prefix, caller, strerror((int)detail));
    case CURLE_SSH:
      return snprintf(out, outlen, "SSL error %ld%s%s\n", detail, prefix, caller);
    case CURLE_PROXY:
      return snprintf(out, outlen, "PROXY error %ld%s%s\n", detail, prefix, caller);
    case CURLE_HTTP_RETURNED_ERROR:
      return snprintf(out, outlen, "HTTP response code %ld%s%s\n", detail, prefix, caller);
    default:
      return 0;
  }
}

void http_fprint_error(http* ctx, FILE* out) {
  if (!ctx) {
    fprintf(out, "%s", "HTTP end of file");
  } else if (ctx->errdetail != 0) {
    fprint_errdetail(ctx, out);
  } else if (ctx->err != CURLE_OK) {
    fprint_err(ctx, out);
  } else if (ctx->mc != CURLM_OK) {
    fprint_mc(ctx, out);
  }
}

size_t http_sprint_error(http* ctx, char* out, size_t outlen) {
  if (!ctx) {
    return snprintf(out, outlen, "%s", "HTTP end of file");
  } else if (ctx->errdetail != 0) {
    return sprint_errdetail(ctx, out, outlen);
  } else if (ctx->err != CURLE_OK) {
    return sprint_err(ctx, out, outlen);
  } else if (ctx->mc != CURLM_OK) {
    return sprint_mc(ctx, out, outlen);
  } else {
    return 0;
  }
}

static CURLcode fail_easy(http* ctx, CURLcode err, const char* caller) {
  if (ctx->err == CURLE_OK) {
    ctx->err     = err;
    ctx->errfunc = caller;
  }
  return err;
}

static CURLMcode fail_multi(http* ctx, CURLMcode mc, const char* caller) {
  if (ctx->mc == CURLM_OK) {
    ctx->mc      = mc;
    ctx->errfunc = caller;
  }
  return mc;
}

static CURLcode fail_detail(http* ctx, CURLcode err, long detail, const char* caller) {
  if (ctx->err == CURLE_OK) {
    ctx->err       = err;
    ctx->errdetail = detail;
    ctx->errfunc   = caller;
  }
  return err;
}

static CURLcode apply_status_easy(http* ctx, CURL* easy, const char* caller) {
  long     n   = 0;
  CURLcode err = curl_easy_getinfo(easy, CURLINFO_OS_ERRNO, &n);
  if (err != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_getinfo(CURLINFO_OS_ERRNO)");
  } else if (n != 0) {
    fail_detail(ctx, err = CURLE_READ_ERROR, n, caller);
  } else if ((err = curl_easy_getinfo(easy, CURLINFO_SSL_VERIFYRESULT, &n)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_getinfo(CURLINFO_SSL_VERIFYRESULT)");
  } else if (n != 0) {
    fail_detail(ctx, err = CURLE_SSH, n, caller);
  } else if ((err = curl_easy_getinfo(easy, CURLINFO_PROXY_ERROR, &n)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_getinfo(CURLINFO_PROXY_ERROR)");
  } else if (n != 0) {
    fail_detail(ctx, err = CURLE_PROXY, n, caller);
  } else if ((err = curl_easy_getinfo(easy, CURLINFO_RESPONSE_CODE, &n)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_getinfo(CURLINFO_RESPONSE_CODE)");
  } else if (n != 200) {
    fail_detail(ctx, err = CURLE_HTTP_RETURNED_ERROR, n, caller);
  }
  return err;
}

static CURLcode apply_msg(http* ctx, CURLMsg* msg, const char* caller) {
  CURLcode err = msg->data.result;
  if (err != CURLE_OK) {
    fail_easy(ctx, err, caller);
  } else {
    CURL* easy = msg->easy_handle;
    if (easy) {
      err = apply_status_easy(ctx, easy, caller);
    }
  }
  return err;
}

static CURLcode loop_apply_msg(http* ctx, const char* caller) {
  CURLMsg* msg;
  int      avail_n = 0;

  while ((msg = curl_multi_info_read(ctx->multi, &avail_n)) != NULL) {
    apply_msg(ctx, msg, caller);
  }
  return ctx->err;
}

#ifdef HAVE_LIBCURL_NOTIFY_ENABLE
static void notify_multi(CURLM* multi, unsigned notification, CURL* easy, void* userp) {
  (void)multi;
  (void)notification;
  (void)easy;
  loop_apply_msg((http*)userp, "http_read()");
}
#endif /* HAVE_LIBCURL_NOTIFY_ENABLE */

static size_t min2(size_t a, size_t b) {
  return a < b ? a : b;
}

/** copy some data from src into caller-provided ctx->torecv */
static size_t copy_some(http* ctx, const void* src, size_t len) {
  const size_t copy_n = min2(len, ctx->torecv.len);
  if (copy_n) {
    char* dst = (char*)ctx->torecv.bytes;
    memcpy(dst, src, copy_n);
    ctx->torecv.bytes = dst + copy_n;
    ctx->torecv.len -= copy_n;
  }
  return copy_n;
}

static size_t on_recv(void* bytes, size_t size, size_t nmemb, void* userp) {
  http*        ctx = (http*)userp;
  const size_t len = size * nmemb;
  /* if no data is already buffered, copy some bytes directly into caller-provided ctx->torecv */
  const size_t copied_n = ctx->received.head == NULL ? copy_some(ctx, bytes, len) : 0;
  if (copied_n < len) {
    /* some data did not fit ctx->torecv, buffer it */
    CURLcode err = recv_list_push_back(&ctx->received, (char*)bytes + copied_n, len - copied_n);
    if (err != CURLE_OK) {
      fail_easy(ctx, err, "http_read()");
      return CURL_WRITEFUNC_ERROR;
    }
  }
  return len;
}

static int setup_multi(http* ctx) {
  CURLM*    multi = curl_multi_init();
  CURLMcode mc    = CURLM_OK;
  if (!multi) {
    mc = CURLM_OUT_OF_MEMORY;
    fail_multi(ctx, mc, "curl_multi_init()");
#ifdef HAVE_LIBCURL_NOTIFY_ENABLE
    /*
     * CURLMOPT_NOTIFYDATA, CURLMOPT_NOTIFYFUNCTION, curl_multi_notify_enable()
     * were added in libcurl 8.17.0
     */
  } else if (((mc = curl_multi_setopt(multi, CURLMOPT_NOTIFYDATA, (void*)ctx)) != CURLM_OK)) {
    fail_multi(ctx, mc, "curl_multi_setopt(CURLMOPT_NOTIFYDATA)");
  } else if ((mc = curl_multi_setopt(multi, CURLMOPT_NOTIFYFUNCTION, &notify_multi)) != CURLM_OK) {
    fail_multi(ctx, mc, "curl_multi_setopt(CURLMOPT_NOTIFYFUNCTION)");
  } else if ((mc = curl_multi_notify_enable(multi, CURLMNOTIFY_EASY_DONE)) != CURLM_OK) {
    fail_multi(ctx, mc, "curl_multi_notify_enable(CURLMNOTIFY_EASY_DONE)");
#endif
  }
#ifdef CURLPIPE_MULTIPLEX
  else {
    (void)curl_multi_setopt(multi, CURLMOPT_PIPELINING, CURLPIPE_MULTIPLEX);
  }
#endif
  ctx->multi = multi;
  return (int)(ctx->mc = mc);
}

static int setup_easy(http* ctx, const char* url) {
  CURL*     easy = curl_easy_init();
  CURLMcode mc   = CURLM_OK;
  CURLcode  err  = CURLE_OK;
  if (!easy) {
    err = CURLE_OUT_OF_MEMORY;
    fail_easy(ctx, err, "curl_easy_init()");
  } else if ((err = curl_easy_setopt(easy, CURLOPT_WRITEDATA, ctx)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_setopt(CURLOPT_WRITEDATA)");
  } else if ((err = curl_easy_setopt(easy, CURLOPT_WRITEFUNCTION, &on_recv)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_setopt(CURLOPT_WRITEFUNCTION)");
  } else if ((err = curl_easy_setopt(easy, CURLOPT_URL, url)) != CURLE_OK) {
    fail_easy(ctx, err, "curl_easy_setopt(CURLOPT_URL)");
  } else if ((mc = curl_multi_add_handle(ctx->multi, easy)) != CURLM_OK) {
    fail_multi(ctx, mc, "curl_multi_add_handle()");

    /* preserve invariant: set ctx->easy only if successfully added to ctx->multi */
    curl_easy_cleanup(easy);
    easy = NULL;
  } else {
    (void)curl_easy_setopt(easy, CURLOPT_ERRORBUFFER, ctx->errbuf);

    /* enlarge the receive buffer for potentially higher transfer speeds */
    (void)curl_easy_setopt(easy, CURLOPT_BUFFERSIZE, 65536L);

    /* HTTP/2 please */
    (void)curl_easy_setopt(easy, CURLOPT_HTTP_VERSION, CURL_HTTP_VERSION_2_0);
    /* (void)curl_easy_setopt(easy, CURLOPT_VERBOSE, 1L); */ /* be verbose */

#ifdef CURLPIPE_MULTIPLEX
    /* wait for pipe connection to confirm */
    (void)curl_easy_setopt(easy, CURLOPT_PIPEWAIT, 1L);
#endif
  }
  ctx->easy = easy;
  return mc != CURLM_OK ? (int)mc : (int)err;
}

int http_open(http* ctx, const char* url) {
  int ret;
  if (!ctx) {
    ret = -1; /* EOF */
  } else if ((ret = setup_multi(ctx)) != 0) {
    http_close(ctx);
  } else if ((ret = setup_easy(ctx, url)) != 0) {
    http_close(ctx);
  }
  return ret;
}

void http_close(http* ctx) {
  if (!ctx) {
    return;
  }
  if (ctx->easy) {
    /* invariant: ctx->easy is set only if successfully added to ctx->multi */
    (void)curl_multi_remove_handle(ctx->multi, ctx->easy);
    (void)curl_easy_cleanup(ctx->easy);
    ctx->easy = NULL;
  }
  if (ctx->multi) {
    (void)curl_multi_cleanup(ctx->multi);
    ctx->multi = NULL;
  }
}

int http_errcode(http* ctx) {
  if (!ctx) {
    return -1; /* EOF */
  }
  if (ctx->errdetail != 0) {
    return (int)ctx->errdetail;
  }
  if (ctx->err != CURLE_OK) {
    return (int)ctx->err;
  }
  if (ctx->mc != CURLM_OK) {
    return (int)ctx->mc;
  }
  return 0;
}

static void consume_some(http* ctx) {
  recv_list* list = &ctx->received;

  if (list->head != NULL && ctx->torecv.len != 0) {
    size_t offset = list->offset;
    size_t avail  = list->head->len;
    if (avail > offset) {
      /* we have some data in ctx->received that can be copied into ctx->torecv */
      size_t copy_n = copy_some(ctx, (const char*)(list->head + 1) + offset, avail - offset);
      if ((list->offset += copy_n) >= avail) {
        /* we consumed the whole list->head, pop it */
        recv_list_pop_front(list);
      }
    }
  }
}

static size_t http_recv(http* ctx, void* dst, size_t dst_start, size_t dst_end, int may_block) {
  size_t    dst_len;
  size_t    got;
  int       still_running = 1;
  CURLMcode mc;

  if (!ctx) {
    return -1; /* EOF */
  } else if (dst_end <= dst_start) {
    return 0;
  }
  dst_len = dst_end - dst_start;

  ctx->torecv.bytes = (char*)dst + dst_start;
  ctx->torecv.len   = dst_len;

  if (ctx->received.head != NULL) {
    /* we have cached data, no need to call libcurl */
  } else if (may_block && (mc = curl_multi_poll(ctx->multi, NULL, 0, 1000, NULL)) != CURLM_OK) {
    fail_multi(ctx, mc, "curl_multi_poll()");
  } else if ((mc = curl_multi_perform(ctx->multi, &still_running)) != CURLM_OK) {
    fail_multi(ctx, mc, "curl_multi_perform()");
  } else if (!still_running) {
    apply_status_easy(ctx, ctx->easy, "http_read()");
  } else if (ctx->err != CURLE_OK) {
    // ctx->err may be set by notify_multi() callback
  } else if (loop_apply_msg(ctx, "http_read()") != CURLE_OK) {
  }
  consume_some(ctx);
  got = dst_len - ctx->torecv.len;
  /* do not keep reference to caller-provided buffer */
  ctx->torecv.bytes = NULL;
  ctx->torecv.len   = 0;
  if (got) {
    return got;
  }
  return still_running ? 0 : (size_t)-1; /* EOF */
}

size_t http_read(http* ctx, void* dst, size_t dst_start, size_t dst_end) {
  return http_recv(ctx, dst, dst_start, dst_end, 1);
}

size_t http_try_read(http* ctx, void* dst, size_t dst_start, size_t dst_end) {
  return http_recv(ctx, dst, dst_start, dst_end, 0);
}

int http_select(http* ctx, int timeout_ms) {
  int       ret;
  CURLMcode mc;
  if (!ctx) {
    ret = -1; /* EOF */
  } else if ((mc = curl_multi_poll(ctx->multi, NULL, 0, timeout_ms, NULL)) != CURLM_OK) {
    ret = (int)fail_multi(ctx, mc, "curl_multi_poll()");
  } else {
    ret = 0;
  }
  return ret;
}
