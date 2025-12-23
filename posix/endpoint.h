/**
 * Copyright (C) 2023-2025 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/endpoint.h should only be #included by posix/posix.c"
#endif

/* ----------------------------------- sockaddr functions --------------------------------------- */

static size_t c_endpoint_unix_path_max(void) {
#ifdef UNIX_PATH_MAX
  return UNIX_PATH_MAX;
#else
  return sizeof(((const struct sockaddr_un*)NULL)->sun_path);
#endif
}

/**
 * create and return a string containing a copy of bytes src,
 * which *may* be '\0' terminated.
 * Conversion from bytes to characters uses UTF-8b
 */
static ptr c_make_string_maxlen(const void* src, size_t max_len) {
  const char* end = memchr(src, 0, max_len);
  size_t      len = end ? (size_t)(end - (const char*)src) : max_len;
  return scheme2k_Sstring_utf8b(src, len);
}

/** create and return a Scheme vector containing four values */
static ptr c_make_vector4(ptr elem0, ptr elem1, ptr elem2, ptr elem3) {
  ptr vec = Smake_vector(4, elem0);
  Svector_set(vec, 1, elem1);
  Svector_set(vec, 2, elem2);
  Svector_set(vec, 3, elem3);
  return vec;
}

/**
 * create and return a string containing dotted-decimal representation
 * of IPv4 address stored in saddr4->sin_addr
 * On errors, return default_value
 */
static ptr c_endpoint_inet_to_ipaddr(const struct sockaddr_in* saddr4, ptr default_value) {
  char   buf[INET_ADDRSTRLEN < INET6_ADDRSTRLEN ? INET_ADDRSTRLEN : INET6_ADDRSTRLEN];
  size_t buf_max = sizeof(buf);
  if (inet_ntop(AF_INET, &saddr4->sin_addr, buf, buf_max) == NULL) {
    return default_value;
  } else {
    return c_make_string_maxlen(buf, buf_max);
  }
}

/**
 * create and return a string containing colon-separated hexadecimal representation
 * of IPv6 address stored in saddr->sin6_addr.
 * On errors, return default_value
 */
static ptr c_endpoint_inet6_to_ipaddr(const struct sockaddr_in6* saddr6, ptr default_value) {
  char   buf[INET6_ADDRSTRLEN];
  size_t buf_max = sizeof(buf);

  if (inet_ntop(AF_INET6, &saddr6->sin6_addr, buf, buf_max) == NULL) {
    return default_value;
  } else {
    return c_make_string_maxlen(buf, buf_max);
  }
}

/**
 * create and return a string containing the unix path stored stored in un->sun_path
 * On errors, return default_value
 */
static ptr c_endpoint_unix_to_path(const struct sockaddr_un* un, ptr default_value) {
  (void)default_value;
  return c_make_string_maxlen(un->sun_path, c_endpoint_unix_path_max());
}

/**
 * extract data from specified C sockaddr,
 * and create and return a Scheme vector containing four elements:
 *   the address family, as an integer
 *   the resolved IP address, converted to a Scheme string
 *   the port
 *   the C sockaddr itself, wrapped in a Scheme bytevector
 */
static ptr c_endpoint_to_vector(const struct sockaddr* saddr, const socklen_t len) {
  sa_family_t family       = 0;
  ptr         empty_string = Smake_string(0, 0);
  ptr         address      = empty_string;
  uint16_t    port         = 0;
  if (len >= sizeof(struct sockaddr)) {
    switch ((family = saddr->sa_family)) {
      case AF_INET:
        if (len >= sizeof(struct sockaddr_in)) {
          const struct sockaddr_in* saddr4 = (const struct sockaddr_in*)saddr;

          address = c_endpoint_inet_to_ipaddr(saddr4, empty_string);
          port    = ntohs(saddr4->sin_port);
        }
        break;
      case AF_INET6:
        if (len >= sizeof(struct sockaddr_in6)) {
          const struct sockaddr_in6* saddr6 = (const struct sockaddr_in6*)saddr;

          address = c_endpoint_inet6_to_ipaddr(saddr6, empty_string);
          port    = ntohs(saddr6->sin6_port);
        }
        break;
      case AF_UNIX:
        if (len >= sizeof(struct sockaddr_un)) {
          address = c_endpoint_unix_to_path((const struct sockaddr_un*)saddr, empty_string);
        }
        break;
    }
  }
  return c_make_vector4(
      Sinteger(family), address, Sinteger(port), scheme2k_Sbytevector((const char*)saddr, len));
}

static void c_endpoint_set_port(struct sockaddr* saddr, socklen_t len, uint16_t port) {
  if (len >= sizeof(struct sockaddr)) {
    switch (saddr->sa_family) {
      case AF_INET:
        if (len >= sizeof(struct sockaddr_in)) {
          ((struct sockaddr_in*)saddr)->sin_port = port;
        }
        break;
      case AF_INET6:
        if (len >= sizeof(struct sockaddr_in6)) {
          ((struct sockaddr_in6*)saddr)->sin6_port = port;
        }
        break;
    }
  }
}

/**
 * request from DNS the IP addresses of specified hostname,
 * and return them as a list (vector1 vector2 ...)
 * where each vector contains four elements:
 *   the address family, as an integer
 *   the resolved IP address, converted to a Scheme string
 *   override_port
 *   a C sockaddr containing all the above, wrapped in a Scheme bytevector
 * On error, return Sinteger(c_errno())
 */
static ptr c_hostname_to_endpoint_list(const char* hostname,
                                       int         preferred_family,
                                       const char* servicename_or_port,
                                       uint16_t    override_port) {
  int err = 0;
  if (!hostname) {
    err = c_errno_set(EINVAL);
  } else {
    struct addrinfo  hints = {};
    struct addrinfo* list  = NULL;
    hints.ai_family        = preferred_family;
    if (getaddrinfo(hostname, servicename_or_port, &hints, &list) != 0) {
      err = c_errno();
    } else {
      struct addrinfo* info     = list;
      ptr              ret      = Snil;
      struct sockaddr* prev     = NULL;
      socklen_t        prev_len = 0;
      override_port             = htons(override_port);
      while (info) {
        struct sockaddr* saddr = info->ai_addr;
        socklen_t        len   = info->ai_addrlen;
        if (override_port) {
          c_endpoint_set_port(saddr, len, override_port);
        }
        /* only add non-duplicate sockaddr. Shall we also check for non-consecutive duplicates? */
        if (!prev || prev_len != len || memcmp(prev, saddr, len) != 0) {
          prev     = saddr;
          prev_len = len;
          ret      = Scons(c_endpoint_to_vector(saddr, len), ret);
        }
        info = info->ai_next;
      }
      freeaddrinfo(list);
      return ret; /* return the list in reverse order. Scheme will unreverse it */
    }
  }
  return Sinteger(err);
}

/** create a bytevector containing a sockaddr_inet with specified IPv4 address and port */
static ptr c_endpoint_inet(const char* ipaddr, const uint16_t port) {
  int err = 0;
  if (!ipaddr) {
    err = c_errno_set(EINVAL);
  } else {
    const ptr           ret   = Smake_bytevector(sizeof(struct sockaddr_in), 0);
    struct sockaddr_in* saddr = (struct sockaddr_in*)Sbytevector_data(ret);
    if ((err = inet_pton(AF_INET, ipaddr, &saddr->sin_addr)) < 0) {
      err = c_errno();
    } else if (err == 0) {
      /* invalid ipaddr string */
      err = c_errno_set(EINVAL);
    } else {
      saddr->sin_family = AF_INET;
      saddr->sin_port   = htons(port);
      return ret;
    }
  }
  return Sinteger(err);
}

/** create a bytevector containing a sockaddr_inet6 with specified IPv6 address and port */
static ptr c_endpoint_inet6(const char* ipaddr6, const uint16_t port) {
  int err = 0;
  if (!ipaddr6) {
    err = c_errno_set(EINVAL);
  } else {
    const ptr            ret   = Smake_bytevector(sizeof(struct sockaddr_in6), 0);
    struct sockaddr_in6* saddr = (struct sockaddr_in6*)Sbytevector_data(ret);
    if ((err = inet_pton(AF_INET6, ipaddr6, &saddr->sin6_addr)) < 0) {
      err = c_errno();
    } else if (err == 0) {
      /* invalid ipaddr6 string */
      err = c_errno_set(EINVAL);
    } else {
      saddr->sin6_family = AF_INET6;
      saddr->sin6_port   = htons(port);
      return ret;
    }
  }
  return Sinteger(err);
}

/**
 * create a bytevector containing a sockaddr_un with specified bytevector path */
static ptr c_endpoint_unix(ptr path) {
  if (Sbytevectorp(path)) {
    const iptr path_len = Sbytevector_length(path);
    if (path_len >= 0 && path_len < (iptr)c_endpoint_unix_path_max()) {
      const ptr           ret   = Smake_bytevector(sizeof(struct sockaddr_un), 0);
      struct sockaddr_un* saddr = (struct sockaddr_un*)Sbytevector_data(ret);
      saddr->sun_family         = AF_UNIX;
      memcpy(saddr->sun_path, Sbytevector_data(path), path_len);
      return ret;
    }
  }
  return Sinteger(c_errno_set(EINVAL));
}
