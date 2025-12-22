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
#error "posix/socket.h should only be #included by posix/posix.c"
#endif

/* ------------------------------------ socket functions ---------------------------------------- */

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

/**
 * create and return a Scheme pair containing (num bytevector)
 * where bytevector contains a copy of len bytes starting at bytes
 */
static ptr c_make_pair_int_bytevector(int num, const void* bytes, size_t len) {
  ptr bv = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bv), bytes, len);
  return Scons(Sinteger(num), bv);
}

static void c_endpoint_set_port(struct sockaddr* saddr, socklen_t len, uint16_t port) {
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

/**
 * request from DNS the IP addresses of specified hostname,
 * and return them as an alist ((family-int1 . bytevector1) (family-int2 . bytevector2) ... )
 * where each bytevector contains the resolved IP address, wrapped in a sockaddr.
 * On error, return Sinteger(c_errno())
 */
static ptr c_hostname_to_addr_alist(const char* hostname,
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
          ret      = Scons(c_make_pair_int_bytevector(saddr->sa_family, saddr, len), ret);
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

static size_t c_endpoint_unix_path_max(void) {
#ifdef UNIX_PATH_MAX
  return UNIX_PATH_MAX;
#else
  return sizeof(((const struct sockaddr_un*)NULL)->sun_path);
#endif
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

/**
 * if peer == 0, get the current address to which specified socket is bound.
 * otherwise, get the address of the peer connected to specified socket.
 * @return a cons containing two values:
 *   the socket family, as an integer
 *   the socket (or peer's) address, i.e. a struct sockaddr stored in a bytevector
 * On error, return Sinteger(c_errno())
 */
static ptr c_socket_sockaddr2(int socket, int peer) {
  struct sockaddr_storage saddr;

  socklen_t len = sizeof(saddr);
  int       err = peer ? getpeername(socket, (struct sockaddr*)&saddr, &len) :
                         getsockname(socket, (struct sockaddr*)&saddr, &len);
  if (err != 0) {
    return Sinteger(c_errno());
  } else if (len > sizeof(saddr)) {
    return Sinteger(c_errno_set(EINVAL));
  } else {
    return c_make_pair_int_bytevector(saddr.ss_family, &saddr, len);
  }
}

/**
 * return uint16_port stored in specified bytevector, which must contain sockaddr bytes,
 * or < 0 if sockaddr family is neither AF_INET nor AF_INET6.
 */
static int c_endpoint_port(ptr bytes) {
  iptr len;
  if (Sbytevectorp(bytes) && (len = Sbytevector_length(bytes)) >= (iptr)sizeof(sa_family_t)) {
    const struct sockaddr* saddr = (const struct sockaddr*)Sbytevector_data(bytes);
    switch (saddr->sa_family) {
      case AF_INET:
        if (len >= (iptr)sizeof(struct sockaddr_in)) {
          return ntohs(((const struct sockaddr_in*)saddr)->sin_port);
        }
        break;
      case AF_INET6:
        if (len >= (iptr)sizeof(struct sockaddr_in6)) {
          return ntohs(((const struct sockaddr_in6*)saddr)->sin6_port);
        }
        break;
      default:
        break;
    }
  }
  return -1;
}

/**
 * create and return a bytevector containing dotted-decimal representation
 * of IPv4 address stored in saddr4->sin_addr
 */
static ptr c_endpoint_inet_to_ipaddr(const struct sockaddr_in* saddr4) {
  char   buf[INET_ADDRSTRLEN < INET6_ADDRSTRLEN ? INET_ADDRSTRLEN : INET6_ADDRSTRLEN];
  size_t buf_max = sizeof(buf);

  if (inet_ntop(AF_INET, &saddr4->sin_addr, buf, buf_max) == NULL) {
    return Sinteger(c_errno_set(EINVAL));
  } else {
    return c_make_string_maxlen(buf, buf_max);
  }
}

/**
 * create and return a bytevector containing colon-separated hexadecimal representation
 * of IPv6 address stored in saddr->sin6_addr
 */
static ptr c_endpoint_inet6_to_ipaddr(const struct sockaddr_in6* saddr6) {
  char   buf[INET6_ADDRSTRLEN];
  size_t buf_max = sizeof(buf);

  if (inet_ntop(AF_INET6, &saddr6->sin6_addr, buf, buf_max) == NULL) {
    return Sinteger(c_errno_set(EINVAL));
  } else {
    return c_make_string_maxlen(buf, buf_max);
  }
}

/**
 * create and return a bytevector containing the unix path stored stored in un->sun_path
 */
static ptr c_endpoint_unix_to_path(const struct sockaddr_un* un) {
  return c_make_string_maxlen(un->sun_path, sizeof(un->sun_path));
}

/**
 * return a newly allocated string containing the textual representation of address stored in
 * specified sockaddr, which must be a bytevector:
 * - if family is 'inet, return IPv4 network address in dotted-decimal format, "ddd.ddd.ddd.ddd"
 * - if family is 'inet6, return IPv6 network address in colon-separated hexadecimal format
 *                    xxxx:xxxx:..." or in IPv6-mapped IPv4 format "xxxx:xxxx:...:ddd.ddd.ddd.ddd"
 * - if family is 'unix, return unix path
 * Otherwise return integer error < 0
 */
static ptr c_endpoint_to_text(ptr bytes) {
  iptr len;
  if (Sbytevectorp(bytes) && (len = Sbytevector_length(bytes)) >= (iptr)sizeof(sa_family_t)) {
    const struct sockaddr* saddr = (const struct sockaddr*)Sbytevector_data(bytes);
    switch (saddr->sa_family) {
      case AF_INET:
        if (len >= (iptr)sizeof(struct sockaddr_in)) {
          return c_endpoint_inet_to_ipaddr((const struct sockaddr_in*)saddr);
        }
        break;
      case AF_INET6:
        if (len >= (iptr)sizeof(struct sockaddr_in6)) {
          return c_endpoint_inet6_to_ipaddr((const struct sockaddr_in6*)saddr);
        }
        break;
      case AF_UNIX:
        if (len >= (iptr)sizeof(struct sockaddr_un)) {
          return c_endpoint_unix_to_path((const struct sockaddr_un*)saddr);
        }
        break;
      default:
        break;
    }
  }
  return Sinteger(c_errno_set(EINVAL));
}

static const namepair socket_families[] = {
#ifdef AF_ALG
    {AF_ALG, "alg"},
#endif
#ifdef AF_APPLETALK
    {AF_APPLETALK, "appletalk"},
#endif
#ifdef AF_ATMPVC
    {AF_ATMPVC, "atmpvc"},
#endif
#ifdef AF_ATMSVC
    {AF_ATMSVC, "atmsvc"},
#endif
#ifdef AF_AX25
    {AF_AX25, "ax25"},
#endif
#ifdef AF_BLUETOOTH
    {AF_BLUETOOTH, "bluetooth"},
#endif
#ifdef AF_CAIF
    {AF_CAIF, "caif"},
#endif
#ifdef AF_CAN
    {AF_CAN, "can"},
#endif
#ifdef AF_DECnet
    {AF_DECnet, "decnet"},
#endif
#ifdef AF_IB
    {AF_IB, "ib"},
#endif
#ifdef AF_IEEE802154
    {AF_IEEE802154, "ieee802154"},
#endif

#ifdef AF_INET
    {AF_INET, "inet"},
#endif
#ifdef AF_INET6
    {AF_INET6, "inet6"},
#endif
#ifdef AF_IPX
    {AF_IPX, "ipx"},
#endif
#ifdef AF_ISDN
    {AF_ISDN, "isdn"},
#endif
#ifdef AF_IUCV
    {AF_IUCV, "iucv"},
#endif
#ifdef AF_KCM
    {AF_KCM, "kcm"},
#endif
#ifdef AF_KEY
    {AF_KEY, "key"},
#endif
#ifdef AF_LLC
    {AF_LLC, "llc"},
#endif
#ifdef AF_MCTP
    {AF_MCTP, "mctp"},
#endif
#ifdef AF_MPLS
    {AF_MPLS, "mpls"},
#endif
#ifdef AF_NETLINK
    {AF_NETLINK, "netlink"},
#endif
#ifdef AF_NETROM
    {AF_NETROM, "netrom"},
#endif
#ifdef AF_PACKET
    {AF_PACKET, "packet"},
#endif
#ifdef AF_PHONET
    {AF_PHONET, "phonet"},
#endif
#ifdef AF_PPPOX
    {AF_PPPOX, "ppox"},
#endif
#ifdef AF_QIPCRTR
    {AF_QIPCRTR, "qipcrtr"},
#endif
#ifdef AF_RDS
    {AF_RDS, "rds"},
#endif
#ifdef AF_ROSE
    {AF_ROSE, "rose"},
#endif
#ifdef AF_RXRPC
    {AF_RXRPC, "rxrpc"},
#endif
#ifdef AF_SMC
    {AF_SMC, "smc"},
#endif
#ifdef AF_TIPC
    {AF_TIPC, "tipc"},
#endif
#ifdef AF_UNIX
    {AF_UNIX, "unix"},
#endif
#ifdef AF_VSOCK
    {AF_VSOCK, "vsock"},
#endif
#ifdef AF_X25
    {AF_X25, "x25"},
#endif
#ifdef AF_XDP
    {AF_XDP, "xdp"},
#endif
};

static const namepair socket_types[] = {
#ifdef SOCK_DGRAM
    {SOCK_DGRAM, "dgram"},
#endif
#ifdef SOCK_RAW
    {SOCK_RAW, "raw"},
#endif
#ifdef SOCK_RDM
    {SOCK_RDM, "rdm"},
#endif
#ifdef SOCK_SEQPACKET
    {SOCK_SEQPACKET, "seqpacket"},
#endif
#ifdef SOCK_STREAM
    {SOCK_STREAM, "stream"},
#endif
};

/**
 * return a Scheme list containing pairs (family . name)
 * where family is a fixnum and name is a symbol
 */
static ptr c_socket_family_list(void) {
  return c_namepair_list(socket_families, N_OF(socket_families));
}

/**
 * return a Scheme list containing pairs (domain . name)
 * where domain is a fixnum and name is a symbol
 */
static ptr c_socket_type_list(void) {
  return c_namepair_list(socket_types, N_OF(socket_types));
}

/**
 * call socket(domain, type, protocol) and return an int
 * indicating the opened file descriptor.
 *
 * On error, return c_errno() i.e. < 0
 */
static int c_socket_fd(int domain, int type, int protocol, ptr close_on_exec) {
  int fd;
#ifdef SOCK_CLOEXEC
  if (close_on_exec != Sfalse) {
    type |= SOCK_CLOEXEC;
  }
#endif
  if ((fd = socket(domain, type, protocol)) >= 0) {
    int err = 0;
#ifndef SOCK_CLOEXEC
    if (close_on_exec != Sfalse) {
      err = fcntl(fd, F_SETFD, FD_CLOEXEC);
    }
#endif
    if (err >= 0) {
      return fd;
    }
    close(fd);
  }
  return c_errno();
}

static int c_socket_bind(int fd, const struct sockaddr* saddr, size_t saddr_len) {
  int       err;
  socklen_t len = (socklen_t)saddr_len;
  if (!saddr || (size_t)len != saddr_len) {
    err = c_errno_set(EINVAL);
  } else if (bind(fd, saddr, len) != 0) {
    err = c_errno();
  } else {
    err = 0;
  }
  return err;
}

static int c_socket_connect(int fd, const struct sockaddr* saddr, size_t saddr_len) {
  int       err;
  socklen_t len = (socklen_t)saddr_len;
  if (!saddr || (size_t)len != saddr_len) {
    err = c_errno_set(EINVAL);
  } else if (connect(fd, saddr, len) != 0) {
    err = c_errno();
  } else {
    err = 0;
  }
  return err;
}

static int c_socket_listen(int fd, int backlog) {
  int err = listen(fd, backlog);
  if (err != 0) {
    err = c_errno();
  }
  return err;
}

static int c_socket_accept(int fd) {
  int err = accept(fd, NULL, 0);
  if (err < 0) {
    err = c_errno();
  }
  return err;
}

/**
 * call socketpair(AF_UNIX, SOCK_STREAM) and return a Scheme cons (socket1_fd . socket2_fd),
 * or c_errno() on error
 */
static ptr c_socketpair_fds(ptr fd1_close_on_exec, ptr fd2_close_on_exec) {
#if defined(AF_UNIX) && defined(SOCK_STREAM)
  int fds[2];
  int err = socketpair(AF_UNIX, SOCK_STREAM, 0, fds);
  if (err < 0) {
    return Sinteger(c_errno());
  }
  if (fd1_close_on_exec != Sfalse) {
    err = fcntl(fds[0], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0 && fd2_close_on_exec != Sfalse) {
    err = fcntl(fds[1], F_SETFD, FD_CLOEXEC);
  }
  if (err == 0) {
    return Scons(Sinteger(fds[0]), Sinteger(fds[1]));
  }
  err = c_errno();
  (void)close(fds[0]);
  (void)close(fds[1]);
  return Sinteger(err);
#elif defined(EAFNOSUPPORT)
  return c_errno_set(EAFNOSUPPORT);
#else
  return c_errno_set(EINVAL);
#endif
}
