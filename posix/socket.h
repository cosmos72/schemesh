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

/** create a bytevector containing a sockaddr_inet with specified IPv4 address and port */
static ptr c_sockaddr_inet(const char* ipaddr, const uint16_t port) {
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
static ptr c_sockaddr_inet6(const char* ipaddr6, const uint16_t port) {
  int err = 0;
  if (!ipaddr6) {
    err = c_errno_set(EINVAL);
  } else {
    const ptr            ret   = Smake_bytevector(sizeof(struct sockaddr_in6), 0);
    struct sockaddr_in6* saddr = (struct sockaddr_in6*)Sbytevector_data(ret);
    if (inet_pton(AF_INET6, ipaddr6, &saddr->sin6_addr) < 0) {
      err = c_errno();
    } else if (err == 0) {
      /* invalid ipaddr string */
      err = c_errno_set(EINVAL);
    } else {
      saddr->sin6_family = AF_INET6;
      saddr->sin6_port   = htons(port);
      return ret;
    }
  }
  return Sinteger(err);
}

static size_t c_sockaddr_unix_path_max(void) {
#ifdef UNIX_PATH_MAX
  return UNIX_PATH_MAX;
#else
  return sizeof(struct sockaddr_un) - sizeof(sa_family_t);
#endif
}

/** create a bytevector containing a sockaddr_un with specified path */
static ptr c_sockaddr_unix(ptr path) {
  if (Sbytevectorp(path)) {
    const size_t path_len = Sbytevector_length(path);
    if (path_len < c_sockaddr_unix_path_max()) {
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

  ptr       bv;
  socklen_t len = sizeof(saddr);
  int       err = peer ? getpeername(socket, (struct sockaddr*)&saddr, &len) :
                         getsockname(socket, (struct sockaddr*)&saddr, &len);
  if (err != 0) {
    return Sinteger(c_errno());
  } else if (len > sizeof(saddr)) {
    return Sinteger(c_errno_set(EINVAL));
  }
  bv = Smake_bytevector(len, 0);
  memcpy(Sbytevector_data(bv), &saddr, len);
  return Scons(Sinteger(saddr.ss_family), bv);
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
