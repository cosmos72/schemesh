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

static const namepair socket_domains[] = {
#ifdef AF_ALG
    {AF_ALG, "alg"},
#endif
#ifdef AF_APPLETALK
    {AF_APPLETALK, "appletalk"},
#endif
#ifdef AF_AX25
    {AF_AX25, "ax25"},
#endif
#ifdef AF_BLUETOOTH
    {AF_BLUETOOTH, "bluetooth"},
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
#ifdef AF_INET
    {AF_INET, "inet"},
#endif
#ifdef AF_INET6
    {AF_INET, "inet6"},
#endif
#ifdef AF_IPX
    {AF_IPX, "ipx"},
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
#ifdef AF_MPLS
    {AF_MPLS, "mpls"},
#endif
#ifdef AF_NETLINK
    {AF_NETLINK, "netlink"},
#endif
#ifdef AF_PACKET
    {AF_PACKET, "packet"},
#endif
#ifdef AF_PPPOX
    {AF_PPPOX, "ppox"},
#endif
#ifdef AF_RDS
    {AF_RDS, "rds"},
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
 * return a Scheme list containing pairs (domain . name)
 * where domain is a fixnum and name is a symbol
 */
static ptr c_socket_domain_list(void) {
  return c_namepair_list(socket_domains, N_OF(socket_domains));
}

/**
 * return a Scheme list containing pairs (domain . name)
 * where domain is a fixnum and name is a symbol
 */
static ptr c_socket_type_list(void) {
  return c_namepair_list(socket_types, N_OF(socket_types));
}

/**
 * call socket(domain, type, protocol) and return a Scheme integer
 * indicating the opened file descriptor.
 *
 * On error, return a Scheme integer equal to c_errno() i.e. < 0
 */
static ptr c_open_socket_fd(int domain, int type, int protocol) {
  const int fd = socket(domain, type, protocol);
  if (fd < 0) {
    return Sinteger(c_errno());
  }
  return Sinteger(fd);
}

/**
 * call socketpair(AF_UNIX, SOCK_STREAM) and return a Scheme cons (socket1_fd . socket2_fd),
 * or c_errno() on error
 */
static ptr c_open_socketpair_fds(ptr fd1_close_on_exec, ptr fd2_close_on_exec) {
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
