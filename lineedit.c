/**
 * Copyright (C) 2023 by Massimiliano Ghilardi
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 */

#include "lineedit.h"
#include "eval.h"  // eval()
#include "posix.h" // c_get_tty_fd()

#include <errno.h>     // EINVAL, errno
#include <stdlib.h>    // getenv(), strtoul
#include <sys/ioctl.h> // ioctl(), TIOCGWINSZ
#include <termios.h>   // tcgetattr(), tcsetattr()
#include <unistd.h>

static unsigned long c_parse_unsigned_long(const char* str) {
  if (str != NULL) {
    char*         end = NULL;
    unsigned long n   = strtoul(str, &end, 10);
    if (n == 0 && *end == '\0') {
      return n;
    }
  }
  errno = EINVAL;
  return 0;
}

ptr c_tty_size(void) {
  unsigned long width = 0, height = 0;
  int           c_err = 0;
#ifdef TIOCGWINSZ
  {
    struct winsize wsize;
    int            err;
    while ((err = ioctl(c_get_tty_fd(), TIOCGWINSZ, &wsize)) != 0 && errno == EINTR) {
    }
    if (err != 0) {
      // save ioctl() error
      c_err = c_errno();
    } else if (wsize.ws_col > 0 && wsize.ws_row > 0) {
      width  = wsize.ws_col;
      height = wsize.ws_row;
    }
  }
#endif /* TIOCGWINSZ */
  if (width == 0) {
    width = c_parse_unsigned_long(getenv("COLUMNS"));
  }
  if (height == 0) {
    width = c_parse_unsigned_long(getenv("LINES"));
  }
  if (width != 0 && height != 0) {
    return Scons(Sunsigned(width), Sunsigned(height));
  }
  if (c_err == 0 && (c_err = c_errno()) == 0) {
    c_err = -EINVAL;
  }
  return Sinteger(c_err);
}

static struct termios saved_termios;

int c_tty_setraw(void) {
  struct termios ios;
  size_t         i;
  const int      tty_fd = c_get_tty_fd();

  while (tcgetattr(tty_fd, &saved_termios) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  ios = saved_termios;
  ios.c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR | ISTRIP | IXOFF | IXON | PARMRK);
  ios.c_oflag |= OPOST | ONLCR;
  ios.c_cflag &= ~(CSIZE | PARENB);
  ios.c_cflag |= CS8;
  ios.c_lflag &= ~(/*ECHO | ECHONL | */ ICANON | IEXTEN | ISIG);
  /* ios.c_lflag |= TOSTOP; */
  for (i = 0; i < NCCS; i++) {
    ios.c_cc[i] = 0;
  }
  ios.c_cc[VMIN] = 1;
  while (tcsetattr(tty_fd, TCSADRAIN, &ios) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

int c_tty_restore(void) {
  const int tty_fd = c_get_tty_fd();
  while (tcsetattr(tty_fd, TCSADRAIN, &saved_termios) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

void define_lineedit_functions(void) {
  Sregister_symbol("c_tty_size", &c_tty_size);
  Sregister_symbol("c_tty_setraw", &c_tty_setraw);
  Sregister_symbol("c_tty_restore", &c_tty_restore);

  /**
   * (tty-size) calls C functions c_tty_size(),
   * which returns controlling tty's (width . height), or c_errno() on error
   */
  eval("(define tty-size   (foreign-procedure \"c_tty_size\" () scheme-object))\n");
  eval("(define tty-setraw! (foreign-procedure \"c_tty_setraw\" () int))\n");
  eval("(define tty-restore! (foreign-procedure \"c_tty_restore\" () int))\n");
}
