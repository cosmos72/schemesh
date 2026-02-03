/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

/******************************************************************************/
/*                                                                            */
/*                           tty-related functions                            */
/*                                                                            */
/******************************************************************************/

/** this file should be included only by posix/posix.c */
#ifndef SCHEME2K_POSIX_POSIX_C
#error "posix/tty.h should only be #included by posix/posix.c"
#endif

static int c_tty_init(void) {
  int fd = c_fd_open_max() - 1;
  if (dup2(0, fd) < 0) {
    return scheme2k_init_failed("dup2(0, tty_fd)");
  } else if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
    (void)close(fd);
    return scheme2k_init_failed("fcntl(tty_fd, F_SETFD, FD_CLOEXEC)");
  } else {
    tty_fd = fd;
  }
  return 0;
}

static void c_tty_quit(void) {
  if (tty_fd >= 0 && tty_pgid >= 0) {
    (void)tcsetpgrp(tty_fd, tty_pgid);
  }
}

static int c_tty_getattr(int fd, struct termios* conf) {
#ifdef SCHEMESH_USE_TTY_IOCTL
  return ioctl(fd, TCGETS, conf);
#else
  return tcgetattr(fd, conf);
#endif
}

static int c_tty_setattr(int fd, const struct termios* conf) {
#ifdef SCHEMESH_USE_TTY_IOCTL
  return ioctl(fd, TCSETSW, conf);
#else
  return tcsetattr(fd, TCSADRAIN, conf);
#endif
}

static struct termios saved_conf;
static struct termios raw_conf;
static int            have_saved_conf = 0;
static int            have_raw_conf   = 0;

/** copy initial_conf into conf, then change conf to raw mode */
static void c_tty_fill_raw_conf(struct termios* conf, const struct termios* initial_conf) {
  *conf = *initial_conf;
  conf->c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR | ISTRIP | IXOFF | IXON | PARMRK);
  conf->c_oflag |= OPOST | ONLCR;
  conf->c_cflag &= ~(CSIZE | PARENB);
  conf->c_cflag |= CS8;
  conf->c_lflag &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
  /* conf->c_lflag |= TOSTOP; */

#ifdef VINTR
  conf->c_cc[VINTR] = 0;
#endif
#ifdef VQUIT
  conf->c_cc[VQUIT] = 0;
#endif
#if 0 /* #ifdef VERASE */
  conf->c_cc[VERASE] = '\x7f';
#endif
#ifdef VKILL
  conf->c_cc[VKILL] = 0;
#endif
#ifdef VEOF
  conf->c_cc[VEOF] = 0;
#endif
#ifdef VTIME
  conf->c_cc[VTIME] = 0;
#endif
  conf->c_cc[VMIN] = 1;
#ifdef VSWTC
  conf->c_cc[VSWTC] = 0;
#endif
#ifdef VSWTCH
  conf->c_cc[VSWTCH] = 0;
#endif
#ifdef VSTART
  conf->c_cc[VSTART] = 0;
#endif
#ifdef VSTOP
  conf->c_cc[VSTOP] = 0;
#endif
#ifdef VSUSP
  conf->c_cc[VSUSP] = 0;
#endif
#ifdef VEOL
  conf->c_cc[VEOL] = 0;
#endif
#ifdef VREPRINT
  conf->c_cc[VREPRINT] = 0;
#endif
#ifdef VDISCARD
  conf->c_cc[VDISCARD] = 0;
#endif
#ifdef VDSUSP
  conf->c_cc[VDSUSP] = 0;
#endif
#ifdef VWERASE
  conf->c_cc[VWERASE] = 0;
#endif
#ifdef VLNEXT
  conf->c_cc[VLNEXT] = 0;
#endif
#ifdef VEOL2
  conf->c_cc[VEOL2] = 0;
#endif
}

/** restore controlling tty to saved config */
static int c_tty_restore(void) {
  /* (void)write(1, "; c_tty_restore\r\n", 17); */
  if (have_saved_conf) {
    while (c_tty_setattr(tty_fd, &saved_conf) != 0) {
      if (errno != EINTR) {
        return c_errno();
      }
    }
  }
  return 0;
}

/** save current config of controlling tty, then set it to raw mode */
static int c_tty_setraw(void) {
  struct termios* conf = &raw_conf;

  /* (void)write(1, "; c_tty_setraw\r\n", 16); */
  /*
   * save current config every time, because one of the executed commands
   * may have changed it: we want to preserve such changes for future commands
   */
  while (c_tty_getattr(tty_fd, &saved_conf) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  have_saved_conf = 1;

  if (!have_raw_conf) {
    c_tty_fill_raw_conf(conf, &saved_conf);
    have_raw_conf = 1;
  }
  while (c_tty_setattr(tty_fd, conf) != 0) {
    if (errno != EINTR) {
      return c_errno();
    }
  }
  return 0;
}

static unsigned long c_parse_unsigned_long(const char* str);

/** return a cons (width . height), or c_errno() on error */
static ptr c_tty_size(void) {
  unsigned long width = 0, height = 0;
  int           err = 0;
#ifdef TIOCGWINSZ
  {
    struct winsize wsize;
    while ((err = ioctl(tty_fd, TIOCGWINSZ, &wsize)) != 0 && errno == EINTR) {
    }
    if (err != 0) {
      /* save ioctl() error */
      err = c_errno();
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
  if (err == 0) {
    err = c_errno_set(EINVAL);
  }
  return Sinteger(err);
}

static unsigned long c_parse_unsigned_long(const char* str) {
  if (str != NULL) {
    char*         end = NULL;
    unsigned long n   = strtoul(str, &end, 10);
    if (*end == '\0') {
      return n;
    }
  }
  return 0;
}
