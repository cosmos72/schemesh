/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#include <dirent.h>        /* opendir()        */
#include <inttypes.h>      /*                  */
#include <sys/sysmacros.h> /* major(), minor() */
#include <sys/types.h>     /* opendir()        */

#include "process_util_linux.h"

static struct timespec boot_time_utc = {0, -1};

static struct timespec get_boot_time_utc(void) {
  struct timespec ret = boot_time_utc;
  if (ret.tv_nsec < 0) {
    struct timespec elapsed_since_boot1;
    struct timespec time_utc;
    struct timespec elapsed_since_boot2;
    if (clock_gettime(CLOCK_BOOTTIME, &elapsed_since_boot1) == 0 && /**/
        clock_gettime(CLOCK_REALTIME, &time_utc) == 0 &&
        clock_gettime(CLOCK_BOOTTIME, &elapsed_since_boot2) == 0) {

      ret = timespec_sub(time_utc, /* */
                         timespec_avg(elapsed_since_boot1, elapsed_since_boot2));
    } else {
      /* failed to get elapsed_since_boot and time_utc */
      ret.tv_nsec = 0;
    }
    boot_time_utc = ret;
  }
  return ret;
}

/**
 * skip whitespace, copy command name in parentheses (...) to dst.
 * Command name may contain any character, including spaces and ')'
 * Solution: scan src from end and skip all numbers and status, stop at ')'
 * Always add '\0' terminator to dst.
 * @return number of copied characters.
 */
static size_t parse_linux_command(const unsigned char** src,
                                  const unsigned char*  src_end,
                                  char                  dst[],
                                  size_t                dstlen) {
  size_t len;
  skip_ws(src);
  /* skip initial '(' */
  if (**src == '(') {
    ++*src;
  }
  while (src_end > *src) {
    --src_end;
    /* skip final ')' and exit loop */
    if (*src_end == ')') {
      break;
    }
  }
  len = src_end - *src;
  if (len >= dstlen) {
    len = dstlen - 1;
  }
  if (dst) {
    memcpy(dst, *src, len);
    dst[len] = '\0';
  }
  *src = src_end + (*src_end == ')');
  return len;
}

static ptr make_tty_name(int64_t tty_nr) {
  if (tty_nr > 0) {
    char        buf[32];
    const char* prefix = NULL;
    unsigned    hi     = major(tty_nr);
    unsigned    lo     = minor(tty_nr);
    int         buflen = 0;
    switch (hi) {
      /**
       * char devices extracted from
       * https://www.kernel.org/doc/Documentation/admin-guide/devices.txt
       */
      case 4:
        if (lo < 64) {
          prefix = "tty";
        } else {
          lo -= 64;
          prefix = "ttyS";
        }
        break;
      case 5:
        if (lo == 1) {
          memcpy(buf, "console", (buflen = 7) + 1);
        }
        break;
      case 22: /* Digiboard serial card */
        prefix = "ttyD";
        break;
      case 24: /* Stallion serial card */
        prefix = "ttyE";
        break;
      case 32: /* Specialix serial card */
        prefix = "ttyX";
        break;
      case 34: /* Z8530 HDLC serial card */
        prefix = "scc";
        break;
      case 43: /* isdn4linux virtual modem */
        prefix = "ttyI";
        break;
      case 46: /* Comtrol Rocketport serial card */
        prefix = "ttyR";
        break;
      case 48: /* SDL RISCom serial card */
        prefix = "ttyL";
        break;
      case 52: /* Spellcaster DataComm/BRI ISDN card */
        prefix = "dcbri";
        break;
      case 54: /* Electrocardiognosis Holter serial card */
        prefix = "holter";
        break;
      case 57: /* Hayes ESP serial card */
        prefix = "ttyP";
        break;
      case 71: /* Computone IntelliPort II serial card */
        prefix = "ttyF";
        break;
      case 75: /* Specialix IO8+ serial card */
        prefix = "ttyW";
        break;
      case 78: /* PAM Software's multimodem boards */
        prefix = "ttyM";
        break;
      case 88: /* COMX synchronous serial card */
        prefix = "comx";
        break;
      case 100: /* Telephony for Linux */
        prefix = "phone";
        break;
      case 105: /* Comtrol VS-1000 serial controller */
        prefix = "ttyV";
        break;
      case 112: /* ISI serial card -  same names as major 78 */
        prefix = "ttyM";
        break;
      case 155: /* TI link cable devices */
        if (lo >= 16) {
          lo -= 16;
          prefix = "tiusb";
        } else if (lo >= 8) {
          lo -= 8;
          prefix = "tiser";
        }
        break;
      case 136: /** Unix98 /dev/pts */
      case 137: /** Unix98 /dev/pts */
      case 138: /** Unix98 /dev/pts */
      case 139: /** Unix98 /dev/pts */
      case 140: /** Unix98 /dev/pts */
      case 141: /** Unix98 /dev/pts */
      case 142: /** Unix98 /dev/pts */
      case 143: /** Unix98 /dev/pts */
        prefix = "pts/";
        lo += (hi - 136) << 8; /* recent Linux versions only use hi == 136 */
        break;
      case 148: /* Technology Concepts serial card */
        prefix = "ttyT";
        break;
      case 154: /* Specialix RIO serial card */
        prefix = "ttySR";
        break;
      case 156: /* Specialix RIO serial card */
        prefix = "ttySR";
        lo += 256;
        break;
      case 158: /* Dialogic GammaLink fax driver */
        prefix = "gfax";
        break;
      case 161: /* IrCOMM devices */
        if (lo < 16) {
          prefix = "ircomm";
        }
        break;
      case 164: /* Chase Research AT/PCI-Fast serial card */
        prefix = "ttyCH";
        break;
      case 166: /* ACM USB modems */
        prefix = "ttyACM";
        break;
      case 172: /* Moxa Intellio serial card */
        prefix = "ttyMX";
        break;
      case 174: /* SmartIO serial card */
        prefix = "ttySI";
        break;
      case 188: /* USB serial converters */
        prefix = "ttyUSB";
        break;
      case 204: /* Low-density serial ports */
        if (lo < 4) {
          prefix = "ttyLU"; /* LinkUp Systems L72xx UART */
        } else if (lo == 4) {
          prefix = "ttyFB"; /* Intel Footbridge (ARM) */
        } else if (lo < 8) {
          lo -= 4;
          prefix = "ttySA"; /* StrongARM builtin serial port */
        } else if (lo < 12) {
          lo -= 8;
          prefix = "ttySC"; /* SCI serial port (SuperH) */
        } else if (lo < 16) {
          lo -= 12;
          prefix = "ttyFW"; /* Firmware console */
        } else if (lo < 32) {
          lo -= 16;
          prefix = "ttyAM"; /* ARM "AMBA" serial port */
        } else if (lo < 40) {
          lo -= 32;
          prefix = "ttyDB"; /* DataBooster serial port */
        } else if (lo == 40) {
          lo     = 0;
          prefix = "ttySG"; /* SGI Altix console port */
        } else if (lo < 44) {
          lo -= 40;
          prefix = "ttySMX"; /* Motorola i.MX port */
        } else if (lo < 46) {
          lo -= 44;
          prefix = "ttyMM"; /* Marvell MPSC port (obsolete unused) */
        } else if (lo < 52) {
          lo -= 46;
          prefix = "ttyCPM"; /* PPC CPM (SCC or SMC) port */
        } else if (lo < 148) {
          if (lo == 82 || lo == 83) {
            lo -= 82;
            prefix = "ttyVR"; /* NEC VR4100 series */
          }
        } else if (lo < 154) {
          lo -= 148;
          prefix = "ttyPSC"; /* PPC PSC port */
        } else if (lo < 170) {
          lo -= 154;
          prefix = "ttyAT0"; /* ATMEL serial port */
        } else if (lo < 186) {
          lo -= 170;
          prefix = "ttyNX"; /* Hilscher netX serial port */
        } else if (lo == 186) {
          lo     = 0;
          prefix = "ttyJ"; /* JTAG1 DCC protocol based serial port emulation */
        } else if (lo < 191) {
          lo -= 187;
          prefix = "ttyUL"; /* Xilinx uartlite port */
        } else if (lo == 191) {
          lo     = 0;
          prefix = "xvc"; /* Xen virtual console port */
        } else if (lo < 196) {
          lo -= 192;
          prefix = "ttyPZ"; /* pmac_zilog port */
        } else if (lo < 205) {
          lo -= 196;
          prefix = "ttyTX"; /* TX39/49 serial port */
        } else if (lo < 209) {
          lo -= 205;
          prefix = "ttySC"; /* SC26xx serial port */
        } else if (lo < 213) {
          lo -= 209;
          prefix = "ttyMAX"; /* MAX3100 serial port */
        }
        break;
      case 208: /* User space serial ports */
        prefix = "ttyU";
        break;
      case 224: /* A2232 serial card */
        prefix = "ttyY";
        break;
      case 227: /* IBM 3270 terminal Unix tty access */
        prefix = "3270/tty";
        break;
      case 228: /* IBM 3270 terminal block-mode access */
        prefix = "3270/tub";
        break;
      case 229: /* WSL virtual console, IBM iSeries/pSeries virtual console */
        prefix = "hvc";
        break;
      case 256: /* Equinox SST multi-port serial boards */
        prefix = "ttyEQ";
        break;
      default:
        break;
    }
    if (prefix) {
      buflen = snprintf(buf, sizeof(buf), "%s%u", prefix, lo);
    }
    /* Sstring_of_length() works as expected only for ASCII chars */
    /*   i.e. 0 < buf[i] < 128 for every i                        */
    return buflen ? Sstring_of_length(buf, buflen) : Svoid;
  }
  return Sfalse;
}

/** convert Scheme exact integer to C DIR* */
static DIR* to_dir(ptr dir_s) {
  uintptr_t dir_u;
  if ((Sfixnump(dir_s) || Sbignump(dir_s)) && (dir_u = Sunsigned_value(dir_s)) != 0) {
    return (DIR*)(void*)dir_u;
  }
  return NULL;
}

/**
 * on success, return scheme unsigned number containing C DIR*
 * on error, return c_errno() < 0
 */
static ptr c_process_open(void) {
  DIR* dir = opendir("/proc");
  if (!dir) {
    return Sinteger(c_errno()); /* < 0 */
  }
  return Sunsigned((uintptr_t)(void*)dir);
}

static void c_process_close(ptr dir_s) {
  DIR* dir = to_dir(dir_s);
  if (dir) {
    closedir(dir);
  }
}

/**
 * read dir until we find a directory whose name is a decimal number
 * and return corresponding dirent*
 *   on end-of-dir, set errno = 0 and return NULL
 *   on error, set errno > 0 and return NULL
 */
static struct dirent* c_process_get_next_pid(DIR* dir) {
  struct dirent* entry;
  if (!dir) {
    c_errno_set(EINVAL);
    return NULL;
  }
  c_errno_set(0);
  do {
    entry = readdir(dir);
  } while (entry && !string_is_decimal_number(entry->d_name));
  return entry;
}

/**
 * skip one process pid in /proc.
 *   on success, return 1
 *   on end-of-dir, return 0
 *   on error, return c_errno() < 0
 */
static int c_process_skip(ptr dir_s) {
  struct dirent* entry = c_process_get_next_pid(to_dir(dir_s));
  if (!entry) {
    return c_errno(); /* 0 if end of dir, otherwise error */
  }
  return 1; /* ok, skipped one pid */
}

/**
 * read one process from /proc.
 *   on success, return cons (command_string . tty_string) and fill bvec.
 *   on end-of-dir, return 0
 *   on I/O error, return c_errno() < 0
 *   on parsing errors, return #f
 */
static ptr c_process_get(ptr dir_s, ptr bvec) {
  char                 buf[4096];
  char                 comm[256];
  const unsigned char* src;
  const unsigned char* src_end;
  uint8_t*             vec;
  DIR*                 dir;
  struct dirent*       entry;
  uint64_t             user_time_ticks, sys_time_ticks, start_time_ticks;
  int64_t              uid, gid, tty_nr;
  int                  buf_written;
  char                 state;
  uint8_t              ok;

  if (!Sbytevectorp(bvec) || Sbytevector_length(bvec) != e_byte_n) {
    return Sinteger(c_errno_set(EINVAL));
  }
  vec = Sbytevector_data(bvec);

  // ---- Read next /proc/<pid>/stat ----
  dir   = to_dir(dir_s);
  entry = c_process_get_next_pid(dir);
  if (!entry) {
    return Sinteger(c_errno()); /* 0 if end of dir, otherwise error */
  }
  memset(vec, '\0', e_byte_n);

  buf_written = snprintf(buf, sizeof(buf), "%s/stat", entry->d_name);

  src_end = src = (const unsigned char*)buf;

  ok = buf_written > 0 && (unsigned)buf_written < sizeof(buf) &&
       (src_end = read_file_at(dirfd(dir), buf, (unsigned char*)buf, sizeof(buf), &uid, &gid)) &&
       parse_int64(&src, vec, e_pid) && parse_linux_command(&src, src_end, comm, sizeof(comm)) &&
       parse_char(&src, &state) && parse_int64(&src, vec, e_ppid) &&
       parse_int64(&src, vec, e_pgid) && parse_int64(&src, vec, e_sid) &&
       parse_int64(&src, &tty_nr, 0) && parse_int64(&src, NULL, 0 /*tty_pgrp*/) &&
       parse_uint64(&src, NULL, 0 /*flags*/) && parse_uint64(&src, vec, e_min_fault) &&
       parse_uint64(&src, NULL, 0 /*child_min_fault*/) && parse_uint64(&src, vec, e_maj_fault) &&
       parse_uint64(&src, NULL, 0 /*child_maj_fault*/) && parse_uint64(&src, &user_time_ticks, 0) &&
       parse_uint64(&src, &sys_time_ticks, 0) && parse_int64(&src, NULL, 0 /*child_user_time*/) &&
       parse_int64(&src, NULL, 0 /*child_sys_time*/) && parse_int64(&src, vec, e_priority) &&
       parse_int64(&src, NULL, 0 /*nice*/) && parse_int64(&src, vec, e_num_thread) &&
       parse_int64(&src, NULL, 0 /*obsolete*/) && parse_uint64(&src, &start_time_ticks, 0) &&
       parse_uint64(&src, vec, e_mem_virt) && parse_uint64(&src, vec, e_mem_rss);

  if (ok) {
    uint64_t tick_per_s, iowait_time_ticks;

    unsigned i;
    /* skip fields 25...39 */
    for (i = 25; i < 40 && parse_uint64(&src, NULL, 0); i++) {
    }
    iowait_time_ticks = 0;
    if (i == 40) {
      ok = parse_uint64(&src, NULL, 0 /*rt_priority*/) && /**/
           parse_uint64(&src, NULL, 0 /*rt_policy*/) &&   /**/
           parse_uint64(&src, &iowait_time_ticks, 0);
    }

    set_int64(vec, e_uid, uid);
    set_int64(vec, e_gid, gid);

    /* convert mem_resident from pages to bytes */
    uint64_multiply(vec, e_mem_rss, get_os_pagesize());

    tick_per_s = get_os_tick_per_s();

    /* convert ticks -> struct timespec */
    {
      struct timespec start_time_utc =
          timespec_add(get_boot_time_utc(), ticks_to_timespec(start_time_ticks, tick_per_s));
      set_timespec(vec, e_start_time, start_time_utc);
    }
    {
      struct timespec user_time = ticks_to_timespec(user_time_ticks, tick_per_s);
      set_timespec(vec, e_user_time, user_time);
    }
    {
      struct timespec sys_time = ticks_to_timespec(sys_time_ticks, tick_per_s);
      set_timespec(vec, e_sys_time, sys_time);
    }
    {
      struct timespec iowait_time = ticks_to_timespec(iowait_time_ticks, tick_per_s);
      set_timespec(vec, e_iowait_time, iowait_time);
    }

    vec[e_state * 8] = (uint8_t)state;

    return Scons(scheme2k_Sstring_utf8b(comm, (size_t)-1), make_tty_name(tty_nr));
  }
#if 0
  fprintf(stderr, "error parsing %s\n", entry->d_name);
  fflush(stderr);
#endif /* 0 */
  return Sfalse;
}
