/**
 * Copyright (C) 2023-2026 by Massimiliano Ghilardi
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 */

#ifndef SCHEME2K_C_PROC_COMMON_H
#define SCHEME2K_C_PROC_COMMON_H

typedef enum {
  e_proc_flag_pid         = 1 << 0,
  e_proc_flag_name        = 1 << 1,
  e_proc_flag_tty         = 1 << 2,
  e_proc_flag_state       = 1 << 3,
  e_proc_flag_user        = 1 << 4,
  e_proc_flag_group       = 1 << 5,
  e_proc_flag_uid         = 1 << 6,
  e_proc_flag_gid         = 1 << 7,
  e_proc_flag_ppid        = 1 << 8,
  e_proc_flag_pgid        = 1 << 9,
  e_proc_flag_sid         = 1 << 10,
  e_proc_flag_mem_rss     = 1 << 11,
  e_proc_flag_mem_virt    = 1 << 12,
  e_proc_flag_start_time  = 1 << 13,
  e_proc_flag_user_time   = 1 << 14,
  e_proc_flag_sys_time    = 1 << 15,
  e_proc_flag_iowait_time = 1 << 16,
  e_proc_flag_priority    = 1 << 17,
  e_proc_flag_threads     = 1 << 18,
  e_proc_flag_min_fault   = 1 << 19,
  e_proc_flag_maj_fault   = 1 << 20,

  e_proc_flag_default =
      e_proc_flag_pid | e_proc_flag_name | e_proc_flag_tty | e_proc_flag_start_time,
  e_proc_flag_long = e_proc_flag_pid | e_proc_flag_name | e_proc_flag_tty | e_proc_flag_state |
                     e_proc_flag_user | e_proc_flag_mem_rss | e_proc_flag_start_time |
                     e_proc_flag_user_time,
  e_proc_flag_verbose = (1 << 21) - 1,

  e_proc_flag_other_users = 1 << 21, /* show processes belonging to other users */
  e_proc_flag_without_tty = 1 << 22, /* show processes not attached to a tty */
} e_proc_flags;

#endif /* SCHEME2K_C_PROC_COMMON_H */
