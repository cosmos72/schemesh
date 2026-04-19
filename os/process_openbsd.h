// Source - https://stackoverflow.com/a/38277403
// Posted by user6558785, modified by community. See post 'Timeline' for change history
// Retrieved 2026-03-19, License - CC BY-SA 3.0

#include <errno.h>
#include <kvm.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/sysctl.h>

#define TRUE 1
#define FALSE 0

struct kinfo_proc* getprocs(int* count, int threads) {
  struct kinfo_proc* procbase = NULL;
  unsigned int       maxslp;
  size_t             size         = sizeof(maxslp);
  int                maxslp_mib[] = {CTL_VM, VM_MAXSLP};

  int mib[6] = {CTL_KERN,
                KERN_PROC,
                threads ? KERN_PROC_KTHREAD | KERN_PROC_SHOW_THREADS : KERN_PROC_KTHREAD,
                0,
                sizeof(struct kinfo_proc),
                0};

  if (sysctl(maxslp_mib, 2, &maxslp, &size, NULL, 0) == -1) {
    perror("list");
    return NULL;
  }

retry:

  if (sysctl(mib, 6, NULL, &size, NULL, 0) == -1) {
    perror("list");
    return NULL;
  }

  size     = 5 * size / 4; /* extra slop */
  procbase = (struct kinfo_proc*)malloc(size);
  if (procbase == NULL) {
    perror("list");
    return NULL;
  }

  mib[5] = (int)(size / sizeof(struct kinfo_proc));
  if (sysctl(mib, 6, procbase, &size, NULL, 0)) {
    if (errno == ENOMEM) {
      free(procbase);
      goto retry;
    }
    perror("list");
    return NULL;
  }

  *count = (int)(size / sizeof(struct kinfo_proc));
  return procbase;
}

int showinfo(int threads) {
  struct kinfo_proc *list, *proc;
  int                count, i;
  if ((list = getprocs(&count, threads)) == NULL) {
    return 1;
  }
  proc = list;
  if (threads) {
    for (i = 0; i < count; ++i, ++proc) {
      if (proc->p_tid != -1) {
        printf("%s: pid: %d (tid: %d)\n", proc->p_comm, proc->p_pid, proc->p_tid);
      }
    }
  } else {
    for (i = 0; i < count; ++i, ++proc) {
      printf("%s: pid: %d\n", proc->p_comm, proc->p_pid);
    }
  }
  return 0;
}

int main(int argc, char* argv[]) {
  if (argc == 1) {
    return showinfo(FALSE);
  } else if (argc == 2 && (!strcmp(argv[1], "-t") || !strcmp(argv[1], "--threads"))) {
    return showinfo(TRUE);
  } else {
    printf("Usage:\n");
    printf("      list [-h] [-t]\n\n");
    printf("Options:\n");
    printf("      -h, --help            Print this information\n");
    printf("      -t, --threads         Show threads\n\n");
    return 0;
  }
}
