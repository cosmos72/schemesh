#define MAX_SLOTS 1024

struct value_hdr {
    pid_t pid;
    size_t len;
    size_t next_free;
    int used;
};

struct slot {
    pid_t pid;
    size_t offset;      /* offset of value_hdr */
    int occupied;
};

struct mailbox_shm {
    pthread_mutex_t mutex;

    size_t heap_start;   // offset of heap base
    size_t heap_size;

    size_t bump;         // bump pointer (offset from heap_start)

    size_t free_list;    // head of free blocks (offset)

    struct slot slots[MAX_SLOTS];

    unsigned char heap[];
};

/*
+-------------------------+
| mailbox_shm             |
| mutex                   |
| slots[]                 |
| allocator metadata      |
+-------------------------+
| variable-size heap      |
| value_hdr + bytes       |
| value_hdr + bytes       |
| ...                     |
+-------------------------+
*/
struct value_hdr {
    pid_t pid;
    size_t len;
    size_t next_free;
    int used;
};


struct free_block {
  size_t size;
  size_t next;
};

#define ALIGN8(x) (((x) + 7) & ~7)
#define NULL_OFF  ((size_t)0)


// lookup

static size_t hash_pid(pid_t pid)
{
  return ((unsigned)pid * 2654435761u)
    % MAX_SLOTS;
}

// Find slot:

size_t idx = hash_pid(pid);

while (slots[idx].occupied &&
       slots[idx].pid != pid)
  {
    idx = (idx + 1) % MAX_SLOTS;
  }
